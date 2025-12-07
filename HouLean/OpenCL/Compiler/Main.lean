import HouLean.OpenCL.Compiler.Types
import HouLean.Meta.Float

open Lean Meta Qq HouLean

namespace HouLean.OpenCL.Compiler



def withFVars (xs : Array Expr) (go : Array String → CompileM α) : CompileM α := do
  let names ← xs.mapM (fun x => nameToString <$> x.fvarId!.getUserName)
  fun ctx => do
  -- Ensure unique names by appending counters for duplicates
  let (usedNames, names) := names.foldl (init:=(ctx.usedNames, #[])) (fun (un, ns) n =>
    if let some count := un[n]? then
      (un.insert n (count + 1), ns.push s!"{n}{count + 1}")
    else
      (un.insert n 0, ns.push n))

  let ctx := { ctx with
      fvarMap := (xs.zip names).foldl (init := ctx.fvarMap) (fun m (x, n) => m.insert x n)
      usedNames := usedNames
    }

  trace[HouLean.OpenCL.compiler] "Introduced variables: {names}"
  go names ctx

def getOpenCLAppOrCompile? (e : Expr) (doWhnf := false) :
    CompileM (Option struct { oclFun : OCLFunction,
                           fn : Expr,
                           args : Array Expr }) := do
  let mut e := e
  if doWhnf then
    e ← whnfC e

  if e.isAppOf ``oclFunction then
    try
      let args := e.getAppArgs
      let name ← unsafe evalExpr String q(String) args[2]!
      let kind ← unsafe evalExpr OpenCLFunction.FunKind q(OpenCLFunction.FunKind) args[3]!

      return some {
        oclFun := { name, kind }
        fn := e.stripArgsN (args.size - 4)
        args := args[4:]
      }
    catch _ =>
      throwError "Can't get compiletime value of oclFunction {e}"


  let (fn, args) := e.withApp fun fn args => (fn, args)
  let info ← getFunInfo fn
  let firstExplicit := info.paramInfo.findIdx (fun p => p.isExplicit)
  let fn := fn.beta (args[0:firstExplicit])
  let args := args[firstExplicit:].toArray

  -- Filter out proof arguments
  let args ← args.filterM (fun arg => do
    let argTypeTy ← liftM (inferType arg >>= inferType)
    return !argTypeTy.isProp)

  trace[HouLean.OpenCL.compiler] "Looking for OpenCL function: {fn} with {args.size} explicit args"

  if let some oclFun ← getOpenCLFunction? fn false then
    trace[HouLean.OpenCL.compiler] "✓ Found existing OpenCL function for {fn}"
    return some { oclFun, fn, args }
  else
    trace[HouLean.OpenCL.compiler] "Compiling new function: {fn}"
    let _ ← compileFunction fn
    if let some oclFun ← getOpenCLFunction? fn false then
      trace[HouLean.OpenCL.compiler] "✓ Successfully compiled {fn}"
      return some { oclFun, fn, args }
    else
      trace[HouLean.OpenCL.compiler] "✗ Failed to compile {fn}"
      return none

def runInterpreter (e : Expr) : MetaM (Option String) := do
  let type ← inferType e

  try
    if (← isDefEq type q(Int16)) then
      let val ← unsafe evalExpr Int16 q(Int16) e
      return some (toString val)

    if (← isDefEq type q(Int32)) then
      let val ← unsafe evalExpr Int32 q(Int32) e
      return some (toString val)

    if (← isDefEq type q(Int64)) then
      let val ← unsafe evalExpr Int64 q(Int64) e
      return some (toString val)

    if (← isDefEq type q(UInt16)) then
      let val ← unsafe evalExpr UInt16 q(UInt16) e
      return some (toString val)

    if (← isDefEq type q(UInt32)) then
      let val ← unsafe evalExpr UInt32 q(UInt32) e
      return some (toString val)

    if (← isDefEq type q(UInt64)) then
      let val ← unsafe evalExpr UInt64 q(UInt64) e
      return some (toString val)

    if (← isDefEq type q(Int)) then
      let val ← unsafe evalExpr Int q(Int) e
      return some (toString val)

    if (← isDefEq type q(Int)) then
      let val ← unsafe evalExpr Int q(Int) e
      return some (toString val)

    if (← isDefEq type q(Float32)) then
      let val ← unsafe evalExpr Float32 q(Float32) e
      return some (Float.toString' val.toFloat ++ "f")

    if (← isDefEq type q(Float64)) then
      let val ← unsafe evalExpr Float64 q(Float64) e
      return some (Float.toString' val ++ "d")

    if (← isDefEq type q(Nat)) then
      let val ← unsafe evalExpr Nat q(Nat) e
      return some (toString val)

    if (← isDefEq type q(Int)) then
      let val ← unsafe evalExpr Int q(Int) e
      return some (toString val)

    if (← isDefEq type q(String)) then
      let val ← unsafe evalExpr String q(String) e
      return some (toString val)

  catch _ =>
    return none

  return none

def simplifyExpr (e : Expr) : CompileM Expr := do
  let e_orig := e
  let e := (← Simp.simp e).expr
  let e ← Meta.liftLets e

  if e != e_orig then
    trace[HouLean.OpenCL.compiler] "Simplified expression:\n  Before: {e_orig}\n  After:  {e}"

  return e

partial def compileExpr (e : Expr) : CompileM CodeExpr := do
  withTraceNode `HouLean.OpenCL.compiler
    (fun r => do
      match r with
      | .ok c => return m!"[{checkEmoji}] Expression compiled: {e}\n  → {← c.toString}"
      | .error m => return m!"[{crossEmoji}] Failed to compile expression: {e}\n  Error: {m.toMessageData}") do

  -- Skip proof expressions
  if (← inferType (← inferType e)).isProp then
    trace[HouLean.OpenCL.compiler] "Skipping proof expression: {e}"
    return .errased

  if let some valueStr ← runInterpreter e then
    return .lit valueStr

  let e ← simplifyExpr e

  match e with
  | .app .. =>
    trace[HouLean.OpenCL.compiler] "Compiling application: {e}"
    let some ⟨oclFun, fn, args⟩ ← getOpenCLAppOrCompile? e
      | throwError m!"No OpenCL implementation found for: {e}\n  Function head: {e.getAppFn}\n  Constructor: {e.getAppFn.ctorName}"

    trace[HouLean.OpenCL.compiler] "Compiling {args.size} arguments for {fn}"
    let args ← args.mapM compileExpr
    return .app oclFun args

  | .fvar .. =>
    let some varName := (← read).fvarMap[e]?
      | throwError m!"Unrecognized free variable: {e}\n  Available variables: {(← read).fvarMap.toArray.map (·.1)}"
    trace[HouLean.OpenCL.compiler] "Resolved free variable {e} → {varName}"
    return .fvar varName

  | .letE .. =>
    throwError m!"Unexpected let binding in expression (should be in body): {e}"

  | .lit (.natVal val) =>
    trace[HouLean.OpenCL.compiler] "Nat literal: {val}"
    return .lit (toString val)

  | .lit (.strVal val) =>
    trace[HouLean.OpenCL.compiler] "String literal: {val}"
    return .lit val

  | _ =>
    throwError m!"Cannot compile expression of type {e.ctorName}: {e}"



def addStatement (stmt : CodeStatement) : CompileM Unit :=
  modify (fun s => {s with statements := s.statements.push stmt})

/--
On Lean level: same as `withLetlDeclD name type cont`
On OpenCL level: it introduces a new variable with the same name and value `oclVal` -/
def withLetVar (name : Name) (val : Expr) (oclVal : CodeExpr) (cont : Expr → CompileM α) : CompileM α := do
  let type ← inferType val
  withLetDecl name type val fun var => do
    withFVars #[var] fun varName => do
      let oclType ← getOpenCLType type
      addStatement (.letE varName[0]! oclType oclVal)
      cont var

/--
On Lean level: same as `withLocalDeclD name type cont`
On OpenCL level: it introduces a new variable with the same name and value `oclVal` -/
def withLocalVar (name : Name) (type : Expr) (oclVal : CodeExpr) (cont : Expr → CompileM α) : CompileM α :=
  withLocalDeclD name type fun var => do
    withFVars #[var] fun varName => do
      let oclType ← getOpenCLType type
      addStatement (.letE varName[0]! oclType oclVal)
      cont var
  -- introduce new fvar with value `val` and emit let statement with `oclVal` value

def etaExpand1 (e : Expr) : MetaM Expr := do
  forallBoundedTelescope (← inferType e) (some 1) fun xs _ => do
    mkLambdaFVars xs (e.beta xs)

-- #check Std.Range
-- #check for i in [0:10:3] do
--          IO.println "asdf"

inductive AppCase where
  | bind (mx f : Expr)
  | forLoop (start stop step init f : Expr)
  | ite (cond t e : Expr)
  | app (fn : Expr) (args : Array Expr)
  | oclFunction (oclFun : OCLFunction) (args : Array Expr)
  | value (e : Expr)

def appCase (e : Expr) : MetaM AppCase := do
  if e.isAppOfArity ``bind 6 then
    return .bind e.appFn!.appArg! (← etaExpand1 e.appArg!)

  if e.isAppOfArity ``forIn 9 then
    let f := e.getRevArg! 0
    let init := e.getRevArg! 1
    let range := e.getRevArg! 2
    let start ← mkAppM ``Std.Range.start #[range]
    let stop ← mkAppM ``Std.Range.stop #[range]
    let step ← mkAppM ``Std.Range.step #[range]
    return .forLoop start stop step init f

  if e.isAppOfArity ``ite 5 then
    return .ite (e.getRevArg! 3) (e.getRevArg! 1) (e.getRevArg! 0)

  if e.isAppOf ``oclFunction then
    let args := e.getAppArgs
    let name ← unsafe evalExpr String q(String) args[2]!
    let kind ← unsafe evalExpr OpenCLFunction.FunKind q(OpenCLFunction.FunKind) args[3]!
    let oclFun := { name, kind : OCLFunction }
    return .oclFunction oclFun args[4:].toArray

  if e.isAppOfArity ``pure 4 ||
     e.isAppOfArity ``ForInStep.yield 2 then
     return .value e.appArg!

  -- keep implicit arguments on the function
  let (fn, args) := e.withApp (fun fn args => (fn,args))
  let info ← getFunInfo fn
  let implicitParamInfo := info.paramInfo.takeWhile (fun info => !info.isExplicit)

  let fn := fn.beta args[0:implicitParamInfo.size]
  let args := args[implicitParamInfo.size:]
  return .app fn args


def getFunctionOrCompile (fn : Expr) : CompileM OCLFunction := do
  if let some oclFun ← getOpenCLFunction? fn false then
    trace[HouLean.OpenCL.compiler] "✓ Found existing OpenCL function for {fn}"
    return oclFun
  else
    trace[HouLean.OpenCL.compiler] "Compiling new function: {fn}"
    let _ ← compileFunction' fn
    if let some oclFun ← getOpenCLFunction? fn false then
      trace[HouLean.OpenCL.compiler] "✓ Successfully compiled {fn}"
      return oclFun
    else
      trace[HouLean.OpenCL.compiler] "✗ Failed to compile {fn}"
      throwError "Failed to compiler {fn}"

def mkUniqueName (name : String) (cont : String → CompileM α) : CompileM α :=
  fun ctx => do
    let (name, ctx) :=
      if let some count := ctx.usedNames[name]? then
        (s!"{name}{count}",
         {ctx with usedNames := ctx.usedNames.insert name (count+1)})
      else
        (name,
         {ctx with usedNames := ctx.usedNames.insert name 1})
    cont name ctx

def withMutVar (name : Name) (type : OCLType) (val : CodeExpr)
    (cont : String → CompileM α) : CompileM α := do
  let name := nameToString name
  mkUniqueName name fun name =>do
  addStatement (.letE name type val)
  cont name

mutual
@[specialize]
partial def compile (e : Expr) (cont : Expr → CodeExpr → CompileM α) (runSimp := true) : CompileM α := do
  trace[HouLean.OpenCL.compiler] "Compiling function body: {e}"

  if let some val ← runInterpreter e then
    return ← cont e (.lit val)

  match e with
  | .letE name type val body _ =>
    -- 1. inline let if type is not OpenGL type
    -- 2. let val ← simplifyExpr val

    compile val fun val' codeVal => do
    if val'.isFVar then
      compile (body.instantiate1 val') cont
    else
      withLetVar name val' codeVal fun var => do
      compile (body.instantiate1 var) cont

  | .app .. =>

    match ← appCase e with
    | .app fn args =>
      trace[HouLean.OpenCL.compiler] m!"application case"
      if runSimp then
        let e ← simplifyExpr e
        compile e cont (runSimp := false)
      else
        compileMany args fun vals oclVals => do
        let oclFun ← getFunctionOrCompile fn
        cont (fn.beta vals) (.app oclFun oclVals)

    | .oclFunction oclFun args =>
      compileMany args fun vals oclVals => do
      cont e (.app oclFun oclVals)

    | .value val =>
      compile val fun val' oclVal => do
        cont val' oclVal

    | .bind mx f =>
      trace[HouLean.OpenCL.compiler] m!"bind case"
      let .lam name t body _ := f | throwError m!"Bug in {decl_name%}, invalid bind case"
      compile mx fun mx' codeVal => do
      if mx'.isAppOfArity ``pure 4 &&
         mx'.appArg!.isFVar then
         compile (body.instantiate1 mx'.appArg!) cont
      else
        withLocalVar name t codeVal fun var => do
         compile (body.instantiate1 var) cont


    | .forLoop start stop step init f => do
      trace[HouLean.OpenCL.compiler] m!"for loop case"
      let initOclType ← getOpenCLType (← inferType init)
      compile init fun _ initCode => do
      compile start fun _ startCode => do
      compile stop fun _ stopCode => do
      compile step fun _ stepCode => do

      -- initialize mutable variable with a value and a name
      -- returns an unique name we can use later
      withMutVar `state initOclType initCode fun stateName => do

      -- stash outer scope statements
      let stmts := (← get).statements
      modify (fun s => {s with statements := #[]})

      let loop ←
        forallTelescope (← inferType f) fun xs _ => do
        -- introduce inly the index with `withFVars`
        withFVars #[xs[0]!] fun varNames => do
        -- the state variable is attached to the mutable variable introduced before
        withReader (fun ctx => {ctx with fvarMap := ctx.fvarMap.insert xs[1]! stateName}) do
        let idxName := varNames[0]!
        let body := f.beta xs
        compile body fun _ retValueCode => do
        -- motate `stateName` variable with the return value
        addStatement (.assignment stateName retValueCode)
        let loopBody := (← get).statements
        return .forLoop idxName startCode stopCode stepCode loopBody

      -- recover previous statement
      modify (fun s => {s with statements := stmts})
      addStatement loop

      -- introduce new free variable and attach loop state variable `stateName` to it
      -- it is local(not let) var as we do not want to accidentally reduce it in to consequent code
      withLocalDeclD `loop (← inferType e) fun loopVar => do
      withReader (fun ctx => { ctx with fvarMap := ctx.fvarMap.insert loopVar stateName}) do

      cont loopVar (.fvar stateName)

    | _ =>
      throwError "hoho"

  | .fvar .. =>
    let some varName := (← read).fvarMap[e]?
      | throwError m!"Unrecognized free variable: {e}\n  Available variables: {(← read).fvarMap.toArray.map (·.1)}"
    trace[HouLean.OpenCL.compiler] "Resolved free variable {e} → {varName}"
    cont e (.fvar varName)

  | .lit (.natVal val) =>
    trace[HouLean.OpenCL.compiler] "Nat literal: {val}"
    cont e (.lit (toString val))

  | .lit (.strVal val) =>
    trace[HouLean.OpenCL.compiler] "String literal: {val}"
    cont e (.lit val)

  | .const .. =>
    throwError m!"unknown const, {e}"

  | _ => throwError m!"asdf, {e}, ctor name {e.ctorName}"

partial def compileMany (es : Array Expr) (cont : Array Expr → Array CodeExpr → CompileM α) :
    CompileM α := do
  go es.toList cont (.emptyWithCapacity es.size) (.emptyWithCapacity es.size)
where
  go (es : List Expr) (cont : Array Expr → Array CodeExpr → CompileM α)
     (vs : Array Expr := #[]) (cs : Array CodeExpr := #[]) := do
    match es with
    | [] => cont vs cs
    | e :: es => compile e fun v c => go es cont (vs.push v) (cs.push c)
end


def compileExpr' (e : Expr) : CompileM (Array CodeStatement) := do
  let stmts ← compile e fun _ c => do
    addStatement (.ret c)
    return (← get).statements
  return stmts


partial def compileFunBody (e : Expr) : CompileM CodeBody := do
  trace[HouLean.OpenCL.compiler] "Compiling function body: {e}"

  match e with
  | .letE name type val body nondep =>
    trace[HouLean.OpenCL.compiler] "Let binding: {name} : {type}"

    -- Simplify the value and check if it introduces nested lets
    let val ← simplifyExpr val
    if val.isLet then
      trace[HouLean.OpenCL.compiler] "Flattening nested let bindings in {name}"
      return ← letTelescope val fun xs valbody => do
        let e' ← mkLetFVars xs (.letE name type valbody body nondep)
        compileFunBody e'

    let valueCode ← compileExpr val

    withLetDecl name type val fun var => do
      let some oclType ← getOpenCLType? type
        | throwError m!"Type {type} is not an OpenCL type\n  In let binding: {name}\n  Full expression: {e}"

      let body := body.instantiate1 var
      withFVars #[var] fun names => do
        trace[HouLean.OpenCL.compiler] "Created let binding: {names[0]!} : {oclType.name}"
        let bodyCode ← compileFunBody body
        return .letE names[0]! oclType valueCode bodyCode

  | .app .. =>

    -- when there is a bind this accidentally runs simplifier on the rest of the whole
    -- program which is likaly undesirable!
    let e ← simplifyExpr e

    if e.isLet then
      return ← compileFunBody e

    -- Handle monadic bind
    if e.isAppOfArity ``bind 6 then
      trace[HouLean.OpenCL.compiler] "Compiling monadic bind"

      let mx := e.appFn!.appArg!

      -- if mx.isAppOf ``forIn then
      --   let f := mx.appArg!
      --   let init := mx.appFn!.appArg!
      --   let initType ← getOpenCLType (← inferType init)
      --   let init ← compileExpr init
      --   return ← forallTelescope (← inferType f) fun xs _ => do
      --     let loopBody := f.beta xs
      --     withFVars xs fun varName => do
      --       let body ← compileFunBody loopBody
      --       logInfo m!"loop body:\n{← body.toString "  "}"

      --       return .letE varName[1]! initType init (CodeBody.forLoop varName[0]! body)

      let valueCode ← compileExpr mx

      let f := e.appArg!
      return ← forallBoundedTelescope (← inferType f) (some 1) fun xs _ => do
        let type ← inferType xs[0]!
        let some oclType ← getOpenCLType? type
          | throwError m!"Type {type} is not an OpenCL type\n  In monadic bind\n  Full expression: {e}"

        let body := f.beta xs
        withFVars xs fun names => do
          trace[HouLean.OpenCL.compiler] "Monadic bind variable: {names[0]!} : {oclType.name}"
          let bodyCode ← compileFunBody body
          return .letE names[0]! oclType valueCode bodyCode

    -- Handle monadic bind
    if e.isAppOfArity ``ite 5 then
      let cond ← mkAppOptM ``decide #[e.getArg! 1, none]
      let tr := e.getArg! 3
      let el := e.getArg! 4
      let cond ← compileExpr cond
      let tr ← compileFunBody tr
      let el ← compileFunBody el
      return .ite cond tr el

    if e.isAppOfArity ``pure 4 then
      let e := e.appArg!
      if e.isAppOfArity ``ForInStep.yield 2 then
        let e := e.appArg!
        let returnValue ← compileExpr e
        return .ret returnValue
      let returnValue ← compileExpr e
      return .ret returnValue


    -- Terminal case: return value
    trace[HouLean.OpenCL.compiler] "Return statement"
    let returnValue ← compileExpr e
    return .ret returnValue

  | .fvar .. =>
    let returnValue ← compileExpr e
    return .ret returnValue
  | .const .. =>
    let returnValue ← compileExpr e
    return .ret returnValue
  | _ =>
    throwError m!"Cannot compile body expression of type {e.ctorName}: {e}"


/-- Function name overrides for better readability in generated code -/
def funNameOverrideMap : NameMap Name :=
  ({} : NameMap Name)
    -- |>.insert ``HMul.hMul `mul
    -- |>.insert ``HAdd.hAdd `add

def mangleFunName (funName : Name) (info : FunInfo) (args : Array Expr) : MetaM String := do
  let mut typeSuffix := ""

  trace[HouLean.OpenCL.compiler] "Mangling function name: {funName}"
  trace[HouLean.OpenCL.compiler] "  Arguments: {args.size}"

  for arg in args, paramInfo in info.paramInfo do
    if paramInfo.isExplicit then
      continue

    let argType ← inferType arg

    if (← isClass? argType).isSome then
      -- Skip typeclasses in mangled name
      trace[HouLean.OpenCL.compiler] "  Skipping typeclass argument: {argType}"
      continue
    else if argType.isType then
      let oclType ← getOpenCLType arg
      typeSuffix := typeSuffix ++ oclType.shortName
      trace[HouLean.OpenCL.compiler] "  Type argument: {arg} → {oclType.shortName}"
    else if ← isDefEq argType q(Nat) then
      let n ← unsafe evalExpr Nat q(Nat) arg
      typeSuffix := typeSuffix ++ toString n
      trace[HouLean.OpenCL.compiler] "  Nat argument: {n}"
    else
      throwError m!"Cannot mangle function name with implicit argument of type: {argType}\n  Argument: {arg}"

  let funName := funName.eraseMacroScopes
  let funName := funNameOverrideMap.get? funName |>.getD funName

  let mut mangledName := toString funName.eraseMacroScopes |>.replace "." "_" |>.toLower
  if typeSuffix != "" then
    mangledName := mangledName ++ "_" ++ typeSuffix

  trace[HouLean.OpenCL.compiler] "  Result: {funName} → {mangledName}"
  return mangledName


instance : MonadRecDepth CompileM where
  withRecDepth n x := fun ctx s => MonadRecDepth.withRecDepth n (x ctx s)
  getRecDepth := liftM (m:=SimpM) <| MonadRecDepth.getRecDepth
  getMaxRecDepth := liftM (m:=SimpM) <| MonadRecDepth.getMaxRecDepth


def compileFunctionCore (f : Expr) : CompileM CodeFunction := do
  withIncRecDepth do
  withTraceNode `HouLean.OpenCL.compiler
    (fun r => do
      match r with
      | .ok c => return m!"[{checkEmoji}] Compiling function: {f}\n{← c.toString}"
      | .error m => return m!"[{crossEmoji}] Compiling function: {f}\n  Error: {m.toMessageData}") do

  forallTelescope (← inferType f) fun xs returnType => do
    let body ← whnfC (f.beta xs)

    let (fn, args) := body.withApp (fun fn args => (fn, args))
    let .const funName _ := fn
      | throwError "Expected constant function head, got: {fn}\n  In body: {body}"

    trace[HouLean.OpenCL.compiler] "Function name: {funName}"
    trace[HouLean.OpenCL.compiler] "Return type: {returnType}"

    let funInfo ← getFunInfo fn
    let mangledName ← mangleFunName funName funInfo args

    -- Unfold definition and reduce instances
    let body := (← unfold body funName).expr
    let body ← whnfC body
    trace[HouLean.OpenCL.compiler] "After unfolding:\n{body}"

    let returnType ← getOpenCLType returnType

    -- Filter out proof arguments
    let xs' ← xs.filterM (fun x => do
      let xTypeTy ← liftM (inferType x >>= inferType)
      return !xTypeTy.isProp)

    trace[HouLean.OpenCL.compiler] "Function parameters: {xs'.size} (filtered from {xs.size})"

    let argTypes ← liftM <| xs'.mapM inferType >>= (·.mapM getOpenCLType)

    let go : CompileM CodeFunction :=
      withFVars xs' fun argNames => do
        trace[HouLean.OpenCL.compiler] "Compiling body with arguments: {argNames}"

        let bodyCode ← compileFunBody body

        let codeFunc := {
          name := mangledName
          args := argTypes.zip argNames
          body := bodyCode
          returnType := returnType
        }

        return codeFunc

    let code ← go
    modify (fun s => {s with compiledFunctions := s.compiledFunctions.push code})

    let codeStr ← code.toString
    trace[HouLean.OpenCL.compiler] "✓ Successfully compiled function: {mangledName}"
    trace[HouLean.OpenCL.compiler] "Generated code:\n{codeStr}"

    addOpenCLFunction f mangledName .normal codeStr
    return code


def compileFunctionCore' (f : Expr) : CompileM CodeFunction' := do
  withIncRecDepth do
  withTraceNode `HouLean.OpenCL.compiler
    (fun r => do
      match r with
      | .ok c => return m!"[{checkEmoji}] Compiling function: {f}\n{← c.toString}"
      | .error m => return m!"[{crossEmoji}] Compiling function: {f}\n  Error: {m.toMessageData}") do

  forallTelescope (← inferType f) fun xs returnType => do
    let body ← whnfC (f.beta xs)

    let (fn, args) := body.withApp (fun fn args => (fn, args))
    let .const funName _ := fn
      | throwError "Expected constant function head, got: {fn}\n  In body: {body}"

    trace[HouLean.OpenCL.compiler] "Function name: {funName}"
    trace[HouLean.OpenCL.compiler] "Return type: {returnType}"

    let funInfo ← getFunInfo fn
    let mangledName ← mangleFunName funName funInfo args

    -- Unfold definition and reduce instances
    let body := (← unfold body funName).expr
    let body ← whnfC body
    trace[HouLean.OpenCL.compiler] "After unfolding:\n{body}"

    let returnType ← getOpenCLType returnType

    -- Filter out proof arguments
    let xs' ← xs.filterM (fun x => do
      let xTypeTy ← liftM (inferType x >>= inferType)
      return !xTypeTy.isProp)

    trace[HouLean.OpenCL.compiler] "Function parameters: {xs'.size} (filtered from {xs.size})"

    let argTypes ← liftM <| xs'.mapM inferType >>= (·.mapM getOpenCLType)

    let go : CompileM CodeFunction' :=
      withFVars xs' fun argNames => do
        trace[HouLean.OpenCL.compiler] "Compiling body with arguments: {argNames}"

        let bodyCode : Array CodeStatement ← compile body fun retVal retCode => do
          addStatement (.ret retCode)
          return (← get).statements

        let codeFunc := {
          name := mangledName
          args := argTypes.zip argNames
          body := bodyCode
          returnType := returnType
        }

        return codeFunc

    let (code, _) ← go {} {}
    -- modify (fun s => {s with compiledFunctions := s.compiledFunctions.push code})

    let codeStr ← code.toString
    trace[HouLean.OpenCL.compiler] "✓ Successfully compiled function: {mangledName}"
    trace[HouLean.OpenCL.compiler] "Generated code:\n{codeStr}"

    addOpenCLFunction f mangledName .normal codeStr
    return code

initialize compileFunctionRef.set compileFunctionCore
initialize compileFunctionRef'.set compileFunctionCore'

def getOpenCLTheorems : MetaM SimpTheorems := do
  let ext ← Lean.Meta.getSimpExtension? `opencl_csimp
  match ext with
  | none => throwError "Simp attribute `opencl_csimp` not found"
  | some ext => ext.getTheorems


def compileLambda (e : Expr) : CompileM CodeFunction := do

  forallTelescope (← inferType e) fun xs returnType => do
    let body := e.beta xs
    let returnType ← getOpenCLType returnType
    let argTypes ← liftM <| (xs.mapM inferType) >>= (·.mapM getOpenCLType)

    withFVars xs fun argNames => do
      let compiledBody ← compileFunBody body

      return {
        name := "(anonymous)"
        args := argTypes.zip argNames
        returnType
        body := compiledBody
      }

def compileLambda' (e : Expr) : CompileM CodeFunction' := do

  forallTelescope (← inferType e) fun xs returnType => do
    let body := e.beta xs
    let returnType ← getOpenCLType returnType
    let argTypes ← liftM <| (xs.mapM inferType) >>= (·.mapM getOpenCLType)

    withFVars xs fun argNames => do
      let compiledBody ← compile body fun _retVal retCode => do
        addStatement (.ret retCode)
        return (← get).statements


      return {
        name := "(anonymous)"
        args := argTypes.zip argNames
        returnType
        body := compiledBody
      }



def compileExpr'' (e : Expr) : MetaM (Array CodeStatement) := do

  let simpMthds := Simp.mkDefaultMethodsCore #[]
  let simpCtx : Simp.Context ← Simp.mkContext
    (config := {zetaDelta := false, zeta := false, iota := false})
    (simpTheorems := #[← getOpenCLTheorems])
  let simpState : Simp.State := {}
  let ctx : Context := {}
  let state : State := {}

  let ((r, _), _) ← compileExpr' e ctx state simpMthds.toMethodsRef simpCtx |>.run simpState
  return r

syntax "#opencl_compile" term : command
open Elab Term Command in
elab_rules : command
| `(#opencl_compile%$tk $fstx:term) => runTermElabM fun _ => Term.withDeclName `_opencl_compile do
  let f ← elabTermAndSynthesize fstx none
  -- Term.synthesizeSyntheticMVarsNoPostponing

  if f.hasMVar then
    logErrorAt tk m!"Can't compile, expression has metavariables!"
    return

  let simpMthds := Simp.mkDefaultMethodsCore #[]
  let simpCtx : Simp.Context ← Simp.mkContext
    (config := {zetaDelta := false, zeta := false, iota := false})
    (simpTheorems := #[← getOpenCLTheorems])
  let simpState : Simp.State := {}
  let ctx : Context := {}
  let state : State := {}

  let ((r, s), _) ← compileLambda f ctx state simpMthds.toMethodsRef simpCtx |>.run simpState

  let msgs ← (s.compiledFunctions.push r).mapM (fun c => c.toString)
  let msg := msgs.joinl (map := id) (· ++ "\n\n" ++ ·)
  logInfoAt tk m!"\n{msg}"


syntax "#opencl_compile'" term : command
open Elab Term Command in
elab_rules : command
| `(#opencl_compile'%$tk $fstx:term) => runTermElabM fun _ => Term.withDeclName `_opencl_compile do
  let f ← elabTermAndSynthesize fstx none
  -- Term.synthesizeSyntheticMVarsNoPostponing

  if f.hasMVar then
    logErrorAt tk m!"Can't compile, expression has metavariables!"
    return

  let simpMthds := Simp.mkDefaultMethodsCore #[]
  let simpCtx : Simp.Context ← Simp.mkContext
    (config := {zetaDelta := false, zeta := false, iota := false})
    (simpTheorems := #[← getOpenCLTheorems])
  let simpState : Simp.State := {}
  let ctx : Context := {}
  let state : State := {}

  let ((r, s), _) ← compileLambda' f ctx state simpMthds.toMethodsRef simpCtx |>.run simpState
  pure ()
  -- let msgs ← (s.compiledFunctions.push r).mapM (fun c => c.toString)
  -- let msg := msgs.joinl (map := id) (· ++ "\n\n" ++ ·)
  -- logInfoAt tk m!""


open Lean Elab Command Term Meta in
elab "#opencl_generate_function_variants" _module:str varArg:num f:term : command => do
  liftTermElabM do
  let f ← elabTermAndSynthesize f none

  let varArg := varArg.getNat
  forallBoundedTelescope (← inferType f) varArg fun xs _ => do

    let mut variants : Array (Array Expr) := #[]

    for x in xs do
      let t ← inferType x

      let vars : Array Expr ← show MetaM _ from do
        if ← isDefEq t q(Type) then
          pure #[q(UInt16), q(UInt32), q(UInt64),
                 q(Int16), q(Int32), q(Int64),
                 q(Float32), q(Float64)]
        else if ← isDefEq t q(Nat) then
          pure #[q(2), q(3), q(4)] --, q(8), q(16)]
        else
          throwError "Unrecognized variant type {t}"

      if variants.size = 0 then
        variants := vars.map (#[·])
      else
        variants := vars.map (fun v => variants.map (·.push v)) |>.flatten

    let mut generatedFunctions : Array String := #[]
    for var in variants do

      -- todo: we need to consume all class arguments here!!!
      let f ← mkAppOptM' f ((var.map some) ++ #[none])

      let simpMthds := Simp.mkDefaultMethodsCore #[]
      let simpCtx : Simp.Context ← Simp.mkContext
        (config := {zetaDelta := false, zeta := false, iota := true})
        (simpTheorems := #[← getOpenCLTheorems])
      let simpState : Simp.State := {}
      let ctx : Compiler.Context := {}
      let state : Compiler.State := {}

      let ((u, _), _) ← compileFunction f ctx state simpMthds.toMethodsRef simpCtx |>.run simpState

      generatedFunctions := generatedFunctions.push u.name

    logInfo m!"Generated functions: {generatedFunctions}"
  pure ()
