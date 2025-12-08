import HouLean.OpenCL.Compiler.Types
import HouLean.Meta.Float

open Lean Meta Qq HouLean

namespace HouLean.OpenCL.Compiler

instance : MonadRecDepth CompileM where
  withRecDepth n x := fun ctx s => MonadRecDepth.withRecDepth n (x ctx s)
  getRecDepth := liftM (m := SimpM) <| MonadRecDepth.getRecDepth
  getMaxRecDepth := liftM (m := SimpM) <| MonadRecDepth.getMaxRecDepth


/--
Introduce free variables into the compilation context with unique names.
Ensures name uniqueness by appending counters for duplicates.

**Parameters:**
- `xs`: Array of expressions representing free variables
- `go`: Continuation that receives the unique string names

**Example:**
```lean
withFVars #[x, y, z] fun names => do
  -- names = #["x", "y", "z"] or #["x1", "y", "z"] if x was already used
```
-/
def withFVars (xs : Array Expr) (go : Array String → CompileM α) : CompileM α := do
  let names ← xs.mapM (fun x => nameToString <$> x.fvarId!.getUserName)
  fun ctx => do
  -- Ensure unique names by appending counters for duplicates
  let (usedNames, names) := names.foldl (init := (ctx.usedNames, #[])) (fun (un, ns) n =>
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


partial def splitList (list : Expr) : MetaM (Array Expr) :=
  go list #[]
where
  go (l : Expr) (xs : Array Expr) : MetaM (Array Expr) := do
    if l.isAppOf ``List.nil then
      return xs
    else if l.isAppOfArity ``List.cons 3 then
      let head := l.getArg! 1
      let tail := l.getArg! 2
      go tail (xs.push head)
    else if l.isAppOfArity ``List.ofFn 3 then
      let n ← unsafe evalExpr Nat q(Nat) (l.getArg! 1)
      let f := l.getArg! 2
      Array.ofFnM (n:=n) fun i =>
        let n : Q(Nat) := mkNatLit n
        let i : Q(Nat) := mkNatLit i.1
        pure (f.beta #[q(⟨$i, sorry_proof⟩ : Fin $n)])
    else
      throwError m!"Can't split argument list {list}!"



/--
Look up or compile an OpenCL function from an expression.
Returns the OCLFunction info along with the function and its arguments.

**Parameters:**
- `e`: Expression potentially representing an OpenCL function application
- `doWhnf`: Whether to reduce to weak head normal form first (default: false)

**Returns:**
- `some { oclFun, fn, args }` if an OpenCL function is found or successfully compiled
- `none` if compilation fails

**Behavior:**
1. Checks if expression is already marked as `oclFunction`
2. Filters out proof arguments
3. Looks up existing compiled function
4. If not found, attempts to compile the function
-/
def getOpenCLAppOrCompile? (e : Expr) (doWhnf := false) :
    CompileM (Option struct { oclFun : OCLFunction, fn : Expr, args : Array Expr }) := do
  withTraceNode `HouLean.OpenCL.compiler
    (fun r => do
      match r with
      | .ok (some _) => return m!"✓ Found/compiled OpenCL function for: {e}"
      | .ok none => return m!"✗ Failed to compile: {e}"
      | .error _ => return m!"✗ Error processing: {e}") do

  let mut e := e
  if doWhnf then
    e ← whnfC e

  if e.isAppOf ``oclFunction then
    try
      let args := e.getAppArgs
      let name ← unsafe evalExpr String q(String) args[2]!
      let kind ← unsafe evalExpr OpenCLFunction.FunKind q(OpenCLFunction.FunKind) args[3]!

      let args : Array Expr := args[4:]

      if 0 < args.size then
        if args[0]!.isAppOf ``argList then
          let args ← splitList args[0]!.appArg!
          return some {
            oclFun := { name, kind }
            fn := e.stripArgsN 1
            args := args
          }

      return some {
        oclFun := { name, kind }
        fn := e.stripArgsN args.size
        args := args
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

  if let some oclFun ← getOpenCLFunction? fn false then
    return some { oclFun, fn, args }
  else
    let _ ← compileFunction fn
    if let some oclFun ← getOpenCLFunction? fn false then
      return some { oclFun, fn, args }
    else
      return none

/--
Attempt to interpret an expression at compile time to produce a constant value.
Supports various numeric types, strings, and basic Lean types.

**Parameters:**
- `e`: Expression to interpret

**Returns:**
- `some str` where `str` is the string representation of the constant value
- `none` if interpretation fails or type is not supported

**Supported types:**
- Integer types: Int16, Int32, Int64, Int, UInt16, UInt32, UInt64
- Floating point: Float32 (with 'f' suffix), Float64 (with 'd' suffix)
- Other: Nat, String
-/
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

    if (← isDefEq type q(Float32)) then
      let val ← unsafe evalExpr Float32 q(Float32) e
      return some (Float.toString' val.toFloat ++ "f")

    if (← isDefEq type q(Float64)) then
      let val ← unsafe evalExpr Float64 q(Float64) e
      return some (Float.toString' val ++ "d")

    if (← isDefEq type q(Nat)) then
      let val ← unsafe evalExpr Nat q(Nat) e
      return some (toString val)

    if (← isDefEq type q(String)) then
      let val ← unsafe evalExpr String q(String) e
      return some (toString val)

  catch _ =>
    return none

  return none

/--
Simplify an expression using simp and lift lets.
Traces the simplification if the expression changes.

**Parameters:**
- `e`: Expression to simplify

**Returns:**
- Simplified expression
-/
def simplifyExpr (e : Expr) : CompileM Expr := do
  let e' := e
  let e := (← Simp.simp e).expr
  -- let e ← Meta.liftLets e
  if e != e' then
    trace[HouLean.OpenCL.compiler] m!"simplified\n{e'}\n==>\n{e}"
  return e

/-- Add a code statement to the current compilation scope -/
def addStatement (stmt : CodeStatement) : CompileM Unit :=
  modify (fun s => {s with statements := s.statements.push stmt})

/--
Introduce a let-bound variable in both Lean and OpenCL contexts.

**Lean level:** Equivalent to `withLetDeclD name type val cont`
**OpenCL level:** Introduces a new variable with the same name and value `oclVal`

**Parameters:**
- `name`: Variable name
- `val`: Lean expression value
- `oclVal`: OpenCL code expression value
- `cont`: Continuation receiving the new Lean variable

**Example:**
```lean
withLetVar `x expr (.lit "42") fun x => do
  -- x is now available as a Lean variable
  -- OpenCL code has: "float x = 42;"
```
-/
def withLetVar (name : Name) (val : Expr) (oclVal : CodeExpr)
    (cont : Expr → CompileM α) : CompileM α := do
  let type ← inferType val
  withLetDecl name type val fun var => do
    withFVars #[var] fun varName => do
      let oclType ← getOpenCLType type
      addStatement (.letE varName[0]! oclType oclVal)
      cont var

/--
Introduce a local variable (not let-bound) in both Lean and OpenCL contexts.

**Lean level:** Equivalent to `withLocalDeclD name type cont`
**OpenCL level:** Introduces a new variable with the same name and value `oclVal`

**Parameters:**
- `name`: Variable name
- `type`: Variable type
- `oclVal`: OpenCL code expression value
- `cont`: Continuation receiving the new Lean variable

**Note:** Uses local declaration to prevent accidental reduction in subsequent code.
-/
def withLocalVar (name : Name) (type : Expr) (oclVal : CodeExpr)
    (cont : Expr → CompileM α) : CompileM α :=
  withLocalDeclD name type fun var => do
    withFVars #[var] fun varName => do
      let oclType ← getOpenCLType type
      addStatement (.letE varName[0]! oclType oclVal)
      cont var

/--
Eta-expand an expression by one argument.

**Example:**
```lean
f : α → β
etaExpand1 f = fun x => f x
```
-/
def etaExpand1 (e : Expr) : MetaM Expr := do
  forallBoundedTelescope (← inferType e) (some 1) fun xs _ => do
    mkLambdaFVars xs (e.beta xs)

/--
Classification of application expressions for compilation.

**Variants:**
- `bind`: Monadic bind operation (mx >>= f)
- `forLoop`: For-in loop with range (start, stop, step, init, body)
- `ite`: If-then-else conditional
- `app`: General function application
- `value`: Pure value (wrapped in pure/yield)
-/
inductive AppCase where
  | bind (mx f : Expr)
  | forLoop (start stop step init f : Expr)
  | ite (cond t e : Expr)
  | app
  | value (e : Expr)

/--
Classify an application expression to determine how to compile it.

**Parameters:**
- `e`: Expression to classify

**Returns:**
- `AppCase` variant describing the structure of the expression

**Recognition patterns:**
- Recognizes `bind`, `forIn`, `ite`, `pure`, and `ForInStep.yield`
- Falls back to general `.app` case for other applications
-/
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

  if e.isAppOfArity ``pure 4 ||
     e.isAppOfArity ``ForInStep.yield 2 then
     return .value e.appArg!

  return .app

/--
Generate a unique name by appending a counter if the name is already used.

**Parameters:**
- `name`: Base name to make unique
- `cont`: Continuation receiving the unique name

**Example:**
If "x" is already used twice, generates "x2".
-/
def mkUniqueName (name : String) (cont : String → CompileM α) : CompileM α :=
  fun ctx => do
    let (name, ctx) :=
      if let some count := ctx.usedNames[name]? then
        (s!"{name}{count}",
         {ctx with usedNames := ctx.usedNames.insert name (count + 1)})
      else
        (name,
         {ctx with usedNames := ctx.usedNames.insert name 1})
    cont name ctx

/--
Introduce a mutable variable in OpenCL context.

**Parameters:**
- `name`: Base variable name
- `type`: OpenCL type
- `val`: Initial value expression
- `cont`: Continuation receiving the unique variable name

**Emits:** Let statement in OpenCL code declaring the mutable variable
-/
def withMutVar (name : Name) (type : OCLType) (val : CodeExpr)
    (cont : String → CompileM α) : CompileM α := do
  let name := nameToString name
  mkUniqueName name fun name => do
  addStatement (.letE name type val)
  cont name

mutual

/--
Main compilation function that converts a Lean expression to OpenCL code.

**Parameters:**
- `e`: Lean expression to compile
- `cont`: Continuation receiving the compiled expression (both Lean and OpenCL forms)
- `runSimp`: Whether to run simplification (default: true)

**Compilation strategy:**
1. Try interpreting as constant value
2. Handle let-bindings
3. Handle applications (bind, forLoop, ite, general app)
4. Handle free variables, literals, and constants

**Expression cases:**
- `.letE`: Compile value, introduce variable, compile body
- `.app`:
  - `.bind`: Monadic bind - compile mx, introduce binding variable, compile continuation
  - `.forLoop`: Generate OpenCL for-loop with mutable state variable
  - `.ite`: Conditional expression
  - `.app`: Look up/compile OpenCL function and compile arguments
  - `.value`: Extract and compile the wrapped value
- `.fvar`: Look up variable name in context
- `.lit`: Handle Nat and String literals
- `.const`: Throw error (unexpected at this stage)

**Throws:** Error if expression cannot be compiled
-/
@[specialize]
partial def compile (e : Expr) (cont : Expr → CodeExpr → CompileM Unit)
    (runSimp := true) : CompileM Unit := do
  withIncRecDepth do
  if let some val ← runInterpreter e then
    return ← cont e (.lit val)

  match e with
  | .letE name type val body _ =>


    if val.isFVar || type.isForall then
      trace[HouLean.OpenCL.compiler] m!"inlining let binding `{name}`"
      compile (body.instantiate1 val) cont
    else
      trace[HouLean.OpenCL.compiler] m!"compiling let binding `{name}`"
      compile val fun val' codeVal => do
        withLetVar name val' codeVal fun var => do
        compile (body.instantiate1 var) cont

  | .app .. =>
    match ← appCase e with
    | .app =>
      if runSimp then
        let e ← simplifyExpr e
        compile e cont (runSimp := false)
      else
        withTraceNode `HouLean.OpenCL.compiler
          (fun r => return m!"[{exceptEmoji r}]Compiling: {e}") do
          if let some r ← getOpenCLAppOrCompile? e then
            compileMany r.args fun _vals oclVals => do
            -- this break if we have `argList` then the arity of fn does not match args
            -- cont (r.fn.beta vals) (.app r.oclFun oclVals)
            cont e (.app r.oclFun oclVals)
          else
            throwError "No OpenCL function for {e}"

    | .value val =>
      compile val fun val' oclVal => do
        cont val' oclVal

    | .bind mx f =>
      let .lam name t body _ := f
        | throwError "Invalid bind: expected lambda, got {f}"
      compile mx fun mx' codeVal => do
      if mx'.isAppOfArity ``pure 4 && mx'.appArg!.isFVar then
         compile (body.instantiate1 mx'.appArg!) cont
      else
        withLocalVar name t codeVal fun var => do
         compile (body.instantiate1 var) cont

    | .forLoop start stop step init f => do
      let initOclType ← getOpenCLType (← inferType init)

      withTraceNode `HouLean.OpenCL.compiler
        (fun _ => return m!"Compiling for-loop") do

      compile init fun _ initCode => do
      compile start fun _ startCode => do
      compile stop fun _ stopCode => do
      compile step fun _ stepCode => do

      withMutVar `state initOclType initCode fun stateName => do

      let (idxName,loopBody) ← do
        forallTelescope (← inferType f) fun xs _ => do
        withFVars #[xs[0]!] fun varNames => do
        withReader (fun ctx => {ctx with fvarMap := ctx.fvarMap.insert xs[1]! stateName}) do
        let idxName := varNames[0]!
        let body := f.beta xs
        let loopBody ← compileScope body (fun _ r =>
          addStatement (.assignment stateName r))
        pure (idxName, loopBody)

      addStatement (.forLoop idxName startCode stopCode stepCode loopBody)

      withLocalDeclD `loop (← inferType e) fun loopVar => do
      withReader (fun ctx => { ctx with fvarMap := ctx.fvarMap.insert loopVar stateName}) do
      cont loopVar (.fvar stateName)

    | .ite cond thn els =>
      let decCond ← mkAppOptM ``decide #[cond, none]
      compile decCond fun _ condCode => do
      let thnBody ← compileScope thn cont
      let elseBody ← compileScope els cont
      addStatement (.ite condCode thnBody elseBody)

  | .fvar .. =>
    let some varName := (← read).fvarMap[e]?
      | throwError "Unrecognized free variable: {e}\nAvailable: {(← read).fvarMap.toArray.map (·.1)}"
    cont e (.fvar varName)

  | .lit (.natVal val) =>
    cont e (.lit (toString val))

  | .lit (.strVal val) =>
    cont e (.lit val)


  | .const ``PUnit.unit _ =>
    cont e CodeExpr.errased

  | .const .. =>
    throwError "Unexpected constant: {e}"

  | _ =>
    throwError "Unsupported expression constructor: {e.ctorName}"

/--
Compile multiple expressions in sequence.

**Parameters:**
- `es`: Array of expressions to compile
- `cont`: Continuation receiving arrays of compiled Lean and OpenCL expressions

**Behavior:** Compiles expressions left-to-right, accumulating results.
-/
partial def compileMany (es : Array Expr)
    (cont : Array Expr → Array CodeExpr → CompileM Unit) : CompileM Unit := do
  go es.toList cont (.emptyWithCapacity es.size) (.emptyWithCapacity es.size)
where
  go (es : List Expr) (cont : Array Expr → Array CodeExpr → CompileM Unit)
     (vs : Array Expr := #[]) (cs : Array CodeExpr := #[]) := do
    match es with
    | [] => cont vs cs
    | e :: es => compile e fun v c => go es cont (vs.push v) (cs.push c)

/--
Compile an expression into a code scope (sequence of statements).
Automatically adds a return statement at the end.

**Parameters:**
- `e`: Expression to compile

**Returns:**
- Array of code statements representing the compiled scope
-/
partial def compileScope (e : Expr) (finish : Expr → CodeExpr → CompileM Unit := fun _ c => addStatement (.ret c)) :
    CompileM (Array CodeStatement) := do
  -- stash statements
  let oldStmts := (← get).statements
  modify (fun s => {s with statements := #[]})
  compile e finish
  let stmts := (← get).statements
  -- recover statements
  modify (fun s => {s with statements := oldStmts})
  return stmts


end


private def removeRepeatedSuffix (s : String) : String :=
  let rep := s.takeRightWhile (·!='_')
  if (rep ++ "_" ++ rep) == s.takeRight (2*rep.length+1) then
    s.dropRight (rep.length+1)
  else
    s

/--
Generate a mangled function name incorporating type information.

**Parameters:**
- `funName`: Base function name
- `info`: Function info containing parameter information
- `args`: Function arguments

**Returns:**
- Mangled name string (e.g., `add_float32` or `matrix_mul_3x3`)

**Mangling rules:**
1. Apply name overrides from `funNameOverrideMap`
2. Convert dots to underscores, lowercase
3. Append type suffixes for:
   - Implicit type arguments (using OCLType.shortName)
   - Implicit Nat arguments (using the nat value)
4. Skip typeclass arguments

**Example:**
```
HMul.hMul {α := Float32} → mul_float32
matrix.mul {n := 3} → matrix_mul_3
```
-/
def mangleFunName (funName : Name) (info : FunInfo) (args : Array Expr) : MetaM String := do
  withTraceNode `HouLean.OpenCL.compiler
    (fun r => do
      match r with
      | .ok name => return m!"Mangled name: {funName} → {name}"
      | .error _ => return m!"Failed to mangle: {funName}") do

  let mut typeSuffix := ""

  for arg in args, paramInfo in info.paramInfo do
    if paramInfo.isExplicit then
      continue

    let argType ← inferType arg

    if (← isClass? argType).isSome then
      -- Skip typeclasses in mangled name
      continue
    else if argType.isType then
      let oclType ← getOpenCLType arg
      typeSuffix := typeSuffix ++ oclType.shortName
    else if ← isDefEq argType q(Nat) then
      let n ← unsafe evalExpr Nat q(Nat) arg
      typeSuffix := typeSuffix ++ toString n
    else
      throwError "Cannot mangle function with implicit argument of type: {argType}\nArgument: {arg}"

  let funName := funName.eraseMacroScopes
  let mut mangledName := toString funName.eraseMacroScopes |>.replace "." "_" |>.toLower
  mangledName := removeRepeatedSuffix mangledName
  if typeSuffix != "" then
    mangledName := mangledName ++ "_" ++ typeSuffix

  return mangledName




open Lean Meta Qq in
def unfold' (e : Expr) (declName : Name) : MetaM Expr := do
  let e := (← unfold e declName).expr
  if ← isProjectionFn declName then
    e.withApp fun fn args => do
      let fn := (← reduceProj? fn).getD fn
      return fn.beta args
  else
    return e

/--
Core function compilation routine. Compiles a Lean function to OpenCL code.

**Parameters:**
- `f`: Function expression to compile

**Returns:**
- Compiled `CodeFunction` with name, arguments, body, and return type

**Compilation steps:**
1. Introduce function parameters via `forallTelescope`
2. Beta-reduce function body
3. Extract function head (must be a constant)
4. Generate mangled name
5. Unfold definition and reduce instances
6. Filter out proof arguments
7. Compile function body to statements
8. Register compiled function in global state

**Throws:** Error if function head is not a constant or compilation fails
-/
def compileFunctionCore (f : Expr) : CompileM CodeFunction := do
  withIncRecDepth do
  withTraceNode `HouLean.OpenCL.compiler
    (fun r => do
      match r with
      | .ok c => return m!"{checkEmoji} Compiled function: {f}\n{← c.toString}"
      | .error m => return m!"{crossEmoji} Failed to compile: {f}\n{m.toMessageData}") do

  forallTelescope (← inferType f) fun xs returnType => do
    let body' := f.beta xs
    let body := body'

    let (fn, args) := body.withApp (fun fn args => (fn, args))
    let .const funName _ := fn
      | throwError "Expected constant function head, got: {fn}\nIn body: {body}"

    let funInfo ← getFunInfo fn
    let mangledName ← mangleFunName funName funInfo args

    -- Unfold definition and reduce instances
    let body ← unfold' body funName

    if body == body' then
      throwError m!"Failed to unfold function body {funName}"

    trace[HouLean.OpenCL.compiler] m!"unfolded function body:\n{body'}\n==>\n{body}"

    let returnType ← getOpenCLType returnType

    -- Filter out proof arguments
    let xs' ← xs.filterM (fun x => do
      let xTypeTy ← liftM (inferType x >>= inferType)
      return !xTypeTy.isProp)

    let argTypes ← liftM <| xs'.mapM inferType >>= (·.mapM getOpenCLType)

    let go : CompileM CodeFunction :=
      withFVars xs' fun argNames => do
        let bodyCode ← compileScope body

        let codeFunc := {
          name := mangledName
          args := argTypes.zip argNames
          body := bodyCode
          returnType := returnType
        }

        return codeFunc

    let (code,s') ← go {} {}
    modify (fun s => {s with compiledFunctions := s.compiledFunctions ++ s'.compiledFunctions.push code})

    let codeStr ← code.toString
    addOpenCLFunction f mangledName .normal codeStr
    return code

initialize compileFunctionRef.set compileFunctionCore

/--
Get the OpenCL-specific simplification theorems.
These theorems are registered with the `opencl_csimp` attribute.
-/
def getOpenCLTheorems : MetaM SimpTheorems := do
  let ext ← Lean.Meta.getSimpExtension? `opencl_csimp
  match ext with
  | none => throwError "Simp attribute `opencl_csimp` not found"
  | some ext => ext.getTheorems

def getOpenCLSimprocs : MetaM Simprocs := do
  let ext ← Simp.getSimprocExtension? `opencl_csimp
  match ext with
  | none => throwError "Simp attribute `opencl_csimp` not found"
  | some ext => ext.getSimprocs

/--
Compile a lambda expression to OpenCL code.

**Parameters:**
- `e`: Lambda expression to compile

**Returns:**
- Anonymous `CodeFunction` with compiled body

**Use case:** Compile inline lambda functions or anonymous functions.
-/
def compileLambda (e : Expr) : CompileM CodeFunction := do
  forallTelescope (← inferType e) fun xs returnType => do
    let body := e.beta xs
    let returnType ← getOpenCLType returnType
    let argTypes ← liftM <| (xs.mapM inferType) >>= (·.mapM getOpenCLType)

    withFVars xs fun argNames => do
      let compiledBody ← compileScope body

      return {
        name := "(anonymous)"
        args := argTypes.zip argNames
        returnType
        body := compiledBody
      }

/--
Command to compile and display OpenCL code for a given term.

**Syntax:** `#opencl_compile <term>`

**Example:**
```lean
#opencl_compile fun (x : Float32) => x * 2.0f + 1.0f
```

**Output:** Pretty-printed OpenCL function code
-/
syntax "#opencl_compile" term : command

open Elab Term Command in
elab_rules : command
| `(#opencl_compile%$tk $fstx:term) =>
  runTermElabM fun _ => Term.withDeclName `_opencl_compile do
  let f ← elabTermAndSynthesize fstx none

  if f.hasMVar then
    logErrorAt tk "Can't compile: expression has metavariables"
    return

  let simpMthds := Simp.mkDefaultMethodsCore #[← getOpenCLSimprocs]
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

open Lean Elab Command Term Meta in
/--
Command to generate multiple function variants with different type instantiations.

**Syntax:** `#opencl_generate_function_variants <module> <varArgCount> <function>`

**Parameters:**
- `module`: Module name (currently unused)
- `varArgCount`: Number of type/nat parameters to vary
- `function`: Function to generate variants for

**Behavior:**
Generates all combinations of:
- Scalar types: UInt16, UInt32, UInt64, Int16, Int32, Int64, Float32, Float64
- Dimension sizes: 2, 3, 4

**Example:**
```lean
#opencl_generate_function_variants "MyModule" 1 (@Vector.add)
-- Generates: vector_add_float32, vector_add_float64, vector_add_int32, etc.
```
-/
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
          pure #[q(2), q(3), q(4)]
        else
          throwError "Unrecognized variant type {t}"

      if variants.size = 0 then
        variants := vars.map (#[·])
      else
        variants := vars.map (fun v => variants.map (·.push v)) |>.flatten

    let mut generatedFunctions : Array String := #[]
    for var in variants do
      let f ← mkAppOptM' f ((var.map some) ++ #[none])

      let simpMthds := Simp.mkDefaultMethodsCore #[]
      let simpCtx : Simp.Context ← Simp.mkContext
        (config := {zetaDelta := false, zeta := false, iota := true})
        (simpTheorems := #[← getOpenCLTheorems])
      let simpState : Simp.State := {}
      let ctx : Compiler.Context := {}
      let state : Compiler.State := {}

      let ((u, _), _) ← compileFunction f ctx state simpMthds.toMethodsRef simpCtx
        |>.run simpState

      generatedFunctions := generatedFunctions.push u.name

    logInfo m!"Generated functions: {generatedFunctions}"
  pure ()

end HouLean.OpenCL.Compiler
