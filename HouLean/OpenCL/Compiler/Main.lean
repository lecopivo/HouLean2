import HouLean.OpenCL.Compiler.Types

open Lean Meta Qq HouLean

namespace HouLean.OpenCL.Compiler

def withFVars (xs : Array Expr) (go : Array String → CompileM α) : CompileM α := do
  let names ← xs.mapM (fun x => nameToString <$> x.fvarId!.getUserName)
  -- todo: ensure that names are unique!
  --       we should store the current names that has been bound
  --       and if we want to use already existing name we just bump a counter on it
  fun ctx =>
  let ctx := { ctx with
      fvarMap := (xs.zip names).foldl (init := ctx.fvarMap) (fun m (x,n) => m.insert x n)
    }
  go names ctx

def getOpenCLAppOrCompile? (e : Expr) (doWhnf := false) :
    CompileM (Option struct { oclFun : OCLFunction,
                           fn : Expr,
                           args : Array Expr }) := do
  let mut e := e
  if doWhnf then
    e ← whnfC e
  let (fn, args) := e.withApp fun fn args => (fn,args)
  let info ← getFunInfo fn
  let firstExplicit := info.paramInfo.findIdx (fun p => p.isExplicit)
  let fn := fn.beta (args[0:firstExplicit])
  let args := args[firstExplicit:].toArray

  if let some oclFun ← getOpenCLFunction? fn false then
    return some { oclFun, fn, args }
  else
    trace[HouLean.OpenCL.compiler] s!"Trying to compile {fn}! "
    let _ ← compileFunction fn
    if let some oclFun ← getOpenCLFunction? fn false then
      return some { oclFun, fn, args }
    else
      return none


/-- Replace `e` with `replacement` if there is an instance `ImplementedBy origial replacement`. -/
partial def implementedBy (original : Expr) : MetaM Expr := do
  let type ← inferType original
  let replacement ← mkFreshExprMVar type
  let cls ← mkConstWithFreshMVarLevels ``ImplementedBy
  let cls := mkAppN cls #[type, original, replacement]
  if let some _ ← synthInstance? cls then
    trace[HouLean.OpenCL.compiler] "implemented by {original} ==> {replacement}"
    return ← implementedBy (← instantiateMVars replacement)
  else
    return original


partial def compileExpr (e : Expr) : CompileM CodeExpr := do
  withTraceNode `HouLean.OpenCL.compiler
    (fun r => do
      match r with
      | .ok c =>  return m!"[{checkEmoji}] {e} ==> {← c.toString}"
      | .error m => return m!"[{crossEmoji}] {e}\n{m.toMessageData}") do

  if (← inferType (← inferType e)).isProp then
    return .errased

  let e ← implementedBy e

  let e' := e
  let e ← whnfC e
  if e != e' then
    trace[HouLean.OpenCL.compiler] "Reduced {e'} ==> {e}"

  match e with
  | .app .. =>
    let some ⟨oclFun, _fn, args⟩ ← getOpenCLAppOrCompile? e
      | throwError m!"Failed to find OpenCL iplementation for {e}! {e.getAppFn.ctorName}"
    let args ← args.mapM compileExpr
    return .app oclFun args

  | .fvar .. =>
    let some varName := (← read).fvarMap[e]?
      | throwError m!"Unrecognized free variable {e}! {(← read).fvarMap.toArray}"
    return .fvar varName

  | .letE .. =>
    throwError m!"can't have let binding in an expression {e}"
  | .lit (.natVal val) =>
    return .lit (toString val)
  | .lit (.strVal val) =>
    return .lit val
  | _ =>
    throwError m!"Don't know how to compiler {e}, case {e.ctorName}!"


partial def compileFunBody (e : Expr) : CompileM CodeBody := do
  match e with
  | .letE name type val body _ =>

    -- todo: we have to determine if `val` is compilable
    --       we might have to decompose it and introduce few more let bindings

    let valueCode ← compileExpr val

    withLetDecl name type val fun var => do
      let some oclType ← getOpenCLType? type
        | throwError m!"Not an OpenCL type {type} in\n{e}"
      let body := body.instantiate1 var
      withFVars #[var] fun names => do
        let bodyCode ← compileFunBody body
        trace[HouLean.OpenCL.compiler] "vars after introducing {names}:\n{(← read).fvarMap.toArray}"
        return .letE names[0]! oclType valueCode bodyCode

  | .app .. =>

    if e.isAppOfArity ``bind 6 then

      let mx := e.appFn!.appArg!
      let valueCode ← compileExpr mx

      let f := e.appArg!
      return ← forallBoundedTelescope (← inferType f) (some 1) fun xs _ => do
        let type ← inferType xs[0]!
        let some oclType ← getOpenCLType? type
          | throwError m!"Not an OpenCL type {type} in\n{e}"
        let body := f.beta xs
        withFVars xs fun names => do
          let bodyCode ← compileFunBody body
          return .letE names[0]! oclType valueCode bodyCode

    let returnValue ← compileExpr e
    return .ret returnValue

  | _ => throwError m!"Do not know how to compile {e}"


/-- To keep readability of the resutling code we rename certain functions to more sane names. -/
def funNameOverrideMap : NameMap Name :=
  ({} : NameMap Name)
    |>.insert ``HMul.hMul `mul
    |>.insert ``HAdd.hAdd `add

def mangleFunName (funName : Name) (info : FunInfo) (args : Array Expr) : MetaM String := do
  let mut typeSuffix := ""

  for arg in args, info in info.paramInfo do
    if info.isExplicit then
      continue
    let t ← inferType arg
    if (← isClass? t).isSome then
      -- we do not include typeclasses in mangled function name
      continue
    else if t.isSort then
      let oclType ← getOpenCLType arg
      typeSuffix := typeSuffix ++ oclType.shortName
    else if ← isDefEq t q(Nat) then
      let n ← unsafe evalExpr Nat q(Nat) arg
      typeSuffix := typeSuffix ++ toString n
    else
      throwError m!"don't know how to mangle function name with implicit argument of type {t}"

  let funName := funName.eraseMacroScopes
  let funName := funNameOverrideMap.get? funName |>.getD funName

  let mut r := toString funName.eraseMacroScopes |>.replace "." "_" |>.toLower
  if typeSuffix != "" then
    r := r ++ "_" ++ typeSuffix

  trace[HouLean.OpenCL.compiler] "Mangled function name {funName} ==> {r}"

  return r


def compileFunctionCore (f : Expr) : CompileM CodeFunction := do

  forallTelescope (← inferType f) fun xs returnType => do
    let body ← whnfC (f.beta xs)

    let (fn, args) := body.withApp (fun fn args => (fn,args))
    let .const funName _ := fn
      | throwError "Expected constant head function in {body}!"

    trace[HouLean.OpenCL.compiler] "Compiling {fn}"

    let funInfo ← getFunInfo fn
    let name ← mangleFunName funName funInfo args

    -- unfold definition of the function and basic reduction (mainly to reduce instances)
    let body := (← unfold body funName).expr

    trace[HouLean.OpenCL.compiler] "Compiling {fn}:\n{body}"


    let returnType ← getOpenCLType returnType
    let argTypes ← liftM <| xs.mapM inferType >>= (·.mapM getOpenCLType)

    let go : CompileM CodeFunction :=
      withFVars xs fun argNames => do

        let bodyCode ← compileFunBody body
        return {
          name := name
          args := argTypes.zip argNames
          body := bodyCode
          returnType := returnType
        }

    let code ← go
    modify (fun s => {s with compiledFunctions := s.compiledFunctions.push code})
    let codeStr ← code.toString
    trace[HouLean.OpenCL.compiler] "compiled {f} to:\n{codeStr}"
    addOpenCLFunction f name .normal codeStr
    return code

run_meta compileFunctionRef.set compileFunctionCore

open Elab Term in
elab "#opencl_compile" f:term : command => do
  Command.liftTermElabM do
  let f ← elabTermAndSynthesize f none
  let (_,s) ← compileFunction f {} {}

  let msgs ← s.compiledFunctions.mapM (fun c => c.toString)
  let msg := msgs.joinl (map:=id) (·++"\n\n"++·)
  logInfo m!"compiled function: {s.compiledFunctions.map (fun c => c.name)}"
  logInfo m!"{msg}"
