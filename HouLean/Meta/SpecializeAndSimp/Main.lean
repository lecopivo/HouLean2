import HouLean.Meta.SpecializeAndSimp.Types
import HouLean.Meta.OverloadedFunction
import Lean

open Lean Meta

namespace HouLean.Meta.SpecializeAndSimp

initialize registerTraceClass `HouLean.specialize
initialize registerTraceClass `HouLean.specialize.detail
initialize registerTraceClass `HouLean.specialize.simplify
initialize registerTraceClass `HouLean.specialize.liftLets

/-- Result of specializing a function application `fn arg₁ .. argₙ` to `fn' arg'₁ ... arg'ₘ`
    where compile-time known arguments are consumed in `fn'`. -/
structure FunSpecializationResult where
  funName : Name
  fn : Expr
  args : Array Expr
  fn' : Expr
  args' : Array Expr
  mangledName : Name

private def exprToString (e : Expr) : MetaM String := do
  let e ← whnf e
  return toString (← ppExpr e)

/-- Check if an argument should contribute to the mangled name. -/
private def shouldAddToMangledName (type : Expr) : MetaM Bool := do
  if (← isClass? type).isSome then return false
  if (← inferType type).isProp then return false
  return true

/-- Specialize a function application by consuming compile-time known arguments. -/
def specializeFunApp (fn : Expr) (args : Array Expr) : MetaM FunSpecializationResult := do
  let some funName := fn.constName?
    | throwError "specializeFunApp: expected const function, got {fn.ctorName}"

  trace[HouLean.specialize.detail] "specializing {funName} with {args.size} args"

  let (fn', args', nameParts) ← go fn args.toList #[] #[] #[]
  let suffix := nameParts.foldl (· ++ "_" ++ ·) ""
  let mangledName := funName.appendAfter suffix

  trace[HouLean.specialize.detail] "mangled name: {mangledName}, remaining args: {args'.size}"

  return { funName, fn, args, fn' := fn'.eta, args', mangledName }
where
  go (e : Expr) (args : List Expr) (vars : Array Expr) (remainingArgs : Array Expr)
     (nameParts : Array String) : MetaM (Expr × Array Expr × Array String) := do
    match args with
    | [] =>
      let f ← mkLambdaFVars vars e
      return (f, remainingArgs, nameParts)
    | arg :: rest =>
      if !(arg.hasFVar || arg.hasMVar) then
        -- Arg is compile-time known, consume it via beta reduction
        let type ← inferType arg
        let nameParts ← if ← shouldAddToMangledName type then
          pure (nameParts.push (← exprToString arg))
        else
          pure nameParts
        go (e.beta #[arg]) rest vars remainingArgs nameParts
      else
        -- Arg has free/meta vars, keep it as a parameter
        forallBoundedTelescope (← inferType e) (some 1) fun xs _ => do
          let some x := xs[0]? | throwError m!"bug in {decl_name%}, very odd case: {e} : {← inferType e}"
          go (e.beta #[x]) rest (vars.push x) (remainingArgs.push arg) nameParts

def addSpecialization (s : Specialization) : M Unit :=
  modify fun state => { state with
    specializations := state.specializations.insert s.fn s
  }

/-- Unfold a definition, handling projections specially. -/
private def unfoldDef (e : Expr) (declName : Name) : MetaM Expr := do
  let unfolded := (← unfold e declName).expr
  if ← isProjectionFn declName then
    unfolded.withApp fun fn args => do
      let fn := (← reduceProj? fn).getD fn
      return fn.beta args
  else
    return unfolded

/-- Define a new specialized function and register it. -/
partial def defineNewSpecialization (r : FunSpecializationResult) : M (Option Name) := do
  forallTelescope (← inferType r.fn') fun xs _ => do
    let body := r.fn'.beta xs
    let body' ← unfoldDef body r.funName

    if body == body' then
      trace[HouLean.specialize] "cannot unfold {r.funName}, skipping specialization"
      return none

    trace[HouLean.specialize] "creating specialization {r.mangledName}"

    let body'' ← specializeAndSimp body'
    let value ← mkLambdaFVars xs body'' >>= instantiateMVars
    let type ← inferType value
    let name ← Meta.mkUniqueDeclName r.mangledName

    let decl : Declaration := .defnDecl {
      name := name
      levelParams := []
      type := type
      value := value
      hints := .regular value.approxDepth
      safety := .safe
    }

    addAndCompile decl
    trace[HouLean.specialize] "compiled {name}"

    addSpecialization {
      originalName := r.funName
      specializationName := name
      fn := r.fn'
    }
    return some name

/-- Check if specialization should be skipped for this expression. -/
private def skipSpecialization (e : Expr) : M Bool := do
  let type ← inferType e
  if (← isClass? type).isSome then return true
  if (← inferType type).isProp then return true
  if ← (← read).skipSpecialization e then return true
  return false

/-- Recursively specialize an expression. -/
partial def specializeExpr (e : Expr) : M Expr := do
  let e ← instantiateMVars e
  trace[HouLean.specialize] m!"{e}"
  if ← skipSpecialization e then
    let (fn, args) := e.withApp (·, ·)
    let args ← args.mapM specializeExpr
    return fn.beta args

  match e with
  | .app .. =>
    let (fn, args) := e.withApp (·, ·)
    match fn with
    | .const .. =>
      let r ← specializeFunApp fn args
      let args' ← r.args'.mapM specializeExpr

      -- No specialization needed
      if fn == r.fn' then
        return r.fn'.beta args'

      -- Check for existing specialization
      let specs := (← get).specializations
      if let some spec := specs[r.fn']? then
        trace[HouLean.specialize.detail] "reusing specialization {spec.specializationName}"
        return ← mkAppM spec.specializationName args'

      -- Create new specialization
      if let some specName ← defineNewSpecialization r then
        return ← mkAppM specName args'
      else
        return r.fn'.beta args'

    | .proj typeName idx struct =>
      if let some struct' ← Meta.reduceProj? fn then
        specializeExpr (mkAppN struct' args)
      else
        let args' ← args.mapM specializeExpr
        return mkAppN (.proj typeName idx struct) args'

    | other =>
      throwError "specializeExpr: unhandled function kind {other.ctorName}"
  | _ => return e

/-- Main entry point: specialize and simplify an expression. -/
partial def specializeAndSimpImpl (e : Expr) : M Expr := do
  withIncRecDepth do
  trace[HouLean.specialize.detail] "processing {e.ctorName}"

  match e with
  | .letE n t v b nonDep =>
    let v' ← specializeAndSimp v

    -- Float out nested lets
    if v'.isLet then
      return ← letTelescope v' fun xs v'' => do
        let e' ← specializeAndSimp (.letE n t v'' b nonDep)
        mkLetFVars xs e'

    withLetDecl n t v' fun v'' => do
      let b' ← specializeAndSimp (b.instantiate1 v'')
      mkLetFVars #[v''] b'

  | .app .. =>
    let e ← simplify e
    let e ← liftLets e

    if e.isLet then
      return ← specializeAndSimp e

    match e with
    | .app .. =>
      match e.getAppFn' with
      | .const .. => specializeExpr e
      | .proj .. => reduceProj e
      | fn => throwError "specializeAndSimpImpl: unhandled app function {fn.ctorName}"
    | _ => return e

  | _ => return e
where
  simplify (e : Expr) : M Expr := do
    withTraceNode `HouLean.specialize.simplify (fun _ => return m!"simplify") do
      return (← Simp.simp e).expr

  liftLets (e : Expr) : M Expr := do
    withTraceNode `HouLean.specialize.liftLets (fun _ => return m!"liftLets") do
      Meta.liftLets e

  reduceProj (e : Expr) : M Expr := do
    let fn := e.getAppFn'
    if let some fn' ← Meta.reduceProj? fn then
      trace[HouLean.specialize.detail] "reduced projection {fn} → {fn'}"
      specializeAndSimp (mkAppN fn' e.getAppArgs)
    else
      trace[HouLean.specialize.detail] "cannot reduce projection {fn}, returning as-is"
      return e

initialize specializeAndSimpRef.set specializeAndSimpImpl

end HouLean.Meta.SpecializeAndSimp
