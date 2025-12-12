import HouLean.Meta.SpecializeAndSimp.Types
import HouLean.Meta.OverloadedFunction
import Lean

open Lean Meta

namespace HouLean.Meta

namespace SpecializeAndSimp

/-- Give expression `fn arg₁ .. argₙ` the `apecializeFunction` returns `fn' arg'₁ ... arg'ₘ`
which is equal to the orignal expression but all arguments known at compile time are consumed in `fn'`
 -/
structure FunSpecializationResult where
  funName : Name

  /-- original function -/
  fn : Expr
  /-- original arguments -/
  args : Array Expr

  fn' : Expr
  args' : Array Expr

  mangledName : Name

def exprToString (e : Expr) : MetaM String := do
  -- reduce before printing
  let e ← whnf e
  let s := toString (← ppExpr e)
  return s


-- todo: should I also add support for "macro types"?
--       arguments of type `Optional α` could specialize for `some _` and `.none` and
--       it would be required to completely eliminate these types from the runtime
def specializeFunApp (fn : Expr) (args : Array Expr) : MetaM FunSpecializationResult := do
  let some funName := fn.constName?
    | throwError m!"Bug in {decl_name%}, trying to specialize non-const function, ctor {fn.ctorName}"
  let (fn', args', ss) ← go fn args.toList #[] #[] #[]
  let ss := ss.foldl (·++"_"++·) ""
  let mangledName := funName.appendAfter ss
  let fn' := fn'.eta
  return { funName, fn, args, fn', args', mangledName }
where
  go (e : Expr) (args : List Expr)
     (vars : Array Expr) (args' : Array Expr) (ss : Array String) :
     MetaM (Expr × Array Expr × Array String) := do
    match args with
    | [] =>
      let f ← mkLambdaFVars vars e
      return (f, args', ss)
    | arg :: args =>
      if !(arg.hasFVar || arg.hasMVar) then
        let type ← inferType arg
        let mut ss := ss

        -- ignore instances and proofs
        if (← isClass? type).isNone &&
           !(← inferType type).isProp then
          ss := ss.push (← exprToString arg)

        go (e.beta #[arg]) args vars args' ss
      else
        forallBoundedTelescope (← inferType e) (some 1) fun xs _ => do
          let x := xs[0]!
          go (e.beta #[x]) args (vars.push x) (args'.push arg) ss


def addSpecialization (s : Specialization) : M Unit := do
  modify (fun state => {state with specializations := state.specializations.insert s.fn s})

def unfold' (e : Expr) (declName : Name) : MetaM Expr := do
  let e := (← unfold e declName).expr
  if ← isProjectionFn declName then
    e.withApp fun fn args => do
      let fn := (← reduceProj? fn).getD fn
      return fn.beta args
  else
    return e

partial def defineNewSpecialization (r : FunSpecializationResult) : M (Option Name) := do
  forallTelescope (← inferType r.fn') fun xs _ => do
    let body := r.fn'.beta xs
    let body' ← unfold' body r.funName

    unless body != body' do
      return none
      -- throwError m!"Unable to unfold the definition of {r.funName}"

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

    addSpecialization {
      originalName := r.funName
      specializationName := name
      fn := r.fn'
    }
    return name

def skipSpecialization (e : Expr) : M Bool := do
  let type ← inferType e

  if (← isClass? type).isSome then
    return .true

  if (← inferType type).isProp then
    return .true

  if ← (← read).skipSpecialization e then
    return .true

  return false

partial def specializeExpr (e : Expr) : M Expr := do
  let e ← instantiateMVars e
  if ← skipSpecialization e then
    return e

  -- todo: hangle non .const functions or prevent them entering altogether
  match e with
  | .app .. =>
    let (fn, args) := e.withApp (fun fn args => (fn,args))
    match fn with
    | .const .. =>
      let r ← specializeFunApp fn args

      let args' ← r.args'.mapM specializeExpr

      -- no specialization to be done
      if fn == r.fn' then
        return r.fn'.beta args'

      -- did we already specialized this?
      let s := (←get).specializations
      if let some spec := s[r.fn']? then
        return ← mkAppM spec.specializationName args'

      -- define new specialization
      if let some specName ← defineNewSpecialization r then
        return ← mkAppM specName args'
      else
        return r.fn'.beta args'
    | e' =>
      throwError "hangle this case {e'.ctorName}"
  | e =>
    return e


partial def specializeAndSimpImpl (e : Expr) : M Expr := do
  match e with
  | .letE n t v b nonDep =>

    let v' ← specializeAndSimp v

    if v'.isLet then
      return ←
        letTelescope v' fun xs v'' => do
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
      | .const .. =>
        specializeExpr e
      | fn =>
        throwError m!"hangle this case: {fn.ctorName}"
    | _ => return e
  | _ =>
    return e
where
  simplify (e : Expr) : M Expr := do
    return (← Simp.simp e).expr
  liftLets (e : Expr) : M Expr := do
    Meta.liftLets e


initialize specializeAndSimpRef.set specializeAndSimpImpl
