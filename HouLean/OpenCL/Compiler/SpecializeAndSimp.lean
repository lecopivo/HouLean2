import HouLean.OpenCL.Compiler.Extension2
import HouLean.OpenCL.Compiler.Specialize
import HouLean.OpenCL.Basic
import HouLean.Meta.OverloadedFunction
import Qq

open Lean Meta Simp

namespace HouLean.OpenCL.Compiler2

namespace SpecializeAndSimp

structure Context where

structure State where


abbrev M := ReaderT Context <| StateT State SimpM

-- opencl_csimp


  def skipSpecialization (e : Expr) : MetaM Bool := do
  let type ← inferType e

  if (← isClass? type).isSome then
    return .true

  if (← inferType type).isProp then
    return .true

  -- has implemented by
  -- we have to really check unification as function like `getElem` might have few implemented_by
  -- but in most cases we want to specialize it
  let s := (compilerExt.getState (← getEnv)).implementedBy
  let candidates ← s.getMatch e
  for c in candidates do
    let (xs,_,_) ← forallMetaTelescope (← inferType c.lhs)
    let b := c.lhs.beta xs
    if ← isDefEq e b then
      return .true

  return false


def specializeAndSimp (e : Expr) : M Expr := sorry

mutual
partial def specializeExpr (e : Expr) : M Expr := do
  let e ← instantiateMVars e
  if ← skipSpecialization e then
    return e

  -- todo: hangle non .const functions or prevent them entering altogether

  let (fn, args) := e.withApp (fun fn args => (fn,args))
  let r ← specializeFunApp fn args

  -- no specialization to be done
  if fn == r.fn' then
    return e

  -- did we already specialized this?
  if let some e ← trySpecialization? r then
    return e
  else

  let e ← defineNewSpecialization r
  return e

partial def trySpecialization? (r : FunSpecializationResult) : M (Option Expr) := do

  -- todo: using ExprMap is flaky as function might have different defeq instances that are not
  --       syntactically equal. The current approach would create different specializations
  let s := (compilerExt.getState (← getEnv)).specializations
  if let some spec := s[r.fn']? then
    -- specialize all arguments
    let args' ← r.args'.mapM specializeExpr
    -- apply specialized function
    return ← mkAppM spec.specializationName args'
  return none

partial def defineNewSpecialization (r : FunSpecializationResult) : M Expr := do

  forallTelescope (← inferType r.fn') fun xs _ => do
    let body := r.fn'.beta xs
    let body' := (← unfold body r.funName).expr

    unless body != body' do
      throwError m!"Unable to unfold the definition of {r.funName}"

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
    compilerExt.add (.specialization {
      originalName := r.funName
      specializationName := name
      fn := r.fn'
    })

    return ← mkAppM name r.args'


end

partial def specializeAndSimp (e : Expr) : M Expr := do
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

    specializeOnly e

  | _ =>
    return e
where
  simplify (e : Expr) : M Expr := do
    return (← Simp.simp e).expr
  liftLets (e : Expr) : M Expr := do
    Meta.liftLets e

  specializeOnly (e : Expr) : M Expr := do
    return (← Simp.simp e).expr



--------------------------------------------------------------------------------

def getTheorems (attr : Name) : MetaM SimpTheorems := do
  let ext ← Lean.Meta.getSimpExtension? attr
  match ext with
  | none => throwError s!"Simp attribute {attr} not found"
  | some ext => ext.getTheorems

def getSimprocs (attr : Name) : MetaM Simprocs := do
  let ext ← Simp.getSimprocExtension? `opencl_csimp
  match ext with
  | none => throwError s!"Simp attribute {attr} not found"
  | some ext => ext.getSimprocs

simproc_decl let_stopper_simproc (_) := fun e => do
  if e.isLet then
    return .done { expr := e }
  return .continue

def M.runInMeta (x : M α) (simpAttrs : Array Name) : MetaM α := do

  let ctx : SpecializeAndSimp.Context := {}
  let s : SpecializeAndSimp.State := {}

  -- todo: always include `let_stopper_simproc`
  let simpMthds := Simp.mkDefaultMethodsCore (← simpAttrs.mapM getSimprocs)
  let simpCtx : Simp.Context ← Simp.mkContext
    (config := {zetaDelta := false, zeta := false, iota := false})
    (simpTheorems := ← simpAttrs.mapM getTheorems)
  let simpState : Simp.State := {}

  let ((r, _), _) ← x ctx s simpMthds.toMethodsRef simpCtx |>.run simpState
  return r

end SpecializeAndSimp


def specializeAndSimp (e : Expr) (simpAttrs : Array Name) (simpCfg : Simp.Config := {}) : MetaM Expr := do
  (SpecializeAndSimp.specializeAndSimp e).runInMeta simpAttrs

def foo (x : Nat) :=
  let a := x*x
  let b := x+x
  a + b + x


attribute [opencl_csimp] SpecializeAndSimp.let_stopper_simproc Nat.zero_add Nat.add_zero foo


open Qq
run_meta
  let e := q(let a :=
               let a1 := 1 + foo 20
               let a2 := 0 + foo (a1 + 30)
               a1 * a2
             let b :=
               let b1 := 21 + 3
               let b2 := 0 + foo a + 0
               b1 * a * b2
             a + b)

  let e' ← specializeAndSimp e #[`opencl_csimp] { zeta := false }

  logInfo e'
