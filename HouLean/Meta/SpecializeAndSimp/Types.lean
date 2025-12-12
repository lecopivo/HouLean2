import Lean

open Lean Meta

namespace HouLean.Meta

namespace SpecializeAndSimp

structure Specialization where
  originalName : Name
  specializationName : Name
  fn : Expr

structure Config where

structure Context where
  config : Config := {}
  skipSpecialization : Expr → MetaM Bool := fun _ => pure false

structure State where
  specializations : ExprMap Specialization := {}

abbrev M := ReaderT Context <| StateT State SimpM

instance : MonadRecDepth M where
  withRecDepth n x := fun ctx s => MonadRecDepth.withRecDepth n (x ctx s)
  getRecDepth := liftM (m := SimpM) <| MonadRecDepth.getRecDepth
  getMaxRecDepth := liftM (m := SimpM) <| MonadRecDepth.getMaxRecDepth

-- forward declare `specializeAndSimp`
initialize specializeAndSimpRef : IO.Ref (Expr → M Expr) ← IO.mkRef (fun e => pure e)
def specializeAndSimp (e : Expr) : M Expr := do
  (← specializeAndSimpRef.get) e


def getTheorems (attr : Name) : MetaM SimpTheorems := do
  if attr == `simp then
    getSimpTheorems
  else
    let ext ← Lean.Meta.getSimpExtension? attr
    match ext with
    | none => throwError s!"Simp attribute {attr} not found"
    | some ext => ext.getTheorems

def getSimprocs (attr : Name) : MetaM Simprocs := do
  if attr == `simp then
    Simp.getSimprocs
  else
    let ext ← Simp.getSimprocExtension? `opencl_csimp
    match ext with
    | none => throwError s!"Simp attribute {attr} not found"
    | some ext => ext.getSimprocs

simproc_decl let_stopper_simproc (_) := fun e => do
  if e.isLet then
    return .done { expr := e }
  return .continue

def M.runInMeta (x : M α)
   (config : Config := {}) (simpConfig : Simp.Config := {})
   (simpAttrs : Array Name := #[])
   (skipSpec : Expr → MetaM Bool) : MetaM α := do

  let ctx : SpecializeAndSimp.Context := {
    config := config,
    skipSpecialization := skipSpec
  }
  let s : SpecializeAndSimp.State := {}

  -- todo: always include `let_stopper_simproc`
  let simpMthds := Simp.mkDefaultMethodsCore (← simpAttrs.mapM getSimprocs)
  let simpCtx : Simp.Context ← Simp.mkContext
    (config := simpConfig)
    (simpTheorems := ← simpAttrs.mapM getTheorems)
  let simpState : Simp.State := {}

  let ((r, _), _) ← x ctx s simpMthds.toMethodsRef simpCtx |>.run simpState
  return r


end SpecializeAndSimp

open SpecializeAndSimp in
def specializeAndSimp (e : Expr)
     (config : SpecializeAndSimp.Config := {}) (simpConfig : Simp.Config := {})
     (simpAttrs : Array Name := #[])
     (skipSpec : Expr → MetaM Bool) : MetaM Expr := do
  (SpecializeAndSimp.specializeAndSimp e).runInMeta config simpConfig simpAttrs skipSpec



-- skip specialization based in implemented_by
  -- -- has implemented by
  -- -- we have to really check unification as function like `getElem` might have few implemented_by
  -- -- but in most cases we want to specialize it
  -- let s := (compilerExt.getState (← getEnv)).implementedBy
  -- let candidates ← s.getMatch e
  -- for c in candidates do
  --   let (xs,_,_) ← forallMetaTelescope (← inferType c.lhs)
  --   let b := c.lhs.beta xs
  --   if ← isDefEq e b then
  --     return .true
