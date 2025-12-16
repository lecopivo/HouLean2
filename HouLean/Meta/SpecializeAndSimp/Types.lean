import Lean

open Lean Meta

namespace HouLean.Meta

namespace SpecializeAndSimp

structure Specialization where
  keys : Array DiscrTree.Key
  originalName : Name
  specializationName : Name
  fn : Expr
deriving BEq

structure Config where

structure Context where
  config : Config := {}

structure State where
  specializations : DiscrTree Specialization := {}
  specOrder : Array Name := {}

private opaque MethodsRefPointed : NonemptyType.{0}
def MethodsRef : Type := MethodsRefPointed.type
instance : Nonempty MethodsRef :=
  by exact MethodsRefPointed.property

abbrev M := ReaderT MethodsRef <| ReaderT Context <| StateT State SimpM

instance : MonadRecDepth M where
  withRecDepth n x := fun mthds ctx s => MonadRecDepth.withRecDepth n (x mthds ctx s)
  getRecDepth := liftM (m := SimpM) <| MonadRecDepth.getRecDepth
  getMaxRecDepth := liftM (m := SimpM) <| MonadRecDepth.getMaxRecDepth


-- forward declare `specializeExpr`
initialize specializeExprRef : IO.Ref (Expr → M Expr) ← IO.mkRef (fun e => pure e)
def specializeExpr (e : Expr) : M Expr := do
  (← specializeExprRef.get) e

-- forward declare `specializeAndSimp`
initialize specializeAndSimpRef : IO.Ref (Expr → M Expr) ← IO.mkRef (fun e => pure e)
def specializeAndSimp (e : Expr) : M Expr := do
  (← specializeAndSimpRef.get) e


structure Methods where
  spec : Expr → M (Option Expr) := fun _ => return none
deriving Inhabited

unsafe def Methods.toMethodsRefImpl (m : Methods) : MethodsRef :=
  unsafeCast m

@[implemented_by Methods.toMethodsRefImpl]
opaque Methods.toMethodsRef (m : Methods) : MethodsRef

unsafe def MethodsRef.toMethodsImpl (m : MethodsRef) : Methods :=
  unsafeCast m

@[implemented_by MethodsRef.toMethodsImpl]
opaque MethodsRef.toMethods (m : MethodsRef) : Methods

def getMethods : M Methods :=
  return MethodsRef.toMethods (← read)

/-- Run customizable specialization function. -/
def customSpec (e : Expr) : M (Option Expr) := do
  (← getMethods).spec e


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
   (customSpec : Expr → M (Option Expr)) : MetaM (α × State) := do

  let ctx : SpecializeAndSimp.Context := {
    config := config,
  }
  let s : SpecializeAndSimp.State := {}
  let mthds : MethodsRef := {
      spec := customSpec
      : Methods
    }.toMethodsRef

  let simpCtx : Simp.Context ← Simp.mkContext
    (config := simpConfig)
    (simpTheorems := ← simpAttrs.mapM getTheorems)
  let simpState : Simp.State := {}
  -- todo: always include `let_stopper_simproc`
  let simpMthds := Simp.mkDefaultMethodsCore (← simpAttrs.mapM getSimprocs)

  let ((r, s), _) ← x mthds ctx s simpMthds.toMethodsRef simpCtx |>.run simpState

  return (r,s)


end SpecializeAndSimp

open SpecializeAndSimp in
def specializeAndSimp (e : Expr)
    (config : SpecializeAndSimp.Config := {}) (simpConfig : Simp.Config := {})
    (simpAttrs : Array Name := #[])
    (customSpec : Expr → M (Option Expr) := fun _ => return none) : MetaM Expr := do
  let (r,_) ← (SpecializeAndSimp.specializeAndSimp e).runInMeta config simpConfig simpAttrs customSpec
  return r
