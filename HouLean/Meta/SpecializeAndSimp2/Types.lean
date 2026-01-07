import Lean
import HouLean.Meta.TimeIt
import HouLean.Meta.SpecializeAndSimp2.Init
import HouLean.Meta.SpecializeAndSimp2.Util

namespace HouLean.Meta.Sas

open Lean Meta

structure SpecializationRequest where
  funName : Name
  specName : Name
  funToSpecialize : Expr

structure Specialization where
  req : SpecializationRequest
  implName : Name

structure Context where
  letVars : Array Expr := #[]

structure State where
  timer : TimeItState := {}
  requests : List SpecializationRequest := []
  specs : Array Specialization := #[]

abbrev SasM := ReaderT Context <| StateT State <| SimpM

instance : MonadTimeIt SasM where
  startTimer n t :=
    modify (fun s => { s with timer := (s.timer.startTimer n t).getD s.timer })
  stopTimer n t :=
    modify (fun s => { s with timer := (s.timer.stopTimer n t).getD s.timer })
  getTimes := do
    return (← get).timer.times.toArray

def SasM.run {α} (x : SasM α) (attrs : Array Name) : MetaM α := do

  let ctx : Context := {}
  let s : State := {}

  let simpCtx ← Simp.mkContext
    (config := { zeta := false, zetaDelta := false, iota := true })
    (simpTheorems := ← attrs.mapM getSimpTheorems')
  let simpState : Simp.State := {}
  let simprocs ← attrs.mapM getSimprocs
  let simpMthds := Simp.mkDefaultMethodsCore simprocs

  let ((r,_),_) ← (x ctx s ).run simpCtx simpState simpMthds

  return r
