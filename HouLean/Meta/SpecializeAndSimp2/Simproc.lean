import Lean
import HouLean.Meta.SpecializeAndSimp2.Types

namespace HouLean.Meta.Sas

open Lean Meta

/-- Simprocs that stops `simp` whenever it encounters:
 - let binding
 - lambda function
 - match statemet

todo: maybe add bind too
-/
simproc_decl binder_backstop (_) := fun e => do
  if e.isLambda ||
     e.isLet ||
     (← Meta.isMatcherApp e) then
    return .done { expr := e }
  return .continue


-- -- todo: remove this and instead always include this simproc automatically in `sas`
-- attribute [simp ↓] binder_backstop
