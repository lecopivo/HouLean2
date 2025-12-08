import Lean

namespace HouLean.Meta

open Lean Elab Term Meta in
elab "typeof% " x:term : term => do
  let x ← elabTerm x none
  inferType x
