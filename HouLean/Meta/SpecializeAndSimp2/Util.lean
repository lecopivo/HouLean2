import Lean

namespace HouLean.Meta.Sas

open Lean Meta

section SimpHelpers

/-- Retrieve simp theorems for the given attribute. -/
def getSimpTheorems' (attr : Name) : MetaM SimpTheorems := do
  if attr == `simp then
    getSimpTheorems
  else
    let some ext ← Lean.Meta.getSimpExtension? attr
      | throwError "simp attribute '{attr}' not found"
    ext.getTheorems

/-- Retrieve simprocs for the given attribute. -/
def getSimprocs (attr : Name) : MetaM Simprocs := do
  if attr == `simp then
    Simp.getSimprocs
  else
    let some ext ← Simp.getSimprocExtension? attr
      | throwError "simproc attribute '{attr}' not found"
    ext.getSimprocs

end SimpHelpers
