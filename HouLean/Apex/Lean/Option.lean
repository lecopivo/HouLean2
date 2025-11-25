import HouLean.Init
import HouLean.Apex.ApexType
import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Apex.Lean.Ite

open HouLean Apex Compiler

namespace HouLean.Apex.Compiler

-- not sure if it shoudl be
--     - ApexType (Option α) (α×Bool)
-- or  - ApexType (Option α) (A×Bool)
instance {α A} [ApexType α A] [Inhabited A] : ApexType (Option α) (A×Bool) where
  toApex x? :=
    match x? with
    | some x => (toApex x, true)
    | none => (default, false)
  fromApex := fun (x, valid) =>  if valid then some (fromApex x) else none


variable
  {α A} [ApexType α A] [Inhabited A]
  {β B} [ApexType β B] [Inhabited B]

def _root_.Option.some.apex_impl (x : α) : Option α := fromApex (α:=Option α) (toApex x, true)

run_meta compilerExt.add (.implementedByName ``Option.some ``Option.some.apex_impl #[some 0, none, none, none, some 1]) default

def _root_.Option.none.apex_impl : Option α := fromApex (α:=Option α) (default, false)

run_meta compilerExt.add (.implementedByName ``Option.none ``Option.none.apex_impl #[some 0, none, none, none]) default

-- def _root_.Option.getD.apex_impl

-- def _root_.Option.map.apex_impl (f : α → β) (x? : Option α) : Option β :=
--   let (x, valid) := toApex x?
--   if valid then
--     some (f (fromApex x))
--   else
--     none

-- run_meta compilerExt.add (.implementedByName ``Option.map ``Option.map.apex_impl #[some 0, none, none, none, some 1, some 2, some 3]) default

def _root_.Option.rec.apex_impl {motive : Option α → Sort u_1}
    (n : motive none) (s : ((val : α) → motive (some val))) (t? : Option α) : motive t? :=
  let tvalid := toApex t?
  let t := tvalid.1
  let valid := tvalid.2
  if valid then
    cast sorry_proof (s (fromApex t))
  else
    cast sorry_proof n

run_meta compilerExt.add (.implementedByName ``Option.rec ``Option.rec.apex_impl #[some 0, none, none, none, some 1, some 2, some 3, some 4]) default
