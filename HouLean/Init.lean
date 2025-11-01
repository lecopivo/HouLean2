import Lean 
import Qq 

namespace HouLean

/-! Kitchen sink file for the very general declarations that are useful everywhere
-/

/-- Reducible version of `Id` -/
abbrev Id' (α : Type u) := α

/-- Reducible version of `id` -/
abbrev id' {α} (a : α) := a


class SetElem (coll : Type u) (idx : Type v) (elem : outParam (Type w)) where
  setElem : coll → idx → elem → coll

export SetElem (setElem)


-- This is a product type into which structures are encoded
structure StructProd (α β : Type) where
  fst : α
  snd : β

-- Structure is encoded into StructProd that always ending with `StructEnd`
inductive StructEnd 
  | nil

