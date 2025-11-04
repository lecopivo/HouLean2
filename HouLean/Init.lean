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


axiom sorryProofAxiom {P : Prop} : P

/-- Same as `sorry` but makes sure that the term is of type `Prop`.

`sorry_proof` is very useful when writing programs such that you do not accidantelly add `sorry`
which would prevent compiler from generating executable code. -/
macro "sorry_proof" : term => do  `(sorryProofAxiom)

/-- Same as `sorry` but makes sure that the term is of type `Prop`.

`sorry_proof` is very useful when writing programs such that you do not accidantelly add `sorry`
which would prevent compiler fomr generating executable code. -/
macro "sorry_proof" : tactic => `(tactic| exact sorry_proof)


def _root_.Array.joinlM [Monad m] [Inhabited β] (xs : Array α) (map : α → m β) (op : β → β → m β) : m β := do
  if h : 0 < xs.size then
    xs[1:].foldlM (init:=(← map xs[0])) λ acc x => do op acc (← map x)
  else
    pure default

def _root_.Array.joinl [Inhabited β] (xs : Array α) (map : α → β) (op : β → β → β) : β := Id.run do
  xs.joinlM map op

def _root_.Array.joinrM [Monad m] [Inhabited β] (xs : Array α) (map : α → m β) (op : β → β → m β) : m β := do
  if _h : 0 < xs.size then
    let n := xs.size - 1
    have : n < xs.size := sorry_proof
    xs[0:n].foldrM (init:=(← map xs[n])) λ x acc => do op (← map x) acc
  else
    pure default

def _root_.Array.joinr [Inhabited β] (xs : Array α) (map : α → β) (op : β → β → β) : β := Id.run do
  xs.joinrM map op
