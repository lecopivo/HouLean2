import Lean
import Qq

namespace HouLean

abbrev Float64 := Float

/-! Kitchen sink file for the very general declarations that are useful everywhere
-/

/-- Reducible version of `Id` -/
abbrev Id' (α : Type u) := α

/-- Reducible version of `id` -/
abbrev id' {α} (a : α) := a

-- Element access syntax
-- this overrides default Lean's syntax
syntax:max (name := getElemsStx) term noWs "[" withoutPosition(term) ", " withoutPosition(term),* "]" : term

-- We might use `getElem!` if working with guaranteed access is complicated/confusing to the users
macro_rules (kind := getElemsStx)
| `($x:ident[ $i:term , $is,* ]) => `(getElem $x ($i,$is,*) (by get_elem_tactic))


class SetElem (coll : Type u) (idx : Type v) (elem : outParam (Type w))
              (valid : outParam (coll → idx → Prop)) where
  setElem (xs : coll) (i : idx) (x : elem) (h : valid xs i) : coll

class SetElem? (coll : Type u) (idx : Type v) (elem : outParam (Type w))
              (valid : outParam (coll → idx → Prop)) extends SetElem coll idx elem valid where
  setElem? (xs : coll) (i : idx) (x : elem) : Option coll
  setElem! [Inhabited elem] (xs : coll) (i : idx) (x : elem) : coll

export SetElem (setElem)
export SetElem? (setElem? setElem!)

axiom sorryProofAxiom {P : Prop} : P
axiom sorryTermAxiom {P : Sort u} : P


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


def sum {α} [Add α] [Zero α] (f : Fin n → α) : α := Fin.foldl n (init := 0) (· + f ·)
