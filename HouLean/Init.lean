import Lean
import Qq
import HouLean.Meta.Float

namespace HouLean


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

-- use native decide to prove element indices for trivial inequalities like `0<4∧1<4`
macro_rules | `(tactic| get_elem_tactic_extensible) => `(tactic| native_decide)


class SetElem (coll : Type u) (idx : Type v) (elem : outParam (Type w))
              (valid : outParam (coll → idx → Prop)) where
  setElem (xs : coll) (i : idx) (x : elem) (h : valid xs i) : coll

class SetElem? (coll : Type u) (idx : Type v) (elem : outParam (Type w))
              (valid : outParam (coll → idx → Prop)) extends SetElem coll idx elem valid where
  setElem? (xs : coll) (i : idx) (x : elem) : Option coll
  setElem! [Inhabited elem] (xs : coll) (i : idx) (x : elem) : coll

/-- Set element of a pointer -/
class SetElemM (Ptr : Type) (Idx : Type) (Elem : outParam Type)
    (m : outParam (Type → Type)) (Valid : outParam (Ptr → Idx → Prop)) where
  setElemM : (ptr : Ptr) → (i : Idx) → Elem → Valid ptr i → m Unit

export SetElem (setElem)
export SetElem? (setElem? setElem!)
export SetElemM (setElemM)

axiom sorryProofAxiom {P : Prop} : P
axiom sorryTermAxiom {P : Sort u} : P


abbrev Float64 := Float
opaque Float16 : Type := UInt16

-- todo move this somewhere
inductive Precision where
  /- | half -/ | single | double
deriving Repr

instance : Max Precision where
  max p q :=
    match p, q with
    | .single, .single => .single
    | _, _ => .double

instance : LE Precision where
  le p q :=
    match p, q with
    | .double, .single => False
    | _, _ => True

def FloatP (prec : Precision) :=
  match prec with
  -- | .half => Float16
  | .single => Float32
  | .double => Float64

instance : Repr (FloatP prec) :=
  match prec with
  | .single => inferInstanceAs (Repr Float32)
  | .double => inferInstanceAs (Repr Float64)

def IntP (prec : Precision) :=
  match prec with
  -- | .half => Int16
  | .single => Int32
  | .double => Int64

instance : Repr (IntP prec) :=
  match prec with
  | .single => inferInstanceAs (Repr Int32)
  | .double => inferInstanceAs (Repr Int64)


def UIntP (prec : Precision) :=
  match prec with
  -- | .half => UInt16
  | .single => UInt32
  | .double => UInt64


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

open Lean.Parser.Term in
macro "∑" binder:funBinder ", " body:term : term => `(sum fun $binder => $body)



/-- Gadget identity function that will stop `simp` from simplifying an expression.

This is useful when the lhs of simp theorem appears on the rhs. You can wrap the occurence
in `no_simp` an prevent simp from an infinite loop.

The main use is for `simp` based compiler. For example for compiling to C we might define this
function, which indicates that `spec` should be replaced with C function with the name `cfun`
```
def cFunction (spec : α) (cfun : String) : α := spec
```
Then we add the following simp theorem
```
theorem compile_sin : Float.sin = cFunction (no_simp Float.sin) "sin" := rfl
```
where we wrapped `Float.sin` in `no_simp` to preven this theorem to be applied again on the `spec`
argument of `cFunction`. -/
def no_simp {α : Sort u} (a : α) := a

simproc_decl no_simp_simproc (no_simp _) := fun e =>
  return .done { expr := e }
