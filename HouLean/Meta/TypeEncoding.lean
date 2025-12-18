
namespace HouLean.Meta

class Uncurry (F : Type) (Args : outParam Type) (Result : outParam Type) where
  uncurry : F → Args → Result

export Uncurry (uncurry)

-- Base case: non-function type (lowest priority)
instance (priority := low) : Uncurry (α → β) α β where
  uncurry f x := f x

-- Recursive case: peel off one argument
instance [Uncurry B Args R] : Uncurry (A → B) (A × Args) R where
  uncurry := fun f (a, rest) => Uncurry.uncurry (f a) rest


structure TypeEncoding (A : Type) (F : outParam Type) {Xs} [Uncurry F Xs A] where
  encode : F
  decode : A → Xs
  valid : (uncurry encode) ∘ decode = id


def Option.typeEncoding (α : Type) [Inhabited α] : TypeEncoding (Option α) (α → Bool → Option α) where
  encode val valid := if valid then some val else none
  decode val? :=
    match val? with
    | some val => (val, true)
    | none => (default, false)
  valid := by ext x; simp[uncurry]; grind
