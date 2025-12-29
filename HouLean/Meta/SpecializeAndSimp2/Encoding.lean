import Lean
import Qq
import HouLean.Meta.Basic
import HouLean.Meta.RunInterpreter
import HouLean.Meta.SpecializeAndSimp2.Types
import HouLean.Meta.RewriteBy
import HouLean.Data.Defs

namespace HouLean.Meta.Sas

-- Typeclass for uncurrying functions
class Uncurry (F : Type u) (Xs : outParam (Type v)) (Y : outParam (Type w)) where
  uncurry : F → (Xs → Y)
export Uncurry (uncurry)

instance : Uncurry (X → Y) X Y where
  uncurry f := f

instance {F X Xs Y} [Uncurry F Xs Y] : Uncurry (X → F) (X×Xs) Y where
  uncurry f := fun (x,xs)  => uncurry (f x) xs

namespace Uncurry

variable {F X Xs Y} [Uncurry F Xs Y]

@[simp ↓]
theorem uncurry_eval_base (f : X → Y) (x) : uncurry f x = f x := by rfl

@[simp ↓]
theorem uncurry_eval_succ (f : X → F) (xs) : uncurry f xs = uncurry (f xs.1) xs.2 := by rfl

end Uncurry

class TypeEncoding (F : outParam (Type u)) {Xs : outParam (Type v)} {Y : Type u} [Uncurry F Xs Y] where
  encode : Y → Xs
  decode : F
  valid : uncurry decode ∘ encode = id


variable {X' Y'} [TypeEncoding (X' → Y')]
variable {F Xs Y} [Uncurry F Xs Y] [TypeEncoding F]

open TypeEncoding

@[simp]
theorem decode_encode (y : Y) : (uncurry (decode (Y:=Y))) (encode y) = y :=
  congrFun TypeEncoding.valid y

@[simp]
theorem decode_encode' (y : Y') : decode (Y:=Y') (encode y) = y :=
  decode_encode y


--- some encodings


namespace OptionEncoding

instance {α : Type u} [Inhabited α] :
    TypeEncoding (α → Bool → Option α) where
  encode x := (x.getD default, x.isSome)
  decode x valid := if valid then some x else none
  valid := by funext x; cases x <;> simp[uncurry]


end OptionEncoding



namespace Vector3

instance :  TypeEncoding (Vector3 → Vector Float 3) where
  encode v := ⟨v[0], v[1], v[2]⟩
  decode v := #v[v.x, v.y, v.z]
  valid := by funext x; simp; grind


@[simp]
theorem decode_getElem_0 (v : Vector3) : (decode (Y:=Vector Float 3) v)[0] = v.x := by rfl
@[simp]
theorem decode_getElem_1 (v : Vector3) : (decode (Y:=Vector Float 3) v)[1] = v.y := by rfl
@[simp]
theorem decode_getElem_2 (v : Vector3) : (decode (Y:=Vector Float 3) v)[2] = v.z := by rfl


end Vector3

@[simp]
theorem unroll_sum_base (f : Fin 1 → Float) : sum f = f 0 := by sorry

@[simp]
theorem unroll_sum_succ (f : Fin (n+1) → Float) :
  sum f = f 0 + sum (fun i : Fin n => f i.succ) := by sorry


variable (u v : Vector Float 3)

set_option trace.Meta.Tactic.simp.rewrite true in
#check sum (fun i : Fin 3 => u[i] * v[i])
  rewrite_by
    simp
