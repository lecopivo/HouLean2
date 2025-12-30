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
