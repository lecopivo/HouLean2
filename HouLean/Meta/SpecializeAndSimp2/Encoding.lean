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


class TypeEncoding (X : Type) (Y : outParam Type) where
  encode : X → Y
  decode : Y → X
  decode_encode : ∀ x, decode (encode x) = x

attribute [simp] TypeEncoding.decode_encode

open TypeEncoding

instance [TypeEncoding X Y] [TypeEncoding X' Y'] : TypeEncoding (X×X') (Y×Y') where
  encode x := (encode x.1, encode x.2)
  decode x := (decode x.1, decode x.2)
  decode_encode := by simp

instance : TypeEncoding Float Float where
  encode x := x
  decode x := x
  decode_encode := by simp

instance : TypeEncoding Float32 Float32 where
  encode x := x
  decode x := x
  decode_encode := by simp

instance : TypeEncoding Nat Nat where
  encode x := x
  decode x := x
  decode_encode := by simp

instance : TypeEncoding Int Int where
  encode x := x
  decode x := x
  decode_encode := by simp

instance : TypeEncoding UInt64 UInt64 where
  encode x := x
  decode x := x
  decode_encode := by simp

instance : TypeEncoding UInt32 UInt32 where
  encode x := x
  decode x := x
  decode_encode := by simp

instance : TypeEncoding UInt16 UInt16 where
  encode x := x
  decode x := x
  decode_encode := by simp

instance : TypeEncoding Int64 Int64 where
  encode x := x
  decode x := x
  decode_encode := by simp

instance : TypeEncoding Int32 Int32 where
  encode x := x
  decode x := x
  decode_encode := by simp

instance : TypeEncoding Int16 Int16 where
  encode x := x
  decode x := x
  decode_encode := by simp

@[simp] theorem Int.encode (x : Int) : encode x = x := by rfl
@[simp] theorem Int.decode (x : Int) : decode x = x := by rfl
@[simp] theorem Nat.encode (x : Nat) : encode x = x := by rfl
@[simp] theorem Nat.decode (x : Nat) : decode x = x := by rfl

instance [TypeEncoding X Y] [Inhabited X] : TypeEncoding (Option X) (Y × Bool) where
  encode x? := (encode (x?.getD default), x?.isSome)
  decode := fun (value, valid) => if valid then some (decode value) else none
  decode_encode := by intro x; cases x <;> simp

structure Vector2 (X : Type) where
  (x0 x1 : X)

instance [TypeEncoding X Y] : TypeEncoding (Vector X 2) (Vector2 Y) where
  encode u := { x0 := encode u[0], x1 := encode u[1] }
  decode u := #v[decode u.x0, decode u.x1 ]
  decode_encode := sorry_proof

structure Vector3 (X : Type) where
  (x0 x1 x2 : X)

instance [TypeEncoding X Y] : TypeEncoding (Vector X 3) (Vector3 Y) where
  encode u := { x0 := encode u[0], x1 := encode u[1], x2 := encode u[2] }
  decode u := #v[decode u.x0, decode u.x1, decode u.x2]
  decode_encode := sorry_proof

variable {X Y} [TypeEncoding X Y]
@[simp] theorem Vector3.decode_getElem_0 (u : Vector3 Y) : (decode (X:=Vector X 3) u)[0] = decode u.x0 := by rfl
@[simp] theorem Vector3.decode_getElem_1 (u : Vector3 Y) : (decode (X:=Vector X 3) u)[1] = decode u.x1 := by rfl
@[simp] theorem Vector3.decode_getElem_2 (u : Vector3 Y) : (decode (X:=Vector X 3) u)[2] = decode u.x2 := by rfl
@[simp] theorem Vector3.encode_vector_mk (x y z : X) : encode #v[x,y,z] = ⟨encode x, encode y, encode z⟩ := by rfl

structure Vector4 (X : Type) where
  (x0 x1 x2 x3 : X)

instance [TypeEncoding X Y] : TypeEncoding (Vector X 4) (Vector4 Y) where
  encode u := { x0 := encode u[0], x1 := encode u[1], x2 := encode u[2], x3 := encode u[3] }
  decode u := #v[decode u.x0, decode u.x1, decode u.x2, decode u.x3]
  decode_encode := sorry_proof

-- #check (by infer_instance : TypeEncoding (Vector (Vector Float 3) 2) _)
