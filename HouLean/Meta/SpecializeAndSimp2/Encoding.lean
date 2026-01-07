import Lean
import Qq
import HouLean.Meta.Basic
import HouLean.Meta.RunInterpreter
import HouLean.Meta.SpecializeAndSimp2.Types
import HouLean.Meta.RewriteBy
import HouLean.Data.Defs
import HouLean.Data.Matrix

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


class TypeEncoding (X : Sort u) (Y : outParam (Sort v)) where
  encode : X → Y
  decode : Y → X
  decode_encode : ∀ x, decode (encode x) = x

open TypeEncoding

class TypeIsomorphism (X : Type) (Y : outParam Type) [TypeEncoding X Y] : Prop where
  encode_decode : ∀ y : Y, encode (decode (X:=X) y) = y

class TypeEncodingIdentity (X : Type) [TypeEncoding X X] : Prop extends TypeIsomorphism X X where
  encode_eq_id : ∀ x : X, encode x = x
  decode_eq_id : ∀ x : X, decode x = x

attribute [simp] TypeEncoding.decode_encode TypeIsomorphism.encode_decode
  TypeEncodingIdentity.encode_eq_id TypeEncodingIdentity.decode_eq_id

instance [TypeEncoding X Y] [TypeEncoding X' Y'] : TypeEncoding (X×X') (Y×Y') where
  encode x := (encode x.1, encode x.2)
  decode x := (decode x.1, decode x.2)
  decode_encode := by simp

variable [TypeEncoding X Y] [TypeEncoding X' Y']
@[simp] theorem encode_prod_mk (x : X) (x' : X') : encode (x,x') = (encode x, encode x') := by rfl
@[simp] theorem decode_prod_mk (y : Y) (y' : Y') : decode (y,y') = (decode (X:=X) y, decode (X:=X') y') := by rfl

instance [TypeEncoding X Y] [TypeEncoding X' Y'] [TypeIsomorphism X Y] [TypeIsomorphism X' Y']
    : TypeIsomorphism (X×X') (Y×Y') where
 encode_decode := by simp[encode,decode]

instance : TypeEncoding Float Float where
  encode x := x
  decode x := x
  decode_encode := by simp

instance : TypeEncodingIdentity Float where
  encode_decode := by simp[encode, decode]
  encode_eq_id := by simp[encode]
  decode_eq_id := by simp[decode]

instance : TypeEncoding Float32 Float32 where
  encode x := x
  decode x := x
  decode_encode := by simp

instance : TypeEncodingIdentity Float32 where
  encode_decode := by simp[encode, decode]
  encode_eq_id := by simp[encode]
  decode_eq_id := by simp[decode]

instance : TypeEncoding Nat Nat where
  encode x := x
  decode x := x
  decode_encode := by simp

instance : TypeEncodingIdentity Nat where
  encode_decode := by simp[encode, decode]
  encode_eq_id := by simp[encode]
  decode_eq_id := by simp[decode]

instance : TypeEncoding Int Int where
  encode x := x
  decode x := x
  decode_encode := by simp

instance : TypeEncodingIdentity Int where
  encode_decode := by simp[encode, decode]
  encode_eq_id := by simp[encode]
  decode_eq_id := by simp[decode]

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
deriving Inhabited

instance [TypeEncoding X Y] : TypeEncoding (Vector X 2) (Vector2 Y) where
  encode u := { x0 := encode u[0], x1 := encode u[1] }
  decode u := #v[decode u.x0, decode u.x1 ]
  decode_encode := sorry_proof

instance [TypeEncoding X Y] [TypeIsomorphism X Y] : TypeIsomorphism (Vector X 2) (Vector2 Y) where
  encode_decode := by simp[encode, decode]

variable {X Y} [TypeEncoding X Y]
@[simp] theorem Vector2.decode_getElem_0 (u : Vector2 Y) : (decode (X:=Vector X 2) u)[0] = decode u.x0 := by rfl
@[simp] theorem Vector2.decode_getElem_1 (u : Vector2 Y) : (decode (X:=Vector X 2) u)[1] = decode u.x1 := by rfl
@[simp] theorem Vector2.encode_vector_mk (a : Array X) (h : a.size = 2) : Vector.mk a h = decode ⟨encode a[0], encode a[1]⟩ := by ext i; match i with | 0 | 1 => simp
@[simp] theorem Vector2.toArray_explicit (u : Vector X 2) : u.toArray = #[u[0], u[1]] := by ext i; simp; match i with | 0 | 1 => simp

structure Vector3 (X : Type) where
  (x0 x1 x2 : X)
deriving Inhabited

instance [TypeEncoding X Y] : TypeEncoding (Vector X 3) (Vector3 Y) where
  encode u := { x0 := encode u[0], x1 := encode u[1], x2 := encode u[2] }
  decode u := #v[decode u.x0, decode u.x1, decode u.x2]
  decode_encode := sorry_proof

instance [TypeEncoding X Y] [TypeIsomorphism X Y] : TypeIsomorphism (Vector X 3) (Vector3 Y) where
  encode_decode := by simp[encode, decode]

@[simp] theorem Vector3.decode_getElem_0 (u : Vector3 Y) : (decode (X:=Vector X 3) u)[0] = decode u.x0 := by rfl
@[simp] theorem Vector3.decode_getElem_1 (u : Vector3 Y) : (decode (X:=Vector X 3) u)[1] = decode u.x1 := by rfl
@[simp] theorem Vector3.decode_getElem_2 (u : Vector3 Y) : (decode (X:=Vector X 3) u)[2] = decode u.x2 := by rfl
@[simp] theorem Vector3.encode_vector_mk (a : Array X) (h : a.size = 3) :
  Vector.mk a h = decode ⟨encode a[0], encode a[1], encode a[2]⟩ := by ext i; match i with | 0 | 1 | 2 => simp
@[simp] theorem Vector3.toArray_explicit (u : Vector X 3) : u.toArray = #[u[0], u[1], u[2]] := by ext i; simp; match i with | 0 | 1 | 2 => simp


structure Vector4 (X : Type) where
  (x0 x1 x2 x3 : X)
deriving Inhabited

instance [TypeEncoding X Y] : TypeEncoding (Vector X 4) (Vector4 Y) where
  encode u := { x0 := encode u[0], x1 := encode u[1], x2 := encode u[2], x3 := encode u[3] }
  decode u := #v[decode u.x0, decode u.x1, decode u.x2, decode u.x3]
  decode_encode := sorry_proof

instance [TypeEncoding X Y] [TypeIsomorphism X Y] : TypeIsomorphism (Vector X 4) (Vector4 Y) where
  encode_decode := by simp[encode, decode]

@[simp] theorem Vector4.decode_getElem_0 (u : Vector4 Y) : (decode (X:=Vector X 4) u)[0] = decode u.x0 := by rfl
@[simp] theorem Vector4.decode_getElem_1 (u : Vector4 Y) : (decode (X:=Vector X 4) u)[1] = decode u.x1 := by rfl
@[simp] theorem Vector4.decode_getElem_2 (u : Vector4 Y) : (decode (X:=Vector X 4) u)[2] = decode u.x2 := by rfl
@[simp] theorem Vector4.decode_getElem_3 (u : Vector4 Y) : (decode (X:=Vector X 4) u)[3] = decode u.x3 := by rfl
@[simp] theorem Vector4.encode_vector_mk (a : Array X) (h : a.size = 4) : Vector.mk a h = decode ⟨encode a[0], encode a[1], encode a[2], encode a[3]⟩ :=
  by ext i; match i with | 0 | 1 | 2 | 3 => simp
@[simp] theorem Vector4.toArray_explicit (u : Vector X 4) : u.toArray = #[u[0], u[1], u[2], u[3]] := by ext i; simp; match i with | 0 | 1 | 2 | 3 => simp


instance {X} {m n : Nat} [TypeEncoding (Vector (Vector X n) m) M] : TypeEncoding (Matrix X m n) M where
  encode A := encode A.data
  decode A := ⟨decode A⟩
  decode_encode := by simp

instance {X} {m n : Nat} [TypeEncoding (Vector (Vector X n) m) M] [TypeIsomorphism (Vector (Vector X n) m) M] :
    TypeIsomorphism (Matrix X m n) M where
  encode_decode := by simp[decode, encode]

@[simp]
theorem matrix_decode_data {X} {m n : Nat} [TypeEncoding (Vector (Vector X n) m) M] (A : M) :
  (decode (X:=Matrix X m n) A).data = decode (X:=(Vector (Vector X n) m)) A := by rfl

@[simp]
theorem encode_matrix_mk {X} {m n : Nat} [TypeEncoding (Vector (Vector X n) m) M] (data : (Vector (Vector X n) m)) :
  (encode (Matrix.mk data)) = encode data := by rfl

@[simp]
theorem matrix_getElem {α} {m n} (A : Matrix α m n) (ij : Nat × Nat) (h) : A[ij]'h = A.data[ij.1][ij.2] := by rfl
