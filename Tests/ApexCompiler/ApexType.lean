import HouLean
import HouLean.Apex.Data.Nat

open HouLean Apex ArrayType

private structure Float2 where (x y : Float)
deriving Inhabited

private structure Float3 where (x y z : Float)
deriving Inhabited

instance : ApexType Float2 Float2 where
  toApex := id
  fromApex := id

instance : ApexType Float3 Float3 where
  toApex := id
  fromApex := id

structure Vec3 (α : Type) where
  (x y z : α)
deriving Inhabited

instance [ApexType α A] : ApexType (Vec3 α) (Vec3 A) where
  toApex v := ⟨toApex v.x, toApex v.y, toApex v.z⟩
  fromApex v := ⟨fromApex v.x, fromApex v.y, fromApex v.z⟩

/-- info: Int -/
#guard_msgs in
#apex_type Nat

/-- info: Int -/
#guard_msgs in
#apex_type Int

/-- info: Float -/
#guard_msgs in
#apex_type Float

/-- info: (Int, Bool) -/
#guard_msgs in
#apex_type (Option Nat)

/-- info: ((Int, Bool), Bool) -/
#guard_msgs in
#apex_type (Option (Option Nat))

/-- info: Vector2 -/
#guard_msgs in
#apex_type Vector2

/-- info: (Float, Float) -/
#guard_msgs in
#apex_type Float2

/-- info: (Float, Float, Float) -/
#guard_msgs in
#apex_type Float3

/-- info: ((Float, Float, Float), Bool) -/
#guard_msgs in
#apex_type Option Float3

/-- info: ((Float, Bool), (Float, Bool), (Float, Bool)) -/
#guard_msgs in
#apex_type Vec3 (Option Float)

/-- info: IntArray -/
#guard_msgs in
#apex_type Array Nat

/-- info: FloatArray -/
#guard_msgs in
#apex_type Array Float

/-- info: IntArray -/
#guard_msgs in
#apex_type Array Int

/-- info: (IntArray, BoolArray) -/
#guard_msgs in
#apex_type Array (Option Nat)

/-- info: ((((IntArray, FloatArray), BoolArray), BoolArray), Matrix3Array) -/
#guard_msgs in
#apex_type Array (Option (Option (Nat × Float)) × Matrix3)

/-- info: ((Float, Float), (Vector2, Vector3)) -/
#guard_msgs in
#apex_type struct { x : Float, y : Float, u : Vector2, v : Vector3 }

/-- info: ((Float, Float), (Float, Float)) -/
#guard_msgs in
#apex_type struct { pos : struct {x : Float, y : Float}, vel : struct { x : Float, y : Float}}

/-- info: (((Float, Float), Bool), (Float, Float)) -/
#guard_msgs in
#apex_type struct { pos : Option struct {x : Float, y : Float}, vel : struct { x : Float, y : Float}}

/--
info: ((((Float, Float), Bool), ((Float, Float), Bool), ((Float, Float), Bool)), (Float, Float))
-/
#guard_msgs in
#apex_type struct { pos : Vec3 (Option struct {x : Float, y : Float}), vel : struct { x : Float, y : Float} }

/-- info: (Float, Bool) -/
#guard_msgs in
#apex_type struct { pos : Option Float }
