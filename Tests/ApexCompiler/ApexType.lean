import HouLean

open HouLean Apex ArrayType

private structure Float2 where (x y : Float)
private structure Float3 where (x y z : Float)

structure Vec3 (α : Type) where
  (x y z : α)

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
