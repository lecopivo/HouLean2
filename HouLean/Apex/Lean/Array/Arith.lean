import HouLean.Apex.Lean.Array.ArrayType

open HouLean Apex Compiler Generated

namespace HouLean.Apex

class ArrayAdd (α : Type) (As : outParam Type) where
  arrayAdd : As → As → As

instance : ArrayAdd Int IntArray where
  arrayAdd := array_AddInt

instance : ArrayAdd Float FloatArray where
  arrayAdd := array_AddFloat

instance : ArrayAdd Vector2 Vector2Array where
  arrayAdd := array_AddVector2

instance : ArrayAdd Vector3 Vector3Array where
  arrayAdd := array_AddVector3

instance : ArrayAdd Vector4 Vector4Array where
  arrayAdd := array_AddVector4

instance : ArrayAdd Matrix3 Matrix3Array where
  arrayAdd := array_AddMatrix3

instance : ArrayAdd Matrix4 Matrix4Array where
  arrayAdd := array_AddMatrix4

class ArraySub (α : Type) (As : outParam Type) where
  arraySub : As → As → As

instance : ArraySub Int IntArray where
  arraySub := array_SubtractInt

instance : ArraySub Float FloatArray where
  arraySub := array_SubtractFloat

instance : ArraySub Vector2 Vector2Array where
  arraySub := array_SubtractVector2

instance : ArraySub Vector3 Vector3Array where
  arraySub := array_SubtractVector3

instance : ArraySub Vector4 Vector4Array where
  arraySub := array_SubtractVector4

instance : ArraySub Matrix3 Matrix3Array where
  arraySub := array_SubtractMatrix3

instance : ArraySub Matrix4 Matrix4Array where
  arraySub := array_SubtractMatrix4

class ArrayMul (α : Type) (As : outParam Type) where
  arrayMul : As → As → As

instance : ArrayMul Int IntArray where
  arrayMul := array_MultiplyInt

instance : ArrayMul Float FloatArray where
  arrayMul := array_MultiplyFloat

instance : ArrayMul Vector2 Vector2Array where
  arrayMul := array_MultiplyVector2

instance : ArrayMul Vector3 Vector3Array where
  arrayMul := array_MultiplyVector3

instance : ArrayMul Vector4 Vector4Array where
  arrayMul := array_MultiplyVector4

class ArrayDiv (α : Type) (As : outParam Type) where
  arrayDiv : As → As → As

instance : ArrayDiv Int IntArray where
  arrayDiv := array_DivideInt

instance : ArrayDiv Float FloatArray where
  arrayDiv := array_DivideFloat

instance : ArrayDiv Vector2 Vector2Array where
  arrayDiv := array_DivideVector2

instance : ArrayDiv Vector3 Vector3Array where
  arrayDiv := array_DivideVector3

instance : ArrayDiv Vector4 Vector4Array where
  arrayDiv := array_DivideVector4
