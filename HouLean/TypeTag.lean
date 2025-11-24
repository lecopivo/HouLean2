import Lean.Data.Json
import HouLean.Data.Defs

namespace HouLean

/-- Enum type for all attribute types -/
inductive AttribTypeTag where
  | float | floatArray
  | int | intArray
  | string | stringArray
  | vector2 | vector2Array
  | vector3 | vector3Array
  | vector4 | vector4Array
  | matrix2 | matrix2Array
  | matrix3 | matrix3Array
  | matrix4 | matrix4Array
  | dict | dictArray
deriving Inhabited, BEq, Repr

/-- Convert attribute type tag to corresponding Lean type. -/
def AttribTypeTag.toLeanType : AttribTypeTag → Type
  | float => Float
  | floatArray => Array Float
  | int => Int
  | intArray => Array Int
  | string => String
  | stringArray => Array String
  | vector2 => Vector2
  | vector2Array => Array Vector2
  | vector3 => Vector3
  | vector3Array => Array Vector3
  | vector4 => Vector4
  | vector4Array => Array Vector4
  | matrix2 => Matrix2
  | matrix2Array => Array Matrix2
  | matrix3 => Matrix3
  | matrix3Array => Array Matrix3
  | matrix4 => Matrix4
  | matrix4Array => Array Matrix4
  | dict => Lean.Json  -- can we model dictionaries directly in Lean?
  | dictArray => Array Lean.Json

def AttribTypeTag.typeAnnotation : AttribTypeTag → String
  | float => "f"
  | floatArray => "f[]"
  | int => "i"
  | intArray => "i[]"
  | string => "s"
  | stringArray => "s[]"
  | vector2 => "u"
  | vector2Array => "u[]"
  | vector3 => "v"
  | vector3Array => "v[]"
  | vector4 => "p"
  | vector4Array => "p[]"
  | matrix2 => "m2"
  | matrix2Array => "m2[]"
  | matrix3 => "m3"
  | matrix3Array => "m3[]"
  | matrix4 => "m4"
  | matrix4Array => "m4[]"
  | dict => "d"
  | dictArray => "d[]"

instance : CoeSort AttribTypeTag Type := ⟨fun t => t.toLeanType⟩

def AttribUntyped := (t : AttribTypeTag) × t.toLeanType
