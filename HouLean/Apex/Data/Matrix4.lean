import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Data.Matrix4

open HouLean.Apex.Generated

namespace HouLean.Matrix4

-- todo: regenerate

-- -- Constructor
-- noncomputable
-- def mk.apex_impl (row0 row1 row2 row3 : Vector4) : Matrix4 := Vector4ToMatrix4 row0 row1 row2 row3

-- attribute [apex_implemented_by HouLean.Matrix4.mk.apex_impl] mk

-- -- Row accessors
-- noncomputable
-- def row0.apex_impl (m : Matrix4) : Vector4 := 
--   let result := Matrix4ToVector4 m
--   result.1

-- attribute [apex_implemented_by HouLean.Matrix4.row0.apex_impl] row0

-- noncomputable
-- def row1.apex_impl (m : Matrix4) : Vector4 := 
--   let result := Matrix4ToVector4 m
--   result.2.1

-- attribute [apex_implemented_by HouLean.Matrix4.row1.apex_impl] row1

-- noncomputable
-- def row2.apex_impl (m : Matrix4) : Vector4 := 
--   let result := Matrix4ToVector4 m
--   result.2.2.1

-- attribute [apex_implemented_by HouLean.Matrix4.row2.apex_impl] row2

-- noncomputable
-- def row3.apex_impl (m : Matrix4) : Vector4 := 
--   let result := Matrix4ToVector4 m
--   result.2.2.2

-- attribute [apex_implemented_by HouLean.Matrix4.row3.apex_impl] row3

-- -- Arithmetic operations
-- noncomputable
-- def add.apex_impl (a b : Matrix4) : Matrix4 := AddMatrix4 a #a[b]

-- attribute [apex_implemented_by HouLean.Matrix4.add.apex_impl] add

-- noncomputable
-- def sub.apex_impl (a b : Matrix4) : Matrix4 := SubtractMatrix4 a #a[b]

-- attribute [apex_implemented_by HouLean.Matrix4.sub.apex_impl] sub

-- noncomputable
-- def mul.apex_impl (a b : Matrix4) : Matrix4 := MultiplyMatrix4 a #a[b]

-- attribute [apex_implemented_by HouLean.Matrix4.mul.apex_impl] mul

-- noncomputable
-- def mulScalar.apex_impl (a : Matrix4) (s : Float) : Matrix4 := MultiplyMatrix4Float a #a[s]

-- attribute [apex_implemented_by HouLean.Matrix4.mulScalar.apex_impl] mulScalar

-- noncomputable
-- def mulMatrix3.apex_impl (a : Matrix4) (b : Matrix3) : Matrix4 := MultiplyMatrix4Matrix3 a #a[b]

-- attribute [apex_implemented_by HouLean.Matrix4.mulMatrix3.apex_impl] mulMatrix3

-- noncomputable
-- def mulVector3.apex_impl (v : Vector3) (m : Matrix4) : Vector3 := MultiplyVector3Matrix4 v #a[m]

-- attribute [apex_implemented_by HouLean.Matrix4.mulVector3.apex_impl] mulVector3

-- noncomputable
-- def mulVector4.apex_impl (v : Vector4) (m : Matrix4) : Vector4 := MultiplyVector4Matrix4 v #a[m]

-- attribute [apex_implemented_by HouLean.Matrix4.mulVector4.apex_impl] mulVector4

-- noncomputable
-- def transpose.apex_impl (m : Matrix4) : Matrix4 := TransposeMatrix4 m

-- attribute [apex_implemented_by HouLean.Matrix4.transpose.apex_impl] transpose

-- noncomputable
-- def invert.apex_impl (m : Matrix4) : Matrix4 := InvertMatrix4 m

-- attribute [apex_implemented_by HouLean.Matrix4.invert.apex_impl] invert

-- noncomputable
-- def lerp.apex_impl (a b : Matrix4) (t : Float) : Matrix4 := LerpMatrix4 a b t

-- attribute [apex_implemented_by HouLean.Matrix4.lerp.apex_impl] lerp

-- noncomputable
-- def almostEquals.apex_impl (a b : Matrix4) (tolerance : Float) : Bool := AlmostEqualsMatrix4 a b tolerance

-- attribute [apex_implemented_by HouLean.Matrix4.almostEquals.apex_impl] almostEquals

-- noncomputable
-- def toMatrix3.apex_impl (m : Matrix4) : Matrix3 := ConvertMatrix4Matrix3 m

-- attribute [apex_implemented_by HouLean.Matrix4.toMatrix3.apex_impl] toMatrix3

-- noncomputable
-- def fromMatrix3.apex_impl (m : Matrix3) : Matrix4 := ConvertMatrix3Matrix4 m

-- attribute [apex_implemented_by HouLean.Matrix4.fromMatrix3.apex_impl] fromMatrix3

-- end Matrix4
