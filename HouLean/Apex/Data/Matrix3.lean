import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Data.Matrix3
import HouLean.Data.Matrix3LinearAlgebra

open HouLean.Apex.Generated

namespace HouLean.Matrix3

-- Constructor
@[apex_implements HouLean.Matrix3.mk]
noncomputable
def mk.apex_impl (row0 row1 row2 : Vector3) : Matrix3 := Vector3ToMatrix3 row0 row1 row2

-- Row accessors
@[apex_implements HouLean.Matrix3.row0]
noncomputable
def row0.apex_impl (m : Matrix3) : Vector3 := 
  let result := Matrix3ToVector3 m
  result.1

@[apex_implements HouLean.Matrix3.row1]
noncomputable
def row1.apex_impl (m : Matrix3) : Vector3 := 
  let result := Matrix3ToVector3 m
  result.2.1

@[apex_implements HouLean.Matrix3.row2]
noncomputable
def row2.apex_impl (m : Matrix3) : Vector3 := 
  let result := Matrix3ToVector3 m
  result.2.2

-- Arithmetic operations
@[apex_implements HouLean.Matrix3.add]
noncomputable
def add.apex_impl (a b : Matrix3) : Matrix3 := AddMatrix3 a #v[b]

@[apex_implements HouLean.Matrix3.sub]
noncomputable
def sub.apex_impl (a b : Matrix3) : Matrix3 := SubtractMatrix3 a #v[b]

@[apex_implements HouLean.Matrix3.matmul]
noncomputable
def mul.apex_impl (a b : Matrix3) : Matrix3 := MultiplyMatrix3 a #v[b]

@[apex_implements HouLean.Matrix3.smul]
noncomputable
def mulScalar.apex_impl (a : Matrix3) (s : Float) : Matrix3 := MultiplyMatrix3Float a #v[s]

@[apex_implements HouLean.Matrix3.vecMul]
noncomputable
def mulVector.apex_impl (v : Vector3) (m : Matrix3) : Vector3 := MultiplyVector3Matrix3 v #v[m]

@[apex_implements HouLean.Matrix3.transpose]
noncomputable
def transpose.apex_impl (m : Matrix3) : Matrix3 := TransposeMatrix3 m

@[apex_implements HouLean.Matrix3.inverse]
noncomputable
def invert.apex_impl (m : Matrix3) : Matrix3 := InvertMatrix3 m

@[apex_implements HouLean.Matrix3.lerp]
noncomputable
def lerp.apex_impl (a b : Matrix3) (t : Float) : Matrix3 := LerpMatrix3 a b t

-- @[apex_implements HouLean.Matrix3.almostEquals]
-- noncomputable
-- def almostEquals.apex_impl (a b : Matrix3) (tolerance : Float) : Bool := AlmostEqualsMatrix3 a b tolerance

end Matrix3
