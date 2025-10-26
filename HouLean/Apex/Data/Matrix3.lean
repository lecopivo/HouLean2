import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Data.Matrix3

open HouLean.Apex.Generated

namespace HouLean.Matrix3

-- Constructor
noncomputable
def mk.apex_impl (row0 row1 row2 : Vector3) : Matrix3 := Vector3ToMatrix3 row0 row1 row2

attribute [apex_implemented_by HouLean.Matrix3.mk.apex_impl] mk

-- Row accessors
noncomputable
def row0.apex_impl (m : Matrix3) : Vector3 := 
  let result := Matrix3ToVector3 m
  result.1

attribute [apex_implemented_by HouLean.Matrix3.row0.apex_impl] row0

noncomputable
def row1.apex_impl (m : Matrix3) : Vector3 := 
  let result := Matrix3ToVector3 m
  result.2.1

attribute [apex_implemented_by HouLean.Matrix3.row1.apex_impl] row1

noncomputable
def row2.apex_impl (m : Matrix3) : Vector3 := 
  let result := Matrix3ToVector3 m
  result.2.2

attribute [apex_implemented_by HouLean.Matrix3.row2.apex_impl] row2

-- Arithmetic operations
noncomputable
def add.apex_impl (a b : Matrix3) : Matrix3 := AddMatrix3 a #v[b]

attribute [apex_implemented_by HouLean.Matrix3.add.apex_impl] add

noncomputable
def sub.apex_impl (a b : Matrix3) : Matrix3 := SubtractMatrix3 a #v[b]

attribute [apex_implemented_by HouLean.Matrix3.sub.apex_impl] sub

noncomputable
def mul.apex_impl (a b : Matrix3) : Matrix3 := MultiplyMatrix3 a #v[b]

attribute [apex_implemented_by HouLean.Matrix3.mul.apex_impl] mul

noncomputable
def mulScalar.apex_impl (a : Matrix3) (s : Float) : Matrix3 := MultiplyMatrix3Float a #v[s]

attribute [apex_implemented_by HouLean.Matrix3.mulScalar.apex_impl] mulScalar

noncomputable
def mulVector.apex_impl (v : Vector3) (m : Matrix3) : Vector3 := MultiplyVector3Matrix3 v #v[m]

attribute [apex_implemented_by HouLean.Matrix3.mulVector.apex_impl] mulVector

noncomputable
def transpose.apex_impl (m : Matrix3) : Matrix3 := TransposeMatrix3 m

attribute [apex_implemented_by HouLean.Matrix3.transpose.apex_impl] transpose

noncomputable
def invert.apex_impl (m : Matrix3) : Matrix3 := InvertMatrix3 m

attribute [apex_implemented_by HouLean.Matrix3.invert.apex_impl] invert

noncomputable
def lerp.apex_impl (a b : Matrix3) (t : Float) : Matrix3 := LerpMatrix3 a b t

attribute [apex_implemented_by HouLean.Matrix3.lerp.apex_impl] lerp

noncomputable
def almostEquals.apex_impl (a b : Matrix3) (tolerance : Float) : Bool := AlmostEqualsMatrix3 a b tolerance

attribute [apex_implemented_by HouLean.Matrix3.almostEquals.apex_impl] almostEquals

end Matrix3
