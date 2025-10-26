import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Data.Matrix2

open HouLean.Apex.Generated

namespace HouLean.Matrix2

-- APEX implementations
-- Note: Matrix2 doesn't have direct APEX node support, so we implement using Matrix3

-- Helper to convert Matrix2 to Matrix3
def toMatrix3 (m : Matrix2) : Matrix3 :=
  ⟨⟨m.row0.x, m.row0.y, 0⟩,
   ⟨m.row1.x, m.row1.y, 0⟩,
   ⟨0, 0, 1⟩⟩

-- Helper to convert Matrix3 to Matrix2
def fromMatrix3 (m : Matrix3) : Matrix2 :=
  ⟨⟨m.row0.x, m.row0.y⟩,
   ⟨m.row1.x, m.row1.y⟩⟩

-- Constructor (uses Matrix3 under the hood)
noncomputable
def mk.apex_impl (row0 row1 : Vector2) : Matrix2 := 
  let m3 := Vector3ToMatrix3 ⟨row0.x, row0.y, 0⟩ ⟨row1.x, row1.y, 0⟩ ⟨0, 0, 1⟩
  fromMatrix3 m3

attribute [apex_implemented_by HouLean.Matrix2.mk.apex_impl] mk

-- Row accessors
noncomputable
def row0.apex_impl (m : Matrix2) : Vector2 := 
  let m3 := toMatrix3 m
  let result := Matrix3ToVector3 m3
  ⟨result.1.x, result.1.y⟩

attribute [apex_implemented_by HouLean.Matrix2.row0.apex_impl] row0

noncomputable
def row1.apex_impl (m : Matrix2) : Vector2 := 
  let m3 := toMatrix3 m
  let result := Matrix3ToVector3 m3
  ⟨result.2.1.x, result.2.1.y⟩

attribute [apex_implemented_by HouLean.Matrix2.row1.apex_impl] row1

-- Arithmetic operations (implemented via Matrix3)
noncomputable
def add.apex_impl (a b : Matrix2) : Matrix2 := 
  let a3 := toMatrix3 a
  let b3 := toMatrix3 b
  fromMatrix3 (AddMatrix3 a3 #v[b3])

attribute [apex_implemented_by HouLean.Matrix2.add.apex_impl] add

noncomputable
def sub.apex_impl (a b : Matrix2) : Matrix2 := 
  let a3 := toMatrix3 a
  let b3 := toMatrix3 b
  fromMatrix3 (SubtractMatrix3 a3 #v[b3])

attribute [apex_implemented_by HouLean.Matrix2.sub.apex_impl] sub

noncomputable
def mul.apex_impl (a b : Matrix2) : Matrix2 := 
  let a3 := toMatrix3 a
  let b3 := toMatrix3 b
  fromMatrix3 (MultiplyMatrix3 a3 #v[b3])

attribute [apex_implemented_by HouLean.Matrix2.mul.apex_impl] mul

noncomputable
def mulScalar.apex_impl (a : Matrix2) (s : Float) : Matrix2 := 
  let a3 := toMatrix3 a
  fromMatrix3 (MultiplyMatrix3Float a3 #v[s])

attribute [apex_implemented_by HouLean.Matrix2.mulScalar.apex_impl] mulScalar

noncomputable
def mulVector.apex_impl (v : Vector2) (m : Matrix2) : Vector2 := 
  let v3 := ⟨v.x, v.y, 0⟩
  let m3 := toMatrix3 m
  let result := MultiplyVector3Matrix3 v3 #v[m3]
  ⟨result.x, result.y⟩

attribute [apex_implemented_by HouLean.Matrix2.mulVector.apex_impl] mulVector

noncomputable
def transpose.apex_impl (m : Matrix2) : Matrix2 := 
  let m3 := toMatrix3 m
  fromMatrix3 (TransposeMatrix3 m3)

attribute [apex_implemented_by HouLean.Matrix2.transpose.apex_impl] transpose

noncomputable
def invert.apex_impl (m : Matrix2) : Matrix2 := 
  let m3 := toMatrix3 m
  fromMatrix3 (InvertMatrix3 m3)

attribute [apex_implemented_by HouLean.Matrix2.invert.apex_impl] invert

noncomputable
def lerp.apex_impl (a b : Matrix2) (t : Float) : Matrix2 := 
  let a3 := toMatrix3 a
  let b3 := toMatrix3 b
  fromMatrix3 (LerpMatrix3 a3 b3 t)

attribute [apex_implemented_by HouLean.Matrix2.lerp.apex_impl] lerp

end Matrix2
