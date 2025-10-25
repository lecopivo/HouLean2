import HouLean.Apex.Data.Float
import HouLean.Apex.Data.Vector3

open HouLean.Apex.Generated

namespace HouLean.Apex.Matrix3

-- Helper to create identity matrix
def identity : Matrix3 :=
  ⟨⟨1, 0, 0⟩, ⟨0, 1, 0⟩, ⟨0, 0, 1⟩⟩

-- Helper to create zero matrix
def zero : Matrix3 :=
  ⟨⟨0, 0, 0⟩, ⟨0, 0, 0⟩, ⟨0, 0, 0⟩⟩

-- Arithmetic operations (Lean implementations)
def add (a b : Matrix3) : Matrix3 :=
  ⟨a.row0 + b.row0, a.row1 + b.row1, a.row2 + b.row2⟩

def sub (a b : Matrix3) : Matrix3 :=
  ⟨a.row0 - b.row0, a.row1 - b.row1, a.row2 - b.row2⟩

def mulScalar (a : Matrix3) (s : Float) : Matrix3 :=
  ⟨a.row0 * s, a.row1 * s, a.row2 * s⟩

def mul (a b : Matrix3) : Matrix3 :=
  -- Matrix multiplication: result[i,j] = sum(a[i,k] * b[k,j])
  let col0 := ⟨b.row0.x, b.row1.x, b.row2.x⟩
  let col1 := ⟨b.row0.y, b.row1.y, b.row2.y⟩
  let col2 := ⟨b.row0.z, b.row1.z, b.row2.z⟩
  ⟨⟨a.row0.dot col0, a.row0.dot col1, a.row0.dot col2⟩,
   ⟨a.row1.dot col0, a.row1.dot col1, a.row1.dot col2⟩,
   ⟨a.row2.dot col0, a.row2.dot col1, a.row2.dot col2⟩⟩

-- Vector-matrix multiplication: v * M (vector on the left, Houdini style)
def mulVector (v : Vector3) (m : Matrix3) : Vector3 :=
  let col0 := ⟨m.row0.x, m.row1.x, m.row2.x⟩
  let col1 := ⟨m.row0.y, m.row1.y, m.row2.y⟩
  let col2 := ⟨m.row0.z, m.row1.z, m.row2.z⟩
  ⟨v.dot col0, v.dot col1, v.dot col2⟩

-- Matrix operations (Lean implementations)
def transpose (m : Matrix3) : Matrix3 :=
  ⟨⟨m.row0.x, m.row1.x, m.row2.x⟩,
   ⟨m.row0.y, m.row1.y, m.row2.y⟩,
   ⟨m.row0.z, m.row1.z, m.row2.z⟩⟩

def determinant (m : Matrix3) : Float :=
  m.row0.x * (m.row1.y * m.row2.z - m.row1.z * m.row2.y) -
  m.row0.y * (m.row1.x * m.row2.z - m.row1.z * m.row2.x) +
  m.row0.z * (m.row1.x * m.row2.y - m.row1.y * m.row2.x)

def invert (m : Matrix3) : Matrix3 :=
  let det := m.determinant
  if det == 0 then m else
    let invDet := 1 / det
    let m00 := (m.row1.y * m.row2.z - m.row1.z * m.row2.y) * invDet
    let m01 := (m.row0.z * m.row2.y - m.row0.y * m.row2.z) * invDet
    let m02 := (m.row0.y * m.row1.z - m.row0.z * m.row1.y) * invDet
    let m10 := (m.row1.z * m.row2.x - m.row1.x * m.row2.z) * invDet
    let m11 := (m.row0.x * m.row2.z - m.row0.z * m.row2.x) * invDet
    let m12 := (m.row0.z * m.row1.x - m.row0.x * m.row1.z) * invDet
    let m20 := (m.row1.x * m.row2.y - m.row1.y * m.row2.x) * invDet
    let m21 := (m.row0.y * m.row2.x - m.row0.x * m.row2.y) * invDet
    let m22 := (m.row0.x * m.row1.y - m.row0.y * m.row1.x) * invDet
    ⟨⟨m00, m01, m02⟩, ⟨m10, m11, m12⟩, ⟨m20, m21, m22⟩⟩

-- Comparison operations (Lean implementations)
def beq (a b : Matrix3) : Bool :=
  a.row0 == b.row0 && a.row1 == b.row1 && a.row2 == b.row2

def almostEquals (a b : Matrix3) (tolerance : Float) : Bool :=
  let diff0 := a.row0 - b.row0
  let diff1 := a.row1 - b.row1
  let diff2 := a.row2 - b.row2
  diff0.lengthSquared <= tolerance * tolerance &&
  diff1.lengthSquared <= tolerance * tolerance &&
  diff2.lengthSquared <= tolerance * tolerance

-- Linear interpolation
def lerp (a b : Matrix3) (t : Float) : Matrix3 :=
  ⟨a.row0.lerp b.row0 t,
   a.row1.lerp b.row1 t,
   a.row2.lerp b.row2 t⟩

-- Operator overloading instances
instance : Add Matrix3 where
  add := add

instance : Sub Matrix3 where
  sub := sub

instance : Mul Matrix3 where
  mul := mul

instance : HMul Matrix3 Float Matrix3 where
  hMul := mulScalar

-- Vector * Matrix (Houdini style: vector on the left)
instance : HMul Vector3 Matrix3 Vector3 where
  hMul := mulVector

instance : BEq Matrix3 where
  beq := beq

-- APEX implementations

-- Constructor
noncomputable
def mk.impl (row0 row1 row2 : Vector3) : Matrix3 := Vector3ToMatrix3 row0 row1 row2

attribute [apex_implemented_by HouLean.Apex.Matrix3.mk.impl] mk

-- Row accessors
noncomputable
def row0.impl (m : Matrix3) : Vector3 := 
  let result := Matrix3ToVector3 m
  result.1

attribute [apex_implemented_by HouLean.Apex.Matrix3.row0.impl] row0

noncomputable
def row1.impl (m : Matrix3) : Vector3 := 
  let result := Matrix3ToVector3 m
  result.2.1

attribute [apex_implemented_by HouLean.Apex.Matrix3.row1.impl] row1

noncomputable
def row2.impl (m : Matrix3) : Vector3 := 
  let result := Matrix3ToVector3 m
  result.2.2

attribute [apex_implemented_by HouLean.Apex.Matrix3.row2.impl] row2

-- Arithmetic operations
noncomputable
def add.impl (a b : Matrix3) : Matrix3 := AddMatrix3 a #v[b]

attribute [apex_implemented_by HouLean.Apex.Matrix3.add.impl] add

noncomputable
def sub.impl (a b : Matrix3) : Matrix3 := SubtractMatrix3 a #v[b]

attribute [apex_implemented_by HouLean.Apex.Matrix3.sub.impl] sub

noncomputable
def mul.impl (a b : Matrix3) : Matrix3 := MultiplyMatrix3 a #v[b]

attribute [apex_implemented_by HouLean.Apex.Matrix3.mul.impl] mul

noncomputable
def mulScalar.impl (a : Matrix3) (s : Float) : Matrix3 := MultiplyMatrix3Float a #v[s]

attribute [apex_implemented_by HouLean.Apex.Matrix3.mulScalar.impl] mulScalar

noncomputable
def mulVector.impl (v : Vector3) (m : Matrix3) : Vector3 := MultiplyVector3Matrix3 v #v[m]

attribute [apex_implemented_by HouLean.Apex.Matrix3.mulVector.impl] mulVector

noncomputable
def transpose.impl (m : Matrix3) : Matrix3 := TransposeMatrix3 m

attribute [apex_implemented_by HouLean.Apex.Matrix3.transpose.impl] transpose

noncomputable
def invert.impl (m : Matrix3) : Matrix3 := InvertMatrix3 m

attribute [apex_implemented_by HouLean.Apex.Matrix3.invert.impl] invert

noncomputable
def lerp.impl (a b : Matrix3) (t : Float) : Matrix3 := LerpMatrix3 a b t

attribute [apex_implemented_by HouLean.Apex.Matrix3.lerp.impl] lerp

noncomputable
def almostEquals.impl (a b : Matrix3) (tolerance : Float) : Bool := AlmostEqualsMatrix3 a b tolerance

attribute [apex_implemented_by HouLean.Apex.Matrix3.almostEquals.impl] almostEquals

end Matrix3
