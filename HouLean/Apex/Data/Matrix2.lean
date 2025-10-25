import HouLean.Apex.Data.Float
import HouLean.Apex.Data.Vector2

open HouLean.Apex.Generated

namespace HouLean.Apex.Matrix2

-- Helper to create identity matrix
def identity : Matrix2 :=
  ⟨⟨1, 0⟩, ⟨0, 1⟩⟩

-- Helper to create zero matrix
def zero : Matrix2 :=
  ⟨⟨0, 0⟩, ⟨0, 0⟩⟩

-- Arithmetic operations (Lean implementations)
def add (a b : Matrix2) : Matrix2 :=
  ⟨a.row0 + b.row0, a.row1 + b.row1⟩

def sub (a b : Matrix2) : Matrix2 :=
  ⟨a.row0 - b.row0, a.row1 - b.row1⟩

def mulScalar (a : Matrix2) (s : Float) : Matrix2 :=
  ⟨a.row0 * s, a.row1 * s⟩

def mul (a b : Matrix2) : Matrix2 :=
  -- Matrix multiplication: result[i,j] = sum(a[i,k] * b[k,j])
  let col0 := ⟨b.row0.x, b.row1.x⟩
  let col1 := ⟨b.row0.y, b.row1.y⟩
  ⟨⟨a.row0.dot col0, a.row0.dot col1⟩,
   ⟨a.row1.dot col0, a.row1.dot col1⟩⟩

-- Vector-matrix multiplication: v * M (vector on the left, Houdini style)
def mulVector (v : Vector2) (m : Matrix2) : Vector2 :=
  let col0 := ⟨m.row0.x, m.row1.x⟩
  let col1 := ⟨m.row0.y, m.row1.y⟩
  ⟨v.dot col0, v.dot col1⟩

-- Matrix operations (Lean implementations)
def transpose (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x, m.row1.x⟩,
   ⟨m.row0.y, m.row1.y⟩⟩

def determinant (m : Matrix2) : Float :=
  m.row0.x * m.row1.y - m.row0.y * m.row1.x

def invert (m : Matrix2) : Matrix2 :=
  let det := m.determinant
  if det == 0 then m else
    let invDet := 1 / det
    ⟨⟨m.row1.y * invDet, -m.row0.y * invDet⟩,
     ⟨-m.row1.x * invDet, m.row0.x * invDet⟩⟩

-- Comparison operations (Lean implementations)
def beq (a b : Matrix2) : Bool :=
  a.row0 == b.row0 && a.row1 == b.row1

def almostEquals (a b : Matrix2) (tolerance : Float) : Bool :=
  let diff0 := a.row0 - b.row0
  let diff1 := a.row1 - b.row1
  diff0.lengthSquared <= tolerance * tolerance &&
  diff1.lengthSquared <= tolerance * tolerance

-- Linear interpolation
def lerp (a b : Matrix2) (t : Float) : Matrix2 :=
  ⟨a.row0.lerp b.row0 t,
   a.row1.lerp b.row1 t⟩

-- Rotation matrix creation
def rotation (angle : Float) : Matrix2 :=
  let c := Float.cos angle
  let s := Float.sin angle
  ⟨⟨c, -s⟩, ⟨s, c⟩⟩

-- Scale matrix creation
def scale (sx sy : Float) : Matrix2 :=
  ⟨⟨sx, 0⟩, ⟨0, sy⟩⟩

-- Operator overloading instances
instance : Add Matrix2 where
  add := add

instance : Sub Matrix2 where
  sub := sub

instance : Mul Matrix2 where
  mul := mul

instance : HMul Matrix2 Float Matrix2 where
  hMul := mulScalar

-- Vector * Matrix (Houdini style: vector on the left)
instance : HMul Vector2 Matrix2 Vector2 where
  hMul := mulVector

instance : BEq Matrix2 where
  beq := beq

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
def mk.impl (row0 row1 : Vector2) : Matrix2 := 
  let m3 := Vector3ToMatrix3 ⟨row0.x, row0.y, 0⟩ ⟨row1.x, row1.y, 0⟩ ⟨0, 0, 1⟩
  fromMatrix3 m3

attribute [apex_implemented_by HouLean.Apex.Matrix2.mk.impl] mk

-- Row accessors
noncomputable
def row0.impl (m : Matrix2) : Vector2 := 
  let m3 := toMatrix3 m
  let result := Matrix3ToVector3 m3
  ⟨result.1.x, result.1.y⟩

attribute [apex_implemented_by HouLean.Apex.Matrix2.row0.impl] row0

noncomputable
def row1.impl (m : Matrix2) : Vector2 := 
  let m3 := toMatrix3 m
  let result := Matrix3ToVector3 m3
  ⟨result.2.1.x, result.2.1.y⟩

attribute [apex_implemented_by HouLean.Apex.Matrix2.row1.impl] row1

-- Arithmetic operations (implemented via Matrix3)
noncomputable
def add.impl (a b : Matrix2) : Matrix2 := 
  let a3 := toMatrix3 a
  let b3 := toMatrix3 b
  fromMatrix3 (AddMatrix3 a3 #v[b3])

attribute [apex_implemented_by HouLean.Apex.Matrix2.add.impl] add

noncomputable
def sub.impl (a b : Matrix2) : Matrix2 := 
  let a3 := toMatrix3 a
  let b3 := toMatrix3 b
  fromMatrix3 (SubtractMatrix3 a3 #v[b3])

attribute [apex_implemented_by HouLean.Apex.Matrix2.sub.impl] sub

noncomputable
def mul.impl (a b : Matrix2) : Matrix2 := 
  let a3 := toMatrix3 a
  let b3 := toMatrix3 b
  fromMatrix3 (MultiplyMatrix3 a3 #v[b3])

attribute [apex_implemented_by HouLean.Apex.Matrix2.mul.impl] mul

noncomputable
def mulScalar.impl (a : Matrix2) (s : Float) : Matrix2 := 
  let a3 := toMatrix3 a
  fromMatrix3 (MultiplyMatrix3Float a3 #v[s])

attribute [apex_implemented_by HouLean.Apex.Matrix2.mulScalar.impl] mulScalar

noncomputable
def mulVector.impl (v : Vector2) (m : Matrix2) : Vector2 := 
  let v3 := ⟨v.x, v.y, 0⟩
  let m3 := toMatrix3 m
  let result := MultiplyVector3Matrix3 v3 #v[m3]
  ⟨result.x, result.y⟩

attribute [apex_implemented_by HouLean.Apex.Matrix2.mulVector.impl] mulVector

noncomputable
def transpose.impl (m : Matrix2) : Matrix2 := 
  let m3 := toMatrix3 m
  fromMatrix3 (TransposeMatrix3 m3)

attribute [apex_implemented_by HouLean.Apex.Matrix2.transpose.impl] transpose

noncomputable
def invert.impl (m : Matrix2) : Matrix2 := 
  let m3 := toMatrix3 m
  fromMatrix3 (InvertMatrix3 m3)

attribute [apex_implemented_by HouLean.Apex.Matrix2.invert.impl] invert

noncomputable
def lerp.impl (a b : Matrix2) (t : Float) : Matrix2 := 
  let a3 := toMatrix3 a
  let b3 := toMatrix3 b
  fromMatrix3 (LerpMatrix3 a3 b3 t)

attribute [apex_implemented_by HouLean.Apex.Matrix2.lerp.impl] lerp

end Matrix2
