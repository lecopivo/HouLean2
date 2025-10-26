import HouLean.Data.Vector3
import HouLean.Data.Vector4
import HouLean.Data.Matrix3

namespace HouLean.Matrix4

-- Helper to create identity matrix
def identity : Matrix4 :=
  ⟨⟨1, 0, 0, 0⟩, ⟨0, 1, 0, 0⟩, ⟨0, 0, 1, 0⟩, ⟨0, 0, 0, 1⟩⟩

-- Helper to create zero matrix
def zero : Matrix4 :=
  ⟨⟨0, 0, 0, 0⟩, ⟨0, 0, 0, 0⟩, ⟨0, 0, 0, 0⟩, ⟨0, 0, 0, 0⟩⟩

-- Arithmetic operations (Lean implementations)
def add (a b : Matrix4) : Matrix4 :=
  ⟨a.row0 + b.row0, a.row1 + b.row1, a.row2 + b.row2, a.row3 + b.row3⟩

def sub (a b : Matrix4) : Matrix4 :=
  ⟨a.row0 - b.row0, a.row1 - b.row1, a.row2 - b.row2, a.row3 - b.row3⟩

def mulScalar (a : Matrix4) (s : Float) : Matrix4 :=
  ⟨a.row0 * s, a.row1 * s, a.row2 * s, a.row3 * s⟩

def mul (a b : Matrix4) : Matrix4 :=
  let col0 := ⟨b.row0.x, b.row1.x, b.row2.x, b.row3.x⟩
  let col1 := ⟨b.row0.y, b.row1.y, b.row2.y, b.row3.y⟩
  let col2 := ⟨b.row0.z, b.row1.z, b.row2.z, b.row3.z⟩
  let col3 := ⟨b.row0.w, b.row1.w, b.row2.w, b.row3.w⟩
  ⟨⟨a.row0.dot col0, a.row0.dot col1, a.row0.dot col2, a.row0.dot col3⟩,
   ⟨a.row1.dot col0, a.row1.dot col1, a.row1.dot col2, a.row1.dot col3⟩,
   ⟨a.row2.dot col0, a.row2.dot col1, a.row2.dot col2, a.row2.dot col3⟩,
   ⟨a.row3.dot col0, a.row3.dot col1, a.row3.dot col2, a.row3.dot col3⟩⟩

def mulMatrix3 (a : Matrix4) (b : Matrix3) : Matrix4 :=
  let col0 := ⟨b.row0.x, b.row1.x, b.row2.x, 0⟩
  let col1 := ⟨b.row0.y, b.row1.y, b.row2.y, 0⟩
  let col2 := ⟨b.row0.z, b.row1.z, b.row2.z, 0⟩
  let col3 := ⟨0, 0, 0, 1⟩
  ⟨⟨a.row0.dot col0, a.row0.dot col1, a.row0.dot col2, a.row0.dot col3⟩,
   ⟨a.row1.dot col0, a.row1.dot col1, a.row1.dot col2, a.row1.dot col3⟩,
   ⟨a.row2.dot col0, a.row2.dot col1, a.row2.dot col2, a.row2.dot col3⟩,
   ⟨a.row3.dot col0, a.row3.dot col1, a.row3.dot col2, a.row3.dot col3⟩⟩

-- Vector-matrix multiplication: v * M (vector on the left, Houdini style)
def mulVector3 (v : Vector3) (m : Matrix4) : Vector3 :=
  let col0 : Vector4 := ⟨m.row0.x, m.row1.x, m.row2.x, m.row3.x⟩
  let col1 : Vector4 := ⟨m.row0.y, m.row1.y, m.row2.y, m.row3.y⟩
  let col2 : Vector4 := ⟨m.row0.z, m.row1.z, m.row2.z, m.row3.z⟩
  let col3 : Vector4 := ⟨m.row0.w, m.row1.w, m.row2.w, m.row3.w⟩
  let v4 : Vector4 := ⟨v.x, v.y, v.z, 1⟩
  let x := v4.dot col0
  let y := v4.dot col1
  let z := v4.dot col2
  let w := v4.dot col3
  if w == 0 then ⟨x, y, z⟩ else ⟨x / w, y / w, z / w⟩

def mulVector4 (v : Vector4) (m : Matrix4) : Vector4 :=
  let col0 := ⟨m.row0.x, m.row1.x, m.row2.x, m.row3.x⟩
  let col1 := ⟨m.row0.y, m.row1.y, m.row2.y, m.row3.y⟩
  let col2 := ⟨m.row0.z, m.row1.z, m.row2.z, m.row3.z⟩
  let col3 := ⟨m.row0.w, m.row1.w, m.row2.w, m.row3.w⟩
  ⟨v.dot col0, v.dot col1, v.dot col2, v.dot col3⟩

-- Matrix operations (Lean implementations)
def transpose (m : Matrix4) : Matrix4 :=
  ⟨⟨m.row0.x, m.row1.x, m.row2.x, m.row3.x⟩,
   ⟨m.row0.y, m.row1.y, m.row2.y, m.row3.y⟩,
   ⟨m.row0.z, m.row1.z, m.row2.z, m.row3.z⟩,
   ⟨m.row0.w, m.row1.w, m.row2.w, m.row3.w⟩⟩

def determinant (m : Matrix4) : Float :=
  let minor00 := 
    m.row1.y * (m.row2.z * m.row3.w - m.row2.w * m.row3.z) -
    m.row1.z * (m.row2.y * m.row3.w - m.row2.w * m.row3.y) +
    m.row1.w * (m.row2.y * m.row3.z - m.row2.z * m.row3.y)
  let minor01 :=
    m.row1.x * (m.row2.z * m.row3.w - m.row2.w * m.row3.z) -
    m.row1.z * (m.row2.x * m.row3.w - m.row2.w * m.row3.x) +
    m.row1.w * (m.row2.x * m.row3.z - m.row2.z * m.row3.x)
  let minor02 :=
    m.row1.x * (m.row2.y * m.row3.w - m.row2.w * m.row3.y) -
    m.row1.y * (m.row2.x * m.row3.w - m.row2.w * m.row3.x) +
    m.row1.w * (m.row2.x * m.row3.y - m.row2.y * m.row3.x)
  let minor03 :=
    m.row1.x * (m.row2.y * m.row3.z - m.row2.z * m.row3.y) -
    m.row1.y * (m.row2.x * m.row3.z - m.row2.z * m.row3.x) +
    m.row1.z * (m.row2.x * m.row3.y - m.row2.y * m.row3.x)
  m.row0.x * minor00 - m.row0.y * minor01 + m.row0.z * minor02 - m.row0.w * minor03

def invert (m : Matrix4) : Matrix4 :=
  let det := m.determinant
  if det == 0 then m else m

-- Comparison operations (Lean implementations)
def beq (a b : Matrix4) : Bool :=
  a.row0 == b.row0 && a.row1 == b.row1 && a.row2 == b.row2 && a.row3 == b.row3

def almostEquals (a b : Matrix4) (tolerance : Float) : Bool :=
  let diff0 := a.row0 - b.row0
  let diff1 := a.row1 - b.row1
  let diff2 := a.row2 - b.row2
  let diff3 := a.row3 - b.row3
  diff0.lengthSquared <= tolerance * tolerance &&
  diff1.lengthSquared <= tolerance * tolerance &&
  diff2.lengthSquared <= tolerance * tolerance &&
  diff3.lengthSquared <= tolerance * tolerance

-- Linear interpolation
def lerp (a b : Matrix4) (t : Float) : Matrix4 :=
  ⟨a.row0.lerp b.row0 t,
   a.row1.lerp b.row1 t,
   a.row2.lerp b.row2 t,
   a.row3.lerp b.row3 t⟩

-- Conversions
def toMatrix3 (m : Matrix4) : Matrix3 :=
  ⟨⟨m.row0.x, m.row0.y, m.row0.z⟩,
   ⟨m.row1.x, m.row1.y, m.row1.z⟩,
   ⟨m.row2.x, m.row2.y, m.row2.z⟩⟩

def fromMatrix3 (m : Matrix3) : Matrix4 :=
  ⟨⟨m.row0.x, m.row0.y, m.row0.z, 0⟩,
   ⟨m.row1.x, m.row1.y, m.row1.z, 0⟩,
   ⟨m.row2.x, m.row2.y, m.row2.z, 0⟩,
   ⟨0, 0, 0, 1⟩⟩

-- Operator overloading instances
instance : Add Matrix4 where
  add := add

instance : Sub Matrix4 where
  sub := sub

instance : Mul Matrix4 where
  mul := mul

instance : HMul Matrix4 Float Matrix4 where
  hMul := mulScalar

instance : HMul Matrix4 Matrix3 Matrix4 where
  hMul := mulMatrix3

-- Vector * Matrix (Houdini style: vector on the left)
instance : HMul Vector3 Matrix4 Vector3 where
  hMul := mulVector3

instance : HMul Vector4 Matrix4 Vector4 where
  hMul := mulVector4

instance : BEq Matrix4 where
  beq := beq

end Matrix4
