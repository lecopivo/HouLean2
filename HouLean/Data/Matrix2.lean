import HouLean.Data.Vector2

namespace HouLean.Matrix2

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

end Matrix2
