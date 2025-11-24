import HouLean.Math
import HouLean.Data.Vector3
import HouLean.Data.Defs

open HouLean.Math

namespace HouLean.Matrix3

-- Note: Matrix3 is row-based: structure Matrix3 where row0 row1 row2 : Vector3

-- ============================================================================
-- Arithmetic operations
-- ============================================================================

defun add (a b : Matrix3) : Matrix3 :=
  ⟨a.row0.add b.row0, a.row1.add b.row1, a.row2.add b.row2⟩

defun sub (a b : Matrix3) : Matrix3 :=
  ⟨a.row0.sub b.row0, a.row1.sub b.row1, a.row2.sub b.row2⟩

defun neg (a : Matrix3) : Matrix3 :=
  ⟨a.row0.neg, a.row1.neg, a.row2.neg⟩

def smul (a : Matrix3) (s : Float) : Matrix3 :=
  ⟨a.row0 * s, a.row1 * s, a.row2 * s⟩

instance : HMul Float Matrix3 Matrix3 := ⟨fun s m => smul m s⟩
instance : HMul Matrix3 Float Matrix3 := ⟨smul⟩

defun hDiv (a : Matrix3) (s : Float) : Matrix3 :=
  ⟨a.row0.hDiv s, a.row1.hDiv s, a.row2.hDiv s⟩

-- ============================================================================
-- Matrix operations
-- ============================================================================

def matmul (a b : Matrix3) : Matrix3 :=
  ⟨⟨a.row0.x * b.row0.x + a.row0.y * b.row1.x + a.row0.z * b.row2.x,
    a.row0.x * b.row0.y + a.row0.y * b.row1.y + a.row0.z * b.row2.y,
    a.row0.x * b.row0.z + a.row0.y * b.row1.z + a.row0.z * b.row2.z⟩,
   ⟨a.row1.x * b.row0.x + a.row1.y * b.row1.x + a.row1.z * b.row2.x,
    a.row1.x * b.row0.y + a.row1.y * b.row1.y + a.row1.z * b.row2.y,
    a.row1.x * b.row0.z + a.row1.y * b.row1.z + a.row1.z * b.row2.z⟩,
   ⟨a.row2.x * b.row0.x + a.row2.y * b.row1.x + a.row2.z * b.row2.x,
    a.row2.x * b.row0.y + a.row2.y * b.row1.y + a.row2.z * b.row2.y,
    a.row2.x * b.row0.z + a.row2.y * b.row1.z + a.row2.z * b.row2.z⟩⟩

instance : HMul Matrix3 Matrix3 Matrix3 := ⟨matmul⟩

def mulVec (m : Matrix3) (v : Vector3) : Vector3 :=
  ⟨m.row0.dot v, m.row1.dot v, m.row2.dot v⟩

instance : HMul Matrix3 Vector3 Vector3 := ⟨mulVec⟩

def vecMul (v : Vector3) (m : Matrix3) : Vector3 :=
  ⟨v.x * m.row0.x + v.y * m.row1.x + v.z * m.row2.x,
   v.x * m.row0.y + v.y * m.row1.y + v.z * m.row2.y,
   v.x * m.row0.z + v.y * m.row1.z + v.z * m.row2.z⟩

instance : HMul Vector3 Matrix3 Vector3 := ⟨vecMul⟩

-- ============================================================================
-- Vector/Matrix operations
-- ============================================================================

defun dot (a b : Matrix3) : Float :=
  a.row0.dot b.row0 + a.row1.dot b.row1 + a.row2.dot b.row2

defun length (m : Matrix3) : Float :=
  Float.sqrt (m.row0.length2 + m.row1.length2 + m.row2.length2)

defun length2 (m : Matrix3) : Float :=
  m.row0.length2 + m.row1.length2 + m.row2.length2

defun normalize (m : Matrix3) : Matrix3 × Float :=
  let len := m.length
  if len == 0 then (m, 0)
  else (⟨m.row0.hDiv len, m.row1.hDiv len, m.row2.hDiv len⟩, len)

defun normalized (m : Matrix3) : Matrix3 :=
  m.normalize.1

defun distance (a b : Matrix3) : Float :=
  (a.sub b).length

defun distance2 (a b : Matrix3) : Float :=
  (a.sub b).length2

defun lerp (a b : Matrix3) (t : Float) : Matrix3 :=
  ⟨a.row0.lerp b.row0 t, a.row1.lerp b.row1 t, a.row2.lerp b.row2 t⟩

defun reflect (v n : Matrix3) : Matrix3 :=
  let d := v.dot n
  ⟨v.row0.reflect (d * n.row0), v.row1.reflect (d * n.row1), v.row2.reflect (d * n.row2)⟩

defun refract (v n : Matrix3) (eta : Float) : Matrix3 :=
  let dt := v.dot n
  let k := 1.0 - eta * eta * (1.0 - dt * dt)
  if k < 0.0 then ⟨⟨0.0, 0.0, 0.0⟩, ⟨0.0, 0.0, 0.0⟩, ⟨0.0, 0.0, 0.0⟩⟩
  else
    let s := eta * dt + k.sqrt
    ⟨⟨eta * v.row0.x - s * n.row0.x, eta * v.row0.y - s * n.row0.y, eta * v.row0.z - s * n.row0.z⟩,
     ⟨eta * v.row1.x - s * n.row1.x, eta * v.row1.y - s * n.row1.y, eta * v.row1.z - s * n.row1.z⟩,
     ⟨eta * v.row2.x - s * n.row2.x, eta * v.row2.y - s * n.row2.y, eta * v.row2.z - s * n.row2.z⟩⟩

defun compMul (m n : Matrix3) : Matrix3 :=
  ⟨m.row0.compMul n.row0, m.row1.compMul n.row1, m.row2.compMul n.row2⟩

defun compDiv (m n : Matrix3) : Matrix3 :=
  ⟨m.row0.compDiv n.row0, m.row1.compDiv n.row1, m.row2.compDiv n.row2⟩

-- ============================================================================
-- Comparison operations
-- ============================================================================

defun beq (a b : Matrix3) : Bool :=
  a.row0.beq b.row0 && a.row1.beq b.row1 && a.row2.beq b.row2

defun blt (a b : Matrix3) : Bool :=
  a.row0.blt b.row0 && a.row1.blt b.row1 && a.row2.blt b.row2

defun ble (a b : Matrix3) : Bool :=
  a.row0.ble b.row0 && a.row1.ble b.row1 && a.row2.ble b.row2

instance : BEq Matrix3 := ⟨beq⟩
instance : LT Matrix3 := ⟨fun x y => blt x y = true⟩
instance : LE Matrix3 := ⟨fun x y => ble x y = true⟩
instance : DecidableLT Matrix3 := by
  intros; simp[DecidableLT, DecidableRel, LT.lt]; infer_instance
instance : DecidableLE Matrix3 := by
  intros; simp[DecidableLE, DecidableRel, LE.le]; infer_instance

-- ============================================================================
-- Component-wise operations
-- ============================================================================

defun abs (m : Matrix3) : Matrix3 :=
  ⟨m.row0.abs, m.row1.abs, m.row2.abs⟩

defun min (a b : Matrix3) : Matrix3 :=
  ⟨a.row0.min b.row0, a.row1.min b.row1, a.row2.min b.row2⟩

defun max (a b : Matrix3) : Matrix3 :=
  ⟨a.row0.max b.row0, a.row1.max b.row1, a.row2.max b.row2⟩

defun sign (m : Matrix3) : Matrix3 :=
  ⟨m.row0.sign, m.row1.sign, m.row2.sign⟩

defun clamp (m lo hi : Matrix3) : Matrix3 :=
  ⟨m.row0.clamp lo.row0 hi.row0, m.row1.clamp lo.row1 hi.row1, m.row2.clamp lo.row2 hi.row2⟩

defun floor (m : Matrix3) : Matrix3 :=
  ⟨m.row0.floor, m.row1.floor, m.row2.floor⟩

defun ceil (m : Matrix3) : Matrix3 :=
  ⟨m.row0.ceil, m.row1.ceil, m.row2.ceil⟩

defun round (m : Matrix3) : Matrix3 :=
  ⟨m.row0.round, m.row1.round, m.row2.round⟩

defun trunc (m : Matrix3) : Matrix3 :=
  ⟨m.row0.trunc, m.row1.trunc, m.row2.trunc⟩

defun fract (m : Matrix3) : Matrix3 :=
  ⟨m.row0.fract, m.row1.fract, m.row2.fract⟩

defun mod (m n : Matrix3) : Matrix3 :=
  ⟨m.row0.mod n.row0, m.row1.mod n.row1, m.row2.mod n.row2⟩

-- ============================================================================
-- Trigonometric Functions (elementwise)
-- ============================================================================

defun sin (m : Matrix3) : Matrix3 :=
  ⟨m.row0.sin, m.row1.sin, m.row2.sin⟩

defun cos (m : Matrix3) : Matrix3 :=
  ⟨m.row0.cos, m.row1.cos, m.row2.cos⟩

defun tan (m : Matrix3) : Matrix3 :=
  ⟨m.row0.tan, m.row1.tan, m.row2.tan⟩

defun asin (m : Matrix3) : Matrix3 :=
  ⟨m.row0.asin, m.row1.asin, m.row2.asin⟩

defun acos (m : Matrix3) : Matrix3 :=
  ⟨m.row0.acos, m.row1.acos, m.row2.acos⟩

defun atan (m : Matrix3) : Matrix3 :=
  ⟨m.row0.atan, m.row1.atan, m.row2.atan⟩

defun atan2 (m n : Matrix3) : Matrix3 :=
  ⟨m.row0.atan2 n.row0, m.row1.atan2 n.row1, m.row2.atan2 n.row2⟩

defun sinh (m : Matrix3) : Matrix3 :=
  ⟨m.row0.sinh, m.row1.sinh, m.row2.sinh⟩

defun cosh (m : Matrix3) : Matrix3 :=
  ⟨m.row0.cosh, m.row1.cosh, m.row2.cosh⟩

defun tanh (m : Matrix3) : Matrix3 :=
  ⟨m.row0.tanh, m.row1.tanh, m.row2.tanh⟩

-- ============================================================================
-- Exponential and Logarithmic Functions (elementwise)
-- ============================================================================

defun exp (m : Matrix3) : Matrix3 :=
  ⟨m.row0.exp, m.row1.exp, m.row2.exp⟩

defun exp2 (m : Matrix3) : Matrix3 :=
  ⟨m.row0.exp2, m.row1.exp2, m.row2.exp2⟩

defun log (m : Matrix3) : Matrix3 :=
  ⟨m.row0.log, m.row1.log, m.row2.log⟩

defun log2 (m : Matrix3) : Matrix3 :=
  ⟨m.row0.log2, m.row1.log2, m.row2.log2⟩

defun log10 (m : Matrix3) : Matrix3 :=
  ⟨m.row0.log10, m.row1.log10, m.row2.log10⟩

defun pow (m n : Matrix3) : Matrix3 :=
  ⟨m.row0.pow n.row0, m.row1.pow n.row1, m.row2.pow n.row2⟩

defun sqrt (m : Matrix3) : Matrix3 :=
  ⟨m.row0.sqrt, m.row1.sqrt, m.row2.sqrt⟩

defun invsqrt (m : Matrix3) : Matrix3 :=
  ⟨m.row0.invsqrt, m.row1.invsqrt, m.row2.invsqrt⟩

-- ============================================================================
-- Interpolation and Smoothing (elementwise)
-- ============================================================================

defun smoothstep (edge0 edge1 m : Matrix3) : Matrix3 :=
  ⟨edge0.row0.smoothstep edge1.row0 m.row0,
   edge0.row1.smoothstep edge1.row1 m.row1,
   edge0.row2.smoothstep edge1.row2 m.row2⟩

defun step (edge m : Matrix3) : Matrix3 :=
  ⟨edge.row0.step m.row0, edge.row1.step m.row1, edge.row2.step m.row2⟩

defun hermite (p0 p1 t0 t1 : Matrix3) (t : Float) : Matrix3 :=
  ⟨p0.row0.hermite p1.row0 t0.row0 t1.row0 t,
   p0.row1.hermite p1.row1 t0.row1 t1.row1 t,
   p0.row2.hermite p1.row2 t0.row2 t1.row2 t⟩

defun catmullRom (p0 p1 p2 p3 : Matrix3) (t : Float) : Matrix3 :=
  ⟨p0.row0.catmullRom p1.row0 p2.row0 p3.row0 t,
   p0.row1.catmullRom p1.row1 p2.row1 p3.row1 t,
   p0.row2.catmullRom p1.row2 p2.row2 p3.row2 t⟩

defun slerp (m n : Matrix3) (t : Float) : Matrix3 :=
  let dot := m.normalized.dot n.normalized
  let dot := dot.clamp (-1.0) 1.0
  let theta := dot.acos
  if theta.abs < 0.001 then m.lerp n t
  else
    let s := theta.sin
    let a := ((1.0 - t) * theta).sin / s
    let b := (t * theta).sin / s
    ⟨⟨a * m.row0.x + b * n.row0.x, a * m.row0.y + b * n.row0.y, a * m.row0.z + b * n.row0.z⟩,
     ⟨a * m.row1.x + b * n.row1.x, a * m.row1.y + b * n.row1.y, a * m.row1.z + b * n.row1.z⟩,
     ⟨a * m.row2.x + b * n.row2.x, a * m.row2.y + b * n.row2.y, a * m.row2.z + b * n.row2.z⟩⟩

-- ============================================================================
-- Conversion and Construction
-- ============================================================================

defun radians (m : Matrix3) : Matrix3 :=
  ⟨m.row0.radians, m.row1.radians, m.row2.radians⟩

defun degrees (m : Matrix3) : Matrix3 :=
  ⟨m.row0.degrees, m.row1.degrees, m.row2.degrees⟩

-- ============================================================================
-- Geometric Queries
-- ============================================================================

defun insideBox (point boxMin boxMax : Matrix3) : Bool :=
  point.row0.insideBox boxMin.row0 boxMax.row0 &&
  point.row1.insideBox boxMin.row1 boxMax.row1 &&
  point.row2.insideBox boxMin.row2 boxMax.row2

defun projectToSegment (point a b : Matrix3) : Matrix3 :=
  let ab := b - a
  let ap := point - a
  let t := ((ap.dot ab) / (ab.dot ab)).clamp 0.0 1.0
  ⟨a.row0 + t * ab.row0, a.row1 + t * ab.row1, a.row2 + t * ab.row2⟩


-- ============================================================================
-- Rotations
-- ============================================================================

/-- Convert 3x3 rotation matrix to quaternion -/
def toQuat (m : Matrix3) : Vector4 :=
  let trace := m.row0.x + m.row1.y + m.row2.z

  if trace > 0.0 then
    let s := (trace + 1.0).sqrt * 2.0
    ⟨(m.row2.y - m.row1.z) / s,
     (m.row0.z - m.row2.x) / s,
     (m.row1.x - m.row0.y) / s,
     0.25 * s⟩
  else if m.row0.x > m.row1.y && m.row0.x > m.row2.z then
    let s := (1.0 + m.row0.x - m.row1.y - m.row2.z).sqrt * 2.0
    ⟨0.25 * s,
     (m.row0.y + m.row1.x) / s,
     (m.row0.z + m.row2.x) / s,
     (m.row2.y - m.row1.z) / s⟩
  else if m.row1.y > m.row2.z then
    let s := (1.0 + m.row1.y - m.row0.x - m.row2.z).sqrt * 2.0
    ⟨(m.row0.y + m.row1.x) / s,
     0.25 * s,
     (m.row1.z + m.row2.y) / s,
     (m.row0.z - m.row2.x) / s⟩
  else
    let s := (1.0 + m.row2.z - m.row0.x - m.row1.y).sqrt * 2.0
    ⟨(m.row0.z + m.row2.x) / s,
     (m.row1.z + m.row2.y) / s,
     0.25 * s,
     (m.row1.x - m.row0.y) / s⟩

end HouLean.Matrix3
