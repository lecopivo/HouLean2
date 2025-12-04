import HouLean.Math
import HouLean.Data.Float
import HouLean.Data.Defs

open HouLean.Math

namespace HouLean.Matrix2

-- Note: Matrix2 is row-based: structure Matrix2 where row0 row1 : Vector2

-- ============================================================================
-- Arithmetic operations
-- ============================================================================

defun add (a b : Matrix2) : Matrix2 :=
  ⟨⟨a.row0.x + b.row0.x, a.row0.y + b.row0.y⟩,
   ⟨a.row1.x + b.row1.x, a.row1.y + b.row1.y⟩⟩

defun sub (a b : Matrix2) : Matrix2 :=
  ⟨⟨a.row0.x - b.row0.x, a.row0.y - b.row0.y⟩,
   ⟨a.row1.x - b.row1.x, a.row1.y - b.row1.y⟩⟩

defun neg (a : Matrix2) : Matrix2 :=
  ⟨⟨-a.row0.x, -a.row0.y⟩,
   ⟨-a.row1.x, -a.row1.y⟩⟩

defun hMul (a : Matrix2) (s : Float) : Matrix2 :=
  ⟨⟨a.row0.x * s, a.row0.y * s⟩,
   ⟨a.row1.x * s, a.row1.y * s⟩⟩

instance : HMul Float Matrix2 Matrix2 := ⟨fun s a => a * s⟩

defun hDiv (a : Matrix2) (s : Float) : Matrix2 :=
  ⟨⟨a.row0.x / s, a.row0.y / s⟩,
   ⟨a.row1.x / s, a.row1.y / s⟩⟩


-- ============================================================================
-- Vector/Matrix operations
-- ============================================================================

defun dot (a b : Matrix2) : Float :=
  a.row0.x * b.row0.x + a.row0.y * b.row0.y +
  a.row1.x * b.row1.x + a.row1.y * b.row1.y

defun length (m : Matrix2) : Float :=
  Float.sqrt (m.row0.x * m.row0.x + m.row0.y * m.row0.y +
              m.row1.x * m.row1.x + m.row1.y * m.row1.y)

defun length2 (m : Matrix2) : Float :=
  m.row0.x * m.row0.x + m.row0.y * m.row0.y +
  m.row1.x * m.row1.x + m.row1.y * m.row1.y

defun normalize (m : Matrix2) : Matrix2 × Float :=
  let len := m.length
  if len == 0 then (m, 0)
  else (⟨⟨m.row0.x / len, m.row0.y / len⟩,
         ⟨m.row1.x / len, m.row1.y / len⟩⟩, len)

defun normalized (m : Matrix2) : Matrix2 :=
  m.normalize.1

defun distance (a b : Matrix2) : Float :=
  (a.sub b).length

defun distance2 (a b : Matrix2) : Float :=
  (b - a).length2

defun lerp (a b : Matrix2) (t : Float) : Matrix2 :=
  ⟨⟨a.row0.x + (b.row0.x - a.row0.x) * t, a.row0.y + (b.row0.y - a.row0.y) * t⟩,
   ⟨a.row1.x + (b.row1.x - a.row1.x) * t, a.row1.y + (b.row1.y - a.row1.y) * t⟩⟩

defun reflect (v n : Matrix2) : Matrix2 :=
  let d := v.dot n
  ⟨⟨v.row0.x - 2.0 * d * n.row0.x, v.row0.y - 2.0 * d * n.row0.y⟩,
   ⟨v.row1.x - 2.0 * d * n.row1.x, v.row1.y - 2.0 * d * n.row1.y⟩⟩

defun refract (v n : Matrix2) (eta : Float) : Matrix2 :=
  let dt := v.dot n
  let k := 1.0 - eta * eta * (1.0 - dt * dt)
  if k < 0.0 then ⟨⟨0.0, 0.0⟩, ⟨0.0, 0.0⟩⟩
  else
    let s := eta * dt + k.sqrt
    ⟨⟨eta * v.row0.x - s * n.row0.x, eta * v.row0.y - s * n.row0.y⟩,
     ⟨eta * v.row1.x - s * n.row1.x, eta * v.row1.y - s * n.row1.y⟩⟩

defun compMul (m n : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x * n.row0.x, m.row0.y * n.row0.y⟩,
   ⟨m.row1.x * n.row1.x, m.row1.y * n.row1.y⟩⟩

defun compDiv (m n : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x / n.row0.x, m.row0.y / n.row0.y⟩,
   ⟨m.row1.x / n.row1.x, m.row1.y / n.row1.y⟩⟩

-- ============================================================================
-- Comparison operations
-- ============================================================================

instance : BEq Matrix2 where
  beq a b :=
    a.row0.x == b.row0.x && a.row0.y == b.row0.y &&
    a.row1.x == b.row1.x && a.row1.y == b.row1.y

instance : LT Matrix2 where
  lt a b :=
    a.row0.x < b.row0.x && a.row0.y < b.row0.y &&
    a.row1.x < b.row1.x && a.row1.y < b.row1.y

instance : DecidableLT Matrix2 := by
  intros; simp[DecidableLT, DecidableRel, LT.lt]; infer_instance

instance : LE Matrix2 where
  le a b :=
    a.row0.x <= b.row0.x && a.row0.y <= b.row0.y &&
    a.row1.x <= b.row1.x && a.row1.y <= b.row1.y

instance : DecidableLE Matrix2 := by
  intros; simp[DecidableLE, DecidableRel, LE.le]; infer_instance

-- ============================================================================
-- Component-wise operations
-- ============================================================================

defun abs (m : Matrix2) : Matrix2 :=
  ⟨⟨Float.abs m.row0.x, Float.abs m.row0.y⟩,
   ⟨Float.abs m.row1.x, Float.abs m.row1.y⟩⟩

defun min (a b : Matrix2) : Matrix2 :=
  ⟨⟨Min.min a.row0.x b.row0.x, Min.min a.row0.y b.row0.y⟩,
   ⟨Min.min a.row1.x b.row1.x, Min.min a.row1.y b.row1.y⟩⟩

defun max (a b : Matrix2) : Matrix2 :=
  ⟨⟨Max.max a.row0.x b.row0.x, Max.max a.row0.y b.row0.y⟩,
   ⟨Max.max a.row1.x b.row1.x, Max.max a.row1.y b.row1.y⟩⟩

-- defun sign (m : Matrix2) : Matrix2 :=
--   ⟨⟨m.row0.x.sign, m.row0.y.sign⟩,
--    ⟨m.row1.x.sign, m.row1.y.sign⟩⟩

-- defun clamp (m lo hi : Matrix2) : Matrix2 :=
--   ⟨⟨m.row0.x.clamp lo.row0.x hi.row0.x, m.row0.y.clamp lo.row0.y hi.row0.y⟩,
--    ⟨m.row1.x.clamp lo.row1.x hi.row1.x, m.row1.y.clamp lo.row1.y hi.row1.y⟩⟩

defun floor (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.floor, m.row0.y.floor⟩,
   ⟨m.row1.x.floor, m.row1.y.floor⟩⟩

defun ceil (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.ceil, m.row0.y.ceil⟩,
   ⟨m.row1.x.ceil, m.row1.y.ceil⟩⟩

defun round (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.round, m.row0.y.round⟩,
   ⟨m.row1.x.round, m.row1.y.round⟩⟩

-- defun trunc (m : Matrix2) : Matrix2 :=
--   ⟨⟨m.row0.x.trunc, m.row0.y.trunc⟩,
--    ⟨m.row1.x.trunc, m.row1.y.trunc⟩⟩

-- defun fract (m : Matrix2) : Matrix2 :=
--   ⟨⟨m.row0.x.fract, m.row0.y.fract⟩,
--    ⟨m.row1.x.fract, m.row1.y.fract⟩⟩

-- defun mod (m n : Matrix2) : Matrix2 :=
--   ⟨⟨m.row0.x.mod n.row0.x, m.row0.y.mod n.row0.y⟩,
--    ⟨m.row1.x.mod n.row1.x, m.row1.y.mod n.row1.y⟩⟩

-- ============================================================================
-- Trigonometric Functions (elementwise)
-- ============================================================================

defun sin (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.sin, m.row0.y.sin⟩,
   ⟨m.row1.x.sin, m.row1.y.sin⟩⟩

defun cos (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.cos, m.row0.y.cos⟩,
   ⟨m.row1.x.cos, m.row1.y.cos⟩⟩

defun tan (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.tan, m.row0.y.tan⟩,
   ⟨m.row1.x.tan, m.row1.y.tan⟩⟩

defun asin (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.asin, m.row0.y.asin⟩,
   ⟨m.row1.x.asin, m.row1.y.asin⟩⟩

defun acos (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.acos, m.row0.y.acos⟩,
   ⟨m.row1.x.acos, m.row1.y.acos⟩⟩

defun atan (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.atan, m.row0.y.atan⟩,
   ⟨m.row1.x.atan, m.row1.y.atan⟩⟩

defun atan2 (m n : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.atan2 n.row0.x, m.row0.y.atan2 n.row0.y⟩,
   ⟨m.row1.x.atan2 n.row1.x, m.row1.y.atan2 n.row1.y⟩⟩

defun sinh (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.sinh, m.row0.y.sinh⟩,
   ⟨m.row1.x.sinh, m.row1.y.sinh⟩⟩

defun cosh (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.cosh, m.row0.y.cosh⟩,
   ⟨m.row1.x.cosh, m.row1.y.cosh⟩⟩

defun tanh (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.tanh, m.row0.y.tanh⟩,
   ⟨m.row1.x.tanh, m.row1.y.tanh⟩⟩

-- ============================================================================
-- Exponential and Logarithmic Functions (elementwise)
-- ============================================================================

defun exp (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.exp, m.row0.y.exp⟩,
   ⟨m.row1.x.exp, m.row1.y.exp⟩⟩

defun exp2 (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.exp2, m.row0.y.exp2⟩,
   ⟨m.row1.x.exp2, m.row1.y.exp2⟩⟩

defun log (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.log, m.row0.y.log⟩,
   ⟨m.row1.x.log, m.row1.y.log⟩⟩

defun log2 (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.log2, m.row0.y.log2⟩,
   ⟨m.row1.x.log2, m.row1.y.log2⟩⟩

defun log10 (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.log10, m.row0.y.log10⟩,
   ⟨m.row1.x.log10, m.row1.y.log10⟩⟩

defun pow (m n : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.pow n.row0.x, m.row0.y.pow n.row0.y⟩,
   ⟨m.row1.x.pow n.row1.x, m.row1.y.pow n.row1.y⟩⟩

defun sqrt (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.sqrt, m.row0.y.sqrt⟩,
   ⟨m.row1.x.sqrt, m.row1.y.sqrt⟩⟩

-- defun invsqrt (m : Matrix2) : Matrix2 :=
--   ⟨⟨m.row0.x.invsqrt, m.row0.y.invsqrt⟩,
--    ⟨m.row1.x.invsqrt, m.row1.y.invsqrt⟩⟩

-- ============================================================================
-- Interpolation and Smoothing (elementwise)
-- ============================================================================

-- defun smoothstep (edge0 edge1 m : Matrix2) : Matrix2 :=
--   ⟨⟨edge0.row0.x.smoothstep edge1.row0.x m.row0.x, edge0.row0.y.smoothstep edge1.row0.y m.row0.y⟩,
--    ⟨edge0.row1.x.smoothstep edge1.row1.x m.row1.x, edge0.row1.y.smoothstep edge1.row1.y m.row1.y⟩⟩

-- defun step (edge m : Matrix2) : Matrix2 :=
--   ⟨⟨edge.row0.x.step m.row0.x, edge.row0.y.step m.row0.y⟩,
--    ⟨edge.row1.x.step m.row1.x, edge.row1.y.step m.row1.y⟩⟩

-- defun hermite (p0 p1 t0 t1 : Matrix2) (t : Float) : Matrix2 :=
--   ⟨⟨p0.row0.x.hermite p1.row0.x t0.row0.x t1.row0.x t, p0.row0.y.hermite p1.row0.y t0.row0.y t1.row0.y t⟩,
--    ⟨p0.row1.x.hermite p1.row1.x t0.row1.x t1.row1.x t, p0.row1.y.hermite p1.row1.y t0.row1.y t1.row1.y t⟩⟩

-- defun catmullRom (p0 p1 p2 p3 : Matrix2) (t : Float) : Matrix2 :=
--   ⟨⟨p0.row0.x.catmullRom p1.row0.x p2.row0.x p3.row0.x t, p0.row0.y.catmullRom p1.row0.y p2.row0.y p3.row0.y t⟩,
--    ⟨p0.row1.x.catmullRom p1.row1.x p2.row1.x p3.row1.x t, p0.row1.y.catmullRom p1.row1.y p2.row1.y p3.row1.y t⟩⟩

-- defun slerp (m n : Matrix2) (t : Float) : Matrix2 :=
--   let dot := m.normalized.dot n.normalized
--   let dot := dot.clamp (-1.0) 1.0
--   let theta := dot.acos
--   if theta.abs < 0.001 then m.lerp n t
--   else
--     let s := theta.sin
--     let a := ((1.0 - t) * theta).sin / s
--     let b := (t * theta).sin / s
--     ⟨⟨a * m.row0.x + b * n.row0.x, a * m.row0.y + b * n.row0.y⟩,
--      ⟨a * m.row1.x + b * n.row1.x, a * m.row1.y + b * n.row1.y⟩⟩

-- ============================================================================
-- Conversion and Construction
-- ============================================================================

defun radians (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.radians, m.row0.y.radians⟩,
   ⟨m.row1.x.radians, m.row1.y.radians⟩⟩

defun degrees (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x.degrees, m.row0.y.degrees⟩,
   ⟨m.row1.x.degrees, m.row1.y.degrees⟩⟩

-- ============================================================================
-- Geometric Queries
-- ============================================================================

defun insideBox (point boxMin boxMax : Matrix2) : Bool :=
  point.row0.x >= boxMin.row0.x && point.row0.x <= boxMax.row0.x &&
  point.row0.y >= boxMin.row0.y && point.row0.y <= boxMax.row0.y &&
  point.row1.x >= boxMin.row1.x && point.row1.x <= boxMax.row1.x &&
  point.row1.y >= boxMin.row1.y && point.row1.y <= boxMax.row1.y

-- defun projectToSegment (point a b : Matrix2) : Matrix2 :=
--   let ab := b - a
--   let ap := point - a
--   let t := ((ap.dot ab) / (ab.dot ab)).clamp 0.0 1.0
--   ⟨⟨a.row0.x + t * ab.row0.x, a.row0.y + t * ab.row0.y⟩,
--    ⟨a.row1.x + t * ab.row1.x, a.row1.y + t * ab.row1.y⟩⟩

end HouLean.Matrix2
