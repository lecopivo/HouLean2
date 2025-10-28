import HouLean.Math
import HouLean.Data.Defs
import HouLean.Data.Float

open HouLean.Math

namespace HouLean.Vector2

-- ============================================================================
-- Arithmetic operations
-- ============================================================================

defun add (a b : Vector2) : Vector2 :=
  ⟨a.x + b.x, a.y + b.y⟩

defun sub (a b : Vector2) : Vector2 :=
  ⟨a.x - b.x, a.y - b.y⟩

defun neg (a : Vector2) : Vector2 :=
  ⟨-a.x, -a.y⟩

defun hMul (a : Vector2) (s : Float) : Vector2 :=
  ⟨a.x * s, a.y * s⟩

-- defun hMul (s : Float) (a : Vector2) : Vector2 :=
--   ⟨a.x * s, a.y * s⟩

defun hDiv (a : Vector2) (s : Float) : Vector2 :=
  ⟨a.x / s, a.y / s⟩


-- ============================================================================
-- Vector operations
-- ============================================================================

defun dot (a b : Vector2) : Float :=
  a.x * b.x + a.y * b.y

defun length (v : Vector2) : Float :=
  Float.sqrt (v.x * v.x + v.y * v.y)

defun length2 (v : Vector2) : Float :=
  v.x * v.x + v.y * v.y

defun normalize (v : Vector2) : Vector2 × Float :=
  let len := v.length
  if len == 0 then (v, 0) else (⟨v.x / len, v.y / len⟩, len)

defun normalized (v : Vector2) : Vector2 :=
  v.normalize.1 

defun distance (a b : Vector2) : Float :=
  (a.sub b).length

defun lerp (a b : Vector2) (t : Float) : Vector2 :=
  ⟨a.x + (b.x - a.x) * t, a.y + (b.y - a.y) * t⟩

defun reflect (v n : Vector2) : Vector2 := 
  let d := v.dot n
  ⟨v.x - 2.0 * d * n.x, v.y - 2.0 * d * n.y⟩

defun refract (v n : Vector2) (eta : Float) : Vector2 := 
  let dt := v.dot n
  let k := 1.0 - eta * eta * (1.0 - dt * dt)
  if k < 0.0 then ⟨0.0, 0.0⟩ 
  else 
    let s := eta * dt + k.sqrt
    ⟨eta * v.x - s * n.x, eta * v.y - s * n.y⟩


-- ============================================================================
-- Comparison operations
-- ============================================================================

instance : BEq Vector2 where
  beq a b := a.x == b.x && a.y == b.y

instance : LT Vector2 where
  lt a b := a.x < b.x && a.y < b.y

instance : DecidableLT Vector2 := by 
  intros; simp[DecidableLT, DecidableRel, LT.lt]; infer_instance

instance : LE Vector2 where
  le a b := a.x <= b.x && a.y <= b.y

instance : DecidableLE Vector2 := by 
  intros; simp[DecidableLE, DecidableRel, LE.le]; infer_instance


-- ============================================================================
-- Component-wise operations
-- ============================================================================

defun abs (v : Vector2) : Vector2 :=
  ⟨Float.abs v.x, Float.abs v.y⟩

defun min (a b : Vector2) : Vector2 :=
  ⟨Min.min a.x b.x, Min.min a.y b.y⟩

defun max (a b : Vector2) : Vector2 :=
  ⟨Max.max a.x b.x, Max.max a.y b.y⟩

defun sign (v : Vector2) : Vector2 := 
  ⟨v.x.sign, v.y.sign⟩

defun clamp (v lo hi : Vector2) : Vector2 := 
  ⟨v.x.clamp lo.x hi.x, v.y.clamp lo.y hi.y⟩

defun floor (v : Vector2) : Vector2 := 
  ⟨v.x.floor, v.y.floor⟩

defun ceil (v : Vector2) : Vector2 := 
  ⟨v.x.ceil, v.y.ceil⟩

defun round (v : Vector2) : Vector2 := 
  ⟨v.x.round, v.y.round⟩

defun trunc (v : Vector2) : Vector2 := 
  ⟨v.x.trunc, v.y.trunc⟩

defun fract (v : Vector2) : Vector2 := 
  ⟨v.x.fract, v.y.fract⟩

defun mod (v w : Vector2) : Vector2 := 
  ⟨v.x.mod w.x, v.y.mod w.y⟩


-- ============================================================================
-- Coordinate system conversions
-- ============================================================================

def toPolar (v : Vector2) : Vector2 :=
  let r := v.length
  let theta := Float.atan2 v.y v.x
  ⟨r, theta⟩

def fromPolar (polar : Vector2) : Vector2 :=
  let r := polar.x
  let theta := polar.y
  ⟨r * Float.cos theta, r * Float.sin theta⟩


-- ============================================================================
-- Overloaded Math Functions
-- ============================================================================

-- ============================================================================
-- Trigonometric Functions (elementwise)
-- ============================================================================

defun sin (v : Vector2) : Vector2 := 
  ⟨v.x.sin, v.y.sin⟩

defun cos (v : Vector2) : Vector2 := 
  ⟨v.x.cos, v.y.cos⟩

defun tan (v : Vector2) : Vector2 := 
  ⟨v.x.tan, v.y.tan⟩

defun asin (v : Vector2) : Vector2 := 
  ⟨v.x.asin, v.y.asin⟩

defun acos (v : Vector2) : Vector2 := 
  ⟨v.x.acos, v.y.acos⟩

defun atan (v : Vector2) : Vector2 := 
  ⟨v.x.atan, v.y.atan⟩

defun atan2 (v w : Vector2) : Vector2 := 
  ⟨v.x.atan2 w.x, v.y.atan2 w.y⟩

defun sinh (v : Vector2) : Vector2 := 
  ⟨v.x.sinh, v.y.sinh⟩

defun cosh (v : Vector2) : Vector2 := 
  ⟨v.x.cosh, v.y.cosh⟩

defun tanh (v : Vector2) : Vector2 := 
  ⟨v.x.tanh, v.y.tanh⟩


-- ============================================================================
-- Exponential and Logarithmic Functions (elementwise)
-- ============================================================================

defun exp (v : Vector2) : Vector2 := 
  ⟨v.x.exp, v.y.exp⟩

defun exp2 (v : Vector2) : Vector2 := 
  ⟨v.x.exp2, v.y.exp2⟩

defun log (v : Vector2) : Vector2 := 
  ⟨v.x.log, v.y.log⟩

defun log2 (v : Vector2) : Vector2 := 
  ⟨v.x.log2, v.y.log2⟩

defun log10 (v : Vector2) : Vector2 := 
  ⟨v.x.log10, v.y.log10⟩

defun pow (v w : Vector2) : Vector2 := 
  ⟨v.x.pow w.x, v.y.pow w.y⟩

defun sqrt (v : Vector2) : Vector2 := 
  ⟨v.x.sqrt, v.y.sqrt⟩

defun invsqrt (v : Vector2) : Vector2 := 
  ⟨v.x.invsqrt, v.y.invsqrt⟩


-- ============================================================================
-- Interpolation and Smoothing (elementwise)
-- ============================================================================

defun smoothstep (edge0 edge1 v : Vector2) : Vector2 := 
  ⟨edge0.x.smoothstep edge1.x v.x, edge0.y.smoothstep edge1.y v.y⟩

defun step (edge v : Vector2) : Vector2 := 
  ⟨edge.x.step v.x, edge.y.step v.y⟩

defun hermite (p0 p1 t0 t1 : Vector2) (t : Float) : Vector2 := 
  ⟨p0.x.hermite p1.x t0.x t1.x t, p0.y.hermite p1.y t0.y t1.y t⟩

defun catmullRom (p0 p1 p2 p3 : Vector2) (t : Float) : Vector2 := 
  ⟨p0.x.catmullRom p1.x p2.x p3.x t, p0.y.catmullRom p1.y p2.y p3.y t⟩

defun slerp (v w : Vector2) (t : Float) : Vector2 := 
  let dot := v.normalized.dot w.normalized
  let dot := dot.clamp (-1.0) 1.0
  let theta := dot.acos
  if theta.abs < 0.001 then v.lerp w t
  else
    let s := theta.sin
    let a := ((1.0 - t) * theta).sin / s
    let b := (t * theta).sin / s
    ⟨a * v.x + b * w.x, a * v.y + b * w.y⟩

-- ============================================================================
-- Conversion and Construction
-- ============================================================================

defun radians (v : Vector2) : Vector2 := 
  ⟨v.x.radians, v.y.radians⟩

defun degrees (v : Vector2) : Vector2 := 
  ⟨v.x.degrees, v.y.degrees⟩


-- ============================================================================
-- Geometric Queries
-- ============================================================================

defun insideBox (point boxMin boxMax : Vector2) : Bool := 
  point.x >= boxMin.x && point.x <= boxMax.x &&
  point.y >= boxMin.y && point.y <= boxMax.y

defun projectToSegment (point a b : Vector2) : Vector2 := 
  let ab := b - a
  let ap := point - a
  let t := ((ap.dot ab) / (ab.dot ab)).clamp 0.0 1.0
  ⟨a.x + t * ab.x, a.y + t * ab.y⟩


-- ============================================================================
-- Additional Vector2 Specific Functions
-- ============================================================================

/-- Perpendicular vector (90° counter-clockwise rotation). -/
def perp (v : Vector2) : Vector2 := 
  ⟨-v.y, v.x⟩

/-- Angle of vector in radians. -/
def angle (v : Vector2) : Float := 
  v.y.atan2 v.x

/-- Angle between two vectors in radians. -/
def angleBetween (v w : Vector2) : Float := 
  let dot := v.normalized.dot w.normalized
  dot.clamp (-1.0) 1.0 |> (·.acos)

/-- Rotate vector by angle (in radians). -/
def rotate (v : Vector2) (angle : Float) : Vector2 := 
  let c := angle.cos
  let s := angle.sin
  ⟨v.x * c - v.y * s, v.x * s + v.y * c⟩

/-- Project vector v onto w. -/
def project (v w : Vector2) : Vector2 := 
  let d := v.dot w
  let len2 := w.length2
  if len2 == 0.0 then ⟨0.0, 0.0⟩ else ⟨w.x * d / len2, w.y * d / len2⟩

end HouLean.Vector2
