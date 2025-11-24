import HouLean.Math
import HouLean.Data.Defs
import HouLean.Data.Float
import HouLean.Data.Vector3
import HouLean.Data.Matrix3

open HouLean.Math

namespace HouLean.Vector4

-- ============================================================================
-- Arithmetic operations
-- ============================================================================

defun add (a b : Vector4) : Vector4 :=
  ⟨a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w⟩

defun sub (a b : Vector4) : Vector4 :=
  ⟨a.x - b.x, a.y - b.y, a.z - b.z, a.w - b.w⟩

defun neg (a : Vector4) : Vector4 :=
  ⟨-a.x, -a.y, -a.z, -a.w⟩

def smul (a : Vector4) (s : Float) : Vector4 :=
  ⟨s * a.x, s * a.y, s * a.z, s * a.w⟩

instance : HMul Float Vector4 Vector4 := ⟨fun s v => smul v s⟩
instance : HMul Vector4 Float Vector4 := ⟨smul⟩

defun hDiv (a : Vector4) (s : Float) : Vector4 :=
  ⟨a.x / s, a.y / s, a.z / s, a.w / s⟩

defun div (a : Vector4) (b : Vector4) : Vector4 :=
  ⟨a.x / b.x, a.y / b.y, a.z / b.z, a.w / b.w⟩


-- ============================================================================
-- Vector operations
-- ============================================================================

defun dot (a b : Vector4) : Float :=
  a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w

defun length (v : Vector4) : Float :=
  Float.sqrt (v.x * v.x + v.y * v.y + v.z * v.z + v.w * v.w)

defun length2 (v : Vector4) : Float :=
  v.x * v.x + v.y * v.y + v.z * v.z + v.w * v.w

defun normalize (v : Vector4) : Vector4 × Float :=
  let len := v.length
  if len == 0 then (v, 0) else (⟨v.x / len, v.y / len, v.z / len, v.w / len⟩, len)

defun normalized (v : Vector4) : Vector4 :=
  v.normalize.1

defun distance (a b : Vector4) : Float :=
  (a.sub b).length

defun distance2 (a b : Vector4) : Float :=
  (a.sub b).length2

defun lerp (a b : Vector4) (t : Float) : Vector4 :=
  ⟨a.x + (b.x - a.x) * t,
   a.y + (b.y - a.y) * t,
   a.z + (b.z - a.z) * t,
   a.w + (b.w - a.w) * t⟩

defun reflect (v n : Vector4) : Vector4 :=
  let d := v.dot n
  ⟨v.x - 2.0 * d * n.x,
   v.y - 2.0 * d * n.y,
   v.z - 2.0 * d * n.z,
   v.w - 2.0 * d * n.w⟩

defun refract (v n : Vector4) (eta : Float) : Vector4 :=
  let dt := v.dot n
  let k := 1.0 - eta * eta * (1.0 - dt * dt)
  if k < 0.0 then ⟨0.0, 0.0, 0.0, 0.0⟩
  else
    let s := eta * dt + k.sqrt
    ⟨eta * v.x - s * n.x,
     eta * v.y - s * n.y,
     eta * v.z - s * n.z,
     eta * v.w - s * n.w⟩

defun compMul (a b : Vector4) : Vector4 :=
  ⟨a.x * b.x, a.y * b.y, a.z * b.z, a.w * b.w⟩

defun compDiv (a b : Vector4) : Vector4 :=
  ⟨a.x / b.x, a.y / b.y, a.z / b.z, a.w / b.w⟩


-- ============================================================================
-- Comparison operations
-- ============================================================================

defun beq (a b : Vector4) : Bool :=
  a.x == b.x && a.y == b.y && a.z == b.z && a.w == b.w

defun blt (a b : Vector4) : Bool :=
  a.x < b.x && a.y < b.y && a.z < b.z && a.w < b.w

defun ble (a b : Vector4) : Bool :=
  a.x <= b.x && a.y <= b.y && a.z <= b.z && a.w <= b.w

instance : BEq Vector4 := ⟨beq⟩
instance : LT Vector4 := ⟨fun x y => blt x y = true⟩
instance : LE Vector4 := ⟨fun x y => ble x y = true⟩
instance : DecidableLT Vector4 := by
  intros; simp[DecidableLT, DecidableRel, LT.lt]; infer_instance
instance : DecidableLE Vector4 := by
  intros; simp[DecidableLE, DecidableRel, LE.le]; infer_instance


-- ============================================================================
-- Component-wise operations
-- ============================================================================

defun abs (v : Vector4) : Vector4 :=
  ⟨Float.abs v.x, Float.abs v.y, Float.abs v.z, Float.abs v.w⟩

defun min (a b : Vector4) : Vector4 :=
  ⟨Min.min a.x b.x, Min.min a.y b.y, Min.min a.z b.z, Min.min a.w b.w⟩

defun max (a b : Vector4) : Vector4 :=
  ⟨Max.max a.x b.x, Max.max a.y b.y, Max.max a.z b.z, Max.max a.w b.w⟩

defun sign (v : Vector4) : Vector4 :=
  ⟨v.x.sign, v.y.sign, v.z.sign, v.w.sign⟩

defun clamp (v lo hi : Vector4) : Vector4 :=
  ⟨v.x.clamp lo.x hi.x, v.y.clamp lo.y hi.y, v.z.clamp lo.z hi.z, v.w.clamp lo.w hi.w⟩

defun floor (v : Vector4) : Vector4 :=
  ⟨v.x.floor, v.y.floor, v.z.floor, v.w.floor⟩

defun ceil (v : Vector4) : Vector4 :=
  ⟨v.x.ceil, v.y.ceil, v.z.ceil, v.w.ceil⟩

defun round (v : Vector4) : Vector4 :=
  ⟨v.x.round, v.y.round, v.z.round, v.w.round⟩

defun trunc (v : Vector4) : Vector4 :=
  ⟨v.x.trunc, v.y.trunc, v.z.trunc, v.w.trunc⟩

defun fract (v : Vector4) : Vector4 :=
  ⟨v.x.fract, v.y.fract, v.z.fract, v.w.fract⟩

defun mod (v w : Vector4) : Vector4 :=
  ⟨v.x.mod w.x, v.y.mod w.y, v.z.mod w.z, v.w.mod w.w⟩

-- ============================================================================
-- Trigonometric Functions (elementwise)
-- ============================================================================

defun sin (v : Vector4) : Vector4 :=
  ⟨v.x.sin, v.y.sin, v.z.sin, v.w.sin⟩

defun cos (v : Vector4) : Vector4 :=
  ⟨v.x.cos, v.y.cos, v.z.cos, v.w.cos⟩

defun tan (v : Vector4) : Vector4 :=
  ⟨v.x.tan, v.y.tan, v.z.tan, v.w.tan⟩

defun asin (v : Vector4) : Vector4 :=
  ⟨v.x.asin, v.y.asin, v.z.asin, v.w.asin⟩

defun acos (v : Vector4) : Vector4 :=
  ⟨v.x.acos, v.y.acos, v.z.acos, v.w.acos⟩

defun atan (v : Vector4) : Vector4 :=
  ⟨v.x.atan, v.y.atan, v.z.atan, v.w.atan⟩

defun atan2 (v w : Vector4) : Vector4 :=
  ⟨v.x.atan2 w.x, v.y.atan2 w.y, v.z.atan2 w.z, v.w.atan2 w.w⟩

defun sinh (v : Vector4) : Vector4 :=
  ⟨v.x.sinh, v.y.sinh, v.z.sinh, v.w.sinh⟩

defun cosh (v : Vector4) : Vector4 :=
  ⟨v.x.cosh, v.y.cosh, v.z.cosh, v.w.cosh⟩

defun tanh (v : Vector4) : Vector4 :=
  ⟨v.x.tanh, v.y.tanh, v.z.tanh, v.w.tanh⟩

-- ============================================================================
-- Exponential and Logarithmic Functions (elementwise)
-- ============================================================================

defun exp (v : Vector4) : Vector4 :=
  ⟨v.x.exp, v.y.exp, v.z.exp, v.w.exp⟩

defun exp2 (v : Vector4) : Vector4 :=
  ⟨v.x.exp2, v.y.exp2, v.z.exp2, v.w.exp2⟩

defun log (v : Vector4) : Vector4 :=
  ⟨v.x.log, v.y.log, v.z.log, v.w.log⟩

defun log2 (v : Vector4) : Vector4 :=
  ⟨v.x.log2, v.y.log2, v.z.log2, v.w.log2⟩

defun log10 (v : Vector4) : Vector4 :=
  ⟨v.x.log10, v.y.log10, v.z.log10, v.w.log10⟩

defun pow (v w : Vector4) : Vector4 :=
  ⟨v.x.pow w.x, v.y.pow w.y, v.z.pow w.z, v.w.pow w.w⟩

defun sqrt (v : Vector4) : Vector4 :=
  ⟨v.x.sqrt, v.y.sqrt, v.z.sqrt, v.w.sqrt⟩

defun invsqrt (v : Vector4) : Vector4 :=
  ⟨v.x.invsqrt, v.y.invsqrt, v.z.invsqrt, v.w.invsqrt⟩

-- ============================================================================
-- Interpolation and Smoothing (elementwise)
-- ============================================================================

defun smoothstep (edge0 edge1 v : Vector4) : Vector4 :=
  ⟨edge0.x.smoothstep edge1.x v.x,
   edge0.y.smoothstep edge1.y v.y,
   edge0.z.smoothstep edge1.z v.z,
   edge0.w.smoothstep edge1.w v.w⟩

defun step (edge v : Vector4) : Vector4 :=
  ⟨edge.x.step v.x, edge.y.step v.y, edge.z.step v.z, edge.w.step v.w⟩

defun hermite (p0 p1 t0 t1 : Vector4) (t : Float) : Vector4 :=
  ⟨p0.x.hermite p1.x t0.x t1.x t,
   p0.y.hermite p1.y t0.y t1.y t,
   p0.z.hermite p1.z t0.z t1.z t,
   p0.w.hermite p1.w t0.w t1.w t⟩

defun catmullRom (p0 p1 p2 p3 : Vector4) (t : Float) : Vector4 :=
  ⟨p0.x.catmullRom p1.x p2.x p3.x t,
   p0.y.catmullRom p1.y p2.y p3.y t,
   p0.z.catmullRom p1.z p2.z p3.z t,
   p0.w.catmullRom p1.w p2.w p3.w t⟩

-- ============================================================================
-- Conversion and Construction
-- ============================================================================

defun radians (v : Vector4) : Vector4 :=
  ⟨v.x.radians, v.y.radians, v.z.radians, v.w.radians⟩

defun degrees (v : Vector4) : Vector4 :=
  ⟨v.x.degrees, v.y.degrees, v.z.degrees, v.w.degrees⟩

/-- Convert Vector3 to Vector4 with w component -/
def fromVector3 (v : Vector3) (w : Float := 0.0) : Vector4 :=
  ⟨v.x, v.y, v.z, w⟩

/-- Extract xyz as Vector3 -/
def toVector3 (v : Vector4) : Vector3 :=
  ⟨v.x, v.y, v.z⟩

-- ============================================================================
-- Quaternion Operations (Vector4 represents quaternion as (x,y,z,w))
-- ============================================================================

/-- Identity quaternion -/
def quatIdentity : Vector4 := ⟨0, 0, 0, 1⟩

/-- Quaternion conjugate (inverse for unit quaternions) -/
def quatConjugate (q : Vector4) : Vector4 :=
  ⟨-q.x, -q.y, -q.z, q.w⟩

/-- Quaternion multiplication -/
def quatMul (q1 q2 : Vector4) : Vector4 :=
  ⟨q1.w * q2.x + q1.x * q2.w + q1.y * q2.z - q1.z * q2.y,
   q1.w * q2.y - q1.x * q2.z + q1.y * q2.w + q1.z * q2.x,
   q1.w * q2.z + q1.x * q2.y - q1.y * q2.x + q1.z * q2.w,
   q1.w * q2.w - q1.x * q2.x - q1.y * q2.y - q1.z * q2.z⟩

/-- Rotate a vector by a quaternion -/
def quatRotate (q : Vector4) (v : Vector3) : Vector3 :=
  -- v' = q * v * q^(-1)
  -- Optimized formula: v' = v + 2 * cross(q.xyz, cross(q.xyz, v) + q.w * v)
  let qv : Vector3 := ⟨q.x, q.y, q.z⟩
  let uv := qv.cross v
  let uuv := qv.cross uv
  let uv_scaled := 2.0 * q.w * uv
  let uuv_scaled := 2.0 * uuv
  ⟨v.x + uv_scaled.x + uuv_scaled.x,
   v.y + uv_scaled.y + uuv_scaled.y,
   v.z + uv_scaled.z + uuv_scaled.z⟩

/-- Spherical linear interpolation between quaternions -/
def quatSlerp (q1 q2 : Vector4) (t : Float) : Vector4 :=
  let dot := q1.dot q2
  -- Choose the shortest path
  let q2' := if dot < 0.0 then q2.neg else q2
  let dot' := if dot < 0.0 then -dot else dot
  let dot_clamped := dot'.clamp (-1.0) 1.0

  let theta := dot_clamped.acos

  if theta.abs < 0.001 then
    -- Use linear interpolation for small angles
    q1.lerp q2' t |>.normalized
  else
    let s := theta.sin
    let a := ((1.0 - t) * theta).sin / s
    let b := (t * theta).sin / s
    ⟨a * q1.x + b * q2'.x,
     a * q1.y + b * q2'.y,
     a * q1.z + b * q2'.z,
     a * q1.w + b * q2'.w⟩  |> Vector4.normalized

/-- Convert quaternion to 3x3 rotation matrix -/
def quatToMatrix3 (q : Vector4) : Matrix3 :=
  let qn := q.normalized
  let x := qn.x
  let y := qn.y
  let z := qn.z
  let w := qn.w

  let x2 := x + x
  let y2 := y + y
  let z2 := z + z

  let xx := x * x2
  let xy := x * y2
  let xz := x * z2
  let yy := y * y2
  let yz := y * z2
  let zz := z * z2
  let wx := w * x2
  let wy := w * y2
  let wz := w * z2

  ⟨⟨1.0 - (yy + zz), xy - wz, xz + wy⟩,
   ⟨xy + wz, 1.0 - (xx + zz), yz - wx⟩,
   ⟨xz - wy, yz + wx, 1.0 - (xx + yy)⟩⟩

/-- Convert quaternion to axis-angle representation -/
def quatToAxisAngle (q : Vector4) : Vector3 × Float :=
  let qn := q.normalized
  let angle := 2.0 * qn.w.acos
  let s := (1.0 - qn.w * qn.w).sqrt

  if s < 0.001 then
    -- Axis is arbitrary for small angles
    (⟨1.0, 0.0, 0.0⟩, angle)
  else
    let axis := ⟨qn.x / s, qn.y / s, qn.z / s⟩
    (axis, angle)

-- /-- Convert quaternion to Euler angles (XYZ order, in radians) -/
-- def quatToEuler (q : Vector4) : Vector3 :=
--   let qn := q.normalized
--   let x := qn.x
--   let y := qn.y
--   let z := qn.z
--   let w := qn.w

--   -- Roll (x-axis rotation)
--   let sinr_cosp := 2.0 * (w * x + y * z)
--   let cosr_cosp := 1.0 - 2.0 * (x * x + y * y)
--   let roll := Float.atan2 sinr_cosp cosr_cosp

--   -- Pitch (y-axis rotation)
--   let sinp := 2.0 * (w * y - z * x)
--   let pitch := if sinp.abs >= 1.0 then
--     Float.copysign (Float.pi / 2.0) sinp -- Use 90 degrees if out of range
--   else
--     sinp.asin

--   -- Yaw (z-axis rotation)
--   let siny_cosp := 2.0 * (w * z + x * y)
--   let cosy_cosp := 1.0 - 2.0 * (y * y + z * z)
--   let yaw := Float.atan2 siny_cosp cosy_cosp

--   ⟨roll, pitch, yaw⟩

/-- Create quaternion from axis-angle -/
def _root_.HouLean.Vector3.axisAngleToQuat (axis : Vector3) (angle : Float) : Vector4 :=
  let half_angle := angle / 2.0
  let s := half_angle.sin
  let axis_n := axis.normalized
  ⟨s * axis_n.x, s * axis_n.y, s * axis_n.z, half_angle.cos⟩

/-- Create quaternion from Euler angles (XYZ order, in radians) -/
def eulerToQuat (euler : Vector3) : Vector4 :=
  let roll := euler.x
  let pitch := euler.y
  let yaw := euler.z

  let cr := (roll / 2.0).cos
  let sr := (roll / 2.0).sin
  let cp := (pitch / 2.0).cos
  let sp := (pitch / 2.0).sin
  let cy := (yaw / 2.0).cos
  let sy := (yaw / 2.0).sin

  ⟨sr * cp * cy - cr * sp * sy,
   cr * sp * cy + sr * cp * sy,
   cr * cp * sy - sr * sp * cy,
   cr * cp * cy + sr * sp * sy⟩

/-- Create look rotation quaternion from forward direction -/
def lookRotation (forward : Vector3) (up : Vector3 := ⟨0, 1, 0⟩) : Vector4 :=
  let fwd := forward.normalized
  let right := up.cross fwd |>.normalized
  let up' := fwd.cross right

  -- Build rotation matrix from basis vectors
  let m : Matrix3 := ⟨right, up', fwd⟩
  m.toQuat

/-- Get angle between two quaternions -/
def quatAngleBetween (q1 q2 : Vector4) : Float :=
  let dot := (q1.dot q2).abs.clamp 0.0 1.0
  2.0 * dot.acos

/-- Quaternion inverse -/
def quatInverse (q : Vector4) : Vector4 :=
  let len2 := q.length2
  if len2 == 0.0 then q
  else
    let conj := q.quatConjugate
    conj.hDiv len2

-- ============================================================================
-- Geometric Queries
-- ============================================================================

defun insideBox (point boxMin boxMax : Vector4) : Bool :=
  point.x >= boxMin.x && point.x <= boxMax.x &&
  point.y >= boxMin.y && point.y <= boxMax.y &&
  point.z >= boxMin.z && point.z <= boxMax.z &&
  point.w >= boxMin.w && point.w <= boxMax.w

defun projectToSegment (point a b : Vector4) : Vector4 :=
  let ab := b - a
  let ap := point - a
  let t := ((ap.dot ab) / (ab.dot ab)).clamp 0.0 1.0
  ⟨a.x + t * ab.x, a.y + t * ab.y, a.z + t * ab.z, a.w + t * ab.w⟩

-- ============================================================================
-- Additional Vector4 Specific Functions
-- ============================================================================

/-- Project vector v onto w -/
def project (v w : Vector4) : Vector4 :=
  let d := v.dot w
  let len2 := w.length2
  if len2 == 0.0 then ⟨0.0, 0.0, 0.0, 0.0⟩
  else ⟨w.x * d / len2, w.y * d / len2, w.z * d / len2, w.w * d / len2⟩

/-- Quantize vector to grid -/
def quantize (v : Vector4) (step : Float) : Vector4 :=
  ⟨Float.round (v.x / step) * step,
   Float.round (v.y / step) * step,
   Float.round (v.z / step) * step,
   Float.round (v.w / step) * step⟩

end HouLean.Vector4
