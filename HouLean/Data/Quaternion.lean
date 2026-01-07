import HouLean.Data.Vector
import HouLean.Data.Matrix

namespace HouLean

open Math

namespace Quaternion

variable {R : Type} [FloatType R]

-- ============================================================================
-- Constructors and Accessors
-- ============================================================================

def mk' (real : R) (imag : Vector R 3) : Quaternion R := .mk #v[imag.x, imag.y, imag.z, real]

@[inline] def x (q : Quaternion R) : R := q.toVector[0]
@[inline] def y (q : Quaternion R) : R := q.toVector[1]
@[inline] def z (q : Quaternion R) : R := q.toVector[2]
@[inline] def w (q : Quaternion R) : R := q.toVector[3]

def real (q : Quaternion R) : R := q.w
def imag (q : Quaternion R) : Vector R 3 := #v[q.x, q.y, q.z]

def identity : Quaternion R := .mk #v[0,0,0,1]

instance [ToString R] : ToString (Quaternion R) := ⟨fun q => s!"{q.w} + {q.x}𝕚 + {q.y}𝕛 + {q.z}𝕜"⟩


-- ============================================================================
-- Basic Arithmetic
-- ============================================================================

-- Hamilton product
def mul (p q : Quaternion R) : Quaternion R :=
  .mk' (p.real * q.real - p.imag.dot q.imag)
       (p.real * q.imag + q.real * p.imag + p.imag.cross3 q.imag)

instance : Add (Quaternion R) := ⟨fun p q => ⟨p.toVector + q.toVector⟩⟩
instance : Sub (Quaternion R) := ⟨fun p q => ⟨p.toVector - q.toVector⟩⟩
instance : Neg (Quaternion R) := ⟨fun q => ⟨-q.toVector⟩⟩
instance : HMul R (Quaternion R) (Quaternion R) := ⟨fun s q => ⟨s * q.toVector⟩⟩
-- instance : HMul (Quaternion R) R (Quaternion R) := ⟨fun q s => ⟨s * q.toVector⟩⟩
instance : HDiv (Quaternion R) R (Quaternion R) := ⟨fun q s => ⟨q.toVector / s⟩⟩
instance : Mul (Quaternion R) := ⟨mul⟩
instance : One (Quaternion R) := ⟨⟨#v[0,0,0,1]⟩⟩
instance : Zero (Quaternion R) := ⟨⟨0⟩⟩


-- ============================================================================
-- Quaternion Operations
-- ============================================================================

defun dot (p q : Quaternion R) : R := p.toVector.dot q.toVector

defun length2 (q : Quaternion R) : R := q.toVector.length2

defun length (q : Quaternion R) : R := q.toVector.length

def conj (q : Quaternion R) : Quaternion R := .mk' q.real (-q.imag)

defun normalize (q : Quaternion R) : Quaternion R × R :=
  let (v, len) := q.toVector.normalize
  (⟨v⟩, len)

defun normalized (q : Quaternion R) : Quaternion R := q.normalize.1

instance : Inv (Quaternion R) := ⟨fun q =>
  let len2 := q.length2
  let tol := ApproxEqual.defaultTol R
  if len2 ≈[tol*tol] 0 then q
  else q.conj / len2⟩

#eval ApproxEqual.defaultTol Float
#eval (Quaternion.mk' 0.0 #v[0.00,0,0.000000001])⁻¹

-- ============================================================================
-- Rotation Constructors
-- ============================================================================

open Math
/-- Create quaternion from axis (should be normalized) and angle in radians -/
def fromAxisAngle (axis : Vector R 3) (angle : R) : Quaternion R :=
  let halfAngle := 0.5 * angle
  let s := sin halfAngle
  let c := cos halfAngle
  .mk' c (s * axis)

/-- Extract axis and angle from quaternion. Returns (axis, angle) -/
def toAxisAngle (q : Quaternion R) : Vector R 3 × R :=
  let qn := q.normalized
  let angle := 2.0 * acos (clamp qn.w (-1:R) 1)
  let s2 := 1 - qn.w * qn.w
  if s2 ≈ 0 then
    (#v[1, 0, 0], 0)
  else
    let is := (sqrt s2)⁻¹
    (is * qn.imag, angle)


-- ============================================================================
-- Rotation Application
-- ============================================================================

/-- Rotate a 3D vector by this quaternion -/
def rotate (q : Quaternion R) (v : Vector R 3) : Vector R 3 :=
  -- q * v * q⁻¹ optimized
  let qv := q.imag
  let uv := qv.cross3 v
  let uuv := qv.cross3 uv
  v + (2.0:R) * (q.w * uv + uuv)

/-- Rotate a 3D vector by the inverse of this quaternion -/
def inverseRotate (q : Quaternion R) (v : Vector R 3) : Vector R 3 :=
  q.conj.rotate v


-- ============================================================================
-- Matrix Conversion
-- ============================================================================

/-- Convert quaternion to 3x3 rotation matrix -/
def toMatrix3 (q : Quaternion R) : Matrix R 3 3 :=
  let x2 := q.x * q.x
  let y2 := q.y * q.y
  let z2 := q.z * q.z
  let xy := q.x * q.y
  let xz := q.x * q.z
  let yz := q.y * q.z
  let wx := q.w * q.x
  let wy := q.w * q.y
  let wz := q.w * q.z
  #m[#v[1 - 2.0*(y2 + z2), 2.0*(xy - wz),     2.0*(xz + wy)    ],
     #v[2.0*(xy + wz),     1 - 2.0*(x2 + z2), 2.0*(yz - wx)    ],
     #v[2.0*(xz - wy),     2.0*(yz + wx),     1 - 2.0*(x2 + y2)]]

/-- Convert quaternion to 4x4 rotation matrix (no translation) -/
def toMatrix4 (q : Quaternion R) : Matrix R 4 4 :=
  let m3 := q.toMatrix3
  #m[#v[m3[0,0], m3[0,1], m3[0,2], 0],
     #v[m3[1,0], m3[1,1], m3[1,2], 0],
     #v[m3[2,0], m3[2,1], m3[2,2], 0],
     #v[0,       0,       0,       1]]



-- ============================================================================
-- Interpolation
-- ============================================================================

/-- Linear interpolation (not normalized) -/
defun lerp (p q : Quaternion R) (t : R) : Quaternion R :=
  ⟨Math.lerp p.toVector q.toVector t⟩

/-- Normalized linear interpolation -/
def nlerp (p q : Quaternion R) (t : R) : Quaternion R :=
  -- Handle antipodal quaternions
  let q' := if p.dot q < 0 then -q else q
  (lerp p q' t).normalized

/-- Spherical linear interpolation -/
def slerp (p q : Quaternion R) (t : R) : Quaternion R :=
  let d := p.dot q
  -- Handle antipodal quaternions
  let (q', d') := if d < 0 then (-q, -d) else (q, d)
  let d'' := clamp d' (-1:R) 1
  let theta := Math.acos d''
  if theta ≈ 0 then
    nlerp p q' t
  else
    let s := Math.sin theta
    let a := Math.sin ((1 - t) * theta) / s
    let b := Math.sin (t * theta) / s
    (a * p + b * q').normalized


-- ============================================================================
-- Comparison and Utilities
-- ============================================================================

def approxEqual (p q : Quaternion R) (tol : R) : Bool :=
  -- Quaternions q and -q represent same rotation
  p.toVector.approxEqual q.toVector tol ||
  p.toVector.approxEqual (-q.toVector) tol

instance : ApproxEqual (Quaternion R) R where
  defaultTol := ApproxEqual.defaultTol R
  approxEqual := approxEqual

/-- Angle between two quaternions in radians -/
def angle (p q : Quaternion R) : R :=
  let d := Math.abs (p.normalized.dot q.normalized)
  2.0 * acos (clamp d (0:R) 1)


-- ============================================================================
-- Matrix Conversion (from matrix)
-- ============================================================================

/-- Create quaternion from 3x3 rotation matrix (Shepperd method) -/
def fromMatrix3 (m : Matrix R 3 3) : Quaternion R :=
  let tr := m[0,0] + m[1,1] + m[2,2]
  if tr > 0 then
    let s := 2.0 * sqrt (tr + 1)
    .mk #v[(m[2,1] - m[1,2]) / s,
           (m[0,2] - m[2,0]) / s,
           (m[1,0] - m[0,1]) / s,
           0.25 * s]
  else if m[0,0] > m[1,1] && m[0,0] > m[2,2] then
    let s := (2.0 :R) * sqrt ((1:R) + m[0,0] - m[1,1] - m[2,2])
    .mk #v[0.25 * s,
           (m[0,1] + m[1,0]) / s,
           (m[0,2] + m[2,0]) / s,
           (m[2,1] - m[1,2]) / s]
  else if m[1,1] > m[2,2] then
    let s := (2.0:R) * sqrt ((1:R) + m[1,1] - m[0,0] - m[2,2])
    .mk #v[(m[0,1] + m[1,0]) / s,
           0.25 * s,
           (m[1,2] + m[2,1]) / s,
           (m[0,2] - m[2,0]) / s]
  else
    let s := (2.0:R) * sqrt ((1:R) + m[2,2] - m[0,0] - m[1,1])
    .mk #v[(m[0,2] + m[2,0]) / s,
           (m[1,2] + m[2,1]) / s,
           0.25 * s,
           (m[1,0] - m[0,1]) / s]

/-- Create quaternion from 4x4 transformation matrix (extracts rotation) -/
def fromMatrix4 (m : Matrix R 4 4) : Quaternion R :=
  fromMatrix3 (Matrix.ofFn fun i j => m[i,j])


-- ============================================================================
-- Euler Angles
-- ============================================================================

/-- Create quaternion from Euler angles (XYZ intrinsic order, radians) -/
def fromEulerXYZ (angles : Vector R 3) : Quaternion R :=
  let qx := fromAxisAngle #v[1,0,0] angles.x
  let qy := fromAxisAngle #v[0,1,0] angles.y
  let qz := fromAxisAngle #v[0,0,1] angles.z
  qx * qy * qz

/-- Create quaternion from Euler angles (ZYX intrinsic order, radians) -/
def fromEulerZYX (angles : Vector R 3) : Quaternion R :=
  let qz := fromAxisAngle #v[0,0,1] angles.z
  let qy := fromAxisAngle #v[0,1,0] angles.y
  let qx := fromAxisAngle #v[1,0,0] angles.x
  qz * qy * qx

/-- Extract Euler angles (XYZ intrinsic order) from quaternion -/
def toEulerXYZ (q : Quaternion R) : Vector R 3 :=
  let m := q.toMatrix3
  let sy := -m[0,2]
  if abs sy ≈ 1 then
    -- Gimbal lock
    let x := atan2 (-m[1,0]) m[1,1]
    let y := if sy > 0 then pi / 2.0 else -pi / 2.0
    #v[x, y, 0]
  else
    let x := atan2 m[1,2] m[2,2]
    let y := asin sy
    let z := atan2 m[0,1] m[0,0]
    #v[x, y, z]

/-- Extract Euler angles (ZYX intrinsic order) from quaternion -/
def toEulerZYX (q : Quaternion R) : Vector R 3 :=
  let m := q.toMatrix3
  let sy := m[2,0]
  if abs sy ≈ 1 then
    -- Gimbal lock
    let z := atan2 m[0,1] m[1,1]
    let y := if sy > 0 then pi / 2.0 else -pi / 2.0
    #v[0, y, z]
  else
    let x := atan2 (-m[2,1]) m[2,2]
    let y := asin sy
    let z := atan2 (-m[1,0]) m[0,0]
    #v[x, y, z]


-- ============================================================================
-- Direction Alignment
-- ============================================================================

/-- Create quaternion that rotates 'from' direction to 'to' direction -/
def fromTwoVectors (fr to : Vector R 3) : Quaternion R :=
  let fn := fr.normalized
  let tn := to.normalized
  let d := fn.dot tn
  if d ≈ 1 then
    identity
  else if d ≈ -1 then
    -- Vectors are opposite, find orthogonal axis
    let axis := if abs fn.x < 0.9
      then fn.cross3 #v[1, 0, 0]
      else fn.cross3 #v[0, 1, 0]
    fromAxisAngle axis.normalized pi
  else
    let axis := fn.cross3 tn
    let s := sqrt (2.0 * (1 + d))
    .mk' (0.5 * s) (s⁻¹ * axis)


-- ============================================================================
-- Exponential/Logarithm Maps
-- ============================================================================

/-- Exponential map: rotation vector (axis * angle) to quaternion -/
def exp (v : Vector R 3) : Quaternion R :=
  let (axis, angle) := v.normalize
  if angle ≈ 0 then identity
  else fromAxisAngle axis angle

/-- Logarithm map: quaternion to rotation vector (axis * angle) -/
def log (q : Quaternion R) : Vector R 3 :=
  let (axis, angle) := q.toAxisAngle
  angle * axis

/-- Power of quaternion: q^t -/
def pow (q : Quaternion R) (t : R) : Quaternion R :=
  let (axis, angle) := q.toAxisAngle
  fromAxisAngle axis (t * angle)


-- ============================================================================
-- Transform Utilities
-- ============================================================================

/-- Build 4x4 rotation matrix with translation -/
def toTransformMatrix (q : Quaternion R) (translation : Vector R 3) : Matrix R 4 4 :=
  let m3 := q.toMatrix3
  #m[#v[m3[0,0], m3[0,1], m3[0,2], translation.x],
     #v[m3[1,0], m3[1,1], m3[1,2], translation.y],
     #v[m3[2,0], m3[2,1], m3[2,2], translation.z],
     #v[0,       0,       0,       1            ]]

/-- Extract rotation and translation from 4x4 matrix -/
def fromTransformMatrix (m : Matrix R 4 4) : Quaternion R × Vector R 3 :=
  (fromMatrix4 m, #v[m[0,3], m[1,3], m[2,3]])

/-- Rotation between two quaternions: returns q such that from * q = to -/
def difference (fr to : Quaternion R) : Quaternion R :=
  to * fr⁻¹

/-- Look-at rotation: rotate local +Z to point from eye toward target -/
def lookAt (forward up : Vector R 3) : Quaternion R :=
  let f := forward.normalized
  let r := (up.cross3 f).normalized
  let u := f.cross3 r
  fromMatrix3 #m[r, u, f]


-- ============================================================================
-- Swing-Twist Decomposition
-- ============================================================================

/-- Decompose quaternion into swing and twist around given axis.
    Returns (swing, twist) where q = swing * twist -/
def swingTwist (q : Quaternion R) (axis : Vector R 3) : Quaternion R × Quaternion R :=
  let projection := (q.imag.dot axis) * axis
  let twist := (Quaternion.mk' q.real projection).normalized
  let swing := q * twist⁻¹
  (swing, twist)

end Quaternion

end HouLean
