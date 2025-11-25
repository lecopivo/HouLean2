import HouLean.Math
import HouLean.LinearAlgebra
import HouLean.Data.Defs
import HouLean.Data.Vector3
import HouLean.Data.Vector4
import HouLean.Data.Matrix3
import HouLean.Data.Matrix4
import HouLean.Data.Float
import HouLean.Data.RigidTransform

open HouLean.Math

namespace HouLean.RigidScaleTransform

-- ============================================================================
-- Construction
-- ============================================================================

def identity : RigidScaleTransform := ⟨⟨⟨0,0,0⟩, ⟨0,0,0,1⟩⟩, 1⟩

def fromTranslate (t : Vector3) : RigidScaleTransform :=
  ⟨⟨t, ⟨0,0,0,1⟩⟩, 1⟩

def fromOrient (q : Vector4) : RigidScaleTransform :=
  ⟨⟨⟨0,0,0⟩, q⟩, 1⟩

def fromScale (s : Float) : RigidScaleTransform :=
  ⟨⟨⟨0,0,0⟩, ⟨0,0,0,1⟩⟩, s⟩

def fromTRS (t : Vector3) (q : Vector4) (s : Float) : RigidScaleTransform :=
  ⟨⟨t, q⟩, s⟩

def fromRigidTransform (rt : RigidTransform) : RigidScaleTransform :=
  ⟨rt, 1⟩

-- ============================================================================
-- Transform operations
-- ============================================================================

/-- Compose two transforms: first apply a, then b -/
def compose (a b : RigidScaleTransform) : RigidScaleTransform :=
  let q := b.toRigidTransform.orient.quatMul a.toRigidTransform.orient -- quaternion multiplication
  let s := b.scale * a.scale
  let t := b.scale * (b.toRigidTransform.orient.quatRotate a.toRigidTransform.translate) + b.toRigidTransform.translate
  ⟨⟨t, q⟩, s⟩

instance : HMul RigidScaleTransform RigidScaleTransform RigidScaleTransform :=
  ⟨compose⟩

/-- Apply transform to a point -/
defun transformPoint (xform : RigidScaleTransform) (p : Vector3) : Vector3 :=
  let rotated := xform.toRigidTransform.orient.quatRotate p
  let scaled := xform.scale * rotated
  scaled + xform.toRigidTransform.translate

/-- Apply transform to a vector (no translation) -/
defun transformVector (xform : RigidScaleTransform) (v : Vector3) : Vector3 :=
  let rotated := xform.toRigidTransform.orient.quatRotate v
  xform.scale * rotated

/-- Apply transform to a normal (inverse transpose semantics) -/
defun transformNormal (xform : RigidScaleTransform) (n : Vector3) : Vector3 :=
  let invScale := 1.0 / xform.scale
  let rotated := xform.toRigidTransform.orient.quatRotate n
  (invScale * rotated).normalized

instance : HMul RigidScaleTransform Vector3 Vector3 :=
  ⟨transformPoint⟩

-- ============================================================================
-- Inverse and related operations
-- ============================================================================

open LinearAlgebra in
/-- Compute inverse transform -/
defun inverse (xform : RigidScaleTransform) : RigidScaleTransform :=
  let invScale := 1.0 / xform.scale
  let invOrient := xform.toRigidTransform.orient.quatConjugate
  let invTranslate := invOrient.quatRotate ((-invScale) * xform.toRigidTransform.translate)
  ⟨⟨invTranslate, invOrient⟩, invScale⟩

/-- Apply inverse transform to a point -/
def inverseTransformPoint (xform : RigidScaleTransform) (p : Vector3) : Vector3 :=
  let translated := p - xform.toRigidTransform.translate
  let invScale := 1.0 / xform.scale
  let unscaled := invScale * translated
  xform.toRigidTransform.orient.quatConjugate.quatRotate unscaled

/-- Apply inverse transform to a vector -/
def inverseTransformVector (xform : RigidScaleTransform) (v : Vector3) : Vector3 :=
  let invScale := 1.0 / xform.scale
  let unscaled := invScale * v
  xform.toRigidTransform.orient.quatConjugate.quatRotate unscaled

-- ============================================================================
-- Interpolation
-- ============================================================================

defun lerp (a b : RigidScaleTransform) (t : Float) : RigidScaleTransform :=
  ⟨⟨a.toRigidTransform.translate.lerp b.toRigidTransform.translate t,
    a.toRigidTransform.orient.quatSlerp b.toRigidTransform.orient t⟩,
   a.scale + (b.scale - a.scale) * t⟩

defun slerp (a b : RigidScaleTransform) (t : Float) : RigidScaleTransform :=
  lerp a b t -- same as lerp since quaternion already uses slerp

-- ============================================================================
-- Comparison operations
-- ============================================================================

defun beq (a b : RigidScaleTransform) : Bool :=
  a.toRigidTransform.translate.beq b.toRigidTransform.translate &&
  a.toRigidTransform.orient.beq b.toRigidTransform.orient &&
  a.scale == b.scale

instance : BEq RigidScaleTransform := ⟨beq⟩

-- ============================================================================
-- Matrix conversion
-- ============================================================================

/-- Convert to 4x4 transformation matrix -/
def toMatrix4 (xform : RigidScaleTransform) : Matrix4 :=
  let rot := xform.toRigidTransform.orient.quatToMatrix3
  let s := xform.scale
  let t := xform.toRigidTransform.translate
  ⟨⟨s * rot.row0.x, s * rot.row0.y, s * rot.row0.z, t.x⟩,
   ⟨s * rot.row1.x, s * rot.row1.y, s * rot.row1.z, t.y⟩,
   ⟨s * rot.row2.x, s * rot.row2.y, s * rot.row2.z, t.z⟩,
   ⟨0, 0, 0, 1⟩⟩

/-- Convert to 3x3 rotation matrix (with scale) -/
def toMatrix3 (xform : RigidScaleTransform) : Matrix3 :=
  let rot := xform.toRigidTransform.orient.quatToMatrix3
  xform.scale * rot

-- ============================================================================
-- Decomposition and queries
-- ============================================================================

/-- Extract rotation as 3x3 matrix (no scale) -/
def rotationMatrix (xform : RigidScaleTransform) : Matrix3 :=
  xform.toRigidTransform.orient.quatToMatrix3

/-- Extract rotation as axis-angle: (axis, angle in radians) -/
def toAxisAngle (xform : RigidScaleTransform) : Vector3 × Float :=
  xform.toRigidTransform.orient.quatToAxisAngle


-- ============================================================================
-- Construction from common representations
-- ============================================================================

def fromMatrix4 (m : Matrix4) : RigidScaleTransform :=
  let t := ⟨m.row0.w, m.row1.w, m.row2.w⟩
  let m3 : Matrix3 :=
      ⟨⟨m.row0.x, m.row0.y, m.row0.z⟩,
      ⟨m.row1.x, m.row1.y, m.row1.z⟩,
      ⟨m.row2.x, m.row2.y, m.row2.z⟩⟩

  -- Extract scale from matrix rows
  let sx := Vector3.length ⟨m.row0.x, m.row0.y, m.row0.z⟩
  let sy := Vector3.length ⟨m.row1.x, m.row1.y, m.row1.z⟩
  let sz := Vector3.length ⟨m.row2.x, m.row2.y, m.row2.z⟩
  let s := (sx + sy + sz) / 3.0 -- uniform scale approximation

  -- Remove scale to get rotation matrix
  let rotMat := if s == 0 then m3 else (1.0 / s) * m3
  let q := rotMat.toQuat

  ⟨⟨t, q⟩, s⟩

-- def fromAxisAngle (axis : Vector3) (angle : Float) : RigidScaleTransform :=
--   ⟨⟨⟨0,0,0⟩, axis.axisAngleToQuat angle⟩, 1⟩

-- def fromEuler (euler : Vector3) : RigidScaleTransform :=
--   ⟨⟨⟨0,0,0⟩, euler.eulerToQuat⟩, 1⟩

-- def fromLookAt (eye target : Vector3) (up : Vector3 := ⟨0,1,0⟩) : RigidScaleTransform :=
--   let fwd := (target - eye).normalized
--   let q := fwd.lookRotation up
--   ⟨⟨eye, q⟩, 1⟩

-- ============================================================================
-- Utility operations
-- ============================================================================

/-- Reset to identity -/
def reset : RigidScaleTransform := identity

/-- Set only translation -/
def withTranslate (xform : RigidScaleTransform) (t : Vector3) : RigidScaleTransform :=
  ⟨⟨t, xform.toRigidTransform.orient⟩, xform.scale⟩

/-- Set only orientation -/
def withOrient (xform : RigidScaleTransform) (q : Vector4) : RigidScaleTransform :=
  ⟨⟨xform.toRigidTransform.translate, q⟩, xform.scale⟩

/-- Set only scale -/
def withScale (xform : RigidScaleTransform) (s : Float) : RigidScaleTransform :=
  ⟨xform.toRigidTransform, s⟩

end HouLean.RigidScaleTransform
