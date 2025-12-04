import HouLean.Math
import HouLean.LinearAlgebra
import HouLean.Data.Defs
import HouLean.Data.Vector3
import HouLean.Data.Vector4
import HouLean.Data.Matrix3
import HouLean.Data.Matrix4
import HouLean.Data.Float

open HouLean.Math

namespace HouLean.RigidTransform

-- ============================================================================
-- Construction
-- ============================================================================

def identity : RigidTransform := ⟨⟨0,0,0⟩, ⟨0,0,0,1⟩⟩

def fromTranslate (t : Vector3) : RigidTransform :=
  ⟨t, ⟨0,0,0,1⟩⟩

def fromOrient (q : Vector4) : RigidTransform :=
  ⟨⟨0,0,0⟩, q⟩

def fromTR (t : Vector3) (q : Vector4) : RigidTransform :=
  ⟨t, q⟩

-- ============================================================================
-- Transform operations
-- ============================================================================

/-- Compose two transforms: first apply a, then b -/
def compose (a b : RigidTransform) : RigidTransform :=
  let q := b.orient.quatMul a.orient -- quaternion multiplication
  let t := (b.orient.quatRotate a.translate) + b.translate
  ⟨t, q⟩

instance : HMul RigidTransform RigidTransform RigidTransform :=
  ⟨compose⟩

/-- Apply transform to a point -/
defun transformPoint (xform : RigidTransform) (p : Vector3) : Vector3 :=
  let rotated := xform.orient.quatRotate p
  rotated + xform.translate

/-- Apply transform to a vector (no translation) -/
defun transformVector (xform : RigidTransform) (v : Vector3) : Vector3 :=
  xform.orient.quatRotate v

/-- Apply transform to a normal -/
defun transformNormal (xform : RigidTransform) (n : Vector3) : Vector3 :=
  xform.orient.quatRotate n

instance : HMul RigidTransform Vector3 Vector3 :=
  ⟨transformPoint⟩

-- ============================================================================
-- Inverse and related operations
-- ============================================================================

open LinearAlgebra in
/-- Compute inverse transform -/
defun inverse (xform : RigidTransform) : RigidTransform :=
  let invOrient := xform.orient.quatConjugate
  let invTranslate := invOrient.quatRotate (-xform.translate)
  ⟨invTranslate, invOrient⟩

/-- Apply inverse transform to a point -/
def inverseTransformPoint (xform : RigidTransform) (p : Vector3) : Vector3 :=
  let translated := p - xform.translate
  xform.orient.quatConjugate.quatRotate translated

/-- Apply inverse transform to a vector -/
def inverseTransformVector (xform : RigidTransform) (v : Vector3) : Vector3 :=
  xform.orient.quatConjugate.quatRotate v

-- ============================================================================
-- Interpolation
-- ============================================================================

-- defun lerp (a b : RigidTransform) (t : Float) : RigidTransform :=
--   ⟨a.translate.lerp b.translate t,
--    a.orient.quatSlerp b.orient t⟩

-- defun slerp (a b : RigidTransform) (t : Float) : RigidTransform :=
--   lerp a b t -- same as lerp since quaternion already uses slerp

-- ============================================================================
-- Comparison operations
-- ============================================================================

defun beq (a b : RigidTransform) : Bool :=
  a.translate.beq b.translate && a.orient.beq b.orient

instance : BEq RigidTransform := ⟨beq⟩

-- ============================================================================
-- Matrix conversion
-- ============================================================================

/-- Convert to 4x4 transformation matrix -/
def toMatrix4 (xform : RigidTransform) : Matrix4 :=
  let rot := xform.orient.quatToMatrix3
  let t := xform.translate
  ⟨⟨rot.row0.x, rot.row0.y, rot.row0.z, t.x⟩,
   ⟨rot.row1.x, rot.row1.y, rot.row1.z, t.y⟩,
   ⟨rot.row2.x, rot.row2.y, rot.row2.z, t.z⟩,
   ⟨0, 0, 0, 1⟩⟩

/-- Convert to 3x3 rotation matrix -/
def toMatrix3 (xform : RigidTransform) : Matrix3 :=
  xform.orient.quatToMatrix3

-- ============================================================================
-- Decomposition and queries
-- ============================================================================

/-- Extract rotation as 3x3 matrix -/
def rotationMatrix (xform : RigidTransform) : Matrix3 :=
  xform.orient.quatToMatrix3

/-- Extract rotation as axis-angle: (axis, angle in radians) -/
def toAxisAngle (xform : RigidTransform) : Vector3 × Float :=
  xform.orient.quatToAxisAngle


-- ============================================================================
-- Construction from common representations
-- ============================================================================

def fromMatrix4 (m : Matrix4) : RigidTransform :=
  let t := ⟨m.row0.w, m.row1.w, m.row2.w⟩
  let m3 : Matrix3 :=
      ⟨⟨m.row0.x, m.row0.y, m.row0.z⟩,
      ⟨m.row1.x, m.row1.y, m.row1.z⟩,
      ⟨m.row2.x, m.row2.y, m.row2.z⟩⟩

  let q := m3.toQuat

  ⟨t, q⟩

-- def fromAxisAngle (axis : Vector3) (angle : Float) : RigidTransform :=
--   ⟨⟨0,0,0⟩, axis.axisAngleToQuat angle⟩

-- def fromEuler (euler : Vector3) : RigidTransform :=
--   ⟨⟨0,0,0⟩, euler.eulerToQuat⟩

-- def fromLookAt (eye target : Vector3) (up : Vector3 := ⟨0,1,0⟩) : RigidTransform :=
--   let fwd := (target - eye).normalized
--   let q := fwd.lookRotation up
--   ⟨eye, q⟩

-- ============================================================================
-- Utility operations
-- ============================================================================

/-- Reset to identity -/
def reset : RigidTransform := identity

/-- Set only translation -/
def withTranslate (xform : RigidTransform) (t : Vector3) : RigidTransform :=
  ⟨t, xform.orient⟩

/-- Set only orientation -/
def withOrient (xform : RigidTransform) (q : Vector4) : RigidTransform :=
  ⟨xform.translate, q⟩

end HouLean.RigidTransform
