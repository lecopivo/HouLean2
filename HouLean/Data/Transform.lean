import HouLean.Math
import HouLean.Data.Defs
import HouLean.Data.Vector3
import HouLean.Data.Vector4
import HouLean.Data.Matrix3
import HouLean.Data.Matrix4
import HouLean.Data.Float
import HouLean.Data.RigidScaleTransform

open HouLean.Math

namespace HouLean.Transform

-- ============================================================================
-- Transform Order Enumeration
-- ============================================================================

/-- Transform order enumeration -/
inductive TransformOrder where
  | SRT : TransformOrder  -- 0: Scale, Rotate, Translate
  | STR : TransformOrder  -- 1: Scale, Translate, Rotate
  | RST : TransformOrder  -- 2: Rotate, Scale, Translate
  | RTS : TransformOrder  -- 3: Rotate, Translate, Scale
  | TSR : TransformOrder  -- 4: Translate, Scale, Rotate
  | TRS : TransformOrder  -- 5: Translate, Rotate, Scale

def TransformOrder.toInt : TransformOrder → Int
  | SRT => 0
  | STR => 1
  | RST => 2
  | RTS => 3
  | TSR => 4
  | TRS => 5

def TransformOrder.fromInt : Int → TransformOrder
  | 0 => SRT
  | 1 => STR
  | 2 => RST
  | 3 => RTS
  | 4 => TSR
  | _ => TRS

-- ============================================================================
-- Rotation Order Enumeration
-- ============================================================================

/-- Rotation order enumeration -/
inductive RotationOrder where
  | XYZ : RotationOrder  -- 0
  | XZY : RotationOrder  -- 1
  | YXZ : RotationOrder  -- 2
  | YZX : RotationOrder  -- 3
  | ZXY : RotationOrder  -- 4
  | ZYX : RotationOrder  -- 5

def RotationOrder.toInt : RotationOrder → Int
  | XYZ => 0
  | XZY => 1
  | YXZ => 2
  | YZX => 3
  | ZXY => 4
  | ZYX => 5

def RotationOrder.fromInt : Int → RotationOrder
  | 0 => XYZ
  | 1 => XZY
  | 2 => YXZ
  | 3 => YZX
  | 4 => ZXY
  | _ => ZYX

-- ============================================================================
-- Construction
-- ============================================================================

def identity : Transform := ⟨⟨0,0,0⟩, ⟨0,0,0⟩, ⟨1,1,1⟩, ⟨0,0,0⟩, ⟨0,0,0⟩, ⟨0,0,0⟩, 0, 0⟩

def fromTranslate (t : Vector3) : Transform :=
  ⟨t, ⟨0,0,0⟩, ⟨1,1,1⟩, ⟨0,0,0⟩, ⟨0,0,0⟩, ⟨0,0,0⟩, 0, 0⟩

def fromRotate (r : Vector3) : Transform :=
  ⟨⟨0,0,0⟩, r, ⟨1,1,1⟩, ⟨0,0,0⟩, ⟨0,0,0⟩, ⟨0,0,0⟩, 0, 0⟩

def fromScale (s : Vector3) : Transform :=
  ⟨⟨0,0,0⟩, ⟨0,0,0⟩, s, ⟨0,0,0⟩, ⟨0,0,0⟩, ⟨0,0,0⟩, 0, 0⟩

def fromUniformScale (s : Float) : Transform :=
  ⟨⟨0,0,0⟩, ⟨0,0,0⟩, ⟨s,s,s⟩, ⟨0,0,0⟩, ⟨0,0,0⟩, ⟨0,0,0⟩, 0, 0⟩

def fromTRS (t r s : Vector3) : Transform :=
  ⟨t, r, s, ⟨0,0,0⟩, ⟨0,0,0⟩, ⟨0,0,0⟩, 5, 0⟩  -- TRS order

-- ============================================================================
-- Euler angle utilities
-- ============================================================================

/-- Convert Euler angles from degrees to radians -/
def rotateRadians (xform : Transform) : Vector3 :=
  xform.rotate.radians

/-- Convert pivot rotation from degrees to radians -/
def pivotRotateRadians (xform : Transform) : Vector3 :=
  xform.pivotRotate.radians

/-- Get rotation order -/
def getRotationOrder (xform : Transform) : RotationOrder :=
  RotationOrder.fromInt xform.rOrd

/-- Get transform order -/
def getTransformOrder (xform : Transform) : TransformOrder :=
  TransformOrder.fromInt xform.xOrd

-- ============================================================================
-- Matrix construction from Euler angles with rotation order
-- ============================================================================

/-- Build rotation matrix from Euler angles with specified rotation order -/
def eulerToMatrix3 (euler : Vector3) (order : RotationOrder) : Matrix3 :=
  let euler_rad := euler.radians
  let qx := (⟨1, 0, 0⟩ : Vector3).axisAngleToQuat euler_rad.x
  let qy := (⟨0, 1, 0⟩ : Vector3).axisAngleToQuat euler_rad.y
  let qz := (⟨0, 0, 1⟩ : Vector3).axisAngleToQuat euler_rad.z

  let q := match order with
    | RotationOrder.XYZ => qz.quatMul (qy.quatMul qx)
    | RotationOrder.XZY => qy.quatMul (qz.quatMul qx)
    | RotationOrder.YXZ => qz.quatMul (qx.quatMul qy)
    | RotationOrder.YZX => qx.quatMul (qz.quatMul qy)
    | RotationOrder.ZXY => qy.quatMul (qx.quatMul qz)
    | RotationOrder.ZYX => qx.quatMul (qy.quatMul qz)

  q.quatToMatrix3

/-- Build shear matrix -/
def shearToMatrix3 (shear : Vector3) : Matrix3 :=
  -- Shear: xy (shear x by y), xz (shear x by z), yz (shear y by z)
  ⟨⟨1, shear.x, shear.y⟩,
   ⟨0, 1, shear.z⟩,
   ⟨0, 0, 1⟩⟩

#exit
-- ============================================================================
-- Matrix conversion
-- ============================================================================

/-- Convert to 4x4 transformation matrix -/
def toMatrix4 (xform : Transform) : Matrix4 :=
  let order := xform.getTransformOrder
  let rotOrder := xform.getRotationOrder

  -- Build component matrices
  let T := Matrix4.fromTranslation xform.translate
  let R := Matrix4.fromMatrix3 (eulerToMatrix3 xform.rotate rotOrder)
  let S := Matrix4.fromScale xform.scale
  let Sh := Matrix4.fromMatrix3 (shearToMatrix3 xform.shear)
  let P := Matrix4.fromTranslation xform.pivot
  let Pinv := Matrix4.fromTranslation (-xform.pivot)
  let PR := Matrix4.fromMatrix3 (eulerToMatrix3 xform.pivotRotate rotOrder)
  let PRinv := Matrix4.fromMatrix3 (eulerToMatrix3 (-xform.pivotRotate) rotOrder)

  -- Apply pivot transformation
  let pivoted := P * PR
  let unpivoted := PRinv * Pinv

  -- Compose according to transform order
  let result := match order with
    | TransformOrder.SRT => T * pivoted * R * S * Sh * unpivoted
    | TransformOrder.STR => pivoted * R * T * S * Sh * unpivoted
    | TransformOrder.RST => T * pivoted * S * Sh * R * unpivoted
    | TransformOrder.RTS => T * S * Sh * pivoted * R * unpivoted
    | TransformOrder.TSR => pivoted * S * Sh * R * T * unpivoted
    | TransformOrder.TRS => pivoted * R * S * Sh * T * unpivoted

  result

/-- Convert to 3x3 matrix (no translation, ignores pivot) -/
def toMatrix3 (xform : Transform) : Matrix3 :=
  let order := xform.getTransformOrder
  let rotOrder := xform.getRotationOrder

  let R := eulerToMatrix3 xform.rotate rotOrder
  let S := Matrix3.fromDiagonal xform.scale
  let Sh := shearToMatrix3 xform.shear

  match order with
    | TransformOrder.SRT | TransformOrder.STR => R * S * Sh
    | TransformOrder.RST | TransformOrder.RTS => S * Sh * R
    | TransformOrder.TSR | TransformOrder.TRS => R * S * Sh

-- ============================================================================
-- Apply transform to geometry
-- ============================================================================

/-- Apply transform to a point -/
defun transformPoint (xform : Transform) (p : Vector3) : Vector3 :=
  let m := xform.toMatrix4
  let p4 : Vector4 := ⟨p.x, p.y, p.z, 1⟩
  let result := m * p4
  ⟨result.x, result.y, result.z⟩

/-- Apply transform to a vector (no translation) -/
def transformVector (xform : Transform) (v : Vector3) : Vector3 :=
  let m := xform.toMatrix3
  m * v

/-- Apply transform to a normal (inverse transpose) -/
def transformNormal (xform : Transform) (n : Vector3) : Vector3 :=
  let m := xform.toMatrix3
  -- For normals, use inverse transpose
  let mInv := m.inverse
  (mInv.transpose * n).normalized

instance : HMul Transform Vector3 Vector3 := ⟨transformPoint⟩

-- ============================================================================
-- Conversion to/from RigidScaleTransform
-- ============================================================================

/-- Convert to RigidScaleTransform (loses shear, pivot, non-uniform scale)
    Uses average scale as uniform scale -/
def toRigidScaleTransform (xform : Transform) : RigidScaleTransform :=
  let rotOrder := xform.getRotationOrder
  let euler_rad := xform.rotate.radians
  let q := euler_rad.eulerToQuat  -- Uses XYZ order by default

  -- For proper rotation order, build quaternion
  let q := match rotOrder with
    | RotationOrder.XYZ => euler_rad.eulerToQuat
    | _ => (eulerToMatrix3 xform.rotate rotOrder).matrix3ToQuat

  let avgScale := (xform.scale.x + xform.scale.y + xform.scale.z) / 3.0

  ⟨xform.translate, q, avgScale⟩

/-- Convert from RigidScaleTransform (sets TRS order, XYZ rotation order) -/
def fromRigidScaleTransform (rst : RigidScaleTransform) : Transform :=
  let euler := rst.orient.quatToEuler.degrees
  let s := ⟨rst.scale, rst.scale, rst.scale⟩
  ⟨rst.translate, euler, s, ⟨0,0,0⟩, ⟨0,0,0⟩, ⟨0,0,0⟩, 5, 0⟩  -- TRS, XYZ

/-- Try to convert to RigidScaleTransform, returns None if has shear or pivot -/
def tryToRigidScaleTransform (xform : Transform) : Option RigidScaleTransform :=
  if xform.shear != ⟨0,0,0⟩ || xform.pivot != ⟨0,0,0⟩ || xform.pivotRotate != ⟨0,0,0⟩ then
    none
  else if xform.scale.x != xform.scale.y || xform.scale.y != xform.scale.z then
    none  -- Non-uniform scale
  else
    some (xform.toRigidScaleTransform)

-- ============================================================================
-- Component access and modification
-- ============================================================================

def withTranslate (xform : Transform) (t : Vector3) : Transform :=
  { xform with translate := t }

def withRotate (xform : Transform) (r : Vector3) : Transform :=
  { xform with rotate := r }

def withScale (xform : Transform) (s : Vector3) : Transform :=
  { xform with scale := s }

def withUniformScale (xform : Transform) (s : Float) : Transform :=
  { xform with scale := ⟨s, s, s⟩ }

def withShear (xform : Transform) (sh : Vector3) : Transform :=
  { xform with shear := sh }

def withPivot (xform : Transform) (p : Vector3) : Transform :=
  { xform with pivot := p }

def withPivotRotate (xform : Transform) (r : Vector3) : Transform :=
  { xform with pivotRotate := r }

def withTransformOrder (xform : Transform) (order : TransformOrder) : Transform :=
  { xform with xOrd := order.toInt }

def withRotationOrder (xform : Transform) (order : RotationOrder) : Transform :=
  { xform with rOrd := order.toInt }

-- ============================================================================
-- Queries
-- ============================================================================

/-- Check if transform has uniform scale -/
def hasUniformScale (xform : Transform) : Bool :=
  xform.scale.x == xform.scale.y && xform.scale.y == xform.scale.z

/-- Check if transform has shear -/
def hasShear (xform : Transform) : Bool :=
  xform.shear != ⟨0, 0, 0⟩

/-- Check if transform has pivot -/
def hasPivot (xform : Transform) : Bool :=
  xform.pivot != ⟨0, 0, 0⟩ || xform.pivotRotate != ⟨0, 0, 0⟩

/-- Check if transform is identity -/
def isIdentity (xform : Transform) : Bool :=
  xform.translate == ⟨0,0,0⟩ &&
  xform.rotate == ⟨0,0,0⟩ &&
  xform.scale == ⟨1,1,1⟩ &&
  xform.shear == ⟨0,0,0⟩ &&
  xform.pivot == ⟨0,0,0⟩ &&
  xform.pivotRotate == ⟨0,0,0⟩

/-- Check if this can be losslessly converted to RigidScaleTransform -/
def isRigidScaleCompatible (xform : Transform) : Bool :=
  xform.hasUniformScale && !xform.hasShear && !xform.hasPivot

-- ============================================================================
-- Comparison
-- ============================================================================

defun beq (a b : Transform) : Bool :=
  a.translate.beq b.translate &&
  a.rotate.beq b.rotate &&
  a.scale.beq b.scale &&
  a.shear.beq b.shear &&
  a.pivot.beq b.pivot &&
  a.pivotRotate.beq b.pivotRotate &&
  a.xOrd == b.xOrd &&
  a.rOrd == b.rOrd

instance : BEq Transform := ⟨beq⟩

-- ============================================================================
-- Interpolation
-- ============================================================================

defun lerp (a b : Transform) (t : Float) : Transform :=
  ⟨a.translate.lerp b.translate t,
   a.rotate.lerp b.rotate t,
   a.scale.lerp b.scale t,
   a.shear.lerp b.shear t,
   a.pivot.lerp b.pivot t,
   a.pivotRotate.lerp b.pivotRotate t,
   a.xOrd,  -- Keep first transform's orders
   a.rOrd⟩

-- ============================================================================
-- Utilities
-- ============================================================================

/-- Reset to identity -/
def reset : Transform := identity

/-- Get determinant (from matrix) -/
def determinant (xform : Transform) : Float :=
  xform.toMatrix3.determinant

/-- Check if transform preserves handedness (positive determinant) -/
def preservesHandedness (xform : Transform) : Bool :=
  xform.determinant > 0

end HouLean.Transform

-- ============================================================================
-- Matrix4 helper functions (add to Matrix4 namespace)
-- ============================================================================

namespace HouLean.Matrix4

def fromTranslation (t : Vector3) : Matrix4 :=
  ⟨⟨1, 0, 0, t.x⟩,
   ⟨0, 1, 0, t.y⟩,
   ⟨0, 0, 1, t.z⟩,
   ⟨0, 0, 0, 1⟩⟩

def fromScale (s : Vector3) : Matrix4 :=
  ⟨⟨s.x, 0, 0, 0⟩,
   ⟨0, s.y, 0, 0⟩,
   ⟨0, 0, s.z, 0⟩,
   ⟨0, 0, 0, 1⟩⟩

def fromMatrix3 (m : Matrix3) : Matrix4 :=
  ⟨⟨m.row0.x, m.row0.y, m.row0.z, 0⟩,
   ⟨m.row1.x, m.row1.y, m.row1.z, 0⟩,
   ⟨m.row2.x, m.row2.y, m.row2.z, 0⟩,
   ⟨0, 0, 0, 1⟩⟩

end HouLean.Matrix4

-- ============================================================================
-- Matrix3 helper functions (add to Matrix3 namespace)
-- ============================================================================

namespace HouLean.Matrix3

def fromDiagonal (d : Vector3) : Matrix3 :=
  ⟨⟨d.x, 0, 0⟩,
   ⟨0, d.y, 0⟩,
   ⟨0, 0, d.z⟩⟩

end HouLean.Matrix3
