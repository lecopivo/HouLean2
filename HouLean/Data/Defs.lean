import Lean
import HouLean.Meta.ProdLike
import HouLean.Meta.EnumType
import HouLean.Data.Float

open Lean Meta

namespace HouLean

structure Vector2 where
  x : Float := 0.0
  y : Float := 0.0
deriving Repr, ProdLike
instance : Inhabited Vector2 := ⟨{}⟩

structure Vector3 where
  x : Float := 0.0
  y : Float := 0.0
  z : Float := 0.0
deriving Repr, ProdLike
instance : Inhabited Vector3 := ⟨{}⟩

structure Vector4 where
  x : Float := 0.0
  y : Float := 0.0
  z : Float := 0.0
  w : Float := 0.0
deriving Repr, ProdLike
instance : Inhabited Vector4 := ⟨{}⟩

structure Matrix2 where
  row0 : Vector2 := ⟨1,0⟩
  row1 : Vector2 := ⟨0,1⟩
deriving Repr, ProdLike
instance : Inhabited Matrix2 := ⟨{}⟩

structure Matrix3 where
  row0 : Vector3 := ⟨1,0,0⟩
  row1 : Vector3 := ⟨0,1,0⟩
  row2 : Vector3 := ⟨0,0,1⟩
deriving Repr, ProdLike
instance : Inhabited Matrix3 := ⟨{}⟩

structure Matrix4 where
  row0 : Vector4 := ⟨1,0,0,0⟩
  row1 : Vector4 := ⟨0,1,0,0⟩
  row2 : Vector4 := ⟨0,0,1,0⟩
  row3 : Vector4 := ⟨0,0,0,1⟩
deriving Repr, ProdLike
instance : Inhabited Matrix4 := ⟨{}⟩

-- todo: Use module system to ensure that the implementation of Matrix does not leak to userspace
--       At some point we might want to use Lean runtime and we might have to completaly change this
--       implementations
structure Matrix (R : Type) (m n : Nat) where
  data : Vector (Vector R n) m


/-- Rigid transformation without scaling.
-/
structure RigidTransform where
  translate : Vector3 := ⟨0,0,0⟩
  orient : Vector4 := ⟨0,0,0,1⟩
deriving ProdLike
instance : Inhabited RigidTransform := ⟨{}⟩

/-- Rigid transformation with scaling.
-/
structure RigidScaleTransform extends RigidTransform where
  scale : Float := 1
deriving ProdLike
instance : Inhabited RigidScaleTransform := ⟨{}⟩

/-- Rigid body velocity without scaling.
    Represents instantaneous velocity in SE(3) Lie algebra.
-/
structure RigidVelocity where
  velocity : Vector3 := ⟨0,0,0⟩
  angularVelocity : Vector3 := ⟨0,0,0⟩
deriving ProdLike
instance : Inhabited RigidVelocity := ⟨{}⟩

/-- Rigid body velocity with scaling.
    Represents instantaneous velocity in SE(3) Lie algebra with scale rate.
-/
structure RigidScaleVelocity extends RigidVelocity where
  scaleVelocity : Float := 0
deriving ProdLike
instance : Inhabited RigidScaleVelocity := ⟨{}⟩


inductive TransformOrder where
  | SRT | STR | RST | RTS | TSR | TRS
deriving EnumType

inductive RotationOrder where
  | XYZ | XZY | YXZ | YZX | ZXY | ZYX
deriving EnumType

/-- Transform parameters for geometric operations -/
structure Transform where
  /-- Translation vector -/
  translate : Vector3 := ⟨0, 0, 0⟩
  /-- Rotation in degrees (Euler angles) -/
  rotate : Vector3 := ⟨0, 0, 0⟩
  /-- Scale factors per axis -/
  scale : Vector3 := ⟨1, 1, 1⟩
  /-- Shear values -/
  shear : Vector3 := ⟨0, 0, 0⟩
  /-- Pivot point for rotation and scaling -/
  pivot : Vector3 := ⟨0, 0, 0⟩
  /-- Pivot rotation applied before main rotation -/
  pivotRotate : Vector3 := ⟨0, 0, 0⟩
  /-- Transform order: 0=SRT, 1=STR, 2=RST, 3=RTS, 4=TSR, 5=TRS -/
  xOrd : Int := 0 -- TransformOrder
  /-- Rotation order: 0=XYZ, 1=XZY, 2=YXZ, 3=YZX, 4=ZXY, 5=ZYX -/
  rOrd : Int := 0  -- RotationOrder
deriving ProdLike
instance : Inhabited Transform := ⟨{}⟩

/-- Bounding box -/
structure BoundingBox where
  /-- Size of the bounding box -/
  size : Vector3 := ⟨1, 1, 1⟩
  /-- Center position of the bounding box -/
  center : Vector3 := ⟨0, 0, 0⟩
deriving ProdLike
instance : Inhabited BoundingBox := ⟨{}⟩

/-- Bounding box -/
structure BoundingBox' (R : Type) [FloatType R] (dim : Nat) where
  /-- Size of the bounding box -/
  size : Vector R dim := .replicate dim 1.0
  /-- Center position of the bounding box -/
  center : Vector R dim := 0
instance {R} [FloatType R] : Inhabited (BoundingBox' R dim) := ⟨{}⟩

/-- Bounding sphere -/
structure BoundingSphere where
  /-- Center of the bounding sphere -/
  center : Vector3 := ⟨0, 0, 0⟩
  /-- Radius of bounding sphere -/
  radius : Float := 1
deriving ProdLike
instance : Inhabited BoundingBox := ⟨{}⟩


/-- Bounding Capsule  (tapered swept sphere).
    A capsule is defined by two endpoint centers, each with its own radius.
    When radii are equal, this is a standard capsule.
    When radii differ, this is a truncated cone with spherical caps. -/
structure Capsule where
  /-- Start point of the capsule's central axis -/
  start : Vector3 := ⟨0, 0, 0⟩
  /-- End point of the capsule's central axis -/
  finish : Vector3 := ⟨0, 1, 0⟩
  /-- Radius at the start point -/
  startRadius : Float := 0.5
  /-- Radius at the end point -/
  endRadius : Float := 0.5
deriving ProdLike
instance : Inhabited Capsule := ⟨{}⟩


/-- Plane specification -/
structure Plane where
  /-- Origin point on the plane -/
  origin : Vector3 := ⟨0, 0, 0⟩
  /-- Normal direction of the plane -/
  normal : Vector3 := ⟨0, 1, 0⟩
  /-- Distance along normal from origin -/
  dist : Float := 0
deriving ProdLike
instance : Inhabited Plane := ⟨{}⟩
