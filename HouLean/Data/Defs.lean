import Lean

open Lean Meta

namespace HouLean

structure Vector2 where
  x : Float
  y : Float
deriving Inhabited, Repr

structure Vector3 where
  x : Float
  y : Float
  z : Float
deriving Inhabited, Repr

structure Vector4 where
  x : Float
  y : Float
  z : Float
  w : Float
deriving Inhabited, Repr

structure Matrix2 where
  row0 : Vector2
  row1 : Vector2
deriving Inhabited, Repr

structure Matrix3 where
  row0 : Vector3
  row1 : Vector3
  row2 : Vector3
deriving Inhabited, Repr

structure Matrix4 where
  row0 : Vector4
  row1 : Vector4
  row2 : Vector4
  row3 : Vector4
deriving Inhabited, Repr


/-- Rigid transformation with scaling. 
-/
structure RigidScaleTransform where
  translate : Vector3
  orient : Vector4
  scale : Float

/-- Rigid transformation with scaling. 
-/
structure ScrewTransform where
  translate : Vector3
  axisAngle : Vector3
  scale : Float


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
  xOrd : Int := 0
  /-- Rotation order: 0=XYZ, 1=XZY, 2=YXZ, 3=YZX, 4=ZXY, 5=ZYX -/
  rOrd : Int := 0


/-- Bounding box specification -/
structure BoundingBox where
  /-- Size of the bounding box -/
  size : Vector3 := ⟨1, 1, 1⟩
  /-- Center position of the bounding box -/
  center : Vector3 := ⟨0, 0, 0⟩

/-- Plane specification -/
structure Plane where
  /-- Origin point on the plane -/
  origin : Vector3 := ⟨0, 0, 0⟩
  /-- Normal direction of the plane -/
  normal : Vector3 := ⟨0, 1, 0⟩
  /-- Distance along normal from origin -/
  dist : Float := 0
