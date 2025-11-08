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

