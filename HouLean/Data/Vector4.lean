import HouLean.Data.Defs

namespace HouLean.Vector4

-- Arithmetic operations (Lean implementations)
def add (a b : Vector4) : Vector4 :=
  ⟨a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w⟩

def sub (a b : Vector4) : Vector4 :=
  ⟨a.x - b.x, a.y - b.y, a.z - b.z, a.w - b.w⟩

def mul (a : Vector4) (s : Float) : Vector4 :=
  ⟨a.x * s, a.y * s, a.z * s, a.w * s⟩

def div (a : Vector4) (s : Float) : Vector4 :=
  ⟨a.x / s, a.y / s, a.z / s, a.w / s⟩

def neg (a : Vector4) : Vector4 :=
  ⟨-a.x, -a.y, -a.z, -a.w⟩

-- Vector operations (Lean implementations)
def dot (a b : Vector4) : Float :=
  a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w

def length (v : Vector4) : Float :=
  Float.sqrt (v.x * v.x + v.y * v.y + v.z * v.z + v.w * v.w)

def lengthSquared (v : Vector4) : Float :=
  v.x * v.x + v.y * v.y + v.z * v.z + v.w * v.w

def normalize (v : Vector4) : Vector4 × Float :=
  let len := v.length
  if len == 0 then (v, 0) else (⟨v.x / len, v.y / len, v.z / len, v.w / len⟩, len)

def distance (a b : Vector4) : Float :=
  (a.sub b).length

def lerp (a b : Vector4) (t : Float) : Vector4 :=
  ⟨a.x + (b.x - a.x) * t,
   a.y + (b.y - a.y) * t,
   a.z + (b.z - a.z) * t,
   a.w + (b.w - a.w) * t⟩

def nlerp (a b : Vector4) (t : Float) : Vector4 :=
  let interpolated := lerp a b t
  interpolated.normalize.1

-- Comparison operations (Lean implementations)
def beq (a b : Vector4) : Bool :=
  a.x == b.x && a.y == b.y && a.z == b.z && a.w == b.w

def blt (a b : Vector4) : Bool :=
  a.x < b.x && a.y < b.y && a.z < b.z && a.w < b.w

def ble (a b : Vector4) : Bool :=
  a.x <= b.x && a.y <= b.y && a.z <= b.z && a.w <= b.w

-- Component-wise operations (Lean implementations)
def abs (v : Vector4) : Vector4 :=
  ⟨Float.abs v.x, Float.abs v.y, Float.abs v.z, Float.abs v.w⟩

def min (a b : Vector4) : Vector4 :=
  ⟨Min.min a.x b.x, Min.min a.y b.y, Min.min a.z b.z, Min.min a.w b.w⟩

def max (a b : Vector4) : Vector4 :=
  ⟨Max.max a.x b.x, Max.max a.y b.y, Max.max a.z b.z, Max.max a.w b.w⟩

-- Operator overloading instances
instance : Add Vector4 where
  add := add

instance : Sub Vector4 where
  sub := sub

instance : Neg Vector4 where
  neg := neg

instance : HMul Vector4 Float Vector4 where
  hMul := mul

instance : HDiv Vector4 Float Vector4 where
  hDiv := div

instance : BEq Vector4 where
  beq := beq

instance : LT Vector4 where
  lt a b := blt a b

instance : LE Vector4 where
  le a b := ble a b

end Vector4
