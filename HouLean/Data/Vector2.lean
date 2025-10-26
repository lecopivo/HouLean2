import HouLean.Data.Defs

namespace HouLean.Vector2

-- Arithmetic operations (Lean implementations)
def add (a b : Vector2) : Vector2 :=
  ⟨a.x + b.x, a.y + b.y⟩

def sub (a b : Vector2) : Vector2 :=
  ⟨a.x - b.x, a.y - b.y⟩

def mul (a : Vector2) (s : Float) : Vector2 :=
  ⟨a.x * s, a.y * s⟩

def div (a : Vector2) (s : Float) : Vector2 :=
  ⟨a.x / s, a.y / s⟩

def neg (a : Vector2) : Vector2 :=
  ⟨-a.x, -a.y⟩

-- Vector operations (Lean implementations)
def dot (a b : Vector2) : Float :=
  a.x * b.x + a.y * b.y

def length (v : Vector2) : Float :=
  Float.sqrt (v.x * v.x + v.y * v.y)

def lengthSquared (v : Vector2) : Float :=
  v.x * v.x + v.y * v.y

def normalize (v : Vector2) : Vector2 × Float :=
  let len := v.length
  if len == 0 then (v, 0) else (⟨v.x / len, v.y / len⟩, len)

def distance (a b : Vector2) : Float :=
  (a.sub b).length

def lerp (a b : Vector2) (t : Float) : Vector2 :=
  ⟨a.x + (b.x - a.x) * t, a.y + (b.y - a.y) * t⟩

-- Comparison operations (Lean implementations)
def beq (a b : Vector2) : Bool :=
  a.x == b.x && a.y == b.y

def blt (a b : Vector2) : Bool :=
  a.x < b.x && a.y < b.y

def ble (a b : Vector2) : Bool :=
  a.x <= b.x && a.y <= b.y

-- Component-wise operations (Lean implementations)
def abs (v : Vector2) : Vector2 :=
  ⟨Float.abs v.x, Float.abs v.y⟩

def min (a b : Vector2) : Vector2 :=
  ⟨Min.min a.x b.x, Min.min a.y b.y⟩

def max (a b : Vector2) : Vector2 :=
  ⟨Max.max a.x b.x, Max.max a.y b.y⟩

def quantize (v : Vector2) (step : Float) : Vector2 :=
  ⟨Float.round (v.x / step) * step, Float.round (v.y / step) * step⟩

-- Coordinate system conversions (Lean implementations)
def toPolar (v : Vector2) : Vector2 :=
  let r := v.length
  let theta := Float.atan2 v.y v.x
  ⟨r, theta⟩

def fromPolar (polar : Vector2) : Vector2 :=
  let r := polar.x
  let theta := polar.y
  ⟨r * Float.cos theta, r * Float.sin theta⟩

-- Operator overloading instances
instance : Add Vector2 where
  add := add

instance : Sub Vector2 where
  sub := sub

instance : Neg Vector2 where
  neg := neg

instance : HMul Vector2 Float Vector2 where
  hMul := mul

instance : HDiv Vector2 Float Vector2 where
  hDiv := div

instance : BEq Vector2 where
  beq := beq

instance : LT Vector2 where
  lt a b := blt a b

instance : LE Vector2 where
  le a b := ble a b

instance : Min Vector2 where
  min a b := a.min b

instance : Max Vector2 where
  max a b := a.max b

end Vector2
