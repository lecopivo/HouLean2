import HouLean.Data.Defs

namespace HouLean.Vector3

-- Arithmetic operations (Lean implementations)
def add (a b : Vector3) : Vector3 :=
  ⟨a.x + b.x, a.y + b.y, a.z + b.z⟩

def sub (a b : Vector3) : Vector3 :=
  ⟨a.x - b.x, a.y - b.y, a.z - b.z⟩

def mul (a : Vector3) (s : Float) : Vector3 :=
  ⟨a.x * s, a.y * s, a.z * s⟩

def div (a : Vector3) (s : Float) : Vector3 :=
  ⟨a.x / s, a.y / s, a.z / s⟩

def neg (a : Vector3) : Vector3 :=
  ⟨-a.x, -a.y, -a.z⟩

-- Vector operations (Lean implementations)
def dot (a b : Vector3) : Float :=
  a.x * b.x + a.y * b.y + a.z * b.z

def cross (a b : Vector3) : Vector3 :=
  ⟨a.y * b.z - a.z * b.y,
   a.z * b.x - a.x * b.z,
   a.x * b.y - a.y * b.x⟩

def length (v : Vector3) : Float :=
  Float.sqrt (v.x * v.x + v.y * v.y + v.z * v.z)

def lengthSquared (v : Vector3) : Float :=
  v.x * v.x + v.y * v.y + v.z * v.z

def normalize (v : Vector3) : Vector3 × Float :=
  let len := v.length
  if len == 0 then (v, 0) else (⟨v.x / len, v.y / len, v.z / len⟩, len)

def distance (a b : Vector3) : Float :=
  (a.sub b).length

def lerp (a b : Vector3) (t : Float) : Vector3 :=
  ⟨a.x + (b.x - a.x) * t,
   a.y + (b.y - a.y) * t,
   a.z + (b.z - a.z) * t⟩

-- Comparison operations (Lean implementations)
def beq (a b : Vector3) : Bool :=
  a.x == b.x && a.y == b.y && a.z == b.z

def blt (a b : Vector3) : Bool :=
  a.x < b.x && a.y < b.y && a.z < b.z

def ble (a b : Vector3) : Bool :=
  a.x <= b.x && a.y <= b.y && a.z <= b.z

-- Component-wise operations (Lean implementations)
def abs (v : Vector3) : Vector3 :=
  ⟨Float.abs v.x, Float.abs v.y, Float.abs v.z⟩

def min (a b : Vector3) : Vector3 :=
  ⟨Min.min a.x b.x, Min.min a.y b.y, Min.min a.z b.z⟩

def max (a b : Vector3) : Vector3 :=
  ⟨Max.max a.x b.x, Max.max a.y b.y, Max.max a.z b.z⟩

def quantize (v : Vector3) (step : Float) : Vector3 :=
  ⟨Float.round (v.x / step) * step,
   Float.round (v.y / step) * step,
   Float.round (v.z / step) * step⟩

-- Coordinate system conversions (Lean implementations)
def toSpherical (v : Vector3) : Vector3 :=
  let r := v.length
  let theta := Float.atan2 v.y v.x
  let phi := Float.acos (v.z / r)
  ⟨r, theta, phi⟩

def fromSpherical (spherical : Vector3) : Vector3 :=
  let r := spherical.x
  let theta := spherical.y
  let phi := spherical.z
  ⟨r * Float.sin phi * Float.cos theta,
   r * Float.sin phi * Float.sin theta,
   r * Float.cos phi⟩

def toGeodetic (v : Vector3) : Vector3 :=
  let lon := Float.atan2 v.y v.x
  let lat := Float.atan2 v.z (Float.sqrt (v.x * v.x + v.y * v.y))
  let alt := v.length
  ⟨lon, lat, alt⟩

def fromGeodetic (geodetic : Vector3) : Vector3 :=
  let lon := geodetic.x
  let lat := geodetic.y
  let alt := geodetic.z
  ⟨alt * Float.cos lat * Float.cos lon,
   alt * Float.cos lat * Float.sin lon,
   alt * Float.sin lat⟩

-- Angle conversions
def degreesToRadians (v : Vector3) : Vector3 :=
  let pi := 3.14159265358979323846
  let factor := pi / 180.0
  ⟨v.x * factor, v.y * factor, v.z * factor⟩

def radiansToDegrees (v : Vector3) : Vector3 :=
  let pi := 3.14159265358979323846
  let factor := 180.0 / pi
  ⟨v.x * factor, v.y * factor, v.z * factor⟩

-- Operator overloading instances
instance : Add Vector3 where
  add := add

instance : Sub Vector3 where
  sub := sub

instance : Neg Vector3 where
  neg := neg

instance : HMul Vector3 Float Vector3 where
  hMul := mul

instance : HDiv Vector3 Float Vector3 where
  hDiv := div

instance : BEq Vector3 where
  beq := beq

instance : LT Vector3 where
  lt a b := blt a b

instance : LE Vector3 where
  le a b := ble a b

end Vector3
