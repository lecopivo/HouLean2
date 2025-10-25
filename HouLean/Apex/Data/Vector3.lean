import HouLean.Apex.Data.Float

open HouLean.Apex.Generated

namespace HouLean.Apex.Vector3

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
  ⟨Float.min a.x b.x, Float.min a.y b.y, Float.min a.z b.z⟩

def max (a b : Vector3) : Vector3 :=
  ⟨Float.max a.x b.x, Float.max a.y b.y, Float.max a.z b.z⟩

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

-- APEX implementations

-- Constructor
noncomputable
def mk.impl (x y z : Float) : Vector3 := FloatToVector3 x y z

attribute [apex_implemented_by HouLean.Apex.Vector3.mk.impl] mk

-- Component accessors
noncomputable
def x.impl (v : Vector3) : Float := GetComponentVector3 v 0

attribute [apex_implemented_by HouLean.Apex.Vector3.x.impl] x

noncomputable
def y.impl (v : Vector3) : Float := GetComponentVector3 v 1

attribute [apex_implemented_by HouLean.Apex.Vector3.y.impl] y

noncomputable
def z.impl (v : Vector3) : Float := GetComponentVector3 v 2

attribute [apex_implemented_by HouLean.Apex.Vector3.z.impl] z

-- Arithmetic operations
noncomputable
def add.impl (a b : Vector3) : Vector3 := AddVector3 a #v[b]

attribute [apex_implemented_by HouLean.Apex.Vector3.add.impl] add

noncomputable
def sub.impl (a b : Vector3) : Vector3 := SubtractVector3 a #v[b]

attribute [apex_implemented_by HouLean.Apex.Vector3.sub.impl] sub

noncomputable
def mul.impl (a : Vector3) (s : Float) : Vector3 := MultiplyVector3Float a #v[s]

attribute [apex_implemented_by HouLean.Apex.Vector3.mul.impl] mul

noncomputable
def neg.impl (a : Vector3) : Vector3 := NegateVector3 a

attribute [apex_implemented_by HouLean.Apex.Vector3.neg.impl] neg

noncomputable
def dot.impl (a b : Vector3) : Float := DotProductVector3 a b

attribute [apex_implemented_by HouLean.Apex.Vector3.dot.impl] dot

noncomputable
def cross.impl (a b : Vector3) : Vector3 := CrossProduct a b

attribute [apex_implemented_by HouLean.Apex.Vector3.cross.impl] cross

noncomputable
def length.impl (v : Vector3) : Float := LengthVector3 v

attribute [apex_implemented_by HouLean.Apex.Vector3.length.impl] length

noncomputable
def normalize.impl (v : Vector3) : Vector3 × Float := 
  let result := NormalizeVector3 v
  (result.1, result.2)

attribute [apex_implemented_by HouLean.Apex.Vector3.normalize.impl] normalize

noncomputable
def distance.impl (a b : Vector3) : Float := DistanceVector3 a b

attribute [apex_implemented_by HouLean.Apex.Vector3.distance.impl] distance

noncomputable
def lerp.impl (a b : Vector3) (t : Float) : Vector3 := LerpVector3 a b t

attribute [apex_implemented_by HouLean.Apex.Vector3.lerp.impl] lerp

noncomputable
def beq.impl (a b : Vector3) : Bool := EqualsVector3 a b

attribute [apex_implemented_by HouLean.Apex.Vector3.beq.impl] beq

noncomputable
def blt.impl (a b : Vector3) : Bool := LessThanVector3 a b

attribute [apex_implemented_by HouLean.Apex.Vector3.blt.impl] blt

noncomputable
def ble.impl (a b : Vector3) : Bool := LessThanOrEqualVector3 a b

attribute [apex_implemented_by HouLean.Apex.Vector3.ble.impl] ble

noncomputable
def abs.impl (v : Vector3) : Vector3 := AbsVector3 v

attribute [apex_implemented_by HouLean.Apex.Vector3.abs.impl] abs

noncomputable
def quantize.impl (v : Vector3) (step : Float) : Vector3 := QuantizeVector3 v step

attribute [apex_implemented_by HouLean.Apex.Vector3.quantize.impl] quantize

noncomputable
def toSpherical.impl (v : Vector3) : Vector3 := CartesianToSpherical v

attribute [apex_implemented_by HouLean.Apex.Vector3.toSpherical.impl] toSpherical

noncomputable
def fromSpherical.impl (spherical : Vector3) : Vector3 := SphericalToCartesian spherical

attribute [apex_implemented_by HouLean.Apex.Vector3.fromSpherical.impl] fromSpherical

noncomputable
def toGeodetic.impl (v : Vector3) : Vector3 := CartesianToGeodetic v

attribute [apex_implemented_by HouLean.Apex.Vector3.toGeodetic.impl] toGeodetic

noncomputable
def fromGeodetic.impl (geodetic : Vector3) : Vector3 := GeodeticToCartesian geodetic

attribute [apex_implemented_by HouLean.Apex.Vector3.fromGeodetic.impl] fromGeodetic

noncomputable
def degreesToRadians.impl (v : Vector3) : Vector3 := DegreesToRadiansVector3 v

attribute [apex_implemented_by HouLean.Apex.Vector3.degreesToRadians.impl] degreesToRadians

noncomputable
def radiansToDegrees.impl (v : Vector3) : Vector3 := RadiansToDegreesVector3 v

attribute [apex_implemented_by HouLean.Apex.Vector3.radiansToDegrees.impl] radiansToDegrees

end Vector3
