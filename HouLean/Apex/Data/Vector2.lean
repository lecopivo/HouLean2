import HouLean.Apex.Data.Float

open HouLean.Apex.Generated

namespace HouLean.Apex.Vector2

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
  ⟨Float.min a.x b.x, Float.min a.y b.y⟩

def max (a b : Vector2) : Vector2 :=
  ⟨Float.max a.x b.x, Float.max a.y b.y⟩

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

-- APEX implementations

-- Constructor
noncomputable
def mk.impl (x y : Float) : Vector2 := FloatToVector2 x y

attribute [apex_implemented_by HouLean.Apex.Vector2.mk.impl] mk

-- Component accessors
noncomputable
def x.impl (v : Vector2) : Float := GetComponentVector2 v 0

attribute [apex_implemented_by HouLean.Apex.Vector2.x.impl] x

noncomputable
def y.impl (v : Vector2) : Float := GetComponentVector2 v 1

attribute [apex_implemented_by HouLean.Apex.Vector2.y.impl] y

-- Arithmetic operations
noncomputable
def add.impl (a b : Vector2) : Vector2 := AddVector2 a #v[b]

attribute [apex_implemented_by HouLean.Apex.Vector2.add.impl] add

noncomputable
def sub.impl (a b : Vector2) : Vector2 := SubtractVector2 a #v[b]

attribute [apex_implemented_by HouLean.Apex.Vector2.sub.impl] sub

noncomputable
def mul.impl (a : Vector2) (s : Float) : Vector2 := MultiplyVector2Float a #v[s]

attribute [apex_implemented_by HouLean.Apex.Vector2.mul.impl] mul

noncomputable
def neg.impl (a : Vector2) : Vector2 := NegateVector2 a

attribute [apex_implemented_by HouLean.Apex.Vector2.neg.impl] neg

noncomputable
def dot.impl (a b : Vector2) : Float := DotProductVector2 a b

attribute [apex_implemented_by HouLean.Apex.Vector2.dot.impl] dot

noncomputable
def length.impl (v : Vector2) : Float := LengthVector2 v

attribute [apex_implemented_by HouLean.Apex.Vector2.length.impl] length

noncomputable
def normalize.impl (v : Vector2) : Vector2 × Float := 
  let result := NormalizeVector2 v
  (result.1, result.2)

attribute [apex_implemented_by HouLean.Apex.Vector2.normalize.impl] normalize

noncomputable
def distance.impl (a b : Vector2) : Float := DistanceVector2 a b

attribute [apex_implemented_by HouLean.Apex.Vector2.distance.impl] distance

noncomputable
def lerp.impl (a b : Vector2) (t : Float) : Vector2 := LerpVector2 a b t

attribute [apex_implemented_by HouLean.Apex.Vector2.lerp.impl] lerp

noncomputable
def beq.impl (a b : Vector2) : Bool := EqualsVector2 a b

attribute [apex_implemented_by HouLean.Apex.Vector2.beq.impl] beq

noncomputable
def blt.impl (a b : Vector2) : Bool := LessThanVector2 a b

attribute [apex_implemented_by HouLean.Apex.Vector2.blt.impl] blt

noncomputable
def ble.impl (a b : Vector2) : Bool := LessThanOrEqualVector2 a b

attribute [apex_implemented_by HouLean.Apex.Vector2.ble.impl] ble

noncomputable
def abs.impl (v : Vector2) : Vector2 := AbsVector2 v

attribute [apex_implemented_by HouLean.Apex.Vector2.abs.impl] abs

noncomputable
def quantize.impl (v : Vector2) (step : Float) : Vector2 := QuantizeVector2 v step

attribute [apex_implemented_by HouLean.Apex.Vector2.quantize.impl] quantize

noncomputable
def toPolar.impl (v : Vector2) : Vector2 := CartesianToPolar v

attribute [apex_implemented_by HouLean.Apex.Vector2.toPolar.impl] toPolar

noncomputable
def fromPolar.impl (polar : Vector2) : Vector2 := PolarToCartesian polar

attribute [apex_implemented_by HouLean.Apex.Vector2.fromPolar.impl] fromPolar

end Vector2
