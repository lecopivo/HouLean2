import HouLean.Apex.Data.Float

open HouLean.Apex.Generated

namespace HouLean.Apex.Vector4

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
  ⟨Float.min a.x b.x, Float.min a.y b.y, Float.min a.z b.z, Float.min a.w b.w⟩

def max (a b : Vector4) : Vector4 :=
  ⟨Float.max a.x b.x, Float.max a.y b.y, Float.max a.z b.z, Float.max a.w b.w⟩

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

-- APEX implementations

-- Constructor
noncomputable
def mk.impl (x y z w : Float) : Vector4 := FloatToVector4 x y z w

attribute [apex_implemented_by HouLean.Apex.Vector4.mk.impl] mk

-- Component accessors
noncomputable
def x.impl (v : Vector4) : Float := GetComponentVector4 v 0

attribute [apex_implemented_by HouLean.Apex.Vector4.x.impl] x

noncomputable
def y.impl (v : Vector4) : Float := GetComponentVector4 v 1

attribute [apex_implemented_by HouLean.Apex.Vector4.y.impl] y

noncomputable
def z.impl (v : Vector4) : Float := GetComponentVector4 v 2

attribute [apex_implemented_by HouLean.Apex.Vector4.z.impl] z

noncomputable
def w.impl (v : Vector4) : Float := GetComponentVector4 v 3

attribute [apex_implemented_by HouLean.Apex.Vector4.w.impl] w

-- Arithmetic operations
-- noncomputable
-- def add.impl (a b : Vector4) : Vector4 := AddVector4 a #v[b]

-- attribute [apex_implemented_by HouLean.Apex.Vector4.add.impl] add

-- noncomputable
-- def sub.impl (a b : Vector4) : Vector4 := SubtractVector4 a #v[b]

-- attribute [apex_implemented_by HouLean.Apex.Vector4.sub.impl] sub

noncomputable
def mul.impl (a : Vector4) (s : Float) : Vector4 := MultiplyVector4Float a #v[s]

attribute [apex_implemented_by HouLean.Apex.Vector4.mul.impl] mul

noncomputable
def dot.impl (a b : Vector4) : Float := DotProductVector4 a b

attribute [apex_implemented_by HouLean.Apex.Vector4.dot.impl] dot

noncomputable
def length.impl (v : Vector4) : Float := LengthVector4 v

attribute [apex_implemented_by HouLean.Apex.Vector4.length.impl] length

noncomputable
def normalize.impl (v : Vector4) : Vector4 × Float := 
  let result := NormalizeVector4 v
  (result.1, result.2)

attribute [apex_implemented_by HouLean.Apex.Vector4.normalize.impl] normalize

noncomputable
def distance.impl (a b : Vector4) : Float := DistanceVector4 a b

attribute [apex_implemented_by HouLean.Apex.Vector4.distance.impl] distance

noncomputable
def lerp.impl (a b : Vector4) (t : Float) : Vector4 := LerpVector4 a b t

attribute [apex_implemented_by HouLean.Apex.Vector4.lerp.impl] lerp

noncomputable
def nlerp.impl (a b : Vector4) (t : Float) : Vector4 := NLerpVector4 a b t

attribute [apex_implemented_by HouLean.Apex.Vector4.nlerp.impl] nlerp

noncomputable
def beq.impl (a b : Vector4) : Bool := EqualsVector4 a b

attribute [apex_implemented_by HouLean.Apex.Vector4.beq.impl] beq

noncomputable
def blt.impl (a b : Vector4) : Bool := LessThanVector4 a b

attribute [apex_implemented_by HouLean.Apex.Vector4.blt.impl] blt

noncomputable
def ble.impl (a b : Vector4) : Bool := LessThanOrEqualVector4 a b

attribute [apex_implemented_by HouLean.Apex.Vector4.ble.impl] ble

noncomputable
def abs.impl (v : Vector4) : Vector4 := AbsVector4 v

attribute [apex_implemented_by HouLean.Apex.Vector4.abs.impl] abs

end Vector4
