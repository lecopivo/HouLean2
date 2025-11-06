import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Data.Vector2

open HouLean.Apex.Generated

namespace HouLean.Vector2
-- Constructor
@[apex_implements HouLean.Vector2.mk]
noncomputable
def mk.apex_impl (x y : Float) : Vector2 := FloatToVector2 x y

-- Component accessors
noncomputable
def x.apex_impl (v : Vector2) : Float := GetComponentVector2 v 0

attribute [apex_implemented_by HouLean.Vector2.x.apex_impl] x

noncomputable
def y.apex_impl (v : Vector2) : Float := GetComponentVector2 v 1

attribute [apex_implemented_by HouLean.Vector2.y.apex_impl] y

-- Arithmetic operations
noncomputable
def add.apex_impl (a b : Vector2) : Vector2 := AddVector2 a #v[b]

attribute [apex_implemented_by HouLean.Vector2.add.apex_impl] add

noncomputable
def sub.apex_impl (a b : Vector2) : Vector2 := SubtractVector2 a #v[b]

attribute [apex_implemented_by HouLean.Vector2.sub.apex_impl] sub

noncomputable
def mul.apex_impl (a : Vector2) (s : Float) : Vector2 := MultiplyVector2Float a #v[s]

attribute [apex_implemented_by HouLean.Vector2.mul.apex_impl] hMul

noncomputable
def neg.apex_impl (a : Vector2) : Vector2 := NegateVector2 a

attribute [apex_implemented_by HouLean.Vector2.neg.apex_impl] neg

noncomputable
def dot.apex_impl (a b : Vector2) : Float := DotProductVector2 a b

attribute [apex_implemented_by HouLean.Vector2.dot.apex_impl] dot

noncomputable
def length.apex_impl (v : Vector2) : Float := LengthVector2 v

attribute [apex_implemented_by HouLean.Vector2.length.apex_impl] length

noncomputable
def normalize.apex_impl (v : Vector2) : Vector2 × Float := 
  let result := NormalizeVector2 v
  (result.1, result.2)

attribute [apex_implemented_by HouLean.Vector2.normalize.apex_impl] normalize

noncomputable
def distance.apex_impl (a b : Vector2) : Float := DistanceVector2 a b

attribute [apex_implemented_by HouLean.Vector2.distance.apex_impl] distance

noncomputable
def lerp.apex_impl (a b : Vector2) (t : Float) : Vector2 := LerpVector2 a b t

attribute [apex_implemented_by HouLean.Vector2.lerp.apex_impl] lerp


-- noncomputable
-- def beq.apex_impl (a b : Vector2) : Bool := EqualsVector2 a b

-- attribute [apex_implemented_by HouLean.Vector2.beq.apex_impl] beq

-- noncomputable
-- def blt.apex_impl (a b : Vector2) : Bool := LessThanVector2 a b

-- attribute [apex_implemented_by HouLean.Vector2.blt.apex_impl] blt

-- noncomputable
-- def ble.apex_impl (a b : Vector2) : Bool := LessThanOrEqualVector2 a b

-- attribute [apex_implemented_by HouLean.Vector2.ble.apex_impl] ble

noncomputable
def abs.apex_impl (v : Vector2) : Vector2 := AbsVector2 v

attribute [apex_implemented_by HouLean.Vector2.abs.apex_impl] abs

-- noncomputable
-- def quantize.apex_impl (v : Vector2) (step : Float) : Vector2 := QuantizeVector2 v step

-- attribute [apex_implemented_by HouLean.Vector2.quantize.apex_impl] quantize

noncomputable
def toPolar.apex_impl (v : Vector2) : Vector2 := CartesianToPolar v

attribute [apex_implemented_by HouLean.Vector2.toPolar.apex_impl] toPolar

noncomputable
def fromPolar.apex_impl (polar : Vector2) : Vector2 := PolarToCartesian polar

attribute [apex_implemented_by HouLean.Vector2.fromPolar.apex_impl] fromPolar

end Vector2
