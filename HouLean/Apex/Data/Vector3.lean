import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Data.Vector3

open HouLean.Apex.Generated

namespace HouLean.Vector3

-- Constructor
@[apex_implements HouLean.Vector3.mk]
noncomputable
def mk.apex_impl (x y z : Float) : Vector3 := FloatToVector3 x y z

-- Component accessors
@[apex_implements HouLean.Vector3.x]
noncomputable
def x.apex_impl (v : Vector3) : Float := GetComponentVector3 v 0

noncomputable
def y.apex_impl (v : Vector3) : Float := GetComponentVector3 v 1

attribute [apex_implemented_by HouLean.Vector3.y.apex_impl] y

noncomputable
def z.apex_impl (v : Vector3) : Float := GetComponentVector3 v 2

attribute [apex_implemented_by HouLean.Vector3.z.apex_impl] z

-- Arithmetic operations
noncomputable
def add.apex_impl (a b : Vector3) : Vector3 := AddVector3 a #v[b]

attribute [apex_implemented_by HouLean.Vector3.add.apex_impl] add

noncomputable
def sub.apex_impl (a b : Vector3) : Vector3 := SubtractVector3 a #v[b]

attribute [apex_implemented_by HouLean.Vector3.sub.apex_impl] sub

noncomputable
def mul.apex_impl (a : Vector3) (s : Float) : Vector3 := MultiplyVector3Float a #v[s]

attribute [apex_implemented_by HouLean.Vector3.mul.apex_impl] mul

noncomputable
def neg.apex_impl (a : Vector3) : Vector3 := NegateVector3 a

attribute [apex_implemented_by HouLean.Vector3.neg.apex_impl] neg

noncomputable
def dot.apex_impl (a b : Vector3) : Float := DotProductVector3 a b

attribute [apex_implemented_by HouLean.Vector3.dot.apex_impl] dot

noncomputable
def cross.apex_impl (a b : Vector3) : Vector3 := CrossProduct a b

attribute [apex_implemented_by HouLean.Vector3.cross.apex_impl] cross

noncomputable
def length.apex_impl (v : Vector3) : Float := LengthVector3 v

attribute [apex_implemented_by HouLean.Vector3.length.apex_impl] length

noncomputable
def normalize.apex_impl (v : Vector3) : Vector3 × Float := 
  let result := NormalizeVector3 v
  (result.1, result.2)

attribute [apex_implemented_by HouLean.Vector3.normalize.apex_impl] normalize

noncomputable
def distance.apex_impl (a b : Vector3) : Float := DistanceVector3 a b

attribute [apex_implemented_by HouLean.Vector3.distance.apex_impl] distance

noncomputable
def lerp.apex_impl (a b : Vector3) (t : Float) : Vector3 := LerpVector3 a b t

attribute [apex_implemented_by HouLean.Vector3.lerp.apex_impl] lerp

noncomputable
def beq.apex_impl (a b : Vector3) : Bool := EqualsVector3 a b

attribute [apex_implemented_by HouLean.Vector3.beq.apex_impl] beq

noncomputable
def blt.apex_impl (a b : Vector3) : Bool := LessThanVector3 a b

attribute [apex_implemented_by HouLean.Vector3.blt.apex_impl] blt

noncomputable
def ble.apex_impl (a b : Vector3) : Bool := LessThanOrEqualVector3 a b

attribute [apex_implemented_by HouLean.Vector3.ble.apex_impl] ble

noncomputable
def abs.apex_impl (v : Vector3) : Vector3 := AbsVector3 v

attribute [apex_implemented_by HouLean.Vector3.abs.apex_impl] abs

noncomputable
def quantize.apex_impl (v : Vector3) (step : Float) : Vector3 := QuantizeVector3 v step

attribute [apex_implemented_by HouLean.Vector3.quantize.apex_impl] quantize

noncomputable
def toSpherical.apex_impl (v : Vector3) : Vector3 := CartesianToSpherical v

attribute [apex_implemented_by HouLean.Vector3.toSpherical.apex_impl] toSpherical

noncomputable
def fromSpherical.apex_impl (spherical : Vector3) : Vector3 := SphericalToCartesian spherical

attribute [apex_implemented_by HouLean.Vector3.fromSpherical.apex_impl] fromSpherical

noncomputable
def toGeodetic.apex_impl (v : Vector3) : Vector3 := CartesianToGeodetic v

attribute [apex_implemented_by HouLean.Vector3.toGeodetic.apex_impl] toGeodetic

noncomputable
def fromGeodetic.apex_impl (geodetic : Vector3) : Vector3 := GeodeticToCartesian geodetic

attribute [apex_implemented_by HouLean.Vector3.fromGeodetic.apex_impl] fromGeodetic

noncomputable
def degreesToRadians.apex_impl (v : Vector3) : Vector3 := DegreesToRadiansVector3 v

attribute [apex_implemented_by HouLean.Vector3.degreesToRadians.apex_impl] degreesToRadians

noncomputable
def radiansToDegrees.apex_impl (v : Vector3) : Vector3 := RadiansToDegreesVector3 v

attribute [apex_implemented_by HouLean.Vector3.radiansToDegrees.apex_impl] radiansToDegrees

end Vector3
