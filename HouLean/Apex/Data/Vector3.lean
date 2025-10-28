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

@[apex_implements HouLean.Vector3.y]
noncomputable
def y.apex_impl (v : Vector3) : Float := GetComponentVector3 v 1

@[apex_implements HouLean.Vector3.z]
noncomputable
def z.apex_impl (v : Vector3) : Float := GetComponentVector3 v 2

-- Arithmetic operations
@[apex_implements HouLean.Vector3.add]
noncomputable
def add.apex_impl (a b : Vector3) : Vector3 := AddVector3 a #v[b]

@[apex_implements HouLean.Vector3.sub]
noncomputable
def sub.apex_impl (a b : Vector3) : Vector3 := SubtractVector3 a #v[b]

@[apex_implements HouLean.Vector3.smul]
noncomputable
def mul.apex_impl (a : Vector3) (s : Float) : Vector3 := MultiplyVector3Float a #v[s]

@[apex_implements HouLean.Vector3.neg]
noncomputable
def neg.apex_impl (a : Vector3) : Vector3 := NegateVector3 a

@[apex_implements HouLean.Vector3.dot]
noncomputable
def dot.apex_impl (a b : Vector3) : Float := DotProductVector3 a b

@[apex_implements HouLean.Vector3.cross]
noncomputable
def cross.apex_impl (a b : Vector3) : Vector3 := CrossProduct a b

@[apex_implements HouLean.Vector3.length]
noncomputable
def length.apex_impl (v : Vector3) : Float := LengthVector3 v

@[apex_implements HouLean.Vector3.normalize]
noncomputable
def normalize.apex_impl (v : Vector3) : Vector3 × Float := NormalizeVector3 v

@[apex_implements HouLean.Vector3.distance]
noncomputable
def distance.apex_impl (a b : Vector3) : Float := DistanceVector3 a b

@[apex_implements HouLean.Vector3.lerp]
noncomputable
def lerp.apex_impl (a b : Vector3) (t : Float) : Vector3 := LerpVector3 a b t

@[apex_implements HouLean.Vector3.beq]
noncomputable
def beq.apex_impl (a b : Vector3) : Bool := EqualsVector3 a b

@[apex_implements HouLean.Vector3.blt]
noncomputable
def blt.apex_impl (a b : Vector3) : Bool := LessThanVector3 a b

@[apex_implements HouLean.Vector3.ble]
noncomputable
def ble.apex_impl (a b : Vector3) : Bool := LessThanOrEqualVector3 a b

@[apex_implements HouLean.Vector3.abs]
noncomputable
def abs.apex_impl (v : Vector3) : Vector3 := AbsVector3 v

@[apex_implements HouLean.Vector3.quantize]
noncomputable
def quantize.apex_impl (v : Vector3) (step : Float) : Vector3 := QuantizeVector3 v step

@[apex_implements HouLean.Vector3.toSpherical]
noncomputable
def toSpherical.apex_impl (v : Vector3) : Vector3 := CartesianToSpherical v

@[apex_implements HouLean.Vector3.fromSpherical]
noncomputable
def fromSpherical.apex_impl (spherical : Vector3) : Vector3 := SphericalToCartesian spherical

@[apex_implements HouLean.Vector3.toGeodetic]
noncomputable
def toGeodetic.apex_impl (v : Vector3) : Vector3 := CartesianToGeodetic v

@[apex_implements HouLean.Vector3.fromGeodetic]
noncomputable
def fromGeodetic.apex_impl (geodetic : Vector3) : Vector3 := GeodeticToCartesian geodetic

@[apex_implements HouLean.Vector3.radians]
noncomputable
def degreesToRadians.apex_impl (v : Vector3) : Vector3 := DegreesToRadiansVector3 v

@[apex_implements HouLean.Vector3.degrees]
noncomputable
def radiansToDegrees.apex_impl (v : Vector3) : Vector3 := RadiansToDegreesVector3 v

end Vector3
