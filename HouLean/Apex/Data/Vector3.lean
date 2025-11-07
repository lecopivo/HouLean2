import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Data.Vector3

open HouLean.Apex Generated

namespace HouLean.Vector3

-- Constructor
@[apex_implements HouLean.Vector3.mk]
def mk.apex_impl (x y z : Float) : Vector3 := FloatToVector3 x y z

-- Component accessors
@[apex_implements HouLean.Vector3.x]
def x.apex_impl (v : Vector3) : Float := (Vector3ToFloat v).1

@[apex_implements HouLean.Vector3.y]
def y.apex_impl (v : Vector3) : Float := (Vector3ToFloat v).2.1

@[apex_implements HouLean.Vector3.z]
def z.apex_impl (v : Vector3) : Float := (Vector3ToFloat v).2.2

def rec.apex_impl {motive : Vector3 → Sort u} (f : (x y z : Float) → motive { x := x, y := y, z := z })  (t : Vector3) : motive t := 
  f t.x t.y t.z

run_meta Compiler.compilerExt.add (.implementedByName ``Vector3.rec ``Vector3.rec.apex_impl #[some 0, some 1, some 2])

-- Arithmetic operations
@[apex_implements HouLean.Vector3.add]
def add.apex_impl (a b : Vector3) : Vector3 := AddVector3 a #a[b]

@[apex_implements HouLean.Vector3.sub]
def sub.apex_impl (a b : Vector3) : Vector3 := SubtractVector3 a #a[b]

@[apex_implements HouLean.Vector3.smul]
def mul.apex_impl (a : Vector3) (s : Float) : Vector3 := MultiplyVector3Float a #a[s]

@[apex_implements HouLean.Vector3.neg]
def neg.apex_impl (a : Vector3) : Vector3 := NegateVector3 a

@[apex_implements HouLean.Vector3.dot]
def dot.apex_impl (a b : Vector3) : Float := DotProductVector3 a b

@[apex_implements HouLean.Vector3.cross]
def cross.apex_impl (a b : Vector3) : Vector3 := CrossProduct a b

@[apex_implements HouLean.Vector3.length]
def length.apex_impl (v : Vector3) : Float := LengthVector3 v

@[apex_implements HouLean.Vector3.normalize]
def normalize.apex_impl (v : Vector3) : Vector3 × Float := NormalizeVector3 v

@[apex_implements HouLean.Vector3.distance]
def distance.apex_impl (a b : Vector3) : Float := DistanceVector3 a b

@[apex_implements HouLean.Vector3.lerp]
def lerp.apex_impl (a b : Vector3) (t : Float) : Vector3 := LerpVector3 a b t

@[apex_implements HouLean.Vector3.beq]
def beq.apex_impl (a b : Vector3) : Bool := EqualsVector3 a b

@[apex_implements HouLean.Vector3.blt]
def blt.apex_impl (a b : Vector3) : Bool := LessThanVector3 a b

@[apex_implements HouLean.Vector3.ble]
def ble.apex_impl (a b : Vector3) : Bool := LessThanOrEqualVector3 a b

@[apex_implements HouLean.Vector3.abs]
def abs.apex_impl (v : Vector3) : Vector3 := AbsVector3 v

@[apex_implements HouLean.Vector3.quantize]
def quantize.apex_impl (v : Vector3) (step : Float) : Vector3 := QuantizeVector3 v step

@[apex_implements HouLean.Vector3.toSpherical]
def toSpherical.apex_impl (v : Vector3) : Vector3 := CartesianToSpherical v

@[apex_implements HouLean.Vector3.fromSpherical]
def fromSpherical.apex_impl (spherical : Vector3) : Vector3 := SphericalToCartesian spherical

@[apex_implements HouLean.Vector3.toGeodetic]
def toGeodetic.apex_impl (v : Vector3) : Vector3 := CartesianToGeodetic v

@[apex_implements HouLean.Vector3.fromGeodetic]
def fromGeodetic.apex_impl (geodetic : Vector3) : Vector3 := GeodeticToCartesian geodetic

@[apex_implements HouLean.Vector3.radians]
def degreesToRadians.apex_impl (v : Vector3) : Vector3 := DegreesToRadiansVector3 v

@[apex_implements HouLean.Vector3.degrees]
def radiansToDegrees.apex_impl (v : Vector3) : Vector3 := RadiansToDegreesVector3 v

end Vector3
