import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy

namespace HouLean.Apex

open Generated

-- there are some problematic functions which have unused arguments
-- uncomment this to see them
set_option linter.unusedVariables false

-- High-Level API for APEX Graph Network
-- This builds on top of the basic opaque bindings

def apexPanic {α : Type} [Inhabited α] (str : String) : α := default

/-- Cause an error during compilation to APEX graph -/
macro "apexPanic!" msg:str : term => `(apexPanic $msg)

-- ============================================================================
-- Type Class Instances for Standard Operations
-- ============================================================================

-- Numeric operations
@[apex_implements Float.add]
def Float.add.impl (x y : Float) : Float := AddFloat x #v[y]

@[apex_implements Float.add]
def Float.sub.impl (x y : Float) : Float := SubtractFloat x #v[y]

@[apex_implements Float.mul]
def Float.mul.impl (x y : Float) : Float := MultiplyFloat x #v[y]

@[apex_implements Float.div]
def Float.div.impl (x y : Float) : Float := DivideFloat x #v[y]

@[apex_implements Float.neg]
def Float.neg.impl (x : Float) : Float := NegateFloat x

@[apex_implements Int.add]
def Int.add.impl (x y : Int) : Int := AddInt x #v[y]

@[apex_implements Int.sub]
def Int.sub.impl (x y : Int) : Int := SubtractInt x #v[y]

@[apex_implements Int.mul]
def Int.mul.impl (x y : Int) : Int := MultiplyInt x #v[y]

-- todo: is this the right divistion?
@[apex_implements Int.ediv]
def Int.div.impl (x y : Int) : Int := DivideInt x #v[y]

@[apex_implements Int.neg]
def Int.neg.impl (x : Int) : Int := NegateInt x

-- todo: is this the right modulo?
@[apex_implements Int.emod]
def Int.mod.impl (x y : Int) : Int := Modulo x y

-- Vector2 operations
instance : Add Vector2 where
  add x y := AddVector2 x #v[y]

instance : Sub Vector2 where
  sub x y := SubtractVector2 x #v[y]

-- missing
instance : Mul Vector2 where
  mul x y := -- MultiplyVector2 x #v[y]
    apexPanic! "Element wise multiplication for Vector2 is not implementde!" 

instance : Neg Vector2 where
  neg x := NegateVector2 x

instance : HMul Vector2 Float Vector2 where
  hMul v s := MultiplyVector2Float v #v[s]

instance : HMul Float Vector2 Vector2 where
  hMul s v := MultiplyVector2Float v #v[s]

-- Vector3 operations
instance : Add Vector3 where
  add x y := AddVector3 x #v[y]

instance : Sub Vector3 where
  sub x y := SubtractVector3 x #v[y]

instance : Mul Vector3 where
  mul x y := MultiplyVector3 x #v[y]

instance : Div Vector3 where
  div x y := DivideVector3 x #v[y]

instance : Neg Vector3 where
  neg x := NegateVector3 x

instance : HMul Vector3 Float Vector3 where
  hMul v s := MultiplyVector3Float v #v[s]

instance : HMul Float Vector3 Vector3 where
  hMul s v := MultiplyVector3Float v #v[s]

instance : HMul Vector3 Matrix3 Vector3 where
  hMul v m := MultiplyVector3Matrix3 v #v[m]

instance : HMul Vector3 Matrix4 Vector3 where
  hMul v m := MultiplyVector3Matrix4 v #v[m]

-- Vector4 operations
instance : Add Vector4 where
  add x y := -- AddVector4 x #v[y]
    apexPanic! "Addition for Vector4 is not implemented!" 

instance : Sub Vector4 where
  sub x y := -- SubtractVector4 x #v[y]
    apexPanic! "Subtraction for Vector4 is not implemented!" 

instance : HMul Vector4 Float Vector4 where
  hMul v s := MultiplyVector4Float v #v[s]

instance : HMul Float Vector4 Vector4 where
  hMul s v := MultiplyVector4Float v #v[s]

instance : HMul Vector4 Matrix4 Vector4 where
  hMul v m := MultiplyVector4Matrix4 v #v[m]

instance : Neg Vector4 where
  neg x := -- NegateVector4 x
    apexPanic! "Negation for Vector4 is not implemented!" 

-- Matrix operations
instance : Add Matrix3 where
  add x y := AddMatrix3 x #v[y]

instance : Sub Matrix3 where
  sub x y := SubtractMatrix3 x #v[y]

instance : Mul Matrix3 where
  mul x y := MultiplyMatrix3 x #v[y]

instance : HMul Matrix3 Float Matrix3 where
  hMul m s := MultiplyMatrix3Float m #v[s]

instance : HMul Float Matrix3 Matrix3 where
  hMul s m := MultiplyMatrix3Float m #v[s]

instance : Add Matrix4 where
  add x y := AddMatrix4 x #v[y]

instance : Sub Matrix4 where
  sub x y := SubtractMatrix4 x #v[y]

instance : Mul Matrix4 where
  mul x y := MultiplyMatrix4 x #v[y]

instance : HMul Matrix4 Float Matrix4 where
  hMul m s := MultiplyMatrix4Float m #v[s]

instance : HMul Float Matrix4 Matrix4 where
  hMul s m := MultiplyMatrix4Float m #v[s]

instance : HMul Matrix4 Matrix3 Matrix4 where
  hMul m1 m2 := MultiplyMatrix4Matrix3 m1 #v[m2]

-- String concatenation
instance : Add String where
  add x y := AddString x #v[y]

-- Boolean operations
instance : AndOp Bool where
  and x y := And #v[x, y]

instance : OrOp Bool where
  or x y := Or #v[x, y]

-- ============================================================================
-- Comparison Instances
-- ============================================================================

instance : BEq Float where
  beq x y := EqualsFloat x y

instance : LT Float where
  lt x y := LessThanFloat x y

instance : LE Float where
  le x y := LessThanOrEqualFloat x y

instance : BEq Int where
  beq x y := EqualsInt x y

instance : LT Int where
  lt x y := LessThanInt x y

instance : LE Int where
  le x y := LessThanOrEqualInt x y

instance : BEq Bool where
  beq x y := EqualsBool x y

instance : BEq String where
  beq x y := EqualsString x y

instance : BEq Vector2 where
  beq x y := EqualsVector2 x y

instance : BEq Vector3 where
  beq x y := EqualsVector3 x y

instance : BEq Vector4 where
  beq x y := EqualsVector4 x y

-- ============================================================================
-- Vector Projections and Construction
-- ============================================================================

namespace Vector2

@[apex_implements HouLean.Apex.Vector2.x]
def x.impl (v : Vector2) : Float := 
  let (x, _) := Vector2ToFloat v
  x

@[apex_implements HouLean.Apex.Vector2.y]
def y.impl (v : Vector2) : Float := 
  let (_, y) := Vector2ToFloat v
  y

@[apex_implements HouLean.Apex.Vector2.mk]
def mk.impl (x y : Float) : Vector2 := FloatToVector2 x y


end Vector2

namespace Vector3

@[apex_implements HouLean.Apex.Vector3.x]
def x.impl (v : Vector3) : Float := 
  let (x, _, _) := Vector3ToFloat v
  x

@[apex_implements HouLean.Apex.Vector3.y]
def y.impl (v : Vector3) : Float := 
  let (_, y, _) := Vector3ToFloat v
  y

@[apex_implements HouLean.Apex.Vector3.z]
def z.impl (v : Vector3) : Float := 
  let (_, _, z) := Vector3ToFloat v
  z

@[apex_implements HouLean.Apex.Vector3.mk]
def mk.impl (x y z : Float) : Vector3 := FloatToVector3 x y z

def cross (a b : Vector3) : Vector3 := CrossProduct a b

end Vector3

namespace Vector4

@[apex_implements HouLean.Apex.Vector4.x]
def x.impl (v : Vector4) : Float := 
  let (x, _, _, _) := Vector4ToFloat v
  x

@[apex_implements HouLean.Apex.Vector4.y]
def y.impl (v : Vector4) : Float := 
  let (_, y, _, _) := Vector4ToFloat v
  y

@[apex_implements HouLean.Apex.Vector4.z]
def z.impl (v : Vector4) : Float := 
  let (_, _, z, _) := Vector4ToFloat v
  z

@[apex_implements HouLean.Apex.Vector4.w]
def w.impl (v : Vector4) : Float := 
  let (_, _, _, w) := Vector4ToFloat v
  w

@[apex_implements HouLean.Apex.Vector4.mk]
def mk.impl (x y z w : Float) : Vector4 := FloatToVector4 x y z w

def nlerp (a b : Vector4) (bias : Float) : Vector4 := NLerpVector4 a b bias

end Vector4

-- ============================================================================
-- Math Functions
-- ============================================================================

namespace Math

-- Trigonometry
@[apex_implements Float.sin]
def sin (x : Float) : Float := Sine x
@[apex_implements Float.cos]
def cos (x : Float) : Float := Cosine x
@[apex_implements Float.tan]
def tan (x : Float) : Float := Tan x
@[apex_implements Float.asin]
def asin (x : Float) : Float := Asin x
@[apex_implements Float.acos]
def acos (x : Float) : Float := Acos x
@[apex_implements Float.atan]
def atan (x : Float) : Float := Atan x
@[apex_implements Float.atan2]
def atan2 (y x : Float) : Float := Atan2 x y


-- Rounding
@[apex_implements Float.floor]
def floor (x : Float) : Float := Floor x
@[apex_implements Float.ceil]
def ceil (x : Float) : Float := Ceil x
@[apex_implements Float.round]
def round (x : Float) : Float := Round x 0
def frac (x : Float) : Float := Frac x


-- Power and roots
@[apex_implements Float.pow]
def pow (base exp : Float) : Float := Exponent base exp
@[apex_implements Float.sqrt]
def sqrt (x : Float) : Float := Exponent x 0.5

-- Absolute value typeclass
class Abs (α : Type) where
  abs : α → α

export Abs (abs)

instance : Abs Float where
  abs := AbsFloat

instance : Abs Int where
  abs := AbsInt

instance : Abs Vector2 where
  abs := AbsVector2

instance : Abs Vector3 where
  abs := AbsVector3

instance : Abs Vector4 where
  abs := AbsVector4

-- Min/Max typeclasses
class Min (α : Type) where
  min : α → α → α

export Min (min)

instance : Min Float where
  min a b := MinFloat a #v[b]

instance : Min Int where
  min a b := MinInt a #v[b]

class Max (α : Type) where
  max : α → α → α

export Max (max)

instance : Max Float where
  max a b := MaxFloat a #v[b]

instance : Max Int where
  max a b := MaxInt a #v[b]

-- Clamping typeclass
class Clamp (α : Type) where
  clamp : α → α → α → α

export Clamp (clamp)

instance : Clamp Float where
  clamp := ClampFloat

instance : Clamp Int where
  clamp := ClampInt

-- Length/Magnitude typeclass
class HasLength (α : Type) where
  length : α → Float

export HasLength (length)

instance : HasLength Vector2 where
  length := LengthVector2

instance : HasLength Vector3 where
  length := LengthVector3

instance : HasLength Vector4 where
  length := LengthVector4

-- Normalize typeclass
class Normalize (α : Type) where
  normalize : α → α × Float
  normalized : α → α

export Normalize (normalize normalized)

instance : Normalize Vector2 where
  normalize := NormalizeVector2
  normalized v := (NormalizeVector2 v).1

instance : Normalize Vector3 where
  normalize := NormalizeVector3
  normalized v := (NormalizeVector3 v).1

instance : Normalize Vector4 where
  normalize := NormalizeVector4
  normalized v := (NormalizeVector4 v).1

-- Dot product typeclass
class DotProduct (α : Type) where
  dot : α → α → Float

export DotProduct (dot)

instance : DotProduct Vector2 where
  dot := DotProductVector2

instance : DotProduct Vector3 where
  dot := DotProductVector3

instance : DotProduct Vector4 where
  dot := DotProductVector4

-- Distance typeclass
class Distance (α : Type) where
  distance : α → α → Float

export Distance (distance)

instance : Distance Vector2 where
  distance := DistanceVector2

instance : Distance Vector3 where
  distance := DistanceVector3

instance : Distance Vector4 where
  distance := DistanceVector4

-- Quantize typeclass
class Quantize (α : Type) where
  quantize : α → Float → α

export Quantize (quantize)

instance : Quantize Float where
  quantize := QuantizeFloat

instance : Quantize Vector2 where
  quantize := QuantizeVector2

instance : Quantize Vector3 where
  quantize := QuantizeVector3

-- Invert typeclass (for matrices)
class Invert (α : Type) where
  invert : α → α

export Invert (invert)

instance : Invert Matrix3 where
  invert := InvertMatrix3

instance : Invert Matrix4 where
  invert := InvertMatrix4

-- Transpose typeclass
class Transpose (α : Type) where
  transpose : α → α

export Transpose (transpose)

instance : Transpose Matrix3 where
  transpose := TransposeMatrix3

instance : Transpose Matrix4 where
  transpose := TransposeMatrix4

-- Angle conversion typeclasses
class DegreesToRadians (α : Type) where
  degToRad : α → α

export DegreesToRadians (degToRad)

instance : DegreesToRadians Float where
  degToRad := DegreesToRadiansFloat

instance : DegreesToRadians Vector3 where
  degToRad := DegreesToRadiansVector3

class RadiansToDegrees (α : Type) where
  radToDeg : α → α

export RadiansToDegrees (radToDeg)

instance : RadiansToDegrees Float where
  radToDeg := RadiansToDegreesFloat

instance : RadiansToDegrees Vector3 where
  radToDeg := RadiansToDegreesVector3

-- Interpolation typeclass
class Lerp (α : Type) where
  lerp : α → α → Float → α

export Lerp (lerp)

instance : Lerp Float where
  lerp := LerpFloat

instance : Lerp Vector2 where
  lerp := LerpVector2

instance : Lerp Vector3 where
  lerp := LerpVector3

instance : Lerp Vector4 where
  lerp := LerpVector4

instance : Lerp Matrix3 where
  lerp := LerpMatrix3

instance : Lerp Matrix4 where
  lerp := LerpMatrix4

-- Mapping
def fit (val srcMin srcMax dstMin dstMax : Float) : Float := 
  Fit val srcMin srcMax dstMin dstMax

def fit01 (val dstMin dstMax : Float) : Float := Fit01 val dstMin dstMax
def fit10 (val dstMin dstMax : Float) : Float := Fit10 val dstMin dstMax
def fit11 (val dstMin dstMax : Float) : Float := Fit11 val dstMin dstMax

-- Noise
def noise (x : Float) : Float := NoiseFloatFloat x

-- Complement
def complement (x : Float) : Float := Complement x

-- Average typeclass
class Average (α : Type) where
  average : VariadicArg α n → α

export Average (average)

instance : Average Float where
  average xs := AverageFloat xs

-- AlmostEquals typeclass
class AlmostEquals (α : Type) where
  almostEquals : α → α → Float → Bool

export AlmostEquals (almostEquals)

instance : AlmostEquals Float where
  almostEquals := AlmostEqualsFloat

instance : AlmostEquals Matrix3 where
  almostEquals := AlmostEqualsMatrix3

instance : AlmostEquals Matrix4 where
  almostEquals := AlmostEqualsMatrix4

end Math

-- ============================================================================
-- Matrix Operations
-- ============================================================================

namespace Matrix3

def getComponent (m : Matrix3) (row col : Int) : Float := 
  GetComponentMatrix3 m row col

def setComponent (m : Matrix3) (value : Float) (row col : Int) : Matrix3 := 
  SetComponentMatrix3 m value row col

def toRows (m : Matrix3) : Vector3 × Vector3 × Vector3 := Matrix3ToVector3 m

def fromRows (r1 r2 r3 : Vector3) : Matrix3 := Vector3ToMatrix3 r1 r2 r3

end Matrix3

namespace Matrix4

def getComponent (m : Matrix4) (row col : Int) : Float := 
  GetComponentMatrix4 m row col

def setComponent (m : Matrix4) (value : Float) (row col : Int) : Matrix4 := 
  SetComponentMatrix4 m value row col

def toRows (m : Matrix4) : Vector4 × Vector4 × Vector4 × Vector4 := Matrix4ToVector4 m

def fromRows (r1 r2 r3 r4 : Vector4) : Matrix4 := Vector4ToMatrix4 r1 r2 r3 r4

end Matrix4

-- ============================================================================
-- Coordinate System Conversions
-- ============================================================================

namespace Coords

def polarToCartesian (polar : Vector2) : Vector2 := PolarToCartesian polar
def cartesianToPolar (cart : Vector2) : Vector2 := CartesianToPolar cart

def sphericalToCartesian (spherical : Vector3) : Vector3 := SphericalToCartesian spherical
def cartesianToSpherical (cart : Vector3) : Vector3 := CartesianToSpherical cart

def geodeticToCartesian (geodetic : Vector3) : Vector3 := GeodeticToCartesian geodetic
def cartesianToGeodetic (cart : Vector3) : Vector3 := CartesianToGeodetic cart

end Coords

-- ============================================================================
-- Conversion Type Classes
-- ============================================================================

class ApexCast (α β : Type) where
  cast : α → β

instance : ApexCast Bool Int where
  cast := ConvertBoolInt

instance : ApexCast Int Bool where
  cast := ConvertIntBool

instance : ApexCast Float Int where
  cast := ConvertFloatInt

instance : ApexCast Int Float where
  cast := ConvertIntFloat

instance : ApexCast Matrix3 Matrix4 where
  cast := ConvertMatrix3Matrix4

instance : ApexCast Matrix4 Matrix3 where
  cast := ConvertMatrix4Matrix3

instance : ApexCast Matrix4 Vector4 where
  cast := ConvertMatrix4Vector4

instance : ApexCast Vector4 Matrix3 where
  cast := ConvertVector4Matrix3

instance : ApexCast ColorRamp FloatRamp where
  cast := ConvertColorRampFloatRamp

instance : ApexCast FloatRamp ColorRamp where
  cast := ConvertFloatRampColorRamp

def cast [ApexCast α β] (x : α) : β := ApexCast.cast x


-- ============================================================================
-- Option
-- ============================================================================

-- During compilation we should treat `Option α` the same as `α × Bool`
-- Helper function to convert APEX's (α × Bool) to Option α
def toOption {α : Type} (x : α × Bool) : Option α := 
  if x.2 then
    some x.1
  else
    none

def fromOption {α : Type} [Inhabited α] (x : Option α) : α × Bool :=
  match x with
  | some x => (x, true)
  | none => (default, false)

-- attribute [apex_type_implemented_by toOption fromOption] Option

-- Bool should get translate automatically once convert enum(simple inductives) to Int
-- apex_type_implemente_by Bool Bool.toInt Int.toBool
-- apex_type_implemente_by Nat  Nat.toInt Int.toNat
-- apex_type_implemente_by Nat  Nat.toInt Int.toNat
