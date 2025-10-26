import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Data.Vector4

open HouLean.Apex.Generated

namespace HouLean.Vector4

-- APEX implementations

-- Constructor
noncomputable
def mk.apex_impl (x y z w : Float) : Vector4 := FloatToVector4 x y z w

attribute [apex_implemented_by HouLean.Vector4.mk.apex_impl] mk

-- Component accessors
noncomputable
def x.apex_impl (v : Vector4) : Float := GetComponentVector4 v 0

attribute [apex_implemented_by HouLean.Vector4.x.apex_impl] x

noncomputable
def y.apex_impl (v : Vector4) : Float := GetComponentVector4 v 1

attribute [apex_implemented_by HouLean.Vector4.y.apex_impl] y

noncomputable
def z.apex_impl (v : Vector4) : Float := GetComponentVector4 v 2

attribute [apex_implemented_by HouLean.Vector4.z.apex_impl] z

noncomputable
def w.apex_impl (v : Vector4) : Float := GetComponentVector4 v 3

attribute [apex_implemented_by HouLean.Vector4.w.apex_impl] w

-- Arithmetic operations
-- noncomputable
-- def add.apex_impl (a b : Vector4) : Vector4 := AddVector4 a #v[b]

-- attribute [apex_implemented_by HouLean.Vector4.add.apex_impl] add

-- noncomputable
-- def sub.apex_impl (a b : Vector4) : Vector4 := SubtractVector4 a #v[b]

-- attribute [apex_implemented_by HouLean.Vector4.sub.apex_impl] sub

noncomputable
def mul.apex_impl (a : Vector4) (s : Float) : Vector4 := MultiplyVector4Float a #v[s]

attribute [apex_implemented_by HouLean.Vector4.mul.apex_impl] mul

noncomputable
def dot.apex_impl (a b : Vector4) : Float := DotProductVector4 a b

attribute [apex_implemented_by HouLean.Vector4.dot.apex_impl] dot

noncomputable
def length.apex_impl (v : Vector4) : Float := LengthVector4 v

attribute [apex_implemented_by HouLean.Vector4.length.apex_impl] length

noncomputable
def normalize.apex_impl (v : Vector4) : Vector4 × Float := 
  let result := NormalizeVector4 v
  (result.1, result.2)

attribute [apex_implemented_by HouLean.Vector4.normalize.apex_impl] normalize

noncomputable
def distance.apex_impl (a b : Vector4) : Float := DistanceVector4 a b

attribute [apex_implemented_by HouLean.Vector4.distance.apex_impl] distance

noncomputable
def lerp.apex_impl (a b : Vector4) (t : Float) : Vector4 := LerpVector4 a b t

attribute [apex_implemented_by HouLean.Vector4.lerp.apex_impl] lerp

noncomputable
def nlerp.apex_impl (a b : Vector4) (t : Float) : Vector4 := NLerpVector4 a b t

attribute [apex_implemented_by HouLean.Vector4.nlerp.apex_impl] nlerp

noncomputable
def beq.apex_impl (a b : Vector4) : Bool := EqualsVector4 a b

attribute [apex_implemented_by HouLean.Vector4.beq.apex_impl] beq

noncomputable
def blt.apex_impl (a b : Vector4) : Bool := LessThanVector4 a b

attribute [apex_implemented_by HouLean.Vector4.blt.apex_impl] blt

noncomputable
def ble.apex_impl (a b : Vector4) : Bool := LessThanOrEqualVector4 a b

attribute [apex_implemented_by HouLean.Vector4.ble.apex_impl] ble

noncomputable
def abs.apex_impl (v : Vector4) : Vector4 := AbsVector4 v

attribute [apex_implemented_by HouLean.Vector4.abs.apex_impl] abs

end Vector4
