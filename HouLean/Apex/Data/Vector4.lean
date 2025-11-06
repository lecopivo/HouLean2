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
