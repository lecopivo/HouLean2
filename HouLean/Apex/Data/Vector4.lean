import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Data.Vector4

open HouLean.Apex Generated

namespace HouLean.Vector4

-- APEX implementations

-- Constructor
@[apex_implements Vector4.mk]
def mk.apex_impl (x y z w : Float) : Vector4 := FloatToVector4 x y z w

-- Component accessors
@[apex_implements Vector4.x]
def x.apex_impl (v : Vector4) : Float := (Generated.Vector4ToFloat v).1

@[apex_implements Vector4.y]
def y.apex_impl (v : Vector4) : Float := (Generated.Vector4ToFloat v).2.1

@[apex_implements Vector4.z]
def z.apex_impl (v : Vector4) : Float := (Generated.Vector4ToFloat v).2.2.1

@[apex_implements Vector4.w]
def w.apex_impl (v : Vector4) : Float := (Generated.Vector4ToFloat v).2.2.2
