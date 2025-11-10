import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Apex.Lean.Decidable

open HouLean.Apex.Generated

-- Float arithmetic operations
@[apex_implements Float.add]
noncomputable
def Float.add.impl (x y : Float) : Float := AddFloat x #a[y]

@[apex_implements Float.sub]
noncomputable
def Float.sub.impl (x y : Float) : Float := SubtractFloat x #a[y]

@[apex_implements Float.mul]
noncomputable
def Float.mul.impl (x y : Float) : Float := MultiplyFloat x #a[y]

@[apex_implements Float.div]
noncomputable
def Float.div.impl (x y : Float) : Float := DivideFloat x #a[y]

@[apex_implements Float.neg]
noncomputable
def Float.neg.impl (x : Float) : Float := NegateFloat x

-- Float comparison operations
@[apex_implements Float.beq]
noncomputable
def Float.beq.impl (x y : Float) : Bool := EqualsFloat x y

-- @[apex_implements Float.decLt]
-- noncomputable
-- def Float.blt.impl (x y : Float) : Bool := LessThanFloat x y

-- @[apex_implements Float.decLe]
-- noncomputable
-- def Float.ble.impl (x y : Float) : Bool := LessThanOrEqualFloat x y

-- Float mathematical functions
@[apex_implements Float.abs]
noncomputable
def Float.abs.impl (x : Float) : Float := AbsFloat x

@[apex_implements Float.sqrt]
noncomputable
def Float.sqrt.impl (x : Float) : Float := Exponent x 0.5

@[apex_implements Float.pow]
noncomputable
def Float.pow.impl (x y : Float) : Float := Exponent x y

@[apex_implements Float.sin]
noncomputable
def Float.sin.impl (x : Float) : Float := Sine x

@[apex_implements Float.cos]
noncomputable
def Float.cos.impl (x : Float) : Float := Cosine x

@[apex_implements Float.tan]
noncomputable
def Float.tan.impl (x : Float) : Float := Tan x

@[apex_implements Float.asin]
noncomputable
def Float.asin.impl (x : Float) : Float := Asin x

@[apex_implements Float.acos]
noncomputable
def Float.acos.impl (x : Float) : Float := Acos x

@[apex_implements Float.atan]
noncomputable
def Float.atan.impl (x : Float) : Float := Atan x

@[apex_implements Float.atan2]
noncomputable
def Float.atan2.impl (y x : Float) : Float := Atan2 x y

-- Float utility functions
@[apex_implements Float.floor]
noncomputable
def Float.floor.impl (x : Float) : Float := Floor x

@[apex_implements Float.ceil]
noncomputable
def Float.ceil.impl (x : Float) : Float := Ceil x

@[apex_implements Float.round]
noncomputable
def Float.round.impl (x : Float) : Float := Round x 0

-- @[apex_implements Float.frac]
-- noncomputable
-- def Float.frac.impl (x : Float) : Float := Frac x

@[apex_implements Float.min]
noncomputable
def Float.min.impl (x y : Float) : Float := MinFloat x #a[y]

@[apex_implements Float.max]
noncomputable
def Float.max.impl (x y : Float) : Float := MaxFloat x #a[y]

@[apex_implements Float.clamp]
noncomputable
def Float.clamp.impl (x min max : Float) : Float := ClampFloat x min max

@[apex_implements Float.lerp]
noncomputable
def Float.lerp.impl (a b t : Float) : Float := LerpFloat a b t
