import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy

open HouLean.Apex.Generated

-- Float arithmetic operations
noncomputable
def Float.add.impl (x y : Float) : Float := AddFloat x #v[y]

attribute [apex_implemented_by Float.add.impl] Float.add

noncomputable
def Float.sub.impl (x y : Float) : Float := SubtractFloat x #v[y]

attribute [apex_implemented_by Float.sub.impl] Float.sub

noncomputable
def Float.mul.impl (x y : Float) : Float := MultiplyFloat x #v[y]

attribute [apex_implemented_by Float.mul.impl] Float.mul

noncomputable
def Float.div.impl (x y : Float) : Float := DivideFloat x #v[y]

attribute [apex_implemented_by Float.div.impl] Float.div

noncomputable
def Float.neg.impl (x : Float) : Float := NegateFloat x

attribute [apex_implemented_by Float.neg.impl] Float.neg

-- Float comparison operations
noncomputable
def Float.beq.impl (x y : Float) : Bool := EqualsFloat x y

attribute [apex_implemented_by Float.beq.impl] Float.beq

-- noncomputable
-- def Float.blt.impl (x y : Float) : Bool := LessThanFloat x y

-- attribute [apex_implemented_by Float.blt.impl] Float.blt

-- noncomputable
-- def Float.ble.impl (x y : Float) : Bool := LessThanOrEqualFloat x y

-- attribute [apex_implemented_by Float.ble.impl] Float.ble

-- Float mathematical functions
noncomputable
def Float.abs.impl (x : Float) : Float := AbsFloat x

attribute [apex_implemented_by Float.abs.impl] Float.abs

noncomputable
def Float.sqrt.impl (x : Float) : Float := Exponent x 0.5

attribute [apex_implemented_by Float.sqrt.impl] Float.sqrt

noncomputable
def Float.pow.impl (x y : Float) : Float := Exponent x y

attribute [apex_implemented_by Float.pow.impl] Float.pow

noncomputable
def Float.sin.impl (x : Float) : Float := Sine x

attribute [apex_implemented_by Float.sin.impl] Float.sin

noncomputable
def Float.cos.impl (x : Float) : Float := Cosine x

attribute [apex_implemented_by Float.cos.impl] Float.cos

noncomputable
def Float.tan.impl (x : Float) : Float := Tan x

attribute [apex_implemented_by Float.tan.impl] Float.tan

noncomputable
def Float.asin.impl (x : Float) : Float := Asin x

attribute [apex_implemented_by Float.asin.impl] Float.asin

noncomputable
def Float.acos.impl (x : Float) : Float := Acos x

attribute [apex_implemented_by Float.acos.impl] Float.acos

noncomputable
def Float.atan.impl (x : Float) : Float := Atan x

attribute [apex_implemented_by Float.atan.impl] Float.atan

noncomputable
def Float.atan2.impl (y x : Float) : Float := Atan2 x y

attribute [apex_implemented_by Float.atan2.impl] Float.atan2

-- Float utility functions
noncomputable
def Float.floor.impl (x : Float) : Float := Floor x

attribute [apex_implemented_by Float.floor.impl] Float.floor

noncomputable
def Float.ceil.impl (x : Float) : Float := Ceil x

attribute [apex_implemented_by Float.ceil.impl] Float.ceil

noncomputable
def Float.round.impl (x : Float) : Float := Round x 0

attribute [apex_implemented_by Float.round.impl] Float.round

-- noncomputable
-- def Float.frac.impl (x : Float) : Float := Frac x

-- attribute [apex_implemented_by Float.frac.impl] Float.frac

noncomputable
def Float.min.impl (x y : Float) : Float := MinFloat x #v[y]

@[apex_implemented_by Float.min.impl] 
def Float.min (x y : Float) : Float := Min.min x y

noncomputable
def Float.max.impl (x y : Float) : Float := MaxFloat x #v[y]

@[apex_implemented_by Float.max.impl] 
def Float.max (x y : Float) : Float := Max.max x y

noncomputable
def Float.clamp.impl (x min max : Float) : Float := ClampFloat x min max

@[apex_implemented_by Float.clamp.impl] 
def Float.clamp (x min max : Float) : Float := (x.max min).min max

noncomputable
def Float.lerp.impl (a b t : Float) : Float := LerpFloat a b t

@[apex_implemented_by Float.lerp.impl]
def Float.lerp (a b t : Float) : Float := a + t * (b - a)


