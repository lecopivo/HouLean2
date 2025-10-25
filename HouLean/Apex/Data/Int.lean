import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy

open HouLean.Apex.Generated

-- Int arithmetic operations
noncomputable
def Int.add.impl (x y : Int) : Int := AddInt x #v[y]

attribute [apex_implemented_by Int.add.impl] Int.add

noncomputable
def Int.sub.impl (x y : Int) : Int := SubtractInt x #v[y]

attribute [apex_implemented_by Int.sub.impl] Int.sub

noncomputable
def Int.mul.impl (x y : Int) : Int := MultiplyInt x #v[y]

attribute [apex_implemented_by Int.mul.impl] Int.mul

noncomputable
def Int.div.impl (x y : Int) : Int := DivideInt x #v[y]

attribute [apex_implemented_by Int.div.impl] Int.ediv

noncomputable
def Int.neg.impl (x : Int) : Int := NegateInt x

attribute [apex_implemented_by Int.neg.impl] Int.neg

noncomputable
def Int.mod.impl (x y : Int) : Int := Modulo x y

attribute [apex_implemented_by Int.mod.impl] Int.emod

-- Int comparison operations
-- noncomputable
-- def Int.beq.impl (x y : Int) : Bool := EqualsInt x y

-- attribute [apex_implemented_by Int.beq.impl] Int.beq

-- noncomputable
-- def Int.blt.impl (x y : Int) : Bool := LessThanInt x y

-- attribute [apex_implemented_by Int.blt.impl] Int.blt

-- noncomputable
-- def Int.ble.impl (x y : Int) : Bool := LessThanOrEqualInt x y

-- attribute [apex_implemented_by Int.ble.impl] Int.ble

-- Int utility functions

noncomputable
def Int.abs.impl (x : Int) : Int := AbsInt x

@[apex_implemented_by Int.abs.impl] 
def Int.abs (x : Int) : Int := x.natAbs

noncomputable
def Int.min.impl (x y : Int) : Int := MinInt x #v[y]

@[apex_implemented_by Int.min.impl] 
def Int.min (x y : Int) : Int := Min.min x y

noncomputable
def Int.max.impl (x y : Int) : Int := MaxInt x #v[y]

@[apex_implemented_by Int.max.impl] 
def Int.max (x y : Int) : Int := Max.max x y

noncomputable
def Int.clamp.impl (x min max : Int) : Int := ClampInt x min max

@[apex_implemented_by Int.clamp.impl] 
def Int.clamp (x min max : Int) : Int := (x.max min).min max

-- Int bitwise operations
-- noncomputable
-- def Int.land.impl (x y : Int) : Int := BitwiseAnd #v[x, y]

-- attribute [apex_implemented_by Int.land.impl] Int.land

-- noncomputable
-- def Int.lor.impl (x y : Int) : Int := BitwiseOr #v[x, y]

-- attribute [apex_implemented_by Int.lor.impl] Int.lor

-- noncomputable
-- def Int.lxor.impl (x y : Int) : Int := BitwiseXor #v[x, y]

-- attribute [apex_implemented_by Int.lxor.impl] Int.lxor

-- noncomputable
-- def Int.complement.impl (x : Int) : Int := BitwiseNot x

-- attribute [apex_implemented_by Int.complement.impl] Int.complement

