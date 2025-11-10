import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Apex.Lean.Decidable

open HouLean.Apex.Generated

-- Int arithmetic operations
@[apex_implements Int.add]
def Int.add.apex_impl (x y : Int) : Int := AddInt x #a[y]

@[apex_implements Int.sub]
def Int.sub.apex_impl (x y : Int) : Int := SubtractInt x #a[y]

@[apex_implements Int.mul]
def Int.mul.apex_impl (x y : Int) : Int := MultiplyInt x #a[y]

@[apex_implements Int.ediv]
def Int.div.apex_impl (x y : Int) : Int := DivideInt x #a[y]

@[apex_implements Int.neg]
def Int.neg.apex_impl (x : Int) : Int := NegateInt x

@[apex_implements Int.emod]
def Int.mod.apex_impl (x y : Int) : Int := Modulo x y

-- @[apex_implements Int.decEq]
-- def Int.decEq.apex_impl (x y : Int) : Bool := EqualsInt x y 

-- @[apex_implements Int.decLt]
-- def Int.decLt.apex_impl (x y : Int) : Bool := LessThanInt x y

-- @[apex_implements Int.decLe]
-- def Int.decLe.apex_impl (x y : Int) : Bool := LessThanOrEqualInt x y

-- todo: somehow make sure that `min x y` uses this
def Int.min.apex_impl (x y : Int) : Int := MinInt x #a[y]

-- todo: somehow make sure that `max x y` uses this
def Int.max.apex_impl (x y : Int) : Int := MaxInt x #a[y]

def Int.clamp (x lo hi : Int) : Int := min (max x lo) hi

@[apex_implements Int.clamp]
def Int.clamp.apex_impl (x lo hi : Int) : Int := ClampInt x lo hi

-- Int bitwise operations
-- noncomputable
-- def Int.land.apex_impl (x y : Int) : Int := BitwiseAnd #a[x, y]

-- attribute [apex_implemented_by Int.land.apex_impl] Int.land

-- noncomputable
-- def Int.lor.apex_impl (x y : Int) : Int := BitwiseOr #a[x, y]

-- attribute [apex_implemented_by Int.lor.apex_impl] Int.lor

-- noncomputable
-- def Int.lxor.apex_impl (x y : Int) : Int := BitwiseXor #a[x, y]

-- attribute [apex_implemented_by Int.lxor.apex_impl] Int.lxor

-- noncomputable
-- def Int.complement.apex_impl (x : Int) : Int := BitwiseNot x

-- attribute [apex_implemented_by Int.complement.apex_impl] Int.complement

