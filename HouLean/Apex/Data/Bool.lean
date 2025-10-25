import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy

open HouLean.Apex.Generated

-- Bool logical operations
noncomputable
def Bool.and.impl (x y : Bool) : Bool := And #v[x, y]

attribute [apex_implemented_by Bool.and.impl] Bool.and

noncomputable
def Bool.or.impl (x y : Bool) : Bool := Or #v[x, y]

attribute [apex_implemented_by Bool.or.impl] Bool.or

noncomputable
def Bool.xor.impl (x y : Bool) : Bool := Xor #v[x, y]

attribute [apex_implemented_by Bool.xor.impl] Bool.xor

-- noncomputable
-- def Bool.not.impl (x : Bool) : Bool := Not x

-- attribute [apex_implemented_by Bool.not.impl] Bool.not

-- Bool comparison operations
-- noncomputable
-- def Bool.beq.impl (x y : Bool) : Bool := EqualsBool x y

-- attribute [apex_implemented_by Bool.beq.impl] Bool.beq

-- noncomputable
-- def Bool.blt.impl (x y : Bool) : Bool := LessThanBool x y

-- attribute [apex_implemented_by Bool.blt.impl] Bool.blt

-- noncomputable
-- def Bool.ble.impl (x y : Bool) : Bool := LessThanOrEqualBool x y

-- attribute [apex_implemented_by Bool.ble.impl] Bool.ble
