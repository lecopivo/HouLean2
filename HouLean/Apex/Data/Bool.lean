import HouLean.Apex.Lean.Decidable
import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy

open HouLean.Apex.Generated

-- Bool logical operations
@[apex_implements Bool.and]
def Bool.and.impl (x y : Bool) : Bool := And #v[x, y]

@[apex_implements Bool.or]
def Bool.or.impl (x y : Bool) : Bool := Or #v[x, y]

@[apex_implements Bool.xor]
def Bool.xor.impl (x y : Bool) : Bool := Xor #v[x, y]

@[apex_implements Bool.decEq]
def Bool.decEq.apex_impl (x y : Bool) : Bool := EqualsBool x y 

@[apex_implements Bool.instDecidableLt]
def Bool.decLt.apex_impl (x y : Bool) : Bool := LessThanBool x y

@[apex_implements Bool.instDecidableLe]
def Bool.decLe.apex_impl (x y : Bool) : Bool := LessThanOrEqualBool x y

def Bool.rec.apex_impl {motive : Bool → Sort u} (e : motive false) (t : motive true) (c : Bool) : motive c :=
  if h : c then
    cast (by grind) t
  else
    cast (by grind) e

open HouLean.Apex.Compiler in
run_meta compilerExt.add (.implementedByName ``Bool.rec ``Bool.rec.apex_impl #[some 0, some 1, some 2, some 3]) default

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
