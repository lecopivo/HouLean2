import HouLean.Apex.Lean.Decidable
import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy

open HouLean Apex Compiler Generated

-- Bool logical operations
@[apex_implements Bool.and]
def Bool.and.impl (x y : Bool) : Bool := Generated.And #a[x, y]

@[apex_implements Bool.or]
def Bool.or.impl (x y : Bool) : Bool := Generated.Or #a[x, y]

@[apex_implements Bool.xor]
def Bool.xor.impl (x y : Bool) : Bool := Generated.Xor #a[x, y]

-- @[apex_implements Bool.decEq]
-- def Bool.decEq.apex_impl (x y : Bool) : Bool := EqualsBool x y 

-- @[apex_implements Bool.instDecidableLt]
-- def Bool.decLt.apex_impl (x y : Bool) : Bool := LessThanBool x y

-- @[apex_implements Bool.instDecidableLe]
-- def Bool.decLe.apex_impl (x y : Bool) : Bool := LessThanOrEqualBool x y

open TwoWaySwitch in
unsafe def Bool.rec.apex_impl {motive : Bool → Type u} [∀ b, TwoWaySwitch (motive b)]
    (e : motive false) (t : motive true) (c : Bool) : motive c :=
  twoWaySwitch (α:=motive c) (unsafeCast e) (unsafeCast t) c

open HouLean.Apex.Compiler in
run_meta compilerExt.add (.implementedByName ``Bool.rec ``Bool.rec.apex_impl #[some 0, none, some 1, some 2, some 3]) default


-- /-- This is a hack to ensure that `if c then a else b` for `c : Bool` will generate only one 
-- `TwoWaySwitch` and not additional `EqualsBool` node -/
-- class BoolDecide (a b : Bool) where
--   boolDecide : Decidable (a = b)

-- unsafe instance (priority:=high) : BoolDecide a true where
--   boolDecide := fromApex a

-- unsafe instance (priority:=high) : BoolDecide a b where
--   boolDecide := fromApex (EqualsBool a b)

-- def instDecidableEqBool.apex_impl (a b : Bool) [d : BoolDecide a b] : Decidable (a = b) := d.1

-- open HouLean.Apex.Compiler in
-- run_meta compilerExt.add (.implementedByName ``instDecidableEqBool ``instDecidableEqBool.apex_impl #[some 0, some 1, none])
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
