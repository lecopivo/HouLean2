import HouLean.Apex.Lean.Decidable

open HouLean Apex Compiler

-- def ite.apex_impl {α} [TwoWaySwitch α] (c : Prop) [h : Decidable c] (t e : α) :=
--   TwoWaySwitch.twoWaySwitch e t (decide c)

-- unsafe def dite.apex_impl {α} [TwoWaySwitch α] (c : Prop) [h : Decidable c] (t : c → α) (e : ¬c → α) :=
--   TwoWaySwitch.twoWaySwitch (e (unsafeCast ())) (t (unsafeCast ())) (decide c)

-- run_meta compilerExt.add (.implementedByName ``ite ``ite.apex_impl
--   #[some 0, none, some 1, some 2, some 3, some 4]) default

-- run_meta compilerExt.add (.implementedByName ``dite ``dite.apex_impl
--   #[some 0, none, some 1, some 2, some 3, some 4]) default
