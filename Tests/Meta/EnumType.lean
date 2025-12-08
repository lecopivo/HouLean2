import HouLean.Meta.EnumType
import Lean.Elab.GuardMsgs

open HouLean

namespace Tests.Meta.EnumType

inductive A where
  | a | b | c
deriving EnumType

/--
info: def Tests.Meta.EnumType.instEnumTypeA : EnumType A :=
{ n := 3,
  toFin := fun x =>
    match x with
    | A.a => 0
    | A.b => 1
    | A.c => 2,
  fromFin := fun i =>
    match i with
    | 0 => A.a
    | 1 => A.b
    | 2 => A.c }
-/
#guard_msgs in
#print instEnumTypeA
