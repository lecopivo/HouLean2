import HouLean.Meta.SpecializeAndSimp2.Encoding

open Lean

-- namespace HouLean.Meta.Sas

/-! ## Option Encoding -/
def Option.decode (x : α) (valid : Bool) : Option α :=
  if valid then some x else none

namespace Option

@[simp] theorem decode_true (x : α) : Option.decode x true = some x := by simp [Option.decode]
@[simp] theorem decode_false (x : α) : Option.decode x false = none := by simp [Option.decode]

@[simp] theorem decode_getD (x y : α) (valid : Bool) :
    (Option.decode x valid).getD y = if valid then x else y := by
  cases valid <;> simp [Option.decode]

@[simp] theorem decode_isSome (x : α) (valid : Bool) :
    (Option.decode x valid).isSome = valid := by
  cases valid <;> simp [Option.decode]

@[simp] theorem decode_isNone (x : α) (valid : Bool) :
    (Option.decode x valid).isNone = !valid := by
  cases valid <;> simp [Option.decode]
