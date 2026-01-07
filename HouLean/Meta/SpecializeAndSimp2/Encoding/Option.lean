import HouLean.Meta.SpecializeAndSimp2.Encoding

open Lean

-- namespace HouLean.Meta.Sas

/-! ## Option Encoding -/
def Option.decode (x : α) (valid : Bool) : Option α :=
  if valid then some x else none

namespace Option

@[simp] theorem decode_true (x : α) : Option.decode x true = some x := by simp [Option.decode]
@[simp] theorem decode_false (x : α) : Option.decode x false = none := by simp [Option.decode]

-- @[simp] theorem decode_getD (x y : α) (valid : Bool) :
--     (Option.decode x valid).getD y = if valid then x else y := by
--   cases valid <;> simp [Option.decode]

-- @[simp] theorem decode_isSome (x : α) (valid : Bool) :
--     (Option.decode x valid).isSome = valid := by
--   cases valid <;> simp [Option.decode]

-- @[simp] theorem decode_isNone (x : α) (valid : Bool) :
--     (Option.decode x valid).isNone = !valid := by
--   cases valid <;> simp [Option.decode]


def encodeVal {α} [Inhabited α] (x : Option α) : α := x.getD default
def encodeValid {α} [Inhabited α] (x : Option α) : Bool := x.isSome

@[simp]
theorem encodeVal_some {α} [Inhabited α] (x : α) : encodeVal (some x) = x := by simp[encodeVal]
@[simp]
theorem encodeVal_none {α} [Inhabited α] : encodeVal (none : Option α) = default := by simp[encodeVal]

@[simp]
theorem encodeValid_some {α} [Inhabited α] (x : α) : encodeValid (some x) = true := by simp[encodeValid]
@[simp]
theorem encodeValid_none {α} [Inhabited α] : encodeValid (none : Option α) = false := by simp[encodeValid]


-- all these are getting out of hand, ... how to connect them to the actual Option API?
@[simp]
theorem decode_encode {α} [Inhabited α] (x : Option α) :
  decode x.encodeVal x.encodeValid = x := by cases x <;> simp

@[simp]
theorem encodeValid_decode {α} [Inhabited α] (x : α) (v : Bool) :
  encodeValid (decode x v) = v := by cases v <;> simp

@[simp]
theorem encodeVal_decode {α} [Inhabited α] (x : α) (v : Bool) :
  encodeVal (decode x v) = if v then x else default := by cases v <;> simp

@[simp]
theorem decode_ite {α} [Inhabited α] (x y : α) (v : Bool) :
  decode (if v then x else y) v = decode x v := by cases v <;> simp

-- @[simp]
-- theorem encodeVal_decode {α} [Inhabited α] (x : α) (v : Bool) :
--   encodeVal (decode x v) = if v then x else  := by cases v <;> simp



theorem decode_if {α} [Inhabited α] (x : α) (valid : Bool) : Option.decode (if valid then x else default) valid = Option.decode x valid := by cases valid <;> simp[Option.decode]
