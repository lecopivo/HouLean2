import HouLean.Meta.OverloadedFunction
import HouLean.Meta.RewriteBy

open HouLean.Meta

namespace Tests.Meta.OverloadedFunction

set_option linter.unusedVariables false

declfun foo {α} (x : α) : α

defun foo (x : Nat) := 10 * x
defun foo (x : Float) := x.cos

/-- info: 10 -/
#guard_msgs in
#eval foo (1 : Nat)

/-- info: 0.540302 -/
#guard_msgs in
#eval foo (1 : Float)


defun Float32.foo (x : Float32) := x.exp


/-- info: Float32.foo 1.0 : Float32 -/
#guard_msgs in
#check (foo (1.0 : Float32))
  rewrite_by
    simp [foo]
