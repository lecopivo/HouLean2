import HouLean.Meta.OverloadedFunction

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
