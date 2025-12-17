import HouLean.Meta.FloatLinter

namespace Test.Meta.FloatLinter

set_option linter.floatUsage true

/--
warning: Use of `Float` detected in definition body. Consider `Float32` or `Float64`.

Note: This linter can be disabled with `set_option linter.floatUsage false`
-/
#guard_msgs in
def foo (x : Float32) : Float32 :=
  let a := x.toFloat
  a.toFloat32

/--
error: `Test.Meta.FloatLinter.foo` has already been declared
---
warning: Use of `Float` detected in definition body. Consider `Float32` or `Float64`.

Note: This linter can be disabled with `set_option linter.floatUsage false`
-/
#guard_msgs in
def foo (x : Float) : Float := x
