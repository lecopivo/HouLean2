import HouLean.Data.Vector
import HouLean.OpenCL.Data.Vector
import HouLean.OpenCL.Data.Matrix
import HouLean.OpenCL.Compiler.Main
import HouLean.OpenCL.WorkItemFunctions

open HouLean OpenCL

namespace Tests.OpenCL.Basic


#exit
/--
info:
float houlean_math_lerp_ff(float x, float y, float t)
{
    return x + ((y - x) * t);
}

float (anonymous)(float x, float y)
{
    float a = x * y;
    float b = houlean_math_lerp_ff(x, y, 0.300000011920929f);
    return a + b;
}
-/
#guard_msgs in
#opencl_compile (fun x y : Float32 =>
  let a := x * y
  let b := Math.lerp x y (0.3:Float32)
  a + b)

/--
info:
float3 vector_lerp_f3(float3 x, float3 y, float t)
{
    return x + (t * (y - x));
}

float3 houlean_math_lerp_f3f(float3 x, float3 y, float t)
{
    return vector_lerp_f3(x, y, t);
}

float3 (anonymous)(float3 x, float3 y)
{
    float3 a = x + y;
    float3 b = houlean_math_lerp_f3f(x, y, 0.300000011920929f);
    return a + b;
}
-/
#guard_msgs in
#opencl_compile (fun x y : Vector Float32 3 =>
  let a := x + y
  let b := Math.lerp x y (0.3:Float32)
  a + b)


/-- error: Not an OpenCL type Matrix Float32 3 3! -/
#guard_msgs in
#opencl_compile (fun x y : Matrix Float32 3 3 =>
  let a := x + y
  let b := Math.lerp x y (0.3 : Float32)
  a + b)


set_option linter.unusedVariables false in
abbrev Foo (n : Nat) := Float

def foo (n : Nat) (x : Foo n) : Float :=
  let a := x + x
  a

/--
info:
double tests_opencl_basic_foo(uint n, double x)
{
    double a = x + x;
    return a;
}

double (anonymous)(uint n, double x)
{
    return tests_opencl_basic_foo(n, x);
}
-/
#guard_msgs in
#opencl_compile foo
