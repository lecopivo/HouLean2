import HouLean.OpenCL.Data.Matrix
import HouLean.OpenCL.Data.Prod
import HouLean.OpenCL.Data.Init

open HouLean OpenCL Math

open HoudiniMatrixVecMul

namespace Tests.OpenCL.Vector


/--
info:
double vector_dot_d3(double3 u, double3 v)
{
    return ((u.x * v.x) + (u.y * v.y)) + (u.y * v.y);
}

double (anonymous)(double3 x, double3 y)
{
    return vector_dot_d3(x, y);
}
-/
#guard_msgs in
#opencl_compile (fun x y : Vector Float 3 => x.dot y)

-- todo: where is `vector_dot_d3`? ... it seems to be a problem with `defun dot ...` as it does not use
--       `Vector.dot` in the instance
/--
info:
double (anonymous)(double3 x, double3 y)
{
    return ((x.x * y.x) + (x.y * y.y)) + (x.y * y.y);
}
-/
#guard_msgs in
#opencl_compile (fun x y : Vector Float 3 => dot x y)



set_option trace.HouLean.OpenCL.compiler true in
#opencl_compile (fun x : Vector Float 3 => x.normalize)

#check (fun x : Vector Float 3 => x.normalize)


attribute [opencl_csimp] Math.dot

#check Vector.dot

set_option trace.Meta.Tactic.simp true in
#check  (fun x y : Vector Float 3 => dot x y)
  rewrite_by
    unfold dot Vector.instDotOfAddOfZeroOfMul
    simp only []
    simp only []



declfun foo {α} (x : α) : α

defun foo (x : Nat) := 10 * x
defun foo (x : Float) := 10 * x


#check foo (42 : Nat)
#check foo (42 : Float)


instance [Add α] [Mul α] : Foo α where
  foo x := x * x + x

#check foo (42 : Int)

Add α → Mul α → Foo α
