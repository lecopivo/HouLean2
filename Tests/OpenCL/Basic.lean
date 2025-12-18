import HouLean.Data.Vector
import HouLean.OpenCL.Reference
import HouLean.OpenCL.Data.Vector
import HouLean.OpenCL.Data.Matrix
import HouLean.OpenCL.Compiler.Main
import HouLean.OpenCL.WorkItemFunctions

open HouLean OpenCL

namespace Tests.OpenCL.Basic



/--
info: float main(float x, float y){
      const float a = x * y;
      const float b = mix(x, y, 0.3);
      return a + b;
}
-/
#guard_msgs in
#opencl_compile (fun x y : Float32 =>
  let a := x * y
  let b := Math.lerp x y (0.3:Float32)
  a + b)

/--
info: float3 main(float3 x, float3 y){
      const float3 a = x + y;
      const float3 b = mix(x, y, 0.3);
      return a + b;
}
-/
#guard_msgs in
#opencl_compile (fun x y : Vector Float32 3 =>
  let a := x + y
  let b := Math.lerp x y (0.3:Float32)
  a + b)


/--
info: matrix33float houlean_matrix_add_float32_3_3(matrix33float a, matrix33float b){
      return (matrix33float){a.row0 + b.row0, a.row1 + b.row1, a.row2 + b.row2};
}

matrix33float add_add_matrix_float32_3_3(matrix33float a, matrix33float a1){
      return houlean_matrix_add_float32_3_3(a, a1);
}

matrix33float hadd_hadd_matrix_float32_3_3_matrix_float32_3_3_matrix_float32_3_3(matrix33float a, matrix33float a1){
      return add_add_matrix_float32_3_3(a, a1);
}

matrix33float houlean_matrix_sub_float32_3_3(matrix33float a, matrix33float b){
      return (matrix33float){a.row0 - b.row0, a.row1 - b.row1, a.row2 - b.row2};
}

matrix33float sub_sub_matrix_float32_3_3(matrix33float a, matrix33float a1){
      return houlean_matrix_sub_float32_3_3(a, a1);
}

matrix33float hsub_hsub_matrix_float32_3_3_matrix_float32_3_3_matrix_float32_3_3(matrix33float a, matrix33float a1){
      return sub_sub_matrix_float32_3_3(a, a1);
}

matrix33float houlean_matrix_smul_float32_3_3_0_3(matrix33float a){
      return (matrix33float){0.3 * a.row0, 0.3 * a.row1, 0.3 * a.row2};
}

matrix33float hmul_hmul_float32_matrix_float32_3_3_matrix_float32_3_3_0_3(matrix33float a){
      return houlean_matrix_smul_float32_3_3_0_3(a);
}

matrix33float houlean_matrix_lerp_float32_3_3_0_3(matrix33float a, matrix33float b){
      return
        hadd_hadd_matrix_float32_3_3_matrix_float32_3_3_matrix_float32_3_3(a,
          hmul_hmul_float32_matrix_float32_3_3_matrix_float32_3_3_0_3(hsub_hsub_matrix_float32_3_3_matrix_float32_3_3_matrix_float32_3_3(b,
              a)));
}

matrix33float houlean_math_lerp_lerp_matrix_float32_3_3_float32_0_3(matrix33float x, matrix33float y){
      return houlean_matrix_lerp_float32_3_3_0_3(x, y);
}

matrix33float main(matrix33float x, matrix33float y){
      const matrix33float a = hadd_hadd_matrix_float32_3_3_matrix_float32_3_3_matrix_float32_3_3(x, y);
      const matrix33float b = houlean_math_lerp_lerp_matrix_float32_3_3_float32_0_3(x, y);
      return hadd_hadd_matrix_float32_3_3_matrix_float32_3_3_matrix_float32_3_3(a, b);
}
-/
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
info: double tests_opencl_basic_foo1(uint n, double x){
      const double a = x + x;
      return a;
}

double main(uint x, double x1){
      return tests_opencl_basic_foo1(x, x1);
}
-/
#guard_msgs in
#opencl_compile fun x => foo x
