import HouLean.Data.Vector
import HouLean.OpenCL.Data.Vector
import HouLean.OpenCL.Data.Matrix
import HouLean.OpenCL.Compiler.Main
import HouLean.OpenCL.WorkItemFunctions

open HouLean OpenCL

namespace Tests.OpenCL.Basic

/--
info:
double houlean_math_lerp_dd(double x, double y, double t)
{
    return x + ((y - x) * t);
}

double (anonymous)(double x, double y)
{
    double a = x * y;
    double b = houlean_math_lerp_dd(x, y, 0.3d);
    return a + b;
}
-/
#guard_msgs in
#opencl_compile (fun x y : Float =>
  let a := x * y
  let b := Math.lerp x y 0.3
  a + b)

/--
info:
double3 vector_lerp_d3(double3 x, double3 y, double t)
{
    return x + (t * (y - x));
}

double3 houlean_math_lerp_d3d(double3 x, double3 y, double t)
{
    return vector_lerp_d3(x, y, t);
}

double3 (anonymous)(double3 x, double3 y)
{
    double3 a = x + y;
    double3 b = houlean_math_lerp_d3d(x, y, 0.3d);
    return a + b;
}
-/
#guard_msgs in
#opencl_compile (fun x y : Vector Float 3 =>
  let a := x + y
  let b := Math.lerp x y 0.3
  a + b)


/--
info:
matrix33f houlean_matrix_add_f33(matrix33f a, matrix33f b)
{
    return (matrix33f){a.row0 + b.row0, a.row1 + b.row1, a.row2 + b.row2};
}

matrix33f add_f33(matrix33f a, matrix33f a1)
{
    return houlean_matrix_add_f33(a, a1);
}

matrix33f hadd_f33f33f33(matrix33f a, matrix33f a1)
{
    return add_f33(a, a1);
}

matrix33f houlean_matrix_smul_f33(float s, matrix33f a)
{
    return (matrix33f){s * a.row0, s * a.row1, s * a.row2};
}

matrix33f hmul_ff33f33(float a, matrix33f a1)
{
    return houlean_matrix_smul_f33(a, a1);
}

matrix33f houlean_matrix_sub_f33(matrix33f a, matrix33f b)
{
    return (matrix33f){a.row0 - b.row0, a.row1 - b.row1, a.row2 - b.row2};
}

matrix33f sub_f33(matrix33f a, matrix33f a1)
{
    return houlean_matrix_sub_f33(a, a1);
}

matrix33f hsub_f33f33f33(matrix33f a, matrix33f a1)
{
    return sub_f33(a, a1);
}

matrix33f houlean_matrix_lerp_f33(matrix33f a, matrix33f b, float t)
{
    return hadd_f33f33f33(a, hmul_ff33f33(t, hsub_f33f33f33(b, a)));
}

matrix33f houlean_math_lerp_f33f(matrix33f x, matrix33f y, float t)
{
    return houlean_matrix_lerp_f33(x, y, t);
}

matrix33f (anonymous)(matrix33f x, matrix33f y)
{
    matrix33f a = hadd_f33f33f33(x, y);
    matrix33f b = houlean_math_lerp_f33f(x, y, 0.300000011920929f);
    return hadd_f33f33f33(a, b);
}
-/
#guard_msgs in
#opencl_compile (fun x y : Matrix Float32 3 3 =>
  let a := x + y
  let b := Math.lerp x y (0.3 : Float32)
  a + b)
