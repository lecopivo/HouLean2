import HouLean.OpenCL.Data.Matrix
import HouLean.OpenCL.Data.Init

open HouLean OpenCL Math

open HoudiniMatrixVecMul


/--
info:
float3 hdiv_hdiv_f3ff3(float3 a, float a1)
{
    return (float3){a.x / a1, a.y / a1, a.z / a1};
}

matrix33f houlean_matrix_sdiv_f33(matrix33f a, float s)
{
    return (matrix33f){hdiv_hdiv_f3ff3(a.row0, s), hdiv_hdiv_f3ff3(a.row1, s), hdiv_hdiv_f3ff3(a.row2, s)};
}

matrix33f hdiv_hdiv_f33ff33(matrix33f a, float a1)
{
    return houlean_matrix_sdiv_f33(a, a1);
}

matrix33f (anonymous)(float s, matrix33f A)
{
    return hdiv_hdiv_f33ff33(A, s);
}
-/
#guard_msgs in
#opencl_compile (fun (s : Float32) (A : Matrix Float32 3 3) => A / s)


/--
info:
float houlean_math_sin_sin_f(float x)
{
    return sin(x);
}

float3 vector_sin_f3(float3 x)
{
    return (float3){houlean_math_sin_sin_f(x.x), houlean_math_sin_sin_f(x.y), houlean_math_sin_sin_f(x.z)};
}

float vector_dot_f3(float3 u, float3 v)
{
    return ((u.x * v.x) + (u.y * v.y)) + (u.z * v.z);
}

float3 (anonymous)(float3 v)
{
    float3 a = v + vector_sin_f3(v);
    float b = vector_dot_f3(v, v);
    return b * a;
}
-/
#guard_msgs in
#opencl_compile (fun (v : Vector Float32 3) =>
  let a := v + v.sin
  let b := v.dot v
  b * a)

/--
info:
float3 houlean_math_sin_sin_f3(float3 x)
{
    return (float3){houlean_math_sin_sin_f(x.x), houlean_math_sin_sin_f(x.y), houlean_math_sin_sin_f(x.z)};
}

float3 (anonymous)(float3 x)
{
    return houlean_math_sin_sin_f3(x);
}
-/
#guard_msgs in
#opencl_compile Math.sin (α:=Vector Float32 3)

/--
info:
float3 vector_zero_f3()
{
    return (float3){0.0f, 0.0f, 0.0f};
}

float3 houlean_matrix_vecmul_sum_rows_f33(float3 v, matrix33f a)
{
    return ((vector_zero_f3() + (v.x * a.row0)) + (v.y * a.row1)) + (v.z * a.row2);
}

float3 hmul_hmul_f3f33f3(float3 a, matrix33f a1)
{
    return houlean_matrix_vecmul_sum_rows_f33(a, a1);
}

float3 houlean_matrix_coli_f330(matrix33f a)
{
    return (float3){a.row0.x, a.row1.x, a.row2.x};
}

float3 houlean_matrix_coli_f331(matrix33f a)
{
    return (float3){a.row0.y, a.row1.y, a.row2.y};
}

float3 houlean_matrix_coli_f332(matrix33f a)
{
    return (float3){a.row0.z, a.row1.z, a.row2.z};
}

matrix33f houlean_matrix_matmul_f333(matrix33f a, matrix33f b)
{
    return (matrix33f){(float3){vector_dot_f3(a.row0, houlean_matrix_coli_f330(b)), vector_dot_f3(a.row0, houlean_matrix_coli_f331(b)), vector_dot_f3(a.row0, houlean_matrix_coli_f332(b))}, (float3){vector_dot_f3(a.row1, houlean_matrix_coli_f330(b)), vector_dot_f3(a.row1, houlean_matrix_coli_f331(b)), vector_dot_f3(a.row1, houlean_matrix_coli_f332(b))}, (float3){vector_dot_f3(a.row2, houlean_matrix_coli_f330(b)), vector_dot_f3(a.row2, houlean_matrix_coli_f331(b)), vector_dot_f3(a.row2, houlean_matrix_coli_f332(b))}};
}

matrix33f hmul_hmul_f33f33f33(matrix33f a, matrix33f a1)
{
    return houlean_matrix_matmul_f333(a, a1);
}

float3 (anonymous)(float3 v, matrix33f A, matrix33f B)
{
    return hmul_hmul_f3f33f3(v, hmul_hmul_f33f33f33(A, B));
}
-/
#guard_msgs in
#opencl_compile (fun (v : Vector Float32 3) (A B : Matrix Float32 3 3) => v * (A * B))
