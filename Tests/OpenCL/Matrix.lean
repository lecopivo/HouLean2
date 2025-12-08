import HouLean.OpenCL.Data.Matrix
import HouLean.OpenCL.Data.Init
import HouLean.OpenCL.Data.MProd
import HouLean.Data.LinearAlgebra.LUDecomposition

open HouLean OpenCL Math

open HoudiniMatrixVecMul


/--
info:
float inv_f(float a)
{
    return 1.0f / a;
}

matrix33f houlean_matrix_sdiv_f33(matrix33f a, float s)
{
    float is = inv_f(s);
    return (matrix33f){is * a.row0, is * a.row1, is * a.row2};
}

matrix33f hdiv_f33ff33(matrix33f a, float a1)
{
    return houlean_matrix_sdiv_f33(a, a1);
}

matrix33f (anonymous)(float s, matrix33f A)
{
    return hdiv_f33ff33(A, s);
}
-/
#guard_msgs in
#opencl_compile (fun (s : Float32) (A : Matrix Float32 3 3) => A / s)


/--
info:
float houlean_math_sin_f(float x)
{
    return sin(x);
}

float3 vector_sin_f3(float3 x)
{
    return (float3){houlean_math_sin_f(x.x), houlean_math_sin_f(x.y), houlean_math_sin_f(x.z)};
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
float3 houlean_math_sin_f3(float3 x)
{
    return vector_sin_f3(x);
}

float3 (anonymous)(float3 x)
{
    return houlean_math_sin_f3(x);
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

float3 hmul_f3f33f3(float3 a, matrix33f a1)
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

matrix33f hmul_f33f33f33(matrix33f a, matrix33f a1)
{
    return houlean_matrix_matmul_f333(a, a1);
}

float3 (anonymous)(float3 v, matrix33f A, matrix33f B)
{
    return hmul_f3f33f3(v, hmul_f33f33f33(A, B));
}
-/
#guard_msgs in
#opencl_compile (fun (v : Vector Float32 3) (A B : Matrix Float32 3 3) => v * (A * B))


/--
info:
mprodf44f44 houlean_matrix_lu4_f(matrix44f a)
{
    float u00 = a.row0.x;
    float u01 = a.row0.y;
    float u02 = a.row0.z;
    float u03 = a.row0.w;
    float l10 = a.row1.x / u00;
    float l20 = a.row2.x / u00;
    float l30 = a.row3.x / u00;
    float u11 = a.row1.y - (l10 * u01);
    float u12 = a.row1.z - (l10 * u02);
    float u13 = a.row1.w - (l10 * u03);
    float l21 = (a.row2.y - (l20 * u01)) / u11;
    float l31 = (a.row3.y - (l30 * u01)) / u11;
    float u22 = (a.row2.z - (l20 * u02)) - (l21 * u12);
    float u23 = (a.row2.w - (l20 * u03)) - (l21 * u13);
    float l32 = ((a.row3.z - (l30 * u02)) - (l31 * u12)) / u22;
    float u33 = ((a.row3.w - (l30 * u03)) - (l31 * u13)) - (l32 * u23);
    matrix44f L = (matrix44f){(float4){1.0f, 0.0f, 0.0f, 0.0f}, (float4){l10, 1.0f, 0.0f, 0.0f}, (float4){l20, l21, 1.0f, 0.0f}, (float4){l30, l31, l32, 1.0f}};
    matrix44f U = (matrix44f){(float4){u00, u01, u02, u03}, (float4){0.0f, u11, u12, u13}, (float4){0.0f, 0.0f, u22, u23}, (float4){0.0f, 0.0f, 0.0f, u33}};
    return (mprodf44f44){L, U};
}

mprodf44f44 (anonymous)(matrix44f A)
{
    return houlean_matrix_lu4_f(A);
}
-/
#guard_msgs in
#opencl_compile (fun (A : Matrix Float32 4 4) => A.lu4)
