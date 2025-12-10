import HouLean.OpenCL.Data.Matrix
import HouLean.OpenCL.Data.Init
import HouLean.OpenCL.Data.MProd
import HouLean.Data.LinearAlgebra.Basic

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
float houlean_math_sin_f(float x)
{
    return sin(x);
}

float3 vector_sin_f3(float3 x)
{
    return (float3){houlean_math_sin_f(x.x), houlean_math_sin_f(x.y), houlean_math_sin_f(x.z)};
}

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

float vector_dot_f3(float3 u, float3 v)
{
    return ((u.x * v.x) + (u.y * v.y)) + (u.z * v.z);
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
float houlean_matrix_det2_f(matrix22f a)
{
    return (a.row0.x * a.row1.y) - (a.row0.y * a.row1.x);
}

float inv_f(float a)
{
    return 1.0f / a;
}

matrix22f houlean_matrix_inv2_f(matrix22f a)
{
    float d = houlean_matrix_det2_f(a);
    float id = inv_f(d);
    return (matrix22f){(float2){a.row1.y * id, ( -a.row0.y) * id}, (float2){( -a.row1.x) * id, a.row0.x * id}};
}

matrix22f (anonymous)(matrix22f A)
{
    return houlean_matrix_inv2_f(A);
}
-/
#guard_msgs in
#opencl_compile (fun (A : Matrix Float32 2 2) => A.inv2)


/--
info:
float houlean_matrix_det3_f(matrix33f a)
{
    return ((a.row0.x * ((a.row1.y * a.row2.z) - (a.row1.z * a.row2.y))) - (a.row0.y * ((a.row1.x * a.row2.z) - (a.row1.z * a.row2.x)))) + (a.row0.z * ((a.row1.x * a.row2.y) - (a.row1.y * a.row2.x)));
}

float inv_f(float a)
{
    return 1.0f / a;
}

matrix33f houlean_matrix_inv3_f(matrix33f a)
{
    float d = houlean_matrix_det3_f(a);
    float id = inv_f(d);
    return (matrix33f){(float3){((a.row1.y * a.row2.z) - (a.row1.z * a.row2.y)) * id, ((a.row0.z * a.row2.y) - (a.row0.y * a.row2.z)) * id, ((a.row0.y * a.row1.z) - (a.row0.z * a.row1.y)) * id}, (float3){((a.row1.z * a.row2.x) - (a.row1.x * a.row2.z)) * id, ((a.row0.x * a.row2.z) - (a.row0.z * a.row2.x)) * id, ((a.row0.z * a.row1.x) - (a.row0.x * a.row1.z)) * id}, (float3){((a.row1.x * a.row2.y) - (a.row1.y * a.row2.x)) * id, ((a.row0.y * a.row2.x) - (a.row0.x * a.row2.y)) * id, ((a.row0.x * a.row1.y) - (a.row0.y * a.row1.x)) * id}};
}

matrix33f (anonymous)(matrix33f A)
{
    return houlean_matrix_inv3_f(A);
}
-/
#guard_msgs in
#opencl_compile (fun (A : Matrix Float32 3 3) => A.inv3)


/--
info:
float inv_f(float a)
{
    return 1.0f / a;
}

matrix44f houlean_matrix_inv4_f(matrix44f a)
{
    float s0 = (a.row0.x * a.row1.y) - (a.row1.x * a.row0.y);
    float s1 = (a.row0.x * a.row1.z) - (a.row1.x * a.row0.z);
    float s2 = (a.row0.x * a.row1.w) - (a.row1.x * a.row0.w);
    float s3 = (a.row0.y * a.row1.z) - (a.row1.y * a.row0.z);
    float s4 = (a.row0.y * a.row1.w) - (a.row1.y * a.row0.w);
    float s5 = (a.row0.z * a.row1.w) - (a.row1.z * a.row0.w);
    float c5 = (a.row2.z * a.row3.w) - (a.row3.z * a.row2.w);
    float c4 = (a.row2.y * a.row3.w) - (a.row3.y * a.row2.w);
    float c3 = (a.row2.y * a.row3.z) - (a.row3.y * a.row2.z);
    float c2 = (a.row2.x * a.row3.w) - (a.row3.x * a.row2.w);
    float c1 = (a.row2.x * a.row3.z) - (a.row3.x * a.row2.z);
    float c0 = (a.row2.x * a.row3.y) - (a.row3.x * a.row2.y);
    float d = (((((s0 * c5) - (s1 * c4)) + (s2 * c3)) + (s3 * c2)) - (s4 * c1)) + (s5 * c0);
    float id = inv_f(d);
    return (matrix44f){(float4){(((a.row1.y * c5) - (a.row1.z * c4)) + (a.row1.w * c3)) * id, (((( -a.row0.y) * c5) + (a.row0.z * c4)) - (a.row0.w * c3)) * id, (((a.row3.y * s5) - (a.row3.z * s4)) + (a.row3.w * s3)) * id, (((( -a.row2.y) * s5) + (a.row2.z * s4)) - (a.row2.w * s3)) * id}, (float4){(((( -a.row1.x) * c5) + (a.row1.z * c2)) - (a.row1.w * c1)) * id, (((a.row0.x * c5) - (a.row0.z * c2)) + (a.row0.w * c1)) * id, (((( -a.row3.x) * s5) + (a.row3.z * s2)) - (a.row3.w * s1)) * id, (((a.row2.x * s5) - (a.row2.z * s2)) + (a.row2.w * s1)) * id}, (float4){(((a.row1.x * c4) - (a.row1.y * c2)) + (a.row1.w * c0)) * id, (((( -a.row0.x) * c4) + (a.row0.y * c2)) - (a.row0.w * c0)) * id, (((a.row3.x * s4) - (a.row3.y * s2)) + (a.row3.w * s0)) * id, (((( -a.row2.x) * s4) + (a.row2.y * s2)) - (a.row2.w * s0)) * id}, (float4){(((( -a.row1.x) * c3) + (a.row1.y * c1)) - (a.row1.z * c0)) * id, (((a.row0.x * c3) - (a.row0.y * c1)) + (a.row0.z * c0)) * id, (((( -a.row3.x) * s3) + (a.row3.y * s1)) - (a.row3.z * s0)) * id, (((a.row2.x * s3) - (a.row2.y * s1)) + (a.row2.z * s0)) * id}};
}

matrix44f (anonymous)(matrix44f A)
{
    return houlean_matrix_inv4_f(A);
}
-/
#guard_msgs in
set_option maxRecDepth 1000 in
#opencl_compile (fun (A : Matrix Float32 4 4) => A.inv4)
