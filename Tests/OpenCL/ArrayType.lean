import HouLean.OpenCL.Data.ArrayType
import HouLean.OpenCL.Data.Matrix
import HouLean.Meta.DoNotation


open HouLean OpenCL
/--
info:
float3 vector_zero_f3()
{
    return (float3){0.0f, 0.0f, 0.0f};
}

float3 houlean_opencl_arraytype_get_f3pf(float * a, ulong a1)
{
    return vload3(a1, a);
}

float3 getelem_pfulf3(float * xs, ulong i)
{
    return houlean_opencl_arraytype_get_f3pf(xs, i);
}

float3 (anonymous)(float * a)
{
    float3 b = vector_zero_f3();
    float3 state = b;
    for (uint i = 0; i < 10; i += 1)
    {
        float3 ai = getelem_pfulf3(a, (ulong)(i));
        float3 b1 = state + ai;
        state = b1;
    }
    float3 r = state;
    float3 this = r;
    return this;
}
-/
#guard_msgs in
#opencl_compile fun (a : ArrayPointer (Vector Float32 3)) => show OpenCLM _ from do
  let mut b : Vector Float32 3 := 0
  for i in [0:10] do
    let ai ← a[i.toUInt64]
    b := b + ai
  return b

/--
info:
matrix33f houlean_matrix_zeron_f33()
{
    return (matrix33f){vector_zero_f3(), vector_zero_f3(), vector_zero_f3()};
}

void houlean_opencl_arraytype_set_f3pf(float * a, ulong a1, float3 a2)
{
    return vstore3(a2, a1, a);
}

void houlean_opencl_matrix_vstore_f33(float * ptr, ulong off, matrix33f value)
{
    houlean_opencl_arraytype_set_f3pf(ptr, (ulong)(((uint)(off) * (uint)(3)) + (uint)(0)), value.row0);
    houlean_opencl_arraytype_set_f3pf(ptr, (ulong)(((uint)(off) * (uint)(3)) + (uint)(1)), value.row1);
    return houlean_opencl_arraytype_set_f3pf(ptr, (ulong)(((uint)(off) * (uint)(3)) + (uint)(2)), value.row2);
}

void houlean_opencl_arraytype_set_f33pf(float * a, ulong a1, matrix33f a2)
{
    return houlean_opencl_matrix_vstore_f33(a, a1, a2);
}

matrix33f (anonymous)(float * a)
{
    matrix33f b = houlean_matrix_zeron_f33();
    for (uint i = 0; i < 10; i += 1)
    {
        houlean_opencl_arraytype_set_f33pf(a, (ulong)(i), b);
    }
    ;
    matrix33f this = b;
    return this;
}
-/
#guard_msgs in
#opencl_compile fun (a : ArrayPointer (Matrix Float32 3 3)) => show OpenCLM _ from do
  let mut b : Matrix Float32 3 3 := 0
  for i in [0:10] do
    ArrayType.set a i.toUInt64 b
  return b




-- #opencl_compile fun (a : ArrayPointer (Matrix Float32 3 3)) => show OpenCLM _ from do
--   let mut b : Matrix Float32 3 3 := 0
--   for i in [0:10] do
--     let ai ← a[i.toUInt64]
--     b := b + ai
--   return b
