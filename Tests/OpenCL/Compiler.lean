import HouLean.OpenCL.Compiler


import HouLean.OpenCL.Data.Fin
import HouLean.OpenCL.Data.Vector
import HouLean.OpenCL.Data.Matrix
import HouLean.OpenCL.Reference

import HouLean.Data.RigidScaleTransform

namespace Test.OpenCL.Compiler

open HouLean Math


/--
info: float vector_dot_float32_3(float3 u, float3 v){
      const float a = u.x * v.x;
      const float a1 = a + u.y * v.y;
      const float a2 = a1 + u.z * v.z;
      return a2;
}

float main(float3 x, float3 y){
      return vector_dot_float32_3(x, y);
}
-/
#guard_msgs in
#opencl_compile (fun x y : Vector Float32 3 => x.dot y)

/--
info: float main(float3 x){
      return x.x;
}
-/
#guard_msgs in
#opencl_compile (fun x : Vector Float32 3 => x[0])

/--
info: float3 houlean_matrix_col_float32_3_3_0(matrix33float a){
      return (float3){a.row0.x, a.row1.x, a.row2.x};
}

float vector_dot_float32_3(float3 u, float3 v){
      const float a = u.x * v.x;
      const float a1 = a + u.y * v.y;
      const float a2 = a1 + u.z * v.z;
      return a2;
}

float3 houlean_matrix_col_float32_3_3_1(matrix33float a){
      return (float3){a.row0.y, a.row1.y, a.row2.y};
}

float3 houlean_matrix_col_float32_3_3_2(matrix33float a){
      return (float3){a.row0.z, a.row1.z, a.row2.z};
}

matrix33float houlean_matrix_matmul_float32_3_3_3(matrix33float a, matrix33float b){
      return
        (matrix33float){(float3){vector_dot_float32_3(a.row0, houlean_matrix_col_float32_3_3_0(b)),
              vector_dot_float32_3(a.row0, houlean_matrix_col_float32_3_3_1(b)),
              vector_dot_float32_3(a.row0, houlean_matrix_col_float32_3_3_2(b))},
          (float3){vector_dot_float32_3(a.row1, houlean_matrix_col_float32_3_3_0(b)),
              vector_dot_float32_3(a.row1, houlean_matrix_col_float32_3_3_1(b)),
              vector_dot_float32_3(a.row1, houlean_matrix_col_float32_3_3_2(b))},
          (float3){vector_dot_float32_3(a.row2, houlean_matrix_col_float32_3_3_0(b)),
              vector_dot_float32_3(a.row2, houlean_matrix_col_float32_3_3_1(b)),
              vector_dot_float32_3(a.row2, houlean_matrix_col_float32_3_3_2(b))}};
}

matrix33float hmul_hmul_matrix_float32_3_3_matrix_float32_3_3_matrix_float32_3_3(matrix33float a, matrix33float a1){
      return houlean_matrix_matmul_float32_3_3_3(a, a1);
}

matrix33float main(matrix33float x, matrix33float y){
      return hmul_hmul_matrix_float32_3_3_matrix_float32_3_3_matrix_float32_3_3(x, y);
}
-/
#guard_msgs in
#opencl_compile (fun x y : Matrix Float32 3 3 => x * y)


/--
info: float main(float x, float y){
      return x + y;
}
-/
#guard_msgs in
#opencl_compile (fun x y : Float32 => x + y)


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

matrix33float main(matrix33float x, matrix33float y){
      return hadd_hadd_matrix_float32_3_3_matrix_float32_3_3_matrix_float32_3_3(x, y);
}
-/
#guard_msgs in
#opencl_compile (fun x y : Matrix Float32 3 3 => x + y)


/--
info: Prod_float2_float vector_split1_2_float32(float3 v){
      return (Prod_float2_float){(float2){v.x, v.y}, v.z};
}

Prod_float2_float main(float3 x){
      const Prod_float2_float a = vector_split1_2_float32(x);
      return a;
}
-/
#guard_msgs in
#opencl_compile (fun (x : Vector Float32 3) =>
  let a := x.split1
  a)


/--
info: Prod_matrix33float_matrix33float main(matrix44float A, float3 x){
      const matrix33float a =
      (matrix33float){(float3){A.row0.x, A.row0.y, A.row0.z}, (float3){A.row1.x, A.row1.y, A.row1.z},
          (float3){A.row2.x, A.row2.y, A.row2.z}};
      const matrix33float b =
      (matrix33float){(float3){A.row1.y, A.row1.z, A.row1.w}, (float3){A.row2.y, A.row2.z, A.row2.w},
          (float3){A.row3.y, A.row3.z, A.row3.w}};
      const Prod_matrix33float_matrix33float c = (Prod_matrix33float_matrix33float){a, b};
      return c;
}
-/
#guard_msgs in
#opencl_compile (fun (A : Matrix Float32 4 4) (x : Vector Float32 3) =>
  let a := Matrix.ofFn (m:=3) (n:=3) fun i j _ => A[i,j]
  let b := Matrix.ofFn (m:=3) (n:=3) fun i j _ => A[i+1,j+1]
  let c := (a, b)
  c)


/--
info: Prod_Prod_matrix33float_float3_Prod_float3_float houlean_matrix_split1_3_float32(matrix44float a){
      return
        (Prod_Prod_matrix33float_float3_Prod_float3_float){(Prod_matrix33float_float3){(matrix33float){(float3){a.row0.x,
                      a.row0.y, a.row0.z},
                  (float3){a.row1.x, a.row1.y, a.row1.z}, (float3){a.row2.x, a.row2.y, a.row2.z}},
              (float3){a.row0.w, a.row1.w, a.row2.w}},
          (Prod_float3_float){(float3){a.row3.x, a.row3.y, a.row3.z}, a.row3.w}};
}

Prod_Prod_matrix33float_float3_Prod_float3_float main(matrix44float A){
      return houlean_matrix_split1_3_float32(A);
}
-/
#guard_msgs in
#opencl_compile (fun (A : Matrix Float32 4 4) => A.split1)


/--
info: float main(Prod_float_float x){
      const Prod_float_float x1 = x;
      return x1.fst;
}
-/
#guard_msgs in
#opencl_compile (fun (x : Float32 × Float32) =>
  let x := x
  x.1)


/--
info: Prod_float_float main(float x){
      const Prod_float_Prod_float_float a = (Prod_float_Prod_float_float){x, (Prod_float_float){x, x}};
      return (Prod_float_float){a.fst + a.snd.snd, a.snd.fst};
}
-/
#guard_msgs in
#opencl_compile (fun x : Float32 =>
  let a := (x,x,x)
  (a.1 + a.2.2, a.2.1))


/--
info: Prod_float_float main(float x){
      return (Prod_float_float){x, x};
}
-/
#guard_msgs in
#opencl_compile (fun x : Float32 => (x,x))


/--
info: Prod_Prod_matrix33float_float3_Prod_float3_float houlean_matrix_split1_3_float32(matrix44float a){
      return
        (Prod_Prod_matrix33float_float3_Prod_float3_float){(Prod_matrix33float_float3){(matrix33float){(float3){a.row0.x,
                      a.row0.y, a.row0.z},
                  (float3){a.row1.x, a.row1.y, a.row1.z}, (float3){a.row2.x, a.row2.y, a.row2.z}},
              (float3){a.row0.w, a.row1.w, a.row2.w}},
          (Prod_float3_float){(float3){a.row3.x, a.row3.y, a.row3.z}, a.row3.w}};
}

float vector_dot_float32_3(float3 u, float3 v){
      const float a = u.x * v.x;
      const float a1 = a + u.y * v.y;
      const float a2 = a1 + u.z * v.z;
      return a2;
}

float3 houlean_matrix_mulvec_float32_3_3(matrix33float a, float3 v){
      return
        (float3){vector_dot_float32_3(a.row0, v), vector_dot_float32_3(a.row1, v), vector_dot_float32_3(a.row2, v)};
}

float3 hmul_hmul_matrix_float32_3_3_vector_float32_3_vector_float32_3(matrix33float a, float3 a1){
      return houlean_matrix_mulvec_float32_3_3(a, a1);
}

float inv_inv_float32(float a){
      return 1.0 / a;
}

float3 hdiv_hdiv_vector_float32_3_float32_vector_float32_3(float3 a, float a1){
      const float is = inv_inv_float32(a1);
      return is * a;
}

float3 houlean_matrix_transformpointright_float32_3(matrix44float transform, float3 point){
      const Prod_Prod_matrix33float_float3_Prod_float3_float tmp = houlean_matrix_split1_3_float32(transform);
      const float w = vector_dot_float32_3(point, tmp.snd.fst) + tmp.snd.snd;
      return
        hdiv_hdiv_vector_float32_3_float32_vector_float32_3(hmul_hmul_matrix_float32_3_3_vector_float32_3_vector_float32_3(tmp.fst.fst,
              point) +
            tmp.fst.snd,
          w);
}

float3 main(matrix44float A, float3 x){
      return houlean_matrix_transformpointright_float32_3(A, x);
}
-/
#guard_msgs in
#opencl_compile (fun (A : Matrix Float32 4 4) (x : Vector Float32 3) => A.transformPointRight x)



/--
info: HouLean_Vector3 houlean_vector3_cross1(HouLean_Vector3 a, HouLean_Vector3 b){
      return (HouLean_Vector3){a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x};
}

HouLean_Vector3 houlean_vector3_smul1(HouLean_Vector3 a, double s){
      return (HouLean_Vector3){s * a.x, s * a.y, s * a.z};
}

HouLean_Vector3 hmul_hmul_float_vector3_vector3(double a, HouLean_Vector3 a1){
      return houlean_vector3_smul1(a1, a);
}

HouLean_Vector3 houlean_vector3_smul_2(HouLean_Vector3 a){
      return (HouLean_Vector3){2.0 * a.x, 2.0 * a.y, 2.0 * a.z};
}

HouLean_Vector3 hmul_hmul_float_vector3_vector3_2(HouLean_Vector3 a){
      return houlean_vector3_smul_2(a);
}

HouLean_Vector3 houlean_vector4_quatrotate1(HouLean_Vector4 q, HouLean_Vector3 v){
      const HouLean_Vector3 qv = (HouLean_Vector3){q.x, q.y, q.z};
      const HouLean_Vector3 uv = houlean_vector3_cross1(qv, v);
      const HouLean_Vector3 uuv = houlean_vector3_cross1(qv, uv);
      const HouLean_Vector3 uv_scaled = hmul_hmul_float_vector3_vector3(2.0 * q.w, uv);
      const HouLean_Vector3 uuv_scaled = hmul_hmul_float_vector3_vector3_2(uuv);
      return
        (HouLean_Vector3){v.x + uv_scaled.x + uuv_scaled.x, v.y + uv_scaled.y + uuv_scaled.y,
          v.z + uv_scaled.z + uuv_scaled.z};
}

HouLean_Vector3 houlean_vector3_add1(HouLean_Vector3 a, HouLean_Vector3 b){
      return (HouLean_Vector3){a.x + b.x, a.y + b.y, a.z + b.z};
}

HouLean_Vector3 add_add_vector3(HouLean_Vector3 a, HouLean_Vector3 a1){
      return houlean_vector3_add1(a, a1);
}

HouLean_Vector3 hadd_hadd_vector3_vector3_vector3(HouLean_Vector3 a, HouLean_Vector3 a1){
      return add_add_vector3(a, a1);
}

HouLean_Vector3 houlean_rigidtransform_transformpoint1(HouLean_RigidTransform xform, HouLean_Vector3 p){
      const HouLean_Vector3 rotated = houlean_vector4_quatrotate1(xform.orient, p);
      return hadd_hadd_vector3_vector3_vector3(rotated, xform.translate);
}

HouLean_Vector3 main(HouLean_RigidTransform t, HouLean_Vector3 x){
      return houlean_rigidtransform_transformpoint1(t, x);
}
-/
#guard_msgs in
#opencl_compile (fun (t : RigidTransform) (x : Vector3) =>
  t.transformPoint x)


def foo := (fun x y : Matrix Float32 3 3 => x + y)

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

matrix33float test_opencl_compiler_foo1(matrix33float x, matrix33float y){
      return hadd_hadd_matrix_float32_3_3_matrix_float32_3_3_matrix_float32_3_3(x, y);
}

matrix33float main(matrix33float x, matrix33float y){
      return test_opencl_compiler_foo1(x, y);
}
-/
#guard_msgs in
#opencl_compile (fun x y => foo x y)


/--
info: double main(double x){
      return acos(x);
}
-/
#guard_msgs in
#opencl_compile (fun x : Float => Math.acos x)


/--
info: def Vector.length2_Float32_3 := ⏎
fun u =>
  let a := u[0] * u[0];
  let a := a + u[1] * u[1];
  let a := a + u[2] * u[2];
  a

def Vector.length_Float32_3 := ⏎
fun u => sqrt u.length2_Float32_3

def HouLean.Math.ApproxEqual.approxEqual_Float32_Float32_0_1e_6 := ⏎
fun x => decide (1e-6 ≥ abs (x - 0))

def Inv.inv_Float32 := ⏎
fun a => 1 / a

def HDiv.hDiv_Vector_Float32_3_Float32_Vector_Float32_3 := ⏎
fun a a_1 =>
  let is := Inv.inv_Float32 a_1;
  is * a

def Vector.normalize_Float32_3 := ⏎
fun u =>
  let len := u.length_Float32_3;
  if ApproxEqual.approxEqual_Float32_Float32_0_1e_6 len = true then (u, 0)
  else (HDiv.hDiv_Vector_Float32_3_Float32_Vector_Float32_3 u len, len)

def Vector.normalized_Float32_3 := ⏎
fun u => u.normalize_Float32_3.1

def Vector.dot_Float32_3 := ⏎
fun u v =>
  let a := u[0] * v[0];
  let a := a + u[1] * v[1];
  let a := a + u[2] * v[2];
  a

def Vector.slerp_Float32_3 := ⏎
fun v w t =>
  let d := v.normalized_Float32_3.dot_Float32_3 w.normalized_Float32_3;
  let d := clamp d (-1) 1;
  let theta := acos d;
  let s := sin theta;
  let a := sin ((1 - t) * theta) / s;
  let b := sin (t * theta) / s;
  if ApproxEqual.approxEqual_Float32_Float32_0_1e_6 theta = true then lerp v w t else a * v + b * w

def HouLean.Math.Slerp.slerp_Vector_Float32_3_Float32 := ⏎
fun x y t => x.slerp_Float32_3 y t

Resulting specialization:
  fun x y w => Slerp.slerp_Vector_Float32_3_Float32 x y w
-/
#guard_msgs in
#opencl_sas (fun (x y : Vector Float32 3) (w : Float32) => Math.slerp x y w)
