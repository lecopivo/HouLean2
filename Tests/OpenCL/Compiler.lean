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


/-- error: Constructor encountered; should compile structure/enum: (#v[v[0], v[1]], v[2]) -/
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
error: Constructor encountered; should compile structure/enum: (({
      data :=
        #v[#v[(a.row 0 ⋯)[0], (a.row 0 ⋯)[1], (a.row 0 ⋯)[2]], #v[(a.row 1 ⋯)[0], (a.row 1 ⋯)[1], (a.row 1 ⋯)[2]],
          #v[(a.row 2 ⋯)[0], (a.row 2 ⋯)[1], (a.row 2 ⋯)[2]]] },
    #v[(a.row 0 ⋯)[3], (a.row 1 ⋯)[3], (a.row 2 ⋯)[3]]),
  #v[(a.row 3 ⋯)[0], (a.row 3 ⋯)[1], (a.row 3 ⋯)[2]], (a.row 3 ⋯)[3])
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


/-- error: Constructor encountered; should compile structure/enum: (x, x) -/
#guard_msgs in
#opencl_compile (fun x : Float32 => (x,x))


/--
error: Constructor encountered; should compile structure/enum: (({
      data :=
        #v[#v[(a.row 0 ⋯)[0], (a.row 0 ⋯)[1], (a.row 0 ⋯)[2]], #v[(a.row 1 ⋯)[0], (a.row 1 ⋯)[1], (a.row 1 ⋯)[2]],
          #v[(a.row 2 ⋯)[0], (a.row 2 ⋯)[1], (a.row 2 ⋯)[2]]] },
    #v[(a.row 0 ⋯)[3], (a.row 1 ⋯)[3], (a.row 2 ⋯)[3]]),
  #v[(a.row 3 ⋯)[0], (a.row 3 ⋯)[1], (a.row 3 ⋯)[2]], (a.row 3 ⋯)[3])
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



/-- error: Don't know how to compile: u.normalize.1 -/
#guard_msgs in
#opencl_compile (fun (x y : Vector Float32 3) (w : Float32) => Math.slerp x y w)
