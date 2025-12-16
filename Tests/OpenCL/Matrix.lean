import HouLean.OpenCL.Data.Matrix
import HouLean.OpenCL.Data.Init
-- import HouLean.OpenCL.Data.MProd
import HouLean.Data.LinearAlgebra.Basic

open HouLean OpenCL Math

open HoudiniMatrixVecMul


variable (A B : Matrix Float32 3 3) (s : Float32) (u v : Vector Float32 3)
  (A2 B2 : Matrix Float32 2 2) (A4 B4 : Matrix Float32 4 4)


/--
info: Resulting specialization:
  (A.row 0 ⋯)[1]
-/
#guard_msgs in
#opencl_sas A[0,1]


/--
info: HouLean.Matrix.col_Float32_3_3_1:
fun a => #v[(a.row 0 ⋯)[1], (a.row 1 ⋯)[1], (a.row 2 ⋯)[1]]

Resulting specialization:
  A.col_Float32_3_3_1
-/
#guard_msgs in
#opencl_sas A.col 1


/--
info: Inv.inv_Float32:
fun a => 1e0 / a

HouLean.Matrix.fromRows_3_Float32_3:
fun f => { data := #v[f 0 ⋯, f 1 ⋯, f 2 ⋯] }

HouLean.Matrix.mapRows_3_Float32_3_Float32_3:
fun f a => Matrix.fromRows_3_Float32_3 fun i h => f (a.row i h)

HouLean.Matrix.sdiv_Float32_3_3:
fun a s =>
  let is := Inv.inv_Float32 s;
  Matrix.mapRows_3_Float32_3_Float32_3 (fun x => is * x) a

HDiv.hDiv_Matrix_Float32_3_3_Float32_Matrix_Float32_3_3:
fun a a_1 => a.sdiv_Float32_3_3 a_1

Resulting specialization:
  HDiv.hDiv_Matrix_Float32_3_3_Float32_Matrix_Float32_3_3 A s
-/
#guard_msgs in
#opencl_sas (A / s)


/--
info: Vector.sin_Float32_3:
fun x => #v[sin x[0], sin x[1], sin x[2]]

Vector.dot_Float32_3:
fun u v =>
  let a := u[0] * v[0];
  let a := a + u[1] * v[1];
  let a := a + u[2] * v[2];
  a

Resulting specialization:
  let a := v + v.sin_Float32_3;
  let b := v.dot_Float32_3 v;
  b * a
-/
#guard_msgs in
#opencl_sas (
  let a := v + v.sin
  let b := v.dot v
  b * a)


/--
info: Vector.sin_Float32_3:
fun x => #v[sin x[0], sin x[1], sin x[2]]

HouLean.Math.Sin.sin_Vector_Float32_3:
fun x => x.sin_Float32_3

Resulting specialization:
  Sin.sin_Vector_Float32_3 v
-/
#guard_msgs in
#opencl_sas Math.sin v


/--
info: HouLean.Matrix.ofFn_3_3_Float32:
fun f => { data := #v[#v[f 0 0 ⋯, f 0 1 ⋯, f 0 2 ⋯], #v[f 1 0 ⋯, f 1 1 ⋯, f 1 2 ⋯], #v[f 2 0 ⋯, f 2 1 ⋯, f 2 2 ⋯]] }

HouLean.Matrix.matMul_Float32_3_3_3:
fun a b => Matrix.ofFn_3_3_Float32 fun i j x => (a.row i ⋯).dot (b.col j ⋯)

HMul.hMul_Matrix_Float32_3_3_Matrix_Float32_3_3_Matrix_Float32_3_3:
fun a a_1 => a.matMul_Float32_3_3_3 a_1

HouLean.Matrix.col_Float32_3_3_0:
fun a => #v[(a.row 0 ⋯)[0], (a.row 1 ⋯)[0], (a.row 2 ⋯)[0]]

Vector.dot_Float32_3:
fun u v =>
  let a := u[0] * v[0];
  let a := a + u[1] * v[1];
  let a := a + u[2] * v[2];
  a

HouLean.Matrix.col_Float32_3_3_1:
fun a => #v[(a.row 0 ⋯)[1], (a.row 1 ⋯)[1], (a.row 2 ⋯)[1]]

HouLean.Matrix.col_Float32_3_3_2:
fun a => #v[(a.row 0 ⋯)[2], (a.row 1 ⋯)[2], (a.row 2 ⋯)[2]]

HouLean.Matrix.vecMul_Float32_3_3:
fun v a =>
  #v[v.dot_Float32_3 a.col_Float32_3_3_0, v.dot_Float32_3 a.col_Float32_3_3_1, v.dot_Float32_3 a.col_Float32_3_3_2]

HMul.hMul_Vector_Float32_3_Matrix_Float32_3_3_Vector_Float32_3:
fun a a_1 => Matrix.vecMul_Float32_3_3 a a_1

Resulting specialization:
  HMul.hMul_Vector_Float32_3_Matrix_Float32_3_3_Vector_Float32_3 v
    (HMul.hMul_Matrix_Float32_3_3_Matrix_Float32_3_3_Matrix_Float32_3_3 A B)
-/
#guard_msgs in
#opencl_sas (v * (A * B))


/--
info: HouLean.Matrix.det2_Float32:
fun a => (a.row 0 ⋯)[0] * (a.row 1 ⋯)[1] - (a.row 0 ⋯)[1] * (a.row 1 ⋯)[0]

Inv.inv_Float32:
fun a => 1e0 / a

HouLean.Matrix.inv2_Float32:
fun a =>
  let d := a.det2_Float32;
  let id := Inv.inv_Float32 d;
  { data := #v[#v[(a.row 1 ⋯)[1] * id, -(a.row 0 ⋯)[1] * id], #v[-(a.row 1 ⋯)[0] * id, (a.row 0 ⋯)[0] * id]] }

Resulting specialization:
  A2.inv2_Float32
-/
#guard_msgs in
#opencl_sas (A2.inv2)


/--
info: HouLean.Matrix.det3_Float32:
fun a =>
  (a.row 0 ⋯)[0] * ((a.row 1 ⋯)[1] * (a.row 2 ⋯)[2] - (a.row 1 ⋯)[2] * (a.row 2 ⋯)[1]) -
      (a.row 0 ⋯)[1] * ((a.row 1 ⋯)[0] * (a.row 2 ⋯)[2] - (a.row 1 ⋯)[2] * (a.row 2 ⋯)[0]) +
    (a.row 0 ⋯)[2] * ((a.row 1 ⋯)[0] * (a.row 2 ⋯)[1] - (a.row 1 ⋯)[1] * (a.row 2 ⋯)[0])

Inv.inv_Float32:
fun a => 1e0 / a

HouLean.Matrix.inv3_Float32:
fun a =>
  let d := a.det3_Float32;
  let id := Inv.inv_Float32 d;
  {
    data :=
      #v[#v[((a.row 1 ⋯)[1] * (a.row 2 ⋯)[2] - (a.row 1 ⋯)[2] * (a.row 2 ⋯)[1]) * id,
          ((a.row 0 ⋯)[2] * (a.row 2 ⋯)[1] - (a.row 0 ⋯)[1] * (a.row 2 ⋯)[2]) * id,
          ((a.row 0 ⋯)[1] * (a.row 1 ⋯)[2] - (a.row 0 ⋯)[2] * (a.row 1 ⋯)[1]) * id],
        #v[((a.row 1 ⋯)[2] * (a.row 2 ⋯)[0] - (a.row 1 ⋯)[0] * (a.row 2 ⋯)[2]) * id,
          ((a.row 0 ⋯)[0] * (a.row 2 ⋯)[2] - (a.row 0 ⋯)[2] * (a.row 2 ⋯)[0]) * id,
          ((a.row 0 ⋯)[2] * (a.row 1 ⋯)[0] - (a.row 0 ⋯)[0] * (a.row 1 ⋯)[2]) * id],
        #v[((a.row 1 ⋯)[0] * (a.row 2 ⋯)[1] - (a.row 1 ⋯)[1] * (a.row 2 ⋯)[0]) * id,
          ((a.row 0 ⋯)[1] * (a.row 2 ⋯)[0] - (a.row 0 ⋯)[0] * (a.row 2 ⋯)[1]) * id,
          ((a.row 0 ⋯)[0] * (a.row 1 ⋯)[1] - (a.row 0 ⋯)[1] * (a.row 1 ⋯)[0]) * id]] }

Resulting specialization:
  A.inv3_Float32
-/
#guard_msgs in
#opencl_sas (A.inv3)


/--
info: Inv.inv_Float32:
fun a => 1e0 / a

HouLean.Matrix.inv4_Float32:
fun a =>
  let s0 := (a.row 0 ⋯)[0] * (a.row 1 ⋯)[1] - (a.row 1 ⋯)[0] * (a.row 0 ⋯)[1];
  let s1 := (a.row 0 ⋯)[0] * (a.row 1 ⋯)[2] - (a.row 1 ⋯)[0] * (a.row 0 ⋯)[2];
  let s2 := (a.row 0 ⋯)[0] * (a.row 1 ⋯)[3] - (a.row 1 ⋯)[0] * (a.row 0 ⋯)[3];
  let s3 := (a.row 0 ⋯)[1] * (a.row 1 ⋯)[2] - (a.row 1 ⋯)[1] * (a.row 0 ⋯)[2];
  let s4 := (a.row 0 ⋯)[1] * (a.row 1 ⋯)[3] - (a.row 1 ⋯)[1] * (a.row 0 ⋯)[3];
  let s5 := (a.row 0 ⋯)[2] * (a.row 1 ⋯)[3] - (a.row 1 ⋯)[2] * (a.row 0 ⋯)[3];
  let c5 := (a.row 2 ⋯)[2] * (a.row 3 ⋯)[3] - (a.row 3 ⋯)[2] * (a.row 2 ⋯)[3];
  let c4 := (a.row 2 ⋯)[1] * (a.row 3 ⋯)[3] - (a.row 3 ⋯)[1] * (a.row 2 ⋯)[3];
  let c3 := (a.row 2 ⋯)[1] * (a.row 3 ⋯)[2] - (a.row 3 ⋯)[1] * (a.row 2 ⋯)[2];
  let c2 := (a.row 2 ⋯)[0] * (a.row 3 ⋯)[3] - (a.row 3 ⋯)[0] * (a.row 2 ⋯)[3];
  let c1 := (a.row 2 ⋯)[0] * (a.row 3 ⋯)[2] - (a.row 3 ⋯)[0] * (a.row 2 ⋯)[2];
  let c0 := (a.row 2 ⋯)[0] * (a.row 3 ⋯)[1] - (a.row 3 ⋯)[0] * (a.row 2 ⋯)[1];
  let d := s0 * c5 - s1 * c4 + s2 * c3 + s3 * c2 - s4 * c1 + s5 * c0;
  let id := Inv.inv_Float32 d;
  {
    data :=
      #v[#v[((a.row 1 ⋯)[1] * c5 - (a.row 1 ⋯)[2] * c4 + (a.row 1 ⋯)[3] * c3) * id,
          (-(a.row 0 ⋯)[1] * c5 + (a.row 0 ⋯)[2] * c4 - (a.row 0 ⋯)[3] * c3) * id,
          ((a.row 3 ⋯)[1] * s5 - (a.row 3 ⋯)[2] * s4 + (a.row 3 ⋯)[3] * s3) * id,
          (-(a.row 2 ⋯)[1] * s5 + (a.row 2 ⋯)[2] * s4 - (a.row 2 ⋯)[3] * s3) * id],
        #v[(-(a.row 1 ⋯)[0] * c5 + (a.row 1 ⋯)[2] * c2 - (a.row 1 ⋯)[3] * c1) * id,
          ((a.row 0 ⋯)[0] * c5 - (a.row 0 ⋯)[2] * c2 + (a.row 0 ⋯)[3] * c1) * id,
          (-(a.row 3 ⋯)[0] * s5 + (a.row 3 ⋯)[2] * s2 - (a.row 3 ⋯)[3] * s1) * id,
          ((a.row 2 ⋯)[0] * s5 - (a.row 2 ⋯)[2] * s2 + (a.row 2 ⋯)[3] * s1) * id],
        #v[((a.row 1 ⋯)[0] * c4 - (a.row 1 ⋯)[1] * c2 + (a.row 1 ⋯)[3] * c0) * id,
          (-(a.row 0 ⋯)[0] * c4 + (a.row 0 ⋯)[1] * c2 - (a.row 0 ⋯)[3] * c0) * id,
          ((a.row 3 ⋯)[0] * s4 - (a.row 3 ⋯)[1] * s2 + (a.row 3 ⋯)[3] * s0) * id,
          (-(a.row 2 ⋯)[0] * s4 + (a.row 2 ⋯)[1] * s2 - (a.row 2 ⋯)[3] * s0) * id],
        #v[(-(a.row 1 ⋯)[0] * c3 + (a.row 1 ⋯)[1] * c1 - (a.row 1 ⋯)[2] * c0) * id,
          ((a.row 0 ⋯)[0] * c3 - (a.row 0 ⋯)[1] * c1 + (a.row 0 ⋯)[2] * c0) * id,
          (-(a.row 3 ⋯)[0] * s3 + (a.row 3 ⋯)[1] * s1 - (a.row 3 ⋯)[2] * s0) * id,
          ((a.row 2 ⋯)[0] * s3 - (a.row 2 ⋯)[1] * s1 + (a.row 2 ⋯)[2] * s0) * id]] }

Resulting specialization:
  A4.inv4_Float32
-/
#guard_msgs in
#opencl_sas (A4.inv4)


attribute [opencl_csimp] Matrix.fromRows Matrix.mapRows₂ Matrix.ofFn

/--
info: HouLean.Matrix.col_Float32_3_3_0:
fun a => #v[(a.row 0 ⋯)[0], (a.row 1 ⋯)[0], (a.row 2 ⋯)[0]]

Vector.dot_Float32_3:
fun u v =>
  let a := u[0] * v[0];
  let a := a + u[1] * v[1];
  let a := a + u[2] * v[2];
  a

HouLean.Matrix.col_Float32_3_3_1:
fun a => #v[(a.row 0 ⋯)[1], (a.row 1 ⋯)[1], (a.row 2 ⋯)[1]]

HouLean.Matrix.col_Float32_3_3_2:
fun a => #v[(a.row 0 ⋯)[2], (a.row 1 ⋯)[2], (a.row 2 ⋯)[2]]

HouLean.Matrix.matMul_Float32_3_3_3:
fun a b =>
  {
    data :=
      #v[#v[(a.row 0 ⋯).dot_Float32_3 b.col_Float32_3_3_0, (a.row 0 ⋯).dot_Float32_3 b.col_Float32_3_3_1,
          (a.row 0 ⋯).dot_Float32_3 b.col_Float32_3_3_2],
        #v[(a.row 1 ⋯).dot_Float32_3 b.col_Float32_3_3_0, (a.row 1 ⋯).dot_Float32_3 b.col_Float32_3_3_1,
          (a.row 1 ⋯).dot_Float32_3 b.col_Float32_3_3_2],
        #v[(a.row 2 ⋯).dot_Float32_3 b.col_Float32_3_3_0, (a.row 2 ⋯).dot_Float32_3 b.col_Float32_3_3_1,
          (a.row 2 ⋯).dot_Float32_3 b.col_Float32_3_3_2]] }

HMul.hMul_Matrix_Float32_3_3_Matrix_Float32_3_3_Matrix_Float32_3_3:
fun a a_1 => a.matMul_Float32_3_3_3 a_1

Resulting specialization:
  HMul.hMul_Matrix_Float32_3_3_Matrix_Float32_3_3_Matrix_Float32_3_3 A A
-/
#guard_msgs in
#opencl_sas (A * A)


/--
info: HouLean.Matrix.identity_Float_3:
{
  data :=
    #v[#v[if ↑0 = ↑0 then 1 else 0, if ↑0 = ↑1 then 1 else 0, if ↑0 = ↑2 then 1 else 0],
      #v[if ↑1 = ↑0 then 1 else 0, if ↑1 = ↑1 then 1 else 0, if ↑1 = ↑2 then 1 else 0],
      #v[if ↑2 = ↑0 then 1 else 0, if ↑2 = ↑1 then 1 else 0, if ↑2 = ↑2 then 1 else 0]] }

Resulting specialization:
  Matrix.identity_Float_3
-/
#guard_msgs in
#opencl_sas Matrix.identity Float 3 -- does not reduce element of matrix
