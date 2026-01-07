import HouLean.Meta.SpecializeAndSimp2.Main
import HouLean.Meta.SpecializeAndSimp2.Command
import HouLean.Meta.FloatPolynomial
import HouLean.Data.Vector
import HouLean.Data.Matrix
import HouLean.Meta.RewriteBy

namespace Test.Meta.Sas2

open Lean Elab Command Term

open HouLean.Meta.Sas HouLean NormalMatrixVecMul

@[simp]
theorem Fin.foldl_inline (n) (f : α → Fin (n+1) → α) (init : α) : Fin.foldl (n+1) f init = Fin.foldl n (fun x i => f x ⟨i.1+1, by omega⟩) (f init 0) := by
  sorry_proof

-- todo: change to simproc to make faster
@[simp]
theorem Fin.foldr_inline (n) (f : Fin (n+1) → α → α) (init : α) : Fin.foldr (n+1) f init = Fin.foldr n (fun i x => f ⟨i.1, by omega⟩ x) (f ⟨n, by omega⟩ init) := by
  sorry_proof

attribute [simp] HouLean.sum -- remove this as it should be inlined automatically


attribute [simp] Vector.zipWith Vector.map Vector.ofFn Vector.foldr Vector.foldl Vector.mapIdx Vector.mapFinIdx
  Matrix.ofFn Matrix.mapRows Matrix.mapIdx Matrix.mapFinIdx Matrix.mapRowsFinIdx

/-- info: fun A => A -/
#guard_msgs in
#sas fun A : Matrix Float 3 4 => A

/-- info: fun A => A.x0.x1 -/
#guard_msgs in
#sas fun A : Matrix Float 3 4 => A[0,1]

/-- info: fun x y z w => { x0 := { x0 := x, x1 := y }, x1 := { x0 := z, x1 := w } } -/
#guard_msgs in
#sas fun x y z w : Float => #m[x, y; z, w]

/-- info: fun x => { x0 := { x0 := x, x1 := 0 }, x1 := { x0 := 0, x1 := x } } -/
#guard_msgs in
#sas fun x : Float => Matrix.ofFn (m:=2) (n:=2) fun i j _ => if i = j then x else 0

/--
info: def HouLean.Math.Sin.sin.Float := ⏎
  fun x => x.sin

def HouLean.Matrix.map.Float_Float_2_2 := ⏎
  fun a =>
    { x0 := { x0 := Math.Sin.sin.Float a.x0.x0, x1 := Math.Sin.sin.Float a.x0.x1 },
      x1 := { x0 := Math.Sin.sin.Float a.x1.x0, x1 := Math.Sin.sin.Float a.x1.x1 } }

def HouLean.Matrix.sin.Float_2_2 := ⏎
  fun a =>
    let tmp := Matrix.map.Float_Float_2_2 a;
    tmp

fun A =>
  let tmp := Matrix.sin.Float_2_2 A;
  tmp
-/
#guard_msgs in
#sas fun A : Matrix Float 2 2 => A.sin

/--
info: def HouLean.Matrix.row.Float_2_2_0 := ⏎
  fun a => a.x0

def Vector.dot.Float_2 := ⏎
  fun a a_1 => 0 + a.x0 * a_1.x0 + a.x1 * a_1.x1

def HouLean.Matrix.row.Float_2_2_1 := ⏎
  fun a => a.x1

def HouLean.Matrix.mulVec.Float_2_2 := ⏎
  fun a a_1 =>
    let tmp := Matrix.row.Float_2_2_0 a;
    let tmp := Vector.dot.Float_2 tmp a_1;
    let tmp_1 := Matrix.row.Float_2_2_1 a;
    let tmp_2 := Vector.dot.Float_2 tmp_1 a_1;
    { x0 := tmp, x1 := tmp_2 }

def HMul.hMul.Matrix_Float_2_2_Vector_Float_2_Vector_Float_2 := ⏎
  fun a a_1 =>
    let tmp := Matrix.mulVec.Float_2_2 a a_1;
    tmp

fun A v =>
  let tmp := HMul.hMul.Matrix_Float_2_2_Vector_Float_2_Vector_Float_2 A v;
  tmp
-/
#guard_msgs in
#sas fun (A : Matrix Float 2 2) (v : Vector Float 2) => A * v

/--
info: def HouLean.Matrix.col.Float_2_2_0 := ⏎
  fun a => { x0 := a.x0.x0, x1 := a.x1.x0 }

def Vector.dot.Float_2 := ⏎
  fun a a_1 => 0 + a.x0 * a_1.x0 + a.x1 * a_1.x1

def HouLean.Matrix.col.Float_2_2_1 := ⏎
  fun a => { x0 := a.x0.x1, x1 := a.x1.x1 }

def HouLean.Matrix.vecMul.Float_2_2 := ⏎
  fun a a_1 =>
    let tmp := Matrix.col.Float_2_2_0 a_1;
    let tmp := Vector.dot.Float_2 a tmp;
    let tmp_1 := Matrix.col.Float_2_2_1 a_1;
    let tmp_2 := Vector.dot.Float_2 a tmp_1;
    { x0 := tmp, x1 := tmp_2 }

def HMul.hMul.Vector_Float_2_Matrix_Float_2_2_Vector_Float_2 := ⏎
  fun a a_1 =>
    let tmp := Matrix.vecMul.Float_2_2 a a_1;
    tmp

fun A v =>
  let tmp := HMul.hMul.Vector_Float_2_Matrix_Float_2_2_Vector_Float_2 v A;
  tmp
-/
#guard_msgs in
#sas fun (A : Matrix Float 2 2) (v : Vector Float 2) => v * A
