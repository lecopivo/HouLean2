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
  Matrix.ofFn Matrix.mapRows Matrix.mapIdx Matrix.mapFinIdx Matrix.mapRowsFinIdx Matrix.fromRows

 -- do somethig about these function as they cause problems with dependent types
attribute [simp] Matrix.row Matrix.col

/-- info: fun A => A -/
#guard_msgs in
#sas fun A : Matrix Float 3 4 => A

/-- info: fun A => A.x0.x1 -/
#guard_msgs in
#sas fun A : Matrix Float 3 4 => A[0,1]

/--
info: def HouLean.Matrix.xw.Float_3_4 := ⏎
  fun a => a.x0.x3

fun A =>
  let tmp := Matrix.xw.Float_3_4 A;
  tmp
-/
#guard_msgs in
#sas fun A : Matrix Float 3 4 => A.xw

/-- info: fun x y z w => { x0 := { x0 := x, x1 := y }, x1 := { x0 := z, x1 := w } } -/
#guard_msgs in
#sas fun x y z w : Float => #m[x, y; z, w]

/-- info: fun x => { x0 := { x0 := x, x1 := 0 }, x1 := { x0 := 0, x1 := x } } -/
#guard_msgs in
#sas fun x : Float => Matrix.ofFn (m:=2) (n:=2) fun i j => if i = j then x else 0

/-- info: { x0 := { x0 := 0, x1 := 1 }, x1 := { x0 := 1, x1 := 0 } } -/
#guard_msgs in
#sas Matrix.fromRows (m:=2) (n:=2) fun i =>
   if i = 0 then #v[0, 1] else #v[1, 0]

/--
info: def HouLean.Math.Sin.sin.Float := ⏎
  fun x => x.sin

def HouLean.Matrix.map.Float_Float_2_3 := ⏎
  fun a =>
    { x0 := { x0 := Math.Sin.sin.Float a.x0.x0, x1 := Math.Sin.sin.Float a.x0.x1, x2 := Math.Sin.sin.Float a.x0.x2 },
      x1 := { x0 := Math.Sin.sin.Float a.x1.x0, x1 := Math.Sin.sin.Float a.x1.x1, x2 := Math.Sin.sin.Float a.x1.x2 } }

def HouLean.Matrix.sin.Float_2_3 := ⏎
  fun a =>
    let tmp := Matrix.map.Float_Float_2_3 a;
    tmp

fun A =>
  let tmp := Matrix.sin.Float_2_3 A;
  tmp
-/
#guard_msgs in
#sas fun A : Matrix Float 2 3 => A.sin

/--
info: def HouLean.Matrix.identity.Float_3 := ⏎
  { x0 := { x0 := 1, x1 := 0, x2 := 0 }, x1 := { x0 := 0, x1 := 1, x2 := 0 }, x2 := { x0 := 0, x1 := 0, x2 := 1 } }

Matrix.identity.Float_3
-/
#guard_msgs in
#sas Matrix.identity Float 3


attribute [simp] Matrix.mapRows₂ Vector.mapFinIdx Matrix.mapRows Matrix.mapRowsFinIdx

/--
info: def Vector.neg.Float_2 := ⏎
  fun a => { x0 := -a.x0, x1 := -a.x1 }

def Neg.neg.Vector_Float_2 := ⏎
  fun a =>
    let tmp := Vector.neg.Float_2 a;
    tmp

def HouLean.Matrix.neg.Float_2_2 := ⏎
  fun a =>
    let tmp := Neg.neg.Vector_Float_2 a.x0;
    let tmp_1 := Neg.neg.Vector_Float_2 a.x1;
    { x0 := tmp, x1 := tmp_1 }

def Neg.neg.Matrix_Float_2_2 := ⏎
  fun a =>
    let tmp := Matrix.neg.Float_2_2 a;
    tmp

fun A =>
  let tmp := Neg.neg.Matrix_Float_2_2 A;
  tmp
-/
#guard_msgs in
#sas fun A : Matrix Float 2 2 => -A

attribute [simp] Matrix.row

/--
info: def Vector.add.Float_2 := ⏎
  fun a a_1 => { x0 := a.x0 + a_1.x0, x1 := a.x1 + a_1.x1 }

def Add.add.Vector_Float_2 := ⏎
  fun a a_1 =>
    let tmp := Vector.add.Float_2 a a_1;
    tmp

def HAdd.hAdd.Vector_Float_2_Vector_Float_2_Vector_Float_2 := ⏎
  fun a a_1 =>
    let tmp := Add.add.Vector_Float_2 a a_1;
    tmp

def HouLean.Matrix.add.Float_2_2 := ⏎
  fun a a_1 =>
    let tmp := HAdd.hAdd.Vector_Float_2_Vector_Float_2_Vector_Float_2 a.x0 a_1.x0;
    let tmp_1 := HAdd.hAdd.Vector_Float_2_Vector_Float_2_Vector_Float_2 a.x1 a_1.x1;
    { x0 := tmp, x1 := tmp_1 }

def Add.add.Matrix_Float_2_2 := ⏎
  fun a a_1 =>
    let tmp := Matrix.add.Float_2_2 a a_1;
    tmp

def HAdd.hAdd.Matrix_Float_2_2_Matrix_Float_2_2_Matrix_Float_2_2 := ⏎
  fun a a_1 =>
    let tmp := Add.add.Matrix_Float_2_2 a a_1;
    tmp

fun A B =>
  let tmp := HAdd.hAdd.Matrix_Float_2_2_Matrix_Float_2_2_Matrix_Float_2_2 A B;
  tmp
-/
#guard_msgs in
#sas fun A B : Matrix Float 2 2 => A + B


/--
info: def Vector.dot.Float_2 := ⏎
  fun a a_1 => 0 + a.x0 * a_1.x0 + a.x1 * a_1.x1

def HouLean.Matrix.mulVec.Float_2_2 := ⏎
  fun a a_1 =>
    let tmp := Vector.dot.Float_2 a.x0 a_1;
    let tmp_1 := Vector.dot.Float_2 a.x1 a_1;
    { x0 := tmp, x1 := tmp_1 }

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
info: def Vector.dot.Float_2 := ⏎
  fun a a_1 => 0 + a.x0 * a_1.x0 + a.x1 * a_1.x1

def HouLean.Matrix.vecMul.Float_2_2 := ⏎
  fun a a_1 =>
    let tmp := Vector.dot.Float_2 a { x0 := a_1.x0.x0, x1 := a_1.x1.x0 };
    let tmp_1 := Vector.dot.Float_2 a { x0 := a_1.x0.x1, x1 := a_1.x1.x1 };
    { x0 := tmp, x1 := tmp_1 }

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


/--
info: def Vector.dot.Float_3 := ⏎
  fun a a_1 => 0 + a.x0 * a_1.x0 + a.x1 * a_1.x1 + a.x2 * a_1.x2

def HouLean.Matrix.matMul.Float_2_3_2 := ⏎
  fun a a_1 =>
    let tmp := Vector.dot.Float_3 a.x0 { x0 := a_1.x0.x0, x1 := a_1.x1.x0, x2 := a_1.x2.x0 };
    let tmp_1 := Vector.dot.Float_3 a.x0 { x0 := a_1.x0.x1, x1 := a_1.x1.x1, x2 := a_1.x2.x1 };
    let tmp_2 := Vector.dot.Float_3 a.x1 { x0 := a_1.x0.x0, x1 := a_1.x1.x0, x2 := a_1.x2.x0 };
    let tmp_3 := Vector.dot.Float_3 a.x1 { x0 := a_1.x0.x1, x1 := a_1.x1.x1, x2 := a_1.x2.x1 };
    { x0 := { x0 := tmp, x1 := tmp_1 }, x1 := { x0 := tmp_2, x1 := tmp_3 } }

def HMul.hMul.Matrix_Float_2_3_Matrix_Float_3_2_Matrix_Float_2_2 := ⏎
  fun a a_1 =>
    let tmp := Matrix.matMul.Float_2_3_2 a a_1;
    tmp

fun A B =>
  let tmp := HMul.hMul.Matrix_Float_2_3_Matrix_Float_3_2_Matrix_Float_2_2 A B;
  tmp
-/
#guard_msgs in
#sas fun (A : Matrix Float 2 3) (B : Matrix Float 3 2) => A * B


/--
info: def HouLean.Matrix.split1.«3_Float» := ⏎
  fun a =>
    (({ x0 := { x0 := a.x0.x0, x1 := a.x0.x1, x2 := a.x0.x2 }, x1 := { x0 := a.x1.x0, x1 := a.x1.x1, x2 := a.x1.x2 },
          x2 := { x0 := a.x2.x0, x1 := a.x2.x1, x2 := a.x2.x2 } },
        { x0 := a.x0.x3, x1 := a.x1.x3, x2 := a.x2.x3 }),
      { x0 := a.x3.x0, x1 := a.x3.x1, x2 := a.x3.x2 }, a.x3.x3)

fun A =>
  let tmp := Matrix.split1.«3_Float» A;
  tmp
-/
#guard_msgs in
#sas (fun (A : Matrix Float 4 4) => A.split1)


/--
info: def HouLean.Matrix.split1.«3_Float» := ⏎
  fun a =>
    (({ x0 := { x0 := a.x0.x0, x1 := a.x0.x1, x2 := a.x0.x2 }, x1 := { x0 := a.x1.x0, x1 := a.x1.x1, x2 := a.x1.x2 },
          x2 := { x0 := a.x2.x0, x1 := a.x2.x1, x2 := a.x2.x2 } },
        { x0 := a.x0.x3, x1 := a.x1.x3, x2 := a.x2.x3 }),
      { x0 := a.x3.x0, x1 := a.x3.x1, x2 := a.x3.x2 }, a.x3.x3)

def Vector.dot.Float_3 := ⏎
  fun a a_1 => 0 + a.x0 * a_1.x0 + a.x1 * a_1.x1 + a.x2 * a_1.x2

def HouLean.Matrix.mulVec.Float_3_3 := ⏎
  fun a a_1 =>
    let tmp := Vector.dot.Float_3 a.x0 a_1;
    let tmp_1 := Vector.dot.Float_3 a.x1 a_1;
    let tmp_2 := Vector.dot.Float_3 a.x2 a_1;
    { x0 := tmp, x1 := tmp_1, x2 := tmp_2 }

def HMul.hMul.Matrix_Float_3_3_Vector_Float_3_Vector_Float_3 := ⏎
  fun a a_1 =>
    let tmp := Matrix.mulVec.Float_3_3 a a_1;
    tmp

def Vector.add.Float_3 := ⏎
  fun a a_1 => { x0 := a.x0 + a_1.x0, x1 := a.x1 + a_1.x1, x2 := a.x2 + a_1.x2 }

def Add.add.Vector_Float_3 := ⏎
  fun a a_1 =>
    let tmp := Vector.add.Float_3 a a_1;
    tmp

def HAdd.hAdd.Vector_Float_3_Vector_Float_3_Vector_Float_3 := ⏎
  fun a a_1 =>
    let tmp := Add.add.Vector_Float_3 a a_1;
    tmp

def Inv.inv.Float := ⏎
  fun a => 1.0 / a

def Vector.hmul.Float_Float_Float_3 := ⏎
  fun a a_1 => { x0 := a * a_1.x0, x1 := a * a_1.x1, x2 := a * a_1.x2 }

def HMul.hMul.Float_Vector_Float_3_Vector_Float_3 := ⏎
  fun a a_1 =>
    let tmp := Vector.hmul.Float_Float_Float_3 a a_1;
    tmp

def HDiv.hDiv.Vector_Float_3_Float_Vector_Float_3 := ⏎
  fun a a_1 =>
    let tmp := Inv.inv.Float a_1;
    let tmp := HMul.hMul.Float_Vector_Float_3_Vector_Float_3 tmp a;
    tmp

def HouLean.Matrix.transformPointRight.Float_3 := ⏎
  fun a a_1 =>
    let tmp := Matrix.split1.«3_Float» a;
    let w := (TypeEncoding.decode a_1).dot (TypeEncoding.decode tmp.snd.fst) + tmp.snd.snd;
    let tmp_1 := HMul.hMul.Matrix_Float_3_3_Vector_Float_3_Vector_Float_3 tmp.fst.fst a_1;
    let tmp := HAdd.hAdd.Vector_Float_3_Vector_Float_3_Vector_Float_3 tmp_1 tmp.fst.snd;
    let tmp := HDiv.hDiv.Vector_Float_3_Float_Vector_Float_3 tmp w;
    tmp

fun A v =>
  let tmp := Matrix.transformPointRight.Float_3 A v;
  tmp
-/
#guard_msgs in
#sas (fun (A : Matrix Float 4 4) (v : Vector Float 3) => A.transformPointRight v)
