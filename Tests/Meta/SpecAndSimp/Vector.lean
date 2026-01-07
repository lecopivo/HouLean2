import HouLean.Meta.SpecializeAndSimp2.Main
import HouLean.Meta.SpecializeAndSimp2.Command
import HouLean.Meta.FloatPolynomial
import HouLean.Data.Vector
import HouLean.Data.Matrix
import HouLean.Meta.RewriteBy

namespace Test.Meta.Sas2

open Lean Elab Command Term

open HouLean.Meta.Sas


set_option pp.funBinderTypes true

/-- info: fun (x : Vector3 Float) => x -/
#guard_msgs in
#sas fun x : Vector Float 3 => x

/-- info: fun (x : Vector3 Float) => x.x0 -/
#guard_msgs in
#sas fun x : Vector Float 3 => x[0]

/-- info: fun (x : Vector3 Float) => x.x1 -/
#guard_msgs in
#sas fun x : Vector Float 3 => x[1]

/-- info: fun (x : Vector3 Float) => x.x2 -/
#guard_msgs in
#sas fun x : Vector Float 3 => x[2]

def f1 {α : Type} (x : Vector α 3) := x

/--
info: def Test.Meta.Sas2.f1.Float := ⏎
  fun (a : Vector3 Float) => a

fun (x : Vector3 Float) =>
  let tmp := f1.Float x;
  tmp
-/
#guard_msgs in
#sas fun x : Vector Float 3 => f1 x

/--
info: def Vector.y.Float_3 := ⏎
  fun (a : Vector3 Float) => a.x1

fun (u : Vector3 Float) =>
  let tmp := Vector.y.Float_3 u;
  tmp
-/
#guard_msgs in
#sas fun u : Vector Float 3 => u.y

-- these are higher order function that should be inline anyway
attribute [simp] Vector.zipWith Vector.map Vector.ofFn Vector.foldr Vector.foldl

/-- info: fun (x y z : Float) => { x0 := x, x1 := y, x2 := z } -/
#guard_msgs in
#sas fun x y z : Float => #v[x,y,z]

/-- info: fun (x y z : Vector3 Float) => { x0 := x, x1 := y, x2 := z } -/
#guard_msgs in
#sas fun x y z : Vector Float 3 => #v[x,y,z]

/-- info: fun (x y z : Vector3 (Vector3 Float)) => { x0 := x, x1 := y, x2 := z } -/
#guard_msgs in
#sas fun x y z : Vector (Vector Float 3) 3 => #v[x,y,z]

/--
info: def Vector.neg.Float_3 := ⏎
  fun (a : Vector3 Float) => { x0 := -a.x0, x1 := -a.x1, x2 := -a.x2 }

def Neg.neg.Vector_Float_3 := ⏎
  fun (a : Vector3 Float) =>
    let tmp := Vector.neg.Float_3 a;
    tmp

fun (x : Vector3 Float) =>
  let tmp := Neg.neg.Vector_Float_3 x;
  tmp
-/
#guard_msgs in
#sas fun x : Vector Float 3 => -x

/--
info: def Vector.add.Float_3 := ⏎
  fun (a a_1 : Vector3 Float) => { x0 := a.x0 + a_1.x0, x1 := a.x1 + a_1.x1, x2 := a.x2 + a_1.x2 }

def Add.add.Vector_Float_3 := ⏎
  fun (a a_1 : Vector3 Float) =>
    let tmp := Vector.add.Float_3 a a_1;
    tmp

def HAdd.hAdd.Vector_Float_3_Vector_Float_3_Vector_Float_3 := ⏎
  fun (a a_1 : Vector3 Float) =>
    let tmp := Add.add.Vector_Float_3 a a_1;
    tmp

fun (x y : Vector3 Float) =>
  let tmp := HAdd.hAdd.Vector_Float_3_Vector_Float_3_Vector_Float_3 x y;
  tmp
-/
#guard_msgs in
#sas fun x y : Vector Float 3 => x + y

/--
info: def HouLean.Math.Sin.sin.Float := ⏎
  fun (a : Float) => a.sin

def Vector.sin.Float_3 := ⏎
  fun (a : Vector3 Float) =>
    let tmp := HouLean.Math.Sin.sin.Float a.x0;
    let tmp_1 := HouLean.Math.Sin.sin.Float a.x1;
    let tmp_2 := HouLean.Math.Sin.sin.Float a.x2;
    { x0 := tmp, x1 := tmp_1, x2 := tmp_2 }

def HouLean.Math.Sin.sin.Vector_Float_3 := ⏎
  fun (a : Vector3 Float) =>
    let tmp := Vector.sin.Float_3 a;
    tmp

fun (x : Vector3 Float) =>
  let tmp := HouLean.Math.Sin.sin.Vector_Float_3 x;
  tmp
-/
#guard_msgs in
#sas fun x : Vector Float 3 => HouLean.Math.sin x

/-- info: fun (f : Fin 3 → Float) => { x0 := f 0, x1 := f 1, x2 := f 2 } -/
#guard_msgs in
#sas fun f : Fin 3 → Float => Vector.ofFn f

/--
info: def HouLean.basisVector.Float_3_1 := ⏎
  { x0 := 0, x1 := 1, x2 := 0 }

HouLean.basisVector.Float_3_1
-/
#guard_msgs in
#sas HouLean.basisVector Float 3 1

/--
info: def Inv.inv.Float := ⏎
  fun (a : Float) => 1.0 / a

def Vector.hmul.Float_Float_Float_3 := ⏎
  fun (a : Float) (a_1 : Vector3 Float) => { x0 := a * a_1.x0, x1 := a * a_1.x1, x2 := a * a_1.x2 }

def HMul.hMul.Float_Vector_Float_3_Vector_Float_3 := ⏎
  fun (a : Float) (a_1 : Vector3 Float) =>
    let tmp := Vector.hmul.Float_Float_Float_3 a a_1;
    tmp

def HDiv.hDiv.Vector_Float_3_Float_Vector_Float_3 := ⏎
  fun (a : Vector3 Float) (a_1 : Float) =>
    let tmp := Inv.inv.Float a_1;
    let tmp := HMul.hMul.Float_Vector_Float_3_Vector_Float_3 tmp a;
    tmp

fun (u : Vector3 Float) (r : Float) =>
  let tmp := HDiv.hDiv.Vector_Float_3_Float_Vector_Float_3 u r;
  tmp
-/
#guard_msgs in
#sas fun (u : Vector Float 3) (r : Float) => u / r


/--info: def Vector.split1.«2_Float» := ⏎
  fun (a : Vector3 Float) => ({ x0 := a.x0, x1 := a.x1 }, a.x2)

fun (x : Vector3 Float) =>
  let tmp := Vector.split1.«2_Float» x;
  tmp
-/
#guard_msgs in
#sas fun x : Vector Float 3 => x.split1

/--
info: def Vector.split.«4_Float_2_2» := ⏎
  fun (a : Vector4 Float) => ({ x0 := a.x0, x1 := a.x1 }, { x0 := a.x2, x1 := a.x3 })

fun (x : Vector4 Float) =>
  let tmp := Vector.split.«4_Float_2_2» x;
  tmp
-/
#guard_msgs in
#sas fun x : Vector Float 4 => x.split 2 2

/--
info: def Vector.sum.Float_3 := ⏎
  fun (a : Vector3 Float) => a.x0 + (a.x1 + (a.x2 + 0))

fun (x : Vector3 Float) =>
  let tmp := Vector.sum.Float_3 x;
  tmp
-/
#guard_msgs in
#sas (fun (x : Vector Float 3) => x.sum)


-- BROKEN FUNCTIONS

-- Vector.foldl and Vector.fodlr

-- for some reason `simp` is not doing the same work in `sas` as normally and this case this failure
-- #sas fun x : Vector Float 3 => x.foldr (·+·) 0
-- #check (fun x : Vector Float 3 => x.foldr (·+·) 0) rewrite_by simp
