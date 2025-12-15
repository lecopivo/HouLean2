import HouLean.OpenCL.Compiler.SpecAndSimp
import HouLean.Data.Vector
import HouLean.OpenCL.Data2.Vector
import HouLean.OpenCL.Data.Init
import HouLean.OpenCL.Data.Fin
import HouLean.OpenCL.Reference

namespace Test.OpenCL.SpecAndSimp

open HouLean Math

variable (u v w : Vector Float 3) (x y : Float)

/--
info: Vector.dot_Float_3:
fun u v =>
  let a := u[0] * v[0];
  let a := a + u[1] * v[1];
  let a := a + u[2] * v[2];
  a

Resulting specialization:
  u.dot_Float_3 v
-/
#guard_msgs in
#opencl_sas u.dot v


-- todo: why is it flipping inequalities ?? :(
/--
info: Resulting specialization:
  decide (y > x)
-/
#guard_msgs in
#opencl_sas (decide (x < y))

/--
info: Resulting specialization:
  decide (x > y)
-/
#guard_msgs in
#opencl_sas (decide (x > y))


/--
info: Resulting specialization:
  !(decide (y > x) && decide (x > 0e0))
-/
#guard_msgs in
#opencl_sas decide (¬(x < y ∧ 0 < x))

/--
info: Resulting specialization:
  -0.1343
-/
#guard_msgs in
#opencl_sas (-134.3e-3 : Float)

/--
info: HouLean.Math.CatmullRom.catmullRom_Float_Float:
fun p0 p1 p2 p3 t =>
  let t2 := t * t;
  let t3 := t2 * t;
  0.5 * (2e0 * p1 + (-p0 + p2) * t + (2e0 * p0 - 5e0 * p1 + 4e0 * p2 - p3) * t2 + (-p0 + 3e0 * p1 - 3e0 * p2 + p3) * t3)

Resulting specialization:
  CatmullRom.catmullRom_Float_Float (exp (sin x)) x y (x + y) x
-/
#guard_msgs in
#opencl_sas (catmullRom (exp (sin x)) x y (x + y) x)


/--
info: HouLean.Math.Hermite.hermite_Float_Float:
fun p0 p1 t0 t1 t =>
  let t2 := t * t;
  let t3 := t2 * t;
  let h00 := 2e0 * t3 - 3e0 * t2 + 1e0;
  let h10 := t3 - 2e0 * t2 + t;
  let h01 := -2e0 * t3 + 3e0 * t2;
  let h11 := t3 - t2;
  h00 * p0 + h10 * t0 + h01 * p1 + h11 * t1

Vector.hermite_Float_3:
fun p0 p1 t0 t1 t =>
  #v[Hermite.hermite_Float_Float p0[0] p1[0] t0[0] t1[0] t, Hermite.hermite_Float_Float p0[1] p1[1] t0[1] t1[1] t,
    Hermite.hermite_Float_Float p0[2] p1[2] t0[2] t1[2] t]

HouLean.Math.Hermite.hermite_Vector_Float_3_Float:
fun p0 p1 t0 t1 t => p0.hermite_Float_3 p1 t0 t1 t

Resulting specialization:
  Hermite.hermite_Vector_Float_3_Float u v (u + v) (3e0 * u) x
-/
#guard_msgs in
#opencl_sas (hermite u v (u+v) ((3.0:Float)*u) x)


/--
info: Resulting specialization:
  let a := v[0];
  let a := a + v[1];
  let a := a + v[2];
  a
-/
#guard_msgs in
#opencl_sas (∑ (i : Fin 3), v[i])


/--
info: Resulting specialization:
  #v[sin v[0], sin v[1], sin v[2]]
-/
#guard_msgs in
#opencl_sas (v.map sin)


/--
info: Vector.zero_Float_3:
#v[0e0, 0, 0]

Zero.«zero_Vector_Float_(instOfNatNat_3)_1»:
Vector.zero_Float_3

OfNat.ofNat_Vector_Float_3_0:
Zero.«zero_Vector_Float_(instOfNatNat_3)_1»

Resulting specialization:
  OfNat.ofNat_Vector_Float_3_0
-/
#guard_msgs in
#opencl_sas (0 : Vector Float 3)


/--
info: Resulting specialization:
  #v[1e0, 1, 1]
-/
#guard_msgs in
#opencl_sas (.replicate _ 1 : Vector Float 3)

/--
info: Resulting specialization:
  let a := v[0];
  let a := a + v[1];
  let a := a + v[2];
  a
-/
#guard_msgs in
#opencl_sas (v.foldl (·+·) 0)


/--
info: Resulting specialization:
  let a := v[2];
  let a := v[1] + a;
  let a := v[0] + a;
  a
-/
#guard_msgs in
#opencl_sas (v.foldr (·+·) 0)

/--
info: Resulting specialization:
  let a := u[0];
  let a := a + u[1];
  let a := a + u[2];
  a
-/
#guard_msgs in
#opencl_sas (u.sum)

/--
info: Resulting specialization:
  #v[10 * ↑0, 10 * ↑1, 10 * ↑2, 10 * ↑3, 10 * ↑4, 10 * ↑5, 10 * ↑6, 10 * ↑7, 10 * ↑8, 10 * ↑9]
-/
#guard_msgs in
#opencl_sas (Vector.ofFn (fun i : Fin 10 => 10*i.1))


variable (n : Nat) (i : Fin 3)

/-- error: Expected `n` to be value known at compile time! -/
#guard_msgs in
#opencl_sas (Vector.ofFn (fun i : Fin n => 10*i.1))

/--
info: Fin.val_3:
fun self => self.1

Resulting specialization:
  u[i.val_3]
-/
#guard_msgs in
#opencl_sas (u[i])

/--
info: Vector.dot_Float_3:
fun u v =>
  let a := u[0] * v[0];
  let a := a + u[1] * v[1];
  let a := a + u[2] * v[2];
  a

Vector.refract_Float_3:
fun v normal eta =>
  let dt := v.dot_Float_3 normal;
  let k := 1e0 - eta * eta * (1e0 - dt * dt);
  let s := eta * dt + sqrt k;
  if k < 0e0 then 0 else eta * v - s * normal

HouLean.Math.Refract.refract_Vector_Float_3_Float:
fun v n eta => v.refract_Float_3 n eta

Resulting specialization:
  Refract.refract_Vector_Float_3_Float u v x
-/
#guard_msgs in
#opencl_sas refract u v x

/--
info: Vector.compDiv_Float_3:
fun x y => #v[x[0] / y[0], x[1] / y[1], x[2] / y[2]]

HouLean.Math.CompDiv.compDiv_Vector_Float_3:
fun x y => x.compDiv_Float_3 y

Resulting specialization:
  CompDiv.compDiv_Vector_Float_3 u v
-/
#guard_msgs in
#opencl_sas compDiv u v

/--
info: Vector.length2_Float_3:
fun u =>
  let a := u[0] * u[0];
  let a := a + u[1] * u[1];
  let a := a + u[2] * u[2];
  a

Vector.length_Float_3:
fun u => sqrt u.length2_Float_3

HouLean.Math.ApproxEqual.approxEqual_Float_Float_0e0_1e_6:
fun x => decide (1e-6 ≥ abs (x - 0e0))

Inv.inv_Float:
fun a => 1e0 / a

HDiv.hDiv_Vector_Float_3_Float_Vector_Float_3:
fun a a_1 =>
  let is := Inv.inv_Float a_1;
  #v[is * a[0], is * a[1], is * a[2]]

Vector.normalize_Float_3:
fun u =>
  let len := u.length_Float_3;
  if ApproxEqual.approxEqual_Float_Float_0e0_1e_6 len = true then (u, 0e0)
  else (HDiv.hDiv_Vector_Float_3_Float_Vector_Float_3 u len, len)

Prod.fst_Vector_Float_3_Float:
fun self => self.1

Vector.normalized_Float_3:
fun u => u.normalize_Float_3.fst_Vector_Float_3_Float

Vector.dot_Float_3:
fun u v =>
  let a := u[0] * v[0];
  let a := a + u[1] * v[1];
  let a := a + u[2] * v[2];
  a

Vector.slerp_Float_3:
fun v w t =>
  let d := v.normalized_Float_3.dot_Float_3 w.normalized_Float_3;
  let d := clamp d (-1e0) 1e0;
  let theta := acos d;
  let s := sin theta;
  let a := sin ((1e0 - t) * theta) / s;
  let b := sin (t * theta) / s;
  if ApproxEqual.approxEqual_Float_Float_0e0_1e_6 theta = true then lerp v w t else a * v + b * w

HouLean.Math.Slerp.slerp_Vector_Float_3_Float:
fun x y t => x.slerp_Float_3 y t

Resulting specialization:
  Slerp.slerp_Vector_Float_3_Float u v x
-/
#guard_msgs in
#opencl_sas slerp u v x


/--
info: Vector.dot_Float_3:
fun u v =>
  let a := u[0] * v[0];
  let a := a + u[1] * v[1];
  let a := a + u[2] * v[2];
  a

Vector.length2_Float_3:
fun u =>
  let a := u[0] * u[0];
  let a := a + u[1] * u[1];
  let a := a + u[2] * u[2];
  a

Vector.projectToSegment_Float_3:
fun point a b =>
  let ab := b - a;
  let ap := point - a;
  let t := clamp (ap.dot_Float_3 ab / ab.length2_Float_3) 0e0 1e0;
  a + t * ab

HouLean.Math.ProjectToSegment.projectToSegment_Vector_Float_3:
fun point a b => point.projectToSegment_Float_3 a b

Resulting specialization:
  ProjectToSegment.projectToSegment_Vector_Float_3 u v w
-/
#guard_msgs in
#opencl_sas projectToSegment u v w


private def foo (x y : Float) := x + y


/--
info: _private.Tests.OpenCL.SpecAndSimp.0.Test.OpenCL.SpecAndSimp.foo_0e0:
fun y => 0e0 + y

Resulting specialization:
  foo_0e0 x
-/
#guard_msgs in
#opencl_sas (foo 0.0 x)
