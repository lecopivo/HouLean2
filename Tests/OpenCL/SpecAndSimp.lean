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
#opencl_ssimp u.dot v


-- todo: why is it flipping inequalities ?? :(
/--
info: Resulting specialization:
  decide (y > x)
-/
#guard_msgs in
#opencl_ssimp (decide (x < y))

/--
info: Resulting specialization:
  decide (x > y)
-/
#guard_msgs in
#opencl_ssimp (decide (x > y))


/--
info: Resulting specialization:
  !(decide (y > x) && decide (x > 0e0))
-/
#guard_msgs in
#opencl_ssimp decide (¬(x < y ∧ 0 < x))

/--
info: Resulting specialization:
  -0.1343
-/
#guard_msgs in
#opencl_ssimp (-134.3e-3 : Float)

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
#opencl_ssimp (catmullRom (exp (sin x)) x y (x + y) x)


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

HouLean.Math.Hermite.«hermite_Vector Float 3_Float»:
fun p0 p1 t0 t1 t => p0.hermite_Float_3 p1 t0 t1 t

Resulting specialization:
  Hermite.«hermite_Vector Float 3_Float» u v (u + v) (3e0 * u) x
-/
#guard_msgs in
#opencl_ssimp (hermite u v (u+v) ((3.0:Float)*u) x)


/--
info: Resulting specialization:
  let a := v[0];
  let a := a + v[1];
  let a := a + v[2];
  a
-/
#guard_msgs in
#opencl_ssimp (∑ (i : Fin 3), v[i])
