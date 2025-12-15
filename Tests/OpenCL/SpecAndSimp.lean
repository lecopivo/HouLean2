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


/--
info: Resulting specialization:
  #v[sin v[0], sin v[1], sin v[2]]
-/
#guard_msgs in
#opencl_ssimp (v.map sin)


/--
info: Vector.zero_Float_3:
#v[0, 0, 0]

Zero.«zero_Vector Float (instOfNatNat 3).1»:
Vector.zero_Float_3

OfNat.«ofNat_Vector Float 3_0»:
Zero.«zero_Vector Float (instOfNatNat 3).1»

Resulting specialization:
  OfNat.«ofNat_Vector Float 3_0»
-/
#guard_msgs in
#opencl_ssimp (0 : Vector Float 3)


/--
info: Resulting specialization:
  #v[1, 1, 1]
-/
#guard_msgs in
#opencl_ssimp (.replicate _ 1 : Vector Float 3)



/--
info: Vector.toArray_Float_3:
fun self => self.1

Array.size_Float:
fun a => a.toList.length

LE.le_Nat:
fun a a_1 => a.le a_1

dite_Float:
fun c [h : Decidable c] t e => Decidable.rec (fun h => e h) (fun h => t h) h

Array.«foldlM_Float_Float_fun type => type_fun x1 x2 => pure (x1 + x2)_Float.ofScientific 0 false 0_0»:
fun as stop =>
  let fold := fun stop h => Array.foldlM.loop (fun x1 x2 => pure (x1 + x2)) as stop h (stop - 0) 0 0;
  dite_Float (LE.le_Nat stop as.size_Float) (fun h => fold stop h) fun h => fold as.size ⋯

Array.«foldl_Float_Float_fun x1 x2 => x1 + x2_Float.ofScientific 0 false 0_0»:
fun as stop => as.«foldlM_Float_Float_fun type => type_fun x1 x2 => pure (x1 + x2)_Float.ofScientific 0 false 0_0» stop

Vector.«foldl_Float_Float_3_fun x1 x2 => x1 + x2_Float.ofScientific 0 false 0»:
fun xs =>
  xs.toArray_Float_3.«foldl_Float_Float_fun x1 x2 => x1 + x2_Float.ofScientific 0 false 0_0»
    xs.toArray_Float_3.size_Float

Resulting specialization:
  v.«foldl_Float_Float_3_fun x1 x2 => x1 + x2_Float.ofScientific 0 false 0»
---
error: code generator does not support recursor `Decidable.rec` yet, consider using 'match ... with' and/or structural recursion
---
error: failed to compile definition, compiler IR check failed at `Array.«foldlM_Float_Float_fun type => type_fun x1 x2 => pure (x1 + x2)_Float.ofScientific 0 false 0_0»`. Error: depends on declaration 'dite_Float', which has no executable code; consider marking definition as 'noncomputable'
---
error: failed to compile definition, compiler IR check failed at `Array.«foldl_Float_Float_fun x1 x2 => x1 + x2_Float.ofScientific 0 false 0_0»`. Error: depends on declaration 'Array.«foldlM_Float_Float_fun type => type_fun x1 x2 => pure (x1 + x2)_Float.ofScientific 0 false 0_0»', which has no executable code; consider marking definition as 'noncomputable'
---
error: failed to compile definition, compiler IR check failed at `Vector.«foldl_Float_Float_3_fun x1 x2 => x1 + x2_Float.ofScientific 0 false 0»`. Error: depends on declaration 'Array.«foldl_Float_Float_fun x1 x2 => x1 + x2_Float.ofScientific 0 false 0_0»', which has no executable code; consider marking definition as 'noncomputable'
-/
#guard_msgs in
#opencl_ssimp (v.foldl (·+·) 0)





/--
info: Vector.toArray_Float_3:
fun self => self.1

Array.size_Float:
fun a => a.toList.length

LE.le_Nat:
fun a a_1 => a.le a_1

dite_Float:
fun c [h : Decidable c] t e => Decidable.rec (fun h => e h) (fun h => t h) h

Array.«foldrM_Float_Float_fun type => type_fun x1 x2 => pure (x1 + x2)_Float.ofScientific 0 false 0_0»:
fun as start =>
  dite_Float (LE.le_Nat start as.size_Float)
    (fun h => if 0 < start then Array.foldrM.fold (fun x1 x2 => pure (x1 + x2)) as 0 start h 0 else pure 0) fun h =>
    if 0 < as.size then Array.foldrM.fold (fun x1 x2 => pure (x1 + x2)) as 0 as.size ⋯ 0 else pure 0

Array.«foldr_Float_Float_fun x1 x2 => x1 + x2_Float.ofScientific 0 false 0_0»:
fun as start =>
  as.«foldrM_Float_Float_fun type => type_fun x1 x2 => pure (x1 + x2)_Float.ofScientific 0 false 0_0» start

Vector.«foldr_Float_Float_3_fun x1 x2 => x1 + x2_Float.ofScientific 0 false 0»:
fun xs =>
  xs.toArray_Float_3.«foldr_Float_Float_fun x1 x2 => x1 + x2_Float.ofScientific 0 false 0_0»
    xs.toArray_Float_3.size_Float

Resulting specialization:
  v.«foldr_Float_Float_3_fun x1 x2 => x1 + x2_Float.ofScientific 0 false 0»
---
error: code generator does not support recursor `Decidable.rec` yet, consider using 'match ... with' and/or structural recursion
---
error: failed to compile definition, compiler IR check failed at `Array.«foldrM_Float_Float_fun type => type_fun x1 x2 => pure (x1 + x2)_Float.ofScientific 0 false 0_0»`. Error: depends on declaration 'dite_Float', which has no executable code; consider marking definition as 'noncomputable'
---
error: failed to compile definition, compiler IR check failed at `Array.«foldr_Float_Float_fun x1 x2 => x1 + x2_Float.ofScientific 0 false 0_0»`. Error: depends on declaration 'Array.«foldrM_Float_Float_fun type => type_fun x1 x2 => pure (x1 + x2)_Float.ofScientific 0 false 0_0»', which has no executable code; consider marking definition as 'noncomputable'
---
error: failed to compile definition, compiler IR check failed at `Vector.«foldr_Float_Float_3_fun x1 x2 => x1 + x2_Float.ofScientific 0 false 0»`. Error: depends on declaration 'Array.«foldr_Float_Float_fun x1 x2 => x1 + x2_Float.ofScientific 0 false 0_0»', which has no executable code; consider marking definition as 'noncomputable'
-/
#guard_msgs in
#opencl_ssimp (v.foldr (·+·) 0)



/--
info: Vector.toArray_Float_3:
fun self => self.1

Array.size_Float:
fun a => a.toList.length

LE.le_Nat:
fun a a_1 => a.le a_1

dite_Float:
fun c [h : Decidable c] t e => Decidable.rec (fun h => e h) (fun h => t h) h

Array.«foldrM_Float_Float_fun type => type_fun x1 x2 => pure (x1 + x2)_Float.ofScientific 0 false 0_0»:
fun as start =>
  dite_Float (LE.le_Nat start as.size_Float)
    (fun h => if 0 < start then Array.foldrM.fold (fun x1 x2 => pure (x1 + x2)) as 0 start h 0 else pure 0) fun h =>
    if 0 < as.size then Array.foldrM.fold (fun x1 x2 => pure (x1 + x2)) as 0 as.size ⋯ 0 else pure 0

Array.«foldr_Float_Float_fun x1 x2 => x1 + x2_Float.ofScientific 0 false 0_0»:
fun as start =>
  as.«foldrM_Float_Float_fun type => type_fun x1 x2 => pure (x1 + x2)_Float.ofScientific 0 false 0_0» start

Array.sum_Float:
fun a => a.«foldr_Float_Float_fun x1 x2 => x1 + x2_Float.ofScientific 0 false 0_0» a.size_Float

Vector.sum_Float_3:
fun xs => xs.toArray_Float_3.sum_Float

Resulting specialization:
  u.sum_Float_3
---
error: code generator does not support recursor `Decidable.rec` yet, consider using 'match ... with' and/or structural recursion
---
error: failed to compile definition, compiler IR check failed at `Array.«foldrM_Float_Float_fun type => type_fun x1 x2 => pure (x1 + x2)_Float.ofScientific 0 false 0_0»`. Error: depends on declaration 'dite_Float', which has no executable code; consider marking definition as 'noncomputable'
---
error: failed to compile definition, compiler IR check failed at `Array.«foldr_Float_Float_fun x1 x2 => x1 + x2_Float.ofScientific 0 false 0_0»`. Error: depends on declaration 'Array.«foldrM_Float_Float_fun type => type_fun x1 x2 => pure (x1 + x2)_Float.ofScientific 0 false 0_0»', which has no executable code; consider marking definition as 'noncomputable'
---
error: failed to compile definition, compiler IR check failed at `Array.sum_Float`. Error: depends on declaration 'Array.«foldr_Float_Float_fun x1 x2 => x1 + x2_Float.ofScientific 0 false 0_0»', which has no executable code; consider marking definition as 'noncomputable'
---
error: failed to compile definition, compiler IR check failed at `Vector.sum_Float_3`. Error: depends on declaration 'Array.sum_Float', which has no executable code; consider marking definition as 'noncomputable'
-/
#guard_msgs in
#opencl_ssimp (u.sum)




/--
info: Resulting specialization:
  #v[10 * ↑0, 10 * ↑1, 10 * ↑2, 10 * ↑3, 10 * ↑4, 10 * ↑5, 10 * ↑6, 10 * ↑7, 10 * ↑8, 10 * ↑9]
-/
#guard_msgs in
#opencl_ssimp (Vector.ofFn (fun i : Fin 10 => 10*i.1))




variable (n : Nat) (i : Fin 3)

/-- error: Expected `n` to be value known at compile time! -/
#guard_msgs in
#opencl_ssimp (Vector.ofFn (fun i : Fin n => 10*i.1))

/--
info: Fin.val_3:
fun self => self.1

Resulting specialization:
  u[i.val_3]
-/
#guard_msgs in
#opencl_ssimp (u[i])
