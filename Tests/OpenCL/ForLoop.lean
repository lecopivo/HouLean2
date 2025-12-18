import HouLean.OpenCL.Compiler
import HouLean.OpenCL.Data.Init
import HouLean.OpenCL.Data.Vector
import HouLean.OpenCL.Data.ArrayType
import HouLean.Meta.DoNotation
import HouLean.OpenCL.Reference

open HouLean OpenCL

namespace Tests.OpenCL.ForLoop

attribute [opencl_csimp] Id.run
impl_by {α : Type} (a : α) : ForInStep.yield a ==> a



/--
info: double3 main(double a){
      const double x = a;
      double state = x;
      for (uint✝ i = 0;i < 10; i += 2)
        {
              const double x1 = state;
              const double x2 = x1 * x1 * (double)(i);
              const double x3 = x2 + x2;
              state = x3;
        }
      const double x1 = state;
      return (double3){x1 + x1, x1 * x1, 5.0 * x1};
}
-/
#guard_msgs in
#opencl_compile (fun a : Float =>
  Id.run do
    let mut x : Float := a
    for i in [0:10:2] do
      x := x * x * i.toFloat
      x := x+x
    #v[x + x, x*x, 5*x])


/--
info: double3 main(double a){
      const double x = a;
      double state = x;
      for (uint✝ i = 0;i < 10; i += 2)
        {
              const double x1 = state;
              double state1 = x1;
              for (uint✝ j = 5;j < 20; j += 1)
                {
                      const double x2 = state1;
                      const double x3 = x2 * x2 * (double)(i) * (double)(j);
                      const double x4 = x3 + x3;
                      state1 = x4;
                }
              const double x2 = state1;
              state = x2;
        }
      const double x1 = state;
      return (double3){x1 + x1, x1 * x1, 5.0 * x1};
}
-/
#guard_msgs in
#opencl_compile (fun a : Float =>
  Id.run do
    let mut x : Float := a
    for i in [0:10:2] do
      for j in [5:20] do
        x := x * x * i.toFloat * j.toFloat
        x := x+x
    #v[x + x, x*x, 5*x])



def fib {α} [Add α] [One α] [Zero α] (n : Nat) := Id.run do
  let mut a := (0 : α)
  let mut b := (1 : α)
  for _ in [0:n] do
    let tmp := b
    b := a + b
    a := tmp
  return b


/--
info: double tests_opencl_forloop_fib_float(uint n){
      const double a = 0.0;
      const double b = 1.0;
      MProd_double_double state = (MProd_double_double){a, b};
      for (uint✝ x = 0;x < n; x += 1)
        {
              const double a1 = state.fst;
              const double b1 = state.snd;
              const double tmp = b1;
              const double b2 = a1 + b1;
              const double a2 = tmp;
              state = (MProd_double_double){a2, b2};
        }
      return state.snd;
}

double main(uint n){
      return tests_opencl_forloop_fib_float(n);
}
-/
#guard_msgs in
#opencl_compile fib (α:=Float)


def foo (res : Vector Nat 3) (idx : Vector Nat 3) (mass : ArrayPointer Float) (vel : ArrayPointer (Vector Float 3)) := do
  let mut m := 0.0
  let mut p := #v[0.0,0,0]
  for i in [0:3] do
    for j in [0:3] do
      for k in [0:3] do
        let linIdx := (idx.x + i) + res.x * (idx.y + j) + res.x * res.y * (idx.z + k)
        let mi ← mass[linIdx]
        let vi ← vel[linIdx]
        m += mi
        p += mi * vi
  return (m, p / m)

/-- error: Don't know how to compile type: Pointer Float -/
#guard_msgs in
#opencl_compile foo
