import HouLean.OpenCL.Compiler.Main
import HouLean.OpenCL.Data.Int
import HouLean.OpenCL.Data.Vector
import HouLean.OpenCL.Data.MProd
import HouLean.OpenCL.Data.Prod

open HouLean.OpenCL

namespace Tests.OpenCL.ForLoop

/--
info:
double3 (anonymous)(double a)
{
    double state = a;
    for (uint i = 0; i < 10; i += 2)
    {
        double x = (state * state) * (double)(i);
        double x1 = x + x;
        state = x1;
    }
    double r = state;
    return (double3){r + r, r * r, 5.0d * r};
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
info:
double3 (anonymous)(double a)
{
    double state = a;
    for (uint i = 0; i < 10; i += 2)
    {
        double state1 = state;
        for (uint j = 5; j < 20; j += 1)
        {
            double x = ((state1 * state1) * (double)(i)) * (double)(j);
            double x1 = x + x;
            state1 = x1;
        }
        double r = state1;
        state = r;
    }
    double r = state;
    return (double3){r + r, r * r, 5.0d * r};
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
info:
double tests_opencl_forloop_fib_d(uint n)
{
    double a = 0.0d;
    double b = 1.0d;
    mprod_d_d state = (mprod_d_d){a, b};
    for (uint x = 0; x < n; x += 1)
    {
        double a1 = state.fst;
        double b1 = state.snd;
        double b2 = a1 + b1;
        state = (mprod_d_d){b1, b2};
    }
    mprod_d_d r = state;
    return r.snd;
}

double (anonymous)(uint n)
{
    return tests_opencl_forloop_fib_d(n);
}
-/
#guard_msgs in
#opencl_compile fib (α:=Float)
