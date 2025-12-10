import HouLean.OpenCL.Compiler.Main
import HouLean.OpenCL.Data.Int
import HouLean.OpenCL.Data.Vector
import HouLean.OpenCL.Data.MProd
import HouLean.OpenCL.Data.Prod
import HouLean.OpenCL.Data.ArrayType
import HouLean.Meta.DoNotation

open HouLean OpenCL

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



def foo (res : Vector Nat 3) (idx : Vector Nat 3) (mass : ArrayPointer Float) (vel : ArrayPointer (Vector Float 3)) := do
  let mut m := 0.0
  let mut p := #v[0.0,0,0]
  for i in [0:3] do
    for j in [0:3] do
      for k in [0:3] do
        let linIdx := (idx.x + i) + res.x * (idx.y + j) + res.x * res.y * (idx.z + k) |>.toUInt64
        let mi ← mass[linIdx]
        let vi ← vel[linIdx]
        m += mi
        p += mi * vi
  return (m, p / m)


/--
info:
uint vector_x_ui3(uint3 a)
{
    return a.x;
}

uint vector_y_ui3(uint3 a)
{
    return a.y;
}

uint vector_z_ui3(uint3 a)
{
    return a.z;
}

double houlean_opencl_arraytype_get_dpd(double * a, ulong a1)
{
    return a[a1];
}

double getelem_pduld(double * xs, ulong i)
{
    return houlean_opencl_arraytype_get_dpd(xs, i);
}

double3 houlean_opencl_arraytype_get_d3pd(double * a, ulong a1)
{
    return vload3(a1, a);
}

double3 getelem_pduld3(double * xs, ulong i)
{
    return houlean_opencl_arraytype_get_d3pd(xs, i);
}

double inv_d(double a)
{
    return 1.0d / a;
}

double3 hdiv_d3dd3(double3 a, double a1)
{
    double is = inv_d(a1);
    return (double3){is * a.x, is * a.y, is * a.z};
}

prod_d_d3 tests_opencl_forloop_foo(uint3 res, uint3 idx, double * mass, double * vel)
{
    double m = 0.0d;
    double3 p = (double3){0.0d, 0.0d, 0.0d};
    mprod_d_d3 state = (mprod_d_d3){m, p};
    for (uint i = 0; i < 3; i += 1)
    {
        double m1 = state.fst;
        double3 p1 = state.snd;
        mprod_d_d3 state1 = (mprod_d_d3){m1, p1};
        for (uint j = 0; j < 3; j += 1)
        {
            double m2 = state1.fst;
            double3 p2 = state1.snd;
            mprod_d_d3 state2 = (mprod_d_d3){m2, p2};
            for (uint k = 0; k < 3; k += 1)
            {
                double m3 = state2.fst;
                double3 p3 = state2.snd;
                ulong linIdx = (ulong)(((vector_x_ui3(idx) + i) + (vector_x_ui3(res) * (vector_y_ui3(idx) + j))) + ((vector_x_ui3(res) * vector_y_ui3(res)) * (vector_z_ui3(idx) + k)));
                double mi = getelem_pduld(mass, linIdx);
                double3 vi = getelem_pduld3(vel, linIdx);
                double m4 = m3 + mi;
                double3 p4 = p3 + (mi * vi);
                state2 = (mprod_d_d3){m4, p4};
            }
            mprod_d_d3 r = state2;
            state1 = (mprod_d_d3){r.fst, r.snd};
        }
        mprod_d_d3 r = state1;
        state = (mprod_d_d3){r.fst, r.snd};
    }
    mprod_d_d3 r = state;
    return (prod_d_d3){r.fst, hdiv_d3dd3(r.snd, r.fst)};
}

prod_d_d3 (anonymous)(uint3 res, uint3 idx, double * mass, double * vel)
{
    return tests_opencl_forloop_foo(res, idx, mass, vel);
}
-/
#guard_msgs in
#opencl_compile foo
