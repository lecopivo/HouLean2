import HouLean.OpenCL.Compiler.Main
import HouLean.OpenCL.Data.Float
import HouLean.OpenCL.Data.Init
import HouLean.OpenCL.Data.Int

open HouLean Math

namespace Tests.OpenCL.IfThenElse

/--
info:
double (anonymous)(double x)
{
    if (0.0d < x)
    {
        double x1 = log(x);
        return x1;
    }
    else
    {
        return x;
    }
}
-/
#guard_msgs in
#opencl_compile (fun x : Float => Id.run do
  let mut x := x
  if 0 < x then
    x := log x
  return x)


/--
info:
double (anonymous)(double x)
{
    if (0.0d < x)
    {
        double x1 = log(x);
        return x1;
    }
    else
    {
        if (x < 0.0d)
        {
            double x1 = log( -x);
            return x1;
        }
        else
        {
            return x;
        }
    }
}
-/
#guard_msgs in
#opencl_compile (fun x : Float => Id.run do
  let mut x := x
  if 0 < x then
    x := log x
    return x
  if x < 0 then
    x := log (-x)
  return x)


/--
info:
double (anonymous)(double x)
{
    double state = x;
    for (uint i = 0; i < 10; i += 1)
    {
        if (0.0d < state)
        {
            double x1 = log(state) + (double)(i);
            if (x1 < 0.0d)
            {
                double x2 = log( -x1) - (double)(i);
                state = x2;
            }
            else
            {
                state = x1;
            }
        }
        else
        {
            if (state < 0.0d)
            {
                double x1 = log( -state) - (double)(i);
                state = x1;
            }
            else
            {
                state = state;
            }
        }
    }
    double r = state;
    return r;
}
-/
#guard_msgs in
#opencl_compile (fun x : Float => Id.run do
  let mut x := x
  for i in [0:10] do
    if 0 < x then
      x := log x + i.toFloat
    if x < 0 then
      x := log (-x) - i.toFloat
  return x)
