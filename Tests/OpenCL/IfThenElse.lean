import HouLean.OpenCL.Compiler
import HouLean.OpenCL.Reference
-- import HouLean.OpenCL.Data.Float
import HouLean.OpenCL.Data.Init
-- import HouLean.OpenCL.Data.Int


import HouLean.Meta.RewriteBy

open HouLean Math

namespace Tests.OpenCL.IfThenElse

#exit

/--
info:
double houlean_math_log_d(double x)
{
    return log(x);
}

double (anonymous)(double x)
{
    if (0.0d < x)
    {
        double x1 = houlean_math_log_d(x);
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
double houlean_math_log_d(double x)
{
    return log(x);
}

double (anonymous)(double x)
{
    if (0.0d < x)
    {
        double x1 = houlean_math_log_d(x);
        return x1;
    }
    else
    {
        if (x < 0.0d)
        {
            double x1 = houlean_math_log_d( -x);
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
double houlean_math_log_d(double x)
{
    return log(x);
}

double (anonymous)(double x)
{
    double state = x;
    for (uint i = 0; i < 10; i += 1)
    {
        if (0.0d < state)
        {
            double x1 = houlean_math_log_d(state) + (double)(i);
            if (x1 < 0.0d)
            {
                double x2 = houlean_math_log_d( -x1) - (double)(i);
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
                double x1 = houlean_math_log_d( -state) - (double)(i);
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
