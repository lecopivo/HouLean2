import HouLean.OpenCL.Data.Matrix
import HouLean.OpenCL.Data.Prod
import HouLean.OpenCL.Data.Init

open HouLean OpenCL Math

open HoudiniMatrixVecMul

namespace Tests.OpenCL.Vector


/--
info:
double vector_dot_d3(double3 u, double3 v)
{
    return ((u.x * v.x) + (u.y * v.y)) + (u.y * v.y);
}

double (anonymous)(double3 x, double3 y)
{
    return vector_dot_d3(x, y);
}
-/
#guard_msgs in
#opencl_compile (fun x y : Vector Float 3 => x.dot y)

-- todo: where is `vector_dot_d3`? ... it seems to be a problem with `defun dot ...` as it does not use
--       `Vector.dot` in the instance
/--
info:
double (anonymous)(double3 x, double3 y)
{
    return ((x.x * y.x) + (x.y * y.y)) + (x.y * y.y);
}
-/
#guard_msgs in
#opencl_compile (fun x y : Vector Float 3 => dot x y)


/--
info:
double vector_length2_d3(double3 u)
{
    return ((u.x * u.x) + (u.y * u.y)) + (u.y * u.y);
}

double vector_length_d3(double3 u)
{
    return sqrt(vector_length2_d3(u));
}

bool houlean_math_approxequal_approxequal_d(double x, double y, double tol)
{
    return fabs(x - y) <= tol;
}

double3 hdiv_hdiv_d3dd3(double3 a, double a1)
{
    return (double3){a.x / a1, a.y / a1, a.y / a1};
}

prodd3d vector_normalize_d3(double3 u)
{
    double len = vector_length_d3(u);
    if (houlean_math_approxequal_approxequal_d(len, 0.0d, 1e-9d))
    {
        return (prodd3d){u, 0.0d};
    }
    else
    {
        return (prodd3d){hdiv_hdiv_d3dd3(u, len), len};
    }
}

prodd3d (anonymous)(double3 x)
{
    return vector_normalize_d3(x);
}
-/
#guard_msgs in
#opencl_compile (fun x : Vector Float 3 => x.normalize)
