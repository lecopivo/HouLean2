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
    return ((u.x * v.x) + (u.y * v.y)) + (u.z * v.z);
}

double (anonymous)(double3 x, double3 y)
{
    return vector_dot_d3(x, y);
}
-/
#guard_msgs in
#opencl_compile (fun x y : Vector Float 3 => x.dot y)

/--
info:
double houlean_math_dot_d3d(double3 x, double3 y)
{
    return vector_dot_d3(x, y);
}

double (anonymous)(double3 x, double3 y)
{
    return houlean_math_dot_d3d(x, y);
}
-/
#guard_msgs in
#opencl_compile (fun x y : Vector Float 3 => dot x y)


/--
info:
double houlean_math_sqrt_d(double x)
{
    return sqrt(x);
}

double vector_length2_d3(double3 u)
{
    return ((u.x * u.x) + (u.y * u.y)) + (u.z * u.z);
}

double vector_length_d3(double3 u)
{
    return houlean_math_sqrt_d(vector_length2_d3(u));
}

bool houlean_math_approxequal_d(double x, double y, double tol)
{
    return fabs(x - y) <= tol;
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

prodd3d vector_normalize_d3(double3 u)
{
    double len = vector_length_d3(u);
    if (houlean_math_approxequal_d(len, 0.0d, 1e-9d))
    {
        return (prodd3d){u, 0.0d};
    }
    else
    {
        return (prodd3d){hdiv_d3dd3(u, len), len};
    }
}

prodd3d (anonymous)(double3 x)
{
    return vector_normalize_d3(x);
}
-/
#guard_msgs in
#opencl_compile (fun x : Vector Float 3 => x.normalize)
