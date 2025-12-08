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

double houlean_math_abs_d(double x)
{
    return fabs(x);
}

bool houlean_math_approxequal_dd(double x, double y, double tol)
{
    return houlean_math_abs_d(x - y) <= tol;
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
    if (houlean_math_approxequal_dd(len, 0.0d, 1e-6d))
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


/--
info:
double3 vector_normalized_d3(double3 u)
{
    return vector_normalize_d3(u).fst;
}

double houlean_math_clamp_dd(double x, double lo, double hi)
{
    if (x < lo)
    {
        return lo;
    }
    else
    {
        if (hi < x)
        {
            return hi;
        }
        else
        {
            return x;
        }
    }
}

double houlean_math_acos_d(double x)
{
    return acos(x);
}

double3 vector_lerp_d3(double3 x, double3 y, double t)
{
    return x + (t * (y - x));
}

double3 houlean_math_lerp_d3d(double3 x, double3 y, double t)
{
    return vector_lerp_d3(x, y, t);
}

double houlean_math_sin_d(double x)
{
    return sin(x);
}

double3 vector_slerp_d3(double3 v, double3 w, double t)
{
    double d = vector_dot_d3(vector_normalized_d3(v), vector_normalized_d3(w));
    double d1 = houlean_math_clamp_dd(d, -1.0d, 1.0d);
    double theta = houlean_math_acos_d(d1);
    if (houlean_math_approxequal_dd(theta, 0.0d, 1e-6d))
    {
        return houlean_math_lerp_d3d(v, w, t);
    }
    else
    {
        double s = houlean_math_sin_d(theta);
        double a = houlean_math_sin_d((1.0d - t) * theta) / s;
        double b = houlean_math_sin_d(t * theta) / s;
        return (a * v) + (b * w);
    }
}

double3 (anonymous)(double3 x, double3 y, double t)
{
    return vector_slerp_d3(x, y, t);
}
-/
#guard_msgs in
#opencl_compile (fun (x y : Vector Float 3) (t : Float) => x.slerp y t)



/--
info:
double3 houlean_math_clamp_d3d3(double3 x, double3 lo, double3 hi)
{
    return (double3){houlean_math_clamp_dd(x.x, lo.x, hi.x), houlean_math_clamp_dd(x.y, lo.y, hi.y), houlean_math_clamp_dd(x.z, lo.z, hi.z)};
}

double3 (anonymous)(double3 x, double3 bbmin, double3 bbmax)
{
    return houlean_math_clamp_d3d3(x, bbmin, bbmax);
}
-/
#guard_msgs in
#opencl_compile (fun x bbmin bbmax : Vector Float 3 => clamp x bbmin bbmax)
