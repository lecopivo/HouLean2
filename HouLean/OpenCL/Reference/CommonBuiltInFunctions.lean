import HouLean.OpenCL.Compiler.RewriteRules
import HouLean.Data.Float
import HouLean.Data.Vector

open HouLean OpenCL Compiler Math

namespace HouLean.OpenCL

variable {R} [FloatType R] {n} [AllowedVectorSize n] (x y z w : R) (u v : Vector R n)


impl_by (lo hi : R) : clamp x lo hi ==> clamp(x, lo, hi)
impl_by (lo hi : R) : clamp u lo hi ==> clamp(u, lo, hi)
-- impl_by : degrees x ==> degrees(x)
-- impl_by : max x y ==> max(x,y)  -- already defined in Math Built-in Function
-- impl_by : max u y ==> max(u,y)  -- `Max.max` is not heterogenous in Lean
-- impl_by : min x y ==> min(x,y)  -- already defined in Math Built-in Function
-- impl_by : min u y ==> min(u,y)  -- `Min.min` is not heterogenous in Lean
impl_by : lerp x y w ==> mix(x, y, w)
impl_by : lerp u v w ==> mix(u, v, w)
-- impl_by : radians x ==> radians(x)
impl_by (edge : R) : step x edge ==> step(edge, x)
impl_by (edge : Vector R n) : step u edge ==> step(edge, u)
impl_by (edge0 edge1 : R) : smoothstep x edge0 edge1 ==> smoothstep(edge0, edge1, x)
impl_by (edge0 edge1 : Vector R n) : smoothstep u edge0 edge1 ==> smoothstep(edge0, edge1, x)
impl_by : sign x ==> sign(x)
