import HouLean.OpenCL.Compiler.RewriteRules
import HouLean.Data.Vector

open HouLean OpenCL Compiler Math

namespace HouLean.OpenCL

variable {R} [FloatType R] {n} [AllowedVectorSize n] (x y z w : R) (u v : Vector R n)

impl_by (u v : Vector R 3) : u.cross3 v ==> cross(u, v)
impl_by : distance u v ==> distance(u, v)
impl_by : dot u v ==> distance(u, v)
impl_by : length u ==> length(u)
impl_by : normalized u ==> normalize(u)
-- impl_by : fast_distance u v ==> fast_distance(u,v)
-- impl_by : fast_length u v ==> fast_length(u,v)
-- impl_by : fast_normalize u ==> fast_normalize(u)
