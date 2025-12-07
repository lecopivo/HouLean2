import HouLean.OpenCL.Data.Matrix
import HouLean.OpenCL.Data.Init

open HouLean OpenCL Math

open HoudiniMatrixVecMul

/--
info:
float3 hdiv_hdiv_f3ff3(float3 a, float a1)
{
    return (float3){(a.x / a1), (a.y / a1), (a.y / a1)};
}

matrix33f houlean_matrix_sdiv_f33(matrix33f a, float s)
{
    return (matrix33f){hdiv_hdiv_f3ff3(a.row0, s), hdiv_hdiv_f3ff3(a.row1, s), hdiv_hdiv_f3ff3(a.row2, s)};
}

matrix33f hdiv_hdiv_f33ff33(matrix33f a, float a1)
{
    return houlean_matrix_sdiv_f33(a, a1);
}

matrix33f (anonymous)(float s, matrix33f A)
{
    return hdiv_hdiv_f33ff33(A, s);
}
-/
#guard_msgs in
#opencl_compile (fun (s : Float32) (A : Matrix Float32 3 3) => A / s)

/--
info:
float houlean_math_sin_sin_f(float x)
{
    return sin(x);
}

float3 vector_sin_f3(float3 x)
{
    return (float3){houlean_math_sin_sin_f(x.x), houlean_math_sin_sin_f(x.y), houlean_math_sin_sin_f(x.y)};
}

float vector_dot_f3(float3 u, float3 v)
{
    return ((u.x * v.x) + (u.y * v.y)) + (u.y * v.y);
}

float3 (anonymous)(float3 v)
{
    float3 a = v + vector_sin_f3(v);
    float b = vector_dot_f3(v, v);
    return b * a;
}
-/
#guard_msgs in
#opencl_compile (fun (v : Vector Float32 3) =>
  let a := v + v.sin
  let b := v.dot v
  b * a)

/--
info:
float3 houlean_math_sin_sin_f3(float3 x)
{
    return (float3){houlean_math_sin_sin_f(x.x), houlean_math_sin_sin_f(x.y), houlean_math_sin_sin_f(x.y)};
}

float3 (anonymous)(float3 x)
{
    return houlean_math_sin_sin_f3(x);
}
-/
#guard_msgs in
#opencl_compile Math.sin (α:=Vector Float32 3)


-- #opencl_compile (fun (s : Float32) (v : Vector Float32 3) (A B : Matrix Float32 3 3) => (s * (A - B) / s) * A * v)


#exit
-- attribute [opencl_csimp]
--   Vector.dot Matrix.col Matrix.matMul Matrix.mulVec Matrix.sdiv Matrix.smul Matrix.sub

set_option trace.HouLean.OpenCL.compiler true in
#opencl_compile (fun (s : Float32) (v : Vector Float32 3) (A B : Matrix Float32 3 3) => ((s * (A - B) / s) * A) * v)
-- #opencl_compile (fun (s : Int32) (v : Vector Int32 3) (A B : Matrix Int32 3 3) => ((s * (A - B) / s) * A) * v)

-- #eval
--  IO.println ((OpenCLType.definition? (Matrix Float64 4 8)).getD "")

variable [Add α] [Mul α] [Div α] [Zero α] [Sqrt α] [ApproxEqual α] [AtomicOpenCLType α] [Inhabited α]

instance [inst : OpenCLType α] : OpenCLType (OpenCLM α) where
  name := inst.name
  shortName := inst.shortName

instance [ty : AtomicOpenCLType α] : OpenCLType (Pointer α) where
  name := ty.name ++ "*"
  shortName := "p" ++ ty.shortName

instance : OpenCLType Unit where
  name := "void"
  shortName := "v"


macro "*" ptr:term " := " val:term : doElem => `(doElem| Pointer.set $ptr 0 $val)

def Vector.OpenCL.normalize {r t} (u : Vector α n) (norm : Pointer α (const:=false) r t) : OpenCLM (Vector α n) := do
  let len := u.length
  if len ≈ 0 then
    *norm := 0
    return u
  else
    *norm := len
    return u / len

def Vector.OpenCL.normalize'(u : Vector α n) (norm : Pointer α) : OpenCLM (Vector α n) := do
  let (u,len) := u.normalize
  norm.set 0 len
  return u


set_option trace.HouLean.OpenCL.compiler true in
#opencl_compile (fun (u : Vector Float 4) (norm : Pointer Float) => Vector.OpenCL.normalize u norm)


def asdf (v : Vector Float 3) :=
  let (u,len) := v.normalize
  u.sum + len

instance : Inhabited (Pointer Float) := sorry

def asdf' (v : Vector Float 3) := do
  let len : Pointer Float := oclFunction (type:=Pointer Float) ""
  let u ← Vector.OpenCL.normalize v len
  let len ← len.toConst.get 0
  return u.sum + len

def asdf'' (v : Vector Float 3) := do
  let len : Pointer Float := oclFunction (type:=Pointer Float) ""
  let u ← Vector.OpenCL.normalize v len
  let len ← len.toConst.get 0
  return u.sum + len

set_option trace.HouLean.OpenCL.compiler true in
#opencl_compile asdf''
