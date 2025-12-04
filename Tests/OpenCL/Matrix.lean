import HouLean.OpenCL.Data.Matrix

open HouLean OpenCL Math

open HoudiniMatrixVecMul

-- attribute [opencl_csimp]
--   Vector.dot Matrix.col Matrix.matMul Matrix.mulVec Matrix.sdiv Matrix.smul Matrix.sub

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
