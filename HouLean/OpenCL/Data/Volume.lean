import HouLean.OpenCL.Data.ArrayType
import HouLean.OpenCL.Data.Fin
import HouLean.OpenCL.Data.Vector
import HouLean.OpenCL.Data.Matrix
import HouLean.OpenCL.Data.Int
import HouLean.OpenCL.Data.Prod
import HouLean.Data.Vector
import HouLean.Meta.DoNotation

namespace HouLean.OpenCL


structure Frame (R : Type) (dim : Nat) where
  toWorldMatrix : Matrix R (dim+1) (dim+1)
  toFrameMatrix : Matrix R (dim+1) (dim+1)
deriving Inhabited

instance [t : OpenCLType R] : OpenCLType (Frame R dim) where
  name := s!"frame{t.shortName}{dim}"
  shortName := s!"frame{t.shortName}{dim}"
  definition? := s!"structure frame{t.shortName}{dim} \{ ... }"

implemented_by [Inhabited R] (f : Frame R dim) :
  f.toWorldMatrix = oclFunction (_ → _) ".toWorldMatrix" .postfix f

implemented_by [Inhabited R] (f : Frame R dim) :
  f.toWorldMatrix = oclFunction (_ → _) ".toFrameMatrix" .postfix f

set_option linter.unusedVariables false in
/-- Vector living in a particular frame -/
def FVector {dim} (R : Type) (frame : Frame R dim) := Vector R dim

-- make this dimension independent?
structure VolumeData (type : Type) {Ptr} [ArrayType type Ptr] where
  stride : Vector Nat 3
  offset : Nat
  ptr : ArrayPointer type

instance [t : OpenCLType type] {Ptr} [ArrayType type Ptr] : OpenCLType (VolumeData type) where
  name := s!"volumedata{t.shortName}"
  shortName := s!"voldata{t.shortName}"

implemented_by {type : Type} {Ptr} [ArrayType type Ptr] (data : VolumeData type) :
  data.stride = oclFunction (_ → _) ".stride" .postfix data

implemented_by {type : Type} {Ptr} [ArrayType type Ptr] (data : VolumeData type) :
  data.offset = oclFunction (_ → _) ".offset" .postfix data

implemented_by {type : Type} {Ptr} [ArrayType type Ptr] [Inhabited Ptr] (data : VolumeData type) :
  data.ptr = oclFunction (_ → _) ".ptr" .postfix data

set_option linter.unusedVariables false in
abbrev Volume (type : Type) (res : Vector Nat 3) (frame : Frame R 3) (name : String := "")
    (input : Nat := 0) (read := true) (write := false) {Ptr} [ArrayType type Ptr] :=
  VolumeData type

variable {R} [FloatType R]
  {type : Type} {res : Vector Nat 3} {frame : Frame R 3} {name : String} {input : Nat}
  {Ptr} [ArrayType type Ptr]

instance {write} :
    GetElem (Volume type res frame name input (read:=true) write) (Nat×Nat×Nat) (OpenCLM type)
      (fun _ i => i.1 < res.x ∧ i.2.1 < res.y ∧ i.2.2 < res.z) where
  getElem vol i _ := do
    let (ix,iy,iz) := i
    let idx := vol.offset + vol.stride.x * ix + vol.stride.y * iy + vol.stride.z * iz
    getElem vol.ptr idx .intro

instance {write} :
    SetElemM (Volume type res frame name input (read:=true) write)
      (Nat×Nat×Nat) type OpenCLM
      (fun _ i => i.1 < res.x ∧ i.2.1 < res.y ∧ i.2.2 < res.z) where
  setElemM vol i x _ := do
    let (ix,iy,iz) := i
    let idx := vol.offset + vol.stride.x * ix + vol.stride.y * iy + vol.stride.z * iz
    setElemM vol.ptr idx x .intro


namespace Volume

class Interpolate (Idx Dom Val : Type) (Val' : outParam Type) where
  interpolate : (Idx → Val) → (Dom → Val')

export Interpolate (interpolate)

instance : Interpolate Int Float Float Float where
  interpolate data x :=
    let xi := x.floor
    let w := x - xi
    let i := xi.toInt64.toInt
    Math.lerp (data i) (data (i+1)) w

instance [Interpolate Idx Dom Float Float] : Interpolate (Int×Idx) (Float×Dom) Float Float where
  interpolate data x :=
    let (x0, x1) := x
    interpolate (fun i => interpolate (fun idx => data (i, idx)) x1) x0

    -- interpolate (fun idx => interpolate (fun i => data (i, idx)) x0) x1

end Volume

-- use native decide to prove element indices for trivial inequalities like `0<4∧1<4`
macro_rules | `(tactic| get_elem_tactic_extensible) => `(tactic| sorry_proof)

set_option linter.unusedVariables false

set_option pp.proofs false
def kernel
  (res) (frame : Frame Float32 3)
  (surface : Volume Float32 res frame)
  (mass    : Volume Float32 res frame (write:=true))
  (x : Float32)
  (i : Nat×Nat×Nat)
  : OpenCLM Unit := do


  -- let mut x := x
  for i in [0:res.x] do
  for j in [0:res.y] do
  for k in [0:res.z] do
  -- --   -- have : i < res.x := by get_elem_tactic
  -- --   -- have : j < res.y := by get_elem_tactic
  -- --   -- have : k < res.z := by get_elem_tactic
    let sdf ← surface[i,j,k]
    let m ← mass[i,j,k]

    if sdf < 0 then
      mass[i,j,k] ← 10*m


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

float houlean_opencl_arraytype_get_fpf(float * a, ulong a1)
{
    return a[a1];
}

float getelem_pfuif(float * xs, uint i)
{
    return houlean_opencl_arraytype_get_fpf(xs, (ulong)(i));
}

float getelem_voldatafpuipuiuif(volumedataf xs, prod_ui_puiui i)
{
    uint idx = ((xs.offset + (vector_x_ui3(xs.stride) * i.fst)) + (vector_y_ui3(xs.stride) * i.snd.fst)) + (vector_z_ui3(xs.stride) * i.snd.snd);
    return getelem_pfuif(xs.ptr, idx);
}

float getelem_voldatafpuipuiuif(volumedataf xs, prod_ui_puiui i)
{
    uint idx = ((xs.offset + (vector_x_ui3(xs.stride) * i.fst)) + (vector_y_ui3(xs.stride) * i.snd.fst)) + (vector_z_ui3(xs.stride) * i.snd.snd);
    return getelem_pfuif(xs.ptr, idx);
}

void houlean_opencl_arraytype_set_fpf(float * a, ulong a1, float a2)
{
    return a[a1] = a2;
}

void houlean_setelemm_pfuif(float * ptr, uint i, float a)
{
    return houlean_opencl_arraytype_set_fpf(ptr, (ulong)(i), a);
}

void houlean_setelemm_voldatafpuipuiuif(volumedataf ptr, prod_ui_puiui i, float a)
{
    uint idx = ((ptr.offset + (vector_x_ui3(ptr.stride) * i.fst)) + (vector_y_ui3(ptr.stride) * i.snd.fst)) + (vector_z_ui3(ptr.stride) * i.snd.snd);
    return houlean_setelemm_pfuif(ptr.ptr, idx, a);
}

void houlean_opencl_kernel(uint3 res, framef3 frame, volumedataf surface, volumedataf mass, float x, prod_ui_puiui i)
{
    for (uint i1 = 0; i1 < vector_x_ui3(res); i1 += 1)
    {
        for (uint j = 0; j < vector_y_ui3(res); j += 1)
        {
            for (uint k = 0; k < vector_z_ui3(res); k += 1)
            {
                float sdf = getelem_voldatafpuipuiuif(surface, (prod_ui_puiui){i1, (prod_ui_ui){j, k}});
                float m = getelem_voldatafpuipuiuif(mass, (prod_ui_puiui){i1, (prod_ui_ui){j, k}});
                if (sdf < 0.0f)
                {
                    houlean_setelemm_voldatafpuipuiuif(mass, (prod_ui_puiui){i1, (prod_ui_ui){j, k}}, 10.0f * m);
                }
                else
                {

                }
            }
            ;
        }
        ;
    }
    ;
    return ;
}

void (anonymous)(uint3 res, framef3 frame, volumedataf surface, volumedataf mass, float x, prod_ui_puiui i)
{
    return houlean_opencl_kernel(res, frame, surface, mass, x, i);
}
-/
#guard_msgs in
#opencl_compile kernel




-- def kernel
--   {npoints res} {frame : Frame Float32 3}
--   (P   : Attribute .point (Vector Float32 3) npoints (write:=true))
--   (vel : Attribute .point (Vector Float32 3) npoints (write:=true))
--   (surface : Volume Float32 res frame (input:=1))
--   (mass    : Volume Float32 res frame (input:=1))
--   (maxiter : Nat)
--   : OpenCLM Unit := do

--   let idx := (← getGlobalId 0).toNat
--   if h : ¬(idx < npoints) then
--     return
--   else

--   let mut p ← P[idx]

--   for i in [0:maxiter] do
--     let (dist, grad) ← surface.sampleAndGradient p
--     let (normal, gradLength) := grad.normalize
--     if 0 < dist || gradLength ≈ 0 then
--       break
--     p -= dist * normal

--   P[idx] ← p
