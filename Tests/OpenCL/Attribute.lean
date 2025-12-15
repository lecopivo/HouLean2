#exit
import HouLean.OpenCL.Data.Attribute
import HouLean.OpenCL.Data.Volume
import HouLean.OpenCL.Data.Int
import HouLean.OpenCL.WorkItemFunctions

open HouLean OpenCL

namespace Test.OpenCL.Attribute

-- use native decide to prove element indices for trivial inequalities like `0<4∧1<4`
macro_rules | `(tactic| get_elem_tactic_extensible) => `(tactic| sorry_proof)


set_option pp.proofs false
def kernel1
  (res) (frame : Frame Float32 3)
  (surface : Volume Float32 res frame)
  (mass    : Volume Float32 res frame (write:=true))
  : OpenCLM Unit := do

  -- let mut x := x
  for i in [0:res.x] do
  for j in [0:res.y] do
  for k in [0:res.z] do
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

void test_opencl_attribute_kernel1(uint3 res, framef3 frame, volumedataf surface, volumedataf mass)
{
    for (uint i = 0; i < vector_x_ui3(res); i += 1)
    {
        for (uint j = 0; j < vector_y_ui3(res); j += 1)
        {
            for (uint k = 0; k < vector_z_ui3(res); k += 1)
            {
                float sdf = getelem_voldatafpuipuiuif(surface, (prod_ui_puiui){i, (prod_ui_ui){j, k}});
                float m = getelem_voldatafpuipuiuif(mass, (prod_ui_puiui){i, (prod_ui_ui){j, k}});
                if (sdf < 0.0f)
                {
                    houlean_setelemm_voldatafpuipuiuif(mass, (prod_ui_puiui){i, (prod_ui_ui){j, k}}, 10.0f * m);
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

void (anonymous)(uint3 res, framef3 frame, volumedataf surface, volumedataf mass)
{
    return test_opencl_attribute_kernel1(res, frame, surface, mass);
}
-/
#guard_msgs in
#opencl_compile kernel1



def kernel2
    (npoints res) (frame : Frame Float 3)
    (P   : Attribute .point (Vector Float 3) npoints (write:=true))
    (surface : Volume Float res frame (input:=1))
    (maxiter : Nat)
    : OpenCLM Unit := do

  let idx := (← getGlobalId 0).toNat

  let mut p ← P[idx]

  for i in [0:maxiter] do
    let (dist, grad) ← surface.sampleAndGradient p
    let (normal, gradLength) := grad.normalize
    if 0 < dist || gradLength ≈ 0 then
      break
    p -= dist * normal

  P[idx] ← p


-- set_option trace.HouLean.OpenCL.compiler true in
-- #opencl_compile kernel2
