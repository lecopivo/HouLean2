import HouLean.OpenCL.Data.Matrix
import HouLean.OpenCL.Data.Pointer
import HouLean.OpenCL.Data.Attribute
-- import HouLean.OpenCL.Data.ArrayRef
import HouLean.OpenCL.Data.NanoVDB
-- import HouLean.OpenCL.Data.Unit
-- import HouLean.OpenCL.Data.Int
-- import HouLean.OpenCL.Data.Bool
-- import HouLean.OpenCL.WorkItemFunctions
-- import HouLean.OpenCL.Data.ArrayType

open HouLean OpenCL

namespace Test.OpenCL.MPM

def getGridIdx (tilestartsxyx : DPointer (Vector Int32 3)) : OpenCLM (Vector Int32 3) := do
  let idx ← getGlobalId 0

  let tileoffset := idx &&& 511
  let tileidx := idx >>> 9
  let tilebasexyz ← tilestartsxyx.get tileidx

  let gidx := tilebasexyz[0] + (tileoffset &&& 7).toUInt32.toInt32
  let gidy := tilebasexyz[1] + ((tileoffset >>> 3) &&& 7).toUInt32.toInt32
  let gidz := tilebasexyz[2] + ((tileoffset >>> 6) &&& 7).toUInt32.toInt32

  return #v[gidx, gidy, gidz]

-- #opencl_compile getGridIdx

noncomputable
def clearSurfaces
    (tilestartsxyx : DPointer (Vector Int32 3)) -- missing global, const, restric
    (surface_grid : NanoVDB Float32)       -- missing global, restric
    (surface_grid_out : NanoVDB Float32)   -- missing global, restric
    : OpenCLM Unit := do

  let surface_grid_acc ← surface_grid.getReadAccessor'
  let surface_grid_out_acc ← surface_grid_out.getReadAccessor'

  let g_coord ← getGridIdx tilestartsxyx
  let active ← surface_grid_acc.isActive g_coord
  if !active then
    return

  surface_grid_acc.set g_coord 0
  surface_grid_out_acc.set g_coord 0
  return

-- #opencl_compile clearSurfaces

noncomputable
def clearGrids
    (tilestartsxyx : DPointer (Vector Int32 3))  -- missing global, const, restric
    (mass_grid : NanoVDB Float32)                    -- missing global, restric
    (vel_grid : NanoVDB (Vector Float32 3))      -- missing global, restric
    : OpenCLM Unit := do

  let mass_grid_acc ← mass_grid.getReadAccessor'
  let vel_grid_acc ← vel_grid.getReadAccessor'

  let g_coord ← getGridIdx tilestartsxyx
  if !(← mass_grid_acc.isActive g_coord) then
    return

  mass_grid_acc.set g_coord 0
  vel_grid_acc.set g_coord (0 : Vector Float32 3)
  return

-- #opencl_compile clearGrids



def deformationGradientUpdate
    (F C : Matrix Float32 3 3) (dt : Float32) : Matrix Float32 3 3 :=
  let I := Matrix.identity Float32 3
  F * (I + dt * C)

-- #opencl_compile deformationGradientUpdate

open Math

def getLameParameters
    (Jp : Float32) (type : Int32) (hardening E nu : Float32) :
    Float32 × Float32 :=

  let lambda := E * nu / ((1.0 + nu) * (1.0 - 2.0 * nu))
  let nu := E / (2.0 * (1.0 + nu))

  if type == 3 && hardening > 0 then
    let h := clamp (exp (hardening * (1.0 - Jp))) (0.1:Float32) 5.0
    (h * lambda, h * nu)
  else
    (lambda, nu)


set_option linter.unusedVariables false

-- #ifdef HAS_global_airresist
--         int global_airresist_length,
--         global float* restrict global_airresist,
-- #endif
-- #ifdef HAS_global_targetv
--         int global_targetv_length,
--         global float* restrict global_targetv,
-- #endif
--         int particlesep_length,
--         global float* restrict particlesep)

def intergrateForce {npts}
    (dt : Float32)
    (P : Attribute .point (Vector Float32 3) npts)
    (v : Attribute .point (Vector Float32 3) npts)
    (density: Attribute .point Float32 npts)
    (targetv? : Option (Attribute .point (Vector Float32 3) npts))
    (airresist? : Option (Attribute .point Float32 npts))
    (force? : Option (Attribute .point (Vector Float32 3) npts))
    (age? : Option (Attribute .point Float32 npts))
    (global_airresist? : Option Float32)
    (global_targetv? : Option (Vector Float32 3)) : OpenCLM Unit := do

  let idx ← getGlobalId 0
  if idx ≥ npts.toUSize then
    return

  let idx := idx.toUInt64

  if let some age := age? then
    age.set idx ((← age.get idx) + dt)

  let mut vel ← v[idx]
  let mut mass : Float32 := 1.0

  let mut airresist : Float32 := 0
  if let some r := global_airresist? then
    airresist += r
  if let some r := airresist? then
    airresist += (← r[idx])

  let mut targetv : Vector Float32 3 := 0
  if let some tv := global_targetv? then
    targetv += tv
  if let some tv := targetv? then
    targetv += (← tv[idx])

  if airresist > 0 then
    vel -= targetv
    let scale := 1 / (1 + airresist * length vel * dt)
    vel := scale * vel
    vel += targetv

  if let some f := force? then
    vel += dt * (← f[idx])

  v[idx] ← vel
