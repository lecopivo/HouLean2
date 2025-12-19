import HouLean.OpenCL.Data.Matrix
import HouLean.OpenCL.Data.Pointer
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
