import HouLean.OpenCL.Data.Matrix
import HouLean.OpenCL.Data.ArrayRef
import HouLean.OpenCL.Data.NanoVDB
import HouLean.OpenCL.Data.Unit
import HouLean.OpenCL.Data.Int
import HouLean.OpenCL.Data.Bool
import HouLean.OpenCL.WorkItemFunctions
import HouLean.OpenCL.Data.ArrayPointer

open HouLean OpenCL

namespace Test.OpenCL.MPM



noncomputable
def clearSurfaces
  (tilestartsxyx : ArrayPointer (Vector Int32 3)) -- missing global, const, restric
  (surface_grid : NanoVDB Float32)       -- missing global, restric
  (surface_grid_out : NanoVDB Float32)   -- missing global, restric
  : OpenCLM Unit := do

  let surface_grid_acc ← surface_grid.getReadAccessor
  let surface_grid_out_acc ← surface_grid_out.getReadAccessor

  let idx ← getGlobalId 0

  let tileoffset := idx &&& 511
  let tileidx := idx >>> 9
  let tilebasexyz ← tilestartsxyx[tileidx.toUInt64]

  let gidx := tilebasexyz[0] + (tileoffset &&& 7).toInt32
  let gidy := tilebasexyz[1] + ((tileoffset >>> 3) &&& 7).toInt32
  let gidz := tilebasexyz[2] + ((tileoffset >>> 6) &&& 7).toInt32

  let g_coord : NanoVDB.Coord := #v[gidx, gidy, gidz]
  let active ← surface_grid_acc.isActive g_coord
  if !active then
    return

  let _ ← surface_grid_acc.set g_coord 0
  let _ ← surface_grid_out_acc.set g_coord 0
  return


-- #opencl_compile clearSurfaces


noncomputable
def clearGrids
  (tilestartsxyx : ArrayPointer (Vector Int32 3))  -- missing global, const, restric
  (mass_grid : NanoVDB Float32)                    -- missing global, restric
  (vel_grid : NanoVDB (NanoVDB.Vec3 Float32))      -- missing global, restric
  : OpenCLM Unit := do


  let mass_grid_acc ← mass_grid.getReadAccessor
  let vel_grid_acc ← vel_grid.getReadAccessor

  let idx ← getGlobalId 0

  let tileoffset := idx &&& 511
  let tileidx := idx >>> 9
  let tilebasexyz ← tilestartsxyx[tileidx.toUInt64]

  let gidx := tilebasexyz[0] + (tileoffset &&& 7).toInt32
  let gidy := tilebasexyz[1] + ((tileoffset >>> 3) &&& 7).toInt32
  let gidz := tilebasexyz[2] + ((tileoffset >>> 6) &&& 7).toInt32

  let g_coord : NanoVDB.Coord := #v[gidx, gidy, gidz]
  if !(← mass_grid_acc.isActive g_coord) then
    return

  let _ ← mass_grid_acc.set g_coord 0
  let _ ← vel_grid_acc.set g_coord (0 : Vector Float32 3)
  return

-- #opencl_compile clearGrids
