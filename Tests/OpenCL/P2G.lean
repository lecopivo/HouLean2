import HouLean.OpenCL.Data.Init
import HouLean.OpenCL.Data.Vector
import HouLean.OpenCL.Data.Matrix
import HouLean.OpenCL.Data.NanoVDB
import HouLean.OpenCL.WorkItemFunctions

open HouLean OpenCL

def gridToLinearIdx (coord : Vector Int32 3) (res : Vector UInt32 3) : Int64 := sorry

structure Grid where
  res : Vector UInt32 3
  bbmin : Vector Float32 3
  voxsize : Float32

def Grid.bbmax (g : Grid) : Vector Float32 3 :=
  g.bbmin + g.voxsize * g.res.map (·.toFloat32)

def Grid.isInside (g : Grid) (p : Vector Float32 3) (pad : Float32) : Bool := sorry

def Grid.toGridIntSpace (g : Grid) (p : Vector Float32 3) : Vector Int32 3 := sorry

-- summing rows together, i.e. reducing columnts, returns row vector of constants ones
def buildQuadStencil (base_diff : Vector Float32 3) : Matrix Float32 3 3 :=
    #m[ base_diff.map (fun x => 0.5*(1.5 - x)^2),
        base_diff.map (fun x => 0.75 - (x - 1.0)^2),
        base_diff.map (fun x => 0.5 * (x - 0.5)^2)]

def getQuadStencilWeightFromDist (d : Float32) : Float32 :=
  if (d < 0.5) then
    0.75 - d * d
  else if (d < 1.5) then
    0.5 * (1.5 - d) * (1.5 - d)
  else
    0.0

def getQuadStencilWeightFromDistVec (dx : Vector Float32 3) : Float32 :=
  getQuadStencilWeightFromDist dx.x.abs *
  getQuadStencilWeightFromDist dx.y.abs *
  getQuadStencilWeightFromDist dx.z.abs


def getMass (density gridscale dx : Float32) : Float32 :=
  let vol0 := (dx / gridscale).pow 3.0
  let mass := density * vol0
  mass



-- opencl_macro_type:
--      - when as input of a function is expected to be always called with explicit constructor
--      - for each constructor we will generate a new function variant
noncomputable
def p2g (npts : Nat)
      (P : ArrayPointer (Vector Float32 3))
      (v : ArrayPointer (Vector Float32 3))
      (density : ArrayPointer Float32)
      (C : ArrayPointer (Matrix Float32 3 3))
      (xformtoworld : Matrix Float32 4 4)
      (tilestartsxyz : ArrayPointer (Vector Int32 3))
      (mass_grid : NanoVDB Float32)
      (vel_grid : NanoVDB (NanoVDB.Vec3 Float32))
      -- (surface_grid : Option (NanoVDB Float32)) -- option will be `opencl_macro_type` i.e. type that has to be elided at compile time
      -- (state_grid : Option (NanoVDB Float32))   -- option will be `opencl_macro_type` i.e. type that has to be elided at compile time
      (gridscale : ArrayPointer Float32)
      (res : Vector UInt32 3)
      (bbmin : Vector Float32 3)
      (dx : Float32)
      (cellcounts : ArrayPointer Int32)
      (celloffsets : ArrayPointer Int32)
      (sortidx : ArrayPointer UInt32)
      (grididx : ArrayPointer Int64)
      : OpenCLM Unit := do

  let mass_grid_acc ← mass_grid.getReadAccessor
  let vel_grid_acc ← vel_grid.getReadAccessor

  -- let surface_grid_acc ← surface_grid.mapM (·.getReadAccessor)
  -- let state_grid_acc ← state_grid.mapM (·.getReadAccessor)

  let bin : Grid := {
    res := res
    bbmin := bbmin
    voxsize := dx
  }

  let idx ← getGlobalId 0

  let tileoffset := idx &&& 511 -- extract lower 8 bits
  let tileidx := idx >>> 9      -- extract the other bits
  let tilebasexyz ← tilestartsxyz[tileidx.toUInt64]

  let gidx := tilebasexyz.x + (tileoffset &&& 7).toInt32
  let gidy := tilebasexyz.y + ((tileoffset >>> 3) &&& 7).toInt32
  let gidz := tilebasexyz.z + ((tileoffset >>> 6) &&& 7).toInt32

  let g_coord : NanoVDB.Coord := {  data := #v[gidx, gidy, gidz] }--  := gidx.toInt32, y := gidy.toInt32, z := gidz.toInt32 }
  if !(← mass_grid_acc.isActive g_coord) then
      return

  let g_opos : Vector Float32 3 := #v[
    g_coord.data.x.toFloat32 + 0.5,
    g_coord.data.y.toFloat32 + 0.5,
    g_coord.data.z.toFloat32 + 0.5
  ]
  let g_wpos := xformtoworld.transformPointLeft g_opos --  @Math.transformPoint _ _ sorry xformtoworld g_opos

  let pad := 1.5 * dx
  let bbmax := bin.bbmax

  if !(bin.isInside g_wpos (pad:=1.5 * dx)) then
    let _ ← mass_grid_acc.set g_coord 0
    let _ ← vel_grid_acc.set g_coord ⟨0⟩
    return

  let bin_coord := bin.toGridIntSpace g_wpos

  let mut grid_mass := 0.0
  let mut grid_vel : Vector Float32 3 := 0

  let g_gridscale ← gridscale[(0:UInt64)]

  for i in [0:3] do
    for j in [0:3] do
      for k in [0:3] do
        let i : Int32 := i.toInt32 - 1
        let j : Int32 := j.toInt32 - 1
        let k : Int32 := k.toInt32 - 1

        let coord : Vector Int32 3 := bin_coord + #v[i, j, k]
        let linidx : Int64 := gridToLinearIdx coord bin.res

        if linidx < 0 then
          continue

        -- SPARSEFACTROR

        let ncount ← cellcounts[linidx.toUInt64]
        let noff   ← celloffsets[linidx.toUInt64]

        for l in [0:ncount.toInt.toNat] do
          let l : Int32 := l.toInt32
          let ptidx ← sortidx[(noff + l).toInt64.toUInt64]

          let affine ← C[ptidx.toUInt64]
          let p_wpos ← P[ptidx.toUInt64]
          let p_vel  ← v[ptidx.toUInt64]

          let wdiff := g_wpos - p_wpos
          let vdiff := wdiff / dx

          let weight := getQuadStencilWeightFromDistVec vdiff
          if weight == 0 then
            continue

          let p_density ← density[ptidx.toUInt64]
          let p_mass := getMass p_density g_gridscale dx

          grid_mass := grid_mass + weight * p_mass
          grid_vel := grid_vel + weight * (p_mass * p_vel + wdiff * affine)


  let _ ← mass_grid_acc.set g_coord grid_mass
  let _ ← vel_grid_acc.set g_coord grid_vel



open Qq
