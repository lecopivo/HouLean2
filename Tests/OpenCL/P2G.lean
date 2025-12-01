import HouLean.OpenCL.Data.Vector
import HouLean.OpenCL.Data.Matrix
import HouLean.OpenCL.WorkItemFunctions

open HouLean OpenCL

def gridToLinearIdx (coord : Vector Int 3) (res : Vector Nat 3) : Int64 := sorry

structure Grid where
  res : Vector Nat 3
  bbmin : Vector Float32 3
  voxsize : Float32

def Grid.bbmax (g : Grid) : Vector Float32 3 :=
  g.bbmin + g.voxsize * g.res.map (·.toFloat32)

def getQuadStencilWeightFromDistVec (dx : Vector Float32 3) : Float32 := sorry

def Grid.toGridIntSpace (g : Grid) (p : Vector Float32 3) : Vector Int 3 := sorry

def VDBRef (α : Type) : Type := sorry

def VDBRef.get {α} (grid : VDBRef α) (coord : Vector Int32 3) : OpenCLM α := sorry
def VDBRef.set {α} (grid : VDBRef α) (coord : Vector Int32 3) (val : α) : OpenCLM Unit := sorry

def VDBRef.isActive {α} (grid : VDBRef α) (coord : Vector Int32 3) : OpenCLM Bool := sorry

def getMass (density gridscale dx : Float32) : Float32 :=
  let vol0 := (dx / gridscale).pow 3.0
  let mass := density * vol0
  mass


def p2g (npts : Nat)
      (P : ConstPointer Float32)
      (v : ConstPointer Float32)
      (density : ConstPointer Float32)
      (C : ConstPointer Float32)
      (xformtoworld : Matrix Float32 4 4)
      (tilestartsxyz : ConstPointer Int)
      (mass_grid : VDBRef Float32)
      (vel_grid : VDBRef (Vector Float32 3))
      (gridscale : ConstPointer Float32)
      (res : Vector Nat 3)
      (bbmin : Vector Float32 3)
      (dx : Float32)
      (cellcounts : ConstPointer Int)
      (celloffsets : ConstPointer Int)
      (sortidx : ConstPointer Nat)
      (girdidx : ConstPointer Int64)
      : OpenCLM Unit := do

  -- cnanovdb_readaccessor mass_grid_acc;
  -- cnanovdb_getreadaccessor(mass_grid, &mass_grid_acc);


  let bin : Grid:= {
    res := res
    bbmin := bbmin
    voxsize := dx
  }

  let idx ← getGlobalId 0

  let tileoffset := idx &&& 511 -- extract lower 8 bits
  let tileidx := idx >>> 9 -- extract the other bits
  let tilebasexyz ← ArrayType.get tilestartsxyz tileidx.toUInt64

  let gidx := tilebasexyz.x + (tileoffset &&& 7).toInt32.toInt
  let gidy := tilebasexyz.y + ((tileoffset >>> 3) &&& 7).toInt32.toInt
  let gidz := tilebasexyz.z + ((tileoffset >>> 6) &&& 7).toInt32.toInt

  let g_coord : Coord := { x := gidx.toInt32, y := gidy.toInt32, z := gidz.toInt32 }
  if !(← mass_grid.isActive g_coord) then
      return

  let g_opos : Vector3 := {
    x := g_coord.x.toFloat + 0.5
    y := g_coord.y.toFloat + 0.5
    z := g_coord.z.toFloat + 0.5
  }
  let g_wpos := @Math.transformPoint _ _ sorry xformtoworld g_opos

  let pad := 1.5 * dx
  let bbmax := bin.bbmax
  if (g_wpos <= bin.bbmin + ⟨pad,pad,pad⟩ || g_wpos >= bin.bbmax - ⟨pad,pad,pad⟩) then
    mass_grid.set g_coord 0.0
    vel_grid.set  g_coord ⟨0,0,0⟩
    return

  let bin_coord : Int3 := bin.toGridIntSpace g_wpos

  let mut grid_mass := 0.0
  let mut grid_vel : Vector3 := ⟨0,0,0⟩

  let g_gridscale ← ArrayType.get gridscale 0

  for i in [0:3] do
    for j in [0:3] do
      for k in [0:3] do
        let i : Int := i.toInt32.toInt - 1
        let j : Int := j.toInt32.toInt - 1
        let k : Int := k.toInt32.toInt - 1

        let coord : Int3 := bin_coord + Int3.mk i j k
        let linidx : Int64 := gridToLinearIdx coord bin.res

        if linidx < 0 then
          continue

        -- SPARSEFACTROR

        let ncount : Int ← ArrayType.get cellcounts linidx.toUInt64
        let noff : Int ← ArrayType.get celloffsets linidx.toUInt64

        for l in [0:ncount.toNat] do
          let ptidx ← ArrayType.get sortidx (noff.toNat.toUInt64 + l.toUInt64)

          let affine ← ArrayType.get C ptidx.toUInt64
          let p_wpos ← ArrayType.get P ptidx.toUInt64
          let p_vel ← ArrayType.get v ptidx.toUInt64

          let wdiff := g_wpos - p_wpos
          let vdiff := wdiff / dx

          let weight := getQuadStencilWeightFromDistVec vdiff
          if weight == 0 then
            continue

          let p_density ← ArrayType.get density ptidx.toUInt64
          let p_mass := getMass p_density g_gridscale dx

          grid_mass := grid_mass + weight * p_mass
          grid_vel := grid_vel + weight * (p_mass * p_vel + affine * wdiff)


  mass_grid.set g_coord grid_mass
  vel_grid.set g_coord grid_vel
  return



open Qq
