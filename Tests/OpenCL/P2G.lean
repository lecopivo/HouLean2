import HouLean.OpenCL.Data.Vector3
import HouLean.OpenCL.Data.Matrix3
import HouLean.OpenCL.Data.ArrayRef
import HouLean.OpenCL.WorkItemFunctions

open HouLean OpenCL

structure Int3 where
  x : Int
  y : Int
  z : Int

instance : Add Int3 := sorry

structure Coord where
  x : Int32
  y : Int32
  z : Int32

def gridToLinearIdx (coord : Int3) (res : Int3) : Int64 := sorry

structure Grid where
  res : Int3
  bbmin : Vector3
  voxsize : Float

def Grid.bbmax (g : Grid) : Vector3 := {
  x := g.bbmin.x + g.voxsize * g.res.x.toInt32.toFloat
  y := g.bbmin.y + g.voxsize * g.res.y.toInt32.toFloat
  z := g.bbmin.z + g.voxsize * g.res.z.toInt32.toFloat
}

def getQuadStencilWeightFromDistVec (dx : Vector3) : Float := sorry

def Grid.toGridIntSpace (g : Grid) (p : Vector3) : Int3 := sorry

instance : ArrayType Int3 where
  get := sorry
  set := sorry

instance : ArrayType Int where
  get := sorry
  set := sorry

instance : ArrayType Nat where
  get := sorry
  set := sorry

instance : ArrayType Float where
  get := sorry
  set := sorry

instance : ArrayType Matrix3 where
  get := sorry
  set := sorry

def VDBRef (α : Type) : Type := sorry

def VDBRef.get {α} (grid : VDBRef α) (coord : Coord) : OpenCLM α := sorry
def VDBRef.set {α} (grid : VDBRef α) (coord : Coord) (val : α) : OpenCLM Unit := sorry

def VDBRef.isActive {α} (grid : VDBRef α) (coord : Coord) : OpenCLM Bool := sorry

def getMass (density gridscale dx : Float) : Float :=
  let vol0 := Float.pow (dx / gridscale) 3.0
  let mass := density * vol0
  mass

def getQuadStencilWeightFromDistVec (v : Vector3) : Float := sorry

def p2g (npts : Nat)
      (P : ArrayRef Vector3)
      (v : ArrayRef Vector3)
      (density : ArrayRef Float)
      (C : ArrayRef Matrix3)
      (xformtoworld : Matrix4)
      (tilestartsxyz : ArrayRef Int3)
      (mass_grid : VDBRef Float) -- todo: change to vdb reference
      (vel_grid : VDBRef Vector3)  -- todo: change to vdb reference
      (gridscale : ArrayRef Float)
      (res : Int3)
      (bbmin : Vector3)
      (dx : Float)
      (cellcounts : ArrayRef Int)
      (celloffsets : ArrayRef Int)
      (sortidx : ArrayRef Nat)
      (girdidx : ArrayRef Int64)
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
run_meta Compiler.compileFunction q(p2g)
