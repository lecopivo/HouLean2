import HouLean

open HouLean Apex


#check (by infer_instance : ProdLike (struct {label : String}) _)

#check (by infer_instance : ApexTypeFlatten (struct {label : String}) _)

set_option trace.HouLean.Apex.compiler true
@[apex]
def run (geo : Geometry) : VisualizeM Geometry := do
  let mut geo := geo

  geo ← visualize geo
  -- let (_ : Int)  ← visualize (α:=Int) geo.numPoints
  --   { label := "input number of points" }

  geo := geo.subdivide

  geo ← visualize geo
  -- visualize "subdivided_num_points" geo.numPoints
  --   { label := "input number of points after subdivision" }

  geo := geo.fractal {
    divisions := 2,
    smoothing := 5.0,
    addVertexNormals := true
  }

  geo ← visualize geo
  -- visualize "fractal_num_points" geo.numPoints
  --   { label := "input number of points after fractal" }

  return geo


-- @[apex]
-- def aa := visualize' "hihi" 0.1001


open Lean Meta Qq
