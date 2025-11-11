import HouLean

open HouLean Apex

set_option trace.HouLean.Apex.compiler true 
@[apex]
def run (geo : Geometry) : VisualizeM Geometry := do
  let mut geo := geo

  visualize "input_geo" geo
  visualize "input_num_points" geo.numPoints 
    { label := "input number of points" }

  geo := geo.subdivide

  visualize "subdivided_geo" geo
  visualize "subdivided_num_points" geo.numPoints 
    { label := "input number of points after subdivision" }

  geo := geo.fractal { 
    divisions := 2,
    smoothing := 5.0,
    addVertexNormals := true
  }

  visualize "fractal_geo" geo
  visualize "fractal_num_points" geo.numPoints
    { label := "input number of points after fractal" }

  return geo



open Lean Meta Qq

