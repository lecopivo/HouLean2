import HouLean

open HouLean Apex


@[apex]
def run (geo : Geometry) : VisualizeM Geometry := do
  let mut geo := geo

  visualize "input_geo" geo
  visualize "input_num_points" geo.numPoints

  geo := geo.subdivide

  visualize "subdivided_geo" geo
  visualize "subdivided_num_points" geo.numPoints

  geo := geo.fractal { 
    divisions := 2,
    smoothing := 5.0,
    addVertexNormals := true
  }

  visualize "fractal_geo" geo
  visualize "fractal_num_points" geo.numPoints

  return geo
