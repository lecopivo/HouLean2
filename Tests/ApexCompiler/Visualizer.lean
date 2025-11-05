import HouLean

open HouLean Apex

------------------------------------------------------------------
-- move to Apex.Lean.String
class ApexToString (α : Type u) where
  toString : α → String

instance : ApexToString Int where
  toString := Generated.string_FromInteger
    
def _root_.ToString.toString.apex_impl {α} [ApexToString α] (a : α) : String := 
  ApexToString.toString a

open Compiler in
run_meta compilerExt.add (.implementedByName ``toString ``ToString.toString.apex_impl
  #[some 0, none, some 2]) default

------------------------------------------------------------------

-- move to Apex.Data.Geometry
attribute [apex_node "Value<Geometry>"] Geometry.default

------------------------------------------------------------------


@[apex]
def run (geo : Geometry) (visualizer : String) : 
    struct { vis : Option Geometry, result : Geometry} := Id.run do


  let go : VisualizeM Geometry := do
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
  
  let (vis?, result) ← go visualizer
  return {
    vis := vis?
    result := result
  }
