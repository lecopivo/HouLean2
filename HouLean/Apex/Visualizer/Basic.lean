import HouLean.Apex.Data
import HouLean.Apex.Sop
import HouLean.Apex.Geometry
import HouLean.Meta.AnonymousStruct

namespace HouLean.Apex

class Visualizer (α : Type) {Options : outParam Type} 
    (defaultOptions : outParam Options) where
  visualize : α → Options → Geometry


instance : Visualizer Float {min := 0.0, max := 1.0 : struct {min:Float,max:Float}} where
  visualize x _ := 
    SOP.font { text := toString x }

instance : Visualizer Geometry () where
  visualize geo _ := geo

instance : Visualizer Int { label := "value"  :struct { label : String }} where
  visualize x opts := 
    SOP.font { text := opts.label ++ ": " ++ toString x }

abbrev VisualizeM := ReaderT String <| StateM Geometry

def visualize {α Options} {defaultOpts : Options} [Visualizer α defaultOpts]
    (visualizerName : String) (x : α) (opts := defaultOpts) :
    VisualizeM Unit := 
  fun toVisualize vis =>
    if toVisualize == visualizerName then
      ((), (vis.merge #a[Visualizer.visualize x opts]))
    else
      ((), vis)

def withVisualizer (vis : String) (go : VisualizeM Geometry) : Geometry :=
  let (vis, geo) := go vis Geometry.default
  let vis := Geometry.mergePacked #a[vis] |>.setPointAttrib 0 "__visualizer" (1:Int)
  geo.merge #a[vis]
