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

instance : Visualizer Int () where
  visualize x _ := 
    SOP.font { text := toString x }

abbrev VisualizeM (α : Type) := (Option String) → ((Option Geometry) × α)
    
instance : Monad VisualizeM where
  pure x := fun _ => (none, x)
  bind mx f := fun visName? => 
    let (vis?, x) := (mx visName?)
    match vis? with
    | some vis => -- got visualization, stop requestiong it
      let (_,y) := f x none
      (vis, y)
    | none => -- no visualization obtained from previous step, keep on looking
      f x visName?

def visualize {α Options} {defaultOpts : Options} [Visualizer α defaultOpts]
    (visualizerName : String) (x : α) (opts := defaultOpts) :
    VisualizeM Unit := 
  fun visName? =>
    match visName? with
    | none => (none, ())
    | some visName =>
      if visName == visualizerName then
        (some (Visualizer.visualize x opts), ())
      else
        (none, ())

