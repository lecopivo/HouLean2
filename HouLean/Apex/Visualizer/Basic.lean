import HouLean.Apex.Data
import HouLean.Apex.Sop
import HouLean.Apex.Geometry
import HouLean.Meta.AnonymousStruct

namespace HouLean.Apex

class Visualizer (α : Type) {Options : outParam Type}
    (defaultOptions : outParam Options) where
  visualize : α → Options → Geometry


@[default_instance]
instance : Visualizer Float {min := 0.0, max := 1.0 : struct {min:Float,max:Float}} where
  visualize x _ :=
    SOP.font { text := toString x }

instance : Visualizer Geometry () where
  visualize geo _ := geo

instance : Visualizer Int { label := "value"  :struct { label : String }} where
  visualize x opts :=
    SOP.font { text := opts.label ++ ": " ++ toString x }


abbrev VisualizeM :=
  ReaderT /- visualizer to show -/ Int <|
  StateM (/- number of existing visualizers -/ Int
          ×
          /- visualization geometry -/ Geometry)


def visualize {α Options ts ts'} {defaultOpts : Options} [Visualizer α defaultOpts] [ApexTypeFlatten α ts] [ApexTypeFlatten Options ts']
    (x : α) (opts := defaultOpts) :
    VisualizeM α :=
  fun toVisualize s =>
    (ctxIte (toVisualize == s.1) (opts, x, s)
      (fun (opts, x, (visNum, vis)) => (opts, x, visNum+1, vis.merge (Visualizer.visualize x opts)))
      (fun (opts, x, (visNum, vis)) => (opts, x, visNum+1, vis)))
    |>.snd

def withVisualizer (visId : Int) (go : VisualizeM Geometry) : Geometry :=
  let (geo, _, vis) := go visId (0, Geometry.default)
  let vis := vis.pack |>.setPointAttrib 0 "__visualizer" (1:Int)
  geo.merge vis


-- #check RigidScaleTransform

-- def Geometry.rotate (geo : Geometry) (orient : Vector4) : Geometry := sorry
-- def Geometry.translate (geo : Geometry) (t : Vector3) : Geometry := sorry

-- instance : Visualizer RigidScaleTransform () where
--   visualize trans geo? := Id.run do
--     let box := SOP.box
--     let box' := box.rotate trans.orient
--     let box'' := box'.translate trans.translate

--     sorry
