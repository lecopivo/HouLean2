import HouLean.Apex.Data
import HouLean.Apex.Sop
import HouLean.Apex.Geometry
import HouLean.Apex.Lean.HashMap
import HouLean.Meta.AnonymousStruct

open Lean Std

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

abbrev VisualizeM' :=
  -- this says which node to visualize and which visualizer on that node to use
  ReaderT (String × Int) <|
  -- this stores which nodes have visualizer attached to it and how many/index of the highest one
  StateM (HashMap String Int × Geometry)


def visualize' {α Options ts ts'} {defaultOpts : Options} [Visualizer α defaultOpts]
    [ApexTypeFlatten α ts] [ApexTypeFlatten Options ts']
    (nodeName : String) (x : α) (opts := defaultOpts) :
    VisualizeM' α :=
  fun (nodeToVisualize, visIdx) (visualizers, visGeo) =>
    let visualizers := visualizers.alter nodeName
        (fun count? => some (count?.map (·+1) |>.getD 0))
    (ctxIte
      (nodeName == nodeToVisualize && visualizers[nodeName]? == some visIdx)
      (opts, x, (visualizers, visGeo))
      (fun (opts, x, (visualizers, visGeo)) =>
        (opts, x, (visualizers, visGeo.merge (Visualizer.visualize x opts))))
      (fun (opts, x, (visualizers, visGeo)) =>
        let visualizers := visualizers.alter nodeName (fun count? => some (count?.map (·+1) |>.getD 0))
        (opts, x, (visualizers, visGeo))))
    |>.snd
