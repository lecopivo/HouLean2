import HouLean

open HouLean Apex


#check (by infer_instance : ProdLike (struct {label : String}) _)

#check (by infer_instance : ApexTypeFlatten (struct {label : String}) _)

-- set_option trace.HouLean.Apex.compiler true
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


@[apex]
def aa := do
  let a ← visualize' "hihi" 0.1001
  let b ← visualize' "hoho" (10 * a)
  let geo ← visualize' "haha" (SOP.box { center := { x := b} })
  return geo

open Std


@[apex]
def t1 (m : HashMap String Float) := m.alter "foo" (fun v? => some ((v?.getD 0) + 0.1))


-- @[apex]
-- def t2 (m : HashMap String Float) := m.alter "foo" (fun v? => (v?.map (· + 0.1)))


@[apex]
def t3 (v? : Option Float) := v?.map (· + 0.1)


@[apex]
def t4 (geo : Geometry) (toVis : String) (id : Nat) :=
  let go : VisualizeM Geometry := do
    let geo ← visualize "a" geo
    let geo := geo.subdivide
    let geo ← visualize "b" geo
    return geo
  let (geo, info, vis) := go (toVis, id) (fromApex (Dict.default), default)
  let vis := vis.pack |>.setPointAttrib 0 "__visualizer" (1 : Int)
  (geo.merge vis, info)


@[apex]
def t5 (x : Float) :=
  let dict := (fromApex (Dict.default) : HashMap String Float)
  dict.insert "a" x


@[apex]
def t6 :=
  let output := fun (input : HouLean.Apex.Geometry) => do
    let visualize ← HouLean.Apex.visualize "a" input default
    let SOP_subdivide : HouLean.Apex.Geometry := HouLean.Apex.SOP.subdivide default visualize
    let visualize_1 ← HouLean.Apex.visualize "b" SOP_subdivide default
    pure visualize_1;
  output


@[apex]
def t7 :=
  fun (input : HouLean.Apex.Geometry) =>
    HouLean.Apex.withVisualizer
      ((let output := fun (input : HouLean.Apex.Geometry) => do
          let visualize ← HouLean.Apex.visualize "visualize" input default
          pure visualize;
        output)
        input)


open Lean Meta Qq
