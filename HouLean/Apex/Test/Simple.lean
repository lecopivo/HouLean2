import HouLean.Apex.Compile.Main
import HouLean.Apex.Data.Vector3
import HouLean.Apex.Data.Float

open HouLean.Apex Compiler



open Qq

/--
info: Nodes:
  0: x : __null__
  1: y : __null__
  2: z : __null__

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /y/__spare__[in]
  5: /y/__spare__[out]
  6: /y/x[in]
  7: /y/x[out]
  8: /z/__spare__[in]
  9: /z/__spare__[out]
  10: /z/x[in]
  11: /z/x[out]

Wires:
  0: /x/x[out] -> /y/x[in]
  1: /y/x[out] -> /z/x[in]

Literals:
-/
#guard_msgs in
run_meta
  let c : GraphCompileM Unit := do
    let e := q(fun x : Float => let y := x; let z := y; z)
    let _ ← functionToApexGraph e
  let (_,⟨g,_,_⟩) ← c default
  IO.println g

-- attribute [apex_node "Add<Float>"] Float.add
-- attribute [apex_node "Multiply<Float>"] Float.mul


run_meta
  let c : GraphCompileM Unit := do
    let e := q(fun x : Float => let y := x + x; y * x)
    let _ ← functionToApexGraph e
  let (_,⟨g,_,_⟩) ← c default
  IO.println g
  IO.println g.pythonBuildScript

run_meta
  let c : GraphCompileM Unit := do
    let e := q(fun x : Float => Generated.AddFloat x #v[x])
    let _ ← functionToApexGraph e
  let (_,⟨g,_,_⟩) ← c default
  IO.println g
  IO.println g.pythonBuildScript

run_meta
  let c : GraphCompileM Unit := do
    let e := q(fun x : Float => let y := x/x; y*x.lerp y x)
    let _ ← functionToApexGraph e
  let (_,⟨g,_,_⟩) ← c default
  IO.println g
  IO.println g.pythonBuildScript


structure Float2 where
  x : Float
  y : Float

instance : Add Float2 := ⟨fun x y => ⟨x.1+y.1, x.2+y.2⟩⟩

run_meta
  let c : GraphCompileM Unit := do
    let e := q(fun x : Float2 => x + x + x)
    let _ ← functionToApexGraph e
  let (_,⟨g,_,_⟩) ← c default
  IO.println g
  IO.println g.pythonBuildScript

run_meta
  let c : GraphCompileM Unit := do
    let e := q(fun x y : Float2 => let z := x + y; z + x + y)
    let _ ← functionToApexGraph e
  let (_,⟨g,_,_⟩) ← c default
  IO.println g
  IO.println g.pythonBuildScript

abbrev add2 (x y : Float × Float) : Float × Float := (x.1+y.1, x.2+y.2)
abbrev add3 (x y : Float × Float × Float) : Float × Float × Float := (x.1+y.1, add2 x.2 y.2)
abbrev dot2 (x y : Float × Float) : Float := (x.1*y.1 + x.2*y.2)
abbrev dot3 (x y : Float × Float × Float) : Float := (x.1*y.1 + dot2 x.2 y.2)

instance : Add Vector2 := ⟨fun x y => Apex.AddVector2 x [y]⟩  
instance : Add Vector3 := ⟨fun x y => Apex.AddVector3 x [y]⟩

abbrev Vector2.x (v : Vector2) : Float := Apex.GetComponentVector2 v 0
abbrev Vector2.y (v : Vector2) : Float := Apex.GetComponentVector2 v 1
abbrev Vector2.mk (x y : Float) : Vector2 := Apex.FloatToVector2 x y

abbrev Vector3.x (v : Vector3) : Float := Apex.GetComponentVector3 v 0
abbrev Vector3.y (v : Vector3) : Float := Apex.GetComponentVector3 v 1
abbrev Vector3.z (v : Vector3) : Float := Apex.GetComponentVector3 v 2
abbrev Vector3.mk (x y z : Float) : Vector3 := Apex.FloatToVector3 x y z

abbrev Vector4.x (v : Vector4) : Float := Apex.GetComponentVector4 v 0
abbrev Vector4.y (v : Vector4) : Float := Apex.GetComponentVector4 v 1
abbrev Vector4.z (v : Vector4) : Float := Apex.GetComponentVector4 v 2
abbrev Vector4.w (v : Vector4) : Float := Apex.GetComponentVector4 v 3
abbrev Vector4.mk (x y z w : Float) : Vector4 := Apex.FloatToVector4 x y z w

instance : Add Vector4 := ⟨fun u v => .mk (u.x+v.x) (u.y+v.y) (u.z+v.z) (u.w+v.w)⟩  

run_meta
  let c : GraphCompileM Unit := do
    let e := q(fun x y : Vector4 => x + y)
    let _ ← functionToApexGraph e
  let (_,⟨g,_⟩) ← c default
  IO.println g
  IO.println g.pythonBuildScript

run_meta
  let c : GraphCompileM Unit := do
    let e := q(fun x : Vector3 => Apex.AddVector3 x [x,x,x,x])
    let _ ← functionToApexGraph e
  let (_,⟨g,_⟩) ← c default
  IO.println g
  IO.println g.pythonBuildScript

run_meta
  let c : GraphCompileM Unit := do
    let e := q(fun x : Float => let y := (x,x); dot2 y y)
    let _ ← functionToApexGraph e
  let (_,⟨g,_⟩) ← c default
  IO.println g
  IO.println g.pythonBuildScript


abbrev foo := fun x : Float => Id.run do
    let mut x : Float := x
    for _ in [0:10] do
      x := x + x
    x

abbrev bar := fun x : Float => Id.run do
    let mut x : Float := x
    let mut y : Float := x
    for _ in [0:10] do
      let tmp := y
      y := x + y
      x := tmp
    x

abbrev foobar := fun x' : Float => Id.run do
    let mut x : Vector3 := Vector3.mk 0 0 0
    let mut y : Vector3 := Vector3.mk 1 10 100
    let mut n : Nat := 0
    for i in [0:10] do
      let tmp := y
      y := x.add y
      x := tmp
      n := n + i*i
    x

attribute [apex_type "Int"] Nat
attribute [apex_node "Add<Int>"] Nat.add
attribute [apex_node "Add<Nat>"] Nat.mul

run_meta
  let c : GraphCompileM Unit := do
    let e := q(foo)
    let _ ← functionToApexGraph e
  let (_,⟨g,_⟩) ← c default
  IO.println g
  IO.println (g.pythonBuildScript true)

run_meta
  let c : GraphCompileM Unit := do
    let e := q(bar)
    let _ ← functionToApexGraph e
  let (_,⟨g,_⟩) ← c default
  IO.println g
  IO.println (g.pythonBuildScript true)

set_option pp.all true in
run_meta
  let c : GraphCompileM Unit := do
    let e := q(foobar)
    let _ ← functionToApexGraph e
  let (_,⟨g,_⟩) ← c default
  IO.println g
  IO.println g.pythonBuildScript



