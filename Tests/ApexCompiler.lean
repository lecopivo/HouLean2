import HouLean

open HouLean.Apex Compiler

open Qq


/--
info: Nodes:
  0: x : __null__

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]

Input Ports:
  2: /x/x[in]

Output Ports:
  3: /x/x[out]

Wires:

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x : Float => let y := x; let z := y; z)
  let g ← programToApexGraph e
  IO.println g


/--
info: Nodes:
  0: x : __null__
  1: add_float : Add<Float>
  2: multiply_float : Multiply<Float>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /add_float/a0[in]
  5: /add_float/b1[in]
  6: /add_float/out0[out]
  7: /add_float/x[in]
  8: /multiply_float/a0[in]
  9: /multiply_float/b1[in]
  10: /multiply_float/out0[out]
  11: /multiply_float/x[in]

Input Ports:
  2: /x/x[in]

Output Ports:
  10: /multiply_float/out0[out]

Wires:
  0: /x/x[out] -> /add_float/a0[in]
  1: /x/x[out] -> /add_float/x[in]
  2: /add_float/out0[out] -> /multiply_float/a0[in]
  3: /x/x[out] -> /multiply_float/x[in]

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x : Float => let y := x + x; y * x)
  let g ← programToApexGraph e
  IO.println g


/--
info: Nodes:
  0: x : __null__
  1: add_float : Add<Float>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /add_float/a0[in]
  5: /add_float/b1[in]
  6: /add_float/out0[out]
  7: /add_float/x[in]

Input Ports:
  2: /x/x[in]

Output Ports:
  6: /add_float/out0[out]

Wires:
  0: /x/x[out] -> /add_float/a0[in]
  1: /x/x[out] -> /add_float/x[in]

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x : Float => Generated.AddFloat x #v[x])
  let g ← programToApexGraph e
  IO.println g


/--
info: Nodes:
  0: x : __null__
  1: divide_float : Divide<Float>
  2: value_float : Value<Float>
  3: lerp_float : Lerp<Float>
  4: multiply_float : Multiply<Float>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /divide_float/a0[in]
  5: /divide_float/b1[in]
  6: /divide_float/out0[out]
  7: /divide_float/x[in]
  8: /value_float/parm[in]
  9: /value_float/value[out]
  10: /lerp_float/a0[in]
  11: /lerp_float/b1[in]
  12: /lerp_float/bias2[in]
  13: /lerp_float/out0[out]
  14: /multiply_float/a0[in]
  15: /multiply_float/b1[in]
  16: /multiply_float/out0[out]
  17: /multiply_float/out0[in]

Input Ports:
  2: /x/x[in]

Output Ports:
  16: /multiply_float/out0[out]

Wires:
  0: /x/x[out] -> /divide_float/a0[in]
  1: /x/x[out] -> /divide_float/x[in]
  2: /x/x[out] -> /lerp_float/a0[in]
  3: /divide_float/out0[out] -> /lerp_float/b1[in]
  4: /value_float/value[out] -> /lerp_float/bias2[in]
  5: /divide_float/out0[out] -> /multiply_float/a0[in]
  6: /lerp_float/out0[out] -> /multiply_float/out0[in]

Literals:
  0: float 0.300000 -> 8
-/
#guard_msgs in
run_meta
  let e := q(fun x : Float => let y := x/x; y*x.lerp y 0.3)
  let g ← programToApexGraph e
  IO.println g


/--
info: Nodes:
  0: value_bool : Value<Bool>

Ports:
  0: /value_bool/parm[in]
  1: /value_bool/value[out]

Wires:

Literals:
  0: bool "false" -> 0
-/
#guard_msgs in
run_meta
  let e := q(false)
  let (_,s) ← toApexGraph e default default
  let g := s.graph
  IO.println g


/--
info: Nodes:
  0: x : __null__

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/fst[in]
  3: /x/fst[out]
  4: /x/snd_fst[in]
  5: /x/snd_fst[out]
  6: /x/snd_snd[in]
  7: /x/snd_snd[out]

Input Ports:
  2: /x/fst[in]
  4: /x/snd_fst[in]
  6: /x/snd_snd[in]

Output Ports:
  3: /x/fst[out]

Wires:

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x : Float×Float×Float => x.1)
  let g ← programToApexGraph e
  IO.println g


/--
info: Nodes:
  0: x : __null__

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/fst[in]
  3: /x/fst[out]
  4: /x/snd_fst[in]
  5: /x/snd_fst[out]
  6: /x/snd_snd[in]
  7: /x/snd_snd[out]

Input Ports:
  2: /x/fst[in]
  4: /x/snd_fst[in]
  6: /x/snd_snd[in]

Output Ports:
  5: /x/snd_fst[out]

Wires:

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x : Float×Float×Float => x.2.1)
  let g ← programToApexGraph e
  IO.println g


/--
info: Nodes:
  0: x : __null__

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/fst[in]
  3: /x/fst[out]
  4: /x/snd_fst[in]
  5: /x/snd_fst[out]
  6: /x/snd_snd[in]
  7: /x/snd_snd[out]

Input Ports:
  2: /x/fst[in]
  4: /x/snd_fst[in]
  6: /x/snd_snd[in]

Output Ports:
  7: /x/snd_snd[out]

Wires:

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x : Float×Float×Float => x.2.2)
  let g ← programToApexGraph e
  IO.println g



attribute [apex_unfold] Id.run Std.Range.forIn'
attribute [apex_type "Int"] Nat
-- attribute [apex_node "Value<Int>"] Int.toNat
-- attribute [apex_node "Value<Int>"] Int.ofNat



run_meta
  let e := q(fun (x : Int) => Id.run do
  let mut x : Int := x
  for _ in [0:5] do
    x := x + x
  return x)
  let g ← programToApexGraph e
  IO.println g.pythonBuildScript

set_option trace.HouLean.Apex.compiler true 
@[apex]
def run' (x : Int) : Int := Id.run do
  let mut x : Int := x
  for _ in [0:5] do
    x := x + x
  return x




run_meta
  let e := q(fun x : Float×Float×Float×Float×Float => x.2.2.1)
  let g ← programToApexGraph e
  IO.println g

run_meta
  let e := q(fun x : Float×Float×Float => x)
  let g ← programToApexGraph e
  IO.println g

open HouLean Apex

attribute [apex_unfold] Id.run Std.Range.forIn'
attribute [apex_type "Int"] Nat
attribute [apex_node "Value<Int>"] Int.toNat
attribute [apex_node "Value<Int>"] Int.ofNat

attribute [apex_unfold] Vector3.compDiv

set_option trace.HouLean.Apex.compiler true in
@[apex]
def run (geo : Geometry) : Geometry := Id.run do
  let mut geo := geo  
  let r := geo.boundingBox
  let size := r.2.1
  let min := r.2.2.1
  for i in [0:geo.numPoints.toNat] do
    let P : Vector3 := geo.pointAttrib (Int.ofNat i) "P"
    let relP := (P - min).compDiv size
    geo := geo.setPointAttrib (Int.ofNat i) "Cd" relP
  return geo

@[apex]
def getBBoxSize (geo : Geometry) : Vector3 :=
  let r := geo.boundingBox
  let size := r.2.1
  size


run_meta
  let e := q(fun v : Vector3 => (v.x, v.y, v.z))
  let g ← programToApexGraph e
  IO.println g

abbrev match1 := fun v : Vector3 => let ⟨x,y,_⟩ := v; (x,y)

run_meta 
  let e := q(match1)
  let g ← programToApexGraph e
  IO.println g

  
run_meta
  let e := q(fun v : Option Float => (v.isSome, v.getD default))
  let g ← programToApexGraph e
  IO.println g



run_meta
  let e := q(fun x : Float×Float×Float => x)
  let g ← programToApexGraph e
  IO.println g



abbrev loop1 := fun (geo : Geometry) => Id.run do
    let mut geo := geo  
    let (_,size,min,_,_) := geo.boundingBox
    for i in [0:geo.numPoints.toNat] do
      let P : Vector3 := geo.pointAttrib (Int.ofNat i) "P"
      let relP := (P - min).compDiv size
      geo := geo.setPointAttrib (Int.ofNat i) "Cd" relP
    return geo

run_meta
  let e := q(loop1)
  let g ← programToApexGraph e
  IO.println g
