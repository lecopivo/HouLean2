import HouLean.Apex.Compile.Main
import HouLean.Apex.Data.Vector2
-- import HouLean.Apex.Data.Float

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


/--
info: Nodes:
  0: x : __null__
  1: add_float : Add<Float>
  2: y : __null__
  3: multiply_float : Multiply<Float>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /add_float/a0[in]
  5: /add_float/b1[in]
  6: /add_float/out0[out]
  7: /add_float/x[in]
  8: /y/__spare__[in]
  9: /y/__spare__[out]
  10: /y/x[in]
  11: /y/x[out]
  12: /multiply_float/a0[in]
  13: /multiply_float/b1[in]
  14: /multiply_float/out0[out]
  15: /multiply_float/x[in]

Wires:
  0: /x/x[out] -> /add_float/a0[in]
  1: /x/x[out] -> /add_float/x[in]
  2: /add_float/out0[out] -> /y/x[in]
  3: /y/x[out] -> /multiply_float/a0[in]
  4: /x/x[out] -> /multiply_float/x[in]

Literals:
-/
#guard_msgs in
run_meta
  let c : GraphCompileM Unit := do
    let e := q(fun x : Float => let y := x + x; y * x)
    let _ ← functionToApexGraph e
  let (_,⟨g,_,_⟩) ← c default
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

Wires:
  0: /x/x[out] -> /add_float/a0[in]
  1: /x/x[out] -> /add_float/x[in]

Literals:
-/
#guard_msgs in
run_meta
  let c : GraphCompileM Unit := do
    let e := q(fun x : Float => Generated.AddFloat x #v[x])
    let _ ← functionToApexGraph e
  let (_,⟨g,_,_⟩) ← c default
  IO.println g


/--
info: Nodes:
  0: x : __null__
  1: divide_float : Divide<Float>
  2: y : __null__
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
  8: /y/__spare__[in]
  9: /y/__spare__[out]
  10: /y/x[in]
  11: /y/x[out]
  12: /lerp_float/a0[in]
  13: /lerp_float/b1[in]
  14: /lerp_float/bias2[in]
  15: /lerp_float/out0[out]
  16: /multiply_float/a0[in]
  17: /multiply_float/b1[in]
  18: /multiply_float/out0[out]
  19: /multiply_float/out0[in]

Wires:
  0: /x/x[out] -> /divide_float/a0[in]
  1: /x/x[out] -> /divide_float/x[in]
  2: /divide_float/out0[out] -> /y/x[in]
  3: /x/x[out] -> /lerp_float/a0[in]
  4: /y/x[out] -> /lerp_float/b1[in]
  5: /x/x[out] -> /lerp_float/bias2[in]
  6: /y/x[out] -> /multiply_float/a0[in]
  7: /lerp_float/out0[out] -> /multiply_float/out0[in]

Literals:
-/
#guard_msgs in
run_meta
  let c : GraphCompileM Unit := do
    let e := q(fun x : Float => let y := x/x; y*x.lerp y x)
    let _ ← functionToApexGraph e
  let (_,⟨g,_,_⟩) ← c default
  IO.println g





