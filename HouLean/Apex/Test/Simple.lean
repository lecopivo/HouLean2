import HouLean
-- import HouLean.Apex.Data.Float

open HouLean.Apex Compiler


open Qq

/--
info: Nodes:
  0: x : __null__
  1: y : __null__
  2: z : __null__
  3: __parms__ : __parms__
  4: __output__ : __output__

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
  12: /__parms__/next[out]
  13: /__parms__/x[out]
  14: /__output__/next[in]
  15: /__output__/x[in]

Wires:
  0: /x/x[out] -> /y/x[in]
  1: /y/x[out] -> /z/x[in]
  2: /__parms__/x[out] -> /x/x[in]
  3: /z/x[out] -> /__output__/x[in]

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
  2: y : __null__
  3: multiply_float : Multiply<Float>
  4: __parms__ : __parms__
  5: __output__ : __output__

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
  16: /__parms__/next[out]
  17: /__parms__/x[out]
  18: /__output__/next[in]
  19: /__output__/out0[in]

Wires:
  0: /x/x[out] -> /add_float/a0[in]
  1: /x/x[out] -> /add_float/x[in]
  2: /add_float/out0[out] -> /y/x[in]
  3: /y/x[out] -> /multiply_float/a0[in]
  4: /x/x[out] -> /multiply_float/x[in]
  5: /__parms__/x[out] -> /x/x[in]
  6: /multiply_float/out0[out] -> /__output__/out0[in]

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
  2: __parms__ : __parms__
  3: __output__ : __output__

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /add_float/a0[in]
  5: /add_float/b1[in]
  6: /add_float/out0[out]
  7: /add_float/x[in]
  8: /__parms__/next[out]
  9: /__parms__/x[out]
  10: /__output__/next[in]
  11: /__output__/out0[in]

Wires:
  0: /x/x[out] -> /add_float/a0[in]
  1: /x/x[out] -> /add_float/x[in]
  2: /__parms__/x[out] -> /x/x[in]
  3: /add_float/out0[out] -> /__output__/out0[in]

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
  2: y : __null__
  3: lerp_float : Lerp<Float>
  4: multiply_float : Multiply<Float>
  5: __parms__ : __parms__
  6: __output__ : __output__

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
  20: /__parms__/next[out]
  21: /__parms__/x[out]
  22: /__output__/next[in]
  23: /__output__/out0[in]

Wires:
  0: /x/x[out] -> /divide_float/a0[in]
  1: /x/x[out] -> /divide_float/x[in]
  2: /divide_float/out0[out] -> /y/x[in]
  3: /x/x[out] -> /lerp_float/a0[in]
  4: /y/x[out] -> /lerp_float/b1[in]
  5: /x/x[out] -> /lerp_float/bias2[in]
  6: /y/x[out] -> /multiply_float/a0[in]
  7: /lerp_float/out0[out] -> /multiply_float/out0[in]
  8: /__parms__/x[out] -> /x/x[in]
  9: /multiply_float/out0[out] -> /__output__/out0[in]

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x : Float => let y := x/x; y*x.lerp y x)
  let g ← programToApexGraph e
  IO.println g
