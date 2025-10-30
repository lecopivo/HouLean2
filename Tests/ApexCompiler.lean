import HouLean

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

Input Ports:
  2: /x/x[in]

Output Ports:
  11: /z/x[out]

Wires:
  0: /x/x[out] -> /y/x[in]
  1: /y/x[out] -> /z/x[in]

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

Input Ports:
  2: /x/x[in]

Output Ports:
  14: /multiply_float/out0[out]

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
  2: y : __null__
  3: value_float : Value<Float>
  4: lerp_float : Lerp<Float>
  5: multiply_float : Multiply<Float>

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
  12: /value_float/parm[in]
  13: /value_float/value[out]
  14: /lerp_float/a0[in]
  15: /lerp_float/b1[in]
  16: /lerp_float/bias2[in]
  17: /lerp_float/out0[out]
  18: /multiply_float/a0[in]
  19: /multiply_float/b1[in]
  20: /multiply_float/out0[out]
  21: /multiply_float/out0[in]

Input Ports:
  2: /x/x[in]

Output Ports:
  20: /multiply_float/out0[out]

Wires:
  0: /x/x[out] -> /divide_float/a0[in]
  1: /x/x[out] -> /divide_float/x[in]
  2: /divide_float/out0[out] -> /y/x[in]
  3: /x/x[out] -> /lerp_float/a0[in]
  4: /y/x[out] -> /lerp_float/b1[in]
  5: /value_float/value[out] -> /lerp_float/bias2[in]
  6: /y/x[out] -> /multiply_float/a0[in]
  7: /lerp_float/out0[out] -> /multiply_float/out0[in]

Literals:
  0: float 0.300000 -> 12
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


/-
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
-- #guard_msgs in

set_option trace.HouLean.Apex.compiler true

run_meta
  let e := q(fun x : Float×Float×Float => x.2.1)
  let g ← programToApexGraph e
  IO.println g

@[apex]
example (x : Float×Float×Float) : Float := x.1

@[apex]
example (x : Float×Float×Float) : Float := x.2.1

@[apex]
example (x : Float×Float×Float) : Float := x.2.2
