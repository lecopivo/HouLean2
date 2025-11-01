import HouLean

open HouLean Apex Compiler Qq


/--
info: Nodes:
  0: x : __null__
  1: value_bool : Value<Bool>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /value_bool/parm[in]
  5: /value_bool/value[out]

Input Ports:
  2: /x/x[in]

Output Ports:
  3: /x/x[out]
  5: /value_bool/value[out]

Wires:

Literals:
  0: bool "true" -> 4
-/
#guard_msgs in
run_meta
  let e := q(fun x : Int => some x)
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
  4: /x/snd[in]
  5: /x/snd[out]

Input Ports:
  2: /x/fst[in]
  4: /x/snd[in]

Output Ports:
  3: /x/fst[out]
  5: /x/snd[out]

Wires:

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x : Option Int => x)
  let g ← programToApexGraph e
  IO.println g


/--
info: Nodes:
  0: x : __null__
  1: value_int : Value<Int>
  2: value_bool : Value<Bool>
  3: equals_bool : Equals<Bool>
  4: twowayswitch_int : TwoWaySwitch<Int>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/fst[in]
  3: /x/fst[out]
  4: /x/snd[in]
  5: /x/snd[out]
  6: /value_int/parm[in]
  7: /value_int/value[out]
  8: /value_bool/parm[in]
  9: /value_bool/value[out]
  10: /equals_bool/a0[in]
  11: /equals_bool/b1[in]
  12: /equals_bool/out0[out]
  13: /twowayswitch_int/a0[in]
  14: /twowayswitch_int/b1[in]
  15: /twowayswitch_int/index2[in]
  16: /twowayswitch_int/out0[out]

Input Ports:
  2: /x/fst[in]
  4: /x/snd[in]

Output Ports:
  16: /twowayswitch_int/out0[out]

Wires:
  0: /x/snd[out] -> /equals_bool/a0[in]
  1: /value_bool/value[out] -> /equals_bool/b1[in]
  2: /x/fst[out] -> /twowayswitch_int/a0[in]
  3: /value_int/value[out] -> /twowayswitch_int/b1[in]
  4: /equals_bool/out0[out] -> /twowayswitch_int/index2[in]

Literals:
  0: int 0 -> 6 ⏎
  1: bool "true" -> 8
-/
#guard_msgs in
run_meta
  let e := q(fun x : Option Int => x.getD default)
  let g ← programToApexGraph e
  IO.println g


-- run_meta
--   let e := q(fun x : Option Int => x.map (fun x => x+x))
--   let g ← programToApexGraph e
--   IO.println g


open Qq
open Lean

