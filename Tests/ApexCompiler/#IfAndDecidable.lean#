import HouLean

open HouLean Apex Compiler

open Qq

open Lean

/--

info: Nodes:
  0: x : __null__
  1: y : __null__
  2: lessthan_int : LessThan<Int>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /y/__spare__[in]
  5: /y/__spare__[out]
  6: /y/x[in]
  7: /y/x[out]
  8: /lessthan_int/a0[in]
  9: /lessthan_int/b1[in]
  10: /lessthan_int/out0[out]

Input Ports:
  2: /x/x[in]
  6: /y/x[in]

Output Ports:
  10: /lessthan_int/out0[out]

Wires:
  0: /x/x[out] -> /lessthan_int/a0[in]
  1: /y/x[out] -> /lessthan_int/b1[in]

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x y : Int => decide (x < y))
  let g ← programToApexGraph e
  IO.println g


/--
info: Nodes:
  0: x : __null__
  1: y : __null__
  2: lessthan_int : LessThan<Int>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /y/__spare__[in]
  5: /y/__spare__[out]
  6: /y/x[in]
  7: /y/x[out]
  8: /lessthan_int/a0[in]
  9: /lessthan_int/b1[in]
  10: /lessthan_int/out0[out]

Input Ports:
  2: /x/x[in]
  6: /y/x[in]

Output Ports:
  10: /lessthan_int/out0[out]

Wires:
  0: /y/x[out] -> /lessthan_int/a0[in]
  1: /x/x[out] -> /lessthan_int/b1[in]

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x y : Int => decide (x > y))
  let g ← programToApexGraph e
  IO.println g

/--
info: Nodes:
  0: x : __null__
  1: y : __null__
  2: lessthanorequal_int : LessThanOrEqual<Int>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /y/__spare__[in]
  5: /y/__spare__[out]
  6: /y/x[in]
  7: /y/x[out]
  8: /lessthanorequal_int/a0[in]
  9: /lessthanorequal_int/b1[in]
  10: /lessthanorequal_int/out0[out]

Input Ports:
  2: /x/x[in]
  6: /y/x[in]

Output Ports:
  10: /lessthanorequal_int/out0[out]

Wires:
  0: /x/x[out] -> /lessthanorequal_int/a0[in]
  1: /y/x[out] -> /lessthanorequal_int/b1[in]

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x y : Int => decide (x ≤ y))
  let g ← programToApexGraph e
  IO.println g

/--
info: Nodes:
  0: x : __null__
  1: y : __null__
  2: lessthanorequal_int : LessThanOrEqual<Int>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /y/__spare__[in]
  5: /y/__spare__[out]
  6: /y/x[in]
  7: /y/x[out]
  8: /lessthanorequal_int/a0[in]
  9: /lessthanorequal_int/b1[in]
  10: /lessthanorequal_int/out0[out]

Input Ports:
  2: /x/x[in]
  6: /y/x[in]

Output Ports:
  10: /lessthanorequal_int/out0[out]

Wires:
  0: /y/x[out] -> /lessthanorequal_int/a0[in]
  1: /x/x[out] -> /lessthanorequal_int/b1[in]

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x y : Int => decide (x ≥ y))
  let g ← programToApexGraph e
  IO.println g

/--
info: Nodes:
  0: x : __null__
  1: y : __null__
  2: equals_int : Equals<Int>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /y/__spare__[in]
  5: /y/__spare__[out]
  6: /y/x[in]
  7: /y/x[out]
  8: /equals_int/a0[in]
  9: /equals_int/b1[in]
  10: /equals_int/out0[out]

Input Ports:
  2: /x/x[in]
  6: /y/x[in]

Output Ports:
  10: /equals_int/out0[out]

Wires:
  0: /x/x[out] -> /equals_int/a0[in]
  1: /y/x[out] -> /equals_int/b1[in]

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x y : Int => decide (x = y))
  let g ← programToApexGraph e
  IO.println g

/--
info: Nodes:
  0: x : __null__
  1: y : __null__
  2: add_int : Add<Int>
  3: multiply_int : Multiply<Int>
  4: lessthan_int : LessThan<Int>
  5: twowayswitch_int : TwoWaySwitch<Int>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /y/__spare__[in]
  5: /y/__spare__[out]
  6: /y/x[in]
  7: /y/x[out]
  8: /add_int/a0[in]
  9: /add_int/b1[in]
  10: /add_int/out0[out]
  11: /add_int/x[in]
  12: /multiply_int/a0[in]
  13: /multiply_int/b1[in]
  14: /multiply_int/out0[out]
  15: /multiply_int/x[in]
  16: /lessthan_int/a0[in]
  17: /lessthan_int/b1[in]
  18: /lessthan_int/out0[out]
  19: /twowayswitch_int/a0[in]
  20: /twowayswitch_int/b1[in]
  21: /twowayswitch_int/index2[in]
  22: /twowayswitch_int/out0[out]

Input Ports:
  2: /x/x[in]
  6: /y/x[in]

Output Ports:
  22: /twowayswitch_int/out0[out]

Wires:
  0: /x/x[out] -> /add_int/a0[in]
  1: /y/x[out] -> /add_int/x[in]
  2: /x/x[out] -> /multiply_int/a0[in]
  3: /y/x[out] -> /multiply_int/x[in]
  4: /x/x[out] -> /lessthan_int/a0[in]
  5: /y/x[out] -> /lessthan_int/b1[in]
  6: /add_int/out0[out] -> /twowayswitch_int/a0[in]
  7: /multiply_int/out0[out] -> /twowayswitch_int/b1[in]
  8: /lessthan_int/out0[out] -> /twowayswitch_int/index2[in]

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x y : Int => if x < y then x + y else x * y)
  let g ← programToApexGraph e
  IO.println g
