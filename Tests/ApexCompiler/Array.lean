import HouLean

open HouLean Apex Compiler

open Lean Qq

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
#apex_graph fun (x : Array Float) => x

/--
info: Nodes:
  0: x : __null__
  1: y : __null__
  2: array::append_float : array::Append<Float>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /y/__spare__[in]
  5: /y/__spare__[out]
  6: /y/x[in]
  7: /y/x[out]
  8: /array::append_float/array0[in]
  9: /array::append_float/value1[in]
  10: /array::append_float/out0[out]
  11: /array::append_float/out1[out]

Input Ports:
  2: /x/x[in]
  6: /y/x[in]

Output Ports:
  10: /array::append_float/out0[out]

Wires:
  0: /x/x[out] -> /array::append_float/array0[in]
  1: /y/x[out] -> /array::append_float/value1[in]

Literals:
-/
#guard_msgs in 
#apex_graph fun (x : Array Float) (y : Float) => x.push y


/--
info: Nodes:
  0: x : __null__
  1: array::add_float : array::Add<Float>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /array::add_float/rundata[out]
  5: /array::add_float/a0[in]
  6: /array::add_float/b1[in]
  7: /array::add_float/out0[out]

Input Ports:
  2: /x/x[in]

Output Ports:
  7: /array::add_float/out0[out]

Wires:
  0: /x/x[out] -> /array::add_float/a0[in]
  1: /x/x[out] -> /array::add_float/b1[in]

Literals:
-/
#guard_msgs in
#apex_graph fun (x : Array Float) => x + x

/--
info: Nodes:
  0: x : __null__
  1: array::multiply_float : array::Multiply<Float>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /array::multiply_float/rundata[out]
  5: /array::multiply_float/a0[in]
  6: /array::multiply_float/b1[in]
  7: /array::multiply_float/out0[out]

Input Ports:
  2: /x/x[in]

Output Ports:
  7: /array::multiply_float/out0[out]

Wires:
  0: /x/x[out] -> /array::multiply_float/a0[in]
  1: /x/x[out] -> /array::multiply_float/b1[in]

Literals:
-/
#guard_msgs in
#apex_graph fun (x : Array Float) => x * x


set_option trace.HouLean.Apex.compiler true in
#apex_graph fun (x : Array Float) => x[0]?

#apex_graph fun (x : Array Float) => x[0]!

#apex_graph fun (x : Array Float) (i : Nat) (h : i < x.size) => x[i]

/--
info: Nodes:
  0: x : __null__
  1: array::add_float : array::Add<Float>
  2: array::add_float1 : array::Add<Float>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/fst[in]
  3: /x/fst[out]
  4: /x/snd[in]
  5: /x/snd[out]
  6: /array::add_float/rundata[out]
  7: /array::add_float/a0[in]
  8: /array::add_float/b1[in]
  9: /array::add_float/out0[out]
  10: /array::add_float1/rundata[out]
  11: /array::add_float1/a0[in]
  12: /array::add_float1/b1[in]
  13: /array::add_float1/out0[out]

Input Ports:
  2: /x/fst[in]
  4: /x/snd[in]

Output Ports:
  9: /array::add_float/out0[out]
  13: /array::add_float1/out0[out]

Wires:
  0: /x/fst[out] -> /array::add_float/a0[in]
  1: /x/fst[out] -> /array::add_float/b1[in]
  2: /x/snd[out] -> /array::add_float1/a0[in]
  3: /x/snd[out] -> /array::add_float1/b1[in]

Literals:
-/
#guard_msgs in
#apex_graph fun (x : Array (Float × Float)) => x + x

/--
info: Nodes:
  0: x : __null__
  1: a : __null__
  2: i : __null__
  3: array::add_float : array::Add<Float>
  4: array::add_float1 : array::Add<Float>
  5: array::add_int : array::Add<Int>
  6: array::append_float : array::Append<Float>
  7: array::append_float1 : array::Append<Float>
  8: array::append_int : array::Append<Int>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/fst[in]
  3: /x/fst[out]
  4: /x/snd_fst[in]
  5: /x/snd_fst[out]
  6: /x/snd_snd[in]
  7: /x/snd_snd[out]
  8: /a/__spare__[in]
  9: /a/__spare__[out]
  10: /a/x[in]
  11: /a/x[out]
  12: /i/__spare__[in]
  13: /i/__spare__[out]
  14: /i/x[in]
  15: /i/x[out]
  16: /array::add_float/rundata[out]
  17: /array::add_float/a0[in]
  18: /array::add_float/b1[in]
  19: /array::add_float/out0[out]
  20: /array::add_float1/rundata[out]
  21: /array::add_float1/a0[in]
  22: /array::add_float1/b1[in]
  23: /array::add_float1/out0[out]
  24: /array::add_int/rundata[out]
  25: /array::add_int/a0[in]
  26: /array::add_int/b1[in]
  27: /array::add_int/out0[out]
  28: /array::append_float/array0[in]
  29: /array::append_float/value1[in]
  30: /array::append_float/out0[out]
  31: /array::append_float/out1[out]
  32: /array::append_float1/array0[in]
  33: /array::append_float1/value1[in]
  34: /array::append_float1/out0[out]
  35: /array::append_float1/out1[out]
  36: /array::append_int/array0[in]
  37: /array::append_int/value1[in]
  38: /array::append_int/out0[out]
  39: /array::append_int/out1[out]

Input Ports:
  2: /x/fst[in]
  4: /x/snd_fst[in]
  6: /x/snd_snd[in]
  10: /a/x[in]
  14: /i/x[in]

Output Ports:
  30: /array::append_float/out0[out]
  34: /array::append_float1/out0[out]
  38: /array::append_int/out0[out]

Wires:
  0: /x/fst[out] -> /array::add_float/a0[in]
  1: /x/fst[out] -> /array::add_float/b1[in]
  2: /x/snd_fst[out] -> /array::add_float1/a0[in]
  3: /x/snd_fst[out] -> /array::add_float1/b1[in]
  4: /x/snd_snd[out] -> /array::add_int/a0[in]
  5: /x/snd_snd[out] -> /array::add_int/b1[in]
  6: /array::add_float/out0[out] -> /array::append_float/array0[in]
  7: /a/x[out] -> /array::append_float/value1[in]
  8: /array::add_float1/out0[out] -> /array::append_float1/array0[in]
  9: /a/x[out] -> /array::append_float1/value1[in]
  10: /array::add_int/out0[out] -> /array::append_int/array0[in]
  11: /i/x[out] -> /array::append_int/value1[in]

Literals:
-/
#guard_msgs in
#apex_graph fun (x : Array (Float × Float × Int)) (a : Float) (i : Int) => (x + x).push (a,a,i)


