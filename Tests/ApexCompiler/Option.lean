import HouLean

open HouLean Apex Compiler Qq


/--
info: Nodes:
  0: fst : Value<Int>
  1: snd : Value<Bool>

Ports:
  0: /fst/parm[in]
  1: /fst/[anonymous][out]
  2: /snd/parm[in]
  3: /snd/[anonymous][out]

Inputs:
  x[in] -> #[/fst/parm[in]]

Outputs:
  /fst/[anonymous][out] -> fst[out]
  /snd/[anonymous][out] -> snd[out]

Wires:

Literals:
  0: bool "true" -> /snd/parm[in]
-/
#guard_msgs in
#apex_graph fun x : Int => some x


/--
info: Nodes:
  0: fst : Value<Int>
  1: snd : Value<Bool>

Ports:
  0: /fst/parm[in]
  1: /fst/[anonymous][out]
  2: /snd/parm[in]
  3: /snd/[anonymous][out]

Inputs:
  x.fst[in] -> #[/fst/parm[in]]
  x.snd[in] -> #[/snd/parm[in]]

Outputs:
  /fst/[anonymous][out] -> fst[out]
  /snd/[anonymous][out] -> snd[out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun x : Option Int => x


/--
info: Nodes:
  0: IfBegin : IfBegin
  1: IfEnd : IfEnd
  2: TwoWaySwitchBool : TwoWaySwitch<Bool>
  3: IfBegin1 : IfBegin
  4: IfEnd1 : IfEnd
  5: value : Value<Int>

Ports:
  0: /IfBegin/[anonymous][out]
  1: /IfBegin/condition[in]
  2: /IfBegin/spare[⋯][in]
  3: /IfBegin/fst[out]
  4: /IfBegin/snd[⋯][out]
  5: /IfEnd/[anonymous][out]
  6: /IfEnd/scope[in]
  7: /IfEnd/spare[⋯][in]
  8: /IfEnd/[anonymous][⋯][out]
  9: /TwoWaySwitchBool/a[in]
  10: /TwoWaySwitchBool/b[in]
  11: /TwoWaySwitchBool/index[in]
  12: /TwoWaySwitchBool/[anonymous][out]
  13: /IfBegin1/[anonymous][out]
  14: /IfBegin1/condition[in]
  15: /IfBegin1/spare[⋯][in]
  16: /IfBegin1/fst[out]
  17: /IfBegin1/snd[⋯][out]
  18: /IfEnd1/[anonymous][out]
  19: /IfEnd1/scope[in]
  20: /IfEnd1/spare[⋯][in]
  21: /IfEnd1/[anonymous][⋯][out]
  22: /value/parm[in]
  23: /value/value[out]

Inputs:
  x.fst[in] -> #[/IfBegin/spare[0][in], /IfBegin/spare[1][in]]
  x.snd[in] -> #[/IfBegin/condition[in], /IfBegin/spare[2][in], /TwoWaySwitchBool/index[in]]

Outputs:
  /IfEnd1/[anonymous][0][out] -> «0»[out]

Wires:
  0: /IfBegin/fst[out] -> /IfEnd/scope[in]
  1: /IfBegin/snd[0][out] -> /IfEnd/spare[0][in]
  2: /IfBegin/snd[1][out] -> /IfEnd/spare[1][in]
  3: /IfBegin/snd[2][out] -> /IfEnd/spare[2][in]
  4: /TwoWaySwitchBool/[anonymous][out] -> /IfBegin1/condition[in]
  5: /IfEnd/[anonymous][0][out] -> /IfBegin1/spare[0][in]
  6: /IfEnd/[anonymous][1][out] -> /IfBegin1/spare[1][in]
  7: /IfEnd/[anonymous][2][out] -> /IfBegin1/spare[2][in]
  8: /IfBegin1/fst[out] -> /IfEnd1/scope[in]
  9: /value/value[out] -> /IfEnd1/spare[0][in]
  10: /IfBegin1/snd[1][out] -> /IfEnd1/spare[1][in]
  11: /IfBegin1/snd[2][out] -> /IfEnd1/spare[2][in]

Literals:
  0: bool "true" -> /TwoWaySwitchBool/a[in] ⏎
  1: bool "false" -> /TwoWaySwitchBool/b[in] ⏎
  2: int 0 -> /value/parm[in]
-/
#guard_msgs in
#apex_graph fun x : Option Int => x.getD default



/--
info: Nodes:
  0: Multiply<Int>1 : Multiply<Int>
  1: TwoWaySwitch<Int>1 : TwoWaySwitch<Int>
  2: TwoWaySwitch<Bool>1 : TwoWaySwitch<Bool>
  3: TwoWaySwitch<Int>2 : TwoWaySwitch<Int>
  4: TwoWaySwitch<Bool>2 : TwoWaySwitch<Bool>

Ports:
  0: /Multiply<Int>1/a[in]
  1: /Multiply<Int>1/b[in]
  2: /Multiply<Int>1/[anonymous][out]
  3: /TwoWaySwitch<Int>1/a[in]
  4: /TwoWaySwitch<Int>1/b[in]
  5: /TwoWaySwitch<Int>1/index[in]
  6: /TwoWaySwitch<Int>1/[anonymous][out]
  7: /TwoWaySwitch<Bool>1/a[in]
  8: /TwoWaySwitch<Bool>1/b[in]
  9: /TwoWaySwitch<Bool>1/index[in]
  10: /TwoWaySwitch<Bool>1/[anonymous][out]
  11: /TwoWaySwitch<Int>2/a[in]
  12: /TwoWaySwitch<Int>2/b[in]
  13: /TwoWaySwitch<Int>2/index[in]
  14: /TwoWaySwitch<Int>2/[anonymous][out]
  15: /TwoWaySwitch<Bool>2/a[in]
  16: /TwoWaySwitch<Bool>2/b[in]
  17: /TwoWaySwitch<Bool>2/index[in]
  18: /TwoWaySwitch<Bool>2/[anonymous][out]

Inputs:
  x.fst[in] -> #[/Multiply<Int>1/a[in]]
  x.snd[in] -> #[/TwoWaySwitch<Int>2/index[in], /TwoWaySwitch<Bool>2/index[in]]
  y.fst[in] -> #[/Multiply<Int>1/b[in]]
  y.snd[in] -> #[/TwoWaySwitch<Int>1/index[in], /TwoWaySwitch<Bool>1/index[in]]

Outputs:
  /TwoWaySwitch<Int>2/[anonymous][out] -> fst[out]
  /TwoWaySwitch<Bool>2/[anonymous][out] -> snd[out]

Wires:
  0: /Multiply<Int>1/[anonymous][out] -> /TwoWaySwitch<Int>1/b[in]
  1: /TwoWaySwitch<Int>1/[anonymous][out] -> /TwoWaySwitch<Int>2/b[in]
  2: /TwoWaySwitch<Bool>1/[anonymous][out] -> /TwoWaySwitch<Bool>2/b[in]

Literals:
  0: int 0 -> 3 ⏎
  1: bool "false" -> 7 ⏎
  2: bool "true" -> 8 ⏎
  3: int 0 -> 11 ⏎
  4: bool "false" -> 15
-/
#guard_msgs in
#apex_graph fun x y : Option Int => do
  let x ← x
  let y ← y
  return x * y


/--
info: Nodes:
  0: IfBegin : IfBegin
  1: AddInt : Add<Int>
  2: value : Value<Int>
  3: IfEnd : IfEnd
  4: value : Value<Bool>
  5: TwoWaySwitchBool : TwoWaySwitch<Bool>
  6: IfBegin1 : IfBegin
  7: IfEnd1 : IfEnd
  8: value : Value<Int>
  9: value : Value<Bool>

Ports:
  0: /IfBegin/[anonymous][out]
  1: /IfBegin/condition[in]
  2: /IfBegin/spare[⋯][in]
  3: /IfBegin/fst[out]
  4: /IfBegin/snd[⋯][out]
  5: /AddInt/a[in]
  6: /AddInt/b[⋯][in]
  7: /AddInt/[anonymous][out]
  8: /value/parm[in]
  9: /value/value[out]
  10: /IfEnd/[anonymous][out]
  11: /IfEnd/scope[in]
  12: /IfEnd/spare[⋯][in]
  13: /IfEnd/[anonymous][⋯][out]
  14: /value/parm[in]
  15: /value/value[out]
  16: /TwoWaySwitchBool/a[in]
  17: /TwoWaySwitchBool/b[in]
  18: /TwoWaySwitchBool/index[in]
  19: /TwoWaySwitchBool/[anonymous][out]
  20: /IfBegin1/[anonymous][out]
  21: /IfBegin1/condition[in]
  22: /IfBegin1/spare[⋯][in]
  23: /IfBegin1/fst[out]
  24: /IfBegin1/snd[⋯][out]
  25: /IfEnd1/[anonymous][out]
  26: /IfEnd1/scope[in]
  27: /IfEnd1/spare[⋯][in]
  28: /IfEnd1/[anonymous][⋯][out]
  29: /value/parm[in]
  30: /value/value[out]
  31: /value/parm[in]
  32: /value/value[out]

Inputs:
  x.fst[in] -> #[/IfBegin/spare[0][in], /IfBegin/spare[1][in]]
  x.snd[in] -> #[/IfBegin/condition[in], /IfBegin/spare[2][in], /TwoWaySwitchBool/index[in]]

Outputs:
  /IfEnd1/[anonymous][1][out] -> fst1[out]
  /IfEnd1/[anonymous][2][out] -> snd2[out]

Wires:
  0: /IfBegin/snd[0][out] -> /AddInt/a[in]
  1: /value/value[out] -> /AddInt/b[0][in]
  2: /IfBegin/fst[out] -> /IfEnd/scope[in]
  3: /IfBegin/snd[0][out] -> /IfEnd/spare[0][in]
  4: /AddInt/[anonymous][out] -> /IfEnd/spare[1][in]
  5: /value/value[out] -> /IfEnd/spare[2][in]
  6: /TwoWaySwitchBool/[anonymous][out] -> /IfBegin1/condition[in]
  7: /IfEnd/[anonymous][0][out] -> /IfBegin1/spare[0][in]
  8: /IfEnd/[anonymous][1][out] -> /IfBegin1/spare[1][in]
  9: /IfEnd/[anonymous][2][out] -> /IfBegin1/spare[2][in]
  10: /IfBegin1/fst[out] -> /IfEnd1/scope[in]
  11: /IfBegin1/snd[0][out] -> /IfEnd1/spare[0][in]
  12: /value/value[out] -> /IfEnd1/spare[1][in]
  13: /value/value[out] -> /IfEnd1/spare[2][in]

Literals:
  0: int 1 -> /value/parm[in] ⏎
  1: bool "true" -> /value/parm[in] ⏎
  2: bool "true" -> /TwoWaySwitchBool/a[in] ⏎
  3: bool "false" -> /TwoWaySwitchBool/b[in] ⏎
  4: int 0 -> /value/parm[in] ⏎
  5: bool "false" -> /value/parm[in]
-/
#guard_msgs in
#apex_graph fun x : Option Int => x.map (· + 1)



open Qq
open Lean
