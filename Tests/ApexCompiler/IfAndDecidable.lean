import HouLean

open HouLean Apex Compiler

open Lean Qq

/--
info: Nodes:
  0: LessThanInt : LessThan<Int>
  1: TwoWaySwitchBool : TwoWaySwitch<Bool>

Ports:
  0: /LessThanInt/a[in]
  1: /LessThanInt/b[in]
  2: /LessThanInt/[anonymous][out]
  3: /TwoWaySwitchBool/a[in]
  4: /TwoWaySwitchBool/b[in]
  5: /TwoWaySwitchBool/index[in]
  6: /TwoWaySwitchBool/[anonymous][out]

Inputs:
  x[in] -> #[/LessThanInt/a[in]]
  y[in] -> #[/LessThanInt/b[in]]

Outputs:
  /TwoWaySwitchBool/[anonymous][out] -> [anonymous][out]

Wires:
  0: /LessThanInt/[anonymous][out] -> /TwoWaySwitchBool/index[in]

Literals:
  0: bool "false" -> /TwoWaySwitchBool/a[in] ⏎
  1: bool "true" -> /TwoWaySwitchBool/b[in]
-/
#guard_msgs in
#apex_graph fun x y : Int => decide (x < y)


/--
info: Nodes:
  0: TwoWaySwitchInt : TwoWaySwitch<Int>

Ports:
  0: /TwoWaySwitchInt/a[in]
  1: /TwoWaySwitchInt/b[in]
  2: /TwoWaySwitchInt/index[in]
  3: /TwoWaySwitchInt/[anonymous][out]

Inputs:
  x[in] -> #[/TwoWaySwitchInt/b[in]]
  y[in] -> #[/TwoWaySwitchInt/a[in]]

Outputs:
  /TwoWaySwitchInt/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
  0: bool "true" -> /TwoWaySwitchInt/index[in]
-/
#guard_msgs in
#apex_graph fun x y : Int => if true then x else y -- todo: the if statement should get eliminated!


/--
info: Nodes:
  0: TwoWaySwitchInt : TwoWaySwitch<Int>

Ports:
  0: /TwoWaySwitchInt/a[in]
  1: /TwoWaySwitchInt/b[in]
  2: /TwoWaySwitchInt/index[in]
  3: /TwoWaySwitchInt/[anonymous][out]

Inputs:
  x[in] -> #[/TwoWaySwitchInt/b[in]]
  y[in] -> #[/TwoWaySwitchInt/a[in]]

Outputs:
  /TwoWaySwitchInt/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
  0: bool "false" -> /TwoWaySwitchInt/index[in]
-/
#guard_msgs in
#apex_graph fun x y : Int => if false then x else y -- todo: the if statement should get eliminated!


/--
info: Nodes:
  0: LessThanInt : LessThan<Int>
  1: TwoWaySwitchBool : TwoWaySwitch<Bool>

Ports:
  0: /LessThanInt/a[in]
  1: /LessThanInt/b[in]
  2: /LessThanInt/[anonymous][out]
  3: /TwoWaySwitchBool/a[in]
  4: /TwoWaySwitchBool/b[in]
  5: /TwoWaySwitchBool/index[in]
  6: /TwoWaySwitchBool/[anonymous][out]

Inputs:
  x[in] -> #[/LessThanInt/b[in]]
  y[in] -> #[/LessThanInt/a[in]]

Outputs:
  /TwoWaySwitchBool/[anonymous][out] -> [anonymous][out]

Wires:
  0: /LessThanInt/[anonymous][out] -> /TwoWaySwitchBool/index[in]

Literals:
  0: bool "false" -> /TwoWaySwitchBool/a[in] ⏎
  1: bool "true" -> /TwoWaySwitchBool/b[in]
-/
#guard_msgs in
#apex_graph fun x y : Int => decide (x > y)

/--
info: Nodes:
  0: LessThanOrEqualInt : LessThanOrEqual<Int>
  1: TwoWaySwitchBool : TwoWaySwitch<Bool>

Ports:
  0: /LessThanOrEqualInt/a[in]
  1: /LessThanOrEqualInt/b[in]
  2: /LessThanOrEqualInt/[anonymous][out]
  3: /TwoWaySwitchBool/a[in]
  4: /TwoWaySwitchBool/b[in]
  5: /TwoWaySwitchBool/index[in]
  6: /TwoWaySwitchBool/[anonymous][out]

Inputs:
  x[in] -> #[/LessThanOrEqualInt/a[in]]
  y[in] -> #[/LessThanOrEqualInt/b[in]]

Outputs:
  /TwoWaySwitchBool/[anonymous][out] -> [anonymous][out]

Wires:
  0: /LessThanOrEqualInt/[anonymous][out] -> /TwoWaySwitchBool/index[in]

Literals:
  0: bool "false" -> /TwoWaySwitchBool/a[in] ⏎
  1: bool "true" -> /TwoWaySwitchBool/b[in]
-/
#guard_msgs in
#apex_graph fun x y : Int => decide (x ≤ y)


/--
info: Nodes:
  0: LessThanOrEqualInt : LessThanOrEqual<Int>
  1: TwoWaySwitchBool : TwoWaySwitch<Bool>

Ports:
  0: /LessThanOrEqualInt/a[in]
  1: /LessThanOrEqualInt/b[in]
  2: /LessThanOrEqualInt/[anonymous][out]
  3: /TwoWaySwitchBool/a[in]
  4: /TwoWaySwitchBool/b[in]
  5: /TwoWaySwitchBool/index[in]
  6: /TwoWaySwitchBool/[anonymous][out]

Inputs:
  x[in] -> #[/LessThanOrEqualInt/b[in]]
  y[in] -> #[/LessThanOrEqualInt/a[in]]

Outputs:
  /TwoWaySwitchBool/[anonymous][out] -> [anonymous][out]

Wires:
  0: /LessThanOrEqualInt/[anonymous][out] -> /TwoWaySwitchBool/index[in]

Literals:
  0: bool "false" -> /TwoWaySwitchBool/a[in] ⏎
  1: bool "true" -> /TwoWaySwitchBool/b[in]
-/
#guard_msgs in
#apex_graph fun x y : Int => decide (x ≥ y)


/--
info: Nodes:
  0: EqualsInt : Equals<Int>
  1: TwoWaySwitchBool : TwoWaySwitch<Bool>

Ports:
  0: /EqualsInt/a[in]
  1: /EqualsInt/b[in]
  2: /EqualsInt/[anonymous][out]
  3: /TwoWaySwitchBool/a[in]
  4: /TwoWaySwitchBool/b[in]
  5: /TwoWaySwitchBool/index[in]
  6: /TwoWaySwitchBool/[anonymous][out]

Inputs:
  x[in] -> #[/EqualsInt/a[in]]
  y[in] -> #[/EqualsInt/b[in]]

Outputs:
  /TwoWaySwitchBool/[anonymous][out] -> [anonymous][out]

Wires:
  0: /EqualsInt/[anonymous][out] -> /TwoWaySwitchBool/index[in]

Literals:
  0: bool "false" -> /TwoWaySwitchBool/a[in] ⏎
  1: bool "true" -> /TwoWaySwitchBool/b[in]
-/
#guard_msgs in
#apex_graph fun x y : Int => decide (x = y)


/--
info: Nodes:
  0: MultiplyInt : Multiply<Int>
  1: AddInt : Add<Int>
  2: LessThanInt : LessThan<Int>
  3: TwoWaySwitchInt : TwoWaySwitch<Int>

Ports:
  0: /MultiplyInt/a[in]
  1: /MultiplyInt/b[⋯][in]
  2: /MultiplyInt/[anonymous][out]
  3: /AddInt/a[in]
  4: /AddInt/b[⋯][in]
  5: /AddInt/[anonymous][out]
  6: /LessThanInt/a[in]
  7: /LessThanInt/b[in]
  8: /LessThanInt/[anonymous][out]
  9: /TwoWaySwitchInt/a[in]
  10: /TwoWaySwitchInt/b[in]
  11: /TwoWaySwitchInt/index[in]
  12: /TwoWaySwitchInt/[anonymous][out]

Inputs:
  x[in] -> #[/MultiplyInt/a[in], /AddInt/a[in], /LessThanInt/a[in]]
  y[in] -> #[/MultiplyInt/b[0][in], /AddInt/b[0][in], /LessThanInt/b[in]]

Outputs:
  /TwoWaySwitchInt/[anonymous][out] -> [anonymous][out]

Wires:
  0: /MultiplyInt/[anonymous][out] -> /TwoWaySwitchInt/a[in]
  1: /AddInt/[anonymous][out] -> /TwoWaySwitchInt/b[in]
  2: /LessThanInt/[anonymous][out] -> /TwoWaySwitchInt/index[in]

Literals:
-/
#guard_msgs in
#apex_graph fun x y : Int => if x < y then x + y else x * y


/--
info: Nodes:
  0: MultiplyInt : Multiply<Int>
  1: AddInt : Add<Int>
  2: LessThanInt : LessThan<Int>
  3: TwoWaySwitchInt : TwoWaySwitch<Int>

Ports:
  0: /MultiplyInt/a[in]
  1: /MultiplyInt/b[⋯][in]
  2: /MultiplyInt/[anonymous][out]
  3: /AddInt/a[in]
  4: /AddInt/b[⋯][in]
  5: /AddInt/[anonymous][out]
  6: /LessThanInt/a[in]
  7: /LessThanInt/b[in]
  8: /LessThanInt/[anonymous][out]
  9: /TwoWaySwitchInt/a[in]
  10: /TwoWaySwitchInt/b[in]
  11: /TwoWaySwitchInt/index[in]
  12: /TwoWaySwitchInt/[anonymous][out]

Inputs:
  x[in] -> #[/MultiplyInt/a[in], /AddInt/a[in], /LessThanInt/a[in]]
  y[in] -> #[/MultiplyInt/b[0][in], /AddInt/b[0][in], /LessThanInt/b[in]]

Outputs:
  /TwoWaySwitchInt/[anonymous][out] -> [anonymous][out]

Wires:
  0: /MultiplyInt/[anonymous][out] -> /TwoWaySwitchInt/a[in]
  1: /AddInt/[anonymous][out] -> /TwoWaySwitchInt/b[in]
  2: /LessThanInt/[anonymous][out] -> /TwoWaySwitchInt/index[in]

Literals:
-/
#guard_msgs in
#apex_graph fun x y : Int => if _ : x < y then x + y else x * y


/--
info: Nodes:
  0: LessThanFloat : LessThan<Float>
  1: TwoWaySwitchFloat : TwoWaySwitch<Float>
  2: TwoWaySwitchFloat1 : TwoWaySwitch<Float>
  3: TwoWaySwitchFloat2 : TwoWaySwitch<Float>
  4: TwoWaySwitchFloat3 : TwoWaySwitch<Float>

Ports:
  0: /LessThanFloat/a[in]
  1: /LessThanFloat/b[in]
  2: /LessThanFloat/[anonymous][out]
  3: /TwoWaySwitchFloat/a[in]
  4: /TwoWaySwitchFloat/b[in]
  5: /TwoWaySwitchFloat/index[in]
  6: /TwoWaySwitchFloat/[anonymous][out]
  7: /TwoWaySwitchFloat1/a[in]
  8: /TwoWaySwitchFloat1/b[in]
  9: /TwoWaySwitchFloat1/index[in]
  10: /TwoWaySwitchFloat1/[anonymous][out]
  11: /TwoWaySwitchFloat2/a[in]
  12: /TwoWaySwitchFloat2/b[in]
  13: /TwoWaySwitchFloat2/index[in]
  14: /TwoWaySwitchFloat2/[anonymous][out]
  15: /TwoWaySwitchFloat3/a[in]
  16: /TwoWaySwitchFloat3/b[in]
  17: /TwoWaySwitchFloat3/index[in]
  18: /TwoWaySwitchFloat3/[anonymous][out]

Inputs:
  x.fst[in] -> #[/LessThanFloat/a[in], /TwoWaySwitchFloat/b[in], /TwoWaySwitchFloat2/a[in]]
  x.snd[in] -> #[/TwoWaySwitchFloat1/b[in], /TwoWaySwitchFloat3/a[in]]
  y.fst[in] -> #[/TwoWaySwitchFloat/a[in], /TwoWaySwitchFloat2/b[in]]
  y.snd[in] -> #[/LessThanFloat/b[in], /TwoWaySwitchFloat1/a[in], /TwoWaySwitchFloat3/b[in]]

Outputs:
  /TwoWaySwitchFloat/[anonymous][out] -> fst.fst[out]
  /TwoWaySwitchFloat1/[anonymous][out] -> fst.snd[out]
  /TwoWaySwitchFloat2/[anonymous][out] -> snd.fst[out]
  /TwoWaySwitchFloat3/[anonymous][out] -> snd.snd[out]

Wires:
  0: /LessThanFloat/[anonymous][out] -> /TwoWaySwitchFloat/index[in]
  1: /LessThanFloat/[anonymous][out] -> /TwoWaySwitchFloat1/index[in]
  2: /LessThanFloat/[anonymous][out] -> /TwoWaySwitchFloat2/index[in]
  3: /LessThanFloat/[anonymous][out] -> /TwoWaySwitchFloat3/index[in]

Literals:
-/
#guard_msgs in
#apex_graph fun x y : Float×Float => if x.1 < y.2 then (x,y) else (y,x)
