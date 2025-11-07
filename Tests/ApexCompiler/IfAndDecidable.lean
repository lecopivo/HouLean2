import HouLean

open HouLean Apex Compiler

open Lean Qq

/--
info: Nodes:
  0: LessThan<Int>1 : LessThan<Int>
  1: TwoWaySwitch<Bool>1 : TwoWaySwitch<Bool>

Ports:
  0: /LessThan<Int>1/a[in]
  1: /LessThan<Int>1/b[in]
  2: /LessThan<Int>1/[anonymous][out]
  3: /TwoWaySwitch<Bool>1/a[in]
  4: /TwoWaySwitch<Bool>1/b[in]
  5: /TwoWaySwitch<Bool>1/index[in]
  6: /TwoWaySwitch<Bool>1/[anonymous][out]

Inputs:
  x[in] -> #[/LessThan<Int>1/a[in]]
  y[in] -> #[/LessThan<Int>1/b[in]]

Outputs:
  /TwoWaySwitch<Bool>1/[anonymous][out] -> [anonymous][out]

Wires:
  0: /LessThan<Int>1/[anonymous][out] -> /TwoWaySwitch<Bool>1/index[in]

Literals:
  0: bool "false" -> 3 ⏎
  1: bool "true" -> 4
-/
#guard_msgs in
#apex_graph fun x y : Int => decide (x < y)


/--
info: Nodes:
  0: LessThan<Int>1 : LessThan<Int>
  1: TwoWaySwitch<Bool>1 : TwoWaySwitch<Bool>

Ports:
  0: /LessThan<Int>1/a[in]
  1: /LessThan<Int>1/b[in]
  2: /LessThan<Int>1/[anonymous][out]
  3: /TwoWaySwitch<Bool>1/a[in]
  4: /TwoWaySwitch<Bool>1/b[in]
  5: /TwoWaySwitch<Bool>1/index[in]
  6: /TwoWaySwitch<Bool>1/[anonymous][out]

Inputs:
  x[in] -> #[/LessThan<Int>1/b[in]]
  y[in] -> #[/LessThan<Int>1/a[in]]

Outputs:
  /TwoWaySwitch<Bool>1/[anonymous][out] -> [anonymous][out]

Wires:
  0: /LessThan<Int>1/[anonymous][out] -> /TwoWaySwitch<Bool>1/index[in]

Literals:
  0: bool "false" -> 3 ⏎
  1: bool "true" -> 4
-/
#guard_msgs in
#apex_graph fun x y : Int => decide (x > y)

/--
info: Nodes:
  0: LessThanOrEqual<Int>1 : LessThanOrEqual<Int>
  1: TwoWaySwitch<Bool>1 : TwoWaySwitch<Bool>

Ports:
  0: /LessThanOrEqual<Int>1/a[in]
  1: /LessThanOrEqual<Int>1/b[in]
  2: /LessThanOrEqual<Int>1/[anonymous][out]
  3: /TwoWaySwitch<Bool>1/a[in]
  4: /TwoWaySwitch<Bool>1/b[in]
  5: /TwoWaySwitch<Bool>1/index[in]
  6: /TwoWaySwitch<Bool>1/[anonymous][out]

Inputs:
  x[in] -> #[/LessThanOrEqual<Int>1/a[in]]
  y[in] -> #[/LessThanOrEqual<Int>1/b[in]]

Outputs:
  /TwoWaySwitch<Bool>1/[anonymous][out] -> [anonymous][out]

Wires:
  0: /LessThanOrEqual<Int>1/[anonymous][out] -> /TwoWaySwitch<Bool>1/index[in]

Literals:
  0: bool "false" -> 3 ⏎
  1: bool "true" -> 4
-/
#guard_msgs in
#apex_graph fun x y : Int => decide (x ≤ y)


/--
info: Nodes:
  0: LessThanOrEqual<Int>1 : LessThanOrEqual<Int>
  1: TwoWaySwitch<Bool>1 : TwoWaySwitch<Bool>

Ports:
  0: /LessThanOrEqual<Int>1/a[in]
  1: /LessThanOrEqual<Int>1/b[in]
  2: /LessThanOrEqual<Int>1/[anonymous][out]
  3: /TwoWaySwitch<Bool>1/a[in]
  4: /TwoWaySwitch<Bool>1/b[in]
  5: /TwoWaySwitch<Bool>1/index[in]
  6: /TwoWaySwitch<Bool>1/[anonymous][out]

Inputs:
  x[in] -> #[/LessThanOrEqual<Int>1/b[in]]
  y[in] -> #[/LessThanOrEqual<Int>1/a[in]]

Outputs:
  /TwoWaySwitch<Bool>1/[anonymous][out] -> [anonymous][out]

Wires:
  0: /LessThanOrEqual<Int>1/[anonymous][out] -> /TwoWaySwitch<Bool>1/index[in]

Literals:
  0: bool "false" -> 3 ⏎
  1: bool "true" -> 4
-/
#guard_msgs in
#apex_graph fun x y : Int => decide (x ≥ y)


/--
info: Nodes:
  0: Equals<Int>1 : Equals<Int>
  1: TwoWaySwitch<Bool>1 : TwoWaySwitch<Bool>

Ports:
  0: /Equals<Int>1/a[in]
  1: /Equals<Int>1/b[in]
  2: /Equals<Int>1/[anonymous][out]
  3: /TwoWaySwitch<Bool>1/a[in]
  4: /TwoWaySwitch<Bool>1/b[in]
  5: /TwoWaySwitch<Bool>1/index[in]
  6: /TwoWaySwitch<Bool>1/[anonymous][out]

Inputs:
  x[in] -> #[/Equals<Int>1/a[in]]
  y[in] -> #[/Equals<Int>1/b[in]]

Outputs:
  /TwoWaySwitch<Bool>1/[anonymous][out] -> [anonymous][out]

Wires:
  0: /Equals<Int>1/[anonymous][out] -> /TwoWaySwitch<Bool>1/index[in]

Literals:
  0: bool "false" -> 3 ⏎
  1: bool "true" -> 4
-/
#guard_msgs in
#apex_graph fun x y : Int => decide (x = y)


/--
info: Nodes:
  0: Multiply<Int>1 : Multiply<Int>
  1: Add<Int>1 : Add<Int>
  2: LessThan<Int>1 : LessThan<Int>
  3: TwoWaySwitch<Int>1 : TwoWaySwitch<Int>

Ports:
  0: /Multiply<Int>1/a[in]
  1: /Multiply<Int>1/b[in]
  2: /Multiply<Int>1/[anonymous][out]
  3: /Add<Int>1/a[in]
  4: /Add<Int>1/b[in]
  5: /Add<Int>1/[anonymous][out]
  6: /LessThan<Int>1/a[in]
  7: /LessThan<Int>1/b[in]
  8: /LessThan<Int>1/[anonymous][out]
  9: /TwoWaySwitch<Int>1/a[in]
  10: /TwoWaySwitch<Int>1/b[in]
  11: /TwoWaySwitch<Int>1/index[in]
  12: /TwoWaySwitch<Int>1/[anonymous][out]

Inputs:
  x[in] -> #[/Multiply<Int>1/a[in], /Add<Int>1/a[in], /LessThan<Int>1/a[in]]
  y[in] -> #[/Multiply<Int>1/b[in], /Add<Int>1/b[in], /LessThan<Int>1/b[in]]

Outputs:
  /TwoWaySwitch<Int>1/[anonymous][out] -> [anonymous][out]

Wires:
  0: /Multiply<Int>1/[anonymous][out] -> /TwoWaySwitch<Int>1/a[in]
  1: /Add<Int>1/[anonymous][out] -> /TwoWaySwitch<Int>1/b[in]
  2: /LessThan<Int>1/[anonymous][out] -> /TwoWaySwitch<Int>1/index[in]

Literals:
-/
#guard_msgs in
#apex_graph fun x y : Int => if x < y then x + y else x * y


/--
info: Nodes:
  0: Multiply<Int>1 : Multiply<Int>
  1: Add<Int>1 : Add<Int>
  2: LessThan<Int>1 : LessThan<Int>
  3: TwoWaySwitch<Int>1 : TwoWaySwitch<Int>

Ports:
  0: /Multiply<Int>1/a[in]
  1: /Multiply<Int>1/b[in]
  2: /Multiply<Int>1/[anonymous][out]
  3: /Add<Int>1/a[in]
  4: /Add<Int>1/b[in]
  5: /Add<Int>1/[anonymous][out]
  6: /LessThan<Int>1/a[in]
  7: /LessThan<Int>1/b[in]
  8: /LessThan<Int>1/[anonymous][out]
  9: /TwoWaySwitch<Int>1/a[in]
  10: /TwoWaySwitch<Int>1/b[in]
  11: /TwoWaySwitch<Int>1/index[in]
  12: /TwoWaySwitch<Int>1/[anonymous][out]

Inputs:
  x[in] -> #[/Multiply<Int>1/a[in], /Add<Int>1/a[in], /LessThan<Int>1/a[in]]
  y[in] -> #[/Multiply<Int>1/b[in], /Add<Int>1/b[in], /LessThan<Int>1/b[in]]

Outputs:
  /TwoWaySwitch<Int>1/[anonymous][out] -> [anonymous][out]

Wires:
  0: /Multiply<Int>1/[anonymous][out] -> /TwoWaySwitch<Int>1/a[in]
  1: /Add<Int>1/[anonymous][out] -> /TwoWaySwitch<Int>1/b[in]
  2: /LessThan<Int>1/[anonymous][out] -> /TwoWaySwitch<Int>1/index[in]

Literals:
-/
#guard_msgs in
#apex_graph fun x y : Int => if _ : x < y then x + y else x * y


/--
info: Nodes:
  0: LessThan<Float>1 : LessThan<Float>
  1: TwoWaySwitch<Float>1 : TwoWaySwitch<Float>
  2: TwoWaySwitch<Float>2 : TwoWaySwitch<Float>
  3: TwoWaySwitch<Float>3 : TwoWaySwitch<Float>
  4: TwoWaySwitch<Float>4 : TwoWaySwitch<Float>

Ports:
  0: /LessThan<Float>1/a[in]
  1: /LessThan<Float>1/b[in]
  2: /LessThan<Float>1/[anonymous][out]
  3: /TwoWaySwitch<Float>1/a[in]
  4: /TwoWaySwitch<Float>1/b[in]
  5: /TwoWaySwitch<Float>1/index[in]
  6: /TwoWaySwitch<Float>1/[anonymous][out]
  7: /TwoWaySwitch<Float>2/a[in]
  8: /TwoWaySwitch<Float>2/b[in]
  9: /TwoWaySwitch<Float>2/index[in]
  10: /TwoWaySwitch<Float>2/[anonymous][out]
  11: /TwoWaySwitch<Float>3/a[in]
  12: /TwoWaySwitch<Float>3/b[in]
  13: /TwoWaySwitch<Float>3/index[in]
  14: /TwoWaySwitch<Float>3/[anonymous][out]
  15: /TwoWaySwitch<Float>4/a[in]
  16: /TwoWaySwitch<Float>4/b[in]
  17: /TwoWaySwitch<Float>4/index[in]
  18: /TwoWaySwitch<Float>4/[anonymous][out]

Inputs:
  x.fst[in] -> #[/LessThan<Float>1/a[in], /TwoWaySwitch<Float>1/b[in], /TwoWaySwitch<Float>3/a[in]]
  x.snd[in] -> #[/TwoWaySwitch<Float>2/b[in], /TwoWaySwitch<Float>4/a[in]]
  y.fst[in] -> #[/TwoWaySwitch<Float>1/a[in], /TwoWaySwitch<Float>3/b[in]]
  y.snd[in] -> #[/LessThan<Float>1/b[in], /TwoWaySwitch<Float>2/a[in], /TwoWaySwitch<Float>4/b[in]]

Outputs:
  /TwoWaySwitch<Float>1/[anonymous][out] -> fst.fst[out]
  /TwoWaySwitch<Float>2/[anonymous][out] -> fst.snd[out]
  /TwoWaySwitch<Float>3/[anonymous][out] -> snd.fst[out]
  /TwoWaySwitch<Float>4/[anonymous][out] -> snd.snd[out]

Wires:
  0: /LessThan<Float>1/[anonymous][out] -> /TwoWaySwitch<Float>1/index[in]
  1: /LessThan<Float>1/[anonymous][out] -> /TwoWaySwitch<Float>2/index[in]
  2: /LessThan<Float>1/[anonymous][out] -> /TwoWaySwitch<Float>3/index[in]
  3: /LessThan<Float>1/[anonymous][out] -> /TwoWaySwitch<Float>4/index[in]

Literals:
-/
#guard_msgs in
#apex_graph fun x y : Float×Float => if x.1 < y.2 then (x,y) else (y,x)
