import HouLean
import HouLean.Apex.Data.UInt64

open HouLean Apex Compiler

open Lean Qq

/--
info: Nodes:

Ports:

Inputs:
  x[in] -> #[]

Outputs:
  x[in] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun (x : Array Float) => x

/--
info: Nodes:
  0: array::Append<Float>1 : array::Append<Float>

Ports:
  0: /array::Append<Float>1/array[in]
  1: /array::Append<Float>1/value[in]
  2: /array::Append<Float>1/fst[out]
  3: /array::Append<Float>1/snd[out]

Inputs:
  x[in] -> #[/array::Append<Float>1/array[in]]
  y[in] -> #[/array::Append<Float>1/value[in]]

Outputs:
  /array::Append<Float>1/fst[out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in 
#apex_graph fun (x : Array Float) (y : Float) => x.push y


/--
info: Nodes:
  0: array::Add<Float>1 : array::Add<Float>

Ports:
  0: /array::Add<Float>1/[anonymous][out]
  1: /array::Add<Float>1/a[in]
  2: /array::Add<Float>1/b[in]
  3: /array::Add<Float>1/[anonymous][out]

Inputs:
  x[in] -> #[/array::Add<Float>1/a[in], /array::Add<Float>1/b[in]]

Outputs:
  /array::Add<Float>1/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun (x : Array Float) => x + x

/--
info: Nodes:
  0: array::Multiply<Float>1 : array::Multiply<Float>

Ports:
  0: /array::Multiply<Float>1/[anonymous][out]
  1: /array::Multiply<Float>1/a[in]
  2: /array::Multiply<Float>1/b[in]
  3: /array::Multiply<Float>1/[anonymous][out]

Inputs:
  x[in] -> #[/array::Multiply<Float>1/a[in], /array::Multiply<Float>1/b[in]]

Outputs:
  /array::Multiply<Float>1/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun (x : Array Float) => x * x


/--
info: Nodes:
  0: Convert<Int,Float>1 : Convert<Int,Float>
  1: array::Get<Float>1 : array::Get<Float>
  2: array::Length<Float>1 : array::Length<Float>
  3: Max<Int>1 : Max<Int>
  4: LessThan<Int>1 : LessThan<Int>
  5: TwoWaySwitch<Bool>1 : TwoWaySwitch<Bool>
  6: TwoWaySwitch<Float>1 : TwoWaySwitch<Float>
  7: TwoWaySwitch<Bool>2 : TwoWaySwitch<Bool>

Ports:
  0: /Convert<Int,Float>1/a[in]
  1: /Convert<Int,Float>1/[anonymous][out]
  2: /array::Get<Float>1/array[in]
  3: /array::Get<Float>1/index[in]
  4: /array::Get<Float>1/default[in]
  5: /array::Get<Float>1/fst[out]
  6: /array::Get<Float>1/snd[out]
  7: /array::Length<Float>1/array[in]
  8: /array::Length<Float>1/[anonymous][out]
  9: /Max<Int>1/a[in]
  10: /Max<Int>1/b[in]
  11: /Max<Int>1/[anonymous][out]
  12: /LessThan<Int>1/a[in]
  13: /LessThan<Int>1/b[in]
  14: /LessThan<Int>1/[anonymous][out]
  15: /TwoWaySwitch<Bool>1/a[in]
  16: /TwoWaySwitch<Bool>1/b[in]
  17: /TwoWaySwitch<Bool>1/index[in]
  18: /TwoWaySwitch<Bool>1/[anonymous][out]
  19: /TwoWaySwitch<Float>1/a[in]
  20: /TwoWaySwitch<Float>1/b[in]
  21: /TwoWaySwitch<Float>1/index[in]
  22: /TwoWaySwitch<Float>1/[anonymous][out]
  23: /TwoWaySwitch<Bool>2/a[in]
  24: /TwoWaySwitch<Bool>2/b[in]
  25: /TwoWaySwitch<Bool>2/index[in]
  26: /TwoWaySwitch<Bool>2/[anonymous][out]

Inputs:
  x[in] -> #[/array::Get<Float>1/array[in], /array::Length<Float>1/array[in]]

Outputs:
  /TwoWaySwitch<Float>1/[anonymous][out] -> fst[out]
  /TwoWaySwitch<Bool>2/[anonymous][out] -> snd[out]

Wires:
  0: /Convert<Int,Float>1/[anonymous][out] -> /array::Get<Float>1/default[in]
  1: /array::Length<Float>1/[anonymous][out] -> /Max<Int>1/a[in]
  2: /Max<Int>1/[anonymous][out] -> /LessThan<Int>1/b[in]
  3: /LessThan<Int>1/[anonymous][out] -> /TwoWaySwitch<Bool>1/index[in]
  4: /Convert<Int,Float>1/[anonymous][out] -> /TwoWaySwitch<Float>1/a[in]
  5: /array::Get<Float>1/fst[out] -> /TwoWaySwitch<Float>1/b[in]
  6: /TwoWaySwitch<Bool>1/[anonymous][out] -> /TwoWaySwitch<Float>1/index[in]
  7: /TwoWaySwitch<Bool>1/[anonymous][out] -> /TwoWaySwitch<Bool>2/index[in]

Literals:
  0: int 0 -> 0 ⏎
  1: int 0 -> 3 ⏎
  2: int 0 -> 10 ⏎
  3: int 0 -> 12 ⏎
  4: bool "false" -> 15 ⏎
  5: bool "true" -> 16 ⏎
  6: bool "false" -> 23 ⏎
  7: bool "true" -> 24
-/
#guard_msgs in
#apex_graph fun (x : Array Float) => x[0]?

/--
info: Nodes:
  0: Convert<Int,Float>1 : Convert<Int,Float>
  1: array::Get<Float>1 : array::Get<Float>

Ports:
  0: /Convert<Int,Float>1/a[in]
  1: /Convert<Int,Float>1/[anonymous][out]
  2: /array::Get<Float>1/array[in]
  3: /array::Get<Float>1/index[in]
  4: /array::Get<Float>1/default[in]
  5: /array::Get<Float>1/fst[out]
  6: /array::Get<Float>1/snd[out]

Inputs:
  x[in] -> #[/array::Get<Float>1/array[in]]
  i[in] -> #[/array::Get<Float>1/index[in]]

Outputs:
  /array::Get<Float>1/fst[out] -> [anonymous][out]

Wires:
  0: /Convert<Int,Float>1/[anonymous][out] -> /array::Get<Float>1/default[in]

Literals:
  0: int 0 -> 0
-/
#guard_msgs in
#apex_graph fun (x : Array Float) (i : Nat) => x[i]!


/--
info: Nodes:
  0: array::Add<Float>1 : array::Add<Float>
  1: array::Add<Float>2 : array::Add<Float>

Ports:
  0: /array::Add<Float>1/[anonymous][out]
  1: /array::Add<Float>1/a[in]
  2: /array::Add<Float>1/b[in]
  3: /array::Add<Float>1/[anonymous][out]
  4: /array::Add<Float>2/[anonymous][out]
  5: /array::Add<Float>2/a[in]
  6: /array::Add<Float>2/b[in]
  7: /array::Add<Float>2/[anonymous][out]

Inputs:
  x.fst[in] -> #[/array::Add<Float>1/a[in], /array::Add<Float>1/b[in]]
  x.snd[in] -> #[/array::Add<Float>2/a[in], /array::Add<Float>2/b[in]]

Outputs:
  /array::Add<Float>1/[anonymous][out] -> fst[out]
  /array::Add<Float>2/[anonymous][out] -> snd[out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun (x : Array (Float × Float)) => x + x

/--
info: Nodes:
  0: array::Add<Float>1 : array::Add<Float>
  1: array::Add<Float>2 : array::Add<Float>
  2: array::Add<Int>1 : array::Add<Int>
  3: array::Append<Float>1 : array::Append<Float>
  4: array::Append<Float>2 : array::Append<Float>
  5: array::Append<Int>1 : array::Append<Int>

Ports:
  0: /array::Add<Float>1/[anonymous][out]
  1: /array::Add<Float>1/a[in]
  2: /array::Add<Float>1/b[in]
  3: /array::Add<Float>1/[anonymous][out]
  4: /array::Add<Float>2/[anonymous][out]
  5: /array::Add<Float>2/a[in]
  6: /array::Add<Float>2/b[in]
  7: /array::Add<Float>2/[anonymous][out]
  8: /array::Add<Int>1/[anonymous][out]
  9: /array::Add<Int>1/a[in]
  10: /array::Add<Int>1/b[in]
  11: /array::Add<Int>1/[anonymous][out]
  12: /array::Append<Float>1/array[in]
  13: /array::Append<Float>1/value[in]
  14: /array::Append<Float>1/fst[out]
  15: /array::Append<Float>1/snd[out]
  16: /array::Append<Float>2/array[in]
  17: /array::Append<Float>2/value[in]
  18: /array::Append<Float>2/fst[out]
  19: /array::Append<Float>2/snd[out]
  20: /array::Append<Int>1/array[in]
  21: /array::Append<Int>1/value[in]
  22: /array::Append<Int>1/fst[out]
  23: /array::Append<Int>1/snd[out]

Inputs:
  x.fst[in] -> #[/array::Add<Float>1/a[in], /array::Add<Float>1/b[in]]
  x.snd.fst[in] -> #[/array::Add<Float>2/a[in], /array::Add<Float>2/b[in]]
  x.snd.snd[in] -> #[/array::Add<Int>1/a[in], /array::Add<Int>1/b[in]]
  a[in] -> #[/array::Append<Float>1/value[in], /array::Append<Float>2/value[in]]
  i[in] -> #[/array::Append<Int>1/value[in]]

Outputs:
  /array::Append<Float>1/fst[out] -> fst[out]
  /array::Append<Float>2/fst[out] -> snd.fst[out]
  /array::Append<Int>1/fst[out] -> snd.snd[out]

Wires:
  0: /array::Add<Float>1/[anonymous][out] -> /array::Append<Float>1/array[in]
  1: /array::Add<Float>2/[anonymous][out] -> /array::Append<Float>2/array[in]
  2: /array::Add<Int>1/[anonymous][out] -> /array::Append<Int>1/array[in]

Literals:
-/
#guard_msgs in
#apex_graph fun (x : Array (Float × Float × Int)) (a : Float) (i : Int) => (x + x).push (a,a,i)


