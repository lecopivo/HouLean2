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
  0: TwoWaySwitch<Int>1 : TwoWaySwitch<Int>

Ports:
  0: /TwoWaySwitch<Int>1/a[in]
  1: /TwoWaySwitch<Int>1/b[in]
  2: /TwoWaySwitch<Int>1/index[in]
  3: /TwoWaySwitch<Int>1/[anonymous][out]

Inputs:
  x.fst[in] -> #[/TwoWaySwitch<Int>1/b[in]]
  x.snd[in] -> #[/TwoWaySwitch<Int>1/index[in]]

Outputs:
  /TwoWaySwitch<Int>1/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
  0: int 0 -> 0
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

open Qq
open Lean

