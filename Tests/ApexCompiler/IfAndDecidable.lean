import HouLean

open HouLean Apex Compiler

open Lean Qq

/--
info: Nodes:
  0: GreaterThanInt : GreaterThan<Int>

Ports:
  0: /GreaterThanInt/a[in]
  1: /GreaterThanInt/b[in]
  2: /GreaterThanInt/[anonymous][out]

Inputs:
  x[in] -> #[/GreaterThanInt/b[in]]
  y[in] -> #[/GreaterThanInt/a[in]]

Outputs:
  /GreaterThanInt/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun x y : Int => decide (x < y)


/--
info: Nodes:
  0: [anonymous] : Value<Int>
  1: y : Value<Int>

Ports:
  0: /[anonymous]/parm[in]
  1: /[anonymous]/[anonymous][out]
  2: /y/parm[in]
  3: /y/[anonymous][out]

Inputs:
  x[in] -> #[/[anonymous]/parm[in]]
  y[in] -> #[/y/parm[in]]

Outputs:
  /[anonymous]/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun x y : Int => if true then x else y -- todo: the if statement should get eliminated!


/--
info: Nodes:
  0: [anonymous] : Value<Int>
  1: x : Value<Int>

Ports:
  0: /[anonymous]/parm[in]
  1: /[anonymous]/[anonymous][out]
  2: /x/parm[in]
  3: /x/[anonymous][out]

Inputs:
  x[in] -> #[/x/parm[in]]
  y[in] -> #[/[anonymous]/parm[in]]

Outputs:
  /[anonymous]/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun x y : Int => if false then x else y -- todo: the if statement should get eliminated!


/--
info: Nodes:
  0: GreaterThanInt : GreaterThan<Int>

Ports:
  0: /GreaterThanInt/a[in]
  1: /GreaterThanInt/b[in]
  2: /GreaterThanInt/[anonymous][out]

Inputs:
  x[in] -> #[/GreaterThanInt/a[in]]
  y[in] -> #[/GreaterThanInt/b[in]]

Outputs:
  /GreaterThanInt/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun x y : Int => decide (x > y)

/--
info: Nodes:
  0: GreaterThanInt : GreaterThan<Int>

Ports:
  0: /GreaterThanInt/a[in]
  1: /GreaterThanInt/b[in]
  2: /GreaterThanInt/[anonymous][out]

Inputs:
  x[in] -> #[/GreaterThanInt/b[in]]
  y[in] -> #[/GreaterThanInt/a[in]]

Outputs:
  /GreaterThanInt/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun x y : Int => decide (x < y)

/--
info: Nodes:
  0: GreaterThanOrEqualInt : GreaterThanOrEqual<Int>

Ports:
  0: /GreaterThanOrEqualInt/a[in]
  1: /GreaterThanOrEqualInt/b[in]
  2: /GreaterThanOrEqualInt/[anonymous][out]

Inputs:
  x[in] -> #[/GreaterThanOrEqualInt/b[in]]
  y[in] -> #[/GreaterThanOrEqualInt/a[in]]

Outputs:
  /GreaterThanOrEqualInt/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun x y : Int => decide (x ≤ y)


/--
info: Nodes:
  0: GreaterThanOrEqualInt : GreaterThanOrEqual<Int>

Ports:
  0: /GreaterThanOrEqualInt/a[in]
  1: /GreaterThanOrEqualInt/b[in]
  2: /GreaterThanOrEqualInt/[anonymous][out]

Inputs:
  x[in] -> #[/GreaterThanOrEqualInt/a[in]]
  y[in] -> #[/GreaterThanOrEqualInt/b[in]]

Outputs:
  /GreaterThanOrEqualInt/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun x y : Int => decide (x ≥ y)


/--
info: Nodes:
  0: EqualsInt : Equals<Int>

Ports:
  0: /EqualsInt/a[in]
  1: /EqualsInt/b[in]
  2: /EqualsInt/[anonymous][out]

Inputs:
  x[in] -> #[/EqualsInt/a[in]]
  y[in] -> #[/EqualsInt/b[in]]

Outputs:
  /EqualsInt/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun x y : Int => decide (x = y)


/--
info: Nodes:
  0: GreaterThanInt : GreaterThan<Int>
  1: IfBegin : IfBegin
  2: AddInt : Add<Int>
  3: IfEnd : IfEnd
  4: TwoWaySwitchBool : TwoWaySwitch<Bool>
  5: IfBegin1 : IfBegin
  6: MultiplyInt : Multiply<Int>
  7: IfEnd1 : IfEnd
  8: TwoWaySwitchInt : TwoWaySwitch<Int>

Ports:
  0: /GreaterThanInt/a[in]
  1: /GreaterThanInt/b[in]
  2: /GreaterThanInt/[anonymous][out]
  3: /IfBegin/[anonymous][out]
  4: /IfBegin/condition[in]
  5: /IfBegin/spare[⋯][in]
  6: /IfBegin/scope[out]
  7: /IfBegin/spare[⋯][out]
  8: /AddInt/a[in]
  9: /AddInt/b[⋯][in]
  10: /AddInt/[anonymous][out]
  11: /IfEnd/[anonymous][out]
  12: /IfEnd/scope[in]
  13: /IfEnd/spare[⋯][in]
  14: /IfEnd/[anonymous][⋯][out]
  15: /TwoWaySwitchBool/a[in]
  16: /TwoWaySwitchBool/b[in]
  17: /TwoWaySwitchBool/index[in]
  18: /TwoWaySwitchBool/[anonymous][out]
  19: /IfBegin1/[anonymous][out]
  20: /IfBegin1/condition[in]
  21: /IfBegin1/spare[⋯][in]
  22: /IfBegin1/scope[out]
  23: /IfBegin1/spare[⋯][out]
  24: /MultiplyInt/a[in]
  25: /MultiplyInt/b[⋯][in]
  26: /MultiplyInt/[anonymous][out]
  27: /IfEnd1/[anonymous][out]
  28: /IfEnd1/scope[in]
  29: /IfEnd1/spare[⋯][in]
  30: /IfEnd1/[anonymous][⋯][out]
  31: /TwoWaySwitchInt/a[in]
  32: /TwoWaySwitchInt/b[in]
  33: /TwoWaySwitchInt/index[in]
  34: /TwoWaySwitchInt/[anonymous][out]

Inputs:
  x[in] -> #[/GreaterThanInt/b[in], /IfBegin/spare[0][in], /IfBegin1/spare[0][in]]
  y[in] -> #[/GreaterThanInt/a[in], /IfBegin/spare[1][in], /IfBegin1/spare[1][in]]

Outputs:
  /TwoWaySwitchInt/[anonymous][out] -> [anonymous][out]

Wires:
  0: /GreaterThanInt/[anonymous][out] -> /IfBegin/condition[in]
  1: /IfBegin/spare[0][out] -> /AddInt/a[in]
  2: /IfBegin/spare[1][out] -> /AddInt/b[0][in]
  3: /IfBegin/scope[out] -> /IfEnd/scope[in]
  4: /AddInt/[anonymous][out] -> /IfEnd/spare[0][in]
  5: /GreaterThanInt/[anonymous][out] -> /TwoWaySwitchBool/index[in]
  6: /TwoWaySwitchBool/[anonymous][out] -> /IfBegin1/condition[in]
  7: /IfBegin1/spare[0][out] -> /MultiplyInt/a[in]
  8: /IfBegin1/spare[1][out] -> /MultiplyInt/b[0][in]
  9: /IfBegin1/scope[out] -> /IfEnd1/scope[in]
  10: /MultiplyInt/[anonymous][out] -> /IfEnd1/spare[0][in]
  11: /IfEnd1/[anonymous][0][out] -> /TwoWaySwitchInt/a[in]
  12: /IfEnd/[anonymous][0][out] -> /TwoWaySwitchInt/b[in]
  13: /GreaterThanInt/[anonymous][out] -> /TwoWaySwitchInt/index[in]

Literals:
  0: bool "true" -> /TwoWaySwitchBool/a[in] ⏎
  1: bool "false" -> /TwoWaySwitchBool/b[in]
-/
#guard_msgs in
#apex_graph fun x y : Int => if x < y then x + y else x * y


/--
info: Nodes:
  0: GreaterThanInt : GreaterThan<Int>
  1: IfBegin : IfBegin
  2: AddInt : Add<Int>
  3: IfEnd : IfEnd
  4: TwoWaySwitchBool : TwoWaySwitch<Bool>
  5: IfBegin1 : IfBegin
  6: MultiplyInt : Multiply<Int>
  7: IfEnd1 : IfEnd
  8: TwoWaySwitchInt : TwoWaySwitch<Int>

Ports:
  0: /GreaterThanInt/a[in]
  1: /GreaterThanInt/b[in]
  2: /GreaterThanInt/[anonymous][out]
  3: /IfBegin/[anonymous][out]
  4: /IfBegin/condition[in]
  5: /IfBegin/spare[⋯][in]
  6: /IfBegin/scope[out]
  7: /IfBegin/spare[⋯][out]
  8: /AddInt/a[in]
  9: /AddInt/b[⋯][in]
  10: /AddInt/[anonymous][out]
  11: /IfEnd/[anonymous][out]
  12: /IfEnd/scope[in]
  13: /IfEnd/spare[⋯][in]
  14: /IfEnd/[anonymous][⋯][out]
  15: /TwoWaySwitchBool/a[in]
  16: /TwoWaySwitchBool/b[in]
  17: /TwoWaySwitchBool/index[in]
  18: /TwoWaySwitchBool/[anonymous][out]
  19: /IfBegin1/[anonymous][out]
  20: /IfBegin1/condition[in]
  21: /IfBegin1/spare[⋯][in]
  22: /IfBegin1/scope[out]
  23: /IfBegin1/spare[⋯][out]
  24: /MultiplyInt/a[in]
  25: /MultiplyInt/b[⋯][in]
  26: /MultiplyInt/[anonymous][out]
  27: /IfEnd1/[anonymous][out]
  28: /IfEnd1/scope[in]
  29: /IfEnd1/spare[⋯][in]
  30: /IfEnd1/[anonymous][⋯][out]
  31: /TwoWaySwitchInt/a[in]
  32: /TwoWaySwitchInt/b[in]
  33: /TwoWaySwitchInt/index[in]
  34: /TwoWaySwitchInt/[anonymous][out]

Inputs:
  x[in] -> #[/GreaterThanInt/b[in], /IfBegin/spare[0][in], /IfBegin1/spare[0][in]]
  y[in] -> #[/GreaterThanInt/a[in], /IfBegin/spare[1][in], /IfBegin1/spare[1][in]]

Outputs:
  /TwoWaySwitchInt/[anonymous][out] -> [anonymous][out]

Wires:
  0: /GreaterThanInt/[anonymous][out] -> /IfBegin/condition[in]
  1: /IfBegin/spare[0][out] -> /AddInt/a[in]
  2: /IfBegin/spare[1][out] -> /AddInt/b[0][in]
  3: /IfBegin/scope[out] -> /IfEnd/scope[in]
  4: /AddInt/[anonymous][out] -> /IfEnd/spare[0][in]
  5: /GreaterThanInt/[anonymous][out] -> /TwoWaySwitchBool/index[in]
  6: /TwoWaySwitchBool/[anonymous][out] -> /IfBegin1/condition[in]
  7: /IfBegin1/spare[0][out] -> /MultiplyInt/a[in]
  8: /IfBegin1/spare[1][out] -> /MultiplyInt/b[0][in]
  9: /IfBegin1/scope[out] -> /IfEnd1/scope[in]
  10: /MultiplyInt/[anonymous][out] -> /IfEnd1/spare[0][in]
  11: /IfEnd1/[anonymous][0][out] -> /TwoWaySwitchInt/a[in]
  12: /IfEnd/[anonymous][0][out] -> /TwoWaySwitchInt/b[in]
  13: /GreaterThanInt/[anonymous][out] -> /TwoWaySwitchInt/index[in]

Literals:
  0: bool "true" -> /TwoWaySwitchBool/a[in] ⏎
  1: bool "false" -> /TwoWaySwitchBool/b[in]
-/
#guard_msgs in
#apex_graph fun x y : Int => if _ : x < y then x + y else x * y


/--
info: Nodes:
  0: GreaterThanFloat : GreaterThan<Float>
  1: IfBegin : IfBegin
  2: IfEnd : IfEnd
  3: TwoWaySwitchBool : TwoWaySwitch<Bool>
  4: IfBegin1 : IfBegin
  5: IfEnd1 : IfEnd
  6: TwoWaySwitchFloat : TwoWaySwitch<Float>
  7: TwoWaySwitchFloat1 : TwoWaySwitch<Float>
  8: TwoWaySwitchFloat2 : TwoWaySwitch<Float>
  9: TwoWaySwitchFloat3 : TwoWaySwitch<Float>

Ports:
  0: /GreaterThanFloat/a[in]
  1: /GreaterThanFloat/b[in]
  2: /GreaterThanFloat/[anonymous][out]
  3: /IfBegin/[anonymous][out]
  4: /IfBegin/condition[in]
  5: /IfBegin/spare[⋯][in]
  6: /IfBegin/scope[out]
  7: /IfBegin/spare[⋯][out]
  8: /IfEnd/[anonymous][out]
  9: /IfEnd/scope[in]
  10: /IfEnd/spare[⋯][in]
  11: /IfEnd/[anonymous][⋯][out]
  12: /TwoWaySwitchBool/a[in]
  13: /TwoWaySwitchBool/b[in]
  14: /TwoWaySwitchBool/index[in]
  15: /TwoWaySwitchBool/[anonymous][out]
  16: /IfBegin1/[anonymous][out]
  17: /IfBegin1/condition[in]
  18: /IfBegin1/spare[⋯][in]
  19: /IfBegin1/scope[out]
  20: /IfBegin1/spare[⋯][out]
  21: /IfEnd1/[anonymous][out]
  22: /IfEnd1/scope[in]
  23: /IfEnd1/spare[⋯][in]
  24: /IfEnd1/[anonymous][⋯][out]
  25: /TwoWaySwitchFloat/a[in]
  26: /TwoWaySwitchFloat/b[in]
  27: /TwoWaySwitchFloat/index[in]
  28: /TwoWaySwitchFloat/[anonymous][out]
  29: /TwoWaySwitchFloat1/a[in]
  30: /TwoWaySwitchFloat1/b[in]
  31: /TwoWaySwitchFloat1/index[in]
  32: /TwoWaySwitchFloat1/[anonymous][out]
  33: /TwoWaySwitchFloat2/a[in]
  34: /TwoWaySwitchFloat2/b[in]
  35: /TwoWaySwitchFloat2/index[in]
  36: /TwoWaySwitchFloat2/[anonymous][out]
  37: /TwoWaySwitchFloat3/a[in]
  38: /TwoWaySwitchFloat3/b[in]
  39: /TwoWaySwitchFloat3/index[in]
  40: /TwoWaySwitchFloat3/[anonymous][out]

Inputs:
  x.fst[in] -> #[/GreaterThanFloat/b[in], /IfBegin/spare[0][in], /IfBegin1/spare[2][in]]
  x.snd[in] -> #[/IfBegin/spare[1][in], /IfBegin1/spare[3][in]]
  y.fst[in] -> #[/IfBegin/spare[2][in], /IfBegin1/spare[0][in]]
  y.snd[in] -> #[/GreaterThanFloat/a[in], /IfBegin/spare[3][in], /IfBegin1/spare[1][in]]

Outputs:
  /TwoWaySwitchFloat/[anonymous][out] -> fst.fst[out]
  /TwoWaySwitchFloat1/[anonymous][out] -> fst.snd[out]
  /TwoWaySwitchFloat2/[anonymous][out] -> snd.fst[out]
  /TwoWaySwitchFloat3/[anonymous][out] -> snd.snd[out]

Wires:
  0: /GreaterThanFloat/[anonymous][out] -> /IfBegin/condition[in]
  1: /IfBegin/scope[out] -> /IfEnd/scope[in]
  2: /IfBegin/spare[0][out] -> /IfEnd/spare[0][in]
  3: /IfBegin/spare[1][out] -> /IfEnd/spare[1][in]
  4: /IfBegin/spare[2][out] -> /IfEnd/spare[2][in]
  5: /IfBegin/spare[3][out] -> /IfEnd/spare[3][in]
  6: /GreaterThanFloat/[anonymous][out] -> /TwoWaySwitchBool/index[in]
  7: /TwoWaySwitchBool/[anonymous][out] -> /IfBegin1/condition[in]
  8: /IfBegin1/scope[out] -> /IfEnd1/scope[in]
  9: /IfBegin1/spare[0][out] -> /IfEnd1/spare[0][in]
  10: /IfBegin1/spare[1][out] -> /IfEnd1/spare[1][in]
  11: /IfBegin1/spare[2][out] -> /IfEnd1/spare[2][in]
  12: /IfBegin1/spare[3][out] -> /IfEnd1/spare[3][in]
  13: /IfEnd1/[anonymous][0][out] -> /TwoWaySwitchFloat/a[in]
  14: /IfEnd/[anonymous][0][out] -> /TwoWaySwitchFloat/b[in]
  15: /GreaterThanFloat/[anonymous][out] -> /TwoWaySwitchFloat/index[in]
  16: /IfEnd1/[anonymous][1][out] -> /TwoWaySwitchFloat1/a[in]
  17: /IfEnd/[anonymous][1][out] -> /TwoWaySwitchFloat1/b[in]
  18: /GreaterThanFloat/[anonymous][out] -> /TwoWaySwitchFloat1/index[in]
  19: /IfEnd1/[anonymous][2][out] -> /TwoWaySwitchFloat2/a[in]
  20: /IfEnd/[anonymous][2][out] -> /TwoWaySwitchFloat2/b[in]
  21: /GreaterThanFloat/[anonymous][out] -> /TwoWaySwitchFloat2/index[in]
  22: /IfEnd1/[anonymous][3][out] -> /TwoWaySwitchFloat3/a[in]
  23: /IfEnd/[anonymous][3][out] -> /TwoWaySwitchFloat3/b[in]
  24: /GreaterThanFloat/[anonymous][out] -> /TwoWaySwitchFloat3/index[in]

Literals:
  0: bool "true" -> /TwoWaySwitchBool/a[in] ⏎
  1: bool "false" -> /TwoWaySwitchBool/b[in]
-/
#guard_msgs in
#apex_graph fun x y : Float×Float => if x.1 < y.2 then (x,y) else (y,x)



set_option trace.HouLean.Apex.compiler true in
#apex_graph fun (geo : Geometry) (x : Int) =>
Id.run do

  if x = 0 then
    geo.fractal
  else
    geo.subdivide


open Qq Lean Meta Apex Compiler 


def betaThroughLet (e : Expr) : MetaM Expr := do
  let (fn, args) := e.withApp (fun fn args => (fn,args))
  let e' ← letTelescope fn fun xs b =>
    mkLetFVars xs b
  unless ← isDefEq e e' do
    throwError m!"Non defeq beta through let\n{e}\n==>\n{e'}"
  return e'


-- fun a b => a + b + 10
run_meta
  let e := q(have a := 1
             have b := 2
             a + b + 10)
  let e'' := q(let a := 1
             let b := 2
             a + b + 10)
  logInfo s!"are equal: {← isDefEq e e''}"
  let e' ← letTelescope e (preserveNondepLet := false) fun xs b =>
    mkLetFVars xs b
  logInfo e'
  -- let (fn, args) := e.withApp (fun fn args => (fn,args))
  -- logInfo m!"fn: {fn}"
  -- logInfo m!"args: {args}"
  -- let e' ← letTelescope fn fun xs b => do
  --   logInfo m!"xs: {xs}"
  --   logInfo m!"b: {b}"
  --   mkLetFVars xs (b.beta args)

  -- IO.println e

  -- let e' ← _root_.betaThroughLet e

  -- logInfo e'

