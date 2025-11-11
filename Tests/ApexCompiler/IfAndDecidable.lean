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
#apex_graph fun x y : Int => if true then x else y


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
#apex_graph fun x y : Int => if false then x else y


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


/-- info: Nat : Type -/
#guard_msgs in
#check Nat


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

Ports:
  0: /GreaterThanInt/a[in]
  1: /GreaterThanInt/b[in]
  2: /GreaterThanInt/[anonymous][out]
  3: /IfBegin/[anonymous][out]
  4: /IfBegin/condition[in]
  5: /IfBegin/spare[⋯][in]
  6: /IfBegin/fst[out]
  7: /IfBegin/snd[⋯][out]
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
  22: /IfBegin1/fst[out]
  23: /IfBegin1/snd[⋯][out]
  24: /MultiplyInt/a[in]
  25: /MultiplyInt/b[⋯][in]
  26: /MultiplyInt/[anonymous][out]
  27: /IfEnd1/[anonymous][out]
  28: /IfEnd1/scope[in]
  29: /IfEnd1/spare[⋯][in]
  30: /IfEnd1/[anonymous][⋯][out]

Inputs:
  x[in] -> #[/GreaterThanInt/b[in], /IfBegin/spare[0][in]]
  y[in] -> #[/GreaterThanInt/a[in], /IfBegin/spare[1][in]]

Outputs:
  /IfEnd1/[anonymous][0][out] -> «0»[out]

Wires:
  0: /GreaterThanInt/[anonymous][out] -> /IfBegin/condition[in]
  1: /IfBegin/snd[0][out] -> /AddInt/a[in]
  2: /IfBegin/snd[1][out] -> /AddInt/b[0][in]
  3: /IfBegin/fst[out] -> /IfEnd/scope[in]
  4: /AddInt/[anonymous][out] -> /IfEnd/spare[0][in]
  5: /IfBegin/snd[1][out] -> /IfEnd/spare[1][in]
  6: /GreaterThanInt/[anonymous][out] -> /TwoWaySwitchBool/index[in]
  7: /TwoWaySwitchBool/[anonymous][out] -> /IfBegin1/condition[in]
  8: /IfEnd/[anonymous][0][out] -> /IfBegin1/spare[0][in]
  9: /IfEnd/[anonymous][1][out] -> /IfBegin1/spare[1][in]
  10: /IfBegin1/snd[0][out] -> /MultiplyInt/a[in]
  11: /IfBegin1/snd[1][out] -> /MultiplyInt/b[0][in]
  12: /IfBegin1/fst[out] -> /IfEnd1/scope[in]
  13: /MultiplyInt/[anonymous][out] -> /IfEnd1/spare[0][in]
  14: /IfBegin1/snd[1][out] -> /IfEnd1/spare[1][in]

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
  2: AddFloat : Add<Float>
  3: AddFloat1 : Add<Float>
  4: IfEnd : IfEnd
  5: TwoWaySwitchBool : TwoWaySwitch<Bool>
  6: IfBegin1 : IfBegin
  7: MultiplyFloat : Multiply<Float>
  8: MultiplyFloat1 : Multiply<Float>
  9: IfEnd1 : IfEnd

Ports:
  0: /GreaterThanFloat/a[in]
  1: /GreaterThanFloat/b[in]
  2: /GreaterThanFloat/[anonymous][out]
  3: /IfBegin/[anonymous][out]
  4: /IfBegin/condition[in]
  5: /IfBegin/spare[⋯][in]
  6: /IfBegin/fst[out]
  7: /IfBegin/snd[⋯][out]
  8: /AddFloat/a[in]
  9: /AddFloat/b[⋯][in]
  10: /AddFloat/[anonymous][out]
  11: /AddFloat1/a[in]
  12: /AddFloat1/b[⋯][in]
  13: /AddFloat1/[anonymous][out]
  14: /IfEnd/[anonymous][out]
  15: /IfEnd/scope[in]
  16: /IfEnd/spare[⋯][in]
  17: /IfEnd/[anonymous][⋯][out]
  18: /TwoWaySwitchBool/a[in]
  19: /TwoWaySwitchBool/b[in]
  20: /TwoWaySwitchBool/index[in]
  21: /TwoWaySwitchBool/[anonymous][out]
  22: /IfBegin1/[anonymous][out]
  23: /IfBegin1/condition[in]
  24: /IfBegin1/spare[⋯][in]
  25: /IfBegin1/fst[out]
  26: /IfBegin1/snd[⋯][out]
  27: /MultiplyFloat/a[in]
  28: /MultiplyFloat/b[⋯][in]
  29: /MultiplyFloat/[anonymous][out]
  30: /MultiplyFloat1/a[in]
  31: /MultiplyFloat1/b[⋯][in]
  32: /MultiplyFloat1/[anonymous][out]
  33: /IfEnd1/[anonymous][out]
  34: /IfEnd1/scope[in]
  35: /IfEnd1/spare[⋯][in]
  36: /IfEnd1/[anonymous][⋯][out]

Inputs:
  x.fst[in] -> #[/GreaterThanFloat/b[in], /IfBegin/spare[0][in]]
  x.snd[in] -> #[/IfBegin/spare[1][in]]
  y.fst[in] -> #[/IfBegin/spare[2][in]]
  y.snd[in] -> #[/GreaterThanFloat/a[in], /IfBegin/spare[3][in]]

Outputs:
  /IfEnd1/[anonymous][0][out] -> fst0[out]
  /IfEnd1/[anonymous][1][out] -> snd1[out]

Wires:
  0: /GreaterThanFloat/[anonymous][out] -> /IfBegin/condition[in]
  1: /IfBegin/snd[0][out] -> /AddFloat/a[in]
  2: /IfBegin/snd[2][out] -> /AddFloat/b[0][in]
  3: /IfBegin/snd[1][out] -> /AddFloat1/a[in]
  4: /IfBegin/snd[3][out] -> /AddFloat1/b[0][in]
  5: /IfBegin/fst[out] -> /IfEnd/scope[in]
  6: /AddFloat/[anonymous][out] -> /IfEnd/spare[0][in]
  7: /AddFloat1/[anonymous][out] -> /IfEnd/spare[1][in]
  8: /IfBegin/snd[2][out] -> /IfEnd/spare[2][in]
  9: /IfBegin/snd[3][out] -> /IfEnd/spare[3][in]
  10: /GreaterThanFloat/[anonymous][out] -> /TwoWaySwitchBool/index[in]
  11: /TwoWaySwitchBool/[anonymous][out] -> /IfBegin1/condition[in]
  12: /IfEnd/[anonymous][0][out] -> /IfBegin1/spare[0][in]
  13: /IfEnd/[anonymous][1][out] -> /IfBegin1/spare[1][in]
  14: /IfEnd/[anonymous][2][out] -> /IfBegin1/spare[2][in]
  15: /IfEnd/[anonymous][3][out] -> /IfBegin1/spare[3][in]
  16: /IfBegin1/snd[2][out] -> /MultiplyFloat/a[in]
  17: /IfBegin1/snd[0][out] -> /MultiplyFloat/b[0][in]
  18: /IfBegin1/snd[3][out] -> /MultiplyFloat1/a[in]
  19: /IfBegin1/snd[1][out] -> /MultiplyFloat1/b[0][in]
  20: /IfBegin1/fst[out] -> /IfEnd1/scope[in]
  21: /MultiplyFloat/[anonymous][out] -> /IfEnd1/spare[0][in]
  22: /MultiplyFloat1/[anonymous][out] -> /IfEnd1/spare[1][in]
  23: /IfBegin1/snd[2][out] -> /IfEnd1/spare[2][in]
  24: /IfBegin1/snd[3][out] -> /IfEnd1/spare[3][in]

Literals:
  0: bool "true" -> /TwoWaySwitchBool/a[in] ⏎
  1: bool "false" -> /TwoWaySwitchBool/b[in]
-/
#guard_msgs in
#apex_graph fun x y : Float×Float => if x.1 < y.2 then (x+y) else (y*x)



/--
info: Nodes:
  0: EqualsInt : Equals<Int>
  1: IfBegin : IfBegin
  2: sop_fractal : sop::fractal
  3: IfEnd : IfEnd
  4: TwoWaySwitchBool : TwoWaySwitch<Bool>
  5: IfBegin1 : IfBegin
  6: sop_subdivide : sop::subdivide
  7: IfEnd1 : IfEnd

Ports:
  0: /EqualsInt/a[in]
  1: /EqualsInt/b[in]
  2: /EqualsInt/[anonymous][out]
  3: /IfBegin/[anonymous][out]
  4: /IfBegin/condition[in]
  5: /IfBegin/spare[⋯][in]
  6: /IfBegin/fst[out]
  7: /IfBegin/snd[⋯][out]
  8: /sop_fractal/[anonymous][out]
  9: /sop_fractal/geo0[in]
  10: /sop_fractal/group[in]
  11: /sop_fractal/divs[in]
  12: /sop_fractal/smooth[in]
  13: /sop_fractal/scale[in]
  14: /sop_fractal/seed[in]
  15: /sop_fractal/fixed[in]
  16: /sop_fractal/vtxnms[in]
  17: /sop_fractal/nmlattrib[in]
  18: /sop_fractal/dir[in]
  19: /sop_fractal/[anonymous][out]
  20: /IfEnd/[anonymous][out]
  21: /IfEnd/scope[in]
  22: /IfEnd/spare[⋯][in]
  23: /IfEnd/[anonymous][⋯][out]
  24: /TwoWaySwitchBool/a[in]
  25: /TwoWaySwitchBool/b[in]
  26: /TwoWaySwitchBool/index[in]
  27: /TwoWaySwitchBool/[anonymous][out]
  28: /IfBegin1/[anonymous][out]
  29: /IfBegin1/condition[in]
  30: /IfBegin1/spare[⋯][in]
  31: /IfBegin1/fst[out]
  32: /IfBegin1/snd[⋯][out]
  33: /sop_subdivide/[anonymous][out]
  34: /sop_subdivide/geo0[in]
  35: /sop_subdivide/geo1[in]
  36: /sop_subdivide/subdivide[in]
  37: /sop_subdivide/creases[in]
  38: /sop_subdivide/iterations[in]
  39: /sop_subdivide/overridecrease[in]
  40: /sop_subdivide/creaseweight[in]
  41: /sop_subdivide/outputcrease[in]
  42: /sop_subdivide/outcreasegroup[in]
  43: /sop_subdivide/closeholes[in]
  44: /sop_subdivide/surroundpoly[in]
  45: /sop_subdivide/bias[in]
  46: /sop_subdivide/smoothvertex[in]
  47: /sop_subdivide/consisttopology[in]
  48: /sop_subdivide/linearcreases[in]
  49: /sop_subdivide/algorithm[in]
  50: /sop_subdivide/buildpolysoups[in]
  51: /sop_subdivide/indepcurves[in]
  52: /sop_subdivide/updatenmls[in]
  53: /sop_subdivide/removeholes[in]
  54: /sop_subdivide/vtxboundary[in]
  55: /sop_subdivide/fvarlinear[in]
  56: /sop_subdivide/creasemethod[in]
  57: /sop_subdivide/trianglesubd[in]
  58: /sop_subdivide/[anonymous][out]
  59: /IfEnd1/[anonymous][out]
  60: /IfEnd1/scope[in]
  61: /IfEnd1/spare[⋯][in]
  62: /IfEnd1/[anonymous][⋯][out]

Inputs:
  geo[in] -> #[/IfBegin/spare[0][in]]
  x[in] -> #[/EqualsInt/a[in]]

Outputs:
  /IfEnd1/[anonymous][0][out] -> «0»[out]

Wires:
  0: /EqualsInt/[anonymous][out] -> /IfBegin/condition[in]
  1: /IfBegin/snd[0][out] -> /sop_fractal/geo0[in]
  2: /IfBegin/fst[out] -> /IfEnd/scope[in]
  3: /sop_fractal/[anonymous][out] -> /IfEnd/spare[0][in]
  4: /EqualsInt/[anonymous][out] -> /TwoWaySwitchBool/index[in]
  5: /TwoWaySwitchBool/[anonymous][out] -> /IfBegin1/condition[in]
  6: /IfEnd/[anonymous][0][out] -> /IfBegin1/spare[0][in]
  7: /IfBegin1/snd[0][out] -> /sop_subdivide/geo0[in]
  8: /IfBegin1/fst[out] -> /IfEnd1/scope[in]
  9: /sop_subdivide/[anonymous][out] -> /IfEnd1/spare[0][in]

Literals:
  0: int 0 -> /EqualsInt/b[in] ⏎
  1: str "" -> /sop_fractal/group[in] ⏎
  2: int 3 -> /sop_fractal/divs[in] ⏎
  3: float 0.500000 -> /sop_fractal/smooth[in] ⏎
  4: float 1.000000 -> /sop_fractal/scale[in] ⏎
  5: int 0 -> /sop_fractal/seed[in] ⏎
  6: int 0 -> /sop_fractal/fixed[in] ⏎
  7: int 0 -> /sop_fractal/vtxnms[in] ⏎
  8: str "N" -> /sop_fractal/nmlattrib[in] ⏎
  9: vector3 "{ x := 0.000000, y := 1.000000, z := 0.000000 }" -> /sop_fractal/dir[in] ⏎
  10: bool "true" -> /TwoWaySwitchBool/a[in] ⏎
  11: bool "false" -> /TwoWaySwitchBool/b[in] ⏎
  12: str "" -> /sop_subdivide/subdivide[in] ⏎
  13: str "" -> /sop_subdivide/creases[in] ⏎
  14: int 1 -> /sop_subdivide/iterations[in] ⏎
  15: int 0 -> /sop_subdivide/overridecrease[in] ⏎
  16: float 0.000000 -> /sop_subdivide/creaseweight[in] ⏎
  17: int 0 -> /sop_subdivide/outputcrease[in] ⏎
  18: str "creasegroup" -> /sop_subdivide/outcreasegroup[in] ⏎
  19: int 1 -> /sop_subdivide/closeholes[in] ⏎
  20: int 0 -> /sop_subdivide/surroundpoly[in] ⏎
  21: float 0.000000 -> /sop_subdivide/bias[in] ⏎
  22: int 0 -> /sop_subdivide/smoothvertex[in] ⏎
  23: int 0 -> /sop_subdivide/consisttopology[in] ⏎
  24: int 0 -> /sop_subdivide/linearcreases[in] ⏎
  25: int 0 -> /sop_subdivide/algorithm[in] ⏎
  26: int 0 -> /sop_subdivide/buildpolysoups[in] ⏎
  27: int 0 -> /sop_subdivide/indepcurves[in] ⏎
  28: int 1 -> /sop_subdivide/updatenmls[in] ⏎
  29: int 0 -> /sop_subdivide/removeholes[in] ⏎
  30: str "edge only" -> /sop_subdivide/vtxboundary[in] ⏎
  31: str "corners plus1" -> /sop_subdivide/fvarlinear[in] ⏎
  32: str "uniform" -> /sop_subdivide/creasemethod[in] ⏎
  33: str "catmull-clark" -> /sop_subdivide/trianglesubd[in]
-/
#guard_msgs in
#apex_graph fun (geo : Geometry) (x : Int) =>
  if x = 0 then
    geo.fractal
  else
    geo.subdivide
