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
  2: value : Value<Float>
  3: value : Value<Float>
  4: AddFloat : Add<Float>
  5: AddFloat1 : Add<Float>
  6: IfEnd : IfEnd
  7: TwoWaySwitchBool : TwoWaySwitch<Bool>
  8: IfBegin1 : IfBegin
  9: MultiplyFloat : Multiply<Float>
  10: MultiplyFloat1 : Multiply<Float>
  11: IfEnd1 : IfEnd

Ports:
  0: /GreaterThanFloat/a[in]
  1: /GreaterThanFloat/b[in]
  2: /GreaterThanFloat/[anonymous][out]
  3: /IfBegin/[anonymous][out]
  4: /IfBegin/condition[in]
  5: /IfBegin/spare[⋯][in]
  6: /IfBegin/fst[out]
  7: /IfBegin/snd[⋯][out]
  8: /value/parm[in]
  9: /value/value[out]
  10: /value/parm[in]
  11: /value/value[out]
  12: /AddFloat/a[in]
  13: /AddFloat/b[⋯][in]
  14: /AddFloat/[anonymous][out]
  15: /AddFloat1/a[in]
  16: /AddFloat1/b[⋯][in]
  17: /AddFloat1/[anonymous][out]
  18: /IfEnd/[anonymous][out]
  19: /IfEnd/scope[in]
  20: /IfEnd/spare[⋯][in]
  21: /IfEnd/[anonymous][⋯][out]
  22: /TwoWaySwitchBool/a[in]
  23: /TwoWaySwitchBool/b[in]
  24: /TwoWaySwitchBool/index[in]
  25: /TwoWaySwitchBool/[anonymous][out]
  26: /IfBegin1/[anonymous][out]
  27: /IfBegin1/condition[in]
  28: /IfBegin1/spare[⋯][in]
  29: /IfBegin1/fst[out]
  30: /IfBegin1/snd[⋯][out]
  31: /MultiplyFloat/a[in]
  32: /MultiplyFloat/b[⋯][in]
  33: /MultiplyFloat/[anonymous][out]
  34: /MultiplyFloat1/a[in]
  35: /MultiplyFloat1/b[⋯][in]
  36: /MultiplyFloat1/[anonymous][out]
  37: /IfEnd1/[anonymous][out]
  38: /IfEnd1/scope[in]
  39: /IfEnd1/spare[⋯][in]
  40: /IfEnd1/[anonymous][⋯][out]

Inputs:
  x.fst[in] -> #[/GreaterThanFloat/b[in], /IfBegin/spare[2][in]]
  x.snd[in] -> #[/IfBegin/spare[3][in]]
  y.fst[in] -> #[/IfBegin/spare[4][in]]
  y.snd[in] -> #[/GreaterThanFloat/a[in], /IfBegin/spare[5][in]]

Outputs:
  /IfEnd1/[anonymous][0][out] -> fst0[out]
  /IfEnd1/[anonymous][1][out] -> snd1[out]

Wires:
  0: /GreaterThanFloat/[anonymous][out] -> /IfBegin/condition[in]
  1: /value/value[out] -> /IfBegin/spare[0][in]
  2: /value/value[out] -> /IfBegin/spare[1][in]
  3: /IfBegin/snd[2][out] -> /AddFloat/a[in]
  4: /IfBegin/snd[4][out] -> /AddFloat/b[0][in]
  5: /IfBegin/snd[3][out] -> /AddFloat1/a[in]
  6: /IfBegin/snd[5][out] -> /AddFloat1/b[0][in]
  7: /IfBegin/fst[out] -> /IfEnd/scope[in]
  8: /AddFloat/[anonymous][out] -> /IfEnd/spare[0][in]
  9: /AddFloat1/[anonymous][out] -> /IfEnd/spare[1][in]
  10: /IfBegin/snd[2][out] -> /IfEnd/spare[2][in]
  11: /IfBegin/snd[3][out] -> /IfEnd/spare[3][in]
  12: /IfBegin/snd[4][out] -> /IfEnd/spare[4][in]
  13: /IfBegin/snd[5][out] -> /IfEnd/spare[5][in]
  14: /GreaterThanFloat/[anonymous][out] -> /TwoWaySwitchBool/index[in]
  15: /TwoWaySwitchBool/[anonymous][out] -> /IfBegin1/condition[in]
  16: /IfEnd/[anonymous][0][out] -> /IfBegin1/spare[0][in]
  17: /IfEnd/[anonymous][1][out] -> /IfBegin1/spare[1][in]
  18: /IfEnd/[anonymous][2][out] -> /IfBegin1/spare[2][in]
  19: /IfEnd/[anonymous][3][out] -> /IfBegin1/spare[3][in]
  20: /IfEnd/[anonymous][4][out] -> /IfBegin1/spare[4][in]
  21: /IfEnd/[anonymous][5][out] -> /IfBegin1/spare[5][in]
  22: /IfBegin1/snd[4][out] -> /MultiplyFloat/a[in]
  23: /IfBegin1/snd[2][out] -> /MultiplyFloat/b[0][in]
  24: /IfBegin1/snd[5][out] -> /MultiplyFloat1/a[in]
  25: /IfBegin1/snd[3][out] -> /MultiplyFloat1/b[0][in]
  26: /IfBegin1/fst[out] -> /IfEnd1/scope[in]
  27: /MultiplyFloat/[anonymous][out] -> /IfEnd1/spare[0][in]
  28: /MultiplyFloat1/[anonymous][out] -> /IfEnd1/spare[1][in]
  29: /IfBegin1/snd[2][out] -> /IfEnd1/spare[2][in]
  30: /IfBegin1/snd[3][out] -> /IfEnd1/spare[3][in]
  31: /IfBegin1/snd[4][out] -> /IfEnd1/spare[4][in]
  32: /IfBegin1/snd[5][out] -> /IfEnd1/spare[5][in]

Literals:
  0: float 0.000000 -> /value/parm[in] ⏎
  1: float 0.000000 -> /value/parm[in] ⏎
  2: bool "true" -> /TwoWaySwitchBool/a[in] ⏎
  3: bool "false" -> /TwoWaySwitchBool/b[in]
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
  18: str "creases" -> /sop_subdivide/outcreasegroup[in] ⏎
  19: int 1 -> /sop_subdivide/closeholes[in] ⏎
  20: int 1 -> /sop_subdivide/surroundpoly[in] ⏎
  21: float 1.000000 -> /sop_subdivide/bias[in] ⏎
  22: int 1 -> /sop_subdivide/smoothvertex[in] ⏎
  23: int 0 -> /sop_subdivide/consisttopology[in] ⏎
  24: int 0 -> /sop_subdivide/linearcreases[in] ⏎
  25: int 0 -> /sop_subdivide/algorithm[in] ⏎
  26: int 0 -> /sop_subdivide/buildpolysoups[in] ⏎
  27: int 0 -> /sop_subdivide/indepcurves[in] ⏎
  28: int 1 -> /sop_subdivide/updatenmls[in] ⏎
  29: int 1 -> /sop_subdivide/removeholes[in] ⏎
  30: str "edge only" -> /sop_subdivide/vtxboundary[in] ⏎
  31: str "corners plus1" -> /sop_subdivide/fvarlinear[in] ⏎
  32: str "uniform" -> /sop_subdivide/creasemethod[in] ⏎
  33: str "catmull-clark" -> /sop_subdivide/trianglesubd[in]
-/
#guard_msgs in
#apex_graph fun (geo : Geometry) (x : Int) => Id.run do
  if x = 0 then
    geo.fractal
  else
    geo.subdivide




/--
info: Nodes:
  0: Modulo : Modulo
  1: EqualsInt : Equals<Int>
  2: IfBegin : IfBegin
  3: sop_fractal : sop::fractal
  4: Modulo1 : Modulo
  5: EqualsInt1 : Equals<Int>
  6: IfBegin1 : IfBegin
  7: sop_fractal1 : sop::fractal
  8: IfEnd : IfEnd
  9: TwoWaySwitchBool : TwoWaySwitch<Bool>
  10: IfBegin2 : IfBegin
  11: sop_subdivide : sop::subdivide
  12: IfEnd1 : IfEnd
  13: IfEnd2 : IfEnd
  14: TwoWaySwitchBool1 : TwoWaySwitch<Bool>
  15: IfBegin3 : IfBegin
  16: sop_subdivide1 : sop::subdivide
  17: Modulo2 : Modulo
  18: EqualsInt2 : Equals<Int>
  19: IfBegin4 : IfBegin
  20: sop_fractal2 : sop::fractal
  21: IfEnd3 : IfEnd
  22: TwoWaySwitchBool2 : TwoWaySwitch<Bool>
  23: IfBegin5 : IfBegin
  24: sop_subdivide2 : sop::subdivide
  25: IfEnd4 : IfEnd
  26: IfEnd5 : IfEnd

Ports:
  0: /Modulo/a[in]
  1: /Modulo/b[in]
  2: /Modulo/[anonymous][out]
  3: /EqualsInt/a[in]
  4: /EqualsInt/b[in]
  5: /EqualsInt/[anonymous][out]
  6: /IfBegin/[anonymous][out]
  7: /IfBegin/condition[in]
  8: /IfBegin/spare[⋯][in]
  9: /IfBegin/fst[out]
  10: /IfBegin/snd[⋯][out]
  11: /sop_fractal/[anonymous][out]
  12: /sop_fractal/geo0[in]
  13: /sop_fractal/group[in]
  14: /sop_fractal/divs[in]
  15: /sop_fractal/smooth[in]
  16: /sop_fractal/scale[in]
  17: /sop_fractal/seed[in]
  18: /sop_fractal/fixed[in]
  19: /sop_fractal/vtxnms[in]
  20: /sop_fractal/nmlattrib[in]
  21: /sop_fractal/dir[in]
  22: /sop_fractal/[anonymous][out]
  23: /Modulo1/a[in]
  24: /Modulo1/b[in]
  25: /Modulo1/[anonymous][out]
  26: /EqualsInt1/a[in]
  27: /EqualsInt1/b[in]
  28: /EqualsInt1/[anonymous][out]
  29: /IfBegin1/[anonymous][out]
  30: /IfBegin1/condition[in]
  31: /IfBegin1/spare[⋯][in]
  32: /IfBegin1/fst[out]
  33: /IfBegin1/snd[⋯][out]
  34: /sop_fractal1/[anonymous][out]
  35: /sop_fractal1/geo0[in]
  36: /sop_fractal1/group[in]
  37: /sop_fractal1/divs[in]
  38: /sop_fractal1/smooth[in]
  39: /sop_fractal1/scale[in]
  40: /sop_fractal1/seed[in]
  41: /sop_fractal1/fixed[in]
  42: /sop_fractal1/vtxnms[in]
  43: /sop_fractal1/nmlattrib[in]
  44: /sop_fractal1/dir[in]
  45: /sop_fractal1/[anonymous][out]
  46: /IfEnd/[anonymous][out]
  47: /IfEnd/scope[in]
  48: /IfEnd/spare[⋯][in]
  49: /IfEnd/[anonymous][⋯][out]
  50: /TwoWaySwitchBool/a[in]
  51: /TwoWaySwitchBool/b[in]
  52: /TwoWaySwitchBool/index[in]
  53: /TwoWaySwitchBool/[anonymous][out]
  54: /IfBegin2/[anonymous][out]
  55: /IfBegin2/condition[in]
  56: /IfBegin2/spare[⋯][in]
  57: /IfBegin2/fst[out]
  58: /IfBegin2/snd[⋯][out]
  59: /sop_subdivide/[anonymous][out]
  60: /sop_subdivide/geo0[in]
  61: /sop_subdivide/geo1[in]
  62: /sop_subdivide/subdivide[in]
  63: /sop_subdivide/creases[in]
  64: /sop_subdivide/iterations[in]
  65: /sop_subdivide/overridecrease[in]
  66: /sop_subdivide/creaseweight[in]
  67: /sop_subdivide/outputcrease[in]
  68: /sop_subdivide/outcreasegroup[in]
  69: /sop_subdivide/closeholes[in]
  70: /sop_subdivide/surroundpoly[in]
  71: /sop_subdivide/bias[in]
  72: /sop_subdivide/smoothvertex[in]
  73: /sop_subdivide/consisttopology[in]
  74: /sop_subdivide/linearcreases[in]
  75: /sop_subdivide/algorithm[in]
  76: /sop_subdivide/buildpolysoups[in]
  77: /sop_subdivide/indepcurves[in]
  78: /sop_subdivide/updatenmls[in]
  79: /sop_subdivide/removeholes[in]
  80: /sop_subdivide/vtxboundary[in]
  81: /sop_subdivide/fvarlinear[in]
  82: /sop_subdivide/creasemethod[in]
  83: /sop_subdivide/trianglesubd[in]
  84: /sop_subdivide/[anonymous][out]
  85: /IfEnd1/[anonymous][out]
  86: /IfEnd1/scope[in]
  87: /IfEnd1/spare[⋯][in]
  88: /IfEnd1/[anonymous][⋯][out]
  89: /IfEnd2/[anonymous][out]
  90: /IfEnd2/scope[in]
  91: /IfEnd2/spare[⋯][in]
  92: /IfEnd2/[anonymous][⋯][out]
  93: /TwoWaySwitchBool1/a[in]
  94: /TwoWaySwitchBool1/b[in]
  95: /TwoWaySwitchBool1/index[in]
  96: /TwoWaySwitchBool1/[anonymous][out]
  97: /IfBegin3/[anonymous][out]
  98: /IfBegin3/condition[in]
  99: /IfBegin3/spare[⋯][in]
  100: /IfBegin3/fst[out]
  101: /IfBegin3/snd[⋯][out]
  102: /sop_subdivide1/[anonymous][out]
  103: /sop_subdivide1/geo0[in]
  104: /sop_subdivide1/geo1[in]
  105: /sop_subdivide1/subdivide[in]
  106: /sop_subdivide1/creases[in]
  107: /sop_subdivide1/iterations[in]
  108: /sop_subdivide1/overridecrease[in]
  109: /sop_subdivide1/creaseweight[in]
  110: /sop_subdivide1/outputcrease[in]
  111: /sop_subdivide1/outcreasegroup[in]
  112: /sop_subdivide1/closeholes[in]
  113: /sop_subdivide1/surroundpoly[in]
  114: /sop_subdivide1/bias[in]
  115: /sop_subdivide1/smoothvertex[in]
  116: /sop_subdivide1/consisttopology[in]
  117: /sop_subdivide1/linearcreases[in]
  118: /sop_subdivide1/algorithm[in]
  119: /sop_subdivide1/buildpolysoups[in]
  120: /sop_subdivide1/indepcurves[in]
  121: /sop_subdivide1/updatenmls[in]
  122: /sop_subdivide1/removeholes[in]
  123: /sop_subdivide1/vtxboundary[in]
  124: /sop_subdivide1/fvarlinear[in]
  125: /sop_subdivide1/creasemethod[in]
  126: /sop_subdivide1/trianglesubd[in]
  127: /sop_subdivide1/[anonymous][out]
  128: /Modulo2/a[in]
  129: /Modulo2/b[in]
  130: /Modulo2/[anonymous][out]
  131: /EqualsInt2/a[in]
  132: /EqualsInt2/b[in]
  133: /EqualsInt2/[anonymous][out]
  134: /IfBegin4/[anonymous][out]
  135: /IfBegin4/condition[in]
  136: /IfBegin4/spare[⋯][in]
  137: /IfBegin4/fst[out]
  138: /IfBegin4/snd[⋯][out]
  139: /sop_fractal2/[anonymous][out]
  140: /sop_fractal2/geo0[in]
  141: /sop_fractal2/group[in]
  142: /sop_fractal2/divs[in]
  143: /sop_fractal2/smooth[in]
  144: /sop_fractal2/scale[in]
  145: /sop_fractal2/seed[in]
  146: /sop_fractal2/fixed[in]
  147: /sop_fractal2/vtxnms[in]
  148: /sop_fractal2/nmlattrib[in]
  149: /sop_fractal2/dir[in]
  150: /sop_fractal2/[anonymous][out]
  151: /IfEnd3/[anonymous][out]
  152: /IfEnd3/scope[in]
  153: /IfEnd3/spare[⋯][in]
  154: /IfEnd3/[anonymous][⋯][out]
  155: /TwoWaySwitchBool2/a[in]
  156: /TwoWaySwitchBool2/b[in]
  157: /TwoWaySwitchBool2/index[in]
  158: /TwoWaySwitchBool2/[anonymous][out]
  159: /IfBegin5/[anonymous][out]
  160: /IfBegin5/condition[in]
  161: /IfBegin5/spare[⋯][in]
  162: /IfBegin5/fst[out]
  163: /IfBegin5/snd[⋯][out]
  164: /sop_subdivide2/[anonymous][out]
  165: /sop_subdivide2/geo0[in]
  166: /sop_subdivide2/geo1[in]
  167: /sop_subdivide2/subdivide[in]
  168: /sop_subdivide2/creases[in]
  169: /sop_subdivide2/iterations[in]
  170: /sop_subdivide2/overridecrease[in]
  171: /sop_subdivide2/creaseweight[in]
  172: /sop_subdivide2/outputcrease[in]
  173: /sop_subdivide2/outcreasegroup[in]
  174: /sop_subdivide2/closeholes[in]
  175: /sop_subdivide2/surroundpoly[in]
  176: /sop_subdivide2/bias[in]
  177: /sop_subdivide2/smoothvertex[in]
  178: /sop_subdivide2/consisttopology[in]
  179: /sop_subdivide2/linearcreases[in]
  180: /sop_subdivide2/algorithm[in]
  181: /sop_subdivide2/buildpolysoups[in]
  182: /sop_subdivide2/indepcurves[in]
  183: /sop_subdivide2/updatenmls[in]
  184: /sop_subdivide2/removeholes[in]
  185: /sop_subdivide2/vtxboundary[in]
  186: /sop_subdivide2/fvarlinear[in]
  187: /sop_subdivide2/creasemethod[in]
  188: /sop_subdivide2/trianglesubd[in]
  189: /sop_subdivide2/[anonymous][out]
  190: /IfEnd4/[anonymous][out]
  191: /IfEnd4/scope[in]
  192: /IfEnd4/spare[⋯][in]
  193: /IfEnd4/[anonymous][⋯][out]
  194: /IfEnd5/[anonymous][out]
  195: /IfEnd5/scope[in]
  196: /IfEnd5/spare[⋯][in]
  197: /IfEnd5/[anonymous][⋯][out]

Inputs:
  geo[in] -> #[/IfBegin/spare[0][in]]
  x[in] -> #[/Modulo/a[in], /IfBegin/spare[1][in]]

Outputs:
  /IfEnd5/[anonymous][0][out] -> «0»[out]

Wires:
  0: /Modulo/[anonymous][out] -> /EqualsInt/a[in]
  1: /EqualsInt/[anonymous][out] -> /IfBegin/condition[in]
  2: /IfBegin/snd[0][out] -> /sop_fractal/geo0[in]
  3: /IfBegin/snd[1][out] -> /Modulo1/a[in]
  4: /Modulo1/[anonymous][out] -> /EqualsInt1/a[in]
  5: /EqualsInt1/[anonymous][out] -> /IfBegin1/condition[in]
  6: /sop_fractal/[anonymous][out] -> /IfBegin1/spare[0][in]
  7: /IfBegin1/snd[0][out] -> /sop_fractal1/geo0[in]
  8: /IfBegin1/fst[out] -> /IfEnd/scope[in]
  9: /sop_fractal1/[anonymous][out] -> /IfEnd/spare[0][in]
  10: /EqualsInt1/[anonymous][out] -> /TwoWaySwitchBool/index[in]
  11: /TwoWaySwitchBool/[anonymous][out] -> /IfBegin2/condition[in]
  12: /IfEnd/[anonymous][0][out] -> /IfBegin2/spare[0][in]
  13: /IfBegin2/snd[0][out] -> /sop_subdivide/geo0[in]
  14: /IfBegin2/fst[out] -> /IfEnd1/scope[in]
  15: /sop_subdivide/[anonymous][out] -> /IfEnd1/spare[0][in]
  16: /IfBegin/fst[out] -> /IfEnd2/scope[in]
  17: /IfEnd1/[anonymous][0][out] -> /IfEnd2/spare[0][in]
  18: /IfBegin/snd[1][out] -> /IfEnd2/spare[1][in]
  19: /EqualsInt/[anonymous][out] -> /TwoWaySwitchBool1/index[in]
  20: /TwoWaySwitchBool1/[anonymous][out] -> /IfBegin3/condition[in]
  21: /IfEnd2/[anonymous][0][out] -> /IfBegin3/spare[0][in]
  22: /IfEnd2/[anonymous][1][out] -> /IfBegin3/spare[1][in]
  23: /IfBegin3/snd[0][out] -> /sop_subdivide1/geo0[in]
  24: /IfBegin3/snd[1][out] -> /Modulo2/a[in]
  25: /Modulo2/[anonymous][out] -> /EqualsInt2/a[in]
  26: /EqualsInt2/[anonymous][out] -> /IfBegin4/condition[in]
  27: /sop_subdivide1/[anonymous][out] -> /IfBegin4/spare[0][in]
  28: /IfBegin4/snd[0][out] -> /sop_fractal2/geo0[in]
  29: /IfBegin4/fst[out] -> /IfEnd3/scope[in]
  30: /sop_fractal2/[anonymous][out] -> /IfEnd3/spare[0][in]
  31: /EqualsInt2/[anonymous][out] -> /TwoWaySwitchBool2/index[in]
  32: /TwoWaySwitchBool2/[anonymous][out] -> /IfBegin5/condition[in]
  33: /IfEnd3/[anonymous][0][out] -> /IfBegin5/spare[0][in]
  34: /IfBegin5/snd[0][out] -> /sop_subdivide2/geo0[in]
  35: /IfBegin5/fst[out] -> /IfEnd4/scope[in]
  36: /sop_subdivide2/[anonymous][out] -> /IfEnd4/spare[0][in]
  37: /IfBegin3/fst[out] -> /IfEnd5/scope[in]
  38: /IfEnd4/[anonymous][0][out] -> /IfEnd5/spare[0][in]
  39: /IfBegin3/snd[1][out] -> /IfEnd5/spare[1][in]

Literals:
  0: int 2 -> /Modulo/b[in] ⏎
  1: int 0 -> /EqualsInt/b[in] ⏎
  2: str "" -> /sop_fractal/group[in] ⏎
  3: int 3 -> /sop_fractal/divs[in] ⏎
  4: float 0.500000 -> /sop_fractal/smooth[in] ⏎
  5: float 1.000000 -> /sop_fractal/scale[in] ⏎
  6: int 0 -> /sop_fractal/seed[in] ⏎
  7: int 0 -> /sop_fractal/fixed[in] ⏎
  8: int 0 -> /sop_fractal/vtxnms[in] ⏎
  9: str "N" -> /sop_fractal/nmlattrib[in] ⏎
  10: vector3 "{ x := 0.000000, y := 1.000000, z := 0.000000 }" -> /sop_fractal/dir[in] ⏎
  11: int 4 -> /Modulo1/b[in] ⏎
  12: int 1 -> /EqualsInt1/b[in] ⏎
  13: str "" -> /sop_fractal1/group[in] ⏎
  14: int 3 -> /sop_fractal1/divs[in] ⏎
  15: float 0.500000 -> /sop_fractal1/smooth[in] ⏎
  16: float 1.000000 -> /sop_fractal1/scale[in] ⏎
  17: int 0 -> /sop_fractal1/seed[in] ⏎
  18: int 0 -> /sop_fractal1/fixed[in] ⏎
  19: int 0 -> /sop_fractal1/vtxnms[in] ⏎
  20: str "N" -> /sop_fractal1/nmlattrib[in] ⏎
  21: vector3 "{ x := 0.000000, y := 1.000000, z := 0.000000 }" -> /sop_fractal1/dir[in] ⏎
  22: bool "true" -> /TwoWaySwitchBool/a[in] ⏎
  23: bool "false" -> /TwoWaySwitchBool/b[in] ⏎
  24: str "" -> /sop_subdivide/subdivide[in] ⏎
  25: str "" -> /sop_subdivide/creases[in] ⏎
  26: int 1 -> /sop_subdivide/iterations[in] ⏎
  27: int 0 -> /sop_subdivide/overridecrease[in] ⏎
  28: float 0.000000 -> /sop_subdivide/creaseweight[in] ⏎
  29: int 0 -> /sop_subdivide/outputcrease[in] ⏎
  30: str "creases" -> /sop_subdivide/outcreasegroup[in] ⏎
  31: int 1 -> /sop_subdivide/closeholes[in] ⏎
  32: int 1 -> /sop_subdivide/surroundpoly[in] ⏎
  33: float 1.000000 -> /sop_subdivide/bias[in] ⏎
  34: int 1 -> /sop_subdivide/smoothvertex[in] ⏎
  35: int 0 -> /sop_subdivide/consisttopology[in] ⏎
  36: int 0 -> /sop_subdivide/linearcreases[in] ⏎
  37: int 0 -> /sop_subdivide/algorithm[in] ⏎
  38: int 0 -> /sop_subdivide/buildpolysoups[in] ⏎
  39: int 0 -> /sop_subdivide/indepcurves[in] ⏎
  40: int 1 -> /sop_subdivide/updatenmls[in] ⏎
  41: int 1 -> /sop_subdivide/removeholes[in] ⏎
  42: str "edge only" -> /sop_subdivide/vtxboundary[in] ⏎
  43: str "corners plus1" -> /sop_subdivide/fvarlinear[in] ⏎
  44: str "uniform" -> /sop_subdivide/creasemethod[in] ⏎
  45: str "catmull-clark" -> /sop_subdivide/trianglesubd[in] ⏎
  46: bool "true" -> /TwoWaySwitchBool1/a[in] ⏎
  47: bool "false" -> /TwoWaySwitchBool1/b[in] ⏎
  48: str "" -> /sop_subdivide1/subdivide[in] ⏎
  49: str "" -> /sop_subdivide1/creases[in] ⏎
  50: int 1 -> /sop_subdivide1/iterations[in] ⏎
  51: int 0 -> /sop_subdivide1/overridecrease[in] ⏎
  52: float 0.000000 -> /sop_subdivide1/creaseweight[in] ⏎
  53: int 0 -> /sop_subdivide1/outputcrease[in] ⏎
  54: str "creases" -> /sop_subdivide1/outcreasegroup[in] ⏎
  55: int 1 -> /sop_subdivide1/closeholes[in] ⏎
  56: int 1 -> /sop_subdivide1/surroundpoly[in] ⏎
  57: float 1.000000 -> /sop_subdivide1/bias[in] ⏎
  58: int 1 -> /sop_subdivide1/smoothvertex[in] ⏎
  59: int 0 -> /sop_subdivide1/consisttopology[in] ⏎
  60: int 0 -> /sop_subdivide1/linearcreases[in] ⏎
  61: int 0 -> /sop_subdivide1/algorithm[in] ⏎
  62: int 0 -> /sop_subdivide1/buildpolysoups[in] ⏎
  63: int 0 -> /sop_subdivide1/indepcurves[in] ⏎
  64: int 1 -> /sop_subdivide1/updatenmls[in] ⏎
  65: int 1 -> /sop_subdivide1/removeholes[in] ⏎
  66: str "edge only" -> /sop_subdivide1/vtxboundary[in] ⏎
  67: str "corners plus1" -> /sop_subdivide1/fvarlinear[in] ⏎
  68: str "uniform" -> /sop_subdivide1/creasemethod[in] ⏎
  69: str "catmull-clark" -> /sop_subdivide1/trianglesubd[in] ⏎
  70: int 4 -> /Modulo2/b[in] ⏎
  71: int 1 -> /EqualsInt2/b[in] ⏎
  72: str "" -> /sop_fractal2/group[in] ⏎
  73: int 3 -> /sop_fractal2/divs[in] ⏎
  74: float 0.500000 -> /sop_fractal2/smooth[in] ⏎
  75: float 1.000000 -> /sop_fractal2/scale[in] ⏎
  76: int 0 -> /sop_fractal2/seed[in] ⏎
  77: int 0 -> /sop_fractal2/fixed[in] ⏎
  78: int 0 -> /sop_fractal2/vtxnms[in] ⏎
  79: str "N" -> /sop_fractal2/nmlattrib[in] ⏎
  80: vector3 "{ x := 0.000000, y := 1.000000, z := 0.000000 }" -> /sop_fractal2/dir[in] ⏎
  81: bool "true" -> /TwoWaySwitchBool2/a[in] ⏎
  82: bool "false" -> /TwoWaySwitchBool2/b[in] ⏎
  83: str "" -> /sop_subdivide2/subdivide[in] ⏎
  84: str "" -> /sop_subdivide2/creases[in] ⏎
  85: int 1 -> /sop_subdivide2/iterations[in] ⏎
  86: int 0 -> /sop_subdivide2/overridecrease[in] ⏎
  87: float 0.000000 -> /sop_subdivide2/creaseweight[in] ⏎
  88: int 0 -> /sop_subdivide2/outputcrease[in] ⏎
  89: str "creases" -> /sop_subdivide2/outcreasegroup[in] ⏎
  90: int 1 -> /sop_subdivide2/closeholes[in] ⏎
  91: int 1 -> /sop_subdivide2/surroundpoly[in] ⏎
  92: float 1.000000 -> /sop_subdivide2/bias[in] ⏎
  93: int 1 -> /sop_subdivide2/smoothvertex[in] ⏎
  94: int 0 -> /sop_subdivide2/consisttopology[in] ⏎
  95: int 0 -> /sop_subdivide2/linearcreases[in] ⏎
  96: int 0 -> /sop_subdivide2/algorithm[in] ⏎
  97: int 0 -> /sop_subdivide2/buildpolysoups[in] ⏎
  98: int 0 -> /sop_subdivide2/indepcurves[in] ⏎
  99: int 1 -> /sop_subdivide2/updatenmls[in] ⏎
  100: int 1 -> /sop_subdivide2/removeholes[in] ⏎
  101: str "edge only" -> /sop_subdivide2/vtxboundary[in] ⏎
  102: str "corners plus1" -> /sop_subdivide2/fvarlinear[in] ⏎
  103: str "uniform" -> /sop_subdivide2/creasemethod[in] ⏎
  104: str "catmull-clark" -> /sop_subdivide2/trianglesubd[in]
-/
#guard_msgs in
#apex_graph fun (geo : Geometry) (x : Int) => Id.run do
  let mut geo := geo

  if x % 2 == 0 then
    geo := geo.fractal
  else
    geo := geo.subdivide

  if x % 4 == 1 then
    geo := geo.fractal
  else
    geo := geo.subdivide

  return geo
