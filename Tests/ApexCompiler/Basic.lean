import HouLean

open HouLean Apex Compiler

open Qq

set_option linter.floatUsage false

/--
info: Nodes:
  0: [anonymous] : Value<Float>

Ports:
  0: /[anonymous]/parm[in]
  1: /[anonymous]/[anonymous][out]

Inputs:
  x[in] -> #[/[anonymous]/parm[in]]

Outputs:
  /[anonymous]/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun x : Float => let y := x; let z := y; z


/--
info: Nodes:
  0: AddFloat : Add<Float>

Ports:
  0: /AddFloat/a[in]
  1: /AddFloat/b[⋯][in]
  2: /AddFloat/[anonymous][out]

Inputs:
  x[in] -> #[/AddFloat/a[in], /AddFloat/b[0][in]]

Outputs:
  /AddFloat/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun x : Float => x + x


/--
info: Nodes:
  0: AddInt : Add<Int>
  1: value : Value<Int>

Ports:
  0: /AddInt/a[in]
  1: /AddInt/b[⋯][in]
  2: /AddInt/[anonymous][out]
  3: /value/parm[in]
  4: /value/value[out]

Inputs:
  x[in] -> #[/AddInt/a[in]]

Outputs:
  /AddInt/[anonymous][out] -> [anonymous][out]

Wires:
  0: /value/value[out] -> /AddInt/b[0][in]

Literals:
  0: int 0 -> /value/parm[in]
-/
#guard_msgs in
#apex_graph fun x : Int => x + 0



/--
info: Nodes:
  0: AddInt : Add<Int>

Ports:
  0: /AddInt/a[in]
  1: /AddInt/b[⋯][in]
  2: /AddInt/[anonymous][out]

Inputs:
  x[in] -> #[/AddInt/b[0][in]]

Outputs:
  /AddInt/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
  0: int 0 -> /AddInt/a[in]
-/
#guard_msgs in
#apex_graph fun x : Int => 0 + x


/--
info: Nodes:
  0: [anonymous] : Value<Int>
  1: _x : Value<Int>

Ports:
  0: /[anonymous]/parm[in]
  1: /[anonymous]/[anonymous][out]
  2: /_x/parm[in]
  3: /_x/[anonymous][out]

Inputs:
  _x[in] -> #[/_x/parm[in]]

Outputs:
  /[anonymous]/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
  0: int 0 -> /[anonymous]/parm[in]
-/
#guard_msgs in
#apex_graph fun _x : Int => 0


/--
info: Nodes:
  0: AddFloat : Add<Float>
  1: MultiplyFloat : Multiply<Float>

Ports:
  0: /AddFloat/a[in]
  1: /AddFloat/b[⋯][in]
  2: /AddFloat/[anonymous][out]
  3: /MultiplyFloat/a[in]
  4: /MultiplyFloat/b[⋯][in]
  5: /MultiplyFloat/[anonymous][out]

Inputs:
  x[in] -> #[/AddFloat/a[in], /AddFloat/b[0][in], /MultiplyFloat/b[0][in]]

Outputs:
  /MultiplyFloat/[anonymous][out] -> [anonymous][out]

Wires:
  0: /AddFloat/[anonymous][out] -> /MultiplyFloat/a[in]

Literals:
-/
#guard_msgs in
#apex_graph fun x : Float => let y := x + x; y * x


/--
info: Nodes:
  0: AddFloat : Add<Float>

Ports:
  0: /AddFloat/a[in]
  1: /AddFloat/b[⋯][in]
  2: /AddFloat/[anonymous][out]

Inputs:
  x[in] -> #[/AddFloat/a[in], /AddFloat/b[0][in], /AddFloat/b[1][in], /AddFloat/b[2][in]]

Outputs:
  /AddFloat/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun x : Float => Generated.AddFloat x #a[x,x,x]


/--
info: Nodes:
  0: DivideFloat : Divide<Float>
  1: SubtractFloat : Subtract<Float>
  2: MultiplyFloat : Multiply<Float>
  3: value : Value<Float>
  4: AddFloat : Add<Float>
  5: MultiplyFloat1 : Multiply<Float>

Ports:
  0: /DivideFloat/a[in]
  1: /DivideFloat/b[⋯][in]
  2: /DivideFloat/[anonymous][out]
  3: /SubtractFloat/a[in]
  4: /SubtractFloat/b[⋯][in]
  5: /SubtractFloat/[anonymous][out]
  6: /MultiplyFloat/a[in]
  7: /MultiplyFloat/b[⋯][in]
  8: /MultiplyFloat/[anonymous][out]
  9: /value/parm[in]
  10: /value/value[out]
  11: /AddFloat/a[in]
  12: /AddFloat/b[⋯][in]
  13: /AddFloat/[anonymous][out]
  14: /MultiplyFloat1/a[in]
  15: /MultiplyFloat1/b[⋯][in]
  16: /MultiplyFloat1/[anonymous][out]

Inputs:
  x[in] -> #[/DivideFloat/a[in], /DivideFloat/b[0][in], /SubtractFloat/b[0][in], /AddFloat/a[in]]

Outputs:
  /MultiplyFloat1/[anonymous][out] -> [anonymous][out]

Wires:
  0: /DivideFloat/[anonymous][out] -> /SubtractFloat/a[in]
  1: /SubtractFloat/[anonymous][out] -> /MultiplyFloat/a[in]
  2: /value/value[out] -> /MultiplyFloat/b[0][in]
  3: /MultiplyFloat/[anonymous][out] -> /AddFloat/b[0][in]
  4: /DivideFloat/[anonymous][out] -> /MultiplyFloat1/a[in]
  5: /AddFloat/[anonymous][out] -> /MultiplyFloat1/b[0][in]

Literals:
  0: float 0.300000 -> /value/parm[in]
-/
#guard_msgs in
#apex_graph fun x : Float => let y := x/x; y* Math.lerp x y 0.3


/--
info: Nodes:
  0: AddInt : Add<Int>
  1: value : Value<Int>

Ports:
  0: /AddInt/a[in]
  1: /AddInt/b[⋯][in]
  2: /AddInt/[anonymous][out]
  3: /value/parm[in]
  4: /value/value[out]

Inputs:
  x[in] -> #[/AddInt/a[in]]

Outputs:
  /AddInt/[anonymous][out] -> [anonymous][out]

Wires:
  0: /value/value[out] -> /AddInt/b[0][in]

Literals:
  0: int 30 -> /value/parm[in]
-/
#guard_msgs in
#apex_graph fun x : Int => x + (10 + 20)

-- /--
-- info: Nodes:

-- Ports:

-- Wires:

-- Literals:
-- -/
-- #guard_msgs in
-- #apex_graph false -- this is wrong :(


/--
info: Nodes:
  0: [anonymous] : Value<Float>
  1: x.snd.fst : Value<Float>
  2: x.snd.snd : Value<Float>

Ports:
  0: /[anonymous]/parm[in]
  1: /[anonymous]/[anonymous][out]
  2: /x.snd.fst/parm[in]
  3: /x.snd.fst/[anonymous][out]
  4: /x.snd.snd/parm[in]
  5: /x.snd.snd/[anonymous][out]

Inputs:
  x.fst[in] -> #[/[anonymous]/parm[in]]
  x.snd.fst[in] -> #[/x.snd.fst/parm[in]]
  x.snd.snd[in] -> #[/x.snd.snd/parm[in]]

Outputs:
  /[anonymous]/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun x : Float×Float×Float => x.1


/--
info: Nodes:
  0: [anonymous] : Value<Float>
  1: x.fst : Value<Float>
  2: x.snd.snd : Value<Float>

Ports:
  0: /[anonymous]/parm[in]
  1: /[anonymous]/[anonymous][out]
  2: /x.fst/parm[in]
  3: /x.fst/[anonymous][out]
  4: /x.snd.snd/parm[in]
  5: /x.snd.snd/[anonymous][out]

Inputs:
  x.fst[in] -> #[/x.fst/parm[in]]
  x.snd.fst[in] -> #[/[anonymous]/parm[in]]
  x.snd.snd[in] -> #[/x.snd.snd/parm[in]]

Outputs:
  /[anonymous]/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun x : Float×Float×Float => x.2.1


/--
info: Nodes:
  0: [anonymous] : Value<Float>
  1: x.fst : Value<Float>
  2: x.snd.fst : Value<Float>

Ports:
  0: /[anonymous]/parm[in]
  1: /[anonymous]/[anonymous][out]
  2: /x.fst/parm[in]
  3: /x.fst/[anonymous][out]
  4: /x.snd.fst/parm[in]
  5: /x.snd.fst/[anonymous][out]

Inputs:
  x.fst[in] -> #[/x.fst/parm[in]]
  x.snd.fst[in] -> #[/x.snd.fst/parm[in]]
  x.snd.snd[in] -> #[/[anonymous]/parm[in]]

Outputs:
  /[anonymous]/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun x : Float×Float×Float => x.2.2


/--
info: Nodes:
  0: [anonymous] : Value<Float>
  1: x.fst : Value<Float>
  2: x.snd.fst : Value<Float>
  3: x.snd.snd.snd.fst : Value<Float>
  4: x.snd.snd.snd.snd : Value<Float>

Ports:
  0: /[anonymous]/parm[in]
  1: /[anonymous]/[anonymous][out]
  2: /x.fst/parm[in]
  3: /x.fst/[anonymous][out]
  4: /x.snd.fst/parm[in]
  5: /x.snd.fst/[anonymous][out]
  6: /x.snd.snd.snd.fst/parm[in]
  7: /x.snd.snd.snd.fst/[anonymous][out]
  8: /x.snd.snd.snd.snd/parm[in]
  9: /x.snd.snd.snd.snd/[anonymous][out]

Inputs:
  x.fst[in] -> #[/x.fst/parm[in]]
  x.snd.fst[in] -> #[/x.snd.fst/parm[in]]
  x.snd.snd.fst[in] -> #[/[anonymous]/parm[in]]
  x.snd.snd.snd.fst[in] -> #[/x.snd.snd.snd.fst/parm[in]]
  x.snd.snd.snd.snd[in] -> #[/x.snd.snd.snd.snd/parm[in]]

Outputs:
  /[anonymous]/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun x : Float×Float×Float×Float×Float => x.2.2.1


/--
info: Nodes:
  0: fst : Value<Float>
  1: snd.fst : Value<Float>
  2: snd.snd : Value<Float>

Ports:
  0: /fst/parm[in]
  1: /fst/[anonymous][out]
  2: /snd.fst/parm[in]
  3: /snd.fst/[anonymous][out]
  4: /snd.snd/parm[in]
  5: /snd.snd/[anonymous][out]

Inputs:
  x.fst[in] -> #[/fst/parm[in]]
  x.snd.fst[in] -> #[/snd.fst/parm[in]]
  x.snd.snd[in] -> #[/snd.snd/parm[in]]

Outputs:
  /fst/[anonymous][out] -> fst[out]
  /snd.fst/[anonymous][out] -> snd.fst[out]
  /snd.snd/[anonymous][out] -> snd.snd[out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun x : Float×Float×Float => x


/--
info: Nodes:
  0: Vector3ToFloat : Vector3ToFloat

Ports:
  0: /Vector3ToFloat/vector[in]
  1: /Vector3ToFloat/fst[out]
  2: /Vector3ToFloat/snd.fst[out]
  3: /Vector3ToFloat/snd.snd[out]

Inputs:
  v[in] -> #[/Vector3ToFloat/vector[in]]

Outputs:
  /Vector3ToFloat/fst[out] -> fst[out]
  /Vector3ToFloat/snd.fst[out] -> snd.fst[out]
  /Vector3ToFloat/snd.snd[out] -> snd.snd.fst[out]
  /Vector3ToFloat/snd.snd[out] -> snd.snd.snd.fst[out]
  /Vector3ToFloat/snd.fst[out] -> snd.snd.snd.snd[out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun v : Vector3 => (v.x, v.y, v.z, (v.z, v.y))


@[apex]
def getBBoxSize (geo : Geometry) : Vector3 :=
  let r := geo.boundingBox
  let size := r.size
  size


abbrev match1 := fun v : Vector3 => let ⟨x,y,_⟩ := v; (x,y)

/--
info: Nodes:
  0: Vector3ToFloat : Vector3ToFloat

Ports:
  0: /Vector3ToFloat/vector[in]
  1: /Vector3ToFloat/fst[out]
  2: /Vector3ToFloat/snd.fst[out]
  3: /Vector3ToFloat/snd.snd[out]

Inputs:
  v[in] -> #[/Vector3ToFloat/vector[in]]

Outputs:
  /Vector3ToFloat/fst[out] -> fst[out]
  /Vector3ToFloat/snd.fst[out] -> snd[out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph match1




def add' (x y : Float) : Float := x + y

/--
info: Nodes:
  0: AddFloat : Add<Float>

Ports:
  0: /AddFloat/a[in]
  1: /AddFloat/b[⋯][in]
  2: /AddFloat/[anonymous][out]

Inputs:
  x[in] -> #[/AddFloat/a[in]]
  y[in] -> #[/AddFloat/b[0][in]]

Outputs:
  /AddFloat/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun x y => add' x y


/--
info: Nodes:
  0: AddInt : Add<Int>

Ports:
  0: /AddInt/a[in]
  1: /AddInt/b[⋯][in]
  2: /AddInt/[anonymous][out]

Inputs:
  x[in] -> #[/AddInt/a[in]]
  y[in] -> #[/AddInt/b[0][in]]

Outputs:
  /AddInt/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun x y : Nat => x + y


/--
info: Nodes:
  0: fst : Value<Int>
  1: snd.fst : Value<String>
  2: snd.snd.fst : Value<Float>
  3: snd.snd.snd : Value<Bool>

Ports:
  0: /fst/parm[in]
  1: /fst/[anonymous][out]
  2: /snd.fst/parm[in]
  3: /snd.fst/[anonymous][out]
  4: /snd.snd.fst/parm[in]
  5: /snd.snd.fst/[anonymous][out]
  6: /snd.snd.snd/parm[in]
  7: /snd.snd.snd/[anonymous][out]

Outputs:
  /fst/[anonymous][out] -> fst[out]
  /snd.fst/[anonymous][out] -> snd.fst[out]
  /snd.snd.fst/[anonymous][out] -> snd.snd.fst[out]
  /snd.snd.snd/[anonymous][out] -> snd.snd.snd[out]

Wires:

Literals:
  0: int 0 -> /fst/parm[in] ⏎
  1: str "hello" -> /snd.fst/parm[in] ⏎
  2: float 3.141590 -> /snd.snd.fst/parm[in] ⏎
  3: bool "true" -> /snd.snd.snd/parm[in]
-/
#guard_msgs in
#apex_graph ((0:Nat), "hello", 3.14159, true)


/--
info: Nodes:
  0: fst : Value<Int>
  1: snd.fst : Value<String>
  2: snd.snd.fst : Value<Float>
  3: snd.snd.snd : Value<Bool>
  4: _x : Value<Int>

Ports:
  0: /fst/parm[in]
  1: /fst/[anonymous][out]
  2: /snd.fst/parm[in]
  3: /snd.fst/[anonymous][out]
  4: /snd.snd.fst/parm[in]
  5: /snd.snd.fst/[anonymous][out]
  6: /snd.snd.snd/parm[in]
  7: /snd.snd.snd/[anonymous][out]
  8: /_x/parm[in]
  9: /_x/[anonymous][out]

Inputs:
  _x[in] -> #[/_x/parm[in]]

Outputs:
  /fst/[anonymous][out] -> fst[out]
  /snd.fst/[anonymous][out] -> snd.fst[out]
  /snd.snd.fst/[anonymous][out] -> snd.snd.fst[out]
  /snd.snd.snd/[anonymous][out] -> snd.snd.snd[out]

Wires:

Literals:
  0: int 0 -> /fst/parm[in] ⏎
  1: str "hello" -> /snd.fst/parm[in] ⏎
  2: float 3.141590 -> /snd.snd.fst/parm[in] ⏎
  3: bool "true" -> /snd.snd.snd/parm[in]
-/
#guard_msgs in
#apex_graph (fun _x : Int => ((0:Nat), "hello", 3.14159, true))



/--
info: Nodes:
  0: fst : Value<Int>
  1: snd.fst : Value<Int>
  2: snd.snd : Value<Bool>

Ports:
  0: /fst/parm[in]
  1: /fst/[anonymous][out]
  2: /snd.fst/parm[in]
  3: /snd.fst/[anonymous][out]
  4: /snd.snd/parm[in]
  5: /snd.snd/[anonymous][out]

Inputs:
  x[in] -> #[/fst/parm[in]]

Outputs:
  /fst/[anonymous][out] -> fst[out]
  /snd.fst/[anonymous][out] -> snd.fst[out]
  /snd.snd/[anonymous][out] -> snd.snd[out]

Wires:

Literals:
  0: int 0 -> /snd.fst/parm[in] ⏎
  1: bool "false" -> /snd.snd/parm[in]
-/
#guard_msgs in
#apex_graph fun x : Int => (x, (none : Option Int))



/--
info: Nodes:
  0: fst : Value<Int>
  1: snd.snd : Value<Bool>

Ports:
  0: /fst/parm[in]
  1: /fst/[anonymous][out]
  2: /snd.snd/parm[in]
  3: /snd.snd/[anonymous][out]

Inputs:
  x[in] -> #[/fst/parm[in]]

Outputs:
  /fst/[anonymous][out] -> fst[out]
  /snd.snd/[anonymous][out] -> snd.snd[out]

Wires:

Literals:
  0: bool "false" -> /snd.snd/parm[in]
-/
#guard_msgs in
#apex_graph fun x : Int => (x, (none : Option Geometry))



/--
info: Nodes:
  0: [anonymous] : Value<Vector2>
  1: _x : Value<Float>

Ports:
  0: /[anonymous]/parm[in]
  1: /[anonymous]/[anonymous][out]
  2: /_x/parm[in]
  3: /_x/[anonymous][out]

Inputs:
  _x[in] -> #[/_x/parm[in]]

Outputs:
  /[anonymous]/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
  0: vector2 "{ x := 0.000000, y := 0.000000 }" -> /[anonymous]/parm[in]
-/
#guard_msgs in
#apex_graph fun _x : Float => (⟨0,0⟩ : Vector2)


/--
info: Nodes:
  0: [anonymous] : Value<Vector3>
  1: _x : Value<Float>

Ports:
  0: /[anonymous]/parm[in]
  1: /[anonymous]/[anonymous][out]
  2: /_x/parm[in]
  3: /_x/[anonymous][out]

Inputs:
  _x[in] -> #[/_x/parm[in]]

Outputs:
  /[anonymous]/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
  0: vector3 "{ x := 0.000000, y := 0.000000, z := 0.000000 }" -> /[anonymous]/parm[in]
-/
#guard_msgs in
#apex_graph fun _x : Float => (⟨0,0,0⟩ : Vector3)





/--
info: Nodes:
  0: [anonymous] : Value<String>
  1: _x : Value<Float>

Ports:
  0: /[anonymous]/parm[in]
  1: /[anonymous]/[anonymous][out]
  2: /_x/parm[in]
  3: /_x/[anonymous][out]

Inputs:
  _x[in] -> #[/_x/parm[in]]

Outputs:
  /[anonymous]/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
  0: str "ab" -> /[anonymous]/parm[in]
-/
#guard_msgs in
#apex_graph fun _x : Float => "a" ++ "b"
