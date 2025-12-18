import HouLean
import HouLean.Apex.Data.UInt64

open HouLean Apex Compiler

open Lean Qq

/--
info: Nodes:
  0: [anonymous] : Value<FloatArray>

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
#apex_graph fun (x : Array Float) => x

/--
info: Nodes:
  0: array_AppendFloat : array::Append<Float>

Ports:
  0: /array_AppendFloat/array[in]
  1: /array_AppendFloat/value[in]
  2: /array_AppendFloat/fst[out]
  3: /array_AppendFloat/snd[out]

Inputs:
  x[in] -> #[/array_AppendFloat/array[in]]
  y[in] -> #[/array_AppendFloat/value[in]]

Outputs:
  /array_AppendFloat/fst[out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun (x : Array Float) (y : Float) => x.push y


/--
info: Nodes:
  0: array_AddFloat : array::Add<Float>

Ports:
  0: /array_AddFloat/[anonymous][out]
  1: /array_AddFloat/a[in]
  2: /array_AddFloat/b[in]
  3: /array_AddFloat/[anonymous][out]

Inputs:
  x[in] -> #[/array_AddFloat/a[in], /array_AddFloat/b[in]]

Outputs:
  /array_AddFloat/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun (x : Array Float) => x + x

/--
info: Nodes:
  0: array_MultiplyFloat : array::Multiply<Float>

Ports:
  0: /array_MultiplyFloat/[anonymous][out]
  1: /array_MultiplyFloat/a[in]
  2: /array_MultiplyFloat/b[in]
  3: /array_MultiplyFloat/[anonymous][out]

Inputs:
  x[in] -> #[/array_MultiplyFloat/a[in], /array_MultiplyFloat/b[in]]

Outputs:
  /array_MultiplyFloat/[anonymous][out] -> [anonymous][out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun (x : Array Float) => x * x


-- todo: fix this
/--
error: Failed to apply implemented_by override Decidable.decide -> decide.apex_impl
failed to synthesize
  ApexDecidable ((fun xs i => i < xs.size) x 0)

Hint: Additional diagnostic information may be available using the `set_option diagnostics true` command.
, unfolded: dite.apex_impl
-/
#guard_msgs in
#apex_graph fun (x : Array Float) => x[0]?

/--
info: Nodes:
  0: array_GetFloat : array::Get<Float>

Ports:
  0: /array_GetFloat/array[in]
  1: /array_GetFloat/index[in]
  2: /array_GetFloat/default[in]
  3: /array_GetFloat/fst[out]
  4: /array_GetFloat/snd[out]

Inputs:
  x[in] -> #[/array_GetFloat/array[in]]
  i[in] -> #[/array_GetFloat/index[in]]

Outputs:
  /array_GetFloat/fst[out] -> [anonymous][out]

Wires:

Literals:
  0: float 0.0 -> /array_GetFloat/default[in]
-/
#guard_msgs in
#apex_graph fun (x : Array Float) (i : Nat) => x[i]!


/--
info: Nodes:
  0: array_AddFloat : array::Add<Float>
  1: array_AddFloat1 : array::Add<Float>

Ports:
  0: /array_AddFloat/[anonymous][out]
  1: /array_AddFloat/a[in]
  2: /array_AddFloat/b[in]
  3: /array_AddFloat/[anonymous][out]
  4: /array_AddFloat1/[anonymous][out]
  5: /array_AddFloat1/a[in]
  6: /array_AddFloat1/b[in]
  7: /array_AddFloat1/[anonymous][out]

Inputs:
  x.fst[in] -> #[/array_AddFloat/a[in], /array_AddFloat/b[in]]
  x.snd[in] -> #[/array_AddFloat1/a[in], /array_AddFloat1/b[in]]

Outputs:
  /array_AddFloat/[anonymous][out] -> fst[out]
  /array_AddFloat1/[anonymous][out] -> snd[out]

Wires:

Literals:
-/
#guard_msgs in
#apex_graph fun (x : Array (Float × Float)) => x + x

/--
info: Nodes:
  0: array_AddFloat : array::Add<Float>
  1: array_AddFloat1 : array::Add<Float>
  2: array_AddInt : array::Add<Int>
  3: array_AppendFloat : array::Append<Float>
  4: array_AppendFloat1 : array::Append<Float>
  5: array_AppendInt : array::Append<Int>

Ports:
  0: /array_AddFloat/[anonymous][out]
  1: /array_AddFloat/a[in]
  2: /array_AddFloat/b[in]
  3: /array_AddFloat/[anonymous][out]
  4: /array_AddFloat1/[anonymous][out]
  5: /array_AddFloat1/a[in]
  6: /array_AddFloat1/b[in]
  7: /array_AddFloat1/[anonymous][out]
  8: /array_AddInt/[anonymous][out]
  9: /array_AddInt/a[in]
  10: /array_AddInt/b[in]
  11: /array_AddInt/[anonymous][out]
  12: /array_AppendFloat/array[in]
  13: /array_AppendFloat/value[in]
  14: /array_AppendFloat/fst[out]
  15: /array_AppendFloat/snd[out]
  16: /array_AppendFloat1/array[in]
  17: /array_AppendFloat1/value[in]
  18: /array_AppendFloat1/fst[out]
  19: /array_AppendFloat1/snd[out]
  20: /array_AppendInt/array[in]
  21: /array_AppendInt/value[in]
  22: /array_AppendInt/fst[out]
  23: /array_AppendInt/snd[out]

Inputs:
  x.fst[in] -> #[/array_AddFloat/a[in], /array_AddFloat/b[in]]
  x.snd.fst[in] -> #[/array_AddFloat1/a[in], /array_AddFloat1/b[in]]
  x.snd.snd[in] -> #[/array_AddInt/a[in], /array_AddInt/b[in]]
  a[in] -> #[/array_AppendFloat/value[in], /array_AppendFloat1/value[in]]
  i[in] -> #[/array_AppendInt/value[in]]

Outputs:
  /array_AppendFloat/fst[out] -> fst[out]
  /array_AppendFloat1/fst[out] -> snd.fst[out]
  /array_AppendInt/fst[out] -> snd.snd[out]

Wires:
  0: /array_AddFloat/[anonymous][out] -> /array_AppendFloat/array[in]
  1: /array_AddFloat1/[anonymous][out] -> /array_AppendFloat1/array[in]
  2: /array_AddInt/[anonymous][out] -> /array_AppendInt/array[in]

Literals:
-/
#guard_msgs in
#apex_graph fun (x : Array (Float × Float × Int)) (a : Float) (i : Int) => (x + x).push (a,a,i)
