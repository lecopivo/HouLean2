import HouLean

open HouLean Apex Compiler


abbrev loop1 := fun (geo : Geometry) => Id.run do
    let mut geo := geo
    let bbox := geo.boundingBox
    for i in [0:geo.numPoints.toNat] do
      let P : Vector3 := geo.pointAttrib (Int.ofNat i) "P"
      let relP := (P - bbox.min).compDiv bbox.size
      geo := geo.setPointAttrib (Int.ofNat i) "Cd" relP
    return geo

-- -- set_option trace.HouLean.Apex.compiler true in
-- @[apex]
-- def loop_set_attrib (geo : Geometry) : Geometry := Id.run do
--   let mut geo := geo
--   if geo.numPoints % 2 = 0 then
--     geo := geo.setPointAttrib (Int.ofNat 0) "Cd" (Vector3.mk 1 0 0)
--   else
--     geo := geo.setPointAttrib (Int.ofNat 0) "Cd" (Vector3.mk 0 0 0)
--   geo := geo.subdivide
--   return geo
--   -- for i in [0:4] do
--   -- return geo

-- #check (by infer_instance : ApexTypeFlatten (Geometry) _)
-- #check (by infer_instance : ApexTypeFlatten (Geometry × Nat) _)
-- #check (by infer_instance : ApexTypeFlatten (Geometry × Id (Int × Nat × Geometry × Unit)) _)

open Qq
/--
info: Nodes:
  0: geo_BoundingBox : geo::BoundingBox
  1: geo_NumPoints : geo::NumPoints
  2: MaxInt : Max<Int>
  3: value : Value<Int>
  4: SubtractInt : Subtract<Int>
  5: value : Value<Int>
  6: MaxInt1 : Max<Int>
  7: value : Value<Int>
  8: AddInt : Add<Int>
  9: value : Value<Int>
  10: SubtractInt1 : Subtract<Int>
  11: value : Value<Int>
  12: MaxInt2 : Max<Int>
  13: value : Value<Int>
  14: DivideInt : Divide<Int>
  15: value : Value<Int>
  16: ForBegin : ForBegin
  17: MultiplyInt : Multiply<Int>
  18: value : Value<Int>
  19: AddInt1 : Add<Int>
  20: MaxInt3 : Max<Int>
  21: value : Value<Int>
  22: geo_PointAttribValueVector3 : geo::PointAttribValue<Vector3>
  23: Vector3ToFloat : Vector3ToFloat
  24: Vector3ToFloat1 : Vector3ToFloat
  25: SubtractFloat : Subtract<Float>
  26: Vector3ToFloat2 : Vector3ToFloat
  27: DivideFloat : Divide<Float>
  28: SubtractFloat1 : Subtract<Float>
  29: DivideFloat1 : Divide<Float>
  30: SubtractFloat2 : Subtract<Float>
  31: DivideFloat2 : Divide<Float>
  32: FloatToVector3 : FloatToVector3
  33: geo_SetPointAttribValueVector3 : geo::SetPointAttribValue<Vector3>
  34: ForEnd : ForEnd

Ports:
  0: /geo_BoundingBox/geo[in]
  1: /geo_BoundingBox/orient[in]
  2: /geo_BoundingBox/local'[in]
  3: /geo_BoundingBox/fst[out]
  4: /geo_BoundingBox/snd.fst[out]
  5: /geo_BoundingBox/snd.snd.fst[out]
  6: /geo_BoundingBox/snd.snd.snd.fst[out]
  7: /geo_BoundingBox/snd.snd.snd.snd[out]
  8: /geo_NumPoints/geo[in]
  9: /geo_NumPoints/[anonymous][out]
  10: /MaxInt/a[in]
  11: /MaxInt/b[⋯][in]
  12: /MaxInt/[anonymous][out]
  13: /value/parm[in]
  14: /value/value[out]
  15: /SubtractInt/a[in]
  16: /SubtractInt/b[⋯][in]
  17: /SubtractInt/[anonymous][out]
  18: /value/parm[in]
  19: /value/value[out]
  20: /MaxInt1/a[in]
  21: /MaxInt1/b[⋯][in]
  22: /MaxInt1/[anonymous][out]
  23: /value/parm[in]
  24: /value/value[out]
  25: /AddInt/a[in]
  26: /AddInt/b[⋯][in]
  27: /AddInt/[anonymous][out]
  28: /value/parm[in]
  29: /value/value[out]
  30: /SubtractInt1/a[in]
  31: /SubtractInt1/b[⋯][in]
  32: /SubtractInt1/[anonymous][out]
  33: /value/parm[in]
  34: /value/value[out]
  35: /MaxInt2/a[in]
  36: /MaxInt2/b[⋯][in]
  37: /MaxInt2/[anonymous][out]
  38: /value/parm[in]
  39: /value/value[out]
  40: /DivideInt/a[in]
  41: /DivideInt/b[⋯][in]
  42: /DivideInt/[anonymous][out]
  43: /value/parm[in]
  44: /value/value[out]
  45: /ForBegin/[anonymous][out]
  46: /ForBegin/iterations[in]
  47: /ForBegin/spare[⋯][in]
  48: /ForBegin/fst[out]
  49: /ForBegin/snd.fst[out]
  50: /ForBegin/snd.snd[⋯][out]
  51: /MultiplyInt/a[in]
  52: /MultiplyInt/b[⋯][in]
  53: /MultiplyInt/[anonymous][out]
  54: /value/parm[in]
  55: /value/value[out]
  56: /AddInt1/a[in]
  57: /AddInt1/b[⋯][in]
  58: /AddInt1/[anonymous][out]
  59: /MaxInt3/a[in]
  60: /MaxInt3/b[⋯][in]
  61: /MaxInt3/[anonymous][out]
  62: /value/parm[in]
  63: /value/value[out]
  64: /geo_PointAttribValueVector3/[anonymous][out]
  65: /geo_PointAttribValueVector3/geo[in]
  66: /geo_PointAttribValueVector3/elemnum[in]
  67: /geo_PointAttribValueVector3/attribname[in]
  68: /geo_PointAttribValueVector3/fst[out]
  69: /geo_PointAttribValueVector3/snd[out]
  70: /Vector3ToFloat/vector[in]
  71: /Vector3ToFloat/fst[out]
  72: /Vector3ToFloat/snd.fst[out]
  73: /Vector3ToFloat/snd.snd[out]
  74: /Vector3ToFloat1/vector[in]
  75: /Vector3ToFloat1/fst[out]
  76: /Vector3ToFloat1/snd.fst[out]
  77: /Vector3ToFloat1/snd.snd[out]
  78: /SubtractFloat/a[in]
  79: /SubtractFloat/b[⋯][in]
  80: /SubtractFloat/[anonymous][out]
  81: /Vector3ToFloat2/vector[in]
  82: /Vector3ToFloat2/fst[out]
  83: /Vector3ToFloat2/snd.fst[out]
  84: /Vector3ToFloat2/snd.snd[out]
  85: /DivideFloat/a[in]
  86: /DivideFloat/b[⋯][in]
  87: /DivideFloat/[anonymous][out]
  88: /SubtractFloat1/a[in]
  89: /SubtractFloat1/b[⋯][in]
  90: /SubtractFloat1/[anonymous][out]
  91: /DivideFloat1/a[in]
  92: /DivideFloat1/b[⋯][in]
  93: /DivideFloat1/[anonymous][out]
  94: /SubtractFloat2/a[in]
  95: /SubtractFloat2/b[⋯][in]
  96: /SubtractFloat2/[anonymous][out]
  97: /DivideFloat2/a[in]
  98: /DivideFloat2/b[⋯][in]
  99: /DivideFloat2/[anonymous][out]
  100: /FloatToVector3/x[in]
  101: /FloatToVector3/y[in]
  102: /FloatToVector3/z[in]
  103: /FloatToVector3/[anonymous][out]
  104: /geo_SetPointAttribValueVector3/[anonymous][out]
  105: /geo_SetPointAttribValueVector3/geo[in]
  106: /geo_SetPointAttribValueVector3/elemnum[in]
  107: /geo_SetPointAttribValueVector3/attribname[in]
  108: /geo_SetPointAttribValueVector3/value[in]
  109: /geo_SetPointAttribValueVector3/fst[out]
  110: /geo_SetPointAttribValueVector3/snd[out]
  111: /ForEnd/[anonymous][out]
  112: /ForEnd/scope[in]
  113: /ForEnd/spare[⋯][in]
  114: /ForEnd/[anonymous][⋯][out]

Inputs:
  geo[in] -> #[/geo_BoundingBox/geo[in], /geo_NumPoints/geo[in], /ForBegin/spare[0][in], /ForBegin/spare[1][in]]

Outputs:
  /ForEnd/[anonymous][0][out] -> «0»[out]

Wires:
  0: /geo_NumPoints/[anonymous][out] -> /MaxInt/a[in]
  1: /value/value[out] -> /MaxInt/b[0][in]
  2: /MaxInt/[anonymous][out] -> /SubtractInt/a[in]
  3: /value/value[out] -> /SubtractInt/b[0][in]
  4: /SubtractInt/[anonymous][out] -> /MaxInt1/a[in]
  5: /value/value[out] -> /MaxInt1/b[0][in]
  6: /MaxInt1/[anonymous][out] -> /AddInt/a[in]
  7: /value/value[out] -> /AddInt/b[0][in]
  8: /AddInt/[anonymous][out] -> /SubtractInt1/a[in]
  9: /value/value[out] -> /SubtractInt1/b[0][in]
  10: /SubtractInt1/[anonymous][out] -> /MaxInt2/a[in]
  11: /value/value[out] -> /MaxInt2/b[0][in]
  12: /MaxInt2/[anonymous][out] -> /DivideInt/a[in]
  13: /value/value[out] -> /DivideInt/b[0][in]
  14: /DivideInt/[anonymous][out] -> /ForBegin/iterations[in]
  15: /geo_BoundingBox/fst[out] -> /ForBegin/spare[2][in]
  16: /geo_BoundingBox/snd.fst[out] -> /ForBegin/spare[3][in]
  17: /geo_BoundingBox/snd.snd.fst[out] -> /ForBegin/spare[4][in]
  18: /geo_BoundingBox/snd.snd.snd.fst[out] -> /ForBegin/spare[5][in]
  19: /geo_BoundingBox/snd.snd.snd.snd[out] -> /ForBegin/spare[6][in]
  20: /ForBegin/snd.fst[out] -> /MultiplyInt/a[in]
  21: /value/value[out] -> /MultiplyInt/b[0][in]
  22: /MultiplyInt/[anonymous][out] -> /AddInt1/b[0][in]
  23: /AddInt1/[anonymous][out] -> /MaxInt3/a[in]
  24: /value/value[out] -> /MaxInt3/b[0][in]
  25: /ForBegin/snd.snd[0][out] -> /geo_PointAttribValueVector3/geo[in]
  26: /MaxInt3/[anonymous][out] -> /geo_PointAttribValueVector3/elemnum[in]
  27: /geo_PointAttribValueVector3/fst[out] -> /Vector3ToFloat/vector[in]
  28: /ForBegin/snd.snd[4][out] -> /Vector3ToFloat1/vector[in]
  29: /Vector3ToFloat/fst[out] -> /SubtractFloat/a[in]
  30: /Vector3ToFloat1/fst[out] -> /SubtractFloat/b[0][in]
  31: /ForBegin/snd.snd[3][out] -> /Vector3ToFloat2/vector[in]
  32: /SubtractFloat/[anonymous][out] -> /DivideFloat/a[in]
  33: /Vector3ToFloat2/fst[out] -> /DivideFloat/b[0][in]
  34: /Vector3ToFloat/snd.fst[out] -> /SubtractFloat1/a[in]
  35: /Vector3ToFloat1/snd.fst[out] -> /SubtractFloat1/b[0][in]
  36: /SubtractFloat1/[anonymous][out] -> /DivideFloat1/a[in]
  37: /Vector3ToFloat2/snd.fst[out] -> /DivideFloat1/b[0][in]
  38: /Vector3ToFloat/snd.snd[out] -> /SubtractFloat2/a[in]
  39: /Vector3ToFloat1/snd.snd[out] -> /SubtractFloat2/b[0][in]
  40: /SubtractFloat2/[anonymous][out] -> /DivideFloat2/a[in]
  41: /Vector3ToFloat2/snd.snd[out] -> /DivideFloat2/b[0][in]
  42: /DivideFloat/[anonymous][out] -> /FloatToVector3/x[in]
  43: /DivideFloat1/[anonymous][out] -> /FloatToVector3/y[in]
  44: /DivideFloat2/[anonymous][out] -> /FloatToVector3/z[in]
  45: /ForBegin/snd.snd[0][out] -> /geo_SetPointAttribValueVector3/geo[in]
  46: /MaxInt3/[anonymous][out] -> /geo_SetPointAttribValueVector3/elemnum[in]
  47: /FloatToVector3/[anonymous][out] -> /geo_SetPointAttribValueVector3/value[in]
  48: /ForBegin/fst[out] -> /ForEnd/scope[in]
  49: /geo_SetPointAttribValueVector3/fst[out] -> /ForEnd/spare[0][in]
  50: /ForBegin/snd.snd[1][out] -> /ForEnd/spare[1][in]
  51: /ForBegin/snd.snd[2][out] -> /ForEnd/spare[2][in]
  52: /ForBegin/snd.snd[3][out] -> /ForEnd/spare[3][in]
  53: /ForBegin/snd.snd[4][out] -> /ForEnd/spare[4][in]
  54: /ForBegin/snd.snd[5][out] -> /ForEnd/spare[5][in]
  55: /ForBegin/snd.snd[6][out] -> /ForEnd/spare[6][in]

Literals:
  0: bool "false" -> /geo_BoundingBox/orient[in] ⏎
  1: bool "false" -> /geo_BoundingBox/local'[in] ⏎
  2: int 0 -> /value/parm[in] ⏎
  3: int 0 -> /value/parm[in] ⏎
  4: int 0 -> /value/parm[in] ⏎
  5: int 1 -> /value/parm[in] ⏎
  6: int 1 -> /value/parm[in] ⏎
  7: int 0 -> /value/parm[in] ⏎
  8: int 1 -> /value/parm[in] ⏎
  9: int 1 -> /value/parm[in] ⏎
  10: int 0 -> /AddInt1/a[in] ⏎
  11: int 0 -> /value/parm[in] ⏎
  12: str "P" -> /geo_PointAttribValueVector3/attribname[in] ⏎
  13: str "Cd" -> /geo_SetPointAttribValueVector3/attribname[in]
-/
#guard_msgs in
run_meta
  let e := q(loop1)
  let g ← programToApexGraph e
  IO.println g


abbrev loop2 := fun (geo : Geometry) => Id.run do
    let mut geo := geo
    let mut x : Int := 0
    let bbox := geo.boundingBox
    for i in [0:geo.numPoints.toNat] do
      let P : Vector3 := geo.pointAttrib (Int.ofNat i) "P"
      let relP := (P - bbox.min).compDiv bbox.size
      x := x + 1
      geo := geo.setPointAttrib (Int.ofNat i) "Cd" relP
    return geo


/--
info: Nodes:
  0: geo_BoundingBox : geo::BoundingBox
  1: geo_NumPoints : geo::NumPoints
  2: MaxInt : Max<Int>
  3: value : Value<Int>
  4: SubtractInt : Subtract<Int>
  5: value : Value<Int>
  6: MaxInt1 : Max<Int>
  7: value : Value<Int>
  8: AddInt : Add<Int>
  9: value : Value<Int>
  10: SubtractInt1 : Subtract<Int>
  11: value : Value<Int>
  12: MaxInt2 : Max<Int>
  13: value : Value<Int>
  14: DivideInt : Divide<Int>
  15: value : Value<Int>
  16: ForBegin : ForBegin
  17: value : Value<Int>
  18: MultiplyInt : Multiply<Int>
  19: value : Value<Int>
  20: AddInt1 : Add<Int>
  21: MaxInt3 : Max<Int>
  22: value : Value<Int>
  23: geo_PointAttribValueVector3 : geo::PointAttribValue<Vector3>
  24: Vector3ToFloat : Vector3ToFloat
  25: Vector3ToFloat1 : Vector3ToFloat
  26: SubtractFloat : Subtract<Float>
  27: Vector3ToFloat2 : Vector3ToFloat
  28: DivideFloat : Divide<Float>
  29: SubtractFloat1 : Subtract<Float>
  30: DivideFloat1 : Divide<Float>
  31: SubtractFloat2 : Subtract<Float>
  32: DivideFloat2 : Divide<Float>
  33: FloatToVector3 : FloatToVector3
  34: AddInt2 : Add<Int>
  35: value : Value<Int>
  36: geo_SetPointAttribValueVector3 : geo::SetPointAttribValue<Vector3>
  37: ForEnd : ForEnd

Ports:
  0: /geo_BoundingBox/geo[in]
  1: /geo_BoundingBox/orient[in]
  2: /geo_BoundingBox/local'[in]
  3: /geo_BoundingBox/fst[out]
  4: /geo_BoundingBox/snd.fst[out]
  5: /geo_BoundingBox/snd.snd.fst[out]
  6: /geo_BoundingBox/snd.snd.snd.fst[out]
  7: /geo_BoundingBox/snd.snd.snd.snd[out]
  8: /geo_NumPoints/geo[in]
  9: /geo_NumPoints/[anonymous][out]
  10: /MaxInt/a[in]
  11: /MaxInt/b[⋯][in]
  12: /MaxInt/[anonymous][out]
  13: /value/parm[in]
  14: /value/value[out]
  15: /SubtractInt/a[in]
  16: /SubtractInt/b[⋯][in]
  17: /SubtractInt/[anonymous][out]
  18: /value/parm[in]
  19: /value/value[out]
  20: /MaxInt1/a[in]
  21: /MaxInt1/b[⋯][in]
  22: /MaxInt1/[anonymous][out]
  23: /value/parm[in]
  24: /value/value[out]
  25: /AddInt/a[in]
  26: /AddInt/b[⋯][in]
  27: /AddInt/[anonymous][out]
  28: /value/parm[in]
  29: /value/value[out]
  30: /SubtractInt1/a[in]
  31: /SubtractInt1/b[⋯][in]
  32: /SubtractInt1/[anonymous][out]
  33: /value/parm[in]
  34: /value/value[out]
  35: /MaxInt2/a[in]
  36: /MaxInt2/b[⋯][in]
  37: /MaxInt2/[anonymous][out]
  38: /value/parm[in]
  39: /value/value[out]
  40: /DivideInt/a[in]
  41: /DivideInt/b[⋯][in]
  42: /DivideInt/[anonymous][out]
  43: /value/parm[in]
  44: /value/value[out]
  45: /ForBegin/[anonymous][out]
  46: /ForBegin/iterations[in]
  47: /ForBegin/spare[⋯][in]
  48: /ForBegin/fst[out]
  49: /ForBegin/snd.fst[out]
  50: /ForBegin/snd.snd[⋯][out]
  51: /value/parm[in]
  52: /value/value[out]
  53: /MultiplyInt/a[in]
  54: /MultiplyInt/b[⋯][in]
  55: /MultiplyInt/[anonymous][out]
  56: /value/parm[in]
  57: /value/value[out]
  58: /AddInt1/a[in]
  59: /AddInt1/b[⋯][in]
  60: /AddInt1/[anonymous][out]
  61: /MaxInt3/a[in]
  62: /MaxInt3/b[⋯][in]
  63: /MaxInt3/[anonymous][out]
  64: /value/parm[in]
  65: /value/value[out]
  66: /geo_PointAttribValueVector3/[anonymous][out]
  67: /geo_PointAttribValueVector3/geo[in]
  68: /geo_PointAttribValueVector3/elemnum[in]
  69: /geo_PointAttribValueVector3/attribname[in]
  70: /geo_PointAttribValueVector3/fst[out]
  71: /geo_PointAttribValueVector3/snd[out]
  72: /Vector3ToFloat/vector[in]
  73: /Vector3ToFloat/fst[out]
  74: /Vector3ToFloat/snd.fst[out]
  75: /Vector3ToFloat/snd.snd[out]
  76: /Vector3ToFloat1/vector[in]
  77: /Vector3ToFloat1/fst[out]
  78: /Vector3ToFloat1/snd.fst[out]
  79: /Vector3ToFloat1/snd.snd[out]
  80: /SubtractFloat/a[in]
  81: /SubtractFloat/b[⋯][in]
  82: /SubtractFloat/[anonymous][out]
  83: /Vector3ToFloat2/vector[in]
  84: /Vector3ToFloat2/fst[out]
  85: /Vector3ToFloat2/snd.fst[out]
  86: /Vector3ToFloat2/snd.snd[out]
  87: /DivideFloat/a[in]
  88: /DivideFloat/b[⋯][in]
  89: /DivideFloat/[anonymous][out]
  90: /SubtractFloat1/a[in]
  91: /SubtractFloat1/b[⋯][in]
  92: /SubtractFloat1/[anonymous][out]
  93: /DivideFloat1/a[in]
  94: /DivideFloat1/b[⋯][in]
  95: /DivideFloat1/[anonymous][out]
  96: /SubtractFloat2/a[in]
  97: /SubtractFloat2/b[⋯][in]
  98: /SubtractFloat2/[anonymous][out]
  99: /DivideFloat2/a[in]
  100: /DivideFloat2/b[⋯][in]
  101: /DivideFloat2/[anonymous][out]
  102: /FloatToVector3/x[in]
  103: /FloatToVector3/y[in]
  104: /FloatToVector3/z[in]
  105: /FloatToVector3/[anonymous][out]
  106: /AddInt2/a[in]
  107: /AddInt2/b[⋯][in]
  108: /AddInt2/[anonymous][out]
  109: /value/parm[in]
  110: /value/value[out]
  111: /geo_SetPointAttribValueVector3/[anonymous][out]
  112: /geo_SetPointAttribValueVector3/geo[in]
  113: /geo_SetPointAttribValueVector3/elemnum[in]
  114: /geo_SetPointAttribValueVector3/attribname[in]
  115: /geo_SetPointAttribValueVector3/value[in]
  116: /geo_SetPointAttribValueVector3/fst[out]
  117: /geo_SetPointAttribValueVector3/snd[out]
  118: /ForEnd/[anonymous][out]
  119: /ForEnd/scope[in]
  120: /ForEnd/spare[⋯][in]
  121: /ForEnd/[anonymous][⋯][out]

Inputs:
  geo[in] -> #[/geo_BoundingBox/geo[in], /geo_NumPoints/geo[in], /ForBegin/spare[0][in], /ForBegin/spare[2][in]]

Outputs:
  /ForEnd/[anonymous][0][out] -> «0»[out]

Wires:
  0: /geo_NumPoints/[anonymous][out] -> /MaxInt/a[in]
  1: /value/value[out] -> /MaxInt/b[0][in]
  2: /MaxInt/[anonymous][out] -> /SubtractInt/a[in]
  3: /value/value[out] -> /SubtractInt/b[0][in]
  4: /SubtractInt/[anonymous][out] -> /MaxInt1/a[in]
  5: /value/value[out] -> /MaxInt1/b[0][in]
  6: /MaxInt1/[anonymous][out] -> /AddInt/a[in]
  7: /value/value[out] -> /AddInt/b[0][in]
  8: /AddInt/[anonymous][out] -> /SubtractInt1/a[in]
  9: /value/value[out] -> /SubtractInt1/b[0][in]
  10: /SubtractInt1/[anonymous][out] -> /MaxInt2/a[in]
  11: /value/value[out] -> /MaxInt2/b[0][in]
  12: /MaxInt2/[anonymous][out] -> /DivideInt/a[in]
  13: /value/value[out] -> /DivideInt/b[0][in]
  14: /DivideInt/[anonymous][out] -> /ForBegin/iterations[in]
  15: /value/value[out] -> /ForBegin/spare[1][in]
  16: /geo_BoundingBox/fst[out] -> /ForBegin/spare[3][in]
  17: /geo_BoundingBox/snd.fst[out] -> /ForBegin/spare[4][in]
  18: /geo_BoundingBox/snd.snd.fst[out] -> /ForBegin/spare[5][in]
  19: /geo_BoundingBox/snd.snd.snd.fst[out] -> /ForBegin/spare[6][in]
  20: /geo_BoundingBox/snd.snd.snd.snd[out] -> /ForBegin/spare[7][in]
  21: /ForBegin/snd.fst[out] -> /MultiplyInt/a[in]
  22: /value/value[out] -> /MultiplyInt/b[0][in]
  23: /MultiplyInt/[anonymous][out] -> /AddInt1/b[0][in]
  24: /AddInt1/[anonymous][out] -> /MaxInt3/a[in]
  25: /value/value[out] -> /MaxInt3/b[0][in]
  26: /ForBegin/snd.snd[0][out] -> /geo_PointAttribValueVector3/geo[in]
  27: /MaxInt3/[anonymous][out] -> /geo_PointAttribValueVector3/elemnum[in]
  28: /geo_PointAttribValueVector3/fst[out] -> /Vector3ToFloat/vector[in]
  29: /ForBegin/snd.snd[5][out] -> /Vector3ToFloat1/vector[in]
  30: /Vector3ToFloat/fst[out] -> /SubtractFloat/a[in]
  31: /Vector3ToFloat1/fst[out] -> /SubtractFloat/b[0][in]
  32: /ForBegin/snd.snd[4][out] -> /Vector3ToFloat2/vector[in]
  33: /SubtractFloat/[anonymous][out] -> /DivideFloat/a[in]
  34: /Vector3ToFloat2/fst[out] -> /DivideFloat/b[0][in]
  35: /Vector3ToFloat/snd.fst[out] -> /SubtractFloat1/a[in]
  36: /Vector3ToFloat1/snd.fst[out] -> /SubtractFloat1/b[0][in]
  37: /SubtractFloat1/[anonymous][out] -> /DivideFloat1/a[in]
  38: /Vector3ToFloat2/snd.fst[out] -> /DivideFloat1/b[0][in]
  39: /Vector3ToFloat/snd.snd[out] -> /SubtractFloat2/a[in]
  40: /Vector3ToFloat1/snd.snd[out] -> /SubtractFloat2/b[0][in]
  41: /SubtractFloat2/[anonymous][out] -> /DivideFloat2/a[in]
  42: /Vector3ToFloat2/snd.snd[out] -> /DivideFloat2/b[0][in]
  43: /DivideFloat/[anonymous][out] -> /FloatToVector3/x[in]
  44: /DivideFloat1/[anonymous][out] -> /FloatToVector3/y[in]
  45: /DivideFloat2/[anonymous][out] -> /FloatToVector3/z[in]
  46: /ForBegin/snd.snd[1][out] -> /AddInt2/a[in]
  47: /value/value[out] -> /AddInt2/b[0][in]
  48: /ForBegin/snd.snd[0][out] -> /geo_SetPointAttribValueVector3/geo[in]
  49: /MaxInt3/[anonymous][out] -> /geo_SetPointAttribValueVector3/elemnum[in]
  50: /FloatToVector3/[anonymous][out] -> /geo_SetPointAttribValueVector3/value[in]
  51: /ForBegin/fst[out] -> /ForEnd/scope[in]
  52: /geo_SetPointAttribValueVector3/fst[out] -> /ForEnd/spare[0][in]
  53: /AddInt2/[anonymous][out] -> /ForEnd/spare[1][in]
  54: /ForBegin/snd.snd[2][out] -> /ForEnd/spare[2][in]
  55: /ForBegin/snd.snd[3][out] -> /ForEnd/spare[3][in]
  56: /ForBegin/snd.snd[4][out] -> /ForEnd/spare[4][in]
  57: /ForBegin/snd.snd[5][out] -> /ForEnd/spare[5][in]
  58: /ForBegin/snd.snd[6][out] -> /ForEnd/spare[6][in]
  59: /ForBegin/snd.snd[7][out] -> /ForEnd/spare[7][in]

Literals:
  0: bool "false" -> /geo_BoundingBox/orient[in] ⏎
  1: bool "false" -> /geo_BoundingBox/local'[in] ⏎
  2: int 0 -> /value/parm[in] ⏎
  3: int 0 -> /value/parm[in] ⏎
  4: int 0 -> /value/parm[in] ⏎
  5: int 1 -> /value/parm[in] ⏎
  6: int 1 -> /value/parm[in] ⏎
  7: int 0 -> /value/parm[in] ⏎
  8: int 1 -> /value/parm[in] ⏎
  9: int 0 -> /value/parm[in] ⏎
  10: int 1 -> /value/parm[in] ⏎
  11: int 0 -> /AddInt1/a[in] ⏎
  12: int 0 -> /value/parm[in] ⏎
  13: str "P" -> /geo_PointAttribValueVector3/attribname[in] ⏎
  14: int 1 -> /value/parm[in] ⏎
  15: str "Cd" -> /geo_SetPointAttribValueVector3/attribname[in]
-/
#guard_msgs in
run_meta
  let e := q(loop2)
  let g ← programToApexGraph e
  IO.println g



/--
info: Nodes:
  0: geo_NumPoints : geo::NumPoints
  1: MaxInt : Max<Int>
  2: value : Value<Int>
  3: SubtractInt : Subtract<Int>
  4: value : Value<Int>
  5: MaxInt1 : Max<Int>
  6: value : Value<Int>
  7: AddInt : Add<Int>
  8: value : Value<Int>
  9: SubtractInt1 : Subtract<Int>
  10: value : Value<Int>
  11: MaxInt2 : Max<Int>
  12: value : Value<Int>
  13: DivideInt : Divide<Int>
  14: value : Value<Int>
  15: ForBegin : ForBegin
  16: MultiplyInt : Multiply<Int>
  17: value : Value<Int>
  18: AddInt1 : Add<Int>
  19: MaxInt3 : Max<Int>
  20: value : Value<Int>
  21: geo_PointAttribValueVector3 : geo::PointAttribValue<Vector3>
  22: Vector3ToFloat : Vector3ToFloat
  23: GreaterThanFloat : GreaterThan<Float>
  24: TwoWaySwitchInt : TwoWaySwitch<Int>
  25: EqualsInt : Equals<Int>
  26: IfBegin : IfBegin
  27: geo_SetPointAttribValueVector3 : geo::SetPointAttribValue<Vector3>
  28: geo_SetPointAttribValueInt : geo::SetPointAttribValue<Int>
  29: IfEnd : IfEnd
  30: TwoWaySwitchBool : TwoWaySwitch<Bool>
  31: IfBegin1 : IfBegin
  32: geo_SetPointAttribValueVector31 : geo::SetPointAttribValue<Vector3>
  33: geo_SetPointAttribValueInt1 : geo::SetPointAttribValue<Int>
  34: IfEnd1 : IfEnd
  35: ForEnd : ForEnd

Ports:
  0: /geo_NumPoints/geo[in]
  1: /geo_NumPoints/[anonymous][out]
  2: /MaxInt/a[in]
  3: /MaxInt/b[⋯][in]
  4: /MaxInt/[anonymous][out]
  5: /value/parm[in]
  6: /value/value[out]
  7: /SubtractInt/a[in]
  8: /SubtractInt/b[⋯][in]
  9: /SubtractInt/[anonymous][out]
  10: /value/parm[in]
  11: /value/value[out]
  12: /MaxInt1/a[in]
  13: /MaxInt1/b[⋯][in]
  14: /MaxInt1/[anonymous][out]
  15: /value/parm[in]
  16: /value/value[out]
  17: /AddInt/a[in]
  18: /AddInt/b[⋯][in]
  19: /AddInt/[anonymous][out]
  20: /value/parm[in]
  21: /value/value[out]
  22: /SubtractInt1/a[in]
  23: /SubtractInt1/b[⋯][in]
  24: /SubtractInt1/[anonymous][out]
  25: /value/parm[in]
  26: /value/value[out]
  27: /MaxInt2/a[in]
  28: /MaxInt2/b[⋯][in]
  29: /MaxInt2/[anonymous][out]
  30: /value/parm[in]
  31: /value/value[out]
  32: /DivideInt/a[in]
  33: /DivideInt/b[⋯][in]
  34: /DivideInt/[anonymous][out]
  35: /value/parm[in]
  36: /value/value[out]
  37: /ForBegin/[anonymous][out]
  38: /ForBegin/iterations[in]
  39: /ForBegin/spare[⋯][in]
  40: /ForBegin/fst[out]
  41: /ForBegin/snd.fst[out]
  42: /ForBegin/snd.snd[⋯][out]
  43: /MultiplyInt/a[in]
  44: /MultiplyInt/b[⋯][in]
  45: /MultiplyInt/[anonymous][out]
  46: /value/parm[in]
  47: /value/value[out]
  48: /AddInt1/a[in]
  49: /AddInt1/b[⋯][in]
  50: /AddInt1/[anonymous][out]
  51: /MaxInt3/a[in]
  52: /MaxInt3/b[⋯][in]
  53: /MaxInt3/[anonymous][out]
  54: /value/parm[in]
  55: /value/value[out]
  56: /geo_PointAttribValueVector3/[anonymous][out]
  57: /geo_PointAttribValueVector3/geo[in]
  58: /geo_PointAttribValueVector3/elemnum[in]
  59: /geo_PointAttribValueVector3/attribname[in]
  60: /geo_PointAttribValueVector3/fst[out]
  61: /geo_PointAttribValueVector3/snd[out]
  62: /Vector3ToFloat/vector[in]
  63: /Vector3ToFloat/fst[out]
  64: /Vector3ToFloat/snd.fst[out]
  65: /Vector3ToFloat/snd.snd[out]
  66: /GreaterThanFloat/a[in]
  67: /GreaterThanFloat/b[in]
  68: /GreaterThanFloat/[anonymous][out]
  69: /TwoWaySwitchInt/a[in]
  70: /TwoWaySwitchInt/b[in]
  71: /TwoWaySwitchInt/index[in]
  72: /TwoWaySwitchInt/[anonymous][out]
  73: /EqualsInt/a[in]
  74: /EqualsInt/b[in]
  75: /EqualsInt/[anonymous][out]
  76: /IfBegin/[anonymous][out]
  77: /IfBegin/condition[in]
  78: /IfBegin/spare[⋯][in]
  79: /IfBegin/fst[out]
  80: /IfBegin/snd[⋯][out]
  81: /geo_SetPointAttribValueVector3/[anonymous][out]
  82: /geo_SetPointAttribValueVector3/geo[in]
  83: /geo_SetPointAttribValueVector3/elemnum[in]
  84: /geo_SetPointAttribValueVector3/attribname[in]
  85: /geo_SetPointAttribValueVector3/value[in]
  86: /geo_SetPointAttribValueVector3/fst[out]
  87: /geo_SetPointAttribValueVector3/snd[out]
  88: /geo_SetPointAttribValueInt/[anonymous][out]
  89: /geo_SetPointAttribValueInt/geo[in]
  90: /geo_SetPointAttribValueInt/elemnum[in]
  91: /geo_SetPointAttribValueInt/attribname[in]
  92: /geo_SetPointAttribValueInt/value[in]
  93: /geo_SetPointAttribValueInt/fst[out]
  94: /geo_SetPointAttribValueInt/snd[out]
  95: /IfEnd/[anonymous][out]
  96: /IfEnd/scope[in]
  97: /IfEnd/spare[⋯][in]
  98: /IfEnd/[anonymous][⋯][out]
  99: /TwoWaySwitchBool/a[in]
  100: /TwoWaySwitchBool/b[in]
  101: /TwoWaySwitchBool/index[in]
  102: /TwoWaySwitchBool/[anonymous][out]
  103: /IfBegin1/[anonymous][out]
  104: /IfBegin1/condition[in]
  105: /IfBegin1/spare[⋯][in]
  106: /IfBegin1/fst[out]
  107: /IfBegin1/snd[⋯][out]
  108: /geo_SetPointAttribValueVector31/[anonymous][out]
  109: /geo_SetPointAttribValueVector31/geo[in]
  110: /geo_SetPointAttribValueVector31/elemnum[in]
  111: /geo_SetPointAttribValueVector31/attribname[in]
  112: /geo_SetPointAttribValueVector31/value[in]
  113: /geo_SetPointAttribValueVector31/fst[out]
  114: /geo_SetPointAttribValueVector31/snd[out]
  115: /geo_SetPointAttribValueInt1/[anonymous][out]
  116: /geo_SetPointAttribValueInt1/geo[in]
  117: /geo_SetPointAttribValueInt1/elemnum[in]
  118: /geo_SetPointAttribValueInt1/attribname[in]
  119: /geo_SetPointAttribValueInt1/value[in]
  120: /geo_SetPointAttribValueInt1/fst[out]
  121: /geo_SetPointAttribValueInt1/snd[out]
  122: /IfEnd1/[anonymous][out]
  123: /IfEnd1/scope[in]
  124: /IfEnd1/spare[⋯][in]
  125: /IfEnd1/[anonymous][⋯][out]
  126: /ForEnd/[anonymous][out]
  127: /ForEnd/scope[in]
  128: /ForEnd/spare[⋯][in]
  129: /ForEnd/[anonymous][⋯][out]

Inputs:
  geo[in] -> #[/geo_NumPoints/geo[in], /ForBegin/spare[0][in], /ForBegin/spare[1][in]]

Outputs:
  /ForEnd/[anonymous][0][out] -> «0»[out]

Wires:
  0: /geo_NumPoints/[anonymous][out] -> /MaxInt/a[in]
  1: /value/value[out] -> /MaxInt/b[0][in]
  2: /MaxInt/[anonymous][out] -> /SubtractInt/a[in]
  3: /value/value[out] -> /SubtractInt/b[0][in]
  4: /SubtractInt/[anonymous][out] -> /MaxInt1/a[in]
  5: /value/value[out] -> /MaxInt1/b[0][in]
  6: /MaxInt1/[anonymous][out] -> /AddInt/a[in]
  7: /value/value[out] -> /AddInt/b[0][in]
  8: /AddInt/[anonymous][out] -> /SubtractInt1/a[in]
  9: /value/value[out] -> /SubtractInt1/b[0][in]
  10: /SubtractInt1/[anonymous][out] -> /MaxInt2/a[in]
  11: /value/value[out] -> /MaxInt2/b[0][in]
  12: /MaxInt2/[anonymous][out] -> /DivideInt/a[in]
  13: /value/value[out] -> /DivideInt/b[0][in]
  14: /DivideInt/[anonymous][out] -> /ForBegin/iterations[in]
  15: /ForBegin/snd.fst[out] -> /MultiplyInt/a[in]
  16: /value/value[out] -> /MultiplyInt/b[0][in]
  17: /MultiplyInt/[anonymous][out] -> /AddInt1/b[0][in]
  18: /AddInt1/[anonymous][out] -> /MaxInt3/a[in]
  19: /value/value[out] -> /MaxInt3/b[0][in]
  20: /ForBegin/snd.snd[0][out] -> /geo_PointAttribValueVector3/geo[in]
  21: /MaxInt3/[anonymous][out] -> /geo_PointAttribValueVector3/elemnum[in]
  22: /geo_PointAttribValueVector3/fst[out] -> /Vector3ToFloat/vector[in]
  23: /Vector3ToFloat/fst[out] -> /GreaterThanFloat/a[in]
  24: /GreaterThanFloat/[anonymous][out] -> /TwoWaySwitchInt/index[in]
  25: /TwoWaySwitchInt/[anonymous][out] -> /EqualsInt/a[in]
  26: /EqualsInt/[anonymous][out] -> /IfBegin/condition[in]
  27: /ForBegin/snd.snd[0][out] -> /IfBegin/spare[1][in]
  28: /MaxInt3/[anonymous][out] -> /IfBegin/spare[2][in]
  29: /IfBegin/snd[1][out] -> /geo_SetPointAttribValueVector3/geo[in]
  30: /IfBegin/snd[2][out] -> /geo_SetPointAttribValueVector3/elemnum[in]
  31: /geo_SetPointAttribValueVector3/fst[out] -> /geo_SetPointAttribValueInt/geo[in]
  32: /IfBegin/snd[2][out] -> /geo_SetPointAttribValueInt/elemnum[in]
  33: /IfBegin/fst[out] -> /IfEnd/scope[in]
  34: /geo_SetPointAttribValueInt/fst[out] -> /IfEnd/spare[0][in]
  35: /IfBegin/snd[1][out] -> /IfEnd/spare[1][in]
  36: /IfBegin/snd[2][out] -> /IfEnd/spare[2][in]
  37: /EqualsInt/[anonymous][out] -> /TwoWaySwitchBool/index[in]
  38: /TwoWaySwitchBool/[anonymous][out] -> /IfBegin1/condition[in]
  39: /IfEnd/[anonymous][0][out] -> /IfBegin1/spare[0][in]
  40: /IfEnd/[anonymous][1][out] -> /IfBegin1/spare[1][in]
  41: /IfEnd/[anonymous][2][out] -> /IfBegin1/spare[2][in]
  42: /IfBegin1/snd[1][out] -> /geo_SetPointAttribValueVector31/geo[in]
  43: /IfBegin1/snd[2][out] -> /geo_SetPointAttribValueVector31/elemnum[in]
  44: /geo_SetPointAttribValueVector31/fst[out] -> /geo_SetPointAttribValueInt1/geo[in]
  45: /IfBegin1/snd[2][out] -> /geo_SetPointAttribValueInt1/elemnum[in]
  46: /IfBegin1/fst[out] -> /IfEnd1/scope[in]
  47: /geo_SetPointAttribValueInt1/fst[out] -> /IfEnd1/spare[0][in]
  48: /IfBegin1/snd[1][out] -> /IfEnd1/spare[1][in]
  49: /IfBegin1/snd[2][out] -> /IfEnd1/spare[2][in]
  50: /ForBegin/fst[out] -> /ForEnd/scope[in]
  51: /IfEnd1/[anonymous][0][out] -> /ForEnd/spare[0][in]
  52: /ForBegin/snd.snd[1][out] -> /ForEnd/spare[1][in]

Literals:
  0: int 0 -> /value/parm[in] ⏎
  1: int 0 -> /value/parm[in] ⏎
  2: int 0 -> /value/parm[in] ⏎
  3: int 1 -> /value/parm[in] ⏎
  4: int 1 -> /value/parm[in] ⏎
  5: int 0 -> /value/parm[in] ⏎
  6: int 1 -> /value/parm[in] ⏎
  7: int 1 -> /value/parm[in] ⏎
  8: int 0 -> /AddInt1/a[in] ⏎
  9: int 0 -> /value/parm[in] ⏎
  10: str "P" -> /geo_PointAttribValueVector3/attribname[in] ⏎
  11: float 0.0 -> /GreaterThanFloat/b[in] ⏎
  12: int 0 -> /TwoWaySwitchInt/a[in] ⏎
  13: int 1 -> /TwoWaySwitchInt/b[in] ⏎
  14: int 1 -> /EqualsInt/b[in] ⏎
  15: str "Cd" -> /geo_SetPointAttribValueVector3/attribname[in] ⏎
  16: vector3 "{ x := 1.000000, y := 0.000000, z := 0.000000 }" -> /geo_SetPointAttribValueVector3/value[in] ⏎
  17: str "mygroup" -> /geo_SetPointAttribValueInt/attribname[in] ⏎
  18: int 1 -> /geo_SetPointAttribValueInt/value[in] ⏎
  19: bool "true" -> /TwoWaySwitchBool/a[in] ⏎
  20: bool "false" -> /TwoWaySwitchBool/b[in] ⏎
  21: str "Cd" -> /geo_SetPointAttribValueVector31/attribname[in] ⏎
  22: vector3 "{ x := 0.000000, y := 0.000000, z := 0.000000 }" -> /geo_SetPointAttribValueVector31/value[in] ⏎
  23: str "mygroup" -> /geo_SetPointAttribValueInt1/attribname[in] ⏎
  24: int 0 -> /geo_SetPointAttribValueInt1/value[in]
-/
#guard_msgs in
#apex_graph fun (geo : Geometry) => Id.run do
  let mut geo := geo
  for i in [0:geo.numPoints.toNat] do
    let P : Vector3 := geo.pointAttrib (Int.ofNat i) "P"
    let condition := (decide (P.x > 0)).toInt

    if condition = 1 then
      geo := geo.setPointAttrib (Int.ofNat i) "Cd" (Vector3.mk 1 0 0)
      geo := geo.setPointAttrib (Int.ofNat i) "mygroup" (1:Int)
    else
      geo := geo.setPointAttrib (Int.ofNat i) "Cd" (Vector3.mk 0 0 0)
      geo := geo.setPointAttrib (Int.ofNat i) "mygroup" (0:Int)
  return geo
