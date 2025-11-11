import HouLean

open HouLean Apex Compiler


set_option trace.HouLean.Apex.compiler true in
#apex_graph fun (x : Int) => Id.run do
  let mut x : Int := x
  for _ in [0:10] do
    x := x + x
  return x

set_option synthInstance.maxSize 5000


set_option trace.HouLean.Apex.compiler true in
@[apex]
def runGeo (geo : Geometry) : Geometry := Id.run do
  let mut geo := geo  
  let (_,size,min,_,_) := geo.boundingBox
  for i in [0:geo.numPoints.toNat] do
    let P : Vector3 := geo.pointAttrib (Int.ofNat i) "P"
    let relP := (P - min).compDiv size
    geo := geo.setPointAttrib (Int.ofNat i) "Cd" relP
  return geo


  

@[apex]
def run (geo : Geometry) : Geometry := Id.run do
  let mut geo := geo  
  let r := geo.boundingBox
  let size := r.2.1
  let min := r.2.2.1
  for i in [0:geo.numPoints.toNat] do
    let P : Vector3 := geo.pointAttrib (Int.ofNat i) "P"
    let relP := (P - min).compDiv size
    geo := geo.setPointAttrib (Int.ofNat i) "Cd" relP
  return geo


abbrev loop1 := fun (geo : Geometry) => Id.run do
    let mut geo := geo  
    let (_,size,min,_,_) := geo.boundingBox
    for i in [0:geo.numPoints.toNat] do
      let P : Vector3 := geo.pointAttrib (Int.ofNat i) "P"
      let relP := (P - min).compDiv size
      geo := geo.setPointAttrib (Int.ofNat i) "Cd" relP
    return geo

set_option trace.HouLean.Apex.compiler true in
@[apex]
def loop_set_attrib (geo : Geometry) : Geometry := Id.run do
  let mut geo := geo  
  for i in [0:4] do
    if (Int.ofNat i) = 0 then
      geo := geo.setPointAttrib (Int.ofNat i) "Cd" (Vector3.mk 1 0 0)
    else
      geo := geo.setPointAttrib (Int.ofNat i) "Cd" (Vector3.mk 0 0 0)
    geo := geo.subdivide
  return geo

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
  geo[in] -> #[/geo_BoundingBox/geo[in], /geo_NumPoints/geo[in], /ForBegin/spare[0][in]]

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
  15: /geo_BoundingBox/snd.snd.fst[out] -> /ForBegin/spare[1][in]
  16: /geo_BoundingBox/snd.snd.snd.fst[out] -> /ForBegin/spare[2][in]
  17: /geo_BoundingBox/snd.snd.snd.snd[out] -> /ForBegin/spare[3][in]
  18: /geo_BoundingBox/snd.fst[out] -> /ForBegin/spare[4][in]
  19: /geo_BoundingBox/snd.snd.fst[out] -> /ForBegin/spare[5][in]
  20: /geo_BoundingBox/snd.snd.snd.fst[out] -> /ForBegin/spare[6][in]
  21: /geo_BoundingBox/snd.snd.snd.snd[out] -> /ForBegin/spare[7][in]
  22: /ForBegin/snd.fst[out] -> /MultiplyInt/a[in]
  23: /value/value[out] -> /MultiplyInt/b[0][in]
  24: /MultiplyInt/[anonymous][out] -> /AddInt1/b[0][in]
  25: /AddInt1/[anonymous][out] -> /MaxInt3/a[in]
  26: /value/value[out] -> /MaxInt3/b[0][in]
  27: /ForBegin/snd.snd[0][out] -> /geo_PointAttribValueVector3/geo[in]
  28: /MaxInt3/[anonymous][out] -> /geo_PointAttribValueVector3/elemnum[in]
  29: /geo_PointAttribValueVector3/fst[out] -> /Vector3ToFloat/vector[in]
  30: /ForBegin/snd.snd[1][out] -> /Vector3ToFloat1/vector[in]
  31: /Vector3ToFloat/fst[out] -> /SubtractFloat/a[in]
  32: /Vector3ToFloat1/fst[out] -> /SubtractFloat/b[0][in]
  33: /ForBegin/snd.snd[4][out] -> /Vector3ToFloat2/vector[in]
  34: /SubtractFloat/[anonymous][out] -> /DivideFloat/a[in]
  35: /Vector3ToFloat2/fst[out] -> /DivideFloat/b[0][in]
  36: /Vector3ToFloat/snd.fst[out] -> /SubtractFloat1/a[in]
  37: /Vector3ToFloat1/snd.fst[out] -> /SubtractFloat1/b[0][in]
  38: /SubtractFloat1/[anonymous][out] -> /DivideFloat1/a[in]
  39: /Vector3ToFloat2/snd.fst[out] -> /DivideFloat1/b[0][in]
  40: /Vector3ToFloat/snd.snd[out] -> /SubtractFloat2/a[in]
  41: /Vector3ToFloat1/snd.snd[out] -> /SubtractFloat2/b[0][in]
  42: /SubtractFloat2/[anonymous][out] -> /DivideFloat2/a[in]
  43: /Vector3ToFloat2/snd.snd[out] -> /DivideFloat2/b[0][in]
  44: /DivideFloat/[anonymous][out] -> /FloatToVector3/x[in]
  45: /DivideFloat1/[anonymous][out] -> /FloatToVector3/y[in]
  46: /DivideFloat2/[anonymous][out] -> /FloatToVector3/z[in]
  47: /ForBegin/snd.snd[0][out] -> /geo_SetPointAttribValueVector3/geo[in]
  48: /MaxInt3/[anonymous][out] -> /geo_SetPointAttribValueVector3/elemnum[in]
  49: /FloatToVector3/[anonymous][out] -> /geo_SetPointAttribValueVector3/value[in]
  50: /ForBegin/fst[out] -> /ForEnd/scope[in]
  51: /geo_SetPointAttribValueVector3/fst[out] -> /ForEnd/spare[0][in]
  52: /ForBegin/snd.snd[1][out] -> /ForEnd/spare[1][in]
  53: /ForBegin/snd.snd[2][out] -> /ForEnd/spare[2][in]
  54: /ForBegin/snd.snd[3][out] -> /ForEnd/spare[3][in]
  55: /ForBegin/snd.snd[4][out] -> /ForEnd/spare[4][in]
  56: /ForBegin/snd.snd[5][out] -> /ForEnd/spare[5][in]
  57: /ForBegin/snd.snd[6][out] -> /ForEnd/spare[6][in]
  58: /ForBegin/snd.snd[7][out] -> /ForEnd/spare[7][in]

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
    let (_,size,min,_,_) := geo.boundingBox
    for i in [0:geo.numPoints.toNat] do
      let P : Vector3 := geo.pointAttrib (Int.ofNat i) "P"
      let relP := (P - min).compDiv size
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
  geo[in] -> #[/geo_BoundingBox/geo[in], /geo_NumPoints/geo[in], /ForBegin/spare[0][in]]

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
  16: /geo_BoundingBox/snd.snd.fst[out] -> /ForBegin/spare[2][in]
  17: /geo_BoundingBox/snd.snd.snd.fst[out] -> /ForBegin/spare[3][in]
  18: /geo_BoundingBox/snd.snd.snd.snd[out] -> /ForBegin/spare[4][in]
  19: /geo_BoundingBox/snd.fst[out] -> /ForBegin/spare[5][in]
  20: /geo_BoundingBox/snd.snd.fst[out] -> /ForBegin/spare[6][in]
  21: /geo_BoundingBox/snd.snd.snd.fst[out] -> /ForBegin/spare[7][in]
  22: /geo_BoundingBox/snd.snd.snd.snd[out] -> /ForBegin/spare[8][in]
  23: /ForBegin/snd.fst[out] -> /MultiplyInt/a[in]
  24: /value/value[out] -> /MultiplyInt/b[0][in]
  25: /MultiplyInt/[anonymous][out] -> /AddInt1/b[0][in]
  26: /AddInt1/[anonymous][out] -> /MaxInt3/a[in]
  27: /value/value[out] -> /MaxInt3/b[0][in]
  28: /ForBegin/snd.snd[0][out] -> /geo_PointAttribValueVector3/geo[in]
  29: /MaxInt3/[anonymous][out] -> /geo_PointAttribValueVector3/elemnum[in]
  30: /geo_PointAttribValueVector3/fst[out] -> /Vector3ToFloat/vector[in]
  31: /ForBegin/snd.snd[2][out] -> /Vector3ToFloat1/vector[in]
  32: /Vector3ToFloat/fst[out] -> /SubtractFloat/a[in]
  33: /Vector3ToFloat1/fst[out] -> /SubtractFloat/b[0][in]
  34: /ForBegin/snd.snd[5][out] -> /Vector3ToFloat2/vector[in]
  35: /SubtractFloat/[anonymous][out] -> /DivideFloat/a[in]
  36: /Vector3ToFloat2/fst[out] -> /DivideFloat/b[0][in]
  37: /Vector3ToFloat/snd.fst[out] -> /SubtractFloat1/a[in]
  38: /Vector3ToFloat1/snd.fst[out] -> /SubtractFloat1/b[0][in]
  39: /SubtractFloat1/[anonymous][out] -> /DivideFloat1/a[in]
  40: /Vector3ToFloat2/snd.fst[out] -> /DivideFloat1/b[0][in]
  41: /Vector3ToFloat/snd.snd[out] -> /SubtractFloat2/a[in]
  42: /Vector3ToFloat1/snd.snd[out] -> /SubtractFloat2/b[0][in]
  43: /SubtractFloat2/[anonymous][out] -> /DivideFloat2/a[in]
  44: /Vector3ToFloat2/snd.snd[out] -> /DivideFloat2/b[0][in]
  45: /DivideFloat/[anonymous][out] -> /FloatToVector3/x[in]
  46: /DivideFloat1/[anonymous][out] -> /FloatToVector3/y[in]
  47: /DivideFloat2/[anonymous][out] -> /FloatToVector3/z[in]
  48: /ForBegin/snd.snd[1][out] -> /AddInt2/a[in]
  49: /value/value[out] -> /AddInt2/b[0][in]
  50: /ForBegin/snd.snd[0][out] -> /geo_SetPointAttribValueVector3/geo[in]
  51: /MaxInt3/[anonymous][out] -> /geo_SetPointAttribValueVector3/elemnum[in]
  52: /FloatToVector3/[anonymous][out] -> /geo_SetPointAttribValueVector3/value[in]
  53: /ForBegin/fst[out] -> /ForEnd/scope[in]
  54: /geo_SetPointAttribValueVector3/fst[out] -> /ForEnd/spare[0][in]
  55: /AddInt2/[anonymous][out] -> /ForEnd/spare[1][in]
  56: /ForBegin/snd.snd[2][out] -> /ForEnd/spare[2][in]
  57: /ForBegin/snd.snd[3][out] -> /ForEnd/spare[3][in]
  58: /ForBegin/snd.snd[4][out] -> /ForEnd/spare[4][in]
  59: /ForBegin/snd.snd[5][out] -> /ForEnd/spare[5][in]
  60: /ForBegin/snd.snd[6][out] -> /ForEnd/spare[6][in]
  61: /ForBegin/snd.snd[7][out] -> /ForEnd/spare[7][in]
  62: /ForBegin/snd.snd[8][out] -> /ForEnd/spare[8][in]

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

