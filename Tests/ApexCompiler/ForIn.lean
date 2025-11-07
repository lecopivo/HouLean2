import HouLean

open HouLean Apex Compiler

run_meta compilerExt.add (.implementedByName ``apexFlatten ``id' #[none, some 3])
run_meta compilerExt.add (.implementedByName ``apexUnflatten! ``id' #[none, some 3])

instance [ApexType α A] : ApexType (Id α) A where
  toApex x := toApex (α:=α) x
  fromApex x := fromApex (α:=α) x
e
set_option trace.HouLean.Apex.compiler true in
#apex_graph fun (x : Int) => Id.run do
  let mut x : Int := x
  for _ in [0:5] do
    x := x + x
  return x


#exit


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

/--
info: Nodes:
  0: geo : __null__
  1: value_bool : Value<Bool>
  2: geo::boundingbox : geo::BoundingBox
  3: geo::numpoints : geo::NumPoints
  4: value_int : Value<Int>
  5: a : __null__
  6: x : __null__
  7: r : __null__
  8: value_string : Value<String>
  9: geo::pointattribvalue_vector3 : geo::PointAttribValue<Vector3>
  10: subtract_vector3 : Subtract<Vector3>
  11: vector3tofloat : Vector3ToFloat
  12: vector3tofloat1 : Vector3ToFloat
  13: divide_float : Divide<Float>
  14: divide_float1 : Divide<Float>
  15: divide_float2 : Divide<Float>
  16: floattovector3 : FloatToVector3
  17: value_string1 : Value<String>
  18: geo::setpointattribvalue_vector3 : geo::SetPointAttribValue<Vector3>
  19: forbegin : ForBegin
  20: forend : ForEnd

Ports:
  0: /geo/__spare__[in]
  1: /geo/__spare__[out]
  2: /geo/x[in]
  3: /geo/x[out]
  4: /value_bool/parm[in]
  5: /value_bool/value[out]
  6: /geo::boundingbox/geo0[in]
  7: /geo::boundingbox/orient1[in]
  8: /geo::boundingbox/local'2[in]
  9: /geo::boundingbox/out0[out]
  10: /geo::boundingbox/out1[out]
  11: /geo::boundingbox/out2[out]
  12: /geo::boundingbox/out3[out]
  13: /geo::boundingbox/out4[out]
  14: /geo::numpoints/geo0[in]
  15: /geo::numpoints/out0[out]
  16: /value_int/a0[in]
  17: /value_int/out0[out]
  18: /a/__spare__[in]
  19: /a/__spare__[out]
  20: /a/x[in]
  21: /a/x[out]
  22: /x/__spare__[in]
  23: /x/__spare__[out]
  24: /r/__spare__[in]
  25: /r/__spare__[out]
  26: /r/x[in]
  27: /r/x[out]
  28: /value_string/parm[in]
  29: /value_string/value[out]
  30: /geo::pointattribvalue_vector3/rundata[out]
  31: /geo::pointattribvalue_vector3/geo0[in]
  32: /geo::pointattribvalue_vector3/elemnum1[in]
  33: /geo::pointattribvalue_vector3/attribname2[in]
  34: /geo::pointattribvalue_vector3/out0[out]
  35: /geo::pointattribvalue_vector3/out1[out]
  36: /subtract_vector3/a0[in]
  37: /subtract_vector3/b1[in]
  38: /subtract_vector3/out0[out]
  39: /subtract_vector3/out2[in]
  40: /vector3tofloat/vector0[in]
  41: /vector3tofloat/out0[out]
  42: /vector3tofloat/out1[out]
  43: /vector3tofloat/out2[out]
  44: /vector3tofloat1/vector0[in]
  45: /vector3tofloat1/out0[out]
  46: /vector3tofloat1/out1[out]
  47: /vector3tofloat1/out2[out]
  48: /divide_float/a0[in]
  49: /divide_float/b1[in]
  50: /divide_float/out0[out]
  51: /divide_float/out0[in]
  52: /divide_float1/a0[in]
  53: /divide_float1/b1[in]
  54: /divide_float1/out0[out]
  55: /divide_float1/out1[in]
  56: /divide_float2/a0[in]
  57: /divide_float2/b1[in]
  58: /divide_float2/out0[out]
  59: /divide_float2/out2[in]
  60: /floattovector3/x0[in]
  61: /floattovector3/y1[in]
  62: /floattovector3/z2[in]
  63: /floattovector3/out0[out]
  64: /value_string1/parm[in]
  65: /value_string1/value[out]
  66: /geo::setpointattribvalue_vector3/rundata[out]
  67: /geo::setpointattribvalue_vector3/geo0[in]
  68: /geo::setpointattribvalue_vector3/elemnum1[in]
  69: /geo::setpointattribvalue_vector3/attribname2[in]
  70: /geo::setpointattribvalue_vector3/value3[in]
  71: /geo::setpointattribvalue_vector3/out0[out]
  72: /geo::setpointattribvalue_vector3/out1[out]
  73: /forbegin/rundata[out]
  74: /forbegin/iterations[in]
  75: /forbegin/__spare__[in]
  76: /forbegin/scope[out]
  77: /forbegin/index[out]
  78: /forbegin/__spare__[out]
  79: /forbegin/x[in]
  80: /forbegin/x[out]
  81: /forend/rundata[out]
  82: /forend/scope[in]
  83: /forend/__spare__[in]
  84: /forend/__spare__[out]
  85: /forend/x[in]
  86: /forend/x[out]

Input Ports:
  2: /geo/x[in]

Output Ports:
  86: /forend/x[out]

Wires:
  0: /geo/x[out] -> /geo::boundingbox/geo0[in]
  1: /value_bool/value[out] -> /geo::boundingbox/orient1[in]
  2: /value_bool/value[out] -> /geo::boundingbox/local'2[in]
  3: /geo/x[out] -> /geo::numpoints/geo0[in]
  4: /geo::numpoints/out0[out] -> /value_int/a0[in]
  5: /r/x[out] -> /geo::pointattribvalue_vector3/geo0[in]
  6: /a/x[out] -> /geo::pointattribvalue_vector3/elemnum1[in]
  7: /value_string/value[out] -> /geo::pointattribvalue_vector3/attribname2[in]
  8: /geo::pointattribvalue_vector3/out0[out] -> /subtract_vector3/a0[in]
  9: /geo::boundingbox/out2[out] -> /subtract_vector3/out2[in]
  10: /subtract_vector3/out0[out] -> /vector3tofloat/vector0[in]
  11: /geo::boundingbox/out1[out] -> /vector3tofloat1/vector0[in]
  12: /vector3tofloat/out0[out] -> /divide_float/a0[in]
  13: /vector3tofloat1/out0[out] -> /divide_float/out0[in]
  14: /vector3tofloat/out1[out] -> /divide_float1/a0[in]
  15: /vector3tofloat1/out1[out] -> /divide_float1/out1[in]
  16: /vector3tofloat/out2[out] -> /divide_float2/a0[in]
  17: /vector3tofloat1/out2[out] -> /divide_float2/out2[in]
  18: /divide_float/out0[out] -> /floattovector3/x0[in]
  19: /divide_float1/out0[out] -> /floattovector3/y1[in]
  20: /divide_float2/out0[out] -> /floattovector3/z2[in]
  21: /r/x[out] -> /geo::setpointattribvalue_vector3/geo0[in]
  22: /a/x[out] -> /geo::setpointattribvalue_vector3/elemnum1[in]
  23: /value_string1/value[out] -> /geo::setpointattribvalue_vector3/attribname2[in]
  24: /floattovector3/out0[out] -> /geo::setpointattribvalue_vector3/value3[in]
  25: /geo/x[out] -> /forbegin/x[in]
  26: /value_int/out0[out] -> /forbegin/iterations[in]
  27: /forbegin/scope[out] -> /forend/scope[in]
  28: /forbegin/index[out] -> /a/x[in]
  29: /forbegin/x[out] -> /r/x[in]
  30: /geo::setpointattribvalue_vector3/out0[out] -> /forend/x[in]

Literals:
  0: bool "false" -> 4 ⏎
  1: str "P" -> 28 ⏎
  2: str "Cd" -> 64
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
  0: geo : __null__
  1: value_int : Value<Int>
  2: value_bool : Value<Bool>
  3: geo::boundingbox : geo::BoundingBox
  4: geo::numpoints : geo::NumPoints
  5: value_int1 : Value<Int>
  6: a : __null__
  7: x : __null__
  8: r : __null__
  9: value_string : Value<String>
  10: geo::pointattribvalue_vector3 : geo::PointAttribValue<Vector3>
  11: subtract_vector3 : Subtract<Vector3>
  12: vector3tofloat : Vector3ToFloat
  13: vector3tofloat1 : Vector3ToFloat
  14: divide_float : Divide<Float>
  15: divide_float1 : Divide<Float>
  16: divide_float2 : Divide<Float>
  17: floattovector3 : FloatToVector3
  18: value_int2 : Value<Int>
  19: add_int : Add<Int>
  20: value_string1 : Value<String>
  21: geo::setpointattribvalue_vector3 : geo::SetPointAttribValue<Vector3>
  22: forbegin : ForBegin
  23: forend : ForEnd

Ports:
  0: /geo/__spare__[in]
  1: /geo/__spare__[out]
  2: /geo/x[in]
  3: /geo/x[out]
  4: /value_int/parm[in]
  5: /value_int/value[out]
  6: /value_bool/parm[in]
  7: /value_bool/value[out]
  8: /geo::boundingbox/geo0[in]
  9: /geo::boundingbox/orient1[in]
  10: /geo::boundingbox/local'2[in]
  11: /geo::boundingbox/out0[out]
  12: /geo::boundingbox/out1[out]
  13: /geo::boundingbox/out2[out]
  14: /geo::boundingbox/out3[out]
  15: /geo::boundingbox/out4[out]
  16: /geo::numpoints/geo0[in]
  17: /geo::numpoints/out0[out]
  18: /value_int1/a0[in]
  19: /value_int1/out0[out]
  20: /a/__spare__[in]
  21: /a/__spare__[out]
  22: /a/x[in]
  23: /a/x[out]
  24: /x/__spare__[in]
  25: /x/__spare__[out]
  26: /r/__spare__[in]
  27: /r/__spare__[out]
  28: /r/fst[in]
  29: /r/fst[out]
  30: /r/snd[in]
  31: /r/snd[out]
  32: /value_string/parm[in]
  33: /value_string/value[out]
  34: /geo::pointattribvalue_vector3/rundata[out]
  35: /geo::pointattribvalue_vector3/geo0[in]
  36: /geo::pointattribvalue_vector3/elemnum1[in]
  37: /geo::pointattribvalue_vector3/attribname2[in]
  38: /geo::pointattribvalue_vector3/out0[out]
  39: /geo::pointattribvalue_vector3/out1[out]
  40: /subtract_vector3/a0[in]
  41: /subtract_vector3/b1[in]
  42: /subtract_vector3/out0[out]
  43: /subtract_vector3/out2[in]
  44: /vector3tofloat/vector0[in]
  45: /vector3tofloat/out0[out]
  46: /vector3tofloat/out1[out]
  47: /vector3tofloat/out2[out]
  48: /vector3tofloat1/vector0[in]
  49: /vector3tofloat1/out0[out]
  50: /vector3tofloat1/out1[out]
  51: /vector3tofloat1/out2[out]
  52: /divide_float/a0[in]
  53: /divide_float/b1[in]
  54: /divide_float/out0[out]
  55: /divide_float/out0[in]
  56: /divide_float1/a0[in]
  57: /divide_float1/b1[in]
  58: /divide_float1/out0[out]
  59: /divide_float1/out1[in]
  60: /divide_float2/a0[in]
  61: /divide_float2/b1[in]
  62: /divide_float2/out0[out]
  63: /divide_float2/out2[in]
  64: /floattovector3/x0[in]
  65: /floattovector3/y1[in]
  66: /floattovector3/z2[in]
  67: /floattovector3/out0[out]
  68: /value_int2/parm[in]
  69: /value_int2/value[out]
  70: /add_int/a0[in]
  71: /add_int/b1[in]
  72: /add_int/out0[out]
  73: /add_int/value[in]
  74: /value_string1/parm[in]
  75: /value_string1/value[out]
  76: /geo::setpointattribvalue_vector3/rundata[out]
  77: /geo::setpointattribvalue_vector3/geo0[in]
  78: /geo::setpointattribvalue_vector3/elemnum1[in]
  79: /geo::setpointattribvalue_vector3/attribname2[in]
  80: /geo::setpointattribvalue_vector3/value3[in]
  81: /geo::setpointattribvalue_vector3/out0[out]
  82: /geo::setpointattribvalue_vector3/out1[out]
  83: /forbegin/rundata[out]
  84: /forbegin/iterations[in]
  85: /forbegin/__spare__[in]
  86: /forbegin/scope[out]
  87: /forbegin/index[out]
  88: /forbegin/__spare__[out]
  89: /forbegin/x[in]
  90: /forbegin/x[out]
  91: /forbegin/value[in]
  92: /forbegin/value[out]
  93: /forend/rundata[out]
  94: /forend/scope[in]
  95: /forend/__spare__[in]
  96: /forend/__spare__[out]
  97: /forend/x[in]
  98: /forend/x[out]
  99: /forend/value[in]
  100: /forend/value[out]

Input Ports:
  2: /geo/x[in]

Output Ports:
  98: /forend/x[out]

Wires:
  0: /geo/x[out] -> /geo::boundingbox/geo0[in]
  1: /value_bool/value[out] -> /geo::boundingbox/orient1[in]
  2: /value_bool/value[out] -> /geo::boundingbox/local'2[in]
  3: /geo/x[out] -> /geo::numpoints/geo0[in]
  4: /geo::numpoints/out0[out] -> /value_int1/a0[in]
  5: /r/fst[out] -> /geo::pointattribvalue_vector3/geo0[in]
  6: /a/x[out] -> /geo::pointattribvalue_vector3/elemnum1[in]
  7: /value_string/value[out] -> /geo::pointattribvalue_vector3/attribname2[in]
  8: /geo::pointattribvalue_vector3/out0[out] -> /subtract_vector3/a0[in]
  9: /geo::boundingbox/out2[out] -> /subtract_vector3/out2[in]
  10: /subtract_vector3/out0[out] -> /vector3tofloat/vector0[in]
  11: /geo::boundingbox/out1[out] -> /vector3tofloat1/vector0[in]
  12: /vector3tofloat/out0[out] -> /divide_float/a0[in]
  13: /vector3tofloat1/out0[out] -> /divide_float/out0[in]
  14: /vector3tofloat/out1[out] -> /divide_float1/a0[in]
  15: /vector3tofloat1/out1[out] -> /divide_float1/out1[in]
  16: /vector3tofloat/out2[out] -> /divide_float2/a0[in]
  17: /vector3tofloat1/out2[out] -> /divide_float2/out2[in]
  18: /divide_float/out0[out] -> /floattovector3/x0[in]
  19: /divide_float1/out0[out] -> /floattovector3/y1[in]
  20: /divide_float2/out0[out] -> /floattovector3/z2[in]
  21: /r/snd[out] -> /add_int/a0[in]
  22: /value_int2/value[out] -> /add_int/value[in]
  23: /r/fst[out] -> /geo::setpointattribvalue_vector3/geo0[in]
  24: /a/x[out] -> /geo::setpointattribvalue_vector3/elemnum1[in]
  25: /value_string1/value[out] -> /geo::setpointattribvalue_vector3/attribname2[in]
  26: /floattovector3/out0[out] -> /geo::setpointattribvalue_vector3/value3[in]
  27: /geo/x[out] -> /forbegin/x[in]
  28: /value_int/value[out] -> /forbegin/value[in]
  29: /value_int1/out0[out] -> /forbegin/iterations[in]
  30: /forbegin/scope[out] -> /forend/scope[in]
  31: /forbegin/index[out] -> /a/x[in]
  32: /forbegin/x[out] -> /r/fst[in]
  33: /forbegin/value[out] -> /r/snd[in]
  34: /geo::setpointattribvalue_vector3/out0[out] -> /forend/x[in]
  35: /add_int/out0[out] -> /forend/value[in]

Literals:
  0: int 0 -> 4 ⏎
  1: bool "false" -> 6 ⏎
  2: str "P" -> 32 ⏎
  3: int 1 -> 68 ⏎
  4: str "Cd" -> 74
-/
#guard_msgs in
run_meta
  let e := q(loop2)
  let g ← programToApexGraph e
  IO.println g
