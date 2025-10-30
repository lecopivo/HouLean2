import HouLean

open HouLean Apex Compiler

open Qq


/--
info: Nodes:
  0: x : __null__

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]

Input Ports:
  2: /x/x[in]

Output Ports:
  3: /x/x[out]

Wires:

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x : Float => let y := x; let z := y; z)
  let g ← programToApexGraph e
  IO.println g


/--
info: Nodes:
  0: x : __null__
  1: add_float : Add<Float>
  2: multiply_float : Multiply<Float>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /add_float/a0[in]
  5: /add_float/b1[in]
  6: /add_float/out0[out]
  7: /add_float/x[in]
  8: /multiply_float/a0[in]
  9: /multiply_float/b1[in]
  10: /multiply_float/out0[out]
  11: /multiply_float/x[in]

Input Ports:
  2: /x/x[in]

Output Ports:
  10: /multiply_float/out0[out]

Wires:
  0: /x/x[out] -> /add_float/a0[in]
  1: /x/x[out] -> /add_float/x[in]
  2: /add_float/out0[out] -> /multiply_float/a0[in]
  3: /x/x[out] -> /multiply_float/x[in]

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x : Float => let y := x + x; y * x)
  let g ← programToApexGraph e
  IO.println g


/--
info: Nodes:
  0: x : __null__
  1: add_float : Add<Float>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /add_float/a0[in]
  5: /add_float/b1[in]
  6: /add_float/out0[out]
  7: /add_float/x[in]

Input Ports:
  2: /x/x[in]

Output Ports:
  6: /add_float/out0[out]

Wires:
  0: /x/x[out] -> /add_float/a0[in]
  1: /x/x[out] -> /add_float/x[in]

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x : Float => Generated.AddFloat x #v[x])
  let g ← programToApexGraph e
  IO.println g


/--
info: Nodes:
  0: x : __null__
  1: divide_float : Divide<Float>
  2: value_float : Value<Float>
  3: lerp_float : Lerp<Float>
  4: multiply_float : Multiply<Float>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /divide_float/a0[in]
  5: /divide_float/b1[in]
  6: /divide_float/out0[out]
  7: /divide_float/x[in]
  8: /value_float/parm[in]
  9: /value_float/value[out]
  10: /lerp_float/a0[in]
  11: /lerp_float/b1[in]
  12: /lerp_float/bias2[in]
  13: /lerp_float/out0[out]
  14: /multiply_float/a0[in]
  15: /multiply_float/b1[in]
  16: /multiply_float/out0[out]
  17: /multiply_float/out0[in]

Input Ports:
  2: /x/x[in]

Output Ports:
  16: /multiply_float/out0[out]

Wires:
  0: /x/x[out] -> /divide_float/a0[in]
  1: /x/x[out] -> /divide_float/x[in]
  2: /x/x[out] -> /lerp_float/a0[in]
  3: /divide_float/out0[out] -> /lerp_float/b1[in]
  4: /value_float/value[out] -> /lerp_float/bias2[in]
  5: /divide_float/out0[out] -> /multiply_float/a0[in]
  6: /lerp_float/out0[out] -> /multiply_float/out0[in]

Literals:
  0: float 0.300000 -> 8
-/
#guard_msgs in
run_meta
  let e := q(fun x : Float => let y := x/x; y*x.lerp y 0.3)
  let g ← programToApexGraph e
  IO.println g


/--
info: Nodes:
  0: value_bool : Value<Bool>

Ports:
  0: /value_bool/parm[in]
  1: /value_bool/value[out]

Wires:

Literals:
  0: bool "false" -> 0
-/
#guard_msgs in
run_meta
  let e := q(false)
  let g ← (toApexGraph e).run'
  IO.println g


/--
info: Nodes:
  0: x : __null__

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/fst[in]
  3: /x/fst[out]
  4: /x/snd_fst[in]
  5: /x/snd_fst[out]
  6: /x/snd_snd[in]
  7: /x/snd_snd[out]

Input Ports:
  2: /x/fst[in]
  4: /x/snd_fst[in]
  6: /x/snd_snd[in]

Output Ports:
  3: /x/fst[out]

Wires:

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x : Float×Float×Float => x.1)
  let g ← programToApexGraph e
  IO.println g


/--
info: Nodes:
  0: x : __null__

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/fst[in]
  3: /x/fst[out]
  4: /x/snd_fst[in]
  5: /x/snd_fst[out]
  6: /x/snd_snd[in]
  7: /x/snd_snd[out]

Input Ports:
  2: /x/fst[in]
  4: /x/snd_fst[in]
  6: /x/snd_snd[in]

Output Ports:
  5: /x/snd_fst[out]

Wires:

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x : Float×Float×Float => x.2.1)
  let g ← programToApexGraph e
  IO.println g


/--
info: Nodes:
  0: x : __null__

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/fst[in]
  3: /x/fst[out]
  4: /x/snd_fst[in]
  5: /x/snd_fst[out]
  6: /x/snd_snd[in]
  7: /x/snd_snd[out]

Input Ports:
  2: /x/fst[in]
  4: /x/snd_fst[in]
  6: /x/snd_snd[in]

Output Ports:
  7: /x/snd_snd[out]

Wires:

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x : Float×Float×Float => x.2.2)
  let g ← programToApexGraph e
  IO.println g


/--
info: import apex
geo = hou.pwd().geometry()
graph = apex.Graph()

# Adding Nodes
n0 = graph.addNode("x", "__null__")
graph.addSubPort(0, "(x, Int)")
n1 = graph.addNode("value_int", "Value<Int>")
n2 = graph.addNode("a", "__null__")
graph.addSubPort(6, "(x, Int)")
n3 = graph.addNode("x1", "__null__")
n4 = graph.addNode("r", "__null__")
graph.addSubPort(12, "(x, Int)")
n5 = graph.addNode("add_int", "Add<Int>")
graph.addSubPort(17, "(x, Int)")
n6 = graph.addNode("forbegin", "ForBegin")
graph.addSubPort(22, "(x, Int)")
n7 = graph.addNode("forend", "ForEnd")
graph.addSubPort(30, "(x, Int)")

# Add Wires
graph.addWire(15, 16) # r[x] -> add_int[a0]
graph.addWire(15, 19) # r[x] -> add_int[x]
graph.addWire(3, 26) # x[x] -> forbegin[x]
graph.addWire(5, 21) # value_int[value] -> forbegin[iterations]
graph.addWire(23, 29) # forbegin[scope] -> forend[scope]
graph.addWire(24, 8) # forbegin[index] -> a[x]
graph.addWire(27, 14) # forbegin[x] -> r[x]
graph.addWire(18, 32) # add_int[out0] -> forend[x]

# Add Inputs and Outputs
graph.addNode("input", "__parms__")
graph.addGraphInput(9, "x")
graph.addWire(35, 2)
graph.addNode("output", "__output__")
graph.addGraphOutput(10, "x0")
graph.addWire(33, 37)
graph.setNodeParm(2, "parm", 5)

# Layout and Save Graph
graph.layout()
graph.saveToGeometry(geo)
-/
#guard_msgs in
run_meta
  let e := q(fun (x : Int) => Id.run do
  let mut x : Int := x
  for _ in [0:5] do
    x := x + x
  return x)
  let g ← programToApexGraph e
  IO.println g.pythonBuildScript


/--
info: Nodes:
  0: x : __null__

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/fst[in]
  3: /x/fst[out]
  4: /x/snd_fst[in]
  5: /x/snd_fst[out]
  6: /x/snd_snd_fst[in]
  7: /x/snd_snd_fst[out]
  8: /x/snd_snd_snd_fst[in]
  9: /x/snd_snd_snd_fst[out]
  10: /x/snd_snd_snd_snd[in]
  11: /x/snd_snd_snd_snd[out]

Input Ports:
  2: /x/fst[in]
  4: /x/snd_fst[in]
  6: /x/snd_snd_fst[in]
  8: /x/snd_snd_snd_fst[in]
  10: /x/snd_snd_snd_snd[in]

Output Ports:
  7: /x/snd_snd_fst[out]

Wires:

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x : Float×Float×Float×Float×Float => x.2.2.1)
  let g ← programToApexGraph e
  IO.println g

/--
info: Nodes:
  0: x : __null__

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/fst[in]
  3: /x/fst[out]
  4: /x/snd_fst[in]
  5: /x/snd_fst[out]
  6: /x/snd_snd[in]
  7: /x/snd_snd[out]

Input Ports:
  2: /x/fst[in]
  4: /x/snd_fst[in]
  6: /x/snd_snd[in]

Output Ports:
  3: /x/fst[out]
  5: /x/snd_fst[out]
  7: /x/snd_snd[out]

Wires:

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x : Float×Float×Float => x)
  let g ← programToApexGraph e
  IO.println g



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

@[apex]
def getBBoxSize (geo : Geometry) : Vector3 :=
  let r := geo.boundingBox
  let size := r.2.1
  size

/--
info: Nodes:
  0: v : __null__
  1: vector3tofloat : Vector3ToFloat
  2: vector3tofloat1 : Vector3ToFloat
  3: vector3tofloat2 : Vector3ToFloat

Ports:
  0: /v/__spare__[in]
  1: /v/__spare__[out]
  2: /v/x[in]
  3: /v/x[out]
  4: /vector3tofloat/vector0[in]
  5: /vector3tofloat/out0[out]
  6: /vector3tofloat/out1[out]
  7: /vector3tofloat/out2[out]
  8: /vector3tofloat1/vector0[in]
  9: /vector3tofloat1/out0[out]
  10: /vector3tofloat1/out1[out]
  11: /vector3tofloat1/out2[out]
  12: /vector3tofloat2/vector0[in]
  13: /vector3tofloat2/out0[out]
  14: /vector3tofloat2/out1[out]
  15: /vector3tofloat2/out2[out]

Input Ports:
  2: /v/x[in]

Output Ports:
  5: /vector3tofloat/out0[out]
  10: /vector3tofloat1/out1[out]
  15: /vector3tofloat2/out2[out]

Wires:
  0: /v/x[out] -> /vector3tofloat/vector0[in]
  1: /v/x[out] -> /vector3tofloat1/vector0[in]
  2: /v/x[out] -> /vector3tofloat2/vector0[in]

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun v : Vector3 => (v.x, v.y, v.z))
  let g ← programToApexGraph e
  IO.println g


abbrev match1 := fun v : Vector3 => let ⟨x,y,_⟩ := v; (x,y)

/--
info: Nodes:
  0: v : __null__
  1: vector3tofloat : Vector3ToFloat
  2: vector3tofloat1 : Vector3ToFloat

Ports:
  0: /v/__spare__[in]
  1: /v/__spare__[out]
  2: /v/x[in]
  3: /v/x[out]
  4: /vector3tofloat/vector0[in]
  5: /vector3tofloat/out0[out]
  6: /vector3tofloat/out1[out]
  7: /vector3tofloat/out2[out]
  8: /vector3tofloat1/vector0[in]
  9: /vector3tofloat1/out0[out]
  10: /vector3tofloat1/out1[out]
  11: /vector3tofloat1/out2[out]

Input Ports:
  2: /v/x[in]

Output Ports:
  5: /vector3tofloat/out0[out]
  10: /vector3tofloat1/out1[out]

Wires:
  0: /v/x[out] -> /vector3tofloat/vector0[in]
  1: /v/x[out] -> /vector3tofloat1/vector0[in]

Literals:
-/
#guard_msgs in
run_meta 
  let e := q(match1)
  let g ← programToApexGraph e
  IO.println g


/-- error: Cannot get APEX type for Option Float -/
#guard_msgs in  
run_meta
  let e := q(fun v : Option Float => (v.isSome, v.getD default))
  let g ← programToApexGraph e
  IO.println g


/--
info: Nodes:
  0: x : __null__

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/fst[in]
  3: /x/fst[out]
  4: /x/snd_fst[in]
  5: /x/snd_fst[out]
  6: /x/snd_snd[in]
  7: /x/snd_snd[out]

Input Ports:
  2: /x/fst[in]
  4: /x/snd_fst[in]
  6: /x/snd_snd[in]

Output Ports:
  3: /x/fst[out]
  5: /x/snd_fst[out]
  7: /x/snd_snd[out]

Wires:

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x : Float×Float×Float => x)
  let g ← programToApexGraph e
  IO.println g



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
  2: value_bool1 : Value<Bool>
  3: geo::boundingbox : geo::BoundingBox
  4: geo::numpoints : geo::NumPoints
  5: value_int : Value<Int>
  6: a : __null__
  7: x : __null__
  8: r : __null__
  9: value_string : Value<String>
  10: geo::pointattribvalue_vector3 : geo::PointAttribValue<Vector3>
  11: subtract_vector3 : Subtract<Vector3>
  12: vector3tofloat : Vector3ToFloat
  13: vector3tofloat1 : Vector3ToFloat
  14: divide_float : Divide<Float>
  15: subtract_vector31 : Subtract<Vector3>
  16: vector3tofloat2 : Vector3ToFloat
  17: vector3tofloat3 : Vector3ToFloat
  18: divide_float1 : Divide<Float>
  19: subtract_vector32 : Subtract<Vector3>
  20: vector3tofloat4 : Vector3ToFloat
  21: vector3tofloat5 : Vector3ToFloat
  22: divide_float2 : Divide<Float>
  23: floattovector3 : FloatToVector3
  24: value_string1 : Value<String>
  25: geo::setpointattribvalue_vector3 : geo::SetPointAttribValue<Vector3>
  26: forbegin : ForBegin
  27: forend : ForEnd

Ports:
  0: /geo/__spare__[in]
  1: /geo/__spare__[out]
  2: /geo/x[in]
  3: /geo/x[out]
  4: /value_bool/parm[in]
  5: /value_bool/value[out]
  6: /value_bool1/parm[in]
  7: /value_bool1/value[out]
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
  18: /value_int/a0[in]
  19: /value_int/out0[out]
  20: /a/__spare__[in]
  21: /a/__spare__[out]
  22: /a/x[in]
  23: /a/x[out]
  24: /x/__spare__[in]
  25: /x/__spare__[out]
  26: /r/__spare__[in]
  27: /r/__spare__[out]
  28: /r/x[in]
  29: /r/x[out]
  30: /value_string/parm[in]
  31: /value_string/value[out]
  32: /geo::pointattribvalue_vector3/rundata[out]
  33: /geo::pointattribvalue_vector3/geo0[in]
  34: /geo::pointattribvalue_vector3/elemnum1[in]
  35: /geo::pointattribvalue_vector3/attribname2[in]
  36: /geo::pointattribvalue_vector3/out0[out]
  37: /geo::pointattribvalue_vector3/out1[out]
  38: /subtract_vector3/a0[in]
  39: /subtract_vector3/b1[in]
  40: /subtract_vector3/out0[out]
  41: /subtract_vector3/out2[in]
  42: /vector3tofloat/vector0[in]
  43: /vector3tofloat/out0[out]
  44: /vector3tofloat/out1[out]
  45: /vector3tofloat/out2[out]
  46: /vector3tofloat1/vector0[in]
  47: /vector3tofloat1/out0[out]
  48: /vector3tofloat1/out1[out]
  49: /vector3tofloat1/out2[out]
  50: /divide_float/a0[in]
  51: /divide_float/b1[in]
  52: /divide_float/out0[out]
  53: /divide_float/out0[in]
  54: /subtract_vector31/a0[in]
  55: /subtract_vector31/b1[in]
  56: /subtract_vector31/out0[out]
  57: /subtract_vector31/out2[in]
  58: /vector3tofloat2/vector0[in]
  59: /vector3tofloat2/out0[out]
  60: /vector3tofloat2/out1[out]
  61: /vector3tofloat2/out2[out]
  62: /vector3tofloat3/vector0[in]
  63: /vector3tofloat3/out0[out]
  64: /vector3tofloat3/out1[out]
  65: /vector3tofloat3/out2[out]
  66: /divide_float1/a0[in]
  67: /divide_float1/b1[in]
  68: /divide_float1/out0[out]
  69: /divide_float1/out1[in]
  70: /subtract_vector32/a0[in]
  71: /subtract_vector32/b1[in]
  72: /subtract_vector32/out0[out]
  73: /subtract_vector32/out2[in]
  74: /vector3tofloat4/vector0[in]
  75: /vector3tofloat4/out0[out]
  76: /vector3tofloat4/out1[out]
  77: /vector3tofloat4/out2[out]
  78: /vector3tofloat5/vector0[in]
  79: /vector3tofloat5/out0[out]
  80: /vector3tofloat5/out1[out]
  81: /vector3tofloat5/out2[out]
  82: /divide_float2/a0[in]
  83: /divide_float2/b1[in]
  84: /divide_float2/out0[out]
  85: /divide_float2/out2[in]
  86: /floattovector3/x0[in]
  87: /floattovector3/y1[in]
  88: /floattovector3/z2[in]
  89: /floattovector3/out0[out]
  90: /value_string1/parm[in]
  91: /value_string1/value[out]
  92: /geo::setpointattribvalue_vector3/rundata[out]
  93: /geo::setpointattribvalue_vector3/geo0[in]
  94: /geo::setpointattribvalue_vector3/elemnum1[in]
  95: /geo::setpointattribvalue_vector3/attribname2[in]
  96: /geo::setpointattribvalue_vector3/value3[in]
  97: /geo::setpointattribvalue_vector3/out0[out]
  98: /geo::setpointattribvalue_vector3/out1[out]
  99: /forbegin/rundata[out]
  100: /forbegin/iterations[in]
  101: /forbegin/__spare__[in]
  102: /forbegin/scope[out]
  103: /forbegin/index[out]
  104: /forbegin/__spare__[out]
  105: /forbegin/x[in]
  106: /forbegin/x[out]
  107: /forend/rundata[out]
  108: /forend/scope[in]
  109: /forend/__spare__[in]
  110: /forend/__spare__[out]
  111: /forend/x[in]
  112: /forend/x[out]

Input Ports:
  2: /geo/x[in]

Output Ports:
  112: /forend/x[out]

Wires:
  0: /geo/x[out] -> /geo::boundingbox/geo0[in]
  1: /value_bool/value[out] -> /geo::boundingbox/orient1[in]
  2: /value_bool1/value[out] -> /geo::boundingbox/local'2[in]
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
  14: /geo::pointattribvalue_vector3/out0[out] -> /subtract_vector31/a0[in]
  15: /geo::boundingbox/out2[out] -> /subtract_vector31/out2[in]
  16: /subtract_vector31/out0[out] -> /vector3tofloat2/vector0[in]
  17: /geo::boundingbox/out1[out] -> /vector3tofloat3/vector0[in]
  18: /vector3tofloat2/out1[out] -> /divide_float1/a0[in]
  19: /vector3tofloat3/out1[out] -> /divide_float1/out1[in]
  20: /geo::pointattribvalue_vector3/out0[out] -> /subtract_vector32/a0[in]
  21: /geo::boundingbox/out2[out] -> /subtract_vector32/out2[in]
  22: /subtract_vector32/out0[out] -> /vector3tofloat4/vector0[in]
  23: /geo::boundingbox/out1[out] -> /vector3tofloat5/vector0[in]
  24: /vector3tofloat4/out2[out] -> /divide_float2/a0[in]
  25: /vector3tofloat5/out2[out] -> /divide_float2/out2[in]
  26: /divide_float/out0[out] -> /floattovector3/x0[in]
  27: /divide_float1/out0[out] -> /floattovector3/y1[in]
  28: /divide_float2/out0[out] -> /floattovector3/z2[in]
  29: /r/x[out] -> /geo::setpointattribvalue_vector3/geo0[in]
  30: /a/x[out] -> /geo::setpointattribvalue_vector3/elemnum1[in]
  31: /value_string1/value[out] -> /geo::setpointattribvalue_vector3/attribname2[in]
  32: /floattovector3/out0[out] -> /geo::setpointattribvalue_vector3/value3[in]
  33: /geo/x[out] -> /forbegin/x[in]
  34: /value_int/out0[out] -> /forbegin/iterations[in]
  35: /forbegin/scope[out] -> /forend/scope[in]
  36: /forbegin/index[out] -> /a/x[in]
  37: /forbegin/x[out] -> /r/x[in]
  38: /geo::setpointattribvalue_vector3/out0[out] -> /forend/x[in]

Literals:
  0: bool "false" -> 4 ⏎
  1: bool "false" -> 6 ⏎
  2: str "P" -> 30 ⏎
  3: str "Cd" -> 90
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
  3: value_bool1 : Value<Bool>
  4: geo::boundingbox : geo::BoundingBox
  5: geo::numpoints : geo::NumPoints
  6: value_int1 : Value<Int>
  7: a : __null__
  8: x : __null__
  9: r : __null__
  10: value_string : Value<String>
  11: geo::pointattribvalue_vector3 : geo::PointAttribValue<Vector3>
  12: subtract_vector3 : Subtract<Vector3>
  13: vector3tofloat : Vector3ToFloat
  14: vector3tofloat1 : Vector3ToFloat
  15: divide_float : Divide<Float>
  16: subtract_vector31 : Subtract<Vector3>
  17: vector3tofloat2 : Vector3ToFloat
  18: vector3tofloat3 : Vector3ToFloat
  19: divide_float1 : Divide<Float>
  20: subtract_vector32 : Subtract<Vector3>
  21: vector3tofloat4 : Vector3ToFloat
  22: vector3tofloat5 : Vector3ToFloat
  23: divide_float2 : Divide<Float>
  24: floattovector3 : FloatToVector3
  25: value_int2 : Value<Int>
  26: add_int : Add<Int>
  27: value_string1 : Value<String>
  28: geo::setpointattribvalue_vector3 : geo::SetPointAttribValue<Vector3>
  29: forbegin : ForBegin
  30: forend : ForEnd

Ports:
  0: /geo/__spare__[in]
  1: /geo/__spare__[out]
  2: /geo/x[in]
  3: /geo/x[out]
  4: /value_int/parm[in]
  5: /value_int/value[out]
  6: /value_bool/parm[in]
  7: /value_bool/value[out]
  8: /value_bool1/parm[in]
  9: /value_bool1/value[out]
  10: /geo::boundingbox/geo0[in]
  11: /geo::boundingbox/orient1[in]
  12: /geo::boundingbox/local'2[in]
  13: /geo::boundingbox/out0[out]
  14: /geo::boundingbox/out1[out]
  15: /geo::boundingbox/out2[out]
  16: /geo::boundingbox/out3[out]
  17: /geo::boundingbox/out4[out]
  18: /geo::numpoints/geo0[in]
  19: /geo::numpoints/out0[out]
  20: /value_int1/a0[in]
  21: /value_int1/out0[out]
  22: /a/__spare__[in]
  23: /a/__spare__[out]
  24: /a/x[in]
  25: /a/x[out]
  26: /x/__spare__[in]
  27: /x/__spare__[out]
  28: /r/__spare__[in]
  29: /r/__spare__[out]
  30: /r/fst[in]
  31: /r/fst[out]
  32: /r/snd[in]
  33: /r/snd[out]
  34: /value_string/parm[in]
  35: /value_string/value[out]
  36: /geo::pointattribvalue_vector3/rundata[out]
  37: /geo::pointattribvalue_vector3/geo0[in]
  38: /geo::pointattribvalue_vector3/elemnum1[in]
  39: /geo::pointattribvalue_vector3/attribname2[in]
  40: /geo::pointattribvalue_vector3/out0[out]
  41: /geo::pointattribvalue_vector3/out1[out]
  42: /subtract_vector3/a0[in]
  43: /subtract_vector3/b1[in]
  44: /subtract_vector3/out0[out]
  45: /subtract_vector3/out2[in]
  46: /vector3tofloat/vector0[in]
  47: /vector3tofloat/out0[out]
  48: /vector3tofloat/out1[out]
  49: /vector3tofloat/out2[out]
  50: /vector3tofloat1/vector0[in]
  51: /vector3tofloat1/out0[out]
  52: /vector3tofloat1/out1[out]
  53: /vector3tofloat1/out2[out]
  54: /divide_float/a0[in]
  55: /divide_float/b1[in]
  56: /divide_float/out0[out]
  57: /divide_float/out0[in]
  58: /subtract_vector31/a0[in]
  59: /subtract_vector31/b1[in]
  60: /subtract_vector31/out0[out]
  61: /subtract_vector31/out2[in]
  62: /vector3tofloat2/vector0[in]
  63: /vector3tofloat2/out0[out]
  64: /vector3tofloat2/out1[out]
  65: /vector3tofloat2/out2[out]
  66: /vector3tofloat3/vector0[in]
  67: /vector3tofloat3/out0[out]
  68: /vector3tofloat3/out1[out]
  69: /vector3tofloat3/out2[out]
  70: /divide_float1/a0[in]
  71: /divide_float1/b1[in]
  72: /divide_float1/out0[out]
  73: /divide_float1/out1[in]
  74: /subtract_vector32/a0[in]
  75: /subtract_vector32/b1[in]
  76: /subtract_vector32/out0[out]
  77: /subtract_vector32/out2[in]
  78: /vector3tofloat4/vector0[in]
  79: /vector3tofloat4/out0[out]
  80: /vector3tofloat4/out1[out]
  81: /vector3tofloat4/out2[out]
  82: /vector3tofloat5/vector0[in]
  83: /vector3tofloat5/out0[out]
  84: /vector3tofloat5/out1[out]
  85: /vector3tofloat5/out2[out]
  86: /divide_float2/a0[in]
  87: /divide_float2/b1[in]
  88: /divide_float2/out0[out]
  89: /divide_float2/out2[in]
  90: /floattovector3/x0[in]
  91: /floattovector3/y1[in]
  92: /floattovector3/z2[in]
  93: /floattovector3/out0[out]
  94: /value_int2/parm[in]
  95: /value_int2/value[out]
  96: /add_int/a0[in]
  97: /add_int/b1[in]
  98: /add_int/out0[out]
  99: /add_int/value[in]
  100: /value_string1/parm[in]
  101: /value_string1/value[out]
  102: /geo::setpointattribvalue_vector3/rundata[out]
  103: /geo::setpointattribvalue_vector3/geo0[in]
  104: /geo::setpointattribvalue_vector3/elemnum1[in]
  105: /geo::setpointattribvalue_vector3/attribname2[in]
  106: /geo::setpointattribvalue_vector3/value3[in]
  107: /geo::setpointattribvalue_vector3/out0[out]
  108: /geo::setpointattribvalue_vector3/out1[out]
  109: /forbegin/rundata[out]
  110: /forbegin/iterations[in]
  111: /forbegin/__spare__[in]
  112: /forbegin/scope[out]
  113: /forbegin/index[out]
  114: /forbegin/__spare__[out]
  115: /forbegin/x[in]
  116: /forbegin/x[out]
  117: /forbegin/value[in]
  118: /forbegin/value[out]
  119: /forend/rundata[out]
  120: /forend/scope[in]
  121: /forend/__spare__[in]
  122: /forend/__spare__[out]
  123: /forend/x[in]
  124: /forend/x[out]
  125: /forend/value[in]
  126: /forend/value[out]

Input Ports:
  2: /geo/x[in]

Output Ports:
  124: /forend/x[out]

Wires:
  0: /geo/x[out] -> /geo::boundingbox/geo0[in]
  1: /value_bool/value[out] -> /geo::boundingbox/orient1[in]
  2: /value_bool1/value[out] -> /geo::boundingbox/local'2[in]
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
  14: /geo::pointattribvalue_vector3/out0[out] -> /subtract_vector31/a0[in]
  15: /geo::boundingbox/out2[out] -> /subtract_vector31/out2[in]
  16: /subtract_vector31/out0[out] -> /vector3tofloat2/vector0[in]
  17: /geo::boundingbox/out1[out] -> /vector3tofloat3/vector0[in]
  18: /vector3tofloat2/out1[out] -> /divide_float1/a0[in]
  19: /vector3tofloat3/out1[out] -> /divide_float1/out1[in]
  20: /geo::pointattribvalue_vector3/out0[out] -> /subtract_vector32/a0[in]
  21: /geo::boundingbox/out2[out] -> /subtract_vector32/out2[in]
  22: /subtract_vector32/out0[out] -> /vector3tofloat4/vector0[in]
  23: /geo::boundingbox/out1[out] -> /vector3tofloat5/vector0[in]
  24: /vector3tofloat4/out2[out] -> /divide_float2/a0[in]
  25: /vector3tofloat5/out2[out] -> /divide_float2/out2[in]
  26: /divide_float/out0[out] -> /floattovector3/x0[in]
  27: /divide_float1/out0[out] -> /floattovector3/y1[in]
  28: /divide_float2/out0[out] -> /floattovector3/z2[in]
  29: /r/snd[out] -> /add_int/a0[in]
  30: /value_int2/value[out] -> /add_int/value[in]
  31: /r/fst[out] -> /geo::setpointattribvalue_vector3/geo0[in]
  32: /a/x[out] -> /geo::setpointattribvalue_vector3/elemnum1[in]
  33: /value_string1/value[out] -> /geo::setpointattribvalue_vector3/attribname2[in]
  34: /floattovector3/out0[out] -> /geo::setpointattribvalue_vector3/value3[in]
  35: /geo/x[out] -> /forbegin/x[in]
  36: /value_int/value[out] -> /forbegin/value[in]
  37: /value_int1/out0[out] -> /forbegin/iterations[in]
  38: /forbegin/scope[out] -> /forend/scope[in]
  39: /forbegin/index[out] -> /a/x[in]
  40: /forbegin/x[out] -> /r/fst[in]
  41: /forbegin/value[out] -> /r/snd[in]
  42: /geo::setpointattribvalue_vector3/out0[out] -> /forend/x[in]
  43: /add_int/out0[out] -> /forend/value[in]

Literals:
  0: int 0 -> 4 ⏎
  1: bool "false" -> 6 ⏎
  2: bool "false" -> 8 ⏎
  3: str "P" -> 34 ⏎
  4: int 1 -> 94 ⏎
  5: str "Cd" -> 100
-/
#guard_msgs in
run_meta
  let e := q(loop2)
  let g ← programToApexGraph e
  IO.println g


def add' (x y : Float) : Float := x + y

/--
info: Nodes:
  0: x : __null__
  1: y : __null__
  2: add_float : Add<Float>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /y/__spare__[in]
  5: /y/__spare__[out]
  6: /y/x[in]
  7: /y/x[out]
  8: /add_float/a0[in]
  9: /add_float/b1[in]
  10: /add_float/out0[out]
  11: /add_float/x[in]

Input Ports:
  2: /x/x[in]
  6: /y/x[in]

Output Ports:
  10: /add_float/out0[out]

Wires:
  0: /x/x[out] -> /add_float/a0[in]
  1: /y/x[out] -> /add_float/x[in]

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x y => add' x y)
  let g ← programToApexGraph e
  IO.println g


/--
info: Nodes:
  0: x : __null__
  1: y : __null__
  2: add_int : Add<Int>

Ports:
  0: /x/__spare__[in]
  1: /x/__spare__[out]
  2: /x/x[in]
  3: /x/x[out]
  4: /y/__spare__[in]
  5: /y/__spare__[out]
  6: /y/x[in]
  7: /y/x[out]
  8: /add_int/a0[in]
  9: /add_int/b1[in]
  10: /add_int/out0[out]
  11: /add_int/x[in]

Input Ports:
  2: /x/x[in]
  6: /y/x[in]

Output Ports:
  10: /add_int/out0[out]

Wires:
  0: /x/x[out] -> /add_int/a0[in]
  1: /y/x[out] -> /add_int/x[in]

Literals:
-/
#guard_msgs in
run_meta
  let e := q(fun x y : Nat => x + y)
  let g ← programToApexGraph e
  IO.println g
