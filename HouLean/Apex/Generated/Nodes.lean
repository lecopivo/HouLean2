import HouLean.Apex.Compile.NodeType
import HouLean.Apex.Generated.Types

set_option synthInstance.maxSize 512

namespace HouLean.Apex.Generated

/-- outputs: (result) -/        
@[apex_node "Abs<Float>"]
opaque AbsFloat (value : Float) : Float

/-- outputs: (result) -/        
@[apex_node "Abs<Int>"]
opaque AbsInt (value : Int) : Int

/-- outputs: (result) -/        
@[apex_node "Abs<Vector2>"]
opaque AbsVector2 (value : Vector2) : Vector2

/-- outputs: (result) -/        
@[apex_node "Abs<Vector3>"]
opaque AbsVector3 (value : Vector3) : Vector3

/-- outputs: (result) -/        
@[apex_node "Abs<Vector4>"]
opaque AbsVector4 (value : Vector4) : Vector4

/-- outputs: (result) -/        
@[apex_node "Acos"]
opaque Acos (radians : Float) : Float

/-- outputs: (result) -/        
@[apex_node "Add<Float,Int>"]
opaque AddFloatInt (a : Float) {numb: Nat} (b : VariadicArg Int numb) : Float

/-- outputs: (result) -/        
@[apex_node "Add<Float>"]
opaque AddFloat (a : Float) {numb: Nat} (b : VariadicArg Float numb) : Float

/-- outputs: (result) -/        
@[apex_node "Add<Int,Float>"]
opaque AddIntFloat (a : Int) {numb: Nat} (b : VariadicArg Float numb) : Int

/-- outputs: (result) -/        
@[apex_node "Add<Int>"]
opaque AddInt (a : Int) {numb: Nat} (b : VariadicArg Int numb) : Int

/-- outputs: (result) -/        
@[apex_node "Add<Matrix3>"]
opaque AddMatrix3 (a : Matrix3) {numb: Nat} (b : VariadicArg Matrix3 numb) : Matrix3

/-- outputs: (result) -/        
@[apex_node "Add<Matrix4>"]
opaque AddMatrix4 (a : Matrix4) {numb: Nat} (b : VariadicArg Matrix4 numb) : Matrix4

/-- outputs: (result) -/        
@[apex_node "Add<String>"]
opaque AddString (a : String) {numb: Nat} (b : VariadicArg String numb) : String

/-- outputs: (result) -/        
@[apex_node "Add<Vector2>"]
opaque AddVector2 (a : Vector2) {numb: Nat} (b : VariadicArg Vector2 numb) : Vector2

/-- outputs: (result) -/        
@[apex_node "Add<Vector3,Float>"]
opaque AddVector3Float (a : Vector3) {numb: Nat} (b : VariadicArg Float numb) : Vector3

/-- outputs: (result) -/        
@[apex_node "Add<Vector3,Int>"]
opaque AddVector3Int (a : Vector3) {numb: Nat} (b : VariadicArg Int numb) : Vector3

/-- outputs: (result) -/        
@[apex_node "Add<Vector3>"]
opaque AddVector3 (a : Vector3) {numb: Nat} (b : VariadicArg Vector3 numb) : Vector3

/-- outputs: (result) -/        
@[apex_node "AlmostEquals<Float>"]
opaque AlmostEqualsFloat (a : Float) (b : Float) (tolerance : Float) : Bool

/-- outputs: (result) -/        
@[apex_node "AlmostEquals<Matrix3>"]
opaque AlmostEqualsMatrix3 (a : Matrix3) (b : Matrix3) (tolerance : Float) : Bool

/-- outputs: (result) -/        
@[apex_node "AlmostEquals<Matrix4>"]
opaque AlmostEqualsMatrix4 (a : Matrix4) (b : Matrix4) (tolerance : Float) : Bool

/-- outputs: (result) -/        
@[apex_node "And"]
opaque And {numinputs: Nat} (inputs : VariadicArg Bool numinputs) : Bool

/-- outputs: (result) -/        
@[apex_node "Asin"]
opaque Asin (radians : Float) : Float

/-- outputs: (result) -/        
@[apex_node "Atan"]
opaque Atan (radians : Float) : Float

/-- outputs: (result) -/        
@[apex_node "Atan2"]
opaque Atan2 (x : Float) (y : Float) : Float

/-- outputs: (result) -/        
@[apex_node "Average<Float>"]
opaque AverageFloat {numvalues: Nat} (values : VariadicArg Float numvalues) : Float

/-- outputs: (result) -/        
@[apex_node "BitwiseAnd"]
opaque BitwiseAnd {numinputs: Nat} (inputs : VariadicArg Int numinputs) : Int

/-- outputs: (result) -/        
@[apex_node "BitwiseNot"]
opaque BitwiseNot (input : Int) : Int

/-- outputs: (result) -/        
@[apex_node "BitwiseOr"]
opaque BitwiseOr {numinputs: Nat} (inputs : VariadicArg Int numinputs) : Int

/-- outputs: (result) -/        
@[apex_node "BitwiseXor"]
opaque BitwiseXor {numinputs: Nat} (inputs : VariadicArg Int numinputs) : Int

/-- outputs: (mask) -/        
@[apex_node "BoolToIntBitMask::2.0"]
opaque BoolToIntBitMask_2_0 {numentries: Nat} (entries : VariadicArg Bool numentries) : Int

/-- outputs: (geodetic) -/        
@[apex_node "CartesianToGeodetic"]
opaque CartesianToGeodetic (cartesian : Vector3) : Vector3

/-- outputs: (polar) -/        
@[apex_node "CartesianToPolar"]
opaque CartesianToPolar (cartesian : Vector2) : Vector2

/-- outputs: (spherical) -/        
@[apex_node "CartesianToSpherical"]
opaque CartesianToSpherical (cartesian : Vector3) : Vector3

/-- outputs: (result) -/        
@[apex_node "Ceil"]
opaque Ceil (val : Float) : Float

/-- outputs: (result) -/        
@[apex_node "Clamp<Float>"]
opaque ClampFloat (val : Float) (min : Float) (max : Float) : Float

/-- outputs: (result) -/        
@[apex_node "Clamp<Int>"]
opaque ClampInt (val : Int) (min : Int) (max : Int) : Int

/-- outputs: (result) -/        
@[apex_node "Complement"]
opaque Complement (val : Float) : Float

/-- outputs: (mask,use_t,use_r,use_s,components) -/        
@[apex_node "ComponentBoolsToInt"]
opaque ComponentBoolsToInt {numentries: Nat} (entries : VariadicArg Bool numentries) (entries0 : Bool) (entries1 : Bool) (entries2 : Bool) (components : Int) (use_t : Bool) (use_r : Bool) (use_s : Bool) : Int×Bool×Bool×Bool×Int

-- special function not supported yet
-- opaque ComponentIntToBools (entries : Int) (use_t : Bool) (use_r : Bool) (use_s : Bool) (components : Int) :...

/-- outputs: (b) -/        
@[apex_node "Convert<ApexNodeID,Bool>"]
opaque ConvertApexNodeIDBool (a : ApexNodeID) : Bool

/-- outputs: (b) -/        
@[apex_node "Convert<ApexNodeID,Int>"]
opaque ConvertApexNodeIDInt (a : ApexNodeID) : Int

/-- outputs: (b) -/        
@[apex_node "Convert<ApexPortID,Bool>"]
opaque ConvertApexPortIDBool (a : ApexPortID) : Bool

/-- outputs: (b) -/        
@[apex_node "Convert<ApexPortID,Int>"]
opaque ConvertApexPortIDInt (a : ApexPortID) : Int

/-- outputs: (b) -/        
@[apex_node "Convert<Bool,Int>"]
opaque ConvertBoolInt (a : Bool) : Int

/-- outputs: (b) -/        
@[apex_node "Convert<ColorRamp,FloatRamp>"]
opaque ConvertColorRampFloatRamp (a : ColorRamp) : FloatRamp

/-- outputs: (b) -/        
@[apex_node "Convert<Float,Int>"]
opaque ConvertFloatInt (a : Float) : Int

/-- outputs: (b) -/        
@[apex_node "Convert<FloatRamp,ColorRamp>"]
opaque ConvertFloatRampColorRamp (a : FloatRamp) : ColorRamp

/-- outputs: (b) -/        
@[apex_node "Convert<Int,Bool>"]
opaque ConvertIntBool (a : Int) : Bool

/-- outputs: (b) -/        
@[apex_node "Convert<Int,Float>"]
opaque ConvertIntFloat (a : Int) : Float

/-- outputs: (b) -/        
@[apex_node "Convert<Matrix3,Matrix4>"]
opaque ConvertMatrix3Matrix4 (a : Matrix3) : Matrix4

/-- outputs: (b) -/        
@[apex_node "Convert<Matrix4,Matrix3>"]
opaque ConvertMatrix4Matrix3 (a : Matrix4) : Matrix3

/-- outputs: (b) -/        
@[apex_node "Convert<Matrix4,Vector4>"]
opaque ConvertMatrix4Vector4 (a : Matrix4) : Vector4

/-- outputs: (b) -/        
@[apex_node "Convert<Vector4,Matrix3>"]
opaque ConvertVector4Matrix3 (a : Vector4) : Matrix3

/-- outputs: (result) -/        
@[apex_node "Cosine"]
opaque Cosine (radians : Float) : Float

/-- outputs: (cross) -/        
@[apex_node "CrossProduct"]
opaque CrossProduct (a : Vector3) (b : Vector3) : Vector3

/-- outputs: (radians) -/        
@[apex_node "DegreesToRadians<Float>"]
opaque DegreesToRadiansFloat (degrees : Float) : Float

/-- outputs: (radians) -/        
@[apex_node "DegreesToRadians<Vector3>"]
opaque DegreesToRadiansVector3 (degrees : Vector3) : Vector3

/-- outputs: (dist) -/        
@[apex_node "Distance<Vector2>"]
opaque DistanceVector2 (a : Vector2) (b : Vector2) : Float

/-- outputs: (dist) -/        
@[apex_node "Distance<Vector3>"]
opaque DistanceVector3 (a : Vector3) (b : Vector3) : Float

/-- outputs: (dist) -/        
@[apex_node "Distance<Vector4>"]
opaque DistanceVector4 (a : Vector4) (b : Vector4) : Float

/-- outputs: (result) -/        
@[apex_node "Divide<Float,Int>"]
opaque DivideFloatInt (a : Float) {numb: Nat} (b : VariadicArg Int numb) : Float

/-- outputs: (result) -/        
@[apex_node "Divide<Float>"]
opaque DivideFloat (a : Float) {numb: Nat} (b : VariadicArg Float numb) : Float

/-- outputs: (result) -/        
@[apex_node "Divide<Int,Float>"]
opaque DivideIntFloat (a : Int) {numb: Nat} (b : VariadicArg Float numb) : Int

/-- outputs: (result) -/        
@[apex_node "Divide<Int>"]
opaque DivideInt (a : Int) {numb: Nat} (b : VariadicArg Int numb) : Int

/-- outputs: (result) -/        
@[apex_node "Divide<Vector3,Float>"]
opaque DivideVector3Float (a : Vector3) {numb: Nat} (b : VariadicArg Float numb) : Vector3

/-- outputs: (result) -/        
@[apex_node "Divide<Vector3,Int>"]
opaque DivideVector3Int (a : Vector3) {numb: Nat} (b : VariadicArg Int numb) : Vector3

/-- outputs: (result) -/        
@[apex_node "Divide<Vector3>"]
opaque DivideVector3 (a : Vector3) {numb: Nat} (b : VariadicArg Vector3 numb) : Vector3

/-- outputs: (dot) -/        
@[apex_node "DotProduct<Vector2>"]
opaque DotProductVector2 (a : Vector2) (b : Vector2) : Float

/-- outputs: (dot) -/        
@[apex_node "DotProduct<Vector3>"]
opaque DotProductVector3 (a : Vector3) (b : Vector3) : Float

/-- outputs: (dot) -/        
@[apex_node "DotProduct<Vector4>"]
opaque DotProductVector4 (a : Vector4) (b : Vector4) : Float

/-- outputs: (hitpos,delta,hit) -/        
@[apex_node "DragPlane"]
opaque DragPlane (startpos : Vector3) (raypos : Vector3) (raydir : Vector3) (planeorigin : Vector3) (planenormal : Vector3) : Vector3×Vector3×Bool

/-- outputs: (result) -/        
@[apex_node "Efit"]
opaque Efit (val : Float) (srcmin : Float) (srcmax : Float) (destmin : Float) (destmax : Float) : Float

/-- outputs: (result) -/        
@[apex_node "Equals<ApexNodeID>"]
opaque EqualsApexNodeID (a : ApexNodeID) (b : ApexNodeID) : Bool

/-- outputs: (result) -/        
@[apex_node "Equals<ApexPortID>"]
opaque EqualsApexPortID (a : ApexPortID) (b : ApexPortID) : Bool

/-- outputs: (result) -/        
@[apex_node "Equals<Bool>"]
opaque EqualsBool (a : Bool) (b : Bool) : Bool

/-- outputs: (result) -/        
@[apex_node "Equals<Float>"]
opaque EqualsFloat (a : Float) (b : Float) : Bool

/-- outputs: (result) -/        
@[apex_node "Equals<Int>"]
opaque EqualsInt (a : Int) (b : Int) : Bool

/-- outputs: (result) -/        
@[apex_node "Equals<String>"]
opaque EqualsString (a : String) (b : String) : Bool

/-- outputs: (result) -/        
@[apex_node "Equals<Vector2>"]
opaque EqualsVector2 (a : Vector2) (b : Vector2) : Bool

/-- outputs: (result) -/        
@[apex_node "Equals<Vector3>"]
opaque EqualsVector3 (a : Vector3) (b : Vector3) : Bool

/-- outputs: (result) -/        
@[apex_node "Equals<Vector4>"]
opaque EqualsVector4 (a : Vector4) (b : Vector4) : Bool

/-- outputs: (result) -/        
@[apex_node "Exponent"]
opaque Exponent (val : Float) (exponent : Float) : Float

-- special function not supported yet
-- opaque ExtractCharacterGraph (input : String) (find : String) (replace : String) (input : ApexGraphHandle) (...

-- special function not supported yet
-- opaque FindCharacterElement (geo0 : Geometry) (pattern : String) (unpack : Int) (invert : Int) (formatstring...

-- special function not supported yet
-- opaque FindOrCreateCharacterGraph (parm : Int) (input : String) (find : String) (replace : String) (formatst...

-- special function not supported yet
-- opaque FindPackedGeo (input : String) (find : String) (replace : String) (geo : Geometry) (attribname : Stri...

/-- outputs: (result) -/        
@[apex_node "Fit"]
opaque Fit (val : Float) (srcmin : Float) (srcmax : Float) (destmin : Float) (destmax : Float) : Float

/-- outputs: (result) -/        
@[apex_node "Fit01"]
opaque Fit01 (val : Float) (destmin : Float) (destmax : Float) : Float

/-- outputs: (result) -/        
@[apex_node "Fit10"]
opaque Fit10 (val : Float) (destmin : Float) (destmax : Float) : Float

/-- outputs: (result) -/        
@[apex_node "Fit11"]
opaque Fit11 (val : Float) (destmin : Float) (destmax : Float) : Float

/-- outputs: (vector) -/        
@[apex_node "FloatToVector2"]
opaque FloatToVector2 (x : Float) (y : Float) : Vector2

/-- outputs: (vector) -/        
@[apex_node "FloatToVector3"]
opaque FloatToVector3 (x : Float) (y : Float) (z : Float) : Vector3

/-- outputs: (vector) -/        
@[apex_node "FloatToVector4"]
opaque FloatToVector4 (x : Float) (y : Float) (z : Float) (w : Float) : Vector4

/-- outputs: (result) -/        
@[apex_node "Floor"]
opaque Floor (val : Float) : Float

-- special function not supported yet
-- opaque ForBegin (iterations : Int) (__spare__ : undefined) : undefined×Int×undefined...

-- special function not supported yet
-- opaque ForEachBegin<ApexNodeID> (array : ApexNodeIDArray) (__spare__ : undefined) : undefined×ApexNodeIDArra...

-- special function not supported yet
-- opaque ForEachBegin<ApexPortID> (array : ApexPortIDArray) (__spare__ : undefined) : undefined×ApexPortIDArra...

-- special function not supported yet
-- opaque ForEachBegin<Bool> (array : BoolArray) (__spare__ : undefined) : undefined×BoolArray×Bool×Int×undefin...

-- special function not supported yet
-- opaque ForEachBegin<Dict> (array : DictArray) (__spare__ : undefined) : undefined×DictArray×Dict×Int×undefin...

-- special function not supported yet
-- opaque ForEachBegin<Float> (array : FloatArray) (__spare__ : undefined) : undefined×FloatArray×Float×Int×und...

-- special function not supported yet
-- opaque ForEachBegin<Geometry> (array : GeometryArray) (__spare__ : undefined) : undefined×GeometryArray×Geom...

-- special function not supported yet
-- opaque ForEachBegin<Int> (array : IntArray) (__spare__ : undefined) : undefined×IntArray×Int×Int×undefined...

-- special function not supported yet
-- opaque ForEachBegin<Matrix3> (array : Matrix3Array) (__spare__ : undefined) : undefined×Matrix3Array×Matrix3...

-- special function not supported yet
-- opaque ForEachBegin<Matrix4> (array : Matrix4Array) (__spare__ : undefined) : undefined×Matrix4Array×Matrix4...

-- special function not supported yet
-- opaque ForEachBegin<String> (array : StringArray) (__spare__ : undefined) : undefined×StringArray×String×Int...

-- special function not supported yet
-- opaque ForEachBegin<Vector2> (array : Vector2Array) (__spare__ : undefined) : undefined×Vector2Array×Vector2...

-- special function not supported yet
-- opaque ForEachBegin<Vector3> (array : Vector3Array) (__spare__ : undefined) : undefined×Vector3Array×Vector3...

-- special function not supported yet
-- opaque ForEachBegin<Vector4> (array : Vector4Array) (__spare__ : undefined) : undefined×Vector4Array×Vector4...

-- special function not supported yet
-- opaque ForEachEnd<ApexNodeID> (scope : undefined) (array : ApexNodeIDArray) (value : ApexNodeID) (__spare__ ...

-- special function not supported yet
-- opaque ForEachEnd<ApexPortID> (scope : undefined) (array : ApexPortIDArray) (value : ApexPortID) (__spare__ ...

-- special function not supported yet
-- opaque ForEachEnd<Bool> (scope : undefined) (array : BoolArray) (value : Bool) (__spare__ : undefined) : Boo...

-- special function not supported yet
-- opaque ForEachEnd<Dict> (scope : undefined) (array : DictArray) (value : Dict) (__spare__ : undefined) : Dic...

-- special function not supported yet
-- opaque ForEachEnd<Float> (scope : undefined) (array : FloatArray) (value : Float) (__spare__ : undefined) : ...

-- special function not supported yet
-- opaque ForEachEnd<Geometry> (scope : undefined) (array : GeometryArray) (value : Geometry) (__spare__ : unde...

-- special function not supported yet
-- opaque ForEachEnd<Int> (scope : undefined) (array : IntArray) (value : Int) (__spare__ : undefined) : IntArr...

-- special function not supported yet
-- opaque ForEachEnd<Matrix3> (scope : undefined) (array : Matrix3Array) (value : Matrix3) (__spare__ : undefin...

-- special function not supported yet
-- opaque ForEachEnd<Matrix4> (scope : undefined) (array : Matrix4Array) (value : Matrix4) (__spare__ : undefin...

-- special function not supported yet
-- opaque ForEachEnd<String> (scope : undefined) (array : StringArray) (value : String) (__spare__ : undefined)...

-- special function not supported yet
-- opaque ForEachEnd<Vector2> (scope : undefined) (array : Vector2Array) (value : Vector2) (__spare__ : undefin...

-- special function not supported yet
-- opaque ForEachEnd<Vector3> (scope : undefined) (array : Vector3Array) (value : Vector3) (__spare__ : undefin...

-- special function not supported yet
-- opaque ForEachEnd<Vector4> (scope : undefined) (array : Vector4Array) (value : Vector4) (__spare__ : undefin...

-- special function not supported yet
-- opaque ForEnd (scope : undefined) (__spare__ : undefined) : undefined...

/-- outputs: (result) -/        
@[apex_node "Frac"]
opaque Frac (val : Float) : Float

/-- outputs: (cartesian) -/        
@[apex_node "GeodeticToCartesian"]
opaque GeodeticToCartesian (geodetic : Vector3) : Vector3

/-- outputs: (result) -/        
@[apex_node "GetComponent<Matrix3>" has_rundata]
opaque GetComponentMatrix3 (matrix : Matrix3) (row : Int) (col : Int) : Float

/-- outputs: (result) -/        
@[apex_node "GetComponent<Matrix4>" has_rundata]
opaque GetComponentMatrix4 (matrix : Matrix4) (row : Int) (col : Int) : Float

/-- outputs: (result) -/        
@[apex_node "GetComponent<Vector2>" has_rundata]
opaque GetComponentVector2 (vector : Vector2) (index : Int) : Float

/-- outputs: (result) -/        
@[apex_node "GetComponent<Vector3>" has_rundata]
opaque GetComponentVector3 (vector : Vector3) (index : Int) : Float

/-- outputs: (result) -/        
@[apex_node "GetComponent<Vector4>" has_rundata]
opaque GetComponentVector4 (vector : Vector4) (index : Int) : Float

/-- outputs: (result) -/        
@[apex_node "GreaterThan<Bool>"]
opaque GreaterThanBool (a : Bool) (b : Bool) : Bool

/-- outputs: (result) -/        
@[apex_node "GreaterThan<Float>"]
opaque GreaterThanFloat (a : Float) (b : Float) : Bool

/-- outputs: (result) -/        
@[apex_node "GreaterThan<Int>"]
opaque GreaterThanInt (a : Int) (b : Int) : Bool

/-- outputs: (result) -/        
@[apex_node "GreaterThan<Vector2>"]
opaque GreaterThanVector2 (a : Vector2) (b : Vector2) : Bool

/-- outputs: (result) -/        
@[apex_node "GreaterThan<Vector3>"]
opaque GreaterThanVector3 (a : Vector3) (b : Vector3) : Bool

/-- outputs: (result) -/        
@[apex_node "GreaterThan<Vector4>"]
opaque GreaterThanVector4 (a : Vector4) (b : Vector4) : Bool

/-- outputs: (result) -/        
@[apex_node "GreaterThanOrEqual<Bool>"]
opaque GreaterThanOrEqualBool (a : Bool) (b : Bool) : Bool

/-- outputs: (result) -/        
@[apex_node "GreaterThanOrEqual<Float>"]
opaque GreaterThanOrEqualFloat (a : Float) (b : Float) : Bool

/-- outputs: (result) -/        
@[apex_node "GreaterThanOrEqual<Int>"]
opaque GreaterThanOrEqualInt (a : Int) (b : Int) : Bool

/-- outputs: (result) -/        
@[apex_node "GreaterThanOrEqual<Vector2>"]
opaque GreaterThanOrEqualVector2 (a : Vector2) (b : Vector2) : Bool

/-- outputs: (result) -/        
@[apex_node "GreaterThanOrEqual<Vector3>"]
opaque GreaterThanOrEqualVector3 (a : Vector3) (b : Vector3) : Bool

/-- outputs: (result) -/        
@[apex_node "GreaterThanOrEqual<Vector4>"]
opaque GreaterThanOrEqualVector4 (a : Vector4) (b : Vector4) : Bool

-- special function not supported yet
-- opaque IfBegin (condition : Bool) (__spare__ : undefined) : undefined×undefined...

-- special function not supported yet
-- opaque IfEnd (scope : undefined) (__spare__ : undefined) : undefined...

-- special function not supported yet
-- opaque IntBitMaskToBool::2.0 (entries : Int) : VariadicArg Bool...

/-- outputs: (result) -/        
@[apex_node "Invert<Matrix3>"]
opaque InvertMatrix3 (a : Matrix3) : Matrix3

/-- outputs: (result) -/        
@[apex_node "Invert<Matrix4>"]
opaque InvertMatrix4 (a : Matrix4) : Matrix4

/-- outputs: (length) -/        
@[apex_node "Length<Vector2>"]
opaque LengthVector2 (vec : Vector2) : Float

/-- outputs: (length) -/        
@[apex_node "Length<Vector3>"]
opaque LengthVector3 (vec : Vector3) : Float

/-- outputs: (length) -/        
@[apex_node "Length<Vector4>"]
opaque LengthVector4 (vec : Vector4) : Float

/-- outputs: (result) -/        
@[apex_node "Lerp<Float>"]
opaque LerpFloat (a : Float) (b : Float) (bias : Float) : Float

/-- outputs: (result) -/        
@[apex_node "Lerp<Matrix3>"]
opaque LerpMatrix3 (a : Matrix3) (b : Matrix3) (bias : Float) : Matrix3

/-- outputs: (result) -/        
@[apex_node "Lerp<Matrix4>"]
opaque LerpMatrix4 (a : Matrix4) (b : Matrix4) (bias : Float) : Matrix4

/-- outputs: (result) -/        
@[apex_node "Lerp<Vector2>"]
opaque LerpVector2 (a : Vector2) (b : Vector2) (bias : Float) : Vector2

/-- outputs: (result) -/        
@[apex_node "Lerp<Vector3>"]
opaque LerpVector3 (a : Vector3) (b : Vector3) (bias : Float) : Vector3

/-- outputs: (result) -/        
@[apex_node "Lerp<Vector4>"]
opaque LerpVector4 (a : Vector4) (b : Vector4) (bias : Float) : Vector4

/-- outputs: (result) -/        
@[apex_node "LessThan<Bool>"]
opaque LessThanBool (a : Bool) (b : Bool) : Bool

/-- outputs: (result) -/        
@[apex_node "LessThan<Float>"]
opaque LessThanFloat (a : Float) (b : Float) : Bool

/-- outputs: (result) -/        
@[apex_node "LessThan<Int>"]
opaque LessThanInt (a : Int) (b : Int) : Bool

/-- outputs: (result) -/        
@[apex_node "LessThan<Vector2>"]
opaque LessThanVector2 (a : Vector2) (b : Vector2) : Bool

/-- outputs: (result) -/        
@[apex_node "LessThan<Vector3>"]
opaque LessThanVector3 (a : Vector3) (b : Vector3) : Bool

/-- outputs: (result) -/        
@[apex_node "LessThan<Vector4>"]
opaque LessThanVector4 (a : Vector4) (b : Vector4) : Bool

/-- outputs: (result) -/        
@[apex_node "LessThanOrEqual<Bool>"]
opaque LessThanOrEqualBool (a : Bool) (b : Bool) : Bool

/-- outputs: (result) -/        
@[apex_node "LessThanOrEqual<Float>"]
opaque LessThanOrEqualFloat (a : Float) (b : Float) : Bool

/-- outputs: (result) -/        
@[apex_node "LessThanOrEqual<Int>"]
opaque LessThanOrEqualInt (a : Int) (b : Int) : Bool

/-- outputs: (result) -/        
@[apex_node "LessThanOrEqual<Vector2>"]
opaque LessThanOrEqualVector2 (a : Vector2) (b : Vector2) : Bool

/-- outputs: (result) -/        
@[apex_node "LessThanOrEqual<Vector3>"]
opaque LessThanOrEqualVector3 (a : Vector3) (b : Vector3) : Bool

/-- outputs: (result) -/        
@[apex_node "LessThanOrEqual<Vector4>"]
opaque LessThanOrEqualVector4 (a : Vector4) (b : Vector4) : Bool

-- special function not supported yet
-- opaque Log (args0 : undefined) (severity : Int) (formatstring : String) {numargs: Nat} (args : VariadicArg U...

/-- outputs: (row1,row2,row3) -/        
@[apex_node "Matrix3ToVector3"]
opaque Matrix3ToVector3 (m : Matrix3) : Vector3×Vector3×Vector3

/-- outputs: (row1,row2,row3,row4) -/        
@[apex_node "Matrix4ToVector4"]
opaque Matrix4ToVector4 (m : Matrix4) : Vector4×Vector4×Vector4×Vector4

/-- outputs: (result) -/        
@[apex_node "Max<Float>"]
opaque MaxFloat (a : Float) {numb: Nat} (b : VariadicArg Float numb) : Float

/-- outputs: (result) -/        
@[apex_node "Max<Int>"]
opaque MaxInt (a : Int) {numb: Nat} (b : VariadicArg Int numb) : Int

/-- outputs: (result) -/        
@[apex_node "Min<Float>"]
opaque MinFloat (a : Float) {numb: Nat} (b : VariadicArg Float numb) : Float

/-- outputs: (result) -/        
@[apex_node "Min<Int>"]
opaque MinInt (a : Int) {numb: Nat} (b : VariadicArg Int numb) : Int

/-- outputs: (result) -/        
@[apex_node "Modulo"]
opaque Modulo (a : Int) (b : Int) : Int

/-- outputs: (result) -/        
@[apex_node "Multiply<Float,Int>"]
opaque MultiplyFloatInt (a : Float) {numb: Nat} (b : VariadicArg Int numb) : Float

/-- outputs: (result) -/        
@[apex_node "Multiply<Float>"]
opaque MultiplyFloat (a : Float) {numb: Nat} (b : VariadicArg Float numb) : Float

/-- outputs: (result) -/        
@[apex_node "Multiply<Int,Float>"]
opaque MultiplyIntFloat (a : Int) {numb: Nat} (b : VariadicArg Float numb) : Int

/-- outputs: (result) -/        
@[apex_node "Multiply<Int>"]
opaque MultiplyInt (a : Int) {numb: Nat} (b : VariadicArg Int numb) : Int

/-- outputs: (result) -/        
@[apex_node "Multiply<Matrix3,Float>"]
opaque MultiplyMatrix3Float (a : Matrix3) {numb: Nat} (b : VariadicArg Float numb) : Matrix3

/-- outputs: (result) -/        
@[apex_node "Multiply<Matrix3>"]
opaque MultiplyMatrix3 (a : Matrix3) {numb: Nat} (b : VariadicArg Matrix3 numb) : Matrix3

/-- outputs: (result) -/        
@[apex_node "Multiply<Matrix4,Float>"]
opaque MultiplyMatrix4Float (a : Matrix4) {numb: Nat} (b : VariadicArg Float numb) : Matrix4

/-- outputs: (result) -/        
@[apex_node "Multiply<Matrix4,Matrix3>"]
opaque MultiplyMatrix4Matrix3 (a : Matrix4) {numb: Nat} (b : VariadicArg Matrix3 numb) : Matrix4

/-- outputs: (result) -/        
@[apex_node "Multiply<Matrix4>"]
opaque MultiplyMatrix4 (a : Matrix4) {numb: Nat} (b : VariadicArg Matrix4 numb) : Matrix4

/-- outputs: (result) -/        
@[apex_node "Multiply<Vector2,Float>"]
opaque MultiplyVector2Float (a : Vector2) {numb: Nat} (b : VariadicArg Float numb) : Vector2

/-- outputs: (result) -/        
@[apex_node "Multiply<Vector3,Float>"]
opaque MultiplyVector3Float (a : Vector3) {numb: Nat} (b : VariadicArg Float numb) : Vector3

/-- outputs: (result) -/        
@[apex_node "Multiply<Vector3,Matrix3>"]
opaque MultiplyVector3Matrix3 (a : Vector3) {numb: Nat} (b : VariadicArg Matrix3 numb) : Vector3

/-- outputs: (result) -/        
@[apex_node "Multiply<Vector3,Matrix4>"]
opaque MultiplyVector3Matrix4 (a : Vector3) {numb: Nat} (b : VariadicArg Matrix4 numb) : Vector3

/-- outputs: (result) -/        
@[apex_node "Multiply<Vector3>"]
opaque MultiplyVector3 (a : Vector3) {numb: Nat} (b : VariadicArg Vector3 numb) : Vector3

/-- outputs: (result) -/        
@[apex_node "Multiply<Vector4,Float>"]
opaque MultiplyVector4Float (a : Vector4) {numb: Nat} (b : VariadicArg Float numb) : Vector4

/-- outputs: (result) -/        
@[apex_node "Multiply<Vector4,Matrix4>"]
opaque MultiplyVector4Matrix4 (a : Vector4) {numb: Nat} (b : VariadicArg Matrix4 numb) : Vector4

/-- outputs: (result) -/        
@[apex_node "NLerp<Vector4>"]
opaque NLerpVector4 (a : Vector4) (b : Vector4) (bias : Float) : Vector4

/-- outputs: (result) -/        
@[apex_node "Negate<Float>"]
opaque NegateFloat (val : Float) : Float

/-- outputs: (result) -/        
@[apex_node "Negate<Int>"]
opaque NegateInt (val : Int) : Int

/-- outputs: (result) -/        
@[apex_node "Negate<Vector2>"]
opaque NegateVector2 (val : Vector2) : Vector2

/-- outputs: (result) -/        
@[apex_node "Negate<Vector3>"]
opaque NegateVector3 (val : Vector3) : Vector3

/-- outputs: (result) -/        
@[apex_node "Noise<Float,Float>"]
opaque NoiseFloatFloat (in' : Float) : Float

/-- outputs: (normalized,length) -/        
@[apex_node "Normalize<Vector2>"]
opaque NormalizeVector2 (vec : Vector2) : Vector2×Float

/-- outputs: (normalized,length) -/        
@[apex_node "Normalize<Vector3>"]
opaque NormalizeVector3 (vec : Vector3) : Vector3×Float

/-- outputs: (normalized,length) -/        
@[apex_node "Normalize<Vector4>"]
opaque NormalizeVector4 (vec : Vector4) : Vector4×Float

/-- outputs: (result) -/        
@[apex_node "Not"]
opaque Not (input : Bool) : Bool

/-- outputs: (value) -/        
@[apex_node "Null<AnimChannel>"]
opaque NullAnimChannel (value : AnimChannel) : AnimChannel

/-- outputs: (value) -/        
@[apex_node "Null<AnimChannelCollection>"]
opaque NullAnimChannelCollection (value : AnimChannelCollection) : AnimChannelCollection

/-- outputs: (value) -/        
@[apex_node "Null<AnimStack>"]
opaque NullAnimStack (value : AnimStack) : AnimStack

/-- outputs: (value) -/        
@[apex_node "Null<ApexGraphHandle>"]
opaque NullApexGraphHandle (value : ApexGraphHandle) : ApexGraphHandle

/-- outputs: (value) -/        
@[apex_node "Null<ApexNodeID>"]
opaque NullApexNodeID (value : ApexNodeID) : ApexNodeID

/-- outputs: (value) -/        
@[apex_node "Null<ApexNodeIDArray>"]
opaque NullApexNodeIDArray (value : ApexNodeIDArray) : ApexNodeIDArray

/-- outputs: (value) -/        
@[apex_node "Null<ApexPortID>"]
opaque NullApexPortID (value : ApexPortID) : ApexPortID

/-- outputs: (value) -/        
@[apex_node "Null<ApexPortIDArray>"]
opaque NullApexPortIDArray (value : ApexPortIDArray) : ApexPortIDArray

/-- outputs: (value) -/        
@[apex_node "Null<Bool>"]
opaque NullBool (value : Bool) : Bool

/-- outputs: (value) -/        
@[apex_node "Null<BoolArray>"]
opaque NullBoolArray (value : BoolArray) : BoolArray

/-- outputs: (value) -/        
@[apex_node "Null<ColorRamp>"]
opaque NullColorRamp (value : ColorRamp) : ColorRamp

/-- outputs: (value) -/        
@[apex_node "Null<DataItem>"]
opaque NullDataItem (value : DataItem) : DataItem

/-- outputs: (value) -/        
@[apex_node "Null<Dict>"]
opaque NullDict (value : Dict) : Dict

/-- outputs: (value) -/        
@[apex_node "Null<DictArray>"]
opaque NullDictArray (value : DictArray) : DictArray

/-- outputs: (value) -/        
@[apex_node "Null<DynamicPath>"]
opaque NullDynamicPath (value : DynamicPath) : DynamicPath

/-- outputs: (value) -/        
@[apex_node "Null<DynamicPathArray>"]
opaque NullDynamicPathArray (value : DynamicPathArray) : DynamicPathArray

/-- outputs: (value) -/        
@[apex_node "Null<FBIKSkeleton>"]
opaque NullFBIKSkeleton (value : FBIKSkeleton) : FBIKSkeleton

/-- outputs: (value) -/        
@[apex_node "Null<FBIKSkeletonArray>"]
opaque NullFBIKSkeletonArray (value : FBIKSkeletonArray) : FBIKSkeletonArray

/-- outputs: (value) -/        
@[apex_node "Null<FBIKSolver>"]
opaque NullFBIKSolver (value : FBIKSolver) : FBIKSolver

/-- outputs: (value) -/        
@[apex_node "Null<FBIKSolverArray>"]
opaque NullFBIKSolverArray (value : FBIKSolverArray) : FBIKSolverArray

/-- outputs: (value) -/        
@[apex_node "Null<FBIKTarget>"]
opaque NullFBIKTarget (value : FBIKTarget) : FBIKTarget

/-- outputs: (value) -/        
@[apex_node "Null<FBIKTargetArray>"]
opaque NullFBIKTargetArray (value : FBIKTargetArray) : FBIKTargetArray

/-- outputs: (value) -/        
@[apex_node "Null<Float>"]
opaque NullFloat (value : Float) : Float

/-- outputs: (value) -/        
@[apex_node "Null<FloatArray>"]
opaque NullFloatArray (value : FloatArray) : FloatArray

/-- outputs: (value) -/        
@[apex_node "Null<FloatRamp>"]
opaque NullFloatRamp (value : FloatRamp) : FloatRamp

/-- outputs: (value) -/        
@[apex_node "Null<Geometry>"]
opaque NullGeometry (value : Geometry) : Geometry

/-- outputs: (value) -/        
@[apex_node "Null<GeometryArray>"]
opaque NullGeometryArray (value : GeometryArray) : GeometryArray

/-- outputs: (value) -/        
@[apex_node "Null<Int>"]
opaque NullInt (value : Int) : Int

/-- outputs: (value) -/        
@[apex_node "Null<IntArray>"]
opaque NullIntArray (value : IntArray) : IntArray

/-- outputs: (value) -/        
@[apex_node "Null<Matrix3>"]
opaque NullMatrix3 (value : Matrix3) : Matrix3

/-- outputs: (value) -/        
@[apex_node "Null<Matrix3Array>"]
opaque NullMatrix3Array (value : Matrix3Array) : Matrix3Array

/-- outputs: (value) -/        
@[apex_node "Null<Matrix4>"]
opaque NullMatrix4 (value : Matrix4) : Matrix4

/-- outputs: (value) -/        
@[apex_node "Null<Matrix4Array>"]
opaque NullMatrix4Array (value : Matrix4Array) : Matrix4Array

/-- outputs: (value) -/        
@[apex_node "Null<SimRootDataId>"]
opaque NullSimRootDataId (value : SimRootDataId) : SimRootDataId

/-- outputs: (value) -/        
@[apex_node "Null<SimRootDataIdArray>"]
opaque NullSimRootDataIdArray (value : SimRootDataIdArray) : SimRootDataIdArray

/-- outputs: (value) -/        
@[apex_node "Null<String>"]
opaque NullString (value : String) : String

/-- outputs: (value) -/        
@[apex_node "Null<StringArray>"]
opaque NullStringArray (value : StringArray) : StringArray

/-- outputs: (value) -/        
@[apex_node "Null<Vector2>"]
opaque NullVector2 (value : Vector2) : Vector2

/-- outputs: (value) -/        
@[apex_node "Null<Vector2Array>"]
opaque NullVector2Array (value : Vector2Array) : Vector2Array

/-- outputs: (value) -/        
@[apex_node "Null<Vector3>"]
opaque NullVector3 (value : Vector3) : Vector3

/-- outputs: (value) -/        
@[apex_node "Null<Vector3Array>"]
opaque NullVector3Array (value : Vector3Array) : Vector3Array

/-- outputs: (value) -/        
@[apex_node "Null<Vector4>"]
opaque NullVector4 (value : Vector4) : Vector4

/-- outputs: (value) -/        
@[apex_node "Null<Vector4Array>"]
opaque NullVector4Array (value : Vector4Array) : Vector4Array

-- special function not supported yet
-- opaque Null<geo::IntersectCache> (value : geo::IntersectCache) : geo::IntersectCache...

/-- outputs: (result) -/        
@[apex_node "Or"]
opaque Or {numinputs: Nat} (inputs : VariadicArg Bool numinputs) : Bool

/-- outputs: (cartesian) -/        
@[apex_node "PolarToCartesian"]
opaque PolarToCartesian (polar : Vector2) : Vector2

/-- outputs: (result) -/        
@[apex_node "Quantize<Float>"]
opaque QuantizeFloat (val : Float) (step : Float) : Float

/-- outputs: (result) -/        
@[apex_node "Quantize<Vector2>"]
opaque QuantizeVector2 (val : Vector2) (step : Float) : Vector2

/-- outputs: (result) -/        
@[apex_node "Quantize<Vector3>"]
opaque QuantizeVector3 (val : Vector3) (step : Float) : Vector3

/-- outputs: (degrees) -/        
@[apex_node "RadiansToDegrees<Float>"]
opaque RadiansToDegreesFloat (radians : Float) : Float

/-- outputs: (degrees) -/        
@[apex_node "RadiansToDegrees<Vector3>"]
opaque RadiansToDegreesVector3 (radians : Vector3) : Vector3

/-- outputs: (result) -/        
@[apex_node "RampLookup<ColorRamp>"]
opaque RampLookupColorRamp (ramp : ColorRamp) (position : Float) : Vector3

/-- outputs: (result) -/        
@[apex_node "RampLookup<FloatRamp>"]
opaque RampLookupFloatRamp (ramp : FloatRamp) (position : Float) : Float

/-- outputs: (result) -/        
@[apex_node "Round"]
opaque Round (val : Float) (ndigits : Int) : Float

-- special function not supported yet
-- opaque RunVex (snippet : String) {numinputs: Nat} (inputs : VariadicArg Untyped numinputs) : VariadicArg Unt...

/-- outputs: (matrix) -/        
@[apex_node "SetComponent<Matrix3>" has_rundata]
opaque SetComponentMatrix3 (matrix : Matrix3) (value : Float) (row : Int) (col : Int) : Matrix3

/-- outputs: (matrix) -/        
@[apex_node "SetComponent<Matrix4>" has_rundata]
opaque SetComponentMatrix4 (matrix : Matrix4) (value : Float) (row : Int) (col : Int) : Matrix4

/-- outputs: (vector) -/        
@[apex_node "SetComponent<Vector2>" has_rundata]
opaque SetComponentVector2 (vector : Vector2) (value : Float) (index : Int) : Vector2

/-- outputs: (vector) -/        
@[apex_node "SetComponent<Vector3>" has_rundata]
opaque SetComponentVector3 (vector : Vector3) (value : Float) (index : Int) : Vector3

/-- outputs: (vector) -/        
@[apex_node "SetComponent<Vector4>" has_rundata]
opaque SetComponentVector4 (vector : Vector4) (value : Float) (index : Int) : Vector4

/-- outputs: (result) -/        
@[apex_node "Sine"]
opaque Sine (radians : Float) : Float

/-- outputs: (result,vector,m,xform,localxform,pretransform,xform,localxform,pretransform,xform,localxform,pretransform,t,r,s,sh,t,r,s,sh,dist,t,r,s,sh,t,r,s,sh,t,r,s,sh,dist,dist,result,result,result,result,value,value,result,result,result,result,result,result,result,result,result,result,result,result,result,result,out,result,result,result,result,result,out,rootout,midout,tipout,localxform,effectivelocal,xform,localxform,pretransform,xform,localxform,pretransform,localxform,effectivelocal,out,vector,vector,t,r,s,sh,result,localxform,localxform,driver_target,driver_root,rest_root,rest_mid,rest_tip,falloff,driver_twist,axis,rootout,midout,tipout,out) -/        
@[apex_node "SmoothIk"]
opaque SmoothIk (a : Float) {numb: Nat} (b : VariadicArg Float numb) (x : Float) (y : Float) (z : Float) (t : Vector3) (r : Vector3) (s : Vector3) (sh : Vector3) (p : Vector3) (pr : Vector3) (xOrd : Int) (rOrd : Int) (local' : Matrix4) (restlocal : Matrix4) (parent : Matrix4) (parentlocal : Matrix4) (scaleinheritance : Int) (local' : Matrix4) (restlocal : Matrix4) (parent : Matrix4) (parentlocal : Matrix4) (scaleinheritance : Int) (local' : Matrix4) (restlocal : Matrix4) (parent : Matrix4) (parentlocal : Matrix4) (scaleinheritance : Int) (m : Matrix4) (xOrd : Int) (rOrd : Int) (p : Vector3) (pr : Vector3) (m : Matrix4) (xOrd : Int) (rOrd : Int) (p : Vector3) (pr : Vector3) (a : Vector3) (b : Vector3) (m : Matrix4) (xOrd : Int) (rOrd : Int) (p : Vector3) (pr : Vector3) (m : Matrix4) (xOrd : Int) (rOrd : Int) (p : Vector3) (pr : Vector3) (m : Matrix4) (xOrd : Int) (rOrd : Int) (p : Vector3) (pr : Vector3) (a : Vector3) (b : Vector3) (a : Vector3) (b : Vector3) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (val : Float) (exponent : Float) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (parm : Float) (parm : Float) (val : Float) (exponent : Float) (val : Float) (exponent : Float) (val : Float) (exponent : Float) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (val : Float) (exponent : Float) (val : Float) (exponent : Float) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (a : Float) (b : Float) (a : Float) (b : Float) (a : Float) (b : Float) (index : Bool) (val : Float) (exponent : Float) (val : Float) (exponent : Float) (val : Float) (exponent : Float) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (a : Float) (b : Float) (a : Float) (b : Float) (index : Bool) (root : Matrix4) (mid : Matrix4) (tip : Matrix4) (rootdriver : Matrix4) (twist : Matrix4) (goal : Matrix4) (stretch : Bool) (blend : Float) (xform : Matrix4) (parent : Matrix4) (parentlocal : Matrix4) (scaleinheritance : Int) (local' : Matrix4) (restlocal : Matrix4) (parent : Matrix4) (parentlocal : Matrix4) (scaleinheritance : Int) (local' : Matrix4) (restlocal : Matrix4) (parent : Matrix4) (parentlocal : Matrix4) (scaleinheritance : Int) (xform : Matrix4) (parent : Matrix4) (parentlocal : Matrix4) (scaleinheritance : Int) {numinput: Nat} (input : VariadicArg Vector3 numinput) (index : Int) (x : Float) (y : Float) (z : Float) (x : Float) (y : Float) (z : Float) (m : Matrix4) (xOrd : Int) (rOrd : Int) (p : Vector3) (pr : Vector3) {numinputs: Nat} (inputs : VariadicArg Bool numinputs) (restlocal : Matrix4) (t : Vector3) (r : Vector3) (s : Vector3) (sh : Vector3) (p : Vector3) (pr : Vector3) (xord : Int) (rord : Int) (mode : Int) (restlocal : Matrix4) (t : Vector3) (r : Vector3) (s : Vector3) (sh : Vector3) (p : Vector3) (pr : Vector3) (xord : Int) (rord : Int) (mode : Int) (rootout : Matrix4) (midout : Matrix4) (tipout : Matrix4) (b2 : Float) (b2 : Float) (b2 : Float) (b2 : Float) (b2 : Float) (b2 : Float) (b2 : Float) (b2 : Float) (b2 : Float) (b2 : Float) (b2 : Float) (b2 : Float) (input2 : Vector3) (input3 : Vector3) (input4 : Vector3) (out : Vector3) (inputs0 : Bool) (inputs1 : Bool) (driver_target : Matrix4) (driver_root : Matrix4) (rest_root : Matrix4) (rest_mid : Matrix4) (rest_tip : Matrix4) (falloff : Float) (driver_twist : Matrix4) (axis : Int) : Float×Vector3×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Float×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Float×Float×Float×Float×Float×Float×Float×Float×Float×Float×Float×Float×Float×Float×Float×Float×Float×Float×Float×Float×Bool×Bool×Float×Float×Float×Float×Float×Bool×Float×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Bool×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Float×Matrix4×Int×Matrix4×Matrix4×Matrix4×Vector3

/-- outputs: (cartesian) -/        
@[apex_node "SphericalToCartesian"]
opaque SphericalToCartesian (spherical : Vector3) : Vector3

/-- outputs: (result) -/        
@[apex_node "Subtract<Float,Int>"]
opaque SubtractFloatInt (a : Float) {numb: Nat} (b : VariadicArg Int numb) : Float

/-- outputs: (result) -/        
@[apex_node "Subtract<Float>"]
opaque SubtractFloat (a : Float) {numb: Nat} (b : VariadicArg Float numb) : Float

/-- outputs: (result) -/        
@[apex_node "Subtract<Int,Float>"]
opaque SubtractIntFloat (a : Int) {numb: Nat} (b : VariadicArg Float numb) : Int

/-- outputs: (result) -/        
@[apex_node "Subtract<Int>"]
opaque SubtractInt (a : Int) {numb: Nat} (b : VariadicArg Int numb) : Int

/-- outputs: (result) -/        
@[apex_node "Subtract<Matrix3>"]
opaque SubtractMatrix3 (a : Matrix3) {numb: Nat} (b : VariadicArg Matrix3 numb) : Matrix3

/-- outputs: (result) -/        
@[apex_node "Subtract<Matrix4>"]
opaque SubtractMatrix4 (a : Matrix4) {numb: Nat} (b : VariadicArg Matrix4 numb) : Matrix4

/-- outputs: (result) -/        
@[apex_node "Subtract<Vector2>"]
opaque SubtractVector2 (a : Vector2) {numb: Nat} (b : VariadicArg Vector2 numb) : Vector2

/-- outputs: (result) -/        
@[apex_node "Subtract<Vector3,Float>"]
opaque SubtractVector3Float (a : Vector3) {numb: Nat} (b : VariadicArg Float numb) : Vector3

/-- outputs: (result) -/        
@[apex_node "Subtract<Vector3,Int>"]
opaque SubtractVector3Int (a : Vector3) {numb: Nat} (b : VariadicArg Int numb) : Vector3

/-- outputs: (result) -/        
@[apex_node "Subtract<Vector3>"]
opaque SubtractVector3 (a : Vector3) {numb: Nat} (b : VariadicArg Vector3 numb) : Vector3

/-- outputs: (out) -/        
@[apex_node "Switch<AnimChannel>"]
opaque SwitchAnimChannel {numinput: Nat} (input : VariadicArg AnimChannel numinput) (index : Int) : AnimChannel

/-- outputs: (out) -/        
@[apex_node "Switch<AnimChannelCollection>"]
opaque SwitchAnimChannelCollection {numinput: Nat} (input : VariadicArg AnimChannelCollection numinput) (index : Int) : AnimChannelCollection

/-- outputs: (out) -/        
@[apex_node "Switch<AnimStack>"]
opaque SwitchAnimStack {numinput: Nat} (input : VariadicArg AnimStack numinput) (index : Int) : AnimStack

/-- outputs: (out) -/        
@[apex_node "Switch<ApexNodeID>"]
opaque SwitchApexNodeID {numinput: Nat} (input : VariadicArg ApexNodeID numinput) (index : Int) : ApexNodeID

/-- outputs: (out) -/        
@[apex_node "Switch<ApexNodeIDArray>"]
opaque SwitchApexNodeIDArray {numinput: Nat} (input : VariadicArg ApexNodeIDArray numinput) (index : Int) : ApexNodeIDArray

/-- outputs: (out) -/        
@[apex_node "Switch<ApexPortID>"]
opaque SwitchApexPortID {numinput: Nat} (input : VariadicArg ApexPortID numinput) (index : Int) : ApexPortID

/-- outputs: (out) -/        
@[apex_node "Switch<ApexPortIDArray>"]
opaque SwitchApexPortIDArray {numinput: Nat} (input : VariadicArg ApexPortIDArray numinput) (index : Int) : ApexPortIDArray

/-- outputs: (out) -/        
@[apex_node "Switch<Bool>"]
opaque SwitchBool {numinput: Nat} (input : VariadicArg Bool numinput) (index : Int) : Bool

/-- outputs: (out) -/        
@[apex_node "Switch<BoolArray>"]
opaque SwitchBoolArray {numinput: Nat} (input : VariadicArg BoolArray numinput) (index : Int) : BoolArray

/-- outputs: (out) -/        
@[apex_node "Switch<ColorRamp>"]
opaque SwitchColorRamp {numinput: Nat} (input : VariadicArg ColorRamp numinput) (index : Int) : ColorRamp

/-- outputs: (out) -/        
@[apex_node "Switch<DataItem>"]
opaque SwitchDataItem {numinput: Nat} (input : VariadicArg DataItem numinput) (index : Int) : DataItem

/-- outputs: (out) -/        
@[apex_node "Switch<Dict>"]
opaque SwitchDict {numinput: Nat} (input : VariadicArg Dict numinput) (index : Int) : Dict

/-- outputs: (out) -/        
@[apex_node "Switch<DictArray>"]
opaque SwitchDictArray {numinput: Nat} (input : VariadicArg DictArray numinput) (index : Int) : DictArray

/-- outputs: (out) -/        
@[apex_node "Switch<DynamicPath>"]
opaque SwitchDynamicPath {numinput: Nat} (input : VariadicArg DynamicPath numinput) (index : Int) : DynamicPath

/-- outputs: (out) -/        
@[apex_node "Switch<DynamicPathArray>"]
opaque SwitchDynamicPathArray {numinput: Nat} (input : VariadicArg DynamicPathArray numinput) (index : Int) : DynamicPathArray

/-- outputs: (out) -/        
@[apex_node "Switch<FBIKSkeleton>"]
opaque SwitchFBIKSkeleton {numinput: Nat} (input : VariadicArg FBIKSkeleton numinput) (index : Int) : FBIKSkeleton

/-- outputs: (out) -/        
@[apex_node "Switch<FBIKSkeletonArray>"]
opaque SwitchFBIKSkeletonArray {numinput: Nat} (input : VariadicArg FBIKSkeletonArray numinput) (index : Int) : FBIKSkeletonArray

/-- outputs: (out) -/        
@[apex_node "Switch<FBIKSolver>"]
opaque SwitchFBIKSolver {numinput: Nat} (input : VariadicArg FBIKSolver numinput) (index : Int) : FBIKSolver

/-- outputs: (out) -/        
@[apex_node "Switch<FBIKSolverArray>"]
opaque SwitchFBIKSolverArray {numinput: Nat} (input : VariadicArg FBIKSolverArray numinput) (index : Int) : FBIKSolverArray

/-- outputs: (out) -/        
@[apex_node "Switch<FBIKTarget>"]
opaque SwitchFBIKTarget {numinput: Nat} (input : VariadicArg FBIKTarget numinput) (index : Int) : FBIKTarget

/-- outputs: (out) -/        
@[apex_node "Switch<FBIKTargetArray>"]
opaque SwitchFBIKTargetArray {numinput: Nat} (input : VariadicArg FBIKTargetArray numinput) (index : Int) : FBIKTargetArray

/-- outputs: (out) -/        
@[apex_node "Switch<Float>"]
opaque SwitchFloat {numinput: Nat} (input : VariadicArg Float numinput) (index : Int) : Float

/-- outputs: (out) -/        
@[apex_node "Switch<FloatArray>"]
opaque SwitchFloatArray {numinput: Nat} (input : VariadicArg FloatArray numinput) (index : Int) : FloatArray

/-- outputs: (out) -/        
@[apex_node "Switch<FloatRamp>"]
opaque SwitchFloatRamp {numinput: Nat} (input : VariadicArg FloatRamp numinput) (index : Int) : FloatRamp

/-- outputs: (out) -/        
@[apex_node "Switch<Geometry>"]
opaque SwitchGeometry {numinput: Nat} (input : VariadicArg Geometry numinput) (index : Int) : Geometry

/-- outputs: (out) -/        
@[apex_node "Switch<GeometryArray>"]
opaque SwitchGeometryArray {numinput: Nat} (input : VariadicArg GeometryArray numinput) (index : Int) : GeometryArray

/-- outputs: (out) -/        
@[apex_node "Switch<Int>"]
opaque SwitchInt {numinput: Nat} (input : VariadicArg Int numinput) (index : Int) : Int

/-- outputs: (out) -/        
@[apex_node "Switch<IntArray>"]
opaque SwitchIntArray {numinput: Nat} (input : VariadicArg IntArray numinput) (index : Int) : IntArray

/-- outputs: (out) -/        
@[apex_node "Switch<Matrix3>"]
opaque SwitchMatrix3 {numinput: Nat} (input : VariadicArg Matrix3 numinput) (index : Int) : Matrix3

/-- outputs: (out) -/        
@[apex_node "Switch<Matrix3Array>"]
opaque SwitchMatrix3Array {numinput: Nat} (input : VariadicArg Matrix3Array numinput) (index : Int) : Matrix3Array

/-- outputs: (out) -/        
@[apex_node "Switch<Matrix4>"]
opaque SwitchMatrix4 {numinput: Nat} (input : VariadicArg Matrix4 numinput) (index : Int) : Matrix4

/-- outputs: (out) -/        
@[apex_node "Switch<Matrix4Array>"]
opaque SwitchMatrix4Array {numinput: Nat} (input : VariadicArg Matrix4Array numinput) (index : Int) : Matrix4Array

/-- outputs: (out) -/        
@[apex_node "Switch<SimRootDataId>"]
opaque SwitchSimRootDataId {numinput: Nat} (input : VariadicArg SimRootDataId numinput) (index : Int) : SimRootDataId

/-- outputs: (out) -/        
@[apex_node "Switch<SimRootDataIdArray>"]
opaque SwitchSimRootDataIdArray {numinput: Nat} (input : VariadicArg SimRootDataIdArray numinput) (index : Int) : SimRootDataIdArray

/-- outputs: (out) -/        
@[apex_node "Switch<String>"]
opaque SwitchString {numinput: Nat} (input : VariadicArg String numinput) (index : Int) : String

/-- outputs: (out) -/        
@[apex_node "Switch<StringArray>"]
opaque SwitchStringArray {numinput: Nat} (input : VariadicArg StringArray numinput) (index : Int) : StringArray

/-- outputs: (out) -/        
@[apex_node "Switch<Vector2>"]
opaque SwitchVector2 {numinput: Nat} (input : VariadicArg Vector2 numinput) (index : Int) : Vector2

/-- outputs: (out) -/        
@[apex_node "Switch<Vector2Array>"]
opaque SwitchVector2Array {numinput: Nat} (input : VariadicArg Vector2Array numinput) (index : Int) : Vector2Array

/-- outputs: (out) -/        
@[apex_node "Switch<Vector3>"]
opaque SwitchVector3 {numinput: Nat} (input : VariadicArg Vector3 numinput) (index : Int) : Vector3

/-- outputs: (out) -/        
@[apex_node "Switch<Vector3Array>"]
opaque SwitchVector3Array {numinput: Nat} (input : VariadicArg Vector3Array numinput) (index : Int) : Vector3Array

/-- outputs: (out) -/        
@[apex_node "Switch<Vector4>"]
opaque SwitchVector4 {numinput: Nat} (input : VariadicArg Vector4 numinput) (index : Int) : Vector4

/-- outputs: (out) -/        
@[apex_node "Switch<Vector4Array>"]
opaque SwitchVector4Array {numinput: Nat} (input : VariadicArg Vector4Array numinput) (index : Int) : Vector4Array

/-- outputs: (out) -/        
@[apex_node "SwitchByName<AnimChannel>"]
opaque SwitchByNameAnimChannel {numinput: Nat} (input : VariadicArg AnimChannel numinput) (name : String) : AnimChannel

/-- outputs: (out) -/        
@[apex_node "SwitchByName<AnimChannelCollection>"]
opaque SwitchByNameAnimChannelCollection {numinput: Nat} (input : VariadicArg AnimChannelCollection numinput) (name : String) : AnimChannelCollection

/-- outputs: (out) -/        
@[apex_node "SwitchByName<AnimStack>"]
opaque SwitchByNameAnimStack {numinput: Nat} (input : VariadicArg AnimStack numinput) (name : String) : AnimStack

/-- outputs: (out) -/        
@[apex_node "SwitchByName<ApexNodeID>"]
opaque SwitchByNameApexNodeID {numinput: Nat} (input : VariadicArg ApexNodeID numinput) (name : String) : ApexNodeID

/-- outputs: (out) -/        
@[apex_node "SwitchByName<ApexNodeIDArray>"]
opaque SwitchByNameApexNodeIDArray {numinput: Nat} (input : VariadicArg ApexNodeIDArray numinput) (name : String) : ApexNodeIDArray

/-- outputs: (out) -/        
@[apex_node "SwitchByName<ApexPortID>"]
opaque SwitchByNameApexPortID {numinput: Nat} (input : VariadicArg ApexPortID numinput) (name : String) : ApexPortID

/-- outputs: (out) -/        
@[apex_node "SwitchByName<ApexPortIDArray>"]
opaque SwitchByNameApexPortIDArray {numinput: Nat} (input : VariadicArg ApexPortIDArray numinput) (name : String) : ApexPortIDArray

/-- outputs: (out) -/        
@[apex_node "SwitchByName<Bool>"]
opaque SwitchByNameBool {numinput: Nat} (input : VariadicArg Bool numinput) (name : String) : Bool

/-- outputs: (out) -/        
@[apex_node "SwitchByName<BoolArray>"]
opaque SwitchByNameBoolArray {numinput: Nat} (input : VariadicArg BoolArray numinput) (name : String) : BoolArray

/-- outputs: (out) -/        
@[apex_node "SwitchByName<ColorRamp>"]
opaque SwitchByNameColorRamp {numinput: Nat} (input : VariadicArg ColorRamp numinput) (name : String) : ColorRamp

/-- outputs: (out) -/        
@[apex_node "SwitchByName<DataItem>"]
opaque SwitchByNameDataItem {numinput: Nat} (input : VariadicArg DataItem numinput) (name : String) : DataItem

/-- outputs: (out) -/        
@[apex_node "SwitchByName<Dict>"]
opaque SwitchByNameDict {numinput: Nat} (input : VariadicArg Dict numinput) (name : String) : Dict

/-- outputs: (out) -/        
@[apex_node "SwitchByName<DictArray>"]
opaque SwitchByNameDictArray {numinput: Nat} (input : VariadicArg DictArray numinput) (name : String) : DictArray

/-- outputs: (out) -/        
@[apex_node "SwitchByName<DynamicPath>"]
opaque SwitchByNameDynamicPath {numinput: Nat} (input : VariadicArg DynamicPath numinput) (name : String) : DynamicPath

/-- outputs: (out) -/        
@[apex_node "SwitchByName<DynamicPathArray>"]
opaque SwitchByNameDynamicPathArray {numinput: Nat} (input : VariadicArg DynamicPathArray numinput) (name : String) : DynamicPathArray

/-- outputs: (out) -/        
@[apex_node "SwitchByName<FBIKSkeleton>"]
opaque SwitchByNameFBIKSkeleton {numinput: Nat} (input : VariadicArg FBIKSkeleton numinput) (name : String) : FBIKSkeleton

/-- outputs: (out) -/        
@[apex_node "SwitchByName<FBIKSkeletonArray>"]
opaque SwitchByNameFBIKSkeletonArray {numinput: Nat} (input : VariadicArg FBIKSkeletonArray numinput) (name : String) : FBIKSkeletonArray

/-- outputs: (out) -/        
@[apex_node "SwitchByName<FBIKSolver>"]
opaque SwitchByNameFBIKSolver {numinput: Nat} (input : VariadicArg FBIKSolver numinput) (name : String) : FBIKSolver

/-- outputs: (out) -/        
@[apex_node "SwitchByName<FBIKSolverArray>"]
opaque SwitchByNameFBIKSolverArray {numinput: Nat} (input : VariadicArg FBIKSolverArray numinput) (name : String) : FBIKSolverArray

/-- outputs: (out) -/        
@[apex_node "SwitchByName<FBIKTarget>"]
opaque SwitchByNameFBIKTarget {numinput: Nat} (input : VariadicArg FBIKTarget numinput) (name : String) : FBIKTarget

/-- outputs: (out) -/        
@[apex_node "SwitchByName<FBIKTargetArray>"]
opaque SwitchByNameFBIKTargetArray {numinput: Nat} (input : VariadicArg FBIKTargetArray numinput) (name : String) : FBIKTargetArray

/-- outputs: (out) -/        
@[apex_node "SwitchByName<Float>"]
opaque SwitchByNameFloat {numinput: Nat} (input : VariadicArg Float numinput) (name : String) : Float

/-- outputs: (out) -/        
@[apex_node "SwitchByName<FloatArray>"]
opaque SwitchByNameFloatArray {numinput: Nat} (input : VariadicArg FloatArray numinput) (name : String) : FloatArray

/-- outputs: (out) -/        
@[apex_node "SwitchByName<FloatRamp>"]
opaque SwitchByNameFloatRamp {numinput: Nat} (input : VariadicArg FloatRamp numinput) (name : String) : FloatRamp

/-- outputs: (out) -/        
@[apex_node "SwitchByName<Geometry>"]
opaque SwitchByNameGeometry {numinput: Nat} (input : VariadicArg Geometry numinput) (name : String) : Geometry

/-- outputs: (out) -/        
@[apex_node "SwitchByName<GeometryArray>"]
opaque SwitchByNameGeometryArray {numinput: Nat} (input : VariadicArg GeometryArray numinput) (name : String) : GeometryArray

/-- outputs: (out) -/        
@[apex_node "SwitchByName<Int>"]
opaque SwitchByNameInt {numinput: Nat} (input : VariadicArg Int numinput) (name : String) : Int

/-- outputs: (out) -/        
@[apex_node "SwitchByName<IntArray>"]
opaque SwitchByNameIntArray {numinput: Nat} (input : VariadicArg IntArray numinput) (name : String) : IntArray

/-- outputs: (out) -/        
@[apex_node "SwitchByName<Matrix3>"]
opaque SwitchByNameMatrix3 {numinput: Nat} (input : VariadicArg Matrix3 numinput) (name : String) : Matrix3

/-- outputs: (out) -/        
@[apex_node "SwitchByName<Matrix3Array>"]
opaque SwitchByNameMatrix3Array {numinput: Nat} (input : VariadicArg Matrix3Array numinput) (name : String) : Matrix3Array

/-- outputs: (out) -/        
@[apex_node "SwitchByName<Matrix4>"]
opaque SwitchByNameMatrix4 {numinput: Nat} (input : VariadicArg Matrix4 numinput) (name : String) : Matrix4

/-- outputs: (out) -/        
@[apex_node "SwitchByName<Matrix4Array>"]
opaque SwitchByNameMatrix4Array {numinput: Nat} (input : VariadicArg Matrix4Array numinput) (name : String) : Matrix4Array

/-- outputs: (out) -/        
@[apex_node "SwitchByName<SimRootDataId>"]
opaque SwitchByNameSimRootDataId {numinput: Nat} (input : VariadicArg SimRootDataId numinput) (name : String) : SimRootDataId

/-- outputs: (out) -/        
@[apex_node "SwitchByName<SimRootDataIdArray>"]
opaque SwitchByNameSimRootDataIdArray {numinput: Nat} (input : VariadicArg SimRootDataIdArray numinput) (name : String) : SimRootDataIdArray

/-- outputs: (out) -/        
@[apex_node "SwitchByName<String>"]
opaque SwitchByNameString {numinput: Nat} (input : VariadicArg String numinput) (name : String) : String

/-- outputs: (out) -/        
@[apex_node "SwitchByName<StringArray>"]
opaque SwitchByNameStringArray {numinput: Nat} (input : VariadicArg StringArray numinput) (name : String) : StringArray

/-- outputs: (out) -/        
@[apex_node "SwitchByName<Vector2>"]
opaque SwitchByNameVector2 {numinput: Nat} (input : VariadicArg Vector2 numinput) (name : String) : Vector2

/-- outputs: (out) -/        
@[apex_node "SwitchByName<Vector2Array>"]
opaque SwitchByNameVector2Array {numinput: Nat} (input : VariadicArg Vector2Array numinput) (name : String) : Vector2Array

/-- outputs: (out) -/        
@[apex_node "SwitchByName<Vector3>"]
opaque SwitchByNameVector3 {numinput: Nat} (input : VariadicArg Vector3 numinput) (name : String) : Vector3

/-- outputs: (out) -/        
@[apex_node "SwitchByName<Vector3Array>"]
opaque SwitchByNameVector3Array {numinput: Nat} (input : VariadicArg Vector3Array numinput) (name : String) : Vector3Array

/-- outputs: (out) -/        
@[apex_node "SwitchByName<Vector4>"]
opaque SwitchByNameVector4 {numinput: Nat} (input : VariadicArg Vector4 numinput) (name : String) : Vector4

/-- outputs: (out) -/        
@[apex_node "SwitchByName<Vector4Array>"]
opaque SwitchByNameVector4Array {numinput: Nat} (input : VariadicArg Vector4Array numinput) (name : String) : Vector4Array

/-- outputs: (result) -/        
@[apex_node "Tan"]
opaque Tan (radians : Float) : Float

/-- outputs: (xform,localxform,restlocal,t,r,s) -/        
@[apex_node "TransformObject" has_rundata]
opaque TransformObject (parent : Matrix4) (parentlocal : Matrix4) (scaleinheritance : Int) (xform : Matrix4) (t : Vector3) (r : Vector3) (s : Vector3) (local' : Matrix4) (restlocal : Matrix4) (xord : Int) (rord : Int) (xformmask : Int) : Matrix4×Matrix4×Matrix4×Vector3×Vector3×Vector3

/-- outputs: (result) -/        
@[apex_node "Transpose<Matrix3>"]
opaque TransposeMatrix3 (a : Matrix3) : Matrix3

/-- outputs: (result) -/        
@[apex_node "Transpose<Matrix4>"]
opaque TransposeMatrix4 (a : Matrix4) : Matrix4

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<AnimChannel>"]
opaque TwoWaySwitchAnimChannel (a : AnimChannel) (b : AnimChannel) (index : Bool) : AnimChannel

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<AnimChannelCollection>"]
opaque TwoWaySwitchAnimChannelCollection (a : AnimChannelCollection) (b : AnimChannelCollection) (index : Bool) : AnimChannelCollection

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<AnimStack>"]
opaque TwoWaySwitchAnimStack (a : AnimStack) (b : AnimStack) (index : Bool) : AnimStack

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<ApexGraphHandle>"]
opaque TwoWaySwitchApexGraphHandle (a : ApexGraphHandle) (b : ApexGraphHandle) (index : Bool) : ApexGraphHandle

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<ApexNodeID>"]
opaque TwoWaySwitchApexNodeID (a : ApexNodeID) (b : ApexNodeID) (index : Bool) : ApexNodeID

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<ApexNodeIDArray>"]
opaque TwoWaySwitchApexNodeIDArray (a : ApexNodeIDArray) (b : ApexNodeIDArray) (index : Bool) : ApexNodeIDArray

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<ApexPortID>"]
opaque TwoWaySwitchApexPortID (a : ApexPortID) (b : ApexPortID) (index : Bool) : ApexPortID

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<ApexPortIDArray>"]
opaque TwoWaySwitchApexPortIDArray (a : ApexPortIDArray) (b : ApexPortIDArray) (index : Bool) : ApexPortIDArray

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<Bool>"]
opaque TwoWaySwitchBool (a : Bool) (b : Bool) (index : Bool) : Bool

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<BoolArray>"]
opaque TwoWaySwitchBoolArray (a : BoolArray) (b : BoolArray) (index : Bool) : BoolArray

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<ColorRamp>"]
opaque TwoWaySwitchColorRamp (a : ColorRamp) (b : ColorRamp) (index : Bool) : ColorRamp

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<DataItem>"]
opaque TwoWaySwitchDataItem (a : DataItem) (b : DataItem) (index : Bool) : DataItem

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<Dict>"]
opaque TwoWaySwitchDict (a : Dict) (b : Dict) (index : Bool) : Dict

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<DictArray>"]
opaque TwoWaySwitchDictArray (a : DictArray) (b : DictArray) (index : Bool) : DictArray

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<DynamicPath>"]
opaque TwoWaySwitchDynamicPath (a : DynamicPath) (b : DynamicPath) (index : Bool) : DynamicPath

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<DynamicPathArray>"]
opaque TwoWaySwitchDynamicPathArray (a : DynamicPathArray) (b : DynamicPathArray) (index : Bool) : DynamicPathArray

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<FBIKSkeleton>"]
opaque TwoWaySwitchFBIKSkeleton (a : FBIKSkeleton) (b : FBIKSkeleton) (index : Bool) : FBIKSkeleton

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<FBIKSkeletonArray>"]
opaque TwoWaySwitchFBIKSkeletonArray (a : FBIKSkeletonArray) (b : FBIKSkeletonArray) (index : Bool) : FBIKSkeletonArray

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<FBIKSolver>"]
opaque TwoWaySwitchFBIKSolver (a : FBIKSolver) (b : FBIKSolver) (index : Bool) : FBIKSolver

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<FBIKSolverArray>"]
opaque TwoWaySwitchFBIKSolverArray (a : FBIKSolverArray) (b : FBIKSolverArray) (index : Bool) : FBIKSolverArray

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<FBIKTarget>"]
opaque TwoWaySwitchFBIKTarget (a : FBIKTarget) (b : FBIKTarget) (index : Bool) : FBIKTarget

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<FBIKTargetArray>"]
opaque TwoWaySwitchFBIKTargetArray (a : FBIKTargetArray) (b : FBIKTargetArray) (index : Bool) : FBIKTargetArray

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<Float>"]
opaque TwoWaySwitchFloat (a : Float) (b : Float) (index : Bool) : Float

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<FloatArray>"]
opaque TwoWaySwitchFloatArray (a : FloatArray) (b : FloatArray) (index : Bool) : FloatArray

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<FloatRamp>"]
opaque TwoWaySwitchFloatRamp (a : FloatRamp) (b : FloatRamp) (index : Bool) : FloatRamp

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<Geometry>"]
opaque TwoWaySwitchGeometry (a : Geometry) (b : Geometry) (index : Bool) : Geometry

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<GeometryArray>"]
opaque TwoWaySwitchGeometryArray (a : GeometryArray) (b : GeometryArray) (index : Bool) : GeometryArray

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<Int>"]
opaque TwoWaySwitchInt (a : Int) (b : Int) (index : Bool) : Int

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<IntArray>"]
opaque TwoWaySwitchIntArray (a : IntArray) (b : IntArray) (index : Bool) : IntArray

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<Matrix3>"]
opaque TwoWaySwitchMatrix3 (a : Matrix3) (b : Matrix3) (index : Bool) : Matrix3

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<Matrix3Array>"]
opaque TwoWaySwitchMatrix3Array (a : Matrix3Array) (b : Matrix3Array) (index : Bool) : Matrix3Array

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<Matrix4>"]
opaque TwoWaySwitchMatrix4 (a : Matrix4) (b : Matrix4) (index : Bool) : Matrix4

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<Matrix4Array>"]
opaque TwoWaySwitchMatrix4Array (a : Matrix4Array) (b : Matrix4Array) (index : Bool) : Matrix4Array

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<SimRootDataId>"]
opaque TwoWaySwitchSimRootDataId (a : SimRootDataId) (b : SimRootDataId) (index : Bool) : SimRootDataId

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<SimRootDataIdArray>"]
opaque TwoWaySwitchSimRootDataIdArray (a : SimRootDataIdArray) (b : SimRootDataIdArray) (index : Bool) : SimRootDataIdArray

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<String>"]
opaque TwoWaySwitchString (a : String) (b : String) (index : Bool) : String

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<StringArray>"]
opaque TwoWaySwitchStringArray (a : StringArray) (b : StringArray) (index : Bool) : StringArray

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<Vector2>"]
opaque TwoWaySwitchVector2 (a : Vector2) (b : Vector2) (index : Bool) : Vector2

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<Vector2Array>"]
opaque TwoWaySwitchVector2Array (a : Vector2Array) (b : Vector2Array) (index : Bool) : Vector2Array

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<Vector3>"]
opaque TwoWaySwitchVector3 (a : Vector3) (b : Vector3) (index : Bool) : Vector3

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<Vector3Array>"]
opaque TwoWaySwitchVector3Array (a : Vector3Array) (b : Vector3Array) (index : Bool) : Vector3Array

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<Vector4>"]
opaque TwoWaySwitchVector4 (a : Vector4) (b : Vector4) (index : Bool) : Vector4

/-- outputs: (out) -/        
@[apex_node "TwoWaySwitch<Vector4Array>"]
opaque TwoWaySwitchVector4Array (a : Vector4Array) (b : Vector4Array) (index : Bool) : Vector4Array

-- special function not supported yet
-- opaque UpdateCharacterElement (input : String) (separators : String) {numvalues: Nat} (values : VariadicArg ...

-- special function not supported yet
-- opaque UpdateCharacterGraph (input : Bool) (condition : Bool) (__spare__ : undefined) (parm : String) (graph...

-- special function not supported yet
-- opaque UpdateNodeTags (graph : ApexGraphHandle) (nodeid : ApexNodeID) (array : StringArray) (array : StringA...

/-- outputs: (value) -/        
@[apex_node "Value<AnimChannel>"]
opaque ValueAnimChannel (parm : AnimChannel) : AnimChannel

/-- outputs: (value) -/        
@[apex_node "Value<AnimChannelCollection>"]
opaque ValueAnimChannelCollection (parm : AnimChannelCollection) : AnimChannelCollection

/-- outputs: (value) -/        
@[apex_node "Value<AnimStack>"]
opaque ValueAnimStack (parm : AnimStack) : AnimStack

/-- outputs: (value) -/        
@[apex_node "Value<ApexGraphHandle>"]
opaque ValueApexGraphHandle (parm : ApexGraphHandle) : ApexGraphHandle

/-- outputs: (value) -/        
@[apex_node "Value<ApexNodeID>"]
opaque ValueApexNodeID (parm : ApexNodeID) : ApexNodeID

/-- outputs: (value) -/        
@[apex_node "Value<ApexNodeIDArray>"]
opaque ValueApexNodeIDArray (parm : ApexNodeIDArray) : ApexNodeIDArray

/-- outputs: (value) -/        
@[apex_node "Value<ApexPortID>"]
opaque ValueApexPortID (parm : ApexPortID) : ApexPortID

/-- outputs: (value) -/        
@[apex_node "Value<ApexPortIDArray>"]
opaque ValueApexPortIDArray (parm : ApexPortIDArray) : ApexPortIDArray

/-- outputs: (value) -/        
@[apex_node "Value<Bool>"]
opaque ValueBool (parm : Bool) : Bool

/-- outputs: (value) -/        
@[apex_node "Value<BoolArray>"]
opaque ValueBoolArray (parm : BoolArray) : BoolArray

/-- outputs: (value) -/        
@[apex_node "Value<ColorRamp>"]
opaque ValueColorRamp (parm : ColorRamp) : ColorRamp

/-- outputs: (value) -/        
@[apex_node "Value<DataItem>"]
opaque ValueDataItem (parm : DataItem) : DataItem

/-- outputs: (value) -/        
@[apex_node "Value<Dict>"]
opaque ValueDict (parm : Dict) : Dict

/-- outputs: (value) -/        
@[apex_node "Value<DictArray>"]
opaque ValueDictArray (parm : DictArray) : DictArray

/-- outputs: (value) -/        
@[apex_node "Value<DynamicPath>"]
opaque ValueDynamicPath (parm : DynamicPath) : DynamicPath

/-- outputs: (value) -/        
@[apex_node "Value<DynamicPathArray>"]
opaque ValueDynamicPathArray (parm : DynamicPathArray) : DynamicPathArray

/-- outputs: (value) -/        
@[apex_node "Value<FBIKSkeleton>"]
opaque ValueFBIKSkeleton (parm : FBIKSkeleton) : FBIKSkeleton

/-- outputs: (value) -/        
@[apex_node "Value<FBIKSkeletonArray>"]
opaque ValueFBIKSkeletonArray (parm : FBIKSkeletonArray) : FBIKSkeletonArray

/-- outputs: (value) -/        
@[apex_node "Value<FBIKSolverArray>"]
opaque ValueFBIKSolverArray (parm : FBIKSolverArray) : FBIKSolverArray

/-- outputs: (value) -/        
@[apex_node "Value<FBIKTargetArray>"]
opaque ValueFBIKTargetArray (parm : FBIKTargetArray) : FBIKTargetArray

/-- outputs: (value) -/        
@[apex_node "Value<Float>"]
opaque ValueFloat (parm : Float) : Float

/-- outputs: (value) -/        
@[apex_node "Value<FloatArray>"]
opaque ValueFloatArray (parm : FloatArray) : FloatArray

/-- outputs: (value) -/        
@[apex_node "Value<FloatRamp>"]
opaque ValueFloatRamp (parm : FloatRamp) : FloatRamp

/-- outputs: (value) -/        
@[apex_node "Value<Geometry>"]
opaque ValueGeometry (parm : Geometry) : Geometry

/-- outputs: (value) -/        
@[apex_node "Value<GeometryArray>"]
opaque ValueGeometryArray (parm : GeometryArray) : GeometryArray

/-- outputs: (value) -/        
@[apex_node "Value<Int>"]
opaque ValueInt (parm : Int) : Int

/-- outputs: (value) -/        
@[apex_node "Value<IntArray>"]
opaque ValueIntArray (parm : IntArray) : IntArray

/-- outputs: (value) -/        
@[apex_node "Value<Matrix3>"]
opaque ValueMatrix3 (parm : Matrix3) : Matrix3

/-- outputs: (value) -/        
@[apex_node "Value<Matrix3Array>"]
opaque ValueMatrix3Array (parm : Matrix3Array) : Matrix3Array

/-- outputs: (value) -/        
@[apex_node "Value<Matrix4>"]
opaque ValueMatrix4 (parm : Matrix4) : Matrix4

/-- outputs: (value) -/        
@[apex_node "Value<Matrix4Array>"]
opaque ValueMatrix4Array (parm : Matrix4Array) : Matrix4Array

/-- outputs: (value) -/        
@[apex_node "Value<SimRootDataIdArray>"]
opaque ValueSimRootDataIdArray (parm : SimRootDataIdArray) : SimRootDataIdArray

/-- outputs: (value) -/        
@[apex_node "Value<String>"]
opaque ValueString (parm : String) : String

/-- outputs: (value) -/        
@[apex_node "Value<StringArray>"]
opaque ValueStringArray (parm : StringArray) : StringArray

/-- outputs: (value) -/        
@[apex_node "Value<Vector2>"]
opaque ValueVector2 (parm : Vector2) : Vector2

/-- outputs: (value) -/        
@[apex_node "Value<Vector2Array>"]
opaque ValueVector2Array (parm : Vector2Array) : Vector2Array

/-- outputs: (value) -/        
@[apex_node "Value<Vector3>"]
opaque ValueVector3 (parm : Vector3) : Vector3

/-- outputs: (value) -/        
@[apex_node "Value<Vector3Array>"]
opaque ValueVector3Array (parm : Vector3Array) : Vector3Array

/-- outputs: (value) -/        
@[apex_node "Value<Vector4>"]
opaque ValueVector4 (parm : Vector4) : Vector4

/-- outputs: (value) -/        
@[apex_node "Value<Vector4Array>"]
opaque ValueVector4Array (parm : Vector4Array) : Vector4Array

/-- outputs: (x,y) -/        
@[apex_node "Vector2ToFloat"]
opaque Vector2ToFloat (vector : Vector2) : Float×Float

/-- outputs: (x,y,z) -/        
@[apex_node "Vector3ToFloat"]
opaque Vector3ToFloat (vector : Vector3) : Float×Float×Float

/-- outputs: (m) -/        
@[apex_node "Vector3ToMatrix3"]
opaque Vector3ToMatrix3 (row1 : Vector3) (row2 : Vector3) (row3 : Vector3) : Matrix3

/-- outputs: (x,y,z,w) -/        
@[apex_node "Vector4ToFloat"]
opaque Vector4ToFloat (vector : Vector4) : Float×Float×Float×Float

/-- outputs: (m) -/        
@[apex_node "Vector4ToMatrix4"]
opaque Vector4ToMatrix4 (row1 : Vector4) (row2 : Vector4) (row3 : Vector4) (row4 : Vector4) : Matrix4

/-- outputs: (result) -/        
@[apex_node "Xor"]
opaque Xor {numinputs: Nat} (inputs : VariadicArg Bool numinputs) : Bool

-- special function not supported yet
-- opaque __null__ (__spare__ : undefined) : undefined...

-- special function not supported yet
-- opaque __parms__  : undefined...

-- special function not supported yet
-- opaque __subnet__ (next : undefined) : undefined...

/-- outputs: (result) -/        
@[apex_node "array::Add<Float>" has_rundata]
opaque array_AddFloat (a : FloatArray) (b : FloatArray) : FloatArray

/-- outputs: (result) -/        
@[apex_node "array::Add<Int>" has_rundata]
opaque array_AddInt (a : IntArray) (b : IntArray) : IntArray

/-- outputs: (result) -/        
@[apex_node "array::Add<Matrix3>" has_rundata]
opaque array_AddMatrix3 (a : Matrix3Array) (b : Matrix3Array) : Matrix3Array

/-- outputs: (result) -/        
@[apex_node "array::Add<Matrix4>" has_rundata]
opaque array_AddMatrix4 (a : Matrix4Array) (b : Matrix4Array) : Matrix4Array

/-- outputs: (result) -/        
@[apex_node "array::Add<Vector2>" has_rundata]
opaque array_AddVector2 (a : Vector2Array) (b : Vector2Array) : Vector2Array

/-- outputs: (result) -/        
@[apex_node "array::Add<Vector3>" has_rundata]
opaque array_AddVector3 (a : Vector3Array) (b : Vector3Array) : Vector3Array

/-- outputs: (result) -/        
@[apex_node "array::Add<Vector4>" has_rundata]
opaque array_AddVector4 (a : Vector4Array) (b : Vector4Array) : Vector4Array

/-- outputs: (array,index) -/        
@[apex_node "array::Append<ApexNodeID>"]
opaque array_AppendApexNodeID (array : ApexNodeIDArray) (value : ApexNodeID) : ApexNodeIDArray×Int

/-- outputs: (array,index) -/        
@[apex_node "array::Append<ApexPortID>"]
opaque array_AppendApexPortID (array : ApexPortIDArray) (value : ApexPortID) : ApexPortIDArray×Int

/-- outputs: (array,index) -/        
@[apex_node "array::Append<Bool>"]
opaque array_AppendBool (array : BoolArray) (value : Bool) : BoolArray×Int

/-- outputs: (array,index) -/        
@[apex_node "array::Append<Dict>"]
opaque array_AppendDict (array : DictArray) (value : Dict) : DictArray×Int

/-- outputs: (array,index) -/        
@[apex_node "array::Append<DynamicPath>"]
opaque array_AppendDynamicPath (array : DynamicPathArray) (value : DynamicPath) : DynamicPathArray×Int

/-- outputs: (array,index) -/        
@[apex_node "array::Append<FBIKSkeleton>"]
opaque array_AppendFBIKSkeleton (array : FBIKSkeletonArray) (value : FBIKSkeleton) : FBIKSkeletonArray×Int

/-- outputs: (array,index) -/        
@[apex_node "array::Append<FBIKSolver>"]
opaque array_AppendFBIKSolver (array : FBIKSolverArray) (value : FBIKSolver) : FBIKSolverArray×Int

/-- outputs: (array,index) -/        
@[apex_node "array::Append<FBIKTarget>"]
opaque array_AppendFBIKTarget (array : FBIKTargetArray) (value : FBIKTarget) : FBIKTargetArray×Int

/-- outputs: (array,index) -/        
@[apex_node "array::Append<Float>"]
opaque array_AppendFloat (array : FloatArray) (value : Float) : FloatArray×Int

/-- outputs: (array,index) -/        
@[apex_node "array::Append<Geometry>"]
opaque array_AppendGeometry (array : GeometryArray) (value : Geometry) : GeometryArray×Int

/-- outputs: (array,index) -/        
@[apex_node "array::Append<Int>"]
opaque array_AppendInt (array : IntArray) (value : Int) : IntArray×Int

/-- outputs: (array,index) -/        
@[apex_node "array::Append<Matrix3>"]
opaque array_AppendMatrix3 (array : Matrix3Array) (value : Matrix3) : Matrix3Array×Int

/-- outputs: (array,index) -/        
@[apex_node "array::Append<Matrix4>"]
opaque array_AppendMatrix4 (array : Matrix4Array) (value : Matrix4) : Matrix4Array×Int

/-- outputs: (array,index) -/        
@[apex_node "array::Append<SimRootDataId>"]
opaque array_AppendSimRootDataId (array : SimRootDataIdArray) (value : SimRootDataId) : SimRootDataIdArray×Int

/-- outputs: (array,index) -/        
@[apex_node "array::Append<String>"]
opaque array_AppendString (array : StringArray) (value : String) : StringArray×Int

/-- outputs: (array,index) -/        
@[apex_node "array::Append<Vector2>"]
opaque array_AppendVector2 (array : Vector2Array) (value : Vector2) : Vector2Array×Int

/-- outputs: (array,index) -/        
@[apex_node "array::Append<Vector3>"]
opaque array_AppendVector3 (array : Vector3Array) (value : Vector3) : Vector3Array×Int

/-- outputs: (array,index) -/        
@[apex_node "array::Append<Vector4>"]
opaque array_AppendVector4 (array : Vector4Array) (value : Vector4) : Vector4Array×Int

/-- outputs: (result) -/        
@[apex_node "array::Build<ApexNodeID>"]
opaque array_BuildApexNodeID {numvalues: Nat} (values : VariadicArg ApexNodeID numvalues) : ApexNodeIDArray

/-- outputs: (result) -/        
@[apex_node "array::Build<ApexPortID>"]
opaque array_BuildApexPortID {numvalues: Nat} (values : VariadicArg ApexPortID numvalues) : ApexPortIDArray

/-- outputs: (result) -/        
@[apex_node "array::Build<Bool>"]
opaque array_BuildBool {numvalues: Nat} (values : VariadicArg Bool numvalues) : BoolArray

/-- outputs: (result) -/        
@[apex_node "array::Build<Dict>"]
opaque array_BuildDict {numvalues: Nat} (values : VariadicArg Dict numvalues) : DictArray

/-- outputs: (result) -/        
@[apex_node "array::Build<DynamicPath>"]
opaque array_BuildDynamicPath {numvalues: Nat} (values : VariadicArg DynamicPath numvalues) : DynamicPathArray

/-- outputs: (result) -/        
@[apex_node "array::Build<FBIKSkeleton>"]
opaque array_BuildFBIKSkeleton {numvalues: Nat} (values : VariadicArg FBIKSkeleton numvalues) : FBIKSkeletonArray

/-- outputs: (result) -/        
@[apex_node "array::Build<FBIKSolver>"]
opaque array_BuildFBIKSolver {numvalues: Nat} (values : VariadicArg FBIKSolver numvalues) : FBIKSolverArray

/-- outputs: (result) -/        
@[apex_node "array::Build<FBIKTarget>"]
opaque array_BuildFBIKTarget {numvalues: Nat} (values : VariadicArg FBIKTarget numvalues) : FBIKTargetArray

/-- outputs: (result) -/        
@[apex_node "array::Build<Float>"]
opaque array_BuildFloat {numvalues: Nat} (values : VariadicArg Float numvalues) : FloatArray

/-- outputs: (result) -/        
@[apex_node "array::Build<Geometry>"]
opaque array_BuildGeometry {numvalues: Nat} (values : VariadicArg Geometry numvalues) : GeometryArray

/-- outputs: (result) -/        
@[apex_node "array::Build<Int>"]
opaque array_BuildInt {numvalues: Nat} (values : VariadicArg Int numvalues) : IntArray

/-- outputs: (result) -/        
@[apex_node "array::Build<Matrix3>"]
opaque array_BuildMatrix3 {numvalues: Nat} (values : VariadicArg Matrix3 numvalues) : Matrix3Array

/-- outputs: (result) -/        
@[apex_node "array::Build<Matrix4>"]
opaque array_BuildMatrix4 {numvalues: Nat} (values : VariadicArg Matrix4 numvalues) : Matrix4Array

/-- outputs: (result) -/        
@[apex_node "array::Build<SimRootDataId>"]
opaque array_BuildSimRootDataId {numvalues: Nat} (values : VariadicArg SimRootDataId numvalues) : SimRootDataIdArray

/-- outputs: (result) -/        
@[apex_node "array::Build<String>"]
opaque array_BuildString {numvalues: Nat} (values : VariadicArg String numvalues) : StringArray

/-- outputs: (result) -/        
@[apex_node "array::Build<Vector2>"]
opaque array_BuildVector2 {numvalues: Nat} (values : VariadicArg Vector2 numvalues) : Vector2Array

/-- outputs: (result) -/        
@[apex_node "array::Build<Vector3>"]
opaque array_BuildVector3 {numvalues: Nat} (values : VariadicArg Vector3 numvalues) : Vector3Array

/-- outputs: (result) -/        
@[apex_node "array::Build<Vector4>"]
opaque array_BuildVector4 {numvalues: Nat} (values : VariadicArg Vector4 numvalues) : Vector4Array

/-- outputs: (array) -/        
@[apex_node "array::Clear<ApexNodeID>"]
opaque array_ClearApexNodeID (array : ApexNodeIDArray) : ApexNodeIDArray

/-- outputs: (array) -/        
@[apex_node "array::Clear<ApexPortID>"]
opaque array_ClearApexPortID (array : ApexPortIDArray) : ApexPortIDArray

/-- outputs: (array) -/        
@[apex_node "array::Clear<Bool>"]
opaque array_ClearBool (array : BoolArray) : BoolArray

/-- outputs: (array) -/        
@[apex_node "array::Clear<Dict>"]
opaque array_ClearDict (array : DictArray) : DictArray

/-- outputs: (array) -/        
@[apex_node "array::Clear<DynamicPath>"]
opaque array_ClearDynamicPath (array : DynamicPathArray) : DynamicPathArray

/-- outputs: (array) -/        
@[apex_node "array::Clear<FBIKSkeleton>"]
opaque array_ClearFBIKSkeleton (array : FBIKSkeletonArray) : FBIKSkeletonArray

/-- outputs: (array) -/        
@[apex_node "array::Clear<FBIKSolver>"]
opaque array_ClearFBIKSolver (array : FBIKSolverArray) : FBIKSolverArray

/-- outputs: (array) -/        
@[apex_node "array::Clear<FBIKTarget>"]
opaque array_ClearFBIKTarget (array : FBIKTargetArray) : FBIKTargetArray

/-- outputs: (array) -/        
@[apex_node "array::Clear<Float>"]
opaque array_ClearFloat (array : FloatArray) : FloatArray

/-- outputs: (array) -/        
@[apex_node "array::Clear<Geometry>"]
opaque array_ClearGeometry (array : GeometryArray) : GeometryArray

/-- outputs: (array) -/        
@[apex_node "array::Clear<Int>"]
opaque array_ClearInt (array : IntArray) : IntArray

/-- outputs: (array) -/        
@[apex_node "array::Clear<Matrix3>"]
opaque array_ClearMatrix3 (array : Matrix3Array) : Matrix3Array

/-- outputs: (array) -/        
@[apex_node "array::Clear<Matrix4>"]
opaque array_ClearMatrix4 (array : Matrix4Array) : Matrix4Array

/-- outputs: (array) -/        
@[apex_node "array::Clear<SimRootDataId>"]
opaque array_ClearSimRootDataId (array : SimRootDataIdArray) : SimRootDataIdArray

/-- outputs: (array) -/        
@[apex_node "array::Clear<String>"]
opaque array_ClearString (array : StringArray) : StringArray

/-- outputs: (array) -/        
@[apex_node "array::Clear<Vector2>"]
opaque array_ClearVector2 (array : Vector2Array) : Vector2Array

/-- outputs: (array) -/        
@[apex_node "array::Clear<Vector3>"]
opaque array_ClearVector3 (array : Vector3Array) : Vector3Array

/-- outputs: (array) -/        
@[apex_node "array::Clear<Vector4>"]
opaque array_ClearVector4 (array : Vector4Array) : Vector4Array

/-- outputs: (result) -/        
@[apex_node "array::Divide<Float>" has_rundata]
opaque array_DivideFloat (dividend : FloatArray) (divisor : FloatArray) : FloatArray

/-- outputs: (result) -/        
@[apex_node "array::Divide<Int>" has_rundata]
opaque array_DivideInt (dividend : IntArray) (divisor : IntArray) : IntArray

/-- outputs: (result) -/        
@[apex_node "array::Divide<Vector2>" has_rundata]
opaque array_DivideVector2 (dividend : Vector2Array) (divisor : Vector2Array) : Vector2Array

/-- outputs: (result) -/        
@[apex_node "array::Divide<Vector3>" has_rundata]
opaque array_DivideVector3 (dividend : Vector3Array) (divisor : Vector3Array) : Vector3Array

/-- outputs: (result) -/        
@[apex_node "array::Divide<Vector4>" has_rundata]
opaque array_DivideVector4 (dividend : Vector4Array) (divisor : Vector4Array) : Vector4Array

/-- outputs: (a) -/        
@[apex_node "array::Extend<ApexNodeID>"]
opaque array_ExtendApexNodeID (a : ApexNodeIDArray) (b : ApexNodeIDArray) : ApexNodeIDArray

/-- outputs: (a) -/        
@[apex_node "array::Extend<ApexPortID>"]
opaque array_ExtendApexPortID (a : ApexPortIDArray) (b : ApexPortIDArray) : ApexPortIDArray

/-- outputs: (a) -/        
@[apex_node "array::Extend<Bool>"]
opaque array_ExtendBool (a : BoolArray) (b : BoolArray) : BoolArray

/-- outputs: (a) -/        
@[apex_node "array::Extend<Dict>"]
opaque array_ExtendDict (a : DictArray) (b : DictArray) : DictArray

/-- outputs: (a) -/        
@[apex_node "array::Extend<DynamicPath>"]
opaque array_ExtendDynamicPath (a : DynamicPathArray) (b : DynamicPathArray) : DynamicPathArray

/-- outputs: (a) -/        
@[apex_node "array::Extend<FBIKSkeleton>"]
opaque array_ExtendFBIKSkeleton (a : FBIKSkeletonArray) (b : FBIKSkeletonArray) : FBIKSkeletonArray

/-- outputs: (a) -/        
@[apex_node "array::Extend<FBIKSolver>"]
opaque array_ExtendFBIKSolver (a : FBIKSolverArray) (b : FBIKSolverArray) : FBIKSolverArray

/-- outputs: (a) -/        
@[apex_node "array::Extend<FBIKTarget>"]
opaque array_ExtendFBIKTarget (a : FBIKTargetArray) (b : FBIKTargetArray) : FBIKTargetArray

/-- outputs: (a) -/        
@[apex_node "array::Extend<Float>"]
opaque array_ExtendFloat (a : FloatArray) (b : FloatArray) : FloatArray

/-- outputs: (a) -/        
@[apex_node "array::Extend<Geometry>"]
opaque array_ExtendGeometry (a : GeometryArray) (b : GeometryArray) : GeometryArray

/-- outputs: (a) -/        
@[apex_node "array::Extend<Int>"]
opaque array_ExtendInt (a : IntArray) (b : IntArray) : IntArray

/-- outputs: (a) -/        
@[apex_node "array::Extend<Matrix3>"]
opaque array_ExtendMatrix3 (a : Matrix3Array) (b : Matrix3Array) : Matrix3Array

/-- outputs: (a) -/        
@[apex_node "array::Extend<Matrix4>"]
opaque array_ExtendMatrix4 (a : Matrix4Array) (b : Matrix4Array) : Matrix4Array

/-- outputs: (a) -/        
@[apex_node "array::Extend<SimRootDataId>"]
opaque array_ExtendSimRootDataId (a : SimRootDataIdArray) (b : SimRootDataIdArray) : SimRootDataIdArray

/-- outputs: (a) -/        
@[apex_node "array::Extend<String>"]
opaque array_ExtendString (a : StringArray) (b : StringArray) : StringArray

/-- outputs: (a) -/        
@[apex_node "array::Extend<Vector2>"]
opaque array_ExtendVector2 (a : Vector2Array) (b : Vector2Array) : Vector2Array

/-- outputs: (a) -/        
@[apex_node "array::Extend<Vector3>"]
opaque array_ExtendVector3 (a : Vector3Array) (b : Vector3Array) : Vector3Array

/-- outputs: (a) -/        
@[apex_node "array::Extend<Vector4>"]
opaque array_ExtendVector4 (a : Vector4Array) (b : Vector4Array) : Vector4Array

/-- outputs: (index) -/        
@[apex_node "array::Find<ApexNodeID>"]
opaque array_FindApexNodeID (array : ApexNodeIDArray) (value : ApexNodeID) : Int

/-- outputs: (index) -/        
@[apex_node "array::Find<ApexPortID>"]
opaque array_FindApexPortID (array : ApexPortIDArray) (value : ApexPortID) : Int

/-- outputs: (index) -/        
@[apex_node "array::Find<Bool>"]
opaque array_FindBool (array : BoolArray) (value : Bool) : Int

/-- outputs: (index) -/        
@[apex_node "array::Find<Float>"]
opaque array_FindFloat (array : FloatArray) (value : Float) : Int

/-- outputs: (index) -/        
@[apex_node "array::Find<Int>"]
opaque array_FindInt (array : IntArray) (value : Int) : Int

/-- outputs: (index) -/        
@[apex_node "array::Find<Matrix3>"]
opaque array_FindMatrix3 (array : Matrix3Array) (value : Matrix3) : Int

/-- outputs: (index) -/        
@[apex_node "array::Find<Matrix4>"]
opaque array_FindMatrix4 (array : Matrix4Array) (value : Matrix4) : Int

/-- outputs: (index) -/        
@[apex_node "array::Find<SimRootDataId>"]
opaque array_FindSimRootDataId (array : SimRootDataIdArray) (value : SimRootDataId) : Int

/-- outputs: (index) -/        
@[apex_node "array::Find<String>"]
opaque array_FindString (array : StringArray) (value : String) : Int

/-- outputs: (index) -/        
@[apex_node "array::Find<Vector2>"]
opaque array_FindVector2 (array : Vector2Array) (value : Vector2) : Int

/-- outputs: (index) -/        
@[apex_node "array::Find<Vector3>"]
opaque array_FindVector3 (array : Vector3Array) (value : Vector3) : Int

/-- outputs: (index) -/        
@[apex_node "array::Find<Vector4>"]
opaque array_FindVector4 (array : Vector4Array) (value : Vector4) : Int

/-- outputs: (value,success) -/        
@[apex_node "array::Get<ApexNodeID>"]
opaque array_GetApexNodeID (array : ApexNodeIDArray) (index : Int) (default : ApexNodeID) : ApexNodeID×Bool

/-- outputs: (value,success) -/        
@[apex_node "array::Get<ApexPortID>"]
opaque array_GetApexPortID (array : ApexPortIDArray) (index : Int) (default : ApexPortID) : ApexPortID×Bool

/-- outputs: (value,success) -/        
@[apex_node "array::Get<Bool>"]
opaque array_GetBool (array : BoolArray) (index : Int) (default : Bool) : Bool×Bool

/-- outputs: (value,success) -/        
@[apex_node "array::Get<Dict>"]
opaque array_GetDict (array : DictArray) (index : Int) (default : Dict) : Dict×Bool

/-- outputs: (value,success) -/        
@[apex_node "array::Get<DynamicPath>"]
opaque array_GetDynamicPath (array : DynamicPathArray) (index : Int) (default : DynamicPath) : DynamicPath×Bool

/-- outputs: (value,success) -/        
@[apex_node "array::Get<FBIKSkeleton>"]
opaque array_GetFBIKSkeleton (array : FBIKSkeletonArray) (index : Int) (default : FBIKSkeleton) : FBIKSkeleton×Bool

/-- outputs: (value,success) -/        
@[apex_node "array::Get<FBIKSolver>"]
opaque array_GetFBIKSolver (array : FBIKSolverArray) (index : Int) (default : FBIKSolver) : FBIKSolver×Bool

/-- outputs: (value,success) -/        
@[apex_node "array::Get<FBIKTarget>"]
opaque array_GetFBIKTarget (array : FBIKTargetArray) (index : Int) (default : FBIKTarget) : FBIKTarget×Bool

/-- outputs: (value,success) -/        
@[apex_node "array::Get<Float>"]
opaque array_GetFloat (array : FloatArray) (index : Int) (default : Float) : Float×Bool

/-- outputs: (value,success) -/        
@[apex_node "array::Get<Geometry>"]
opaque array_GetGeometry (array : GeometryArray) (index : Int) (default : Geometry) : Geometry×Bool

/-- outputs: (value,success) -/        
@[apex_node "array::Get<Int>"]
opaque array_GetInt (array : IntArray) (index : Int) (default : Int) : Int×Bool

/-- outputs: (value,success) -/        
@[apex_node "array::Get<Matrix3>"]
opaque array_GetMatrix3 (array : Matrix3Array) (index : Int) (default : Matrix3) : Matrix3×Bool

/-- outputs: (value,success) -/        
@[apex_node "array::Get<Matrix4>"]
opaque array_GetMatrix4 (array : Matrix4Array) (index : Int) (default : Matrix4) : Matrix4×Bool

/-- outputs: (value,success) -/        
@[apex_node "array::Get<SimRootDataId>"]
opaque array_GetSimRootDataId (array : SimRootDataIdArray) (index : Int) (default : SimRootDataId) : SimRootDataId×Bool

/-- outputs: (value,success) -/        
@[apex_node "array::Get<String>"]
opaque array_GetString (array : StringArray) (index : Int) (default : String) : String×Bool

/-- outputs: (value,success) -/        
@[apex_node "array::Get<Vector2>"]
opaque array_GetVector2 (array : Vector2Array) (index : Int) (default : Vector2) : Vector2×Bool

/-- outputs: (value,success) -/        
@[apex_node "array::Get<Vector3>"]
opaque array_GetVector3 (array : Vector3Array) (index : Int) (default : Vector3) : Vector3×Bool

/-- outputs: (value,success) -/        
@[apex_node "array::Get<Vector4>"]
opaque array_GetVector4 (array : Vector4Array) (index : Int) (default : Vector4) : Vector4×Bool

/-- outputs: (array) -/        
@[apex_node "array::Insert<ApexNodeID>"]
opaque array_InsertApexNodeID (array : ApexNodeIDArray) (value : ApexNodeID) (index : Int) : ApexNodeIDArray

/-- outputs: (array) -/        
@[apex_node "array::Insert<ApexPortID>"]
opaque array_InsertApexPortID (array : ApexPortIDArray) (value : ApexPortID) (index : Int) : ApexPortIDArray

/-- outputs: (array) -/        
@[apex_node "array::Insert<Bool>"]
opaque array_InsertBool (array : BoolArray) (value : Bool) (index : Int) : BoolArray

/-- outputs: (array) -/        
@[apex_node "array::Insert<Dict>"]
opaque array_InsertDict (array : DictArray) (value : Dict) (index : Int) : DictArray

/-- outputs: (array) -/        
@[apex_node "array::Insert<DynamicPath>"]
opaque array_InsertDynamicPath (array : DynamicPathArray) (value : DynamicPath) (index : Int) : DynamicPathArray

/-- outputs: (array) -/        
@[apex_node "array::Insert<FBIKSkeleton>"]
opaque array_InsertFBIKSkeleton (array : FBIKSkeletonArray) (value : FBIKSkeleton) (index : Int) : FBIKSkeletonArray

/-- outputs: (array) -/        
@[apex_node "array::Insert<FBIKSolver>"]
opaque array_InsertFBIKSolver (array : FBIKSolverArray) (value : FBIKSolver) (index : Int) : FBIKSolverArray

/-- outputs: (array) -/        
@[apex_node "array::Insert<FBIKTarget>"]
opaque array_InsertFBIKTarget (array : FBIKTargetArray) (value : FBIKTarget) (index : Int) : FBIKTargetArray

/-- outputs: (array) -/        
@[apex_node "array::Insert<Float>"]
opaque array_InsertFloat (array : FloatArray) (value : Float) (index : Int) : FloatArray

/-- outputs: (array) -/        
@[apex_node "array::Insert<Geometry>"]
opaque array_InsertGeometry (array : GeometryArray) (value : Geometry) (index : Int) : GeometryArray

/-- outputs: (array) -/        
@[apex_node "array::Insert<Int>"]
opaque array_InsertInt (array : IntArray) (value : Int) (index : Int) : IntArray

/-- outputs: (array) -/        
@[apex_node "array::Insert<Matrix3>"]
opaque array_InsertMatrix3 (array : Matrix3Array) (value : Matrix3) (index : Int) : Matrix3Array

/-- outputs: (array) -/        
@[apex_node "array::Insert<Matrix4>"]
opaque array_InsertMatrix4 (array : Matrix4Array) (value : Matrix4) (index : Int) : Matrix4Array

/-- outputs: (array) -/        
@[apex_node "array::Insert<SimRootDataId>"]
opaque array_InsertSimRootDataId (array : SimRootDataIdArray) (value : SimRootDataId) (index : Int) : SimRootDataIdArray

/-- outputs: (array) -/        
@[apex_node "array::Insert<String>"]
opaque array_InsertString (array : StringArray) (value : String) (index : Int) : StringArray

/-- outputs: (array) -/        
@[apex_node "array::Insert<Vector2>"]
opaque array_InsertVector2 (array : Vector2Array) (value : Vector2) (index : Int) : Vector2Array

/-- outputs: (array) -/        
@[apex_node "array::Insert<Vector3>"]
opaque array_InsertVector3 (array : Vector3Array) (value : Vector3) (index : Int) : Vector3Array

/-- outputs: (array) -/        
@[apex_node "array::Insert<Vector4>"]
opaque array_InsertVector4 (array : Vector4Array) (value : Vector4) (index : Int) : Vector4Array

/-- outputs: (length) -/        
@[apex_node "array::Length<ApexNodeID>"]
opaque array_LengthApexNodeID (array : ApexNodeIDArray) : Int

/-- outputs: (length) -/        
@[apex_node "array::Length<ApexPortID>"]
opaque array_LengthApexPortID (array : ApexPortIDArray) : Int

/-- outputs: (length) -/        
@[apex_node "array::Length<Bool>"]
opaque array_LengthBool (array : BoolArray) : Int

/-- outputs: (length) -/        
@[apex_node "array::Length<Dict>"]
opaque array_LengthDict (array : DictArray) : Int

/-- outputs: (length) -/        
@[apex_node "array::Length<DynamicPath>"]
opaque array_LengthDynamicPath (array : DynamicPathArray) : Int

/-- outputs: (length) -/        
@[apex_node "array::Length<FBIKSkeleton>"]
opaque array_LengthFBIKSkeleton (array : FBIKSkeletonArray) : Int

/-- outputs: (length) -/        
@[apex_node "array::Length<FBIKSolver>"]
opaque array_LengthFBIKSolver (array : FBIKSolverArray) : Int

/-- outputs: (length) -/        
@[apex_node "array::Length<FBIKTarget>"]
opaque array_LengthFBIKTarget (array : FBIKTargetArray) : Int

/-- outputs: (length) -/        
@[apex_node "array::Length<Float>"]
opaque array_LengthFloat (array : FloatArray) : Int

/-- outputs: (length) -/        
@[apex_node "array::Length<Geometry>"]
opaque array_LengthGeometry (array : GeometryArray) : Int

/-- outputs: (length) -/        
@[apex_node "array::Length<Int>"]
opaque array_LengthInt (array : IntArray) : Int

/-- outputs: (length) -/        
@[apex_node "array::Length<Matrix3>"]
opaque array_LengthMatrix3 (array : Matrix3Array) : Int

/-- outputs: (length) -/        
@[apex_node "array::Length<Matrix4>"]
opaque array_LengthMatrix4 (array : Matrix4Array) : Int

/-- outputs: (length) -/        
@[apex_node "array::Length<SimRootDataId>"]
opaque array_LengthSimRootDataId (array : SimRootDataIdArray) : Int

/-- outputs: (length) -/        
@[apex_node "array::Length<String>"]
opaque array_LengthString (array : StringArray) : Int

/-- outputs: (length) -/        
@[apex_node "array::Length<Vector2>"]
opaque array_LengthVector2 (array : Vector2Array) : Int

/-- outputs: (length) -/        
@[apex_node "array::Length<Vector3>"]
opaque array_LengthVector3 (array : Vector3Array) : Int

/-- outputs: (length) -/        
@[apex_node "array::Length<Vector4>"]
opaque array_LengthVector4 (array : Vector4Array) : Int

/-- outputs: (result) -/        
@[apex_node "array::Lerp<Float>" has_rundata]
opaque array_LerpFloat (a : FloatArray) (b : FloatArray) (biases : FloatArray) : FloatArray

/-- outputs: (result) -/        
@[apex_node "array::Lerp<Matrix3>" has_rundata]
opaque array_LerpMatrix3 (a : Matrix3Array) (b : Matrix3Array) (biases : FloatArray) : Matrix3Array

/-- outputs: (result) -/        
@[apex_node "array::Lerp<Matrix4>" has_rundata]
opaque array_LerpMatrix4 (a : Matrix4Array) (b : Matrix4Array) (biases : FloatArray) : Matrix4Array

/-- outputs: (result) -/        
@[apex_node "array::Lerp<Vector2>" has_rundata]
opaque array_LerpVector2 (a : Vector2Array) (b : Vector2Array) (biases : FloatArray) : Vector2Array

/-- outputs: (result) -/        
@[apex_node "array::Lerp<Vector3>" has_rundata]
opaque array_LerpVector3 (a : Vector3Array) (b : Vector3Array) (biases : FloatArray) : Vector3Array

/-- outputs: (result) -/        
@[apex_node "array::Lerp<Vector4>" has_rundata]
opaque array_LerpVector4 (a : Vector4Array) (b : Vector4Array) (biases : FloatArray) : Vector4Array

/-- outputs: (value,index) -/        
@[apex_node "array::Max<Bool>" has_rundata]
opaque array_MaxBool (array : BoolArray) : Bool×Int

/-- outputs: (value,index) -/        
@[apex_node "array::Max<Float>" has_rundata]
opaque array_MaxFloat (array : FloatArray) : Float×Int

/-- outputs: (value,index) -/        
@[apex_node "array::Max<Int>" has_rundata]
opaque array_MaxInt (array : IntArray) : Int×Int

/-- outputs: (value,index) -/        
@[apex_node "array::Max<String>" has_rundata]
opaque array_MaxString (array : StringArray) : String×Int

/-- outputs: (value,index) -/        
@[apex_node "array::Max<Vector2>" has_rundata]
opaque array_MaxVector2 (array : Vector2Array) : Vector2×Int

/-- outputs: (value,index) -/        
@[apex_node "array::Max<Vector3>" has_rundata]
opaque array_MaxVector3 (array : Vector3Array) : Vector3×Int

/-- outputs: (value,index) -/        
@[apex_node "array::Max<Vector4>" has_rundata]
opaque array_MaxVector4 (array : Vector4Array) : Vector4×Int

/-- outputs: (value,index) -/        
@[apex_node "array::Min<Bool>" has_rundata]
opaque array_MinBool (array : BoolArray) : Bool×Int

/-- outputs: (value,index) -/        
@[apex_node "array::Min<Float>" has_rundata]
opaque array_MinFloat (array : FloatArray) : Float×Int

/-- outputs: (value,index) -/        
@[apex_node "array::Min<Int>" has_rundata]
opaque array_MinInt (array : IntArray) : Int×Int

/-- outputs: (value,index) -/        
@[apex_node "array::Min<String>" has_rundata]
opaque array_MinString (array : StringArray) : String×Int

/-- outputs: (value,index) -/        
@[apex_node "array::Min<Vector2>" has_rundata]
opaque array_MinVector2 (array : Vector2Array) : Vector2×Int

/-- outputs: (value,index) -/        
@[apex_node "array::Min<Vector3>" has_rundata]
opaque array_MinVector3 (array : Vector3Array) : Vector3×Int

/-- outputs: (value,index) -/        
@[apex_node "array::Min<Vector4>" has_rundata]
opaque array_MinVector4 (array : Vector4Array) : Vector4×Int

/-- outputs: (result) -/        
@[apex_node "array::Multiply<Float>" has_rundata]
opaque array_MultiplyFloat (a : FloatArray) (b : FloatArray) : FloatArray

/-- outputs: (result) -/        
@[apex_node "array::Multiply<Int>" has_rundata]
opaque array_MultiplyInt (a : IntArray) (b : IntArray) : IntArray

/-- outputs: (result) -/        
@[apex_node "array::Multiply<Vector2>" has_rundata]
opaque array_MultiplyVector2 (a : Vector2Array) (b : Vector2Array) : Vector2Array

/-- outputs: (result) -/        
@[apex_node "array::Multiply<Vector3>" has_rundata]
opaque array_MultiplyVector3 (a : Vector3Array) (b : Vector3Array) : Vector3Array

/-- outputs: (result) -/        
@[apex_node "array::Multiply<Vector4>" has_rundata]
opaque array_MultiplyVector4 (a : Vector4Array) (b : Vector4Array) : Vector4Array

/-- outputs: (array) -/        
@[apex_node "array::Remove<ApexNodeID>"]
opaque array_RemoveApexNodeID (array : ApexNodeIDArray) (index : Int) : ApexNodeIDArray

/-- outputs: (array) -/        
@[apex_node "array::Remove<ApexPortID>"]
opaque array_RemoveApexPortID (array : ApexPortIDArray) (index : Int) : ApexPortIDArray

/-- outputs: (array) -/        
@[apex_node "array::Remove<Bool>"]
opaque array_RemoveBool (array : BoolArray) (index : Int) : BoolArray

/-- outputs: (array) -/        
@[apex_node "array::Remove<Dict>"]
opaque array_RemoveDict (array : DictArray) (index : Int) : DictArray

/-- outputs: (array) -/        
@[apex_node "array::Remove<DynamicPath>"]
opaque array_RemoveDynamicPath (array : DynamicPathArray) (index : Int) : DynamicPathArray

/-- outputs: (array) -/        
@[apex_node "array::Remove<FBIKSkeleton>"]
opaque array_RemoveFBIKSkeleton (array : FBIKSkeletonArray) (index : Int) : FBIKSkeletonArray

/-- outputs: (array) -/        
@[apex_node "array::Remove<FBIKSolver>"]
opaque array_RemoveFBIKSolver (array : FBIKSolverArray) (index : Int) : FBIKSolverArray

/-- outputs: (array) -/        
@[apex_node "array::Remove<FBIKTarget>"]
opaque array_RemoveFBIKTarget (array : FBIKTargetArray) (index : Int) : FBIKTargetArray

/-- outputs: (array) -/        
@[apex_node "array::Remove<Float>"]
opaque array_RemoveFloat (array : FloatArray) (index : Int) : FloatArray

/-- outputs: (array) -/        
@[apex_node "array::Remove<Geometry>"]
opaque array_RemoveGeometry (array : GeometryArray) (index : Int) : GeometryArray

/-- outputs: (array) -/        
@[apex_node "array::Remove<Int>"]
opaque array_RemoveInt (array : IntArray) (index : Int) : IntArray

/-- outputs: (array) -/        
@[apex_node "array::Remove<Matrix3>"]
opaque array_RemoveMatrix3 (array : Matrix3Array) (index : Int) : Matrix3Array

/-- outputs: (array) -/        
@[apex_node "array::Remove<Matrix4>"]
opaque array_RemoveMatrix4 (array : Matrix4Array) (index : Int) : Matrix4Array

/-- outputs: (array) -/        
@[apex_node "array::Remove<SimRootDataId>"]
opaque array_RemoveSimRootDataId (array : SimRootDataIdArray) (index : Int) : SimRootDataIdArray

/-- outputs: (array) -/        
@[apex_node "array::Remove<String>"]
opaque array_RemoveString (array : StringArray) (index : Int) : StringArray

/-- outputs: (array) -/        
@[apex_node "array::Remove<Vector2>"]
opaque array_RemoveVector2 (array : Vector2Array) (index : Int) : Vector2Array

/-- outputs: (array) -/        
@[apex_node "array::Remove<Vector3>"]
opaque array_RemoveVector3 (array : Vector3Array) (index : Int) : Vector3Array

/-- outputs: (array) -/        
@[apex_node "array::Remove<Vector4>"]
opaque array_RemoveVector4 (array : Vector4Array) (index : Int) : Vector4Array

/-- outputs: (array) -/        
@[apex_node "array::Reverse<ApexNodeID>"]
opaque array_ReverseApexNodeID (array : ApexNodeIDArray) : ApexNodeIDArray

/-- outputs: (array) -/        
@[apex_node "array::Reverse<ApexPortID>"]
opaque array_ReverseApexPortID (array : ApexPortIDArray) : ApexPortIDArray

/-- outputs: (array) -/        
@[apex_node "array::Reverse<Bool>"]
opaque array_ReverseBool (array : BoolArray) : BoolArray

/-- outputs: (array) -/        
@[apex_node "array::Reverse<Dict>"]
opaque array_ReverseDict (array : DictArray) : DictArray

/-- outputs: (array) -/        
@[apex_node "array::Reverse<DynamicPath>"]
opaque array_ReverseDynamicPath (array : DynamicPathArray) : DynamicPathArray

/-- outputs: (array) -/        
@[apex_node "array::Reverse<FBIKSkeleton>"]
opaque array_ReverseFBIKSkeleton (array : FBIKSkeletonArray) : FBIKSkeletonArray

/-- outputs: (array) -/        
@[apex_node "array::Reverse<FBIKSolver>"]
opaque array_ReverseFBIKSolver (array : FBIKSolverArray) : FBIKSolverArray

/-- outputs: (array) -/        
@[apex_node "array::Reverse<FBIKTarget>"]
opaque array_ReverseFBIKTarget (array : FBIKTargetArray) : FBIKTargetArray

/-- outputs: (array) -/        
@[apex_node "array::Reverse<Float>"]
opaque array_ReverseFloat (array : FloatArray) : FloatArray

/-- outputs: (array) -/        
@[apex_node "array::Reverse<Geometry>"]
opaque array_ReverseGeometry (array : GeometryArray) : GeometryArray

/-- outputs: (array) -/        
@[apex_node "array::Reverse<Int>"]
opaque array_ReverseInt (array : IntArray) : IntArray

/-- outputs: (array) -/        
@[apex_node "array::Reverse<Matrix3>"]
opaque array_ReverseMatrix3 (array : Matrix3Array) : Matrix3Array

/-- outputs: (array) -/        
@[apex_node "array::Reverse<Matrix4>"]
opaque array_ReverseMatrix4 (array : Matrix4Array) : Matrix4Array

/-- outputs: (array) -/        
@[apex_node "array::Reverse<SimRootDataId>"]
opaque array_ReverseSimRootDataId (array : SimRootDataIdArray) : SimRootDataIdArray

/-- outputs: (array) -/        
@[apex_node "array::Reverse<String>"]
opaque array_ReverseString (array : StringArray) : StringArray

/-- outputs: (array) -/        
@[apex_node "array::Reverse<Vector2>"]
opaque array_ReverseVector2 (array : Vector2Array) : Vector2Array

/-- outputs: (array) -/        
@[apex_node "array::Reverse<Vector3>"]
opaque array_ReverseVector3 (array : Vector3Array) : Vector3Array

/-- outputs: (array) -/        
@[apex_node "array::Reverse<Vector4>"]
opaque array_ReverseVector4 (array : Vector4Array) : Vector4Array

/-- outputs: (array) -/        
@[apex_node "array::Scale<Float>"]
opaque array_ScaleFloat (array : FloatArray) (scalar : Float) : FloatArray

/-- outputs: (array) -/        
@[apex_node "array::Scale<Int>"]
opaque array_ScaleInt (array : IntArray) (scalar : Int) : IntArray

/-- outputs: (array) -/        
@[apex_node "array::Scale<Matrix3>"]
opaque array_ScaleMatrix3 (array : Matrix3Array) (scalar : Matrix3) : Matrix3Array

/-- outputs: (array) -/        
@[apex_node "array::Scale<Matrix4>"]
opaque array_ScaleMatrix4 (array : Matrix4Array) (scalar : Matrix4) : Matrix4Array

/-- outputs: (array) -/        
@[apex_node "array::Scale<Vector2>"]
opaque array_ScaleVector2 (array : Vector2Array) (scalar : Vector2) : Vector2Array

/-- outputs: (array) -/        
@[apex_node "array::Scale<Vector3>"]
opaque array_ScaleVector3 (array : Vector3Array) (scalar : Vector3) : Vector3Array

/-- outputs: (array) -/        
@[apex_node "array::Scale<Vector4>"]
opaque array_ScaleVector4 (array : Vector4Array) (scalar : Vector4) : Vector4Array

/-- outputs: (array,success) -/        
@[apex_node "array::Set<ApexNodeID>"]
opaque array_SetApexNodeID (array : ApexNodeIDArray) (index : Int) (value : ApexNodeID) : ApexNodeIDArray×Bool

/-- outputs: (array,success) -/        
@[apex_node "array::Set<ApexPortID>"]
opaque array_SetApexPortID (array : ApexPortIDArray) (index : Int) (value : ApexPortID) : ApexPortIDArray×Bool

/-- outputs: (array,success) -/        
@[apex_node "array::Set<Bool>"]
opaque array_SetBool (array : BoolArray) (index : Int) (value : Bool) : BoolArray×Bool

/-- outputs: (array,success) -/        
@[apex_node "array::Set<Dict>"]
opaque array_SetDict (array : DictArray) (index : Int) (value : Dict) : DictArray×Bool

/-- outputs: (array,success) -/        
@[apex_node "array::Set<DynamicPath>"]
opaque array_SetDynamicPath (array : DynamicPathArray) (index : Int) (value : DynamicPath) : DynamicPathArray×Bool

/-- outputs: (array,success) -/        
@[apex_node "array::Set<FBIKSkeleton>"]
opaque array_SetFBIKSkeleton (array : FBIKSkeletonArray) (index : Int) (value : FBIKSkeleton) : FBIKSkeletonArray×Bool

/-- outputs: (array,success) -/        
@[apex_node "array::Set<FBIKSolver>"]
opaque array_SetFBIKSolver (array : FBIKSolverArray) (index : Int) (value : FBIKSolver) : FBIKSolverArray×Bool

/-- outputs: (array,success) -/        
@[apex_node "array::Set<FBIKTarget>"]
opaque array_SetFBIKTarget (array : FBIKTargetArray) (index : Int) (value : FBIKTarget) : FBIKTargetArray×Bool

/-- outputs: (array,success) -/        
@[apex_node "array::Set<Float>"]
opaque array_SetFloat (array : FloatArray) (index : Int) (value : Float) : FloatArray×Bool

/-- outputs: (array,success) -/        
@[apex_node "array::Set<Geometry>"]
opaque array_SetGeometry (array : GeometryArray) (index : Int) (value : Geometry) : GeometryArray×Bool

/-- outputs: (array,success) -/        
@[apex_node "array::Set<Int>"]
opaque array_SetInt (array : IntArray) (index : Int) (value : Int) : IntArray×Bool

/-- outputs: (array,success) -/        
@[apex_node "array::Set<Matrix3>"]
opaque array_SetMatrix3 (array : Matrix3Array) (index : Int) (value : Matrix3) : Matrix3Array×Bool

/-- outputs: (array,success) -/        
@[apex_node "array::Set<Matrix4>"]
opaque array_SetMatrix4 (array : Matrix4Array) (index : Int) (value : Matrix4) : Matrix4Array×Bool

/-- outputs: (array,success) -/        
@[apex_node "array::Set<SimRootDataId>"]
opaque array_SetSimRootDataId (array : SimRootDataIdArray) (index : Int) (value : SimRootDataId) : SimRootDataIdArray×Bool

/-- outputs: (array,success) -/        
@[apex_node "array::Set<String>"]
opaque array_SetString (array : StringArray) (index : Int) (value : String) : StringArray×Bool

/-- outputs: (array,success) -/        
@[apex_node "array::Set<Vector2>"]
opaque array_SetVector2 (array : Vector2Array) (index : Int) (value : Vector2) : Vector2Array×Bool

/-- outputs: (array,success) -/        
@[apex_node "array::Set<Vector3>"]
opaque array_SetVector3 (array : Vector3Array) (index : Int) (value : Vector3) : Vector3Array×Bool

/-- outputs: (array,success) -/        
@[apex_node "array::Set<Vector4>"]
opaque array_SetVector4 (array : Vector4Array) (index : Int) (value : Vector4) : Vector4Array×Bool

/-- outputs: (array) -/        
@[apex_node "array::Sort<Bool>"]
opaque array_SortBool (array : BoolArray) : BoolArray

/-- outputs: (array) -/        
@[apex_node "array::Sort<Float>"]
opaque array_SortFloat (array : FloatArray) : FloatArray

/-- outputs: (array) -/        
@[apex_node "array::Sort<Int>"]
opaque array_SortInt (array : IntArray) : IntArray

/-- outputs: (array) -/        
@[apex_node "array::Sort<String>"]
opaque array_SortString (array : StringArray) : StringArray

/-- outputs: (array) -/        
@[apex_node "array::Sort<Vector2>"]
opaque array_SortVector2 (array : Vector2Array) : Vector2Array

/-- outputs: (array) -/        
@[apex_node "array::Sort<Vector3>"]
opaque array_SortVector3 (array : Vector3Array) : Vector3Array

/-- outputs: (array) -/        
@[apex_node "array::Sort<Vector4>"]
opaque array_SortVector4 (array : Vector4Array) : Vector4Array

/-- outputs: (array) -/        
@[apex_node "array::SortAndRemoveDuplicates<Bool>"]
opaque array_SortAndRemoveDuplicatesBool (array : BoolArray) : BoolArray

/-- outputs: (array) -/        
@[apex_node "array::SortAndRemoveDuplicates<Float>"]
opaque array_SortAndRemoveDuplicatesFloat (array : FloatArray) : FloatArray

/-- outputs: (array) -/        
@[apex_node "array::SortAndRemoveDuplicates<Int>"]
opaque array_SortAndRemoveDuplicatesInt (array : IntArray) : IntArray

/-- outputs: (array) -/        
@[apex_node "array::SortAndRemoveDuplicates<String>"]
opaque array_SortAndRemoveDuplicatesString (array : StringArray) : StringArray

/-- outputs: (array) -/        
@[apex_node "array::SortAndRemoveDuplicates<Vector2>"]
opaque array_SortAndRemoveDuplicatesVector2 (array : Vector2Array) : Vector2Array

/-- outputs: (array) -/        
@[apex_node "array::SortAndRemoveDuplicates<Vector3>"]
opaque array_SortAndRemoveDuplicatesVector3 (array : Vector3Array) : Vector3Array

/-- outputs: (array) -/        
@[apex_node "array::SortAndRemoveDuplicates<Vector4>"]
opaque array_SortAndRemoveDuplicatesVector4 (array : Vector4Array) : Vector4Array

/-- outputs: (result) -/        
@[apex_node "array::Subtract<Float>" has_rundata]
opaque array_SubtractFloat (a : FloatArray) (b : FloatArray) : FloatArray

/-- outputs: (result) -/        
@[apex_node "array::Subtract<Int>" has_rundata]
opaque array_SubtractInt (a : IntArray) (b : IntArray) : IntArray

/-- outputs: (result) -/        
@[apex_node "array::Subtract<Matrix3>" has_rundata]
opaque array_SubtractMatrix3 (a : Matrix3Array) (b : Matrix3Array) : Matrix3Array

/-- outputs: (result) -/        
@[apex_node "array::Subtract<Matrix4>" has_rundata]
opaque array_SubtractMatrix4 (a : Matrix4Array) (b : Matrix4Array) : Matrix4Array

/-- outputs: (result) -/        
@[apex_node "array::Subtract<Vector2>" has_rundata]
opaque array_SubtractVector2 (a : Vector2Array) (b : Vector2Array) : Vector2Array

/-- outputs: (result) -/        
@[apex_node "array::Subtract<Vector3>" has_rundata]
opaque array_SubtractVector3 (a : Vector3Array) (b : Vector3Array) : Vector3Array

/-- outputs: (result) -/        
@[apex_node "array::Subtract<Vector4>" has_rundata]
opaque array_SubtractVector4 (a : Vector4Array) (b : Vector4Array) : Vector4Array

/-- outputs: (result) -/        
@[apex_node "array::Sum<Float>"]
opaque array_SumFloat (array : FloatArray) : Float

/-- outputs: (result) -/        
@[apex_node "array::Sum<Int>"]
opaque array_SumInt (array : IntArray) : Int

/-- outputs: (result) -/        
@[apex_node "array::Sum<Matrix3>"]
opaque array_SumMatrix3 (array : Matrix3Array) : Matrix3

/-- outputs: (result) -/        
@[apex_node "array::Sum<Matrix4>"]
opaque array_SumMatrix4 (array : Matrix4Array) : Matrix4

/-- outputs: (result) -/        
@[apex_node "array::Sum<Vector2>"]
opaque array_SumVector2 (array : Vector2Array) : Vector2

/-- outputs: (result) -/        
@[apex_node "array::Sum<Vector3>"]
opaque array_SumVector3 (array : Vector3Array) : Vector3

/-- outputs: (result) -/        
@[apex_node "array::Sum<Vector4>"]
opaque array_SumVector4 (array : Vector4Array) : Vector4

/-- outputs: (channel) -/        
@[apex_node "ch::AddKey"]
opaque ch_AddKey (channel : AnimChannel) (time : Float) (value : Float) (setvalue : Bool) (autotangents : Bool) : AnimChannel

/-- outputs: (channels) -/        
@[apex_node "ch::AddKeys" has_rundata]
opaque ch_AddKeys (channels : AnimChannelCollection) (parms : Dict) (time : Float) (addmissing : Bool) (autotangents : Bool) : AnimChannelCollection

/-- outputs: (animstack) -/        
@[apex_node "ch::AnimStackAddKey" has_rundata]
opaque ch_AnimStackAddKey (animstack : AnimStack) {numparms: Nat} (parms : VariadicArg Untyped numparms) (time : Float) : AnimStack

/-- outputs: (animstack) -/        
@[apex_node "ch::AnimStackBuild"]
opaque ch_AnimStackBuild (channelgeo : GeometryArray) (layerdata : GeometryArray) (layernames : StringArray) (activelayer : String) : AnimStack

-- special function not supported yet
-- opaque ch::AnimStackEvaluate (animstack : AnimStack) (time : Float) : VariadicArg Untyped...

/-- outputs: (channels) -/        
@[apex_node "ch::ChannelCollectionFromPrims" has_rundata]
opaque ch_ChannelCollectionFromPrims (geo : Geometry) : AnimChannelCollection

/-- outputs: (channel) -/        
@[apex_node "ch::ChannelFromCollection"]
opaque ch_ChannelFromCollection (channels : AnimChannelCollection) (name : String) : AnimChannel

/-- outputs: (channel) -/        
@[apex_node "ch::ChannelFromPrim"]
opaque ch_ChannelFromPrim (geo : Geometry) (primnum : Int) : AnimChannel

/-- outputs: (geo) -/        
@[apex_node "ch::ChannelToPoints"]
opaque ch_ChannelToPoints (geo : Geometry) (channel : AnimChannel) (samplerate : Float) (buildcurve : Bool) : Geometry

/-- outputs: (geo) -/        
@[apex_node "ch::ChannelToPrim"]
opaque ch_ChannelToPrim (geo : Geometry) (channel : AnimChannel) (path : String) (name : String) (default : Float) : Geometry

-- special function not supported yet
-- opaque ch::ChannelsFromCollection (channels : AnimChannelCollection) : VariadicArg AnimChannel...

-- special function not supported yet
-- opaque ch::ChannelsFromPrims (geo : Geometry) : VariadicArg AnimChannel...

/-- outputs: (channels) -/        
@[apex_node "ch::CollectionDeleteKeys"]
opaque ch_CollectionDeleteKeys (channels : AnimChannelCollection) (start : Float) (end' : Float) (invert : Bool) : AnimChannelCollection

/-- outputs: (channels) -/        
@[apex_node "ch::CollectionEulerFilter"]
opaque ch_CollectionEulerFilter (channels : AnimChannelCollection) (rOrd : Int) : AnimChannelCollection

/-- outputs: (channel) -/        
@[apex_node "ch::CollectionGetChannel"]
opaque ch_CollectionGetChannel (channels : AnimChannelCollection) (index : Int) : AnimChannel

/-- outputs: (array) -/        
@[apex_node "ch::CollectionGetChannelArray"]
opaque ch_CollectionGetChannelArray (channels : AnimChannelCollection) : AnimChannelArray

/-- outputs: (length) -/        
@[apex_node "ch::CollectionLength"]
opaque ch_CollectionLength (channels : AnimChannelCollection) : Int

/-- outputs: (name) -/        
@[apex_node "ch::CollectionNameAt"]
opaque ch_CollectionNameAt (channels : AnimChannelCollection) (index : Int) : String

/-- outputs: (names) -/        
@[apex_node "ch::CollectionNames"]
opaque ch_CollectionNames (channels : AnimChannelCollection) : StringArray

/-- outputs: (outchannels) -/        
@[apex_node "ch::CollectionReduceKeys"]
opaque ch_CollectionReduceKeys (inchannels : AnimChannelCollection) (samplerate : Float) (tolerance : Float) (preserveextrema : Bool) : AnimChannelCollection

/-- outputs: (channels) -/        
@[apex_node "ch::CollectionScaleKeys"]
opaque ch_CollectionScaleKeys (channels : AnimChannelCollection) (pivot : Float) (scale : Float) : AnimChannelCollection

/-- outputs: (channels) -/        
@[apex_node "ch::CollectionSetChannel"]
opaque ch_CollectionSetChannel (channels : AnimChannelCollection) (name : String) (channel : AnimChannel) : AnimChannelCollection

/-- outputs: (channels) -/        
@[apex_node "ch::CollectionSetChannelArray"]
opaque ch_CollectionSetChannelArray (channels : AnimChannelCollection) (array : AnimChannelArray) : AnimChannelCollection

/-- outputs: (channels) -/        
@[apex_node "ch::CollectionSetChannels"]
opaque ch_CollectionSetChannels (channels : AnimChannelCollection) {numinputs: Nat} (inputs : VariadicArg AnimChannel numinputs) : AnimChannelCollection

/-- outputs: (channels) -/        
@[apex_node "ch::CollectionShiftKeys"]
opaque ch_CollectionShiftKeys (channels : AnimChannelCollection) (shift : Float) : AnimChannelCollection

/-- outputs: (channels) -/        
@[apex_node "ch::CollectionSmoothAutoTangents"]
opaque ch_CollectionSmoothAutoTangents (channels : AnimChannelCollection) (force : Bool) : AnimChannelCollection

/-- outputs: (channels) -/        
@[apex_node "ch::CollectionSmoothKeys" has_rundata]
opaque ch_CollectionSmoothKeys (channels : AnimChannelCollection) (pattern : String) (cutoff : Float) (order : Int) (rate : Float) (start : Float) (end' : Float) (resample : Bool) (snap : Bool) : AnimChannelCollection

/-- outputs: (channel) -/        
@[apex_node "ch::DeleteKeys"]
opaque ch_DeleteKeys (channel : AnimChannel) (start : Float) (end' : Float) (invert : Bool) : AnimChannel

/-- outputs: (dict) -/        
@[apex_node "ch::DictUpdateFromChannelGeo" has_rundata]
opaque ch_DictUpdateFromChannelGeo (dict : Dict) (geo : Geometry) (renamefrom : String) (renameto : String) (refgraph : ApexGraphHandle) (time : Float) : Dict

/-- outputs: (channelx,channely,channelz) -/        
@[apex_node "ch::EulerFilter"]
opaque ch_EulerFilter (channelx : AnimChannel) (channely : AnimChannel) (channelz : AnimChannel) (rOrd : Int) : AnimChannel×AnimChannel×AnimChannel

/-- outputs: (value) -/        
@[apex_node "ch::Evaluate"]
opaque ch_Evaluate (channel : AnimChannel) (time : Float) : Float

/-- outputs: (outchannel) -/        
@[apex_node "ch::EvaluateLayered"]
opaque ch_EvaluateLayered {numchannels: Nat} (channels : VariadicArg AnimChannel numchannels) (samplerate : Float) : AnimChannel

/-- outputs: (values) -/        
@[apex_node "ch::EvaluateMulti"]
opaque ch_EvaluateMulti (time : Float) {numchannels: Nat} (channels : VariadicArg AnimChannel numchannels) : FloatArray

/-- outputs: (geo) -/        
@[apex_node "ch::PrimsFromChannelCollection"]
opaque ch_PrimsFromChannelCollection (geo : Geometry) (channels : AnimChannelCollection) : Geometry

/-- outputs: (outchannel) -/        
@[apex_node "ch::ReduceKeys"]
opaque ch_ReduceKeys (inchannel : AnimChannel) (samplerate : Float) (tolerance : Float) (preserveextrema : Bool) : AnimChannel

/-- outputs: (channel) -/        
@[apex_node "ch::ScaleKeys"]
opaque ch_ScaleKeys (channel : AnimChannel) (pivot : Float) (scale : Float) : AnimChannel

/-- outputs: (channel) -/        
@[apex_node "ch::ShiftKeys"]
opaque ch_ShiftKeys (channel : AnimChannel) (shift : Float) : AnimChannel

/-- outputs: (channel) -/        
@[apex_node "ch::SmoothAutoTangents"]
opaque ch_SmoothAutoTangents (channel : AnimChannel) (force : Bool) : AnimChannel

/-- outputs: (channel) -/        
@[apex_node "ch::SmoothKeys" has_rundata]
opaque ch_SmoothKeys (channel : AnimChannel) (cutoff : Float) (order : Int) (rate : Float) (start : Float) (end' : Float) (resample : Bool) (snap : Bool) : AnimChannel

-- special function not supported yet
-- opaque component::AddAbstractControl (value : ApexGraphHandle) (value : ApexPortID) (value : String) (value ...

-- special function not supported yet
-- opaque component::AddColorScheme (value : ApexGraphHandle) (value : String) (value : String) (value : String...

-- special function not supported yet
-- opaque component::AddConfigControl (parm : String) (parm : String) (parm : String) (parm : String) (parm : S...

-- special function not supported yet
-- opaque component::AddControlAction (parm : String) (parm : String) (parm : String) (parm : String) (parm : S...

-- special function not supported yet
-- opaque component::AddControlGroup (parm : ApexGraphHandle) (parm : Dict) (parm : Dict) (parm : ApexNodeID) (...

-- special function not supported yet
-- opaque component::AddControlGroupPrimary (value : Matrix4) (value : Matrix4) (a : Matrix4) (a : Matrix4) {nu...

-- special function not supported yet
-- opaque component::AddLodControls (parm : Float) (parm : Float) (parm : Float) (parm : Float) (parm : Float) ...

-- special function not supported yet
-- opaque component::AddSpaceSwitchControl (parm : Dict) (parm : Dict) (parm : Bool) (value : ApexGraphHandle) ...

-- special function not supported yet
-- opaque component::Blendshape (parm : String) (parm : ApexNodeIDArray) (parm : StringArray) (value : ApexGrap...

-- special function not supported yet
-- opaque component::Bonedeform (parm : Dict) (parm : Int) (value : ApexGraphHandle) (value : String) (value : ...

-- special function not supported yet
-- opaque component::Deltamush (graph : ApexGraphHandle) (name : String) (callback : String) (graph : ApexGraph...

-- special function not supported yet
-- opaque component::FBIK (parm : String) (parm : Dict) (value : ApexGraphHandle) (value : Geometry) (value : S...

-- special function not supported yet
-- opaque component::FKIK (value : String) (input : String) (separators : String) (value : ApexGraphHandle) (va...

-- special function not supported yet
-- opaque component::FindSegments (parm : StringArray) (value : ApexGraphHandle) (value : String) (a : String) ...

-- special function not supported yet
-- opaque component::GetRestTransform (condition : Bool) (__spare__ : undefined) (scope : undefined) (__spare__...

-- special function not supported yet
-- opaque component::GetRig (value : Geometry) (value : String) (value : ApexGraphHandle) (value : Bool) (input...

-- special function not supported yet
-- opaque component::GetTransformChildren (input : ApexNodeID) (checkdefault : Bool) (condition : Bool) (__spar...

-- special function not supported yet
-- opaque component::GetTransformDescendants (graph : ApexGraphHandle) (pattern : String) (breadthfirst : Bool)...

-- special function not supported yet
-- opaque component::GetTransformParent (input : ApexNodeID) (checkdefault : Bool) (condition : Bool) (__spare_...

-- special function not supported yet
-- opaque component::InsertTransformParent (input : ApexNodeID) (checkdefault : Bool) (parm : String) (conditio...

-- special function not supported yet
-- opaque component::Lookat (next : undefined) {numentries: Nat} (entries : VariadicArg Bool numentries) (entri...

-- special function not supported yet
-- opaque component::MappedConstraints (value : ApexGraphHandle) (value : String) (value : String) (value : Str...

-- special function not supported yet
-- opaque component::MultiIk (value : ApexGraphHandle) (value : Geometry) (value : String) (value : String) (va...

-- special function not supported yet
-- opaque component::MultiIk::2.0 (value : ApexGraphHandle) (value : Geometry) (value : String) (value : String...

-- special function not supported yet
-- opaque component::ProcessSegment (value : String) (value : String) (string : String) (find : String) (casese...

-- special function not supported yet
-- opaque component::Rename (parm : String) (parm : String) (parm : String) (parm : ApexNodeIDArray) (value : A...

-- special function not supported yet
-- opaque component::ReverseFoot (value : ApexGraphHandle) (value : ApexNodeID) (value : String) (value : Vecto...

-- special function not supported yet
-- opaque component::ReverseFoot::2.0 (value : ApexGraphHandle) (value : ApexNodeID) (value : String) (value : ...

-- special function not supported yet
-- opaque component::SetRestTransform (graph : ApexGraphHandle) (nodeid : ApexNodeID) (name : String) (callback...

-- special function not supported yet
-- opaque component::SetRestTransformFromSkel (graph : ApexGraphHandle) (nodeid : ApexNodeID) (parm : Bool) (pa...

-- special function not supported yet
-- opaque component::SetTransformParent (graph : ApexGraphHandle) (nodeid : ApexNodeID) (input : ApexNodeID) (c...

-- special function not supported yet
-- opaque component::Spline (parm : StringArray) (value : ApexGraphHandle) (value : Geometry) (value : String) ...

-- special function not supported yet
-- opaque component::Spline2 (parm : StringArray) (parm : StringArray) (value : Float) (value : Matrix4) (m : M...

-- special function not supported yet
-- opaque component::Spline::2.0 (parm : StringArray) (parm : Matrix4Array) (parm : Matrix4Array) (parm : Strin...

-- special function not supported yet
-- opaque component::TransformDriver (array : DictArray) (iterations : Int) (__spare__ : undefined) (array : Di...

-- special function not supported yet
-- opaque component::Twist (parm : ApexNodeIDArray) (parm : String) (parm : Int) (parm : Vector3) (parm : Dict)...

-- special function not supported yet
-- opaque component::UpdateControls (value : ApexGraphHandle) (value : ApexNodeID) (value : String) (value : Ve...

-- special function not supported yet
-- opaque component::UpdateParmLimits (parm : String) (parm : String) (value : ApexGraphHandle) (value : ApexNo...

-- special function not supported yet
-- opaque component::UpdateRig (parm : Dict) (parm : String) (parm : Int) (value : Geometry) (value : String) (...

-- special function not supported yet
-- opaque componentgadget::ReferenceSkeleton (value : Geometry) (value : String) (value : Matrix4) (value : Vec...

-- special function not supported yet
-- opaque controldrawable::SkinControl (parm : String) (parm : String) (value : Geometry) (value : Geometry) (p...

/-- outputs: (value,value,value,b,result,result,value,visibility,value) -/        
@[apex_node "controldrawable::UpdateVisibility"]
opaque controldrawable_UpdateVisibility (parm : Int) (value : Int) (value : Bool) (a : Bool) (a : Int) {numb: Nat} (b : VariadicArg Int numb) {numinputs: Nat} (inputs : VariadicArg Int numinputs) (b2 : Int) (inputs0 : Int) (inputs1 : Int) (value : Int) (value : Int) (visibility : Bool) : Int×Int×Bool×Int×Int×Int×Int×Bool×Int

-- special function not supported yet
-- opaque controlgadget::ClosestRayJoint (parm : Float) (parm : String) (parm : String) (value : Geometry) (val...

-- special function not supported yet
-- opaque controlgadget::MultiXformGadget (parm : Int) (x : Float) (y : Float) (z : Float) (parm : Vector3) (va...

-- special function not supported yet
-- opaque controlgadget::SnapXFormToAxes (parm : Float) (value : Matrix4) (value : Matrix4) (value : Matrix4) (...

-- special function not supported yet
-- opaque controlgadget::TagJointVisualizer (parm : Geometry) (value : Geometry) (value : String) (value : Dict...

-- special function not supported yet
-- opaque controlgadget::TwistJointVisualizer (parm : String) (parm : String) (parm : String) (parm : Float) (p...

/-- outputs: (geo) -/        
@[apex_node "cop::LayersToGeo"]
opaque cop_LayersToGeo {numlayers: Nat} (layers : VariadicArg ImageLayer numlayers) {numgeos: Nat} (geos : VariadicArg Geometry numgeos) {numvdbs: Nat} (vdbs : VariadicArg NanoVDB numvdbs) : Geometry

/-- outputs: (stereogram_0) -/        
@[apex_node "cop::autostereogram" has_rundata]
opaque cop_autostereogram (source_0 : ImageLayer) (depth_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

-- special function not supported yet
-- opaque cop::average<0> {numinput_variadic_geo: Nat} (input_variadic_geo : VariadicArg Geometry numinput_vari...

-- special function not supported yet
-- opaque cop::average<1> {numinput_variadic_geo: Nat} (input_variadic_geo : VariadicArg Geometry numinput_vari...

/-- outputs: (blend_0) -/        
@[apex_node "cop::blend<0>" has_rundata]
opaque cop_blend0 (bg_0 : ImageLayer) (fg_1 : ImageLayer) (mask_2 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (blend_0) -/        
@[apex_node "cop::blend<1>" has_rundata]
opaque cop_blend1 (bg_0 : NanoVDB) (fg_1 : NanoVDB) (mask_2 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

/-- outputs: (program_0) -/        
@[apex_node "cop::blocktogeo" has_rundata]
opaque cop_blocktogeo (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry

/-- outputs: (blur_0) -/        
@[apex_node "cop::blur<0>" has_rundata]
opaque cop_blur0 (source_0 : ImageLayer) (size_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (blur_0) -/        
@[apex_node "cop::blur<1>" has_rundata]
opaque cop_blur1 (source_0 : NanoVDB) (size_1 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

/-- outputs: (bright_0) -/        
@[apex_node "cop::bright<0>" has_rundata]
opaque cop_bright0 (source_0 : ImageLayer) (bright_1 : ImageLayer) (shift_2 : ImageLayer) (mask_3 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (bright_0) -/        
@[apex_node "cop::bright<1>" has_rundata]
opaque cop_bright1 (source_0 : NanoVDB) (bright_1 : NanoVDB) (shift_2 : NanoVDB) (mask_3 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

/-- outputs: (camera_ref_0) -/        
@[apex_node "cop::cameraimport" has_rundata]
opaque cop_cameraimport (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (camera_ref_0) -/        
@[apex_node "cop::cameraproperties" has_rundata]
opaque cop_cameraproperties (source_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (channel_0) -/        
@[apex_node "cop::channelextract<0>" has_rundata]
opaque cop_channelextract0 (channels_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (channel_0) -/        
@[apex_node "cop::channelextract<1>" has_rundata]
opaque cop_channelextract1 (channels_0 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

/-- outputs: (rgba_0) -/        
@[apex_node "cop::channeljoin<0>" has_rundata]
opaque cop_channeljoin0 (red_0 : ImageLayer) (green_1 : ImageLayer) (blue_2 : ImageLayer) (alpha_3 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (rgba_0) -/        
@[apex_node "cop::channeljoin<1>" has_rundata]
opaque cop_channeljoin1 (red_0 : NanoVDB) (green_1 : NanoVDB) (blue_2 : NanoVDB) (alpha_3 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

/-- outputs: (red_0,green_1,blue_2,alpha_3) -/        
@[apex_node "cop::channelsplit<0>" has_rundata]
opaque cop_channelsplit0 (rgba_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer×ImageLayer×ImageLayer×ImageLayer

/-- outputs: (red_0,green_1,blue_2,alpha_3) -/        
@[apex_node "cop::channelsplit<1>" has_rundata]
opaque cop_channelsplit1 (rgba_0 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB×NanoVDB×NanoVDB×NanoVDB

/-- outputs: (swap_0) -/        
@[apex_node "cop::channelswap<0>" has_rundata]
opaque cop_channelswap0 (channels_0 : ImageLayer) (mask_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (swap_0) -/        
@[apex_node "cop::channelswap<1>" has_rundata]
opaque cop_channelswap1 (channels_0 : NanoVDB) (mask_1 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

/-- outputs: (checkerboard_0) -/        
@[apex_node "cop::checkerboard" has_rundata]
opaque cop_checkerboard (size_ref_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (clamp_0) -/        
@[apex_node "cop::clamp<0>" has_rundata]
opaque cop_clamp0 (source_0 : ImageLayer) (lower_1 : ImageLayer) (upper_2 : ImageLayer) (mask_3 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (clamp_0) -/        
@[apex_node "cop::clamp<1>" has_rundata]
opaque cop_clamp1 (source_0 : NanoVDB) (lower_1 : NanoVDB) (upper_2 : NanoVDB) (mask_3 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

/-- outputs: (constant_0) -/        
@[apex_node "cop::constant<0>" has_rundata]
opaque cop_constant0 (source_0 : ImageLayer) (mask_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (constant_0) -/        
@[apex_node "cop::constant<1>" has_rundata]
opaque cop_constant1 (source_0 : NanoVDB) (mask_1 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

-- special function not supported yet
-- opaque cop::contactsheet {numinput_variadic_geo: Nat} (input_variadic_geo : VariadicArg Geometry numinput_va...

/-- outputs: (crop_0) -/        
@[apex_node "cop::crop" has_rundata]
opaque cop_crop (source_0 : ImageLayer) (size_ref_1 : ImageLayer) (bounds_2 : Geometry) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

-- special function not supported yet
-- opaque cop::cryptomatte (crypto1_0 : ImageLayer) {numinput_variadic_geo: Nat} (input_variadic_geo : Variadic...

/-- outputs: (denoise_0) -/        
@[apex_node "cop::denoiseai" has_rundata]
opaque cop_denoiseai (source_0 : ImageLayer) (normal_1 : ImageLayer) (albedo_2 : ImageLayer) (motionvec_3 : ImageLayer) (prevframe_4 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (dCdx_0,dCdy_1) -/        
@[apex_node "cop::derivative" has_rundata]
opaque cop_derivative (source_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer×ImageLayer

/-- outputs: (distort_0) -/        
@[apex_node "cop::distort" has_rundata]
opaque cop_distort (source_0 : ImageLayer) (dir_1 : ImageLayer) (scale_2 : ImageLayer) (mask_3 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (dot_0) -/        
@[apex_node "cop::dot<0>" has_rundata]
opaque cop_dot0 (a_0 : ImageLayer) (b_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (dot_0) -/        
@[apex_node "cop::dot<1>" has_rundata]
opaque cop_dot1 (a_0 : NanoVDB) (b_1 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

/-- outputs: (sdf_0) -/        
@[apex_node "cop::eikonal" has_rundata]
opaque cop_eikonal (initial_dist_0 : ImageLayer) (speed_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (output1_0) -/        
@[apex_node "cop::error<0>" has_rundata]
opaque cop_error0 (input1_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (output1_0) -/        
@[apex_node "cop::error<1>" has_rundata]
opaque cop_error1 (input1_0 : Geometry) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry

/-- outputs: (output1_0) -/        
@[apex_node "cop::error<2>" has_rundata]
opaque cop_error2 (input1_0 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

-- special function not supported yet
-- opaque cop::file (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (output_...

/-- outputs: (flip_0) -/        
@[apex_node "cop::flip" has_rundata]
opaque cop_flip (source_0 : ImageLayer) (mask_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (function_0) -/        
@[apex_node "cop::function<0>" has_rundata]
opaque cop_function0 (source_0 : ImageLayer) (mask_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (function_0) -/        
@[apex_node "cop::function<1>" has_rundata]
opaque cop_function1 (source_0 : NanoVDB) (mask_1 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

/-- outputs: (layer_0) -/        
@[apex_node "cop::geotolayer" has_rundata]
opaque cop_geotolayer (geometry_0 : Geometry) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

-- special function not supported yet
-- opaque cop::geotolayer::2.0 (geometry_0 : Geometry) (size_ref_1 : ImageLayer) (signature : StringArray) (cwd...

/-- outputs: (hextex_0,weights_1) -/        
@[apex_node "cop::hextile" has_rundata]
opaque cop_hextile (direction_0 : ImageLayer) (texcoord_1 : ImageLayer) (weight_2 : ImageLayer) (textotile_3 : ImageLayer) (size_4 : ImageLayer) (scale_5 : ImageLayer) (rot_6 : ImageLayer) (contrast_7 : ImageLayer) (contrast_falloff_8 : ImageLayer) (weightexp_9 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer×ImageLayer

/-- outputs: (hsv_adjust_0) -/        
@[apex_node "cop::hsv" has_rundata]
opaque cop_hsv (source_0 : ImageLayer) (hueshift_1 : ImageLayer) (saturation_2 : ImageLayer) (value_3 : ImageLayer) (mask_4 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (rgb_0) -/        
@[apex_node "cop::idtorgb<0>" has_rundata]
opaque cop_idtorgb0 (id_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (rgb_0) -/        
@[apex_node "cop::idtorgb<1>" has_rundata]
opaque cop_idtorgb1 (id_0 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

/-- outputs: (clean_0) -/        
@[apex_node "cop::illpixel" has_rundata]
opaque cop_illpixel (source_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (invert_0) -/        
@[apex_node "cop::invert<0>" has_rundata]
opaque cop_invert0 (source_0 : ImageLayer) (mask_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (invert_0) -/        
@[apex_node "cop::invert<1>" has_rundata]
opaque cop_invert1 (source_0 : NanoVDB) (mask_1 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

-- special function not supported yet
-- opaque cop::invokeblock {numinput_variadic_geo: Nat} (input_variadic_geo : VariadicArg Geometry numinput_var...

-- special function not supported yet
-- opaque cop::invokegeo (input1_0 : Geometry) {numinput_variadic_geo: Nat} (input_variadic_geo : VariadicArg G...

/-- outputs: (fractal_0) -/        
@[apex_node "cop::julia" has_rundata]
opaque cop_julia (size_ref_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (layer_0) -/        
@[apex_node "cop::layer" has_rundata]
opaque cop_layer (size_ref_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (layer_0) -/        
@[apex_node "cop::layerattribcreate" has_rundata]
opaque cop_layerattribcreate (source_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (layer_0) -/        
@[apex_node "cop::layerattribdelete" has_rundata]
opaque cop_layerattribdelete (source_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (layer_0) -/        
@[apex_node "cop::layerproperties<0>" has_rundata]
opaque cop_layerproperties0 (source_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (layer_0) -/        
@[apex_node "cop::layerproperties<1>" has_rundata]
opaque cop_layerproperties1 (source_0 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

/-- outputs: (geometry_0) -/        
@[apex_node "cop::layertogeo<0>" has_rundata]
opaque cop_layertogeo0 (layer_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry

/-- outputs: (geometry_0) -/        
@[apex_node "cop::layertogeo<1>" has_rundata]
opaque cop_layertogeo1 (layer_0 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry

/-- outputs: (points_0) -/        
@[apex_node "cop::layertopoints" has_rundata]
opaque cop_layertopoints (layer_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry

-- special function not supported yet
-- opaque cop::livevideo (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (ou...

/-- outputs: (result_0) -/        
@[apex_node "cop::matchcamera" has_rundata]
opaque cop_matchcamera (source_0 : ImageLayer) (camera_ref_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (result_0) -/        
@[apex_node "cop::matchudim" has_rundata]
opaque cop_matchudim (source_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (median_0) -/        
@[apex_node "cop::median" has_rundata]
opaque cop_median (source_0 : ImageLayer) (mask_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (mono_0) -/        
@[apex_node "cop::mono<0>" has_rundata]
opaque cop_mono0 (source_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (mono_0) -/        
@[apex_node "cop::mono<1>" has_rundata]
opaque cop_mono1 (source_0 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

/-- outputs: (rgb_0) -/        
@[apex_node "cop::monotorgb<0>" has_rundata]
opaque cop_monotorgb0 (source_0 : ImageLayer) (ramp_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (rgb_0) -/        
@[apex_node "cop::monotorgb<1>" has_rundata]
opaque cop_monotorgb1 (source_0 : NanoVDB) (ramp_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

-- special function not supported yet
-- opaque cop::onnx (input1_0 : ImageLayer) {numinput_variadic_geo: Nat} (input_variadic_geo : VariadicArg Geom...

-- special function not supported yet
-- opaque cop::opencl {numinput_variadic_geo: Nat} (input_variadic_geo : VariadicArg Geometry numinput_variadic...

/-- outputs: (premult_0) -/        
@[apex_node "cop::premult" has_rundata]
opaque cop_premult (source_0 : ImageLayer) (mask_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (result_0) -/        
@[apex_node "cop::projectonlayer" has_rundata]
opaque cop_projectonlayer (target_0 : ImageLayer) (source_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

-- special function not supported yet
-- opaque cop::pythonsnippet {numinput_variadic_geo: Nat} (input_variadic_geo : VariadicArg Geometry numinput_v...

/-- outputs: (ramp_0) -/        
@[apex_node "cop::ramp" has_rundata]
opaque cop_ramp (size_ref_0 : ImageLayer) (pos_1 : ImageLayer) (ramp_2 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

-- special function not supported yet
-- opaque cop::rasterizecurves (camera_ref_0 : ImageLayer) (curves_1 : Geometry) (signature : StringArray) (cwd...

-- special function not supported yet
-- opaque cop::rasterizegeo (camera_ref_0 : ImageLayer) (geometry_1 : Geometry) (signature : StringArray) (cwd_...

/-- outputs: (result_0,depth_1) -/        
@[apex_node "cop::rasterizelayer" has_rundata]
opaque cop_rasterizelayer (camera_ref_0 : ImageLayer) (source_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer×ImageLayer

-- special function not supported yet
-- opaque cop::raytrace (geometry_0 : Geometry) (origins_1 : ImageLayer) (directions_2 : ImageLayer) (tracesets...

/-- outputs: (remap_0) -/        
@[apex_node "cop::remap<0>" has_rundata]
opaque cop_remap0 (source_0 : ImageLayer) (ramp_1 : ImageLayer) (mask_2 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (remap_0) -/        
@[apex_node "cop::remap<1>" has_rundata]
opaque cop_remap1 (source_0 : NanoVDB) (ramp_1 : ImageLayer) (mask_2 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

/-- outputs: (resample_0) -/        
@[apex_node "cop::resample" has_rundata]
opaque cop_resample (source_0 : ImageLayer) (size_ref_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (rgb_0,alpha_1) -/        
@[apex_node "cop::rgbatorgb" has_rundata]
opaque cop_rgbatorgb (rgba_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer×ImageLayer

/-- outputs: (rgba_0) -/        
@[apex_node "cop::rgbtorgba" has_rundata]
opaque cop_rgbtorgba (rgb_0 : ImageLayer) (alpha_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

-- special function not supported yet
-- opaque cop::sequenceblend (blend_0 : ImageLayer) (image1_1 : ImageLayer) {numinput_variadic_geo: Nat} (input...

-- special function not supported yet
-- opaque cop::slapcompimport {numinput_variadic_geo: Nat} (input_variadic_geo : VariadicArg Geometry numinput_...

/-- outputs: (solution_0) -/        
@[apex_node "cop::solvepoissonmultigrid" has_rundata]
opaque cop_solvepoissonmultigrid (rhs_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (geometry_0) -/        
@[apex_node "cop::sopimport" has_rundata]
opaque cop_sopimport (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry

-- special function not supported yet
-- opaque cop::sopinvoke {numinput_variadic_geo: Nat} (input_variadic_geo : VariadicArg Geometry numinput_varia...

-- special function not supported yet
-- opaque cop::sopinvokegraph (graph_0 : Geometry) {numinput_variadic_geo: Nat} (input_variadic_geo : VariadicA...

-- special function not supported yet
-- opaque cop::stamppoint (size_ref_0 : ImageLayer) (points_1 : Geometry) (clipshape_2 : ImageLayer) (pointmask...

/-- outputs: (stash_0) -/        
@[apex_node "cop::stash<0>" has_rundata]
opaque cop_stash0 (source_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (stash_0) -/        
@[apex_node "cop::stash<1>" has_rundata]
opaque cop_stash1 (source_0 : Geometry) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry

/-- outputs: (stash_0) -/        
@[apex_node "cop::stash<2>" has_rundata]
opaque cop_stash2 (source_0 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

/-- outputs: (blur_0) -/        
@[apex_node "cop::streakblur" has_rundata]
opaque cop_streakblur (source_0 : ImageLayer) (direction_1 : ImageLayer) (scalelength_2 : ImageLayer) (mask_3 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

-- special function not supported yet
-- opaque cop::switch<0> {numinput_variadic_geo: Nat} (input_variadic_geo : VariadicArg Geometry numinput_varia...

-- special function not supported yet
-- opaque cop::switch<1> {numinput_variadic_geo: Nat} (input_variadic_geo : VariadicArg Geometry numinput_varia...

-- special function not supported yet
-- opaque cop::switch<2> {numinput_variadic_geo: Nat} (input_variadic_geo : VariadicArg Geometry numinput_varia...

-- special function not supported yet
-- opaque cop::switchbytype<0> (type_ref_0 : ImageLayer) {numinput_variadic_geo: Nat} (input_variadic_geo : Var...

-- special function not supported yet
-- opaque cop::switchbytype<1> (type_ref_0 : ImageLayer) {numinput_variadic_geo: Nat} (input_variadic_geo : Var...

-- special function not supported yet
-- opaque cop::switchbytype<2> (type_ref_0 : Geometry) {numinput_variadic_geo: Nat} (input_variadic_geo : Varia...

-- special function not supported yet
-- opaque cop::switchbytype<3> (type_ref_0 : Geometry) {numinput_variadic_geo: Nat} (input_variadic_geo : Varia...

-- special function not supported yet
-- opaque cop::switchbytype<4> (type_ref_0 : NanoVDB) {numinput_variadic_geo: Nat} (input_variadic_geo : Variad...

-- special function not supported yet
-- opaque cop::switchbytype<5> (type_ref_0 : NanoVDB) {numinput_variadic_geo: Nat} (input_variadic_geo : Variad...

-- special function not supported yet
-- opaque cop::switchbytype<6> (type_ref_0 : NanoVDB) {numinput_variadic_geo: Nat} (input_variadic_geo : Variad...

-- special function not supported yet
-- opaque cop::switchbytype<7> (type_ref_0 : ImageLayer) {numinput_variadic_geo: Nat} (input_variadic_geo : Var...

-- special function not supported yet
-- opaque cop::switchbytype<8> (type_ref_0 : Geometry) {numinput_variadic_geo: Nat} (input_variadic_geo : Varia...

-- special function not supported yet
-- opaque cop::switchifwired<0> {numinput_variadic_geo: Nat} (input_variadic_geo : VariadicArg Geometry numinpu...

-- special function not supported yet
-- opaque cop::switchifwired<1> {numinput_variadic_geo: Nat} (input_variadic_geo : VariadicArg Geometry numinpu...

-- special function not supported yet
-- opaque cop::switchifwired<2> {numinput_variadic_geo: Nat} (input_variadic_geo : VariadicArg Geometry numinpu...

/-- outputs: (chosen_0) -/        
@[apex_node "cop::twowayswitchifwired<0>" has_rundata]
opaque cop_twowayswitchifwired0 (test_0 : ImageLayer) (first_1 : ImageLayer) (second_2 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (chosen_0) -/        
@[apex_node "cop::twowayswitchifwired<1>" has_rundata]
opaque cop_twowayswitchifwired1 (test_0 : ImageLayer) (first_1 : Geometry) (second_2 : Geometry) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry

/-- outputs: (chosen_0) -/        
@[apex_node "cop::twowayswitchifwired<2>" has_rundata]
opaque cop_twowayswitchifwired2 (test_0 : Geometry) (first_1 : ImageLayer) (second_2 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (chosen_0) -/        
@[apex_node "cop::twowayswitchifwired<3>" has_rundata]
opaque cop_twowayswitchifwired3 (test_0 : Geometry) (first_1 : Geometry) (second_2 : Geometry) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry

/-- outputs: (chosen_0) -/        
@[apex_node "cop::twowayswitchifwired<4>" has_rundata]
opaque cop_twowayswitchifwired4 (test_0 : NanoVDB) (first_1 : NanoVDB) (second_2 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

/-- outputs: (chosen_0) -/        
@[apex_node "cop::twowayswitchifwired<5>" has_rundata]
opaque cop_twowayswitchifwired5 (test_0 : NanoVDB) (first_1 : Geometry) (second_2 : Geometry) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry

/-- outputs: (chosen_0) -/        
@[apex_node "cop::twowayswitchifwired<6>" has_rundata]
opaque cop_twowayswitchifwired6 (test_0 : NanoVDB) (first_1 : ImageLayer) (second_2 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (chosen_0) -/        
@[apex_node "cop::twowayswitchifwired<7>" has_rundata]
opaque cop_twowayswitchifwired7 (test_0 : ImageLayer) (first_1 : NanoVDB) (second_2 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

/-- outputs: (chosen_0) -/        
@[apex_node "cop::twowayswitchifwired<8>" has_rundata]
opaque cop_twowayswitchifwired8 (test_0 : Geometry) (first_1 : NanoVDB) (second_2 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

-- special function not supported yet
-- opaque cop::vdbactivatefrompoints (refvdb_0 : NanoVDB) (activate_pts1_1 : Geometry) {numinput_variadic_geo: ...

/-- outputs: (points_0) -/        
@[apex_node "cop::vdbleafpoints" has_rundata]
opaque cop_vdbleafpoints (vdb_0 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry

/-- outputs: (reshape_0) -/        
@[apex_node "cop::vdbreshape" has_rundata]
opaque cop_vdbreshape (source_0 : NanoVDB) (ref_1 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

/-- outputs: (transform_0) -/        
@[apex_node "cop::vectorxform2d" has_rundata]
opaque cop_vectorxform2d (source_0 : ImageLayer) (mask_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (transform_0) -/        
@[apex_node "cop::vectorxform<0>" has_rundata]
opaque cop_vectorxform0 (source_0 : ImageLayer) (mask_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (transform_0) -/        
@[apex_node "cop::vectorxform<1>" has_rundata]
opaque cop_vectorxform1 (source_0 : NanoVDB) (mask_1 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

-- special function not supported yet
-- opaque cop::wrangle {numinput_variadic_geo: Nat} (input_variadic_geo : VariadicArg Geometry numinput_variadi...

/-- outputs: (transform_0) -/        
@[apex_node "cop::xform2d" has_rundata]
opaque cop_xform2d (source_0 : ImageLayer) (mask_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (transform_0) -/        
@[apex_node "cop::xform<0>" has_rundata]
opaque cop_xform0 (source_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer

/-- outputs: (transform_0) -/        
@[apex_node "cop::xform<1>" has_rundata]
opaque cop_xform1 (source_0 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB

/-- outputs: (transform_0) -/        
@[apex_node "cop::xform<2>" has_rundata]
opaque cop_xform2 (source_0 : Geometry) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry

/-- outputs: (dict) -/        
@[apex_node "dict::Build" has_rundata]
opaque dict_Build (dict : Dict) {numargs: Nat} (args : VariadicArg Untyped numargs) : Dict

/-- outputs: (success) -/        
@[apex_node "dict::Contains"]
opaque dict_Contains (dict : Dict) (key : String) : Bool

/-- outputs: (dataids) -/        
@[apex_node "dict::DebugDataIds"]
opaque dict_DebugDataIds (dict : Dict) : Dict

-- special function not supported yet
-- opaque dict::Extract (dict : Dict) : VariadicArg Untyped...

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<AnimChannel>"]
opaque dict_GetAnimChannel (dict : Dict) (key : String) (default : AnimChannel) : AnimChannel×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<AnimChannelArray>"]
opaque dict_GetAnimChannelArray (dict : Dict) (key : String) (default : AnimChannelArray) : AnimChannelArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<AnimChannelCollection>"]
opaque dict_GetAnimChannelCollection (dict : Dict) (key : String) (default : AnimChannelCollection) : AnimChannelCollection×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<AnimChannelCollectionArray>"]
opaque dict_GetAnimChannelCollectionArray (dict : Dict) (key : String) (default : AnimChannelCollectionArray) : AnimChannelCollectionArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<AnimStack>"]
opaque dict_GetAnimStack (dict : Dict) (key : String) (default : AnimStack) : AnimStack×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<AnimStackArray>"]
opaque dict_GetAnimStackArray (dict : Dict) (key : String) (default : AnimStackArray) : AnimStackArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<ApexGraphHandle>"]
opaque dict_GetApexGraphHandle (dict : Dict) (key : String) (default : ApexGraphHandle) : ApexGraphHandle×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<ApexGraphHandleArray>"]
opaque dict_GetApexGraphHandleArray (dict : Dict) (key : String) (default : ApexGraphHandleArray) : ApexGraphHandleArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<ApexNodeID>"]
opaque dict_GetApexNodeID (dict : Dict) (key : String) (default : ApexNodeID) : ApexNodeID×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<ApexNodeIDArray>"]
opaque dict_GetApexNodeIDArray (dict : Dict) (key : String) (default : ApexNodeIDArray) : ApexNodeIDArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<ApexPortID>"]
opaque dict_GetApexPortID (dict : Dict) (key : String) (default : ApexPortID) : ApexPortID×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<ApexPortIDArray>"]
opaque dict_GetApexPortIDArray (dict : Dict) (key : String) (default : ApexPortIDArray) : ApexPortIDArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<Bool>"]
opaque dict_GetBool (dict : Dict) (key : String) (default : Bool) : Bool×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<BoolArray>"]
opaque dict_GetBoolArray (dict : Dict) (key : String) (default : BoolArray) : BoolArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<ColorRamp>"]
opaque dict_GetColorRamp (dict : Dict) (key : String) (default : ColorRamp) : ColorRamp×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<ColorRampArray>"]
opaque dict_GetColorRampArray (dict : Dict) (key : String) (default : ColorRampArray) : ColorRampArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<Dict>"]
opaque dict_GetDict (dict : Dict) (key : String) (default : Dict) : Dict×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<DictArray>"]
opaque dict_GetDictArray (dict : Dict) (key : String) (default : DictArray) : DictArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<DynamicPath>"]
opaque dict_GetDynamicPath (dict : Dict) (key : String) (default : DynamicPath) : DynamicPath×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<DynamicPathArray>"]
opaque dict_GetDynamicPathArray (dict : Dict) (key : String) (default : DynamicPathArray) : DynamicPathArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<FBIKSkeleton>"]
opaque dict_GetFBIKSkeleton (dict : Dict) (key : String) (default : FBIKSkeleton) : FBIKSkeleton×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<FBIKSkeletonArray>"]
opaque dict_GetFBIKSkeletonArray (dict : Dict) (key : String) (default : FBIKSkeletonArray) : FBIKSkeletonArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<FBIKSolver>"]
opaque dict_GetFBIKSolver (dict : Dict) (key : String) (default : FBIKSolver) : FBIKSolver×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<FBIKSolverArray>"]
opaque dict_GetFBIKSolverArray (dict : Dict) (key : String) (default : FBIKSolverArray) : FBIKSolverArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<FBIKTarget>"]
opaque dict_GetFBIKTarget (dict : Dict) (key : String) (default : FBIKTarget) : FBIKTarget×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<FBIKTargetArray>"]
opaque dict_GetFBIKTargetArray (dict : Dict) (key : String) (default : FBIKTargetArray) : FBIKTargetArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<Float>"]
opaque dict_GetFloat (dict : Dict) (key : String) (default : Float) : Float×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<FloatArray>"]
opaque dict_GetFloatArray (dict : Dict) (key : String) (default : FloatArray) : FloatArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<FloatRamp>"]
opaque dict_GetFloatRamp (dict : Dict) (key : String) (default : FloatRamp) : FloatRamp×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<FloatRampArray>"]
opaque dict_GetFloatRampArray (dict : Dict) (key : String) (default : FloatRampArray) : FloatRampArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<Geometry>"]
opaque dict_GetGeometry (dict : Dict) (key : String) (default : Geometry) : Geometry×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<GeometryArray>"]
opaque dict_GetGeometryArray (dict : Dict) (key : String) (default : GeometryArray) : GeometryArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<Int>"]
opaque dict_GetInt (dict : Dict) (key : String) (default : Int) : Int×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<IntArray>"]
opaque dict_GetIntArray (dict : Dict) (key : String) (default : IntArray) : IntArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<Matrix3>"]
opaque dict_GetMatrix3 (dict : Dict) (key : String) (default : Matrix3) : Matrix3×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<Matrix3Array>"]
opaque dict_GetMatrix3Array (dict : Dict) (key : String) (default : Matrix3Array) : Matrix3Array×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<Matrix4>"]
opaque dict_GetMatrix4 (dict : Dict) (key : String) (default : Matrix4) : Matrix4×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<Matrix4Array>"]
opaque dict_GetMatrix4Array (dict : Dict) (key : String) (default : Matrix4Array) : Matrix4Array×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<SimRootDataId>"]
opaque dict_GetSimRootDataId (dict : Dict) (key : String) (default : SimRootDataId) : SimRootDataId×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<SimRootDataIdArray>"]
opaque dict_GetSimRootDataIdArray (dict : Dict) (key : String) (default : SimRootDataIdArray) : SimRootDataIdArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<String>"]
opaque dict_GetString (dict : Dict) (key : String) (default : String) : String×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<StringArray>"]
opaque dict_GetStringArray (dict : Dict) (key : String) (default : StringArray) : StringArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<Vector2>"]
opaque dict_GetVector2 (dict : Dict) (key : String) (default : Vector2) : Vector2×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<Vector2Array>"]
opaque dict_GetVector2Array (dict : Dict) (key : String) (default : Vector2Array) : Vector2Array×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<Vector3>"]
opaque dict_GetVector3 (dict : Dict) (key : String) (default : Vector3) : Vector3×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<Vector3Array>"]
opaque dict_GetVector3Array (dict : Dict) (key : String) (default : Vector3Array) : Vector3Array×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<Vector4>"]
opaque dict_GetVector4 (dict : Dict) (key : String) (default : Vector4) : Vector4×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::Get<Vector4Array>"]
opaque dict_GetVector4Array (dict : Dict) (key : String) (default : Vector4Array) : Vector4Array×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<AnimChannel>"]
opaque dict_GetNestedAnimChannel (dict : Dict) (keys : StringArray) (default : AnimChannel) : AnimChannel×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<AnimChannelCollection>"]
opaque dict_GetNestedAnimChannelCollection (dict : Dict) (keys : StringArray) (default : AnimChannelCollection) : AnimChannelCollection×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<AnimStack>"]
opaque dict_GetNestedAnimStack (dict : Dict) (keys : StringArray) (default : AnimStack) : AnimStack×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<ApexGraphHandle>"]
opaque dict_GetNestedApexGraphHandle (dict : Dict) (keys : StringArray) (default : ApexGraphHandle) : ApexGraphHandle×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<ApexNodeID>"]
opaque dict_GetNestedApexNodeID (dict : Dict) (keys : StringArray) (default : ApexNodeID) : ApexNodeID×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<ApexPortID>"]
opaque dict_GetNestedApexPortID (dict : Dict) (keys : StringArray) (default : ApexPortID) : ApexPortID×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<Bool>"]
opaque dict_GetNestedBool (dict : Dict) (keys : StringArray) (default : Bool) : Bool×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<ColorRamp>"]
opaque dict_GetNestedColorRamp (dict : Dict) (keys : StringArray) (default : ColorRamp) : ColorRamp×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<Dict>"]
opaque dict_GetNestedDict (dict : Dict) (keys : StringArray) (default : Dict) : Dict×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<DynamicPath>"]
opaque dict_GetNestedDynamicPath (dict : Dict) (keys : StringArray) (default : DynamicPath) : DynamicPath×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<FBIKSkeleton>"]
opaque dict_GetNestedFBIKSkeleton (dict : Dict) (keys : StringArray) (default : FBIKSkeleton) : FBIKSkeleton×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<FBIKSolver>"]
opaque dict_GetNestedFBIKSolver (dict : Dict) (keys : StringArray) (default : FBIKSolver) : FBIKSolver×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<FBIKTarget>"]
opaque dict_GetNestedFBIKTarget (dict : Dict) (keys : StringArray) (default : FBIKTarget) : FBIKTarget×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<Float>"]
opaque dict_GetNestedFloat (dict : Dict) (keys : StringArray) (default : Float) : Float×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<FloatRamp>"]
opaque dict_GetNestedFloatRamp (dict : Dict) (keys : StringArray) (default : FloatRamp) : FloatRamp×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<Geometry>"]
opaque dict_GetNestedGeometry (dict : Dict) (keys : StringArray) (default : Geometry) : Geometry×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<Int>"]
opaque dict_GetNestedInt (dict : Dict) (keys : StringArray) (default : Int) : Int×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<Matrix3>"]
opaque dict_GetNestedMatrix3 (dict : Dict) (keys : StringArray) (default : Matrix3) : Matrix3×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<Matrix4>"]
opaque dict_GetNestedMatrix4 (dict : Dict) (keys : StringArray) (default : Matrix4) : Matrix4×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<SimRootDataId>"]
opaque dict_GetNestedSimRootDataId (dict : Dict) (keys : StringArray) (default : SimRootDataId) : SimRootDataId×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<String>"]
opaque dict_GetNestedString (dict : Dict) (keys : StringArray) (default : String) : String×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<Vector2>"]
opaque dict_GetNestedVector2 (dict : Dict) (keys : StringArray) (default : Vector2) : Vector2×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<Vector3>"]
opaque dict_GetNestedVector3 (dict : Dict) (keys : StringArray) (default : Vector3) : Vector3×Bool

/-- outputs: (value,success) -/        
@[apex_node "dict::GetNested<Vector4>"]
opaque dict_GetNestedVector4 (dict : Dict) (keys : StringArray) (default : Vector4) : Vector4×Bool

/-- outputs: (dict) -/        
@[apex_node "dict::Insert" has_rundata]
opaque dict_Insert (dict : Dict) {numargs: Nat} (args : VariadicArg Untyped numargs) : Dict

/-- outputs: (keys) -/        
@[apex_node "dict::Keys"]
opaque dict_Keys (dict : Dict) : StringArray

/-- outputs: (dict) -/        
@[apex_node "dict::PatternRenameKeys"]
opaque dict_PatternRenameKeys (dict : Dict) (keys : String) (pattern : String) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Remove"]
opaque dict_Remove (dict : Dict) (pattern : String) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::RenameKeys"]
opaque dict_RenameKeys (dict : Dict) (keymap : Dict) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<AnimChannel>"]
opaque dict_SetAnimChannel (dict : Dict) (key : String) (value : AnimChannel) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<AnimChannelArray>"]
opaque dict_SetAnimChannelArray (dict : Dict) (key : String) (value : AnimChannelArray) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<AnimChannelCollection>"]
opaque dict_SetAnimChannelCollection (dict : Dict) (key : String) (value : AnimChannelCollection) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<AnimChannelCollectionArray>"]
opaque dict_SetAnimChannelCollectionArray (dict : Dict) (key : String) (value : AnimChannelCollectionArray) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<AnimStack>"]
opaque dict_SetAnimStack (dict : Dict) (key : String) (value : AnimStack) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<AnimStackArray>"]
opaque dict_SetAnimStackArray (dict : Dict) (key : String) (value : AnimStackArray) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<ApexGraphHandle>"]
opaque dict_SetApexGraphHandle (dict : Dict) (key : String) (value : ApexGraphHandle) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<ApexGraphHandleArray>"]
opaque dict_SetApexGraphHandleArray (dict : Dict) (key : String) (value : ApexGraphHandleArray) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<ApexNodeID>"]
opaque dict_SetApexNodeID (dict : Dict) (key : String) (value : ApexNodeID) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<ApexNodeIDArray>"]
opaque dict_SetApexNodeIDArray (dict : Dict) (key : String) (value : ApexNodeIDArray) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<ApexPortID>"]
opaque dict_SetApexPortID (dict : Dict) (key : String) (value : ApexPortID) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<ApexPortIDArray>"]
opaque dict_SetApexPortIDArray (dict : Dict) (key : String) (value : ApexPortIDArray) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<Bool>"]
opaque dict_SetBool (dict : Dict) (key : String) (value : Bool) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<BoolArray>"]
opaque dict_SetBoolArray (dict : Dict) (key : String) (value : BoolArray) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<ColorRamp>"]
opaque dict_SetColorRamp (dict : Dict) (key : String) (value : ColorRamp) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<ColorRampArray>"]
opaque dict_SetColorRampArray (dict : Dict) (key : String) (value : ColorRampArray) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<Dict>"]
opaque dict_SetDict (dict : Dict) (key : String) (value : Dict) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<DictArray>"]
opaque dict_SetDictArray (dict : Dict) (key : String) (value : DictArray) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<DynamicPath>"]
opaque dict_SetDynamicPath (dict : Dict) (key : String) (value : DynamicPath) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<DynamicPathArray>"]
opaque dict_SetDynamicPathArray (dict : Dict) (key : String) (value : DynamicPathArray) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<FBIKSkeleton>"]
opaque dict_SetFBIKSkeleton (dict : Dict) (key : String) (value : FBIKSkeleton) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<FBIKSkeletonArray>"]
opaque dict_SetFBIKSkeletonArray (dict : Dict) (key : String) (value : FBIKSkeletonArray) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<FBIKSolver>"]
opaque dict_SetFBIKSolver (dict : Dict) (key : String) (value : FBIKSolver) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<FBIKSolverArray>"]
opaque dict_SetFBIKSolverArray (dict : Dict) (key : String) (value : FBIKSolverArray) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<FBIKTarget>"]
opaque dict_SetFBIKTarget (dict : Dict) (key : String) (value : FBIKTarget) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<FBIKTargetArray>"]
opaque dict_SetFBIKTargetArray (dict : Dict) (key : String) (value : FBIKTargetArray) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<Float>"]
opaque dict_SetFloat (dict : Dict) (key : String) (value : Float) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<FloatArray>"]
opaque dict_SetFloatArray (dict : Dict) (key : String) (value : FloatArray) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<FloatRamp>"]
opaque dict_SetFloatRamp (dict : Dict) (key : String) (value : FloatRamp) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<FloatRampArray>"]
opaque dict_SetFloatRampArray (dict : Dict) (key : String) (value : FloatRampArray) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<Geometry>"]
opaque dict_SetGeometry (dict : Dict) (key : String) (value : Geometry) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<GeometryArray>"]
opaque dict_SetGeometryArray (dict : Dict) (key : String) (value : GeometryArray) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<Int>"]
opaque dict_SetInt (dict : Dict) (key : String) (value : Int) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<IntArray>"]
opaque dict_SetIntArray (dict : Dict) (key : String) (value : IntArray) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<Matrix3>"]
opaque dict_SetMatrix3 (dict : Dict) (key : String) (value : Matrix3) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<Matrix3Array>"]
opaque dict_SetMatrix3Array (dict : Dict) (key : String) (value : Matrix3Array) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<Matrix4>"]
opaque dict_SetMatrix4 (dict : Dict) (key : String) (value : Matrix4) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<Matrix4Array>"]
opaque dict_SetMatrix4Array (dict : Dict) (key : String) (value : Matrix4Array) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<SimRootDataId>"]
opaque dict_SetSimRootDataId (dict : Dict) (key : String) (value : SimRootDataId) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<SimRootDataIdArray>"]
opaque dict_SetSimRootDataIdArray (dict : Dict) (key : String) (value : SimRootDataIdArray) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<String>"]
opaque dict_SetString (dict : Dict) (key : String) (value : String) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<StringArray>"]
opaque dict_SetStringArray (dict : Dict) (key : String) (value : StringArray) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<Vector2>"]
opaque dict_SetVector2 (dict : Dict) (key : String) (value : Vector2) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<Vector2Array>"]
opaque dict_SetVector2Array (dict : Dict) (key : String) (value : Vector2Array) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<Vector3>"]
opaque dict_SetVector3 (dict : Dict) (key : String) (value : Vector3) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<Vector3Array>"]
opaque dict_SetVector3Array (dict : Dict) (key : String) (value : Vector3Array) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<Vector4>"]
opaque dict_SetVector4 (dict : Dict) (key : String) (value : Vector4) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Set<Vector4Array>"]
opaque dict_SetVector4Array (dict : Dict) (key : String) (value : Vector4Array) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<AnimChannel>"]
opaque dict_SetNestedAnimChannel (dict : Dict) (keys : StringArray) (value : AnimChannel) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<AnimChannelCollection>"]
opaque dict_SetNestedAnimChannelCollection (dict : Dict) (keys : StringArray) (value : AnimChannelCollection) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<AnimStack>"]
opaque dict_SetNestedAnimStack (dict : Dict) (keys : StringArray) (value : AnimStack) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<ApexGraphHandle>"]
opaque dict_SetNestedApexGraphHandle (dict : Dict) (keys : StringArray) (value : ApexGraphHandle) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<ApexNodeID>"]
opaque dict_SetNestedApexNodeID (dict : Dict) (keys : StringArray) (value : ApexNodeID) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<ApexPortID>"]
opaque dict_SetNestedApexPortID (dict : Dict) (keys : StringArray) (value : ApexPortID) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<Bool>"]
opaque dict_SetNestedBool (dict : Dict) (keys : StringArray) (value : Bool) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<ColorRamp>"]
opaque dict_SetNestedColorRamp (dict : Dict) (keys : StringArray) (value : ColorRamp) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<Dict>"]
opaque dict_SetNestedDict (dict : Dict) (keys : StringArray) (value : Dict) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<DynamicPath>"]
opaque dict_SetNestedDynamicPath (dict : Dict) (keys : StringArray) (value : DynamicPath) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<FBIKSkeleton>"]
opaque dict_SetNestedFBIKSkeleton (dict : Dict) (keys : StringArray) (value : FBIKSkeleton) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<FBIKSolver>"]
opaque dict_SetNestedFBIKSolver (dict : Dict) (keys : StringArray) (value : FBIKSolver) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<FBIKTarget>"]
opaque dict_SetNestedFBIKTarget (dict : Dict) (keys : StringArray) (value : FBIKTarget) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<Float>"]
opaque dict_SetNestedFloat (dict : Dict) (keys : StringArray) (value : Float) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<FloatRamp>"]
opaque dict_SetNestedFloatRamp (dict : Dict) (keys : StringArray) (value : FloatRamp) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<Geometry>"]
opaque dict_SetNestedGeometry (dict : Dict) (keys : StringArray) (value : Geometry) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<Int>"]
opaque dict_SetNestedInt (dict : Dict) (keys : StringArray) (value : Int) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<Matrix3>"]
opaque dict_SetNestedMatrix3 (dict : Dict) (keys : StringArray) (value : Matrix3) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<Matrix4>"]
opaque dict_SetNestedMatrix4 (dict : Dict) (keys : StringArray) (value : Matrix4) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<SimRootDataId>"]
opaque dict_SetNestedSimRootDataId (dict : Dict) (keys : StringArray) (value : SimRootDataId) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<String>"]
opaque dict_SetNestedString (dict : Dict) (keys : StringArray) (value : String) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<Vector2>"]
opaque dict_SetNestedVector2 (dict : Dict) (keys : StringArray) (value : Vector2) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<Vector3>"]
opaque dict_SetNestedVector3 (dict : Dict) (keys : StringArray) (value : Vector3) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::SetNested<Vector4>"]
opaque dict_SetNestedVector4 (dict : Dict) (keys : StringArray) (value : Vector4) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Transfer"]
opaque dict_Transfer (dict : Dict) (srcdict : Dict) (keymap : Dict) : Dict

/-- outputs: (dict) -/        
@[apex_node "dict::Update"]
opaque dict_Update (dict : Dict) {numothers: Nat} (others : VariadicArg Dict numothers) (addmissing : Bool) (changetype : Bool) : Dict

/-- outputs: (sum) -/        
@[apex_node "dynamicpath::Add"]
opaque dynamicpath_Add (base : DynamicPath) (layer : DynamicPath) (samplerate : Float) : DynamicPath

/-- outputs: (path) -/        
@[apex_node "dynamicpath::AddTweakTarget"]
opaque dynamicpath_AddTweakTarget (path : DynamicPath) (samplerate : Float) (position : Vector3) (time : Float) (primary : Bool) : DynamicPath

/-- outputs: (pathout) -/        
@[apex_node "dynamicpath::Bake" has_rundata]
opaque dynamicpath_Bake (patharray : DynamicPathArray) (samplerate : Float) : DynamicPath

/-- outputs: (path) -/        
@[apex_node "dynamicpath::DeformLaplacian" has_rundata]
opaque dynamicpath_DeformLaplacian (path : DynamicPath) (samplerate : Float) (pins : Vector3Array) (times : FloatArray) (weights : FloatArray) : DynamicPath

/-- outputs: (position,velocity,sourcetime) -/        
@[apex_node "dynamicpath::Evaluate"]
opaque dynamicpath_Evaluate (patharray : DynamicPathArray) (time : Float) : Vector3×Vector3×Float

/-- outputs: (position,velocity,time) -/        
@[apex_node "dynamicpath::EvaluateInSourceTime"]
opaque dynamicpath_EvaluateInSourceTime (patharray : DynamicPathArray) (sourcetime : Float) (preferend : Bool) : Vector3×Vector3×Float

/-- outputs: (positions,velocities,sourcetimes) -/        
@[apex_node "dynamicpath::EvaluateMulti"]
opaque dynamicpath_EvaluateMulti (patharray : DynamicPathArray) (times : FloatArray) : Vector3Array×Vector3Array×FloatArray

/-- outputs: (positions,velocities,times) -/        
@[apex_node "dynamicpath::EvaluateMultiInSourceTime"]
opaque dynamicpath_EvaluateMultiInSourceTime (patharray : DynamicPathArray) (sourcetimes : FloatArray) (preferend : Bool) : Vector3Array×Vector3Array×FloatArray

/-- outputs: (pathindices,sectionranges) -/        
@[apex_node "dynamicpath::GetAllSections" has_rundata]
opaque dynamicpath_GetAllSections (patharray : DynamicPathArray) (samplerate : Float) (skipbase : Bool) (defaultpathindex : Int) : IntArray×Vector2Array

/-- outputs: (keytimes) -/        
@[apex_node "dynamicpath::GetKeyTimes" has_rundata]
opaque dynamicpath_GetKeyTimes (patharray : DynamicPathArray) (onlyessential : Bool) (samplerate : Float) (skipbase : Bool) : FloatArray

/-- outputs: (pathindex,range) -/        
@[apex_node "dynamicpath::GetSection"]
opaque dynamicpath_GetSection (patharray : DynamicPathArray) (time : Float) (defaultpathindex : Int) : Int×Vector2

/-- outputs: (startime,endtime,life) -/        
@[apex_node "dynamicpath::GetTiming" has_rundata]
opaque dynamicpath_GetTiming (patharray : DynamicPathArray) : Float×Float×Float

/-- outputs: (path) -/        
@[apex_node "dynamicpath::LoadFromAnimStack" has_rundata]
opaque dynamicpath_LoadFromAnimStack (animstack : AnimStack) (tparm : String) (rparm : String) (sparm : String) (times : FloatArray) (parentxforms : Matrix4Array) (parentlocals : Matrix4Array) (restlocals : Matrix4Array) (xord : Int) (rord : Int) (scaleinheritance : Int) (samplerate : Float) : DynamicPath

/-- outputs: (path) -/        
@[apex_node "dynamicpath::LoadFromArray" has_rundata]
opaque dynamicpath_LoadFromArray (positions : Vector3Array) (times : FloatArray) (sourcetimes : FloatArray) (samplerate : Float) : DynamicPath

/-- outputs: (path) -/        
@[apex_node "dynamicpath::LoadFromChannels" has_rundata]
opaque dynamicpath_LoadFromChannels (xchannel : AnimChannel) (ychannel : AnimChannel) (zchannel : AnimChannel) (starttime : Float) (life : Float) (samplerate : Float) : DynamicPath

/-- outputs: (path) -/        
@[apex_node "dynamicpath::LoadFromGeometry" has_rundata]
opaque dynamicpath_LoadFromGeometry (geo : Geometry) : DynamicPath

-- special function not supported yet
-- opaque dynamicpath::LoadFromGraph (graph : ApexGraphHandle) {numparms: Nat} (parms : VariadicArg Dict numpar...

-- special function not supported yet
-- opaque dynamicpath::LoadFromGraphChannels (graph : ApexGraphHandle) (channels : AnimChannelCollection) (star...

/-- outputs: (life) -/        
@[apex_node "dynamicpath::ProjectileLifeFromPlane" has_rundata]
opaque dynamicpath_ProjectileLifeFromPlane (gravity : Vector3) (startpos : Vector3) (endpos : Vector3) (planepos : Vector3) (planerot : Vector3) (clampheight : Bool) (samplerate : Float) (roundlife : Bool) : Float

/-- outputs: (peakposition,peakrotation,time) -/        
@[apex_node "dynamicpath::ProjectilePathEvaluatePeak"]
opaque dynamicpath_ProjectilePathEvaluatePeak (path : DynamicPath) (usepositionhint : Bool) (position : Vector3) (rotation : Vector3) : Vector3×Vector3×Float

/-- outputs: (path) -/        
@[apex_node "dynamicpath::ProjectilePathFromLife" has_rundata]
opaque dynamicpath_ProjectilePathFromLife (gravity : Vector3) (startpos : Vector3) (endpos : Vector3) (starttime : Float) (life : Float) (transitionin : Vector2) (transitionout : Vector2) (shiftbefore : Bool) (shiftafter : Bool) (replaceperiod : Float) (samplerate : Float) (roundlife : Bool) : DynamicPath

/-- outputs: (path) -/        
@[apex_node "dynamicpath::ProjectilePathFromPlane" has_rundata]
opaque dynamicpath_ProjectilePathFromPlane (gravity : Vector3) (startpos : Vector3) (endpos : Vector3) (planepos : Vector3) (planerot : Vector3) (starttime : Float) (transitionin : Vector2) (transitionout : Vector2) (shiftbefore : Bool) (shiftafter : Bool) (replaceperiod : Float) (samplerate : Float) (roundlife : Bool) : DynamicPath

/-- outputs: (path) -/        
@[apex_node "dynamicpath::ProjectilePathFromSpeed" has_rundata]
opaque dynamicpath_ProjectilePathFromSpeed (gravity : Vector3) (startpos : Vector3) (endpos : Vector3) (starttime : Float) (speed : Float) (prefershortpath : Bool) (transitionin : Vector2) (transitionout : Vector2) (shiftbefore : Bool) (shiftafter : Bool) (replaceperiod : Float) (samplerate : Float) (roundlife : Bool) : DynamicPath

/-- outputs: (path) -/        
@[apex_node "dynamicpath::ResolveTweakTargets"]
opaque dynamicpath_ResolveTweakTargets (path : DynamicPath) : DynamicPath

/-- outputs: (geo) -/        
@[apex_node "dynamicpath::SaveToGeometry" has_rundata]
opaque dynamicpath_SaveToGeometry (patharray : DynamicPathArray) (samplerate : Float) (color : Vector3) (dolimit : Int) (limitrange : Vector2) (dohighlight : Int) (highlightrange : Vector2) (highlightcolor : Vector3) (ticksize : Float) (ribbonmode : Int) (useperspective : Bool) (camerapos : Vector3) (cameranormal : Vector3) (keytimes : FloatArray) : Geometry

/-- outputs: (difference) -/        
@[apex_node "dynamicpath::Subtract"]
opaque dynamicpath_Subtract (base : DynamicPath) (layer : DynamicPath) (samplerate : Float) : DynamicPath

/-- outputs: (xform) -/        
@[apex_node "fbik::GetBoneTransform"]
opaque fbik_GetBoneTransform (skeleton : FBIKSkeleton) (bone : String) : Matrix4

/-- outputs: (position,totalmass) -/        
@[apex_node "fbik::GetCenterOfMass"]
opaque fbik_GetCenterOfMass (skel : FBIKSkeleton) : Vector3×Float

/-- outputs: (solver) -/        
@[apex_node "fbik::SetComTarget"]
opaque fbik_SetComTarget (solver : FBIKSolver) (target : FBIKTarget) : FBIKSolver

/-- outputs: (solver) -/        
@[apex_node "fbik::SetSkeleton"]
opaque fbik_SetSkeleton (solver : FBIKSolver) (skel : FBIKSkeleton) : FBIKSolver

/-- outputs: (solver) -/        
@[apex_node "fbik::SetTarget"]
opaque fbik_SetTarget (solver : FBIKSolver) (bone : String) (target : FBIKTarget) : FBIKSolver

/-- outputs: (solver) -/        
@[apex_node "fbik::SetTargets"]
opaque fbik_SetTargets (solver : FBIKSolver) {numtargets: Nat} (targets : VariadicArg FBIKTarget numtargets) : FBIKSolver

/-- outputs: (solver) -/        
@[apex_node "fbik::SetTargetsFromDict"]
opaque fbik_SetTargetsFromDict (solver : FBIKSolver) (targets : Dict) : FBIKSolver

/-- outputs: (solver) -/        
@[apex_node "fbik::SetTargetsFromGeo"]
opaque fbik_SetTargetsFromGeo (solver : FBIKSolver) (geo : Geometry) : FBIKSolver

/-- outputs: (skel) -/        
@[apex_node "fbik::SkeletonFromGeo"]
opaque fbik_SkeletonFromGeo (geo : Geometry) (mapbyattrib : Bool) (mappingattrib : String) (jointconfig : String) (targetconfig : String) : FBIKSkeleton

/-- outputs: (geo) -/        
@[apex_node "fbik::SkeletonUpdateGeo" has_rundata]
opaque fbik_SkeletonUpdateGeo (geo : Geometry) (skel : FBIKSkeleton) (matchname : Bool) : Geometry

/-- outputs: (skel,solver,success) -/        
@[apex_node "fbik::SolveFABRIK"]
opaque fbik_SolveFABRIK (solver : FBIKSolver) : FBIKSkeleton×FBIKSolver×Bool

/-- outputs: (skel,solver,success) -/        
@[apex_node "fbik::SolvePhysIK"]
opaque fbik_SolvePhysIK (solver : FBIKSolver) : FBIKSkeleton×FBIKSolver×Bool

/-- outputs: (solver) -/        
@[apex_node "fbik::Solver"]
opaque fbik_Solver (skel : FBIKSkeleton) (iterations : Int) (tolerance : Float) (damping : Float) (pinroot : Bool) : FBIKSolver

/-- outputs: (target) -/        
@[apex_node "fbik::Target"]
opaque fbik_Target (type : Int) (xform : Matrix4) (offset : Matrix4) (weight : Float) (priority : Int) : FBIKTarget

/-- outputs: (geo,ptnum,primnum) -/        
@[apex_node "geo::AddPacked"]
opaque geo_AddPacked (geo : Geometry) (targetpt : Int) (src : Geometry) : Geometry×Int×Int

-- special function not supported yet
-- opaque geo::AttribIntersectPoints (geo : Geometry) (intersect_cache : geo::IntersectCache) (origin : Vector3...

/-- outputs: (center,size,min,max,xform) -/        
@[apex_node "geo::BoundingBox"]
opaque geo_BoundingBox (geo : Geometry) (orient : Bool) (local' : Bool) : Vector3×Vector3×Vector3×Vector3×Matrix4

/-- outputs: (geo,success) -/        
@[apex_node "geo::CopyDetailAttrib"]
opaque geo_CopyDetailAttrib (geo : Geometry) (src : Geometry) (attribname : String) : Geometry×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::DetailAttribValue<Dict>" has_rundata]
opaque geo_DetailAttribValueDict (geo : Geometry) (attribname : String) : Dict×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::DetailAttribValue<DictArray>" has_rundata]
opaque geo_DetailAttribValueDictArray (geo : Geometry) (attribname : String) : DictArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::DetailAttribValue<Float>" has_rundata]
opaque geo_DetailAttribValueFloat (geo : Geometry) (attribname : String) : Float×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::DetailAttribValue<FloatArray>" has_rundata]
opaque geo_DetailAttribValueFloatArray (geo : Geometry) (attribname : String) : FloatArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::DetailAttribValue<Int>" has_rundata]
opaque geo_DetailAttribValueInt (geo : Geometry) (attribname : String) : Int×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::DetailAttribValue<IntArray>" has_rundata]
opaque geo_DetailAttribValueIntArray (geo : Geometry) (attribname : String) : IntArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::DetailAttribValue<Matrix3>" has_rundata]
opaque geo_DetailAttribValueMatrix3 (geo : Geometry) (attribname : String) : Matrix3×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::DetailAttribValue<Matrix4>" has_rundata]
opaque geo_DetailAttribValueMatrix4 (geo : Geometry) (attribname : String) : Matrix4×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::DetailAttribValue<String>" has_rundata]
opaque geo_DetailAttribValueString (geo : Geometry) (attribname : String) : String×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::DetailAttribValue<StringArray>" has_rundata]
opaque geo_DetailAttribValueStringArray (geo : Geometry) (attribname : String) : StringArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::DetailAttribValue<Vector2>" has_rundata]
opaque geo_DetailAttribValueVector2 (geo : Geometry) (attribname : String) : Vector2×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::DetailAttribValue<Vector3>" has_rundata]
opaque geo_DetailAttribValueVector3 (geo : Geometry) (attribname : String) : Vector3×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::DetailAttribValue<Vector4>" has_rundata]
opaque geo_DetailAttribValueVector4 (geo : Geometry) (attribname : String) : Vector4×Bool

/-- outputs: (geo) -/        
@[apex_node "geo::DisplacePoints"]
opaque geo_DisplacePoints (geo : Geometry) (refgeo : Geometry) (pts : IntArray) (weights : FloatArray) (strength : Float) (normal : Vector3) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::DragPoints"]
opaque geo_DragPoints (geo : Geometry) (pts : IntArray) (weights : FloatArray) (delta : Vector3) : Geometry

/-- outputs: (geo,embedded,refprim) -/        
@[apex_node "geo::ExtractPackedGeo" has_rundata]
opaque geo_ExtractPackedGeo (geo : Geometry) (primnum : Int) : Geometry×Geometry×Int

/-- outputs: (result,size) -/        
@[apex_node "geo::FindPointAttribValue<Int>" has_rundata]
opaque geo_FindPointAttribValueInt (geo : Geometry) (attribname : String) (value : Int) : IntArray×Int

/-- outputs: (result,size) -/        
@[apex_node "geo::FindPointAttribValue<String>" has_rundata]
opaque geo_FindPointAttribValueString (geo : Geometry) (attribname : String) (value : String) : IntArray×Int

/-- outputs: (result,size) -/        
@[apex_node "geo::FindPrimAttribValue<Int>" has_rundata]
opaque geo_FindPrimAttribValueInt (geo : Geometry) (attribname : String) (value : Int) : IntArray×Int

/-- outputs: (result,size) -/        
@[apex_node "geo::FindPrimAttribValue<String>" has_rundata]
opaque geo_FindPrimAttribValueString (geo : Geometry) (attribname : String) (value : String) : IntArray×Int

/-- outputs: (result,size) -/        
@[apex_node "geo::FindVertexAttribValue<Int>" has_rundata]
opaque geo_FindVertexAttribValueInt (geo : Geometry) (attribname : String) (value : Int) : IntArray×Int

/-- outputs: (result,size) -/        
@[apex_node "geo::FindVertexAttribValue<String>" has_rundata]
opaque geo_FindVertexAttribValueString (geo : Geometry) (attribname : String) (value : String) : IntArray×Int

-- special function not supported yet
-- opaque geo::ForEachPointBegin (geo : Geometry) (group : String) (__spare__ : undefined) : undefined×Geometry...

-- special function not supported yet
-- opaque geo::ForEachPointEnd (scope : undefined) (geo : Geometry) (element : Int) (__spare__ : undefined) : G...

-- special function not supported yet
-- opaque geo::ForEachPrimBegin (geo : Geometry) (group : String) (__spare__ : undefined) : undefined×Geometry×...

-- special function not supported yet
-- opaque geo::ForEachPrimEnd (scope : undefined) (geo : Geometry) (element : Int) (__spare__ : undefined) : Ge...

/-- outputs: (geo) -/        
@[apex_node "geo::FromDisk" has_rundata]
opaque geo_FromDisk (filepath : String) (primname : String) : Geometry

/-- outputs: (pts) -/        
@[apex_node "geo::GlobPoints"]
opaque geo_GlobPoints (geo : Geometry) (pattern : String) (ordered : Bool) : IntArray

/-- outputs: (prims) -/        
@[apex_node "geo::GlobPrims"]
opaque geo_GlobPrims (geo : Geometry) (pattern : String) (ordered : Bool) : IntArray

/-- outputs: (groom,surfaces) -/        
@[apex_node "geo::GuideDeform" has_rundata]
opaque geo_GuideDeform (groom : Geometry) (surfaces : Geometry) (groomskel : Geometry) (animskin : Geometry) (restskin : Geometry) (tangent_name : String) (normal_name : String) : Geometry×Geometry

-- special function not supported yet
-- opaque geo::InitIntersectCache (geo : Geometry) : Geometry×geo::IntersectCache...

-- special function not supported yet
-- opaque geo::Intersect (intersect_cache : geo::IntersectCache) (origin : Vector3) (dir : Vector3) : Vector3×V...

-- special function not supported yet
-- opaque geo::IntersectPoints (intersect_cache : geo::IntersectCache) (geo : Geometry) (origin : Vector3) (dir...

/-- outputs: (geo) -/        
@[apex_node "geo::Lattice"]
opaque geo_Lattice (geo : Geometry) (restgeo : Geometry) (deformedgeo : Geometry) (divsx : Int) (divsy : Int) (divsz : Int) (group : String) (interptype : Int) (updatenmls : Bool) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::Lattice::2.0" has_rundata]
opaque geo_Lattice_2_0 (geo : Geometry) (restlattice : Geometry) (deformedlattice : Geometry) (divsx : Int) (divsy : Int) (divsz : Int) (ordx : Int) (ordy : Int) (ordz : Int) (falloff : Float) (group : String) (interptype : Int) (updatenmls : Bool) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::Merge"]
opaque geo_Merge (geo : Geometry) {numsrc: Nat} (src : VariadicArg Geometry numsrc) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::MergePacked" has_rundata]
opaque geo_MergePacked {numsrc: Nat} (src : VariadicArg Geometry numsrc) : Geometry

/-- outputs: (numpts) -/        
@[apex_node "geo::NumPoints"]
opaque geo_NumPoints (geo : Geometry) : Int

/-- outputs: (numprims) -/        
@[apex_node "geo::NumPrims"]
opaque geo_NumPrims (geo : Geometry) : Int

/-- outputs: (value,success) -/        
@[apex_node "geo::PointAttribValue<Dict>" has_rundata]
opaque geo_PointAttribValueDict (geo : Geometry) (elemnum : Int) (attribname : String) : Dict×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PointAttribValue<DictArray>" has_rundata]
opaque geo_PointAttribValueDictArray (geo : Geometry) (elemnum : Int) (attribname : String) : DictArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PointAttribValue<Float>" has_rundata]
opaque geo_PointAttribValueFloat (geo : Geometry) (elemnum : Int) (attribname : String) : Float×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PointAttribValue<FloatArray>" has_rundata]
opaque geo_PointAttribValueFloatArray (geo : Geometry) (elemnum : Int) (attribname : String) : FloatArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PointAttribValue<Int>" has_rundata]
opaque geo_PointAttribValueInt (geo : Geometry) (elemnum : Int) (attribname : String) : Int×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PointAttribValue<IntArray>" has_rundata]
opaque geo_PointAttribValueIntArray (geo : Geometry) (elemnum : Int) (attribname : String) : IntArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PointAttribValue<Matrix3>" has_rundata]
opaque geo_PointAttribValueMatrix3 (geo : Geometry) (elemnum : Int) (attribname : String) : Matrix3×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PointAttribValue<Matrix4>" has_rundata]
opaque geo_PointAttribValueMatrix4 (geo : Geometry) (elemnum : Int) (attribname : String) : Matrix4×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PointAttribValue<String>" has_rundata]
opaque geo_PointAttribValueString (geo : Geometry) (elemnum : Int) (attribname : String) : String×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PointAttribValue<StringArray>" has_rundata]
opaque geo_PointAttribValueStringArray (geo : Geometry) (elemnum : Int) (attribname : String) : StringArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PointAttribValue<Vector2>" has_rundata]
opaque geo_PointAttribValueVector2 (geo : Geometry) (elemnum : Int) (attribname : String) : Vector2×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PointAttribValue<Vector3>" has_rundata]
opaque geo_PointAttribValueVector3 (geo : Geometry) (elemnum : Int) (attribname : String) : Vector3×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PointAttribValue<Vector4>" has_rundata]
opaque geo_PointAttribValueVector4 (geo : Geometry) (elemnum : Int) (attribname : String) : Vector4×Bool

-- special function not supported yet
-- opaque geo::PointAttribValuesByName<Dict> (geo : Geometry) (nameattrib : String) (attribname : String) : Var...

-- special function not supported yet
-- opaque geo::PointAttribValuesByName<DictArray> (geo : Geometry) (nameattrib : String) (attribname : String) ...

-- special function not supported yet
-- opaque geo::PointAttribValuesByName<Float> (geo : Geometry) (nameattrib : String) (attribname : String) : Va...

-- special function not supported yet
-- opaque geo::PointAttribValuesByName<FloatArray> (geo : Geometry) (nameattrib : String) (attribname : String)...

-- special function not supported yet
-- opaque geo::PointAttribValuesByName<Int> (geo : Geometry) (nameattrib : String) (attribname : String) : Vari...

-- special function not supported yet
-- opaque geo::PointAttribValuesByName<IntArray> (geo : Geometry) (nameattrib : String) (attribname : String) :...

-- special function not supported yet
-- opaque geo::PointAttribValuesByName<Matrix3> (geo : Geometry) (nameattrib : String) (attribname : String) : ...

-- special function not supported yet
-- opaque geo::PointAttribValuesByName<Matrix4> (geo : Geometry) (nameattrib : String) (attribname : String) : ...

-- special function not supported yet
-- opaque geo::PointAttribValuesByName<String> (geo : Geometry) (nameattrib : String) (attribname : String) : V...

-- special function not supported yet
-- opaque geo::PointAttribValuesByName<StringArray> (geo : Geometry) (nameattrib : String) (attribname : String...

-- special function not supported yet
-- opaque geo::PointAttribValuesByName<Vector2> (geo : Geometry) (nameattrib : String) (attribname : String) : ...

-- special function not supported yet
-- opaque geo::PointAttribValuesByName<Vector3> (geo : Geometry) (nameattrib : String) (attribname : String) : ...

-- special function not supported yet
-- opaque geo::PointAttribValuesByName<Vector4> (geo : Geometry) (nameattrib : String) (attribname : String) : ...

/-- outputs: (prims) -/        
@[apex_node "geo::PointPrims"]
opaque geo_PointPrims (geo : Geometry) (pt : Int) : IntArray

/-- outputs: (value,success) -/        
@[apex_node "geo::PrimAttribValue<Dict>" has_rundata]
opaque geo_PrimAttribValueDict (geo : Geometry) (elemnum : Int) (attribname : String) : Dict×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PrimAttribValue<DictArray>" has_rundata]
opaque geo_PrimAttribValueDictArray (geo : Geometry) (elemnum : Int) (attribname : String) : DictArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PrimAttribValue<Float>" has_rundata]
opaque geo_PrimAttribValueFloat (geo : Geometry) (elemnum : Int) (attribname : String) : Float×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PrimAttribValue<FloatArray>" has_rundata]
opaque geo_PrimAttribValueFloatArray (geo : Geometry) (elemnum : Int) (attribname : String) : FloatArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PrimAttribValue<Int>" has_rundata]
opaque geo_PrimAttribValueInt (geo : Geometry) (elemnum : Int) (attribname : String) : Int×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PrimAttribValue<IntArray>" has_rundata]
opaque geo_PrimAttribValueIntArray (geo : Geometry) (elemnum : Int) (attribname : String) : IntArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PrimAttribValue<Matrix3>" has_rundata]
opaque geo_PrimAttribValueMatrix3 (geo : Geometry) (elemnum : Int) (attribname : String) : Matrix3×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PrimAttribValue<Matrix4>" has_rundata]
opaque geo_PrimAttribValueMatrix4 (geo : Geometry) (elemnum : Int) (attribname : String) : Matrix4×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PrimAttribValue<String>" has_rundata]
opaque geo_PrimAttribValueString (geo : Geometry) (elemnum : Int) (attribname : String) : String×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PrimAttribValue<StringArray>" has_rundata]
opaque geo_PrimAttribValueStringArray (geo : Geometry) (elemnum : Int) (attribname : String) : StringArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PrimAttribValue<Vector2>" has_rundata]
opaque geo_PrimAttribValueVector2 (geo : Geometry) (elemnum : Int) (attribname : String) : Vector2×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PrimAttribValue<Vector3>" has_rundata]
opaque geo_PrimAttribValueVector3 (geo : Geometry) (elemnum : Int) (attribname : String) : Vector3×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::PrimAttribValue<Vector4>" has_rundata]
opaque geo_PrimAttribValueVector4 (geo : Geometry) (elemnum : Int) (attribname : String) : Vector4×Bool

-- special function not supported yet
-- opaque geo::PrimAttribValuesByName<Dict> (geo : Geometry) (nameattrib : String) (attribname : String) : Vari...

-- special function not supported yet
-- opaque geo::PrimAttribValuesByName<DictArray> (geo : Geometry) (nameattrib : String) (attribname : String) :...

-- special function not supported yet
-- opaque geo::PrimAttribValuesByName<Float> (geo : Geometry) (nameattrib : String) (attribname : String) : Var...

-- special function not supported yet
-- opaque geo::PrimAttribValuesByName<FloatArray> (geo : Geometry) (nameattrib : String) (attribname : String) ...

-- special function not supported yet
-- opaque geo::PrimAttribValuesByName<Int> (geo : Geometry) (nameattrib : String) (attribname : String) : Varia...

-- special function not supported yet
-- opaque geo::PrimAttribValuesByName<IntArray> (geo : Geometry) (nameattrib : String) (attribname : String) : ...

-- special function not supported yet
-- opaque geo::PrimAttribValuesByName<Matrix3> (geo : Geometry) (nameattrib : String) (attribname : String) : V...

-- special function not supported yet
-- opaque geo::PrimAttribValuesByName<Matrix4> (geo : Geometry) (nameattrib : String) (attribname : String) : V...

-- special function not supported yet
-- opaque geo::PrimAttribValuesByName<String> (geo : Geometry) (nameattrib : String) (attribname : String) : Va...

-- special function not supported yet
-- opaque geo::PrimAttribValuesByName<StringArray> (geo : Geometry) (nameattrib : String) (attribname : String)...

-- special function not supported yet
-- opaque geo::PrimAttribValuesByName<Vector2> (geo : Geometry) (nameattrib : String) (attribname : String) : V...

-- special function not supported yet
-- opaque geo::PrimAttribValuesByName<Vector3> (geo : Geometry) (nameattrib : String) (attribname : String) : V...

-- special function not supported yet
-- opaque geo::PrimAttribValuesByName<Vector4> (geo : Geometry) (nameattrib : String) (attribname : String) : V...

/-- outputs: (pts) -/        
@[apex_node "geo::PrimPoints"]
opaque geo_PrimPoints (geo : Geometry) (prim : Int) : IntArray

/-- outputs: (geo) -/        
@[apex_node "geo::Replace"]
opaque geo_Replace (geo : Geometry) (src : Geometry) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetAgentTransforms" has_rundata]
opaque geo_SetAgentTransforms (geo : Geometry) (primnum : Int) {numtransforms: Nat} (transforms : VariadicArg Matrix4 numtransforms) : Geometry

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetDetailAttribValue<Dict>" has_rundata]
opaque geo_SetDetailAttribValueDict (geo : Geometry) (attribname : String) (value : Dict) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetDetailAttribValue<DictArray>" has_rundata]
opaque geo_SetDetailAttribValueDictArray (geo : Geometry) (attribname : String) (value : DictArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetDetailAttribValue<Float>" has_rundata]
opaque geo_SetDetailAttribValueFloat (geo : Geometry) (attribname : String) (value : Float) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetDetailAttribValue<FloatArray>" has_rundata]
opaque geo_SetDetailAttribValueFloatArray (geo : Geometry) (attribname : String) (value : FloatArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetDetailAttribValue<Int>" has_rundata]
opaque geo_SetDetailAttribValueInt (geo : Geometry) (attribname : String) (value : Int) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetDetailAttribValue<IntArray>" has_rundata]
opaque geo_SetDetailAttribValueIntArray (geo : Geometry) (attribname : String) (value : IntArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetDetailAttribValue<Matrix3>" has_rundata]
opaque geo_SetDetailAttribValueMatrix3 (geo : Geometry) (attribname : String) (value : Matrix3) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetDetailAttribValue<Matrix4>" has_rundata]
opaque geo_SetDetailAttribValueMatrix4 (geo : Geometry) (attribname : String) (value : Matrix4) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetDetailAttribValue<String>" has_rundata]
opaque geo_SetDetailAttribValueString (geo : Geometry) (attribname : String) (value : String) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetDetailAttribValue<StringArray>" has_rundata]
opaque geo_SetDetailAttribValueStringArray (geo : Geometry) (attribname : String) (value : StringArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetDetailAttribValue<Vector2>" has_rundata]
opaque geo_SetDetailAttribValueVector2 (geo : Geometry) (attribname : String) (value : Vector2) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetDetailAttribValue<Vector3>" has_rundata]
opaque geo_SetDetailAttribValueVector3 (geo : Geometry) (attribname : String) (value : Vector3) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetDetailAttribValue<Vector4>" has_rundata]
opaque geo_SetDetailAttribValueVector4 (geo : Geometry) (attribname : String) (value : Vector4) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValue<Dict>" has_rundata]
opaque geo_SetPointAttribValueDict (geo : Geometry) (elemnum : Int) (attribname : String) (value : Dict) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValue<DictArray>" has_rundata]
opaque geo_SetPointAttribValueDictArray (geo : Geometry) (elemnum : Int) (attribname : String) (value : DictArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValue<Float>" has_rundata]
opaque geo_SetPointAttribValueFloat (geo : Geometry) (elemnum : Int) (attribname : String) (value : Float) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValue<FloatArray>" has_rundata]
opaque geo_SetPointAttribValueFloatArray (geo : Geometry) (elemnum : Int) (attribname : String) (value : FloatArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValue<Int>" has_rundata]
opaque geo_SetPointAttribValueInt (geo : Geometry) (elemnum : Int) (attribname : String) (value : Int) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValue<IntArray>" has_rundata]
opaque geo_SetPointAttribValueIntArray (geo : Geometry) (elemnum : Int) (attribname : String) (value : IntArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValue<Matrix3>" has_rundata]
opaque geo_SetPointAttribValueMatrix3 (geo : Geometry) (elemnum : Int) (attribname : String) (value : Matrix3) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValue<Matrix4>" has_rundata]
opaque geo_SetPointAttribValueMatrix4 (geo : Geometry) (elemnum : Int) (attribname : String) (value : Matrix4) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValue<String>" has_rundata]
opaque geo_SetPointAttribValueString (geo : Geometry) (elemnum : Int) (attribname : String) (value : String) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValue<StringArray>" has_rundata]
opaque geo_SetPointAttribValueStringArray (geo : Geometry) (elemnum : Int) (attribname : String) (value : StringArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValue<Vector2>" has_rundata]
opaque geo_SetPointAttribValueVector2 (geo : Geometry) (elemnum : Int) (attribname : String) (value : Vector2) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValue<Vector3>" has_rundata]
opaque geo_SetPointAttribValueVector3 (geo : Geometry) (elemnum : Int) (attribname : String) (value : Vector3) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValue<Vector4>" has_rundata]
opaque geo_SetPointAttribValueVector4 (geo : Geometry) (elemnum : Int) (attribname : String) (value : Vector4) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValues<Dict>" has_rundata]
opaque geo_SetPointAttribValuesDict (geo : Geometry) (group : String) (attribname : String) (value : Dict) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValues<DictArray>" has_rundata]
opaque geo_SetPointAttribValuesDictArray (geo : Geometry) (group : String) (attribname : String) (value : DictArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValues<Float>" has_rundata]
opaque geo_SetPointAttribValuesFloat (geo : Geometry) (group : String) (attribname : String) (value : Float) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValues<FloatArray>" has_rundata]
opaque geo_SetPointAttribValuesFloatArray (geo : Geometry) (group : String) (attribname : String) (value : FloatArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValues<Int>" has_rundata]
opaque geo_SetPointAttribValuesInt (geo : Geometry) (group : String) (attribname : String) (value : Int) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValues<IntArray>" has_rundata]
opaque geo_SetPointAttribValuesIntArray (geo : Geometry) (group : String) (attribname : String) (value : IntArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValues<Matrix3>" has_rundata]
opaque geo_SetPointAttribValuesMatrix3 (geo : Geometry) (group : String) (attribname : String) (value : Matrix3) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValues<Matrix4>" has_rundata]
opaque geo_SetPointAttribValuesMatrix4 (geo : Geometry) (group : String) (attribname : String) (value : Matrix4) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValues<String>" has_rundata]
opaque geo_SetPointAttribValuesString (geo : Geometry) (group : String) (attribname : String) (value : String) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValues<StringArray>" has_rundata]
opaque geo_SetPointAttribValuesStringArray (geo : Geometry) (group : String) (attribname : String) (value : StringArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValues<Vector2>" has_rundata]
opaque geo_SetPointAttribValuesVector2 (geo : Geometry) (group : String) (attribname : String) (value : Vector2) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValues<Vector3>" has_rundata]
opaque geo_SetPointAttribValuesVector3 (geo : Geometry) (group : String) (attribname : String) (value : Vector3) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPointAttribValues<Vector4>" has_rundata]
opaque geo_SetPointAttribValuesVector4 (geo : Geometry) (group : String) (attribname : String) (value : Vector4) : Geometry×Bool

/-- outputs: (geo) -/        
@[apex_node "geo::SetPointAttribValuesByName<Dict>" has_rundata]
opaque geo_SetPointAttribValuesByNameDict (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Dict numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPointAttribValuesByName<DictArray>" has_rundata]
opaque geo_SetPointAttribValuesByNameDictArray (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg DictArray numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPointAttribValuesByName<Float>" has_rundata]
opaque geo_SetPointAttribValuesByNameFloat (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Float numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPointAttribValuesByName<FloatArray>" has_rundata]
opaque geo_SetPointAttribValuesByNameFloatArray (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg FloatArray numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPointAttribValuesByName<Int>" has_rundata]
opaque geo_SetPointAttribValuesByNameInt (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Int numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPointAttribValuesByName<IntArray>" has_rundata]
opaque geo_SetPointAttribValuesByNameIntArray (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg IntArray numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPointAttribValuesByName<Matrix3>" has_rundata]
opaque geo_SetPointAttribValuesByNameMatrix3 (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Matrix3 numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPointAttribValuesByName<Matrix4>" has_rundata]
opaque geo_SetPointAttribValuesByNameMatrix4 (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Matrix4 numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPointAttribValuesByName<String>" has_rundata]
opaque geo_SetPointAttribValuesByNameString (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg String numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPointAttribValuesByName<StringArray>" has_rundata]
opaque geo_SetPointAttribValuesByNameStringArray (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg StringArray numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPointAttribValuesByName<Vector2>" has_rundata]
opaque geo_SetPointAttribValuesByNameVector2 (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Vector2 numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPointAttribValuesByName<Vector3>" has_rundata]
opaque geo_SetPointAttribValuesByNameVector3 (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Vector3 numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPointAttribValuesByName<Vector4>" has_rundata]
opaque geo_SetPointAttribValuesByNameVector4 (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Vector4 numvalue) : Geometry

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValue<Dict>" has_rundata]
opaque geo_SetPrimAttribValueDict (geo : Geometry) (elemnum : Int) (attribname : String) (value : Dict) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValue<DictArray>" has_rundata]
opaque geo_SetPrimAttribValueDictArray (geo : Geometry) (elemnum : Int) (attribname : String) (value : DictArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValue<Float>" has_rundata]
opaque geo_SetPrimAttribValueFloat (geo : Geometry) (elemnum : Int) (attribname : String) (value : Float) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValue<FloatArray>" has_rundata]
opaque geo_SetPrimAttribValueFloatArray (geo : Geometry) (elemnum : Int) (attribname : String) (value : FloatArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValue<Int>" has_rundata]
opaque geo_SetPrimAttribValueInt (geo : Geometry) (elemnum : Int) (attribname : String) (value : Int) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValue<IntArray>" has_rundata]
opaque geo_SetPrimAttribValueIntArray (geo : Geometry) (elemnum : Int) (attribname : String) (value : IntArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValue<Matrix3>" has_rundata]
opaque geo_SetPrimAttribValueMatrix3 (geo : Geometry) (elemnum : Int) (attribname : String) (value : Matrix3) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValue<Matrix4>" has_rundata]
opaque geo_SetPrimAttribValueMatrix4 (geo : Geometry) (elemnum : Int) (attribname : String) (value : Matrix4) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValue<String>" has_rundata]
opaque geo_SetPrimAttribValueString (geo : Geometry) (elemnum : Int) (attribname : String) (value : String) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValue<StringArray>" has_rundata]
opaque geo_SetPrimAttribValueStringArray (geo : Geometry) (elemnum : Int) (attribname : String) (value : StringArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValue<Vector2>" has_rundata]
opaque geo_SetPrimAttribValueVector2 (geo : Geometry) (elemnum : Int) (attribname : String) (value : Vector2) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValue<Vector3>" has_rundata]
opaque geo_SetPrimAttribValueVector3 (geo : Geometry) (elemnum : Int) (attribname : String) (value : Vector3) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValue<Vector4>" has_rundata]
opaque geo_SetPrimAttribValueVector4 (geo : Geometry) (elemnum : Int) (attribname : String) (value : Vector4) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValues<Dict>" has_rundata]
opaque geo_SetPrimAttribValuesDict (geo : Geometry) (group : String) (attribname : String) (value : Dict) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValues<DictArray>" has_rundata]
opaque geo_SetPrimAttribValuesDictArray (geo : Geometry) (group : String) (attribname : String) (value : DictArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValues<Float>" has_rundata]
opaque geo_SetPrimAttribValuesFloat (geo : Geometry) (group : String) (attribname : String) (value : Float) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValues<FloatArray>" has_rundata]
opaque geo_SetPrimAttribValuesFloatArray (geo : Geometry) (group : String) (attribname : String) (value : FloatArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValues<Int>" has_rundata]
opaque geo_SetPrimAttribValuesInt (geo : Geometry) (group : String) (attribname : String) (value : Int) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValues<IntArray>" has_rundata]
opaque geo_SetPrimAttribValuesIntArray (geo : Geometry) (group : String) (attribname : String) (value : IntArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValues<Matrix3>" has_rundata]
opaque geo_SetPrimAttribValuesMatrix3 (geo : Geometry) (group : String) (attribname : String) (value : Matrix3) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValues<Matrix4>" has_rundata]
opaque geo_SetPrimAttribValuesMatrix4 (geo : Geometry) (group : String) (attribname : String) (value : Matrix4) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValues<String>" has_rundata]
opaque geo_SetPrimAttribValuesString (geo : Geometry) (group : String) (attribname : String) (value : String) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValues<StringArray>" has_rundata]
opaque geo_SetPrimAttribValuesStringArray (geo : Geometry) (group : String) (attribname : String) (value : StringArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValues<Vector2>" has_rundata]
opaque geo_SetPrimAttribValuesVector2 (geo : Geometry) (group : String) (attribname : String) (value : Vector2) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValues<Vector3>" has_rundata]
opaque geo_SetPrimAttribValuesVector3 (geo : Geometry) (group : String) (attribname : String) (value : Vector3) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetPrimAttribValues<Vector4>" has_rundata]
opaque geo_SetPrimAttribValuesVector4 (geo : Geometry) (group : String) (attribname : String) (value : Vector4) : Geometry×Bool

/-- outputs: (geo) -/        
@[apex_node "geo::SetPrimAttribValuesByName<Dict>" has_rundata]
opaque geo_SetPrimAttribValuesByNameDict (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Dict numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPrimAttribValuesByName<DictArray>" has_rundata]
opaque geo_SetPrimAttribValuesByNameDictArray (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg DictArray numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPrimAttribValuesByName<Float>" has_rundata]
opaque geo_SetPrimAttribValuesByNameFloat (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Float numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPrimAttribValuesByName<FloatArray>" has_rundata]
opaque geo_SetPrimAttribValuesByNameFloatArray (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg FloatArray numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPrimAttribValuesByName<Int>" has_rundata]
opaque geo_SetPrimAttribValuesByNameInt (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Int numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPrimAttribValuesByName<IntArray>" has_rundata]
opaque geo_SetPrimAttribValuesByNameIntArray (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg IntArray numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPrimAttribValuesByName<Matrix3>" has_rundata]
opaque geo_SetPrimAttribValuesByNameMatrix3 (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Matrix3 numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPrimAttribValuesByName<Matrix4>" has_rundata]
opaque geo_SetPrimAttribValuesByNameMatrix4 (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Matrix4 numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPrimAttribValuesByName<String>" has_rundata]
opaque geo_SetPrimAttribValuesByNameString (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg String numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPrimAttribValuesByName<StringArray>" has_rundata]
opaque geo_SetPrimAttribValuesByNameStringArray (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg StringArray numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPrimAttribValuesByName<Vector2>" has_rundata]
opaque geo_SetPrimAttribValuesByNameVector2 (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Vector2 numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPrimAttribValuesByName<Vector3>" has_rundata]
opaque geo_SetPrimAttribValuesByNameVector3 (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Vector3 numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetPrimAttribValuesByName<Vector4>" has_rundata]
opaque geo_SetPrimAttribValuesByNameVector4 (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Vector4 numvalue) : Geometry

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValue<Dict>" has_rundata]
opaque geo_SetVertexAttribValueDict (geo : Geometry) (elemnum : Int) (attribname : String) (value : Dict) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValue<DictArray>" has_rundata]
opaque geo_SetVertexAttribValueDictArray (geo : Geometry) (elemnum : Int) (attribname : String) (value : DictArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValue<Float>" has_rundata]
opaque geo_SetVertexAttribValueFloat (geo : Geometry) (elemnum : Int) (attribname : String) (value : Float) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValue<FloatArray>" has_rundata]
opaque geo_SetVertexAttribValueFloatArray (geo : Geometry) (elemnum : Int) (attribname : String) (value : FloatArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValue<Int>" has_rundata]
opaque geo_SetVertexAttribValueInt (geo : Geometry) (elemnum : Int) (attribname : String) (value : Int) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValue<IntArray>" has_rundata]
opaque geo_SetVertexAttribValueIntArray (geo : Geometry) (elemnum : Int) (attribname : String) (value : IntArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValue<Matrix3>" has_rundata]
opaque geo_SetVertexAttribValueMatrix3 (geo : Geometry) (elemnum : Int) (attribname : String) (value : Matrix3) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValue<Matrix4>" has_rundata]
opaque geo_SetVertexAttribValueMatrix4 (geo : Geometry) (elemnum : Int) (attribname : String) (value : Matrix4) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValue<String>" has_rundata]
opaque geo_SetVertexAttribValueString (geo : Geometry) (elemnum : Int) (attribname : String) (value : String) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValue<StringArray>" has_rundata]
opaque geo_SetVertexAttribValueStringArray (geo : Geometry) (elemnum : Int) (attribname : String) (value : StringArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValue<Vector2>" has_rundata]
opaque geo_SetVertexAttribValueVector2 (geo : Geometry) (elemnum : Int) (attribname : String) (value : Vector2) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValue<Vector3>" has_rundata]
opaque geo_SetVertexAttribValueVector3 (geo : Geometry) (elemnum : Int) (attribname : String) (value : Vector3) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValue<Vector4>" has_rundata]
opaque geo_SetVertexAttribValueVector4 (geo : Geometry) (elemnum : Int) (attribname : String) (value : Vector4) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValues<Dict>" has_rundata]
opaque geo_SetVertexAttribValuesDict (geo : Geometry) (group : String) (attribname : String) (value : Dict) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValues<DictArray>" has_rundata]
opaque geo_SetVertexAttribValuesDictArray (geo : Geometry) (group : String) (attribname : String) (value : DictArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValues<Float>" has_rundata]
opaque geo_SetVertexAttribValuesFloat (geo : Geometry) (group : String) (attribname : String) (value : Float) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValues<FloatArray>" has_rundata]
opaque geo_SetVertexAttribValuesFloatArray (geo : Geometry) (group : String) (attribname : String) (value : FloatArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValues<Int>" has_rundata]
opaque geo_SetVertexAttribValuesInt (geo : Geometry) (group : String) (attribname : String) (value : Int) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValues<IntArray>" has_rundata]
opaque geo_SetVertexAttribValuesIntArray (geo : Geometry) (group : String) (attribname : String) (value : IntArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValues<Matrix3>" has_rundata]
opaque geo_SetVertexAttribValuesMatrix3 (geo : Geometry) (group : String) (attribname : String) (value : Matrix3) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValues<Matrix4>" has_rundata]
opaque geo_SetVertexAttribValuesMatrix4 (geo : Geometry) (group : String) (attribname : String) (value : Matrix4) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValues<String>" has_rundata]
opaque geo_SetVertexAttribValuesString (geo : Geometry) (group : String) (attribname : String) (value : String) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValues<StringArray>" has_rundata]
opaque geo_SetVertexAttribValuesStringArray (geo : Geometry) (group : String) (attribname : String) (value : StringArray) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValues<Vector2>" has_rundata]
opaque geo_SetVertexAttribValuesVector2 (geo : Geometry) (group : String) (attribname : String) (value : Vector2) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValues<Vector3>" has_rundata]
opaque geo_SetVertexAttribValuesVector3 (geo : Geometry) (group : String) (attribname : String) (value : Vector3) : Geometry×Bool

/-- outputs: (geo,success) -/        
@[apex_node "geo::SetVertexAttribValues<Vector4>" has_rundata]
opaque geo_SetVertexAttribValuesVector4 (geo : Geometry) (group : String) (attribname : String) (value : Vector4) : Geometry×Bool

/-- outputs: (geo) -/        
@[apex_node "geo::SetVertexAttribValuesByName<Dict>" has_rundata]
opaque geo_SetVertexAttribValuesByNameDict (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Dict numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetVertexAttribValuesByName<DictArray>" has_rundata]
opaque geo_SetVertexAttribValuesByNameDictArray (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg DictArray numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetVertexAttribValuesByName<Float>" has_rundata]
opaque geo_SetVertexAttribValuesByNameFloat (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Float numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetVertexAttribValuesByName<FloatArray>" has_rundata]
opaque geo_SetVertexAttribValuesByNameFloatArray (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg FloatArray numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetVertexAttribValuesByName<Int>" has_rundata]
opaque geo_SetVertexAttribValuesByNameInt (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Int numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetVertexAttribValuesByName<IntArray>" has_rundata]
opaque geo_SetVertexAttribValuesByNameIntArray (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg IntArray numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetVertexAttribValuesByName<Matrix3>" has_rundata]
opaque geo_SetVertexAttribValuesByNameMatrix3 (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Matrix3 numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetVertexAttribValuesByName<Matrix4>" has_rundata]
opaque geo_SetVertexAttribValuesByNameMatrix4 (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Matrix4 numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetVertexAttribValuesByName<String>" has_rundata]
opaque geo_SetVertexAttribValuesByNameString (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg String numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetVertexAttribValuesByName<StringArray>" has_rundata]
opaque geo_SetVertexAttribValuesByNameStringArray (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg StringArray numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetVertexAttribValuesByName<Vector2>" has_rundata]
opaque geo_SetVertexAttribValuesByNameVector2 (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Vector2 numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetVertexAttribValuesByName<Vector3>" has_rundata]
opaque geo_SetVertexAttribValuesByNameVector3 (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Vector3 numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SetVertexAttribValuesByName<Vector4>" has_rundata]
opaque geo_SetVertexAttribValuesByNameVector4 (geo : Geometry) (nameattrib : String) (attribname : String) {numvalue: Nat} (value : VariadicArg Vector4 numvalue) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::SmoothPoints"]
opaque geo_SmoothPoints (geo : Geometry) (pts : IntArray) (weights : FloatArray) (strength : Float) : Geometry

/-- outputs: (geo) -/        
@[apex_node "geo::Transform"]
opaque geo_Transform (geo : Geometry) (xform : Matrix4) : Geometry

-- special function not supported yet
-- opaque geo::UnpackAndTransform (geo0 : Geometry) (group : String) (limit_iterations : Int) (iterations : Int...

/-- outputs: (geo) -/        
@[apex_node "geo::UpdatePackedGeo"]
opaque geo_UpdatePackedGeo (geo : Geometry) (embedded : Geometry) (primnum : Int) : Geometry

/-- outputs: (value,success) -/        
@[apex_node "geo::VertexAttribValue<Dict>" has_rundata]
opaque geo_VertexAttribValueDict (geo : Geometry) (elemnum : Int) (attribname : String) : Dict×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::VertexAttribValue<DictArray>" has_rundata]
opaque geo_VertexAttribValueDictArray (geo : Geometry) (elemnum : Int) (attribname : String) : DictArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::VertexAttribValue<Float>" has_rundata]
opaque geo_VertexAttribValueFloat (geo : Geometry) (elemnum : Int) (attribname : String) : Float×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::VertexAttribValue<FloatArray>" has_rundata]
opaque geo_VertexAttribValueFloatArray (geo : Geometry) (elemnum : Int) (attribname : String) : FloatArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::VertexAttribValue<Int>" has_rundata]
opaque geo_VertexAttribValueInt (geo : Geometry) (elemnum : Int) (attribname : String) : Int×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::VertexAttribValue<IntArray>" has_rundata]
opaque geo_VertexAttribValueIntArray (geo : Geometry) (elemnum : Int) (attribname : String) : IntArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::VertexAttribValue<Matrix3>" has_rundata]
opaque geo_VertexAttribValueMatrix3 (geo : Geometry) (elemnum : Int) (attribname : String) : Matrix3×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::VertexAttribValue<Matrix4>" has_rundata]
opaque geo_VertexAttribValueMatrix4 (geo : Geometry) (elemnum : Int) (attribname : String) : Matrix4×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::VertexAttribValue<String>" has_rundata]
opaque geo_VertexAttribValueString (geo : Geometry) (elemnum : Int) (attribname : String) : String×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::VertexAttribValue<StringArray>" has_rundata]
opaque geo_VertexAttribValueStringArray (geo : Geometry) (elemnum : Int) (attribname : String) : StringArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::VertexAttribValue<Vector2>" has_rundata]
opaque geo_VertexAttribValueVector2 (geo : Geometry) (elemnum : Int) (attribname : String) : Vector2×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::VertexAttribValue<Vector3>" has_rundata]
opaque geo_VertexAttribValueVector3 (geo : Geometry) (elemnum : Int) (attribname : String) : Vector3×Bool

/-- outputs: (value,success) -/        
@[apex_node "geo::VertexAttribValue<Vector4>" has_rundata]
opaque geo_VertexAttribValueVector4 (geo : Geometry) (elemnum : Int) (attribname : String) : Vector4×Bool

-- special function not supported yet
-- opaque geo::VertexAttribValuesByName<Dict> (geo : Geometry) (nameattrib : String) (attribname : String) : Va...

-- special function not supported yet
-- opaque geo::VertexAttribValuesByName<DictArray> (geo : Geometry) (nameattrib : String) (attribname : String)...

-- special function not supported yet
-- opaque geo::VertexAttribValuesByName<Float> (geo : Geometry) (nameattrib : String) (attribname : String) : V...

-- special function not supported yet
-- opaque geo::VertexAttribValuesByName<FloatArray> (geo : Geometry) (nameattrib : String) (attribname : String...

-- special function not supported yet
-- opaque geo::VertexAttribValuesByName<Int> (geo : Geometry) (nameattrib : String) (attribname : String) : Var...

-- special function not supported yet
-- opaque geo::VertexAttribValuesByName<IntArray> (geo : Geometry) (nameattrib : String) (attribname : String) ...

-- special function not supported yet
-- opaque geo::VertexAttribValuesByName<Matrix3> (geo : Geometry) (nameattrib : String) (attribname : String) :...

-- special function not supported yet
-- opaque geo::VertexAttribValuesByName<Matrix4> (geo : Geometry) (nameattrib : String) (attribname : String) :...

-- special function not supported yet
-- opaque geo::VertexAttribValuesByName<String> (geo : Geometry) (nameattrib : String) (attribname : String) : ...

-- special function not supported yet
-- opaque geo::VertexAttribValuesByName<StringArray> (geo : Geometry) (nameattrib : String) (attribname : Strin...

-- special function not supported yet
-- opaque geo::VertexAttribValuesByName<Vector2> (geo : Geometry) (nameattrib : String) (attribname : String) :...

-- special function not supported yet
-- opaque geo::VertexAttribValuesByName<Vector3> (geo : Geometry) (nameattrib : String) (attribname : String) :...

-- special function not supported yet
-- opaque geo::VertexAttribValuesByName<Vector4> (geo : Geometry) (nameattrib : String) (attribname : String) :...

-- special function not supported yet
-- opaque geoutils::AttributeDelete (value : Geometry) (value : Bool) (value : String) (value : String) (value ...

-- special function not supported yet
-- opaque geoutils::CopyToPointsTargetAttribs (parm : Int) (parm : Int) (parm : Int) (parm : String) (dict : Di...

-- special function not supported yet
-- opaque geoutils::DottedLine (parm : Int) (parm : Int) (parm : Int) (parm : String) (value : Float) (value : ...

-- special function not supported yet
-- opaque geoutils::GhostedGeo (value : Geometry) (value : Vector3) (value : Float) (geo : Geometry) (group : S...

-- special function not supported yet
-- opaque geoutils::SimpleArrow (type : Int) (origin : Vector3) (dir : Vector3) (dist : Float) (points : Int) (...

/-- outputs: (value,value,m,geo,geo,t,geo) -/        
@[apex_node "geoutils::Translate"]
opaque geoutils_Translate (value : Geometry) (value : Vector3) (t : Vector3) (r : Vector3) (s : Vector3) (sh : Vector3) (p : Vector3) (pr : Vector3) (xOrd : Int) (rOrd : Int) (geo : Geometry) (xform : Matrix4) (geo : Geometry) (geo : Geometry) (t : Vector3) : Geometry×Vector3×Matrix4×Geometry×Geometry×Vector3×Geometry

/-- outputs: (value,value,vector,m,geo,geo,s,geo) -/        
@[apex_node "geoutils::UniformScale"]
opaque geoutils_UniformScale (value : Geometry) (value : Float) (x : Float) (y : Float) (z : Float) (t : Vector3) (r : Vector3) (s : Vector3) (sh : Vector3) (p : Vector3) (pr : Vector3) (xOrd : Int) (rOrd : Int) (geo : Geometry) (xform : Matrix4) (geo : Geometry) (geo : Geometry) (s : Float) : Geometry×Float×Vector3×Matrix4×Geometry×Geometry×Float×Geometry

-- special function not supported yet
-- opaque geoutils::Wrangle (value : Geometry) (value : Geometry) (value : Geometry) (value : Geometry) (value ...

/-- outputs: (graph,nodeid) -/        
@[apex_node "graph::AddNode"]
opaque graph_AddNode (graph : ApexGraphHandle) (name : String) (callback : String) (pos : Vector3) (color : Vector3) (parms : Dict) (tags : StringArray) (properties : Dict) : ApexGraphHandle×ApexNodeID

/-- outputs: (graph,nodeid) -/        
@[apex_node "graph::AddNodeToSubnet"]
opaque graph_AddNodeToSubnet (graph : ApexGraphHandle) (subnetid : ApexNodeID) (name : String) (callback : String) : ApexGraphHandle×ApexNodeID

/-- outputs: (graph,nodeid,exists_on_input,graph,nodeid,graph,name,callback,pos,color,parms,tags,properties,graph,nodeid,exists_on_input) -/        
@[apex_node "graph::AddOrUpdateNode"]
opaque graph_AddOrUpdateNode (graph : ApexGraphHandle) (name : String) (callback : String) (graph : ApexGraphHandle) (nodeid : ApexNodeID) (name : String) (callback : String) (pos : Vector3) (color : Vector3) (parms : Dict) (tags : StringArray) (properties : Dict) (graph : ApexGraphHandle) (nodeid : ApexNodeID) (exists_on_input : Bool) (graph : ApexGraphHandle) (name : String) (callback : String) (pos : Vector3) (color : Vector3) (parms : Dict) (tags : StringArray) (properties : Dict) : ApexGraphHandle×ApexNodeID×Bool×ApexGraphHandle×ApexNodeID×ApexGraphHandle×String×String×Vector3×Vector3×Dict×StringArray×Dict×ApexGraphHandle×ApexNodeID×Bool

/-- outputs: (graph,subnetnode) -/        
@[apex_node "graph::AddSubnet"]
opaque graph_AddSubnet (graph : ApexGraphHandle) (contents : ApexGraphHandle) (subnetname : String) (parent : ApexNodeID) : ApexGraphHandle×ApexNodeID

/-- outputs: (graph) -/        
@[apex_node "graph::Compile" has_rundata]
opaque graph_Compile (graph : ApexGraphHandle) (doinvoke : Bool) : ApexGraphHandle

/-- outputs: (graph) -/        
@[apex_node "graph::ConnectInput"]
opaque graph_ConnectInput (graph : ApexGraphHandle) (srcport : ApexPortID) (dstport : ApexPortID) (name : String) : ApexGraphHandle

/-- outputs: (parms) -/        
@[apex_node "graph::DefaultParms"]
opaque graph_DefaultParms (graph : ApexGraphHandle) : Dict

/-- outputs: (graph,success) -/        
@[apex_node "graph::DeleteNode"]
opaque graph_DeleteNode (graph : ApexGraphHandle) (nodeid : ApexNodeID) : ApexGraphHandle×Bool

/-- outputs: (graph) -/        
@[apex_node "graph::DeleteNodes"]
opaque graph_DeleteNodes (graph : ApexGraphHandle) (nodeids : ApexNodeIDArray) : ApexGraphHandle

/-- outputs: (graph) -/        
@[apex_node "graph::DisconnectPort"]
opaque graph_DisconnectPort (graph : ApexGraphHandle) (portid : ApexPortID) (removedynamic : Bool) : ApexGraphHandle

-- special function not supported yet
-- opaque graph::DuplicateNode (graph : ApexGraphHandle) (nodeid : ApexNodeID) (graph : ApexGraphHandle) (name ...

-- special function not supported yet
-- opaque graph::EvaluateOutputs (graph : ApexGraphHandle) (outputnode : String) {numparms: Nat} (parms : Varia...

/-- outputs: (graph,success) -/        
@[apex_node "graph::FindAndConnectInput"]
opaque graph_FindAndConnectInput (graph : ApexGraphHandle) (dstnode : ApexNodeID) (dstname : String) (dstalias : String) (srcnode : ApexNodeID) (srcname : String) (srcalias : String) (variadic : Int) (name : String) : ApexGraphHandle×Bool

/-- outputs: (graph) -/        
@[apex_node "graph::FindAndRemoveWire"]
opaque graph_FindAndRemoveWire (graph : ApexGraphHandle) (src : ApexPortID) (dst : ApexPortID) (removedynamic : Bool) : ApexGraphHandle

/-- outputs: (nodeid) -/        
@[apex_node "graph::FindFirstNode" has_rundata]
opaque graph_FindFirstNode (graph : ApexGraphHandle) (pattern : String) (breadthfirst : Bool) : ApexNodeID

/-- outputs: (portid) -/        
@[apex_node "graph::FindFirstPort" has_rundata]
opaque graph_FindFirstPort (graph : ApexGraphHandle) (pattern : String) (breadthfirst : Bool) : ApexPortID

/-- outputs: (nodeid) -/        
@[apex_node "graph::FindNode"]
opaque graph_FindNode (graph : ApexGraphHandle) (path : String) : ApexNodeID

/-- outputs: (portid) -/        
@[apex_node "graph::FindNodeInput"]
opaque graph_FindNodeInput (graph : ApexGraphHandle) (nodeid : ApexNodeID) (inputname : String) : ApexPortID

/-- outputs: (portid) -/        
@[apex_node "graph::FindNodeOutput"]
opaque graph_FindNodeOutput (graph : ApexGraphHandle) (nodeid : ApexNodeID) (outputname : String) : ApexPortID

-- special function not supported yet
-- opaque graph::FindNodeTags (parm : String) (graph : ApexGraphHandle) (nodeid : ApexNodeID) (scope : undefine...

/-- outputs: (nodes) -/        
@[apex_node "graph::FindNodes" has_rundata]
opaque graph_FindNodes (graph : ApexGraphHandle) (pattern : String) (breadthfirst : Bool) : ApexNodeIDArray

/-- outputs: (graph,nodeid,exists_on_input) -/        
@[apex_node "graph::FindOrAddNode"]
opaque graph_FindOrAddNode (graph : ApexGraphHandle) (name : String) (callback : String) : ApexGraphHandle×ApexNodeID×Bool

/-- outputs: (graph,portid) -/        
@[apex_node "graph::FindOrAddPort"]
opaque graph_FindOrAddPort (graph : ApexGraphHandle) (nodeid : ApexNodeID) (portname : String) : ApexGraphHandle×ApexPortID

/-- outputs: (portid) -/        
@[apex_node "graph::FindPort"]
opaque graph_FindPort (graph : ApexGraphHandle) (nodeid : ApexNodeID) (portname : String) : ApexPortID

/-- outputs: (ports) -/        
@[apex_node "graph::FindPorts" has_rundata]
opaque graph_FindPorts (graph : ApexGraphHandle) (pattern : String) (breadthfirst : Bool) : ApexPortIDArray

/-- outputs: (connected) -/        
@[apex_node "graph::GetConnectedNodes"]
opaque graph_GetConnectedNodes (graph : ApexGraphHandle) (portid : ApexPortID) : ApexNodeIDArray

/-- outputs: (connected,value,success,graph,portid,portid) -/        
@[apex_node "graph::GetConnectedPort"]
opaque graph_GetConnectedPort (graph : ApexGraphHandle) (portid : ApexPortID) (array : ApexPortIDArray) (index : Int) (default : ApexPortID) (portid : ApexPortID) (graph : ApexGraphHandle) (portid : ApexPortID) : ApexPortIDArray×ApexPortID×Bool×ApexGraphHandle×ApexPortID×ApexPortID

/-- outputs: (connected) -/        
@[apex_node "graph::GetConnectedPorts"]
opaque graph_GetConnectedPorts (graph : ApexGraphHandle) (portid : ApexPortID) : ApexPortIDArray

/-- outputs: (result) -/        
@[apex_node "graph::GetPromotedPort"]
opaque graph_GetPromotedPort (graph : ApexGraphHandle) (portid : ApexPortID) : ApexPortID

/-- outputs: (graph,subportid) -/        
@[apex_node "graph::GetSubPort"]
opaque graph_GetSubPort (graph : ApexGraphHandle) (portid : ApexPortID) (portname : String) : ApexGraphHandle×ApexPortID

/-- outputs: (contents) -/        
@[apex_node "graph::GetSubnetContents"]
opaque graph_GetSubnetContents (graph : ApexGraphHandle) (nodeid : ApexNodeID) : ApexGraphHandle

/-- outputs: (ports) -/        
@[apex_node "graph::GraphInputs"]
opaque graph_GraphInputs (graph : ApexGraphHandle) : ApexPortIDArray

/-- outputs: (ports) -/        
@[apex_node "graph::GraphOutputs"]
opaque graph_GraphOutputs (graph : ApexGraphHandle) : ApexPortIDArray

-- special function not supported yet
-- opaque graph::HasNodeTag (parm : String) (graph : ApexGraphHandle) (nodeid : ApexNodeID) (array : StringArra...

-- special function not supported yet
-- opaque graph::HasNodeTags (array : StringArray) (__spare__ : undefined) (scope : undefined) (array : StringA...

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<AnimChannel>" has_rundata]
opaque graph_IfConnectedAnimChannel (input : AnimChannel) (fallback : AnimChannel) (checkdefault : Bool) : AnimChannel×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<AnimChannelCollection>" has_rundata]
opaque graph_IfConnectedAnimChannelCollection (input : AnimChannelCollection) (fallback : AnimChannelCollection) (checkdefault : Bool) : AnimChannelCollection×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<AnimStack>" has_rundata]
opaque graph_IfConnectedAnimStack (input : AnimStack) (fallback : AnimStack) (checkdefault : Bool) : AnimStack×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<ApexNodeID>" has_rundata]
opaque graph_IfConnectedApexNodeID (input : ApexNodeID) (fallback : ApexNodeID) (checkdefault : Bool) : ApexNodeID×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<ApexNodeIDArray>" has_rundata]
opaque graph_IfConnectedApexNodeIDArray (input : ApexNodeIDArray) (fallback : ApexNodeIDArray) (checkdefault : Bool) : ApexNodeIDArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<ApexPortID>" has_rundata]
opaque graph_IfConnectedApexPortID (input : ApexPortID) (fallback : ApexPortID) (checkdefault : Bool) : ApexPortID×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<ApexPortIDArray>" has_rundata]
opaque graph_IfConnectedApexPortIDArray (input : ApexPortIDArray) (fallback : ApexPortIDArray) (checkdefault : Bool) : ApexPortIDArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<Bool>" has_rundata]
opaque graph_IfConnectedBool (input : Bool) (fallback : Bool) (checkdefault : Bool) : Bool×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<BoolArray>" has_rundata]
opaque graph_IfConnectedBoolArray (input : BoolArray) (fallback : BoolArray) (checkdefault : Bool) : BoolArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<ColorRamp>" has_rundata]
opaque graph_IfConnectedColorRamp (input : ColorRamp) (fallback : ColorRamp) (checkdefault : Bool) : ColorRamp×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<DataItem>" has_rundata]
opaque graph_IfConnectedDataItem (input : DataItem) (fallback : DataItem) (checkdefault : Bool) : DataItem×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<Dict>" has_rundata]
opaque graph_IfConnectedDict (input : Dict) (fallback : Dict) (checkdefault : Bool) : Dict×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<DictArray>" has_rundata]
opaque graph_IfConnectedDictArray (input : DictArray) (fallback : DictArray) (checkdefault : Bool) : DictArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<DynamicPath>" has_rundata]
opaque graph_IfConnectedDynamicPath (input : DynamicPath) (fallback : DynamicPath) (checkdefault : Bool) : DynamicPath×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<DynamicPathArray>" has_rundata]
opaque graph_IfConnectedDynamicPathArray (input : DynamicPathArray) (fallback : DynamicPathArray) (checkdefault : Bool) : DynamicPathArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<FBIKSkeleton>" has_rundata]
opaque graph_IfConnectedFBIKSkeleton (input : FBIKSkeleton) (fallback : FBIKSkeleton) (checkdefault : Bool) : FBIKSkeleton×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<FBIKSkeletonArray>" has_rundata]
opaque graph_IfConnectedFBIKSkeletonArray (input : FBIKSkeletonArray) (fallback : FBIKSkeletonArray) (checkdefault : Bool) : FBIKSkeletonArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<FBIKSolver>" has_rundata]
opaque graph_IfConnectedFBIKSolver (input : FBIKSolver) (fallback : FBIKSolver) (checkdefault : Bool) : FBIKSolver×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<FBIKSolverArray>" has_rundata]
opaque graph_IfConnectedFBIKSolverArray (input : FBIKSolverArray) (fallback : FBIKSolverArray) (checkdefault : Bool) : FBIKSolverArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<FBIKTarget>" has_rundata]
opaque graph_IfConnectedFBIKTarget (input : FBIKTarget) (fallback : FBIKTarget) (checkdefault : Bool) : FBIKTarget×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<FBIKTargetArray>" has_rundata]
opaque graph_IfConnectedFBIKTargetArray (input : FBIKTargetArray) (fallback : FBIKTargetArray) (checkdefault : Bool) : FBIKTargetArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<Float>" has_rundata]
opaque graph_IfConnectedFloat (input : Float) (fallback : Float) (checkdefault : Bool) : Float×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<FloatArray>" has_rundata]
opaque graph_IfConnectedFloatArray (input : FloatArray) (fallback : FloatArray) (checkdefault : Bool) : FloatArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<FloatRamp>" has_rundata]
opaque graph_IfConnectedFloatRamp (input : FloatRamp) (fallback : FloatRamp) (checkdefault : Bool) : FloatRamp×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<Geometry>" has_rundata]
opaque graph_IfConnectedGeometry (input : Geometry) (fallback : Geometry) (checkdefault : Bool) : Geometry×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<GeometryArray>" has_rundata]
opaque graph_IfConnectedGeometryArray (input : GeometryArray) (fallback : GeometryArray) (checkdefault : Bool) : GeometryArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<Int>" has_rundata]
opaque graph_IfConnectedInt (input : Int) (fallback : Int) (checkdefault : Bool) : Int×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<IntArray>" has_rundata]
opaque graph_IfConnectedIntArray (input : IntArray) (fallback : IntArray) (checkdefault : Bool) : IntArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<Matrix3>" has_rundata]
opaque graph_IfConnectedMatrix3 (input : Matrix3) (fallback : Matrix3) (checkdefault : Bool) : Matrix3×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<Matrix3Array>" has_rundata]
opaque graph_IfConnectedMatrix3Array (input : Matrix3Array) (fallback : Matrix3Array) (checkdefault : Bool) : Matrix3Array×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<Matrix4>" has_rundata]
opaque graph_IfConnectedMatrix4 (input : Matrix4) (fallback : Matrix4) (checkdefault : Bool) : Matrix4×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<Matrix4Array>" has_rundata]
opaque graph_IfConnectedMatrix4Array (input : Matrix4Array) (fallback : Matrix4Array) (checkdefault : Bool) : Matrix4Array×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<SimRootDataId>" has_rundata]
opaque graph_IfConnectedSimRootDataId (input : SimRootDataId) (fallback : SimRootDataId) (checkdefault : Bool) : SimRootDataId×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<SimRootDataIdArray>" has_rundata]
opaque graph_IfConnectedSimRootDataIdArray (input : SimRootDataIdArray) (fallback : SimRootDataIdArray) (checkdefault : Bool) : SimRootDataIdArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<String>" has_rundata]
opaque graph_IfConnectedString (input : String) (fallback : String) (checkdefault : Bool) : String×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<StringArray>" has_rundata]
opaque graph_IfConnectedStringArray (input : StringArray) (fallback : StringArray) (checkdefault : Bool) : StringArray×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<Vector2>" has_rundata]
opaque graph_IfConnectedVector2 (input : Vector2) (fallback : Vector2) (checkdefault : Bool) : Vector2×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<Vector2Array>" has_rundata]
opaque graph_IfConnectedVector2Array (input : Vector2Array) (fallback : Vector2Array) (checkdefault : Bool) : Vector2Array×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<Vector3>" has_rundata]
opaque graph_IfConnectedVector3 (input : Vector3) (fallback : Vector3) (checkdefault : Bool) : Vector3×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<Vector3Array>" has_rundata]
opaque graph_IfConnectedVector3Array (input : Vector3Array) (fallback : Vector3Array) (checkdefault : Bool) : Vector3Array×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<Vector4>" has_rundata]
opaque graph_IfConnectedVector4 (input : Vector4) (fallback : Vector4) (checkdefault : Bool) : Vector4×Bool

/-- outputs: (value,success) -/        
@[apex_node "graph::IfConnected<Vector4Array>" has_rundata]
opaque graph_IfConnectedVector4Array (input : Vector4Array) (fallback : Vector4Array) (checkdefault : Bool) : Vector4Array×Bool

-- special function not supported yet
-- opaque graph::Invoke (graph : ApexGraphHandle) (outputs : StringArray) (pattern : String) {numparms: Nat} (p...

-- special function not supported yet
-- opaque graph::IsConnected (input : void) (checkdefault : Bool) : Bool...

/-- outputs: (graph) -/        
@[apex_node "graph::Layout"]
opaque graph_Layout (graph : ApexGraphHandle) (nodes : ApexNodeIDArray) (spacing : Vector2) (offset : Vector2) (traverse : Bool) : ApexGraphHandle

/-- outputs: (graph) -/        
@[apex_node "graph::LoadFromGeometry"]
opaque graph_LoadFromGeometry (geo : Geometry) (doinvoke : Bool) (graphname : String) : ApexGraphHandle

/-- outputs: (graph) -/        
@[apex_node "graph::Merge"]
opaque graph_Merge (graph : ApexGraphHandle) (mergegraph : ApexGraphHandle) : ApexGraphHandle

/-- outputs: (name,callback,pos,color,parms,tags,properties,output) -/        
@[apex_node "graph::NodeData" has_rundata]
opaque graph_NodeData (graph : ApexGraphHandle) (nodeid : ApexNodeID) : String×String×Vector3×Vector3×Dict×StringArray×Dict×Dict

/-- outputs: (ports) -/        
@[apex_node "graph::NodeInputs"]
opaque graph_NodeInputs (graph : ApexGraphHandle) (node : ApexNodeID) : ApexPortIDArray

/-- outputs: (ports) -/        
@[apex_node "graph::NodeOutputs"]
opaque graph_NodeOutputs (graph : ApexGraphHandle) (node : ApexNodeID) : ApexPortIDArray

/-- outputs: (graph,subnetnode) -/        
@[apex_node "graph::PackSubnet"]
opaque graph_PackSubnet (graph : ApexGraphHandle) (subnetname : String) (nodes : ApexNodeIDArray) : ApexGraphHandle×ApexNodeID

/-- outputs: (graph,success,graph,success,graph,child,parent,graph) -/        
@[apex_node "graph::ParentNodes"]
opaque graph_ParentNodes (graph : ApexGraphHandle) (dstnode : ApexNodeID) (dstname : String) (dstalias : String) (srcnode : ApexNodeID) (srcname : String) (srcalias : String) (variadic : Int) (name : String) (graph : ApexGraphHandle) (dstnode : ApexNodeID) (dstname : String) (dstalias : String) (srcnode : ApexNodeID) (srcname : String) (srcalias : String) (variadic : Int) (name : String) (graph : ApexGraphHandle) (graph : ApexGraphHandle) (child : ApexNodeID) (parent : ApexNodeID) : ApexGraphHandle×Bool×ApexGraphHandle×Bool×ApexGraphHandle×ApexNodeID×ApexNodeID×ApexGraphHandle

/-- outputs: (name,alias,datatype,porttype,isconnected,ispromoted,linkedport) -/        
@[apex_node "graph::PortData" has_rundata]
opaque graph_PortData (graph : ApexGraphHandle) (portid : ApexPortID) : String×String×String×String×Bool×Bool×ApexPortID

/-- outputs: (nodeid) -/        
@[apex_node "graph::PortNode"]
opaque graph_PortNode (graph : ApexGraphHandle) (portid : ApexPortID) : ApexNodeID

/-- outputs: (graph,graphinput) -/        
@[apex_node "graph::PromoteInput"]
opaque graph_PromoteInput (graph : ApexGraphHandle) (portid : ApexPortID) (subportname : String) (parmnodeid : ApexNodeID) (parmname : String) : ApexGraphHandle×ApexPortID

-- special function not supported yet
-- opaque graph::PromoteNodeInput (input : String) (separators : String) (array : StringArray) (a : Int) (b : I...

-- special function not supported yet
-- opaque graph::PromoteNodeInputs (graph : ApexGraphHandle) (nodeid : ApexNodeID) (portname : String) (graph :...

-- special function not supported yet
-- opaque graph::PromoteNodeOutput (input : String) (separators : String) (array : StringArray) (a : Int) (b : ...

-- special function not supported yet
-- opaque graph::PromoteNodeOutputs (input : ApexNodeID) (checkdefault : Bool) (parm : String) (a : String) (b ...

/-- outputs: (graph,graphoutput) -/        
@[apex_node "graph::PromoteOutput"]
opaque graph_PromoteOutput (graph : ApexGraphHandle) (portid : ApexPortID) (outputname : String) (outputnodeid : ApexNodeID) : ApexGraphHandle×ApexPortID

-- special function not supported yet
-- opaque graph::PromotePort (parm : String) (graph : ApexGraphHandle) (portid : ApexPortID) (a : String) (b : ...

/-- outputs: (properties) -/        
@[apex_node "graph::Properties"]
opaque graph_Properties (graph : ApexGraphHandle) : Dict

/-- outputs: (graph,success) -/        
@[apex_node "graph::RenamePort"]
opaque graph_RenamePort (graph : ApexGraphHandle) (portid : ApexPortID) (name : String) : ApexGraphHandle×Bool

-- special function not supported yet
-- opaque graph::ReplaceNodeTag (graph : ApexGraphHandle) (nodeid : ApexNodeID) (tags : StringArray) (clear : B...

/-- outputs: (graph) -/        
@[apex_node "graph::RewireOutputs"]
opaque graph_RewireOutputs (graph : ApexGraphHandle) (port : ApexPortID) (targetport : ApexPortID) : ApexGraphHandle

/-- outputs: (geo) -/        
@[apex_node "graph::SaveToGeometry"]
opaque graph_SaveToGeometry (graph : ApexGraphHandle) (nodeoutput : Bool) (debug : Bool) : Geometry

/-- outputs: (graph) -/        
@[apex_node "graph::SetDefaultParms"]
opaque graph_SetDefaultParms (graph : ApexGraphHandle) (parms : Dict) (clear : Bool) (addmissing : Bool) : ApexGraphHandle

/-- outputs: (graph) -/        
@[apex_node "graph::SetProperties"]
opaque graph_SetProperties (graph : ApexGraphHandle) (properties : Dict) (clear : Bool) (addmissing : Bool) : ApexGraphHandle

/-- outputs: (graph) -/        
@[apex_node "graph::SetSubnetContents"]
opaque graph_SetSubnetContents (graph : ApexGraphHandle) (nodeid : ApexNodeID) (contents : ApexGraphHandle) : ApexGraphHandle

/-- outputs: (graph) -/        
@[apex_node "graph::Sort"]
opaque graph_Sort (graph : ApexGraphHandle) (layoutnodes : Bool) : ApexGraphHandle

/-- outputs: (graph) -/        
@[apex_node "graph::Template" has_rundata]
opaque graph_Template  : ApexGraphHandle

/-- outputs: (graph) -/        
@[apex_node "graph::UnpackSubnet"]
opaque graph_UnpackSubnet (graph : ApexGraphHandle) (nodeid : ApexNodeID) : ApexGraphHandle

/-- outputs: (graph,nodeid) -/        
@[apex_node "graph::UpdateNode"]
opaque graph_UpdateNode (graph : ApexGraphHandle) (nodeid : ApexNodeID) (name : String) (callback : String) (pos : Vector3) (color : Vector3) (parms : Dict) (tags : StringArray) (properties : Dict) : ApexGraphHandle×ApexNodeID

/-- outputs: (graph,nodeid) -/        
@[apex_node "graph::UpdateNodeParms"]
opaque graph_UpdateNodeParms (graph : ApexGraphHandle) (nodeid : ApexNodeID) (parms : Dict) (clear : Bool) : ApexGraphHandle×ApexNodeID

/-- outputs: (graph,nodeid) -/        
@[apex_node "graph::UpdateNodeProperties"]
opaque graph_UpdateNodeProperties (graph : ApexGraphHandle) (nodeid : ApexNodeID) (properties : Dict) (clear : Bool) : ApexGraphHandle×ApexNodeID

/-- outputs: (graph,nodeid) -/        
@[apex_node "graph::UpdateNodeTags"]
opaque graph_UpdateNodeTags (graph : ApexGraphHandle) (nodeid : ApexNodeID) (tags : StringArray) (clear : Bool) : ApexGraphHandle×ApexNodeID

/-- outputs: (accept) -/        
@[apex_node "graphutils::AcceptsSubport"]
opaque graphutils_AcceptsSubport (graph : ApexGraphHandle) (portid : ApexPortID) : Bool

/-- outputs: (ancestors) -/        
@[apex_node "graphutils::NodeAncestors"]
opaque graphutils_NodeAncestors (graph : ApexGraphHandle) (nodeid : ApexNodeID) : ApexNodeIDArray

/-- outputs: (callback) -/        
@[apex_node "graphutils::NodeCallbackName"]
opaque graphutils_NodeCallbackName (graph : ApexGraphHandle) (nodeid : ApexNodeID) : String

/-- outputs: (name) -/        
@[apex_node "graphutils::NodeName"]
opaque graphutils_NodeName (graph : ApexGraphHandle) (nodeid : ApexNodeID) : String

/-- outputs: (parent) -/        
@[apex_node "graphutils::NodeParent"]
opaque graphutils_NodeParent (graph : ApexGraphHandle) (nodeid : ApexNodeID) : ApexNodeID

/-- outputs: (parms) -/        
@[apex_node "graphutils::NodeParms"]
opaque graphutils_NodeParms (graph : ApexGraphHandle) (nodeid : ApexNodeID) : Dict

/-- outputs: (path) -/        
@[apex_node "graphutils::NodePath"]
opaque graphutils_NodePath (graph : ApexGraphHandle) (nodeid : ApexNodeID) : String

/-- outputs: (properties) -/        
@[apex_node "graphutils::NodeProperties"]
opaque graphutils_NodeProperties (graph : ApexGraphHandle) (nodeid : ApexNodeID) : Dict

/-- outputs: (tags) -/        
@[apex_node "graphutils::NodeTags"]
opaque graphutils_NodeTags (graph : ApexGraphHandle) (nodeid : ApexNodeID) : StringArray

/-- outputs: (outerport) -/        
@[apex_node "graphutils::OuterPort"]
opaque graphutils_OuterPort (graph : ApexGraphHandle) (portid : ApexPortID) : ApexPortID

/-- outputs: (path) -/        
@[apex_node "graphutils::PortPath"]
opaque graphutils_PortPath (graph : ApexGraphHandle) (portid : ApexPortID) (qualify : Bool) : String

-- special function not supported yet
-- opaque guide::AddSetPointTransforms (value : ApexGraphHandle) (value : String) (value : ApexNodeIDArray) (va...

-- special function not supported yet
-- opaque guide::ControlsFromGuides (value : ApexGraphHandle) (value : Geometry) (value : String) (value : Stri...

-- special function not supported yet
-- opaque guide::ExtractGuides (input : String) (find : String) (replace : String) (input : Geometry) (checkdef...

-- special function not supported yet
-- opaque guide::FindOrAddGuide (value : Geometry) (value : String) (value : Matrix4) (value : Int) (value : In...

-- special function not supported yet
-- opaque guide::FindPrimaryAxis (parm : Vector3) (parm : Vector3) (parm : Vector3) (parm : Vector3) (parm : Ve...

-- special function not supported yet
-- opaque guide::SetGuideParent (parm : String) (value : ApexGraphHandle) (value : Geometry) (value : String) (...

-- special function not supported yet
-- opaque guide::SetGuideProperties (value : Geometry) (value : Int) (value : Int) (value : Int) (value : Int) ...

-- special function not supported yet
-- opaque guide::UpdateGuides (input : Bool) (condition : Bool) (__spare__ : undefined) (parm : String) (dict :...

-- special function not supported yet
-- opaque guide::UpdatePatternFromGuides (parm : String) (parm : String) (value : String) (value : Geometry) (v...

-- special function not supported yet
-- opaque guide::UpdateShapesFromGuides (value : ApexGraphHandle) (value : Geometry) (value : Geometry) (geo0 :...

-- special function not supported yet
-- opaque guidecomponent::Add2BoneIK (parm : StringArray) (parm : String) (parm : IntArray) (parm : Float) (par...

-- special function not supported yet
-- opaque guidecomponent::AddComponentControl (parm : Dict) (parm : Int) (parm : Int) (parm : Int) (parm : Int)...

-- special function not supported yet
-- opaque guidecomponent::AddComponentLimb (x : Float) (y : Float) (z : Float) (parm : Float) (parm : Float) (p...

-- special function not supported yet
-- opaque guidecomponent::AddComponentRoot (parm : Vector3) (parm : Float) (parm : String) (value : Geometry) (...

-- special function not supported yet
-- opaque guidecomponent::AddComponentSkeleton (parm : Bool) (parm : Bool) (parm : Bool) (parm : Bool) (parm : ...

-- special function not supported yet
-- opaque guidecomponent::AddComponentSpline (parm : String) (parm : String) (parm : StringArray) (parm : Bool)...

-- special function not supported yet
-- opaque guidecomponent::AddComponentTag (value : Geometry) (value : Geometry) (value : ApexGraphHandle) (valu...

-- special function not supported yet
-- opaque guidecomponent::AddSegmentControl (parm : String) (parm : String) (parm : String) (parm : String) (pa...

-- special function not supported yet
-- opaque guidecomponent::MirrorGuideControls (parm : String) (parm : StringArray) (parm : Dict) (parm : Bool) ...

-- special function not supported yet
-- opaque guidecomponent::SkeletonToFK (parm : Dict) (parm : Bool) (parm : Bool) (parm : Dict) (parm : String) ...

-- special function not supported yet
-- opaque guidecomponentutils::Add2BoneIKCore (parm : Vector3) (parm : Float) (value : Matrix4) (value : Matrix...

-- special function not supported yet
-- opaque guidecomponentutils::AddComponentSkeletonCore (parm : Bool) (parm : Bool) (parm : StringArray) (parm ...

/-- outputs: (radians) -/        
@[apex_node "quaternion::Distance"]
opaque quaternion_Distance (a : Vector4) (b : Vector4) : Float

/-- outputs: (quaternion) -/        
@[apex_node "quaternion::FromAxisAngle"]
opaque quaternion_FromAxisAngle (axis : Vector3) (radians : Float) : Vector4

/-- outputs: (quaternion) -/        
@[apex_node "quaternion::FromEuler"]
opaque quaternion_FromEuler (radians : Vector3) (rOrd : Int) : Vector4

/-- outputs: (quaternion) -/        
@[apex_node "quaternion::FromMatrix<Matrix3>"]
opaque quaternion_FromMatrixMatrix3 (matrix : Matrix3) : Vector4

/-- outputs: (quaternion) -/        
@[apex_node "quaternion::FromMatrix<Matrix4>"]
opaque quaternion_FromMatrixMatrix4 (matrix : Matrix4) : Vector4

/-- outputs: (result) -/        
@[apex_node "quaternion::Invert"]
opaque quaternion_Invert (a : Vector4) : Vector4

/-- outputs: (result) -/        
@[apex_node "quaternion::Multiply"]
opaque quaternion_Multiply (a : Vector4) (b : Vector4) : Vector4

/-- outputs: (result) -/        
@[apex_node "quaternion::Rotate"]
opaque quaternion_Rotate (vector : Vector3) (quaternion : Vector4) : Vector3

/-- outputs: (swing,twist) -/        
@[apex_node "quaternion::SwingTwistDecompose<Float>"]
opaque quaternion_SwingTwistDecomposeFloat (quaternion : Vector4) (axis : Vector3) (reverse : Bool) : Vector4×Float

/-- outputs: (swing,twist) -/        
@[apex_node "quaternion::SwingTwistDecompose<Vector4>"]
opaque quaternion_SwingTwistDecomposeVector4 (quaternion : Vector4) (axis : Vector3) (reverse : Bool) : Vector4×Vector4

/-- outputs: (result,result,result,value,result,result,result,value,result,result,value,out,b,result,swing,twist,a,b,axis,bias,swing_bias_scale,twist_bias_scale,reverse,result) -/        
@[apex_node "quaternion::SwingTwistInterpolate"]
opaque quaternion_SwingTwistInterpolate (a : Vector4) (b : Vector4) (bias : Float) (a : Vector4) (b : Vector4) (bias : Float) (a : Vector4) (b : Vector4) (parm : Float) (a : Vector4) (a : Vector4) (b : Vector4) (a : Vector4) (b : Vector4) (parm : Vector4) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (parm : Bool) {numinput: Nat} (input : VariadicArg Vector4 numinput) (index : Int) (a : Bool) (a : Vector4) (b : Vector4) (quaternion : Vector4) (axis : Vector3) (reverse : Bool) (result : Vector4) (input0 : Vector4) (input1 : Vector4) (b0 : Float) (b0 : Float) (a : Vector4) (b : Vector4) (axis : Vector3) (bias : Float) (swing_bias_scale : Float) (twist_bias_scale : Float) (reverse : Bool) : Vector4×Vector4×Vector4×Float×Vector4×Vector4×Vector4×Vector4×Float×Float×Bool×Vector4×Int×Vector4×Vector4×Vector4×Vector4×Vector4×Vector3×Float×Float×Float×Bool×Vector4

/-- outputs: (axis,radians) -/        
@[apex_node "quaternion::ToAxisAngle"]
opaque quaternion_ToAxisAngle (quaternion : Vector4) : Vector3×Float

/-- outputs: (radians) -/        
@[apex_node "quaternion::ToEuler"]
opaque quaternion_ToEuler (quaternion : Vector4) (rOrd : Int) : Vector3

/-- outputs: (matrix) -/        
@[apex_node "quaternion::ToMatrix<Matrix3>"]
opaque quaternion_ToMatrixMatrix3 (quaternion : Vector4) : Matrix3

/-- outputs: (matrix) -/        
@[apex_node "quaternion::ToMatrix<Matrix4>"]
opaque quaternion_ToMatrixMatrix4 (quaternion : Vector4) : Matrix4

-- special function not supported yet
-- opaque ragdoll::AddMappingToControl (parm : Dict) (value : ApexGraphHandle) (value : ApexNodeID) (value : St...

/-- outputs: (value,value,value,value,value,value,value,value,value,value,value,value,dict,dict,dict,dict,dict,dict,dict,dict,array,index,dict,graph,graph,character_name,collisionshapes_path,collisionshapes_path_is_relative,configuration_attribute_name,mapping_property_name,skeleton_path,skeleton_path_is_relative,skeleton_rig_output_path,graph) -/        
@[apex_node "ragdoll::CreateRagdollConfiguration"]
opaque ragdoll_CreateRagdollConfiguration (parm : Dict) (parm : DictArray) (parm : Dict) (value : ApexGraphHandle) (value : String) (value : String) (value : Bool) (value : String) (value : String) (value : String) (value : Bool) (value : String) (dict : Dict) (key : String) (value : String) (dict : Dict) (key : String) (value : String) (dict : Dict) (key : String) (value : Bool) (dict : Dict) (key : String) (value : String) (dict : Dict) (key : String) (value : String) (dict : Dict) (key : String) (value : String) (dict : Dict) (key : String) (value : Bool) (dict : Dict) (key : String) (value : String) (array : DictArray) (value : Dict) (dict : Dict) (key : String) (value : DictArray) (graph : ApexGraphHandle) (properties : Dict) (clear : Bool) (addmissing : Bool) (graph : ApexGraphHandle) (graph : ApexGraphHandle) (character_name : String) (collisionshapes_path : String) (collisionshapes_path_is_relative : Bool) (configuration_attribute_name : String) (mapping_property_name : String) (skeleton_path : String) (skeleton_path_is_relative : Bool) (skeleton_rig_output_path : String) : Dict×DictArray×Dict×ApexGraphHandle×String×String×Bool×String×String×String×Bool×String×Dict×Dict×Dict×Dict×Dict×Dict×Dict×Dict×DictArray×Int×Dict×ApexGraphHandle×ApexGraphHandle×String×String×Bool×String×String×String×Bool×String×ApexGraphHandle

/-- outputs: (value,b,b,m,m) -/        
@[apex_node "ragdoll::RotateOnlyMatrix"]
opaque ragdoll_RotateOnlyMatrix (value : Matrix4) (a : Matrix4) (a : Matrix3) (m : Matrix4) (m : Matrix4) : Matrix4×Matrix3×Matrix4×Matrix4×Matrix4

/-- outputs: (value,value,value,value,value,value,value,value,value,t,r,s,sh,t,r,s,sh,result,result,result,dist,dict,dict,dict,constraints,src_xform,dst_xform,src_local,dst_local,constraint_name,src_offsetS,dst_offsetS,restlengthS,constraints) -/        
@[apex_node "ragdoll::SetConstraintAttributes"]
opaque ragdoll_SetConstraintAttributes (value : Dict) (value : Matrix4) (value : Matrix4) (value : Matrix4) (value : Matrix4) (value : String) (value : String) (value : String) (value : String) (m : Matrix4) (xOrd : Int) (rOrd : Int) (p : Vector3) (pr : Vector3) (m : Matrix4) (xOrd : Int) (rOrd : Int) (p : Vector3) (pr : Vector3) {numvalues: Nat} (values : VariadicArg String numvalues) {numvalues: Nat} (values : VariadicArg String numvalues) {numvalues: Nat} (values : VariadicArg String numvalues) (a : Vector3) (b : Vector3) (dict : Dict) (keys : StringArray) (value : Matrix4) (dict : Dict) (keys : StringArray) (value : Matrix4) (dict : Dict) (keys : StringArray) (value : Float) (values0 : String) (values1 : String) (values0 : String) (values1 : String) (values0 : String) (values1 : String) (constraints : Dict) (constraints : Dict) (src_xform : Matrix4) (dst_xform : Matrix4) (src_local : Matrix4) (dst_local : Matrix4) (constraint_name : String) (src_offsetS : String) (dst_offsetS : String) (restlengthS : String) : Dict×Matrix4×Matrix4×Matrix4×Matrix4×String×String×String×String×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×StringArray×StringArray×StringArray×Float×Dict×Dict×Dict×Dict×Matrix4×Matrix4×Matrix4×Matrix4×String×String×String×String×Dict

-- special function not supported yet
-- opaque ragdoll::Solve (parm : GeometryArray) (parm : GeometryArray) (parm : GeometryArray) (parm : GeometryA...

/-- outputs: (value,t,r,s,sh,m,m,m) -/        
@[apex_node "ragdoll::TranslateOnlyMatrix"]
opaque ragdoll_TranslateOnlyMatrix (value : Matrix4) (m : Matrix4) (xOrd : Int) (rOrd : Int) (p : Vector3) (pr : Vector3) (t : Vector3) (r : Vector3) (s : Vector3) (sh : Vector3) (p : Vector3) (pr : Vector3) (xOrd : Int) (rOrd : Int) (m : Matrix4) (m : Matrix4) : Matrix4×Vector3×Vector3×Vector3×Vector3×Matrix4×Matrix4×Matrix4

/-- outputs: (geometry,constraints) -/        
@[apex_node "ragdoll::internal::UpdateSimulationGeometry" has_rundata]
opaque ragdoll_internal_UpdateSimulationGeometry (geometry : Geometry) (constraints : Geometry) (skeleton : GeometryArray) (channels : GeometryArray) (globaltime : Float) (resetvelocities : Bool) (customconstraints : Dict) (constraintschannels : Geometry) : Geometry×Geometry

-- special function not supported yet
-- opaque rig::AbstractControl (xform : Matrix4) (x : Float) (y : Float) (__spare__ : undefined) : Matrix4×Floa...

/-- outputs: (localxform) -/        
@[apex_node "rig::CombineParmTransform"]
opaque rig_CombineParmTransform (inputlocalxform : Matrix4) (parentlocalscale : Vector3) (xord : Int) (rord : Int) (t : Vector3) (r : Vector3) (s : Vector3) (p : Vector3) (pr : Vector3) (scaleinheritance : Int) (method : Int) : Matrix4

/-- outputs: (localxform) -/        
@[apex_node "rig::CombineParmTransform::2.0"]
opaque rig_CombineParmTransform_2_0 (restlocal : Matrix4) (t : Vector3) (r : Vector3) (s : Vector3) (sh : Vector3) (p : Vector3) (pr : Vector3) (xord : Int) (rord : Int) (mode : Int) : Matrix4

-- special function not supported yet
-- opaque rig::ControlRigExtract (parm : Geometry) (parm : ApexGraphHandle) (a : String) (b : String) (index : ...

-- special function not supported yet
-- opaque rig::ControlRigExtractSkel (parm : Geometry) (parm : ApexGraphHandle) (a : String) (b : String) (inde...

/-- outputs: (shapeoffset,color,visibility,shapetype) -/        
@[apex_node "rig::ControlShape"]
opaque rig_ControlShape (control : Matrix4) (shapeoffset : Matrix4) (color : Vector3) (visibility : Bool) (shapetype : String) : Matrix4×Vector3×Bool×String

/-- outputs: (geo) -/        
@[apex_node "rig::ControlSpline" has_rundata]
opaque rig_ControlSpline {numcvs: Nat} (cvs : VariadicArg Matrix4 numcvs) {numtwists: Nat} (twists : VariadicArg Float numtwists) (order : Int) : Geometry

/-- outputs: (geo) -/        
@[apex_node "rig::ControlSpline::3.0" has_rundata]
opaque rig_ControlSpline_3_0 {numcvs: Nat} (cvs : VariadicArg Matrix4 numcvs) (splinetype : Int) (order : Int) : Geometry

/-- outputs: (geo) -/        
@[apex_node "rig::ControlSplineFromArray" has_rundata]
opaque rig_ControlSplineFromArray (cvs : Matrix4Array) (twists : FloatArray) (order : Int) : Geometry

/-- outputs: (geo) -/        
@[apex_node "rig::ControlSplineFromArray::3.0" has_rundata]
opaque rig_ControlSplineFromArray_3_0 (cvs : Matrix4Array) (splinetype : Int) (order : Int) : Geometry

/-- outputs: (xform) -/        
@[apex_node "rig::CurveConstraint" has_rundata]
opaque rig_CurveConstraint (geo : Geometry) (prim : Int) (u : Float) (startlocalxform : Matrix4) (upoverride : Vector3) (numsubdivs : Int) : Matrix4

-- special function not supported yet
-- opaque rig::CurveIK (curve : Geometry) (curvexform : Matrix4) {numrestlocal: Nat} (restlocal : VariadicArg M...

/-- outputs: (localxform,effectivelocal) -/        
@[apex_node "rig::ExtractLocalTransform"]
opaque rig_ExtractLocalTransform (xform : Matrix4) (parent : Matrix4) (parentlocal : Matrix4) (scaleinheritance : Int) : Matrix4×Matrix4

/-- outputs: (t,r,s) -/        
@[apex_node "rig::ExtractParmTransform"]
opaque rig_ExtractParmTransform (local' : Matrix4) (inputlocal : Matrix4) (parentlocalscale : Vector3) (xord : Int) (rord : Int) (scaleinheritance : Int) (method : Int) : Vector3×Vector3×Vector3

/-- outputs: (t,r,s,sh) -/        
@[apex_node "rig::ExtractParmTransform::2.0"]
opaque rig_ExtractParmTransform_2_0 (localxform : Matrix4) (restlocal : Matrix4) (p : Vector3) (pr : Vector3) (xord : Int) (rord : Int) (mode : Int) (reflecthint : Vector3) : Vector3×Vector3×Vector3×Vector3

/-- outputs: (xform,localxform,pretransform) -/        
@[apex_node "rig::FkTransform"]
opaque rig_FkTransform (local' : Matrix4) (restlocal : Matrix4) (parent : Matrix4) (parentlocal : Matrix4) (scaleinheritance : Int) : Matrix4×Matrix4×Matrix4

-- special function not supported yet
-- opaque rig::MultiBoneIK {numin': Nat} (in' : VariadicArg Matrix4 numin') (rootdriver : Matrix4) (twist : Mat...

/-- outputs: (out) -/        
@[apex_node "rig::MultiBoneIKFromArray" has_rundata]
opaque rig_MultiBoneIKFromArray (in' : Matrix4Array) (rootdriver : Matrix4) (twist : Matrix4) (goal : Matrix4) (stretch : Float) (squash : Float) (blend : Float) (dampen : Float) (trackingthreshold : Float) (twistoffset : Float) (usetwist : Bool) (resiststraight : Bool) (keeproot : Bool) (keepscales : Bool) (usegoalrot : Bool) : Matrix4Array

/-- outputs: (parent) -/        
@[apex_node "rig::ParentBlend"]
opaque rig_ParentBlend (parent : Matrix4) (parent_bind : Matrix4) (newparent : Matrix4) (newparent_bind : Matrix4) (blend : Float) (components : Int) : Matrix4

/-- outputs: (xform) -/        
@[apex_node "rig::PointConstraint" has_rundata]
opaque rig_PointConstraint (geo : Geometry) (pt_group_name : String) (weights : FloatArray) (x_axis_name : String) (y_axis_name : String) : Matrix4

/-- outputs: (weights) -/        
@[apex_node "rig::PoseWeightInterpolation" has_rundata]
opaque rig_PoseWeightInterpolation (target : FloatArray) (examples : FloatArray) (maxcellsize : Float) (smoothingmode : Int) (passband : Float) (iterations : Int) (secondordersmooth : Bool) (boxconstraints : Bool) (laplacianconstraints : Bool) (influencestrength : Float) (interpmode : Int) (distancepow : Float) (reload : Bool) : FloatArray

/-- outputs: (xform) -/        
@[apex_node "rig::PrimConstraint" has_rundata]
opaque rig_PrimConstraint (geo : Geometry) (primidx : Int) (primuv : Vector2) (x_axis_name : String) (y_axis_name : String) : Matrix4

/-- outputs: (weights) -/        
@[apex_node "rig::RBFInterpolation" has_rundata]
opaque rig_RBFInterpolation (target : FloatArray) (examples : FloatArray) (clamp : Bool) (hyperplane : Bool) (kernel : Int) (exponent : Float) (falloff : Float) (damping : Float) (robustsolver : Bool) (reload : Bool) : FloatArray

-- special function not supported yet
-- opaque rig::SampleSplineTransforms (geo : Geometry) (primnum : Int) (smooth : Bool) : Geometry×VariadicArg M...

-- special function not supported yet
-- opaque rig::SampleSplineTransforms::2.0 (geo : Geometry) (primnum : Int) (tangent : Vector3) (smooth : Bool)...

-- special function not supported yet
-- opaque rig::SampleSplineTransforms::3.0 (geo : Geometry) (primnum : Int) (lookataxis : Vector3) (lookattype ...

/-- outputs: (outgeo,xforms) -/        
@[apex_node "rig::SampleSplineTransformsToArray" has_rundata]
opaque rig_SampleSplineTransformsToArray (geo : Geometry) (primnum : Int) (numsamples : Int) (smooth : Bool) (byedge : Bool) : Geometry×Matrix4Array

/-- outputs: (outgeo,arclength,xforms) -/        
@[apex_node "rig::SampleSplineTransformsToArray::2.0" has_rundata]
opaque rig_SampleSplineTransformsToArray_2_0 (geo : Geometry) (primnum : Int) (numsamples : Int) (tangent : Vector3) (smooth : Bool) (byedge : Bool) : Geometry×Float×Matrix4Array

/-- outputs: (outgeo,arclength,xforms) -/        
@[apex_node "rig::SampleSplineTransformsToArray::3.0" has_rundata]
opaque rig_SampleSplineTransformsToArray_3_0 (geo : Geometry) (primnum : Int) (numsamples : Int) (lookataxis : Vector3) (lookattype : Int) (smooth : Bool) (stretchtype : Int) (stretch : Float) (stretchscale : Float) (squashscale : Float) (startoffset : Float) (startinterp : Int) (endoffset : Float) (endinterp : Int) (overshoot : Bool) (restlengths : FloatArray) (twists : Vector2Array) (twistinterps : IntArray) (pins : Vector2Array) (pininterps : IntArray) : Geometry×Float×Matrix4Array

-- special function not supported yet
-- opaque rig::SplineInterpolateTransforms {numcvs: Nat} (cvs : VariadicArg Matrix4 numcvs) {numtwists: Nat} (t...

-- special function not supported yet
-- opaque rig::SplineInterpolateTransforms::2.0 {numcvs: Nat} (cvs : VariadicArg Matrix4 numcvs) {numtwists: Na...

-- special function not supported yet
-- opaque rig::SplineInterpolateTransforms::3.0 {numcvs: Nat} (cvs : VariadicArg Matrix4 numcvs) (splinetype : ...

/-- outputs: (resampled) -/        
@[apex_node "rig::SplineInterpolateTransformsToArray" has_rundata]
opaque rig_SplineInterpolateTransformsToArray (cvs : Matrix4Array) (twists : FloatArray) (order : Int) (numsamples : Int) (smooth : Bool) (byedge : Bool) : Matrix4Array

/-- outputs: (outgeo,arclength,xforms) -/        
@[apex_node "rig::SplineInterpolateTransformsToArray::2.0" has_rundata]
opaque rig_SplineInterpolateTransformsToArray_2_0 (cvs : Matrix4Array) (twists : FloatArray) (order : Int) (numsamples : Int) (tangent : Vector3) (smooth : Bool) (byedge : Bool) : Geometry×Float×Matrix4Array

/-- outputs: (outgeo,arclength,xforms) -/        
@[apex_node "rig::SplineInterpolateTransformsToArray::3.0" has_rundata]
opaque rig_SplineInterpolateTransformsToArray_3_0 (cvs : Matrix4Array) (splinetype : Int) (order : Int) (numsamples : Int) (lookataxis : Vector3) (lookattype : Int) (smooth : Bool) (stretchtype : Int) (stretch : Float) (stretchscale : Float) (squashscale : Float) (startoffset : Float) (startinterp : Int) (endoffset : Float) (endinterp : Int) (overshoot : Bool) (restlengths : FloatArray) (twists : Vector2Array) (twistinterps : IntArray) (pins : Vector2Array) (pininterps : IntArray) : Geometry×Float×Matrix4Array

/-- outputs: (unitparm,unitarclen,parm,arclen) -/        
@[apex_node "rig::SplineOffset" has_rundata]
opaque rig_SplineOffset (geo : Geometry) (primnum : Int) (offset : Float) (interp : Int) : Float×Float×Float×Float

-- special function not supported yet
-- opaque rig::SplineOffsets (geo : Geometry) (primnum : Int) {numoffset: Nat} (offset : VariadicArg Float numo...

/-- outputs: (unitparm,unitarclen,parm,arclen) -/        
@[apex_node "rig::SplineOffsetsToArray" has_rundata]
opaque rig_SplineOffsetsToArray (geo : Geometry) (primnum : Int) (offset : FloatArray) (interp : IntArray) : FloatArray×FloatArray×FloatArray×FloatArray

/-- outputs: (rootout,midout,tipout) -/        
@[apex_node "rig::TwoBoneIK"]
opaque rig_TwoBoneIK (root : Matrix4) (mid : Matrix4) (tip : Matrix4) (rootdriver : Matrix4) (twist : Matrix4) (goal : Matrix4) (stretch : Bool) (blend : Float) : Matrix4×Matrix4×Matrix4

/-- outputs: (value,value,t,r,s,sh,t,r,s,sh,t,r,s,sh,t,r,s,sh,t,r,s,sh,result,result,xform,localxform,pretransform,t,r,s,sh,localxform,effectivelocal,t,r,s,sh,result,result,dist,dist,dist,xform,localxform,pretransform,result,localxform,effectivelocal,result,result,result,value,result,result,result,result,result,result,result,result,result,result,result,result,result,result,result,out,result,result,result,out,vector,vector,vector,out,m,xform,localxform,pretransform,xform,localxform,pretransform,xform,localxform,pretransform,rootout,midout,tipout,result,localxform,localxform,goal,rootdriver,root,mid,tip,falloff,twist,axis,rootdriverrest,rootout,midout,tipout,out) -/        
@[apex_node "rig::TwoBoneIKSmooth"]
opaque rig_TwoBoneIKSmooth (parm : Float) (parm : Float) (m : Matrix4) (xOrd : Int) (rOrd : Int) (p : Vector3) (pr : Vector3) (m : Matrix4) (xOrd : Int) (rOrd : Int) (p : Vector3) (pr : Vector3) (m : Matrix4) (xOrd : Int) (rOrd : Int) (p : Vector3) (pr : Vector3) (m : Matrix4) (xOrd : Int) (rOrd : Int) (p : Vector3) (pr : Vector3) (m : Matrix4) (xOrd : Int) (rOrd : Int) (p : Vector3) (pr : Vector3) (val : Float) (exponent : Float) (val : Float) (exponent : Float) (local' : Matrix4) (restlocal : Matrix4) (parent : Matrix4) (parentlocal : Matrix4) (scaleinheritance : Int) (m : Matrix4) (xOrd : Int) (rOrd : Int) (p : Vector3) (pr : Vector3) (xform : Matrix4) (parent : Matrix4) (parentlocal : Matrix4) (scaleinheritance : Int) (m : Matrix4) (xOrd : Int) (rOrd : Int) (p : Vector3) (pr : Vector3) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (a : Vector3) (b : Vector3) (a : Vector3) (b : Vector3) (a : Vector3) (b : Vector3) (local' : Matrix4) (restlocal : Matrix4) (parent : Matrix4) (parentlocal : Matrix4) (scaleinheritance : Int) (a : Vector3) {numb: Nat} (b : VariadicArg Vector3 numb) (xform : Matrix4) (parent : Matrix4) (parentlocal : Matrix4) (scaleinheritance : Int) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (a : Vector3) {numb: Nat} (b : VariadicArg Vector3 numb) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (parm : Vector3) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (a : Float) (b : Float) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (val : Float) (exponent : Float) (val : Float) (exponent : Float) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (val : Float) (exponent : Float) (val : Float) (exponent : Float) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (a : Float) (b : Float) (a : Float) (b : Float) (val : Float) (exponent : Float) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (a : Float) (b : Float) (index : Bool) (val : Float) (exponent : Float) (a : Float) {numb: Nat} (b : VariadicArg Float numb) (val : Float) (exponent : Float) (a : Float) (b : Float) (index : Bool) (x : Float) (y : Float) (z : Float) (x : Float) (y : Float) (z : Float) (x : Float) (y : Float) (z : Float) {numinput: Nat} (input : VariadicArg Vector3 numinput) (index : Int) (t : Vector3) (r : Vector3) (s : Vector3) (sh : Vector3) (p : Vector3) (pr : Vector3) (xOrd : Int) (rOrd : Int) (local' : Matrix4) (restlocal : Matrix4) (parent : Matrix4) (parentlocal : Matrix4) (scaleinheritance : Int) (local' : Matrix4) (restlocal : Matrix4) (parent : Matrix4) (parentlocal : Matrix4) (scaleinheritance : Int) (local' : Matrix4) (restlocal : Matrix4) (parent : Matrix4) (parentlocal : Matrix4) (scaleinheritance : Int) (root : Matrix4) (mid : Matrix4) (tip : Matrix4) (rootdriver : Matrix4) (twist : Matrix4) (goal : Matrix4) (stretch : Bool) (blend : Float) {numinputs: Nat} (inputs : VariadicArg Bool numinputs) (restlocal : Matrix4) (t : Vector3) (r : Vector3) (s : Vector3) (sh : Vector3) (p : Vector3) (pr : Vector3) (xord : Int) (rord : Int) (mode : Int) (restlocal : Matrix4) (t : Vector3) (r : Vector3) (s : Vector3) (sh : Vector3) (p : Vector3) (pr : Vector3) (xord : Int) (rord : Int) (mode : Int) (rootout : Matrix4) (midout : Matrix4) (tipout : Matrix4) (b2 : Float) (b2 : Float) (b2 : Float) (b2 : Float) (b2 : Float) (b2 : Float) (b2 : Float) (b2 : Float) (b2 : Float) (b2 : Float) (b2 : Float) (b2 : Float) (input2 : Vector3) (input3 : Vector3) (input4 : Vector3) (out : Vector3) (b0 : Vector3) (b0 : Vector3) (inputs0 : Bool) (inputs1 : Bool) (goal : Matrix4) (rootdriver : Matrix4) (root : Matrix4) (mid : Matrix4) (tip : Matrix4) (falloff : Float) (twist : Matrix4) (axis : Int) (rootdriverrest : Matrix4) : Float×Float×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Vector3×Float×Float×Matrix4×Matrix4×Matrix4×Vector3×Vector3×Vector3×Vector3×Matrix4×Matrix4×Vector3×Vector3×Vector3×Vector3×Float×Float×Float×Float×Float×Matrix4×Matrix4×Matrix4×Vector3×Matrix4×Matrix4×Float×Vector3×Float×Vector3×Float×Bool×Float×Float×Float×Float×Float×Float×Float×Float×Bool×Bool×Float×Float×Float×Float×Float×Float×Float×Float×Vector3×Vector3×Vector3×Vector3×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Bool×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Matrix4×Float×Matrix4×Int×Matrix4×Matrix4×Matrix4×Matrix4×Vector3

/-- outputs: (xform,primidx,primuv) -/        
@[apex_node "rig::UVConstraint" has_rundata]
opaque rig_UVConstraint (geo : Geometry) (uvname : String) (uv : Vector2) (urange : Vector2) (vrange : Vector2) (wrapu : Bool) (wrapv : Bool) : Matrix4×Int×Vector2

/-- outputs: (engine,object) -/        
@[apex_node "sim::CreateObject"]
opaque sim_CreateObject (engine : SimEngine) (name : String) (solveoncreationframe : Bool) : SimEngine×SimRootDataId

/-- outputs: (engine,rel) -/        
@[apex_node "sim::CreateRelationship"]
opaque sim_CreateRelationship (engine : SimEngine) (name : String) (group : SimRootDataIdArray) (affectorgroup : SimRootDataIdArray) : SimEngine×SimRootDataId

/-- outputs: (engine) -/        
@[apex_node "sim::CreateSubData"]
opaque sim_CreateSubData (engine : SimEngine) (rootdata : SimRootDataId) (dataname : String) (datatype : String) (avoidnamecollisions : Bool) (replaceexistingdata : Bool) (parms : Dict) : SimEngine

/-- outputs: (geo) -/        
@[apex_node "sim::EvaluateAtTime"]
opaque sim_EvaluateAtTime (engine : SimEngine) (simtime : Float) (forceresetsim : Bool) (autoresimframe : Bool) (disableresimframe : Bool) (parms : Dict) (datapath : String) : Geometry

/-- outputs: (object) -/        
@[apex_node "sim::FindObject"]
opaque sim_FindObject (engine : SimEngine) (name : String) : SimRootDataId

/-- outputs: (rel) -/        
@[apex_node "sim::FindRelationship"]
opaque sim_FindRelationship (engine : SimEngine) (name : String) : SimRootDataId

/-- outputs: (geo) -/        
@[apex_node "sim::GetGeometry"]
opaque sim_GetGeometry (engine : SimEngine) (rootdata : SimRootDataId) (dataname : String) : Geometry

/-- outputs: (engine) -/        
@[apex_node "sim::LoadSimFile"]
opaque sim_LoadSimFile (engine : SimEngine) (filename : String) : SimEngine

/-- outputs: (engine) -/        
@[apex_node "sim::RemoveObject"]
opaque sim_RemoveObject (engine : SimEngine) (object : SimRootDataId) : SimEngine

/-- outputs: (engine) -/        
@[apex_node "sim::RemoveRelationship"]
opaque sim_RemoveRelationship (engine : SimEngine) (rel : SimRootDataId) : SimEngine

/-- outputs: (engine) -/        
@[apex_node "sim::SetGeometry"]
opaque sim_SetGeometry (engine : SimEngine) (rootdata : SimRootDataId) (dataname : String) (geo : Geometry) : SimEngine

/-- outputs: (engine) -/        
@[apex_node "sim::SimEngine"]
opaque sim_SimEngine (graph : ApexGraphHandle) (timestep : Float) : SimEngine

/-- outputs: (geo,ptnum) -/        
@[apex_node "skel::AddJoint" has_rundata]
opaque skel_AddJoint (geo : Geometry) (name : String) (xform : Matrix4) (scaleinheritance : Int) (color : Vector3) : Geometry×Int

-- special function not supported yet
-- opaque skel::Blend (geo0 : Geometry) (auxgeo : GeometryArray) (group : String) (blendtopose : Int) (poseattr...

/-- outputs: (geo) -/        
@[apex_node "skel::DeleteJoint" has_rundata]
opaque skel_DeleteJoint (geo : Geometry) (ptnum : Int) (reparent : Bool) : Geometry

/-- outputs: (geo) -/        
@[apex_node "skel::DeleteJoints" has_rundata]
opaque skel_DeleteJoints (geo : Geometry) (group : String) (reparent : Bool) (invertgroup : Bool) (descendants : Bool) (excludeselected : Bool) : Geometry

/-- outputs: (pose) -/        
@[apex_node "skel::EvaluateMotionClip" has_rundata]
opaque skel_EvaluateMotionClip (motionclip : Geometry) (time : Float) : Geometry

/-- outputs: (poses) -/        
@[apex_node "skel::EvaluateMotionClipArray" has_rundata]
opaque skel_EvaluateMotionClipArray (motionclips : GeometryArray) (time : Float) : GeometryArray

/-- outputs: (point,success) -/        
@[apex_node "skel::FindFirstJoint" has_rundata]
opaque skel_FindFirstJoint (skel : Geometry) (pattern : String) : Int×Bool

/-- outputs: (ptnum) -/        
@[apex_node "skel::FindJoint"]
opaque skel_FindJoint (geo : Geometry) (name : String) : Int

/-- outputs: (points) -/        
@[apex_node "skel::FindJoints" has_rundata]
opaque skel_FindJoints (skel : Geometry) (pattern : String) : IntArray

/-- outputs: (roots) -/        
@[apex_node "skel::FindRoots"]
opaque skel_FindRoots (geo : Geometry) : IntArray

/-- outputs: (parents) -/        
@[apex_node "skel::GetAncestors" has_rundata]
opaque skel_GetAncestors (geo : Geometry) (joint : Int) : IntArray

/-- outputs: (children) -/        
@[apex_node "skel::GetChildren"]
opaque skel_GetChildren (geo : Geometry) (joint : Int) : IntArray

/-- outputs: (children) -/        
@[apex_node "skel::GetDescendants" has_rundata]
opaque skel_GetDescendants (geo : Geometry) (joint : Int) : IntArray

/-- outputs: (parent) -/        
@[apex_node "skel::GetParent"]
opaque skel_GetParent (geo : Geometry) (joint : Int) : Int

/-- outputs: (local') -/        
@[apex_node "skel::GetPointLocalTransform" has_rundata]
opaque skel_GetPointLocalTransform (geo : Geometry) (name : String) : Matrix4

-- special function not supported yet
-- opaque skel::GetPointLocalTransforms (geo : Geometry) : VariadicArg Matrix4...

/-- outputs: (xform) -/        
@[apex_node "skel::GetPointTransform" has_rundata]
opaque skel_GetPointTransform (geo : Geometry) (name : String) : Matrix4

-- special function not supported yet
-- opaque skel::GetPointTransforms (geo : Geometry) : VariadicArg Matrix4...

/-- outputs: (name,xform,local',scaleinheritance,color,tags,properties) -/        
@[apex_node "skel::JointData"]
opaque skel_JointData (geo : Geometry) (ptnum : Int) : String×Matrix4×Matrix4×Int×Vector3×StringArray×Dict

/-- outputs: (geo) -/        
@[apex_node "skel::SetParent" has_rundata]
opaque skel_SetParent (geo : Geometry) (joint : Int) (parent : Int) (unparentoncycle : Bool) : Geometry

/-- outputs: (geo) -/        
@[apex_node "skel::SetParents" has_rundata]
opaque skel_SetParents (geo : Geometry) (joints : IntArray) (parents : IntArray) (unparentoncycle : Bool) : Geometry

/-- outputs: (geo) -/        
@[apex_node "skel::SetPointTransforms" has_rundata]
opaque skel_SetPointTransforms (geo : Geometry) {numtransforms: Nat} (transforms : VariadicArg Matrix4 numtransforms) : Geometry

/-- outputs: (geo) -/        
@[apex_node "skel::SetPointTransformsFromAgent" has_rundata]
opaque skel_SetPointTransformsFromAgent (geo : Geometry) (agentgeo : Geometry) (agentprimnum : Int) : Geometry

-- special function not supported yet
-- opaque skel::SetTransformObjectRest (graph : ApexGraphHandle) (nodeid : ApexNodeID) (a : String) (b : String...

/-- outputs: (geo) -/        
@[apex_node "skel::SmoothMotion" has_rundata]
opaque skel_SmoothMotion (motionclip : Geometry) (pattern : String) (cutoff : Float) (order : Int) (components : Int) (xord : Int) (rord : Int) (window : Int) (rate : Float) (time : Float) (blend : Float) : Geometry

/-- outputs: (geo) -/        
@[apex_node "skel::SmoothMotionArray" has_rundata]
opaque skel_SmoothMotionArray (skels : GeometryArray) (path : String) (pattern : String) (cutoff : Float) (order : Int) (components : Int) (xord : Int) (rord : Int) (window : Int) (rate : Float) (time : Float) (blend : Float) : Geometry

/-- outputs: (geo) -/        
@[apex_node "skel::SmoothMotionClip" has_rundata]
opaque skel_SmoothMotionClip (motionclip : Geometry) (pattern : String) (cutoff : Float) (order : Int) (components : Int) (xord : Int) (rord : Int) (window : Int) (rate : Float) (blend : AnimChannel) : Geometry

/-- outputs: (geo,success) -/        
@[apex_node "skel::Sort"]
opaque skel_Sort (geo : Geometry) : Geometry×Bool

/-- outputs: (pts,parents,traverseresult) -/        
@[apex_node "skel::Traverse"]
opaque skel_Traverse (geo : Geometry) (roots : IntArray) : IntArray×IntArray×Int

/-- outputs: (geo) -/        
@[apex_node "skel::UpdateJoint"]
opaque skel_UpdateJoint (geo : Geometry) (ptnum : Int) (name : String) (xform : Matrix4) (scaleinheritance : Int) (color : Vector3) : Geometry

-- special function not supported yet
-- opaque skel::UpdateJointTags (parm : StringArray) (value : Geometry) (value : Int) (value : StringArray) (va...

-- special function not supported yet
-- opaque skel::UpdateTransformObject (input : Bool) (graph : ApexGraphHandle) (nodeid : ApexNodeID) (geo : Geo...

-- special function not supported yet
-- opaque skel::UpdateTransformObjects (geo0 : Geometry) (mode : Int) (constrainedgroup : String) (outputlocalt...

/-- outputs: (geo0) -/        
@[apex_node "sop::add" has_rundata]
opaque sop_add (geo0 : Geometry) (keep : Int) (points : DictArray) (remove : Int) (prims : DictArray) (switcher : Int) (group : String) (add : Int) (inc : Int) (attrname : String) (closedall : Int) (addparticlesystem : Int) (particlegroup : String) (appendunusedtoparticlesystem : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::agentlayer::2.0" has_rundata]
opaque sop_agentlayer_2_0 (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (group : String) (addshapes : Int) (shapenameattrib : String) (keepexternalref : Int) (boundsscalemode : Int) (boundsscaleattrib : String) (shapedeformermode : Int) (shapedeformerattrib : String) (transformnamemode : Int) (transformnameattrib : String) (setcurrentlayers : Int) (currentlayers : String) (setcollisionlayers : Int) (collisionlayers : String) (uselayernameattrib : Int) (layernameattrib : String) (numlayers : DictArray) (boundsscale : Vector3) (shapedeformer : String) (transformname : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::agentunpack" has_rundata]
opaque sop_agentunpack (geo0 : Geometry) (group : String) (grouptype : Int) (output : Int) (uniqueagentdefinitions : Int) (applyagentxform : Int) (unpackrestshapesfrom : Int) (layerfilter : String) (shapefilter : String) (limititerations : Int) (iterations : Int) (addshapedeformerattrib : Int) (shapedeformerattrib : String) (addxformnameattrib : Int) (xformnameattrib : String) (addlayernameattrib : Int) (layernameattrib : String) (clipnames : String) (transferattributes : String) (transfergroups : String) (numjointfilters : DictArray) (skeletoncolor : Vector3) (applyjointxforms : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::apex::invokegraph" has_rundata]
opaque sop_apex_invokegraph (geo0 : Geometry) (auxgeo : GeometryArray) (inputbindings : DictArray) (outputtypeinfo : Int) (dictbindings : DictArray) (bindoutputgeo : Int) (apexgeooutput : String) (outputmode : Int) (debugevaluation : Int) (forcereloadgraph : Int) (partialeval : Int) (errorhandlingmode : Int) (asynccook : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::apex::sceneinvoke::2.0" has_rundata]
opaque sop_apex_sceneinvoke_2_0 (geo0 : Geometry) (outputmode : Int) (addpathattrib : Int) (pathattrib : String) (enableanimation : Int) (animationclip : String) (frame : Float) (outputcharactershapes : Int) (characterpattern : String) (shapepattern : String) (includeinvisiblechars : Int) (extraoutputs : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::attribcast" has_rundata]
opaque sop_attribcast (geo0 : Geometry) (numcasts : DictArray) (preferredprecision : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::attribcombine" has_rundata]
opaque sop_attribcombine (geo0 : Geometry) (geo1 : Geometry) (group : String) (grouptype : Int) (dstattrib : String) (class' : Int) (numcombines : DictArray) (postscale : Float) (dothreshold : Int) (threshold : Float) (doclampmin : Int) (clampmin : Float) (doclampmax : Int) (clampmax : Float) (attribmatch : String) (createmissing : Int) (forcescalar : Int) (deletesource : Int) (errormissing : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::attribcomposite" has_rundata]
opaque sop_attribcomposite (auxgeo : GeometryArray) (selectionidx : Int) (detailattribs : Int) (detailattriblist : String) (primitiveattribs : Int) (primattriblist : String) (pointattribs : Int) (pointattriblist : String) (vertexattribs : Int) (vertexattriblist : String) (matchpattrib : Int) (nblends : DictArray) (alphaattrib : String) (compop : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::attribcopy" has_rundata]
opaque sop_attribcopy (geo0 : Geometry) (geo1 : Geometry) (srcgroup : String) (srcgrouptype : Int) (destgroup : String) (destgrouptype : Int) (matchbyattribute : Int) (matchbyattributemethod : Int) (attributetomatch : String) (attrib : Int) (attribname : String) (copyp : Int) (usenewname : Int) (newname : String) (class' : Int) (copyvariable : Int) (copydata : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::attribcreate::2.0" has_rundata]
opaque sop_attribcreate_2_0 (geo0 : Geometry) (group : String) (grouptype : Int) (encodenames : Int) (numattr : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::attribfill" has_rundata]
opaque sop_attribfill (geo0 : Geometry) (mode : Int) (attrib : String) (source : String) (weights : String) (speed : String) (boundary : String) (mindist : Float) (minspeed : Float) (timestep : Float) (unreachableval : Float) (extrapolationtype : Int) (tangentialonly : Int) (globalrotation : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::attribfromparm" has_rundata]
opaque sop_attribfromparm (geo0 : Geometry) (method : Int) (nodepath : String) (category : String) (name : String) (parmfilter : String) (flattenmultiparm : Int) (flattenramp : Int) (evaluateparms : Int) (channelprims : Int) (packlockgeometry : Int) (explicitwires : Int) (explicitports : Int) (updateonmove : Int) (updateonrename : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::attribfromvolume" has_rundata]
opaque sop_attribfromvolume (geo0 : Geometry) (geo1 : Geometry) (group : String) (field : String) (name : String) (createvarmap : Int) (varname : String) (type : Int) (size : Int) (default : Vector4) (rangein : Vector2) (monoenable : Int) (monorampmode : Int) (monopreset : Int) (monoramp : FloatRamp) (vectorenable : Int) (vectorrampmode : Int) (vectorpreset : Int) (vectorramp : ColorRamp) (rangeout : Vector2) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::attribinterpolate" has_rundata]
opaque sop_attribinterpolate (geo0 : Geometry) (geo1 : Geometry) (totype : Int) (interpby : Int) (numberattrib : String) (weightsattrib : String) (pointattribs : String) (vertattribs : String) (primattribs : String) (detailattribs : String) (matchgroups : Int) (computeweights : Int) (computenumberstype : Int) (computednumbersattrib : String) (computedweightsattrib : String) (prescale : Float) (normalize : Int) (threshold : Float) (blend : Float) (group : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::attribmirror" has_rundata]
opaque sop_attribmirror (geo0 : Geometry) (group : String) (grouptype : Int) (usegroupas : Int) (attrib : Int) (attribname : String) (mirroringmethod : Int) (usetolerance : Int) (tolerance : Float) (origin : Vector3) (dist : Float) (dir : Vector3) (mirroringedge : String) (sourcehint : String) (useconnectivityattrib : Int) (connectivityattrib : String) (elementmapname : String) (destgroupname : String) (outputelemmap : Int) (mirrorelemattrib : String) (outputsrcgroup : Int) (mirrorsrcgroup : String) (outputdestgroup : Int) (mirrordestgroup : String) (attribmirror : Int) (uvorig : Vector2) (uvangle : Float) (stringreplace : Int) (search : String) (replace : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::attribpromote" has_rundata]
opaque sop_attribpromote (geo0 : Geometry) (inname : String) (inclass : Int) (outclass : Int) (usepieceattrib : Int) (pieceattrib : String) (method : Int) (useoutname : Int) (outname : String) (deletein : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::attribreorient" has_rundata]
opaque sop_attribreorient (geo0 : Geometry) (geo1 : Geometry) (group : String) (vattribs : String) (qattribs : String) (usenormalattrib : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::attribsort" has_rundata]
opaque sop_attribsort (geo0 : Geometry) (class' : Int) (attrib : String) (component : Int) (order : Int) (useindices : Int) (indices : String) (useopencl : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::attribstringedit" has_rundata]
opaque sop_attribstringedit (geo0 : Geometry) (detailattribs : Int) (detailattriblist : String) (primitiveattribs : Int) (primattriblist : String) (pointattribs : Int) (pointattriblist : String) (vertexattribs : Int) (vertexattriblist : String) (filters : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::attribswap" has_rundata]
opaque sop_attribswap (geo0 : Geometry) (numswaps : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::attribtransfer" has_rundata]
opaque sop_attribtransfer (geo0 : Geometry) (geo1 : Geometry) (srcgroups : String) (srcgrouptype : Int) (dstgroups : String) (dstgrouptype : Int) (detailattribs : Int) (detailattriblist : String) (primitiveattribs : Int) (primattriblist : String) (pointattribs : Int) (pointattriblist : String) (vertexattribs : Int) (vertexattriblist : String) (copyvariable : Int) (matchpattrib : Int) (kernel : String) (kernelradius : Float) (maxsamplecount : Int) (threshold : Int) (thresholddist : Float) (blendwidth : Float) (uniformbias : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::attribute" has_rundata]
opaque sop_attribute (geo0 : Geometry) (ptrenames : DictArray) (ptdel : String) (ptkeep : String) (vtxrenames : DictArray) (vtxdel : String) (vtxkeep : String) (primrenames : DictArray) (primdel : String) (primkeep : String) (detailrenames : DictArray) (dtldel : String) (dtlkeep : String) (rmanconversions : DictArray) (ridefault : Int) (updatevar : Int) (overwrite : Int) (encodenames : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::attribvop" has_rundata]
opaque sop_attribvop (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (geo3 : Geometry) (vex_cwdpath : String) (vex_outputmask : String) (bindings : DictArray) (groupbindings : DictArray) (autobind : Int) (groupautobind : Int) (bindclass : Int) (bindgroup : String) (bindgrouptype : Int) (vex_multithread : Int) (vex_updatenmls : Int) (vex_numcount : Int) (vex_threadjobsize : Int) (vex_matchattrib : String) (vex_selectiongroup : String) (vex_inplace : Int) (vex_precision : String) (vexsrc : Int) (script : String) (vexsnippet : String) (vex_strict : Int) (vex_exportlist : String) (vex_strictvariables : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::basis" has_rundata]
opaque sop_basis (geo0 : Geometry) (group : String) (ubasis : Int) (uparmtype : Int) (uknots : String) (urange : Vector2) (ubias : Float) (uconcat : Int) (udoorigin : Int) (uorigin : Float) (udolength : Int) (ulength : Float) (udoscale : Int) (uscale : Float) (uraise : Int) (orderu : Int) (vbasis : Int) (vparmtype : Int) (vknots : String) (vrange : Vector2) (vbias : Float) (vconcat : Int) (vdoorigin : Int) (vorigin : Float) (vdolength : Int) (vlength : Float) (vdoscale : Int) (vscale : Float) (vraise : Int) (orderv : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::blast" has_rundata]
opaque sop_blast (geo0 : Geometry) (group : String) (grouptype : Int) (computenorms : Int) (negate : Int) (fillhole : Int) (removegrp : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::blendshapes" has_rundata]
opaque sop_blendshapes (geo0 : Geometry) (auxgeo : GeometryArray) (group : String) (selectionidx : Int) (diff : Int) (dopos : Int) (doclr : Int) (donml : Int) (douvw : Int) (dovoxel : Int) (doslerp : Int) (ptidattr : String) (primidattr : String) (morph : Int) (nblends : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::blendshapes::2.0" has_rundata]
opaque sop_blendshapes_2_0 (geo0 : Geometry) (auxgeo : GeometryArray) (group : String) (grouptype : Int) (selectionidx : Int) (diff : Int) (cachedeltas : Int) (pack : Int) (packfirst : Int) (weightperpack : Int) (attribs : String) (ptidattr : String) (primidattr : String) (voxelblend : Int) (doslerp : Int) (maskmode : Int) (maskattrib : String) (maskattribmode : Int) (nblends : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::bonecapturebiharmonic" has_rundata]
opaque sop_bonecapturebiharmonic (geo0 : Geometry) (geo1 : Geometry) (group : String) (maxiter : Int) (difftol : Float) (destroyweights : Int) (blendfactor : Float) (color : Int) (zeroweightcolor : Vector3) (outputcapturetets : Int) (verbose : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::bonedeform" has_rundata]
opaque sop_bonedeform (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (group : String) (skelrootpath : String) (method : Int) (dqblendattrib : String) (bonetransformpath : String) (bonetransformtargetpath : String) (bonetransformregionpath : String) (otherattribs : String) (donormal : Int) (deletecaptureattrib : Int) (deletepointtcolors : Int) (useopencl : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::boolean::2.0" has_rundata]
opaque sop_boolean_2_0 (geo0 : Geometry) (geo1 : Geometry) (agroup : String) (asurface : Int) (resolvea : Int) (bgroup : String) (bsurface : Int) (resolveb : Int) (booleanop : Int) (subtractchoices : Int) (shatterchoices : Int) (opencurvesonly : Int) (generateaaseams : Int) (generatebbseams : Int) (generateabseams : Int) (windingop : Int) (mergenbrs : Int) (detriangulate : Int) (removeinlinepoints : Int) (uniqueseams : Int) (correctnormals : Int) (useaxapolys : Int) (axapolys : String) (useaxbpolys : Int) (axbpolys : String) (useaxalist : Int) (axalist : String) (useaxblist : Int) (axblist : String) (collapsetinyedges : Int) (lengththreshold : Float) (useapolys : Int) (apolys : String) (useainsideb : Int) (ainsideb : String) (useaoutsideb : Int) (aoutsideb : String) (usebpolys : Int) (bpolys : String) (usebinsidea : Int) (binsidea : String) (useboutsidea : Int) (boutsidea : String) (useaboverlap : Int) (aboverlap : String) (useaonlypieces : Int) (aonlypieces : String) (usebonlypieces : Int) (bonlypieces : String) (useabpieces : Int) (abpieces : String) (usereversedpolys : Int) (reversedpolys : String) (useaaseamedges : Int) (aaseamedges : String) (usebbseamedges : Int) (bbseamedges : String) (useabseamedges : Int) (abseamedges : String) (adepth : Vector2) (bdepth : Vector2) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::bound" has_rundata]
opaque sop_bound (geo0 : Geometry) (group : String) (grouptype : Int) (keepOriginal : Int) (createboundinggeo : Int) (createempty : Int) (boundtype : Int) (orientedbbox : Int) (refinementiterations : Int) (dodivs : Int) (divs : Vector3) (rebar : Int) (minsize : Vector3) (orient : Int) (accurate : Int) (minradius : Float) (orientedbrect : Int) (origin : Vector3) (dist : Float) (dir : Vector3) (minpad : Vector3) (maxpad : Vector3) (addboundsgroup : Int) (boundsgroup : String) (addxformattrib : Int) (xformattrib : String) (addradiiattrib : Int) (radiiattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::box" has_rundata]
opaque sop_box (geo0 : Geometry) (type : Int) (surftype : Int) (consolidatepts : Int) (size : Vector3) (t : Vector3) (r : Vector3) (scale : Float) (divrate : Vector3) (orderrate : Vector3) (dodivs : Int) (divs : Vector3) (rebar : Int) (orientedbbox : Int) (vertexnormals : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::cache" has_rundata]
opaque sop_cache (geo0 : Geometry) (geo1 : Geometry) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::cacheif" has_rundata]
opaque sop_cacheif (geo0 : Geometry) (auxgeo : GeometryArray) (checkupstream : Int) (outputattrib : Int) (checkattrib : Int) (attribname : String) (numinputs : DictArray) (checkgroups : Int) (numpatterns : DictArray) (numexpressions : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::cap" has_rundata]
opaque sop_cap (geo0 : Geometry) (group : String) (firstu : Int) (divsu1 : Int) (scaleu1 : Float) (lastu : Int) (divsu2 : Int) (scaleu2 : Float) (firstv : Int) (divsv1 : Int) (scalev1 : Float) (lastv : Int) (divsv2 : Int) (scalev2 : Float) (pshapeu : Int) (pshapev : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::captureattribpack" has_rundata]
opaque sop_captureattribpack (geo0 : Geometry) (class' : Int) (attrib : String) (prefix' : String) (secondaryprefix : String) (packproperties : Int) (packdata : Int) (deletecapture : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::captureattribunpack" has_rundata]
opaque sop_captureattribunpack (geo0 : Geometry) (class' : Int) (attrib : String) (prefix' : String) (secondaryprefix : String) (unpackproperties : Int) (unpackdata : Int) (deletecapture : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::capturepaintcore" has_rundata]
opaque sop_capturepaintcore (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (group : String) (grouptype : Int) (cregion : String) (unlockedregions : String) (deform : Int) (usedisplay : Int) (normalization : Int) (applytogroup : Int) (applyiterations : Int) (clearweights : Int) (geo : DataItem) (dovis : Int) (vistype : Int) (vismode : Int) (colorramp : ColorRamp) (brushop : Int) (brushmode : Int) (value : Float) (min : Float) (max : Float) (radius : Float) (opacity : Float) (falloff : Float) (falloffcurve : FloatRamp) (brushspacing : Float) (usepressure : Int) (strokesegment : DataItem) (savestrokes : Int) (strokefilepath : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::carve" has_rundata]
opaque sop_carve (geo0 : Geometry) (group : String) (arclen : Int) (firstu : Int) (domainu1 : Float) (usedomainu1attrib : Int) (domainu1attrib : String) (secondu : Int) (domainu2 : Float) (usedomainu2attrib : Int) (domainu2attrib : String) (firstv : Int) (domainv1 : Float) (usedomainv1attrib : Int) (domainv1attrib : String) (secondv : Int) (domainv2 : Float) (usedomainv2attrib : Int) (domainv2attrib : String) (onlybreakpoints : Int) (divsu : Int) (divsv : Int) (allubreakpoints : Int) (allvbreakpoints : Int) (stdswitcher : Int) (keepin : Int) (keepout : Int) (extractop : Int) (keepOriginal : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::circle" has_rundata]
opaque sop_circle (type : Int) (orient : Int) (reverse : Int) (rad : Vector2) (t : Vector3) (r : Vector3) (scale : Float) (order : Int) (imperfect : Int) (divs : Int) (arc : Int) (angle : Vector2) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::circlefromedges" has_rundata]
opaque sop_circlefromedges (geo0 : Geometry) (group : String) (grouptype : Int) (getboundary : Int) (explicitradius : Int) (radius : Float) (scale : Float) (outputedgegroup : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::clip" has_rundata]
opaque sop_clip (geo0 : Geometry) (group : String) (clipop : Int) (origin : Vector3) (dist : Float) (dir : Vector3) (newg : Int) (above : String) (below : String) (clippts : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::clip::2.0" has_rundata]
opaque sop_clip_2_0 (geo0 : Geometry) (group : String) (attrib : String) (clipop : Int) (dirtype : Int) (t : Vector3) (r : Vector3) (origin : Vector3) (dir : Vector3) (dist : Float) (snaptol : Float) (dofill : Int) (dosplit : Int) (replace : Int) (useclipedges : Int) (clipedges : String) (useclipprims : Int) (clipprims : String) (useaboveprims : Int) (aboveprims : String) (usebelowprims : Int) (belowprims : String) (usefillpolygons : Int) (fillpolygons : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::cluster" has_rundata]
opaque sop_cluster (geo0 : Geometry) (geo1 : Geometry) (num_clusters : Int) (cluster_attrib : String) (output_center : Int) (num_controls : DictArray) (iterations : Int) (random_seed : Int) (threshold_attrib : String) (threshold_weight : Float) (initial_threshold : Float) (final_threshold : Float) (use_linf : Int) (kmeanpp : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::clustermesh" has_rundata]
opaque sop_clustermesh (geo0 : Geometry) (clustersize : Int) (maskattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::collisiondetectcl" has_rundata]
opaque sop_collisiondetectcl (geo0 : Geometry) (group : String) (tetgroup : String) (radiusscale : Float) (dotets : Int) (coldetect : Int) (dopttri : Int) (doedgeedge : Int) (ccdaccuracy : Float) (ignorepiece : Int) (hittypesattr : String) (hitptsattr : String) (hituvsattr : String) (hitnmlsattr : String) (hitidxattr : String) (hitvtxattr : String) (resetkey : Float) (leaveongpu : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::connectivity" has_rundata]
opaque sop_connectivity (geo0 : Geometry) (connecttype : Int) (primincgroup : String) (pointincgroup : String) (attribname : String) (attribtype : Int) (prefix' : String) (createvarmap : Int) (varname : String) (seamgroup : String) (byuv : Int) (uvattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::control" has_rundata]
opaque sop_control (geo0 : Geometry) (usecolor : Int) (color : Vector3) (size : Vector3) (t : Vector3) (r : Vector3) (scale : Float) (displayicon : Int) (controltype : Int) (orientation : Int) (shadedmode : Int) (packed : Int) (numsnappoints : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::convert" has_rundata]
opaque sop_convert (geo0 : Geometry) (group : String) (fromtype : Int) (totype : Int) (surftype : Int) (stdswitcher : Int) (orderu : Int) (orderv : Int) (pastecoord : Int) (pasteattrib : Int) (new : Int) (interphull : Int) (offset : Float) (lodu : Float) (lodv : Float) (lodtrim : Float) (divu : Int) (divv : Int) (divtrim : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::convertvdb" has_rundata]
opaque sop_convertvdb (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (group : String) (conversion : Int) (vdbclass : Int) (vdbtype : String) (vdbprecision : String) (splitdisjointvolumes : Int) (isovalue : Float) (fogisovalue : Float) (adaptivity : Float) (computenormals : Int) (internaladaptivity : Float) (transferattributes : Int) (sharpenfeatures : Int) (edgetolerance : Float) (surfacegroup : String) (interiorgroup : String) (seamlinegroup : String) (seampoints : String) (surfacemask : Int) (surfacemaskname : String) (surfacemaskoffset : Float) (invertmask : Int) (adaptivityfield : Int) (adaptivityfieldname : String) (prune : Int) (tolerance : Float) (flood : Int) (activateinsidesdf : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::convertvdbpoints" has_rundata]
opaque sop_convertvdbpoints (geo0 : Geometry) (geo1 : Geometry) (conversion : Int) (group : String) (vdbpointsgroup : String) (name : String) (outputname : String) (countname : String) (maskname : String) (keep : Int) (transform : Int) (voxelsize : Float) (pointspervoxel : Int) (refvdb : String) (poscompression : Int) (mode : Int) (attrList : DictArray) (normalcompression : Int) (colorcompression : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::convertvolume" has_rundata]
opaque sop_convertvolume (geo0 : Geometry) (group : String) (iso : Float) (invert : Int) (lod : Float) (computenml : Int) (buildpolysoup : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::convexdecomposition" has_rundata]
opaque sop_convexdecomposition (geo0 : Geometry) (group : String) (usepieceattrib : Int) (pieceattrib : String) (maxconcavity : Float) (treatassolid : Int) (mergesegments : Int) (geometryoutput : Int) (outputsegmentattrib : Int) (segmentattrib : String) (outputinteriorgroup : Int) (interiorgroupname : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::cop2net" has_rundata]
opaque sop_cop2net (usecoppath : Int) (coppath : String) (planemask : String) (method : Int) (plane : Int) (sampling : Int) (t : Vector3) (scale : Float) (visualize : Int) (visrange : Vector2) (frame : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::copytopoints" has_rundata]
opaque sop_copytopoints (geo0 : Geometry) (geo1 : Geometry) (sourcegroup : String) (targetgroup : String) (showguide : Int) (pack : Int) (pivot : Int) (viewportlod : Int) (transform : Int) (doattr : Int) (setpt : String) (mulpt : String) (addpt : String) (subpt : String) (setprim : String) (mulprim : String) (addprim : String) (subprim : String) (setvtx : String) (mulvtx : String) (addvtx : String) (subvtx : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::copytopoints::2.0" has_rundata]
opaque sop_copytopoints_2_0 (geo0 : Geometry) (geo1 : Geometry) (sourcegroup : String) (sourcegrouptype : Int) (targetgroup : String) (useidattrib : Int) (idattrib : String) (pack : Int) (pivot : Int) (viewportlod : Int) (transform : Int) (useimplicitn : Int) (targetattribs : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::copyxform" has_rundata]
opaque sop_copyxform (geo0 : Geometry) (sourcegroup : String) (sourcegrouptype : Int) (ncy : Int) (pack : Int) (pivot : Int) (viewportlod : Int) (xOrd : Int) (rOrd : Int) (t : Vector3) (r : Vector3) (s : Vector3) (shear : Vector3) (scale : Float) (p : Vector3) (pr : Vector3) (newgroups : Int) (newgroupprefix : String) (docopyattrib : Int) (copyattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::crease" has_rundata]
opaque sop_crease (geo0 : Geometry) (group : String) (op : Int) (crease : Float) (addcolor : Int) (creaseattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::crowdmotionpathavoidcore" has_rundata]
opaque sop_crowdmotionpathavoidcore (geo0 : Geometry) (geo1 : Geometry) (group : String) (enableavoidance : Int) (maxcollisiontime : Float) (enableneighbors : Int) (neighbordistance : Float) (maxneighbors : Int) (enableobstacles : Int) (obstacledistance : Float) (obstaclepadding : Float) (horizontalfov : Float) (verticalfov : Float) (fovsamples : Int) (fovseed : Float) (steeringmode : Int) (turnspeedthreshold : Float) (usemaxinitialrotation : Int) (maxinitialrotation : Float) (maxturnrate : Float) (constrainturnaccel : Int) (turnstiffness : Float) (turndamping : Float) (goalpos : Int) (goalposweight : Float) (distancevariance : Float) (addcollisionptgroup : Int) (collisionptgroup : String) (timestep : Float) (usestarttime : Int) (starttime : Float) (useendtime : Int) (endtime : Float) (refup : Vector3) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::crowdmotionpatheditcore" has_rundata]
opaque sop_crowdmotionpatheditcore (geo0 : Geometry) (restattrib : String) (pingroup : String) (pinweightattrib : String) (pinscaleweightattrib : String) (scaleadjustment : Int) (scaleadjustmentweight : Float) (addeditedpathgroup : Int) (editedpathgroup : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::crowdmotionpathevaluatecore" has_rundata]
opaque sop_crowdmotionpathevaluatecore (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (time : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::curve" has_rundata]
opaque sop_curve (geo0 : Geometry) (type : Int) (method : Int) (coords : String) (close : Int) (reverse : Int) (order : Int) (param : Int) (tolerance : Float) (smooth : Float) (csharp : Int) (keepgeo : Int) (normalize : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::curvesect" has_rundata]
opaque sop_curvesect (geo0 : Geometry) (geo1 : Geometry) (leftgroup : String) (rightgroup : String) (xsect : Int) (tolerance : Float) (left : Int) (right : Int) (stdswitcher : Int) (affect : Int) (extractPt : Int) (keepOriginal : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::delete" has_rundata]
opaque sop_delete (geo0 : Geometry) (group : String) (negate : Int) (entity : Int) (affectnumber : Int) (groupop : Int) (filter : Int) (pattern : String) (range : Vector2) (select : Vector2) (affectvolume : Int) (boundtype : Int) (size : Vector3) (t : Vector3) (affectnormal : Int) (camerapath : String) (affectdegenerate : Int) (degenerate : Int) (zaf : Int) (doopen : Int) (tol : Float) (userandom : Int) (globalseed : Float) (useseedattrib : Int) (seedattrib : String) (percent : Float) (removegrp : Int) (keeppoints : Int) (geotype : Int) (dir : Vector3) (angle : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::deltamush" has_rundata]
opaque sop_deltamush (geo0 : Geometry) (geo1 : Geometry) (group : String) (iterations : Int) (stepsize : Float) (method : Int) (symmetrize : Int) (symmetryaxis : Int) (pinborder : Int) (updateaffectednmls : Int) (clampstepsize : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::detangle" has_rundata]
opaque sop_detangle (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (prevpos : String) (disableselfattr : String) (disableexternalattr : String) (weldattr : String) (thickness : Float) (doself : Int) (dotet : Int) (pairedautodisable : Int) (updatedisable : Int) (resetdetangled : Int) (domark : Int) (markattr : String) (doresolve : Int) (maxweight : Float) (capdisplace : Int) (maxdisplace : Float) (resolveall : Int) (resolveallmax : Int) (resolvealledges : Int) (layerattr : String) (layershock : Float) (externalfriction : Float) (selffriction : Float) (static_threshold : Float) (kinetic_scale : Float) (constantcollisiontopology : Int) (sharedcache : String) (resetkey : Float) (updateoverlap : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::deterministicp2gcl" has_rundata]
opaque sop_deterministicp2gcl (geo0 : Geometry) (geo1 : Geometry) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::dissolve::2.0" has_rundata]
opaque sop_dissolve_2_0 (geo0 : Geometry) (group : String) (invertsel : Int) (compnorms : Int) (reminlinepts : Int) (coltol : Float) (remunusedpts : Int) (bridge : Int) (deldegeneratebridges : Int) (boundarycurves : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::divide" has_rundata]
opaque sop_divide (geo0 : Geometry) (geo1 : Geometry) (group : String) (convex : Int) (usemaxsides : Int) (numsides : Int) (planar : Int) (plantol : Float) (noslivers : Int) (avoidsmallangles : Int) (smooth : Int) (weight : Vector2) (divs : Int) (brick : Int) (size : Vector3) (offset : Vector3) (angle : Vector3) (fixsharededges : Int) (removesh : Int) (dual : Int) (dualattribstoswap : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::dopimport::2.0" has_rundata]
opaque sop_dopimport_2_0 (doppath : String) (objpattern : String) (donotsim : Int) (importstyle : Int) (pointsquicksetup : Int) (geodatapath : String) (pack : Int) (pivot : Int) (viewportlod : Int) (adddopobjectpath : Int) (adddopobjectname : Int) (dopobjectnameattrib : String) (adddopdatapath : Int) (dopdatapathattrib : String) (transferattributes : String) (transfergroups : String) (doposxform : Int) (dogeoxform : Int) (keepworldspacepos : Int) (pointvels : Int) (integrateovertime : Float) (addtoexistingvel : Int) (relpattern : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::edgecollapse" has_rundata]
opaque sop_edgecollapse (geo0 : Geometry) (group : String) (removedegen : Int) (updatenmls : Int) (connectivityattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::edgecusp" has_rundata]
opaque sop_edgecusp (geo0 : Geometry) (group : String) (updatenorms : Int) (cutboundaries : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::edgedivide" has_rundata]
opaque sop_edgedivide (geo0 : Geometry) (group : String) (numdivs : Int) (connectpoints : Int) (closepath : Int) (tolerance : Float) (applytoall : Int) (sharedpoints : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::edgeequalize" has_rundata]
opaque sop_edgeequalize (geo0 : Geometry) (group : String) (grouptype : Int) (method : Int) (outputedgegroup : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::edgeflip" has_rundata]
opaque sop_edgeflip (geo0 : Geometry) (group : String) (cycles : Int) (cycleattribs : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::edgestraighten" has_rundata]
opaque sop_edgestraighten (geo0 : Geometry) (group : String) (grouptype : Int) (outputedgegroup : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::edgetransport" has_rundata]
opaque sop_edgetransport (geo0 : Geometry) (method : Int) (ptgroup : String) (primgroup : String) (attribute' : String) (curvedir : Int) (parentattribute : String) (roottype : Int) (rootgroup : String) (operation : Int) (donormalization : Int) (normalization : Int) (rootvalue : Int) (ignoreattribute : Int) (scalebyedge : Int) (rotatebyedge : Int) (splitmethod : Int) (mergemethod : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::ends" has_rundata]
opaque sop_ends (geo0 : Geometry) (group : String) (closeu : Int) (closev : Int) (clampu : Int) (clampv : Int) (pshapeu : Int) (pshapev : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::enumerate" has_rundata]
opaque sop_enumerate (geo0 : Geometry) (group : String) (grouptype : Int) (usepieceattrib : Int) (pieceattrib : String) (piecemode : Int) (attribname : String) (attribtype : Int) (prefix' : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::error" has_rundata]
opaque sop_error (geo0 : Geometry) (numerror : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::extractcentroid" has_rundata]
opaque sop_extractcentroid (geo0 : Geometry) (partitiontype : Int) (pieceattrib : String) (class' : Int) (method : Int) (output : Int) (centroidattrib : String) (transferattributes : String) (transfergroups : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::extractcontours" has_rundata]
opaque sop_extractcontours (geo0 : Geometry) (group : String) (campath : String) (frustumculling : Int) (normaltolerance : Float) (mode : Int) (outputedgegroup : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::extracttransform" has_rundata]
opaque sop_extracttransform (geo0 : Geometry) (geo1 : Geometry) (usepieceattrib : Int) (pieceattrib : String) (pieceattribclass : Int) (extractionmethod : Int) (outputattribs : Int) (computedistortion : Int) (distortionattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::facet" has_rundata]
opaque sop_facet (geo0 : Geometry) (group : String) (grouptype : Int) (prenml : Int) (unit : Int) (unique : Int) (cons : Int) (dist : Float) (accurate : Int) (inline : Int) (inlinedist : Float) (orientPolys : Int) (cusp : Int) (angle : Float) (remove : Int) (mkplanar : Int) (postnml : Int) (reversenml : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::featherattribinterpolate" has_rundata]
opaque sop_featherattribinterpolate (geo0 : Geometry) (geo1 : Geometry) (group : String) (barbattribs : String) (identifierattrib : String) (weightsattrib : String) (blendattrib : String) (shaftsubd : Int) (shaftbasesegs : Int) (useshaftbasesegsattrib : Int) (shaftbasesegsattrib : String) (barbsegmode : Int) (barbsegs : Int) (barbmirror : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::featherbarbtangents" has_rundata]
opaque sop_featherbarbtangents (geo0 : Geometry) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::featherray" has_rundata]
opaque sop_featherray (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (geo3 : Geometry) (group : String) (vertexattribs : String) (pointattribs : String) (createprimnumattribs : Int) (primnumattrib : String) (createprimuvattribs : Int) (primuvattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::feathertemplateinterpolatecore" has_rundata]
opaque sop_feathertemplateinterpolatecore (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (indicesattrib : String) (weightsattrib : String) (shaftbasesegsattrib : String) (sourceuattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::file" has_rundata]
opaque sop_file (geo0 : Geometry) (filemode : Int) (file : String) (objpattern : String) (geodatapath : String) (missingframe : Int) (loadtype : Int) (packedviewedit : Int) (viewportlod : Int) (packexpanded : Int) (delayload : Int) (mkpath : Int) (cachesize : Int) (prefetch : Int) (f : Vector2) (index : Float) (wrap : Int) (retry : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::fit" has_rundata]
opaque sop_fit (geo0 : Geometry) (group : String) (method : Int) (type : Int) (surftype : Int) (orderu : Int) (orderv : Int) (tol : Float) (smooth : Float) (multipleu : Int) (multiplev : Int) (scope : Int) (dataparmu : Int) (dataparmv : Int) (closeu : Int) (closev : Int) (corners : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::font" has_rundata]
opaque sop_font (type : Int) (file : String) (text : String) (halign : Int) (valign : Int) (use_descender : Int) (t : Vector3) (r : Vector3) (s : Vector2) (fontsize : Float) (tracking : Vector2) (autokern : Int) (oblique : Float) (lod : Float) (hole : Int) (addattrib : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::fractal" has_rundata]
opaque sop_fractal (geo0 : Geometry) (group : String) (divs : Int) (smooth : Float) (scale : Float) (seed : Int) (fixed : Int) (vtxnms : Int) (nmlattrib : String) (dir : Vector3) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::fuse" has_rundata]
opaque sop_fuse (geo0 : Geometry) (group : String) (usedist : Int) (dist : Float) (deldegen : Int) (keepunusedpoints : Int) (keepconsolidatedpoints : Int) (grouppropagation : Int) (switcher : Int) (snaptype : Int) (tol3d : Float) (snappointpos : Int) (snappointattribs : Int) (pointattribnames : String) (snapswitcher : Int) (gridtype : Int) (gridspacing : Vector3) (gridlines : Vector3) (gridpow2 : Vector3) (gridoffset : Vector3) (gridround : Int) (gridtol : Float) (updatenml : Int) (accurate : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::fuse::2.0" has_rundata]
opaque sop_fuse_2_0 (geo0 : Geometry) (geo1 : Geometry) (querygroup : String) (posattrib : String) (snaptype : Int) (algorithm : Int) (usetol3d : Int) (tol3d : Float) (targetptattrib : String) (targetclass : Int) (usepositionsnapmethod : Int) (positionsnapmethod : Int) (positionsnapweightname : String) (useradiusattrib : Int) (radiusattrib : String) (usematchattrib : Int) (matchattrib : String) (matchtype : Int) (matchtol : Float) (gridtype : Int) (gridspacing : Vector3) (gridlines : Vector3) (gridpow2 : Vector3) (gridoffset : Vector3) (gridround : Int) (usegridtol : Int) (gridtol : Float) (consolidatesnappedpoints : Int) (keepconsolidatedpoints : Int) (deldegen : Int) (deldegenpoints : Int) (delunusedpoints : Int) (recomputenml : Int) (createsnappedgroup : Int) (snappedgroupname : String) (createsnappedattrib : Int) (snappedattribname : String) (numpointattribs : DictArray) (numgroups : DictArray) (usetargetgroup : Int) (targetgroup : String) (modifyboth : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::grid" has_rundata]
opaque sop_grid (type : Int) (surftype : Int) (orient : Int) (size : Vector2) (t : Vector3) (r : Vector3) (rows : Int) (cols : Int) (orderu : Int) (orderv : Int) (interpu : Int) (interpv : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::groupcombine" has_rundata]
opaque sop_groupcombine (geo0 : Geometry) (numcombine : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::groupcopy" has_rundata]
opaque sop_groupcopy (geo0 : Geometry) (geo1 : Geometry) (primitives : Int) (primgroups : String) (primnameprefix : String) (enablematchbyprimattrib : Int) (matchbyprimattrib : String) (points : Int) (pointgroups : String) (pointnameprefix : String) (enablematchbypointattrib : Int) (matchbypointattrib : String) (edges : Int) (edgegroups : String) (edgenameprefix : String) (vertices : Int) (vertexgroups : String) (vertexnameprefix : String) (enablematchbyvertexattrib : Int) (matchbyvertexattrib : String) (groupnameconflict : String) (copyemptygroups : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::groupcreate" has_rundata]
opaque sop_groupcreate (geo0 : Geometry) (geo1 : Geometry) (groupname : String) (grouptype : Int) (mergeop : Int) (groupbase : Int) (basegroup : String) (ordered : Int) (geotype : Int) (groupbounding : Int) (boundtype : Int) (size : Vector3) (t : Vector3) (includenotwhollycontained : Int) (iso : Float) (invertvolume : Int) (groupnormal : Int) (camerapath : String) (nonplanar : Int) (nonplanartol : Float) (dir : Vector3) (angle : Float) (oppositenormals : Int) (groupedges : Int) (dominedgeangle : Int) (minedgeangle : Float) (domaxedgeangle : Int) (maxedgeangle : Float) (edgeanglebetweenedges : Int) (dominedgelen : Int) (minedgelen : Float) (domaxedgelen : Int) (maxedgelen : Float) (dodepth : Int) (edgestep : Int) (edgeptgrp : String) (unshared : Int) (boundarygroups : Int) (grouprandom : Int) (globalseed : Float) (useseedattrib : Int) (seedattrib : String) (percent : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::groupdelete" has_rundata]
opaque sop_groupdelete (geo0 : Geometry) (deletions : DictArray) (removegrp : Int) (selectiongroup : String) (selectiongrouptype : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::groupexpand" has_rundata]
opaque sop_groupexpand (geo0 : Geometry) (outputgroup : String) (group : String) (grouptype : Int) (primshareedge : Int) (floodfill : Int) (numsteps : Int) (usestepattrib : Int) (stepattrib : String) (bynormal : Int) (normalangle : Float) (overridenormal : Int) (normalattrib : String) (useconnectivityattrib : Int) (connectivityattrib : String) (tol : Float) (usecolgroup : Int) (colgroup : String) (colgrouptype : Int) (colgroupinvert : Int) (colgroupallowonbound : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::groupfindpath" has_rundata]
opaque sop_groupfindpath (geo0 : Geometry) (outgroup : String) (group : String) (grouptype : Int) (pathcontroltype : Int) (operation : Int) (avoidprevious : Int) (edgestyle : Int) (useuvattrib : Int) (uvattrib : String) (usecolgroup : Int) (colgroup : String) (colgrouptype : Int) (colgroupinvert : Int) (colgrouponbnd : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::groupinvert" has_rundata]
opaque sop_groupinvert (geo0 : Geometry) (grouptype : Int) (group : String) (newname : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::grouppromote" has_rundata]
opaque sop_grouppromote (geo0 : Geometry) (promotions : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::grouprange" has_rundata]
opaque sop_grouprange (geo0 : Geometry) (numrange : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::grouprename" has_rundata]
opaque sop_grouprename (geo0 : Geometry) (renames : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::groupsfromname" has_rundata]
opaque sop_groupsfromname (geo0 : Geometry) (attribname : String) (class' : Int) (groupprefix : String) (conflict : Int) (invalidnames : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::grouptransfer" has_rundata]
opaque sop_grouptransfer (geo0 : Geometry) (geo1 : Geometry) (primitives : Int) (primgroups : String) (primnameprefix : String) (points : Int) (pointgroups : String) (pointnameprefix : String) (edges : Int) (edgegroups : String) (edgenameprefix : String) (groupnameconflict : String) (threshold : Int) (thresholddist : Float) (keepifempty : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::guidegroomcore" has_rundata]
opaque sop_guidegroomcore (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (geo3 : Geometry) (stashnode : String) (group : String) (grouptype : Int) (usemask : Int) (mirrorbrush : Int) (mirror : Int) (mirrorgroup : String) (mirrororigin : Vector3) (mirrordir : Vector3) (tool : Int) (raybias : Float) (strandmode : Int) (collidewithskin : Int) (createorient : Int) (orientname : String) (orientup : Vector3) (orientupattrib : String) (orientupattribflip : Int) (templatesourcegroup : String) (widthoverride : Int) (width : Float) (brushsurface : Int) (usepartinglines : Int) (screenradius : Float) (objectradius : Float) (soft : Float) (spacing : Float) (density : Float) (brushmaintainlength : Float) (brushsnaptosurface : Int) (brushbend : Float) (brushbendfalloff : Float) (liftmode : Int) (liftangle : Float) (adjustlengthmode : Int) (adjustlengthvalue : Float) (adjustlengthlength : Float) (adjustlengthmethod : Int) (moverotatewithskin : Int) (sculptdoconstrain : Int) (sculptlockroot : Int) (sculptmaintainevensegments : Int) (smoothtargetsmoothness : Float) (smoothposition : Int) (smoothorient : Int) (blurinskinspace : Int) (blurperpoint : Int) (blurshape : Int) (blurlength : Int) (blurorient : Int) (cutmode : Int) (clumpperpoint : Int) (plantmode : Int) (plantinterpolateguides : Int) (plantinterpolaterelskin : Int) (plantinterpolateorient : Int) (planttemplatesource : Int) (plantlength : Float) (plantsegcount : Int) (drawonskin : Int) (drawinterpolateorient : Int) (drawtemplatesource : Int) (drawlimitlength : Int) (drawcurvetype : Int) (drawsegmode : Int) (drawsegcount : Int) (drawseglength : Float) (orientbrushupmode : Int) (orientbrushperpoint : Int) (painttemplateallowblending : Int) (editactiveprim : Int) (editsoft : Int) (editsoftmode : Int) (editcurvefrac : Float) (editcurvedist : Float) (resamplesegmode : Int) (resamplesegcount : Int) (resampleseglength : Float) (twistangle : Float) (curvemaskramp : FloatRamp) (groom : DataItem) (strands : DataItem) (strokes : DataItem) (previewutil : Int) (paintmaskpoints : Int) (groupnewprims : Int) (namenewprims : Int) (activegroup : String) (nameattrib : String) (activename : String) (overridecolor : Int) (colorbyname : Int) (defaultcolor : Vector3) (templatecolor : Vector3) (colorseed : Int) (folder_grouplist : DictArray) (xOrd : Int) (rOrd : Int) (t : Vector3) (r : Vector3) (s : Vector3) (shear : Vector3) (scale : Float) (p : Vector3) (pr : Vector3) (camxform0 : Vector4) (camxform1 : Vector4) (camxform2 : Vector4) (camxform3 : Vector4) (campos : Vector3) (cursorpos : Vector2) (cursordisplace : Vector3) (raypos : Vector3) (raydir : Vector3) (strengthscale : Float) (button : Int) (shiftkey : Int) (ctrlkey : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::guidemask" has_rundata]
opaque sop_guidemask (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (geo3 : Geometry) (uvattrib : String) (grouptype : Int) (group : String) (zeroungrouped : Int) (inputmask : Float) (inputmaskoverride : Int) (inputmaskcurveattrib : String) (inputmaskattrib : String) (inputmasktexture : String) (vismasks : Int) (outattribtype : Int) (outattrib : String) (createprimgroup : Int) (outprimgroup : String) (createintattrib : Int) (intattrib : String) (intattribvalue : Int) (intattribthresh : Float) (usenoisemask : Int) (noisemaskamount : Float) (noisemaskamountoverride : Int) (noisemaskamountcurveattrib : String) (noisemaskamountattrib : String) (noisemaskamounttexture : String) (noisemaskfreq : Float) (noisemaskgain : Float) (noisemaskgainoverride : Int) (noisemaskgaincurveattrib : String) (noisemaskgainattrib : String) (noisemaskgaintexture : String) (noisemaskbias : Float) (noisemaskbiasoverride : Int) (noisemaskbiascurveattrib : String) (noisemaskbiasattrib : String) (noisemaskbiastexture : String) (noisemaskcenterone : Int) (noisemaskfractal : Int) (noisemaskoct : Float) (noisemasklac : Float) (noisemaskrough : Float) (noisemaskroughoverride : Int) (noisemaskroughcurveattrib : String) (noisemaskroughattrib : String) (noisemaskroughtexture : String) (uselengthmask : Int) (lengthmode : Int) (lengthref : Float) (lengthrefoverride : Int) (lengthrefcurveattrib : String) (lengthrefattrib : String) (lengthreftexture : String) (lengthfalloffrange : Float) (lengthfalloffrangeoverride : Int) (lengthfalloffrangecurveattrib : String) (lengthfalloffrangeattrib : String) (lengthfalloffrangetexture : String) (lengthfalloffdecay : Float) (lengthfalloffdecayoverride : Int) (lengthfalloffdecaycurveattrib : String) (lengthfalloffdecayattrib : String) (lengthfalloffdecaytexture : String) (lengthrangemin : Float) (lengthrangemax : Float) (lengthpresets : Int) (lengthramp : FloatRamp) (useskincurvmask : Int) (skincurvconcavemax : Float) (skincurvconvexmax : Float) (skincurvesmoothstrength : Float) (skincurvpresets : Int) (skincurvramp : FloatRamp) (usegeomask : Int) (geovoxelsize : Float) (geointrange : Float) (geoextrange : Float) (geodepthramppresets : Int) (geodepthramp : FloatRamp) (geodoblur : Int) (geoblurradius : Float) (geobluriters : Int) (userandommask : Int) (randommaskcombine : Int) (randommaskseed : Float) (randommaskfrac : Float) (randommaskvar : Float) (randommaskvargain : Float) (usecurvemask : Int) (curvemaskabsrange : Int) (curvemaskrangemin : Float) (curvemaskrangemax : Float) (curvemaskeffectpos : Float) (curvemaskfalloff : Float) (curvemaskwidth : Float) (curvemaskramp : FloatRamp) (useskinlookupattribs : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::guideprocesscore" has_rundata]
opaque sop_guideprocesscore (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (geotype : Int) (barbside : Int) (uvattrib : String) (grouptype : Int) (group : String) (seed : Float) (vismasks : Int) (blend : Float) (blendoverride : Int) (blendcurveattrib : String) (blendattrib : String) (blendtexture : String) (op : Int) (setlength_mode : Int) (setlength_method : Int) (setlength_blend : Float) (setlength_randomize : Int) (setlength_length : Float) (setlength_lengthoverride : Int) (setlength_lengthoptions : Int) (setlength_lengthinrange : Vector2) (setlength_lengthoutrange : Vector2) (setlength_lengthremapramp : FloatRamp) (setlength_lengthcurveattrib : String) (setlength_lengthattrib : String) (setlength_lengthtexture : String) (setlength_lengthtextureprim : String) (setlength_lengthuvmode : Int) (setlength_lengthrandmin : Float) (setlength_lengthrandminoverride : Int) (setlength_lengthrandminoptions : Int) (setlength_lengthrandmininrange : Vector2) (setlength_lengthrandminoutrange : Vector2) (setlength_lengthrandminremapramp : FloatRamp) (setlength_lengthrandmincurveattrib : String) (setlength_lengthrandminattrib : String) (setlength_lengthrandmintexture : String) (setlength_lengthrandmintextureprim : String) (setlength_lengthrandminuvmode : Int) (setlength_lengthrandmax : Float) (setlength_lengthrandmaxoverride : Int) (setlength_lengthrandmaxoptions : Int) (setlength_lengthrandmaxinrange : Vector2) (setlength_lengthrandmaxoutrange : Vector2) (setlength_lengthrandmaxremapramp : FloatRamp) (setlength_lengthrandmaxcurveattrib : String) (setlength_lengthrandmaxattrib : String) (setlength_lengthrandmaxtexture : String) (setlength_lengthrandmaxtextureprim : String) (setlength_lengthrandmaxuvmode : Int) (setlength_scalefactor : Float) (setlength_scalefactoroverride : Int) (setlength_scalefactoroptions : Int) (setlength_scalefactorinrange : Vector2) (setlength_scalefactoroutrange : Vector2) (setlength_scalefactorremapramp : FloatRamp) (setlength_scalefactorcurveattrib : String) (setlength_scalefactorattrib : String) (setlength_scalefactortexture : String) (setlength_scalefactortextureprim : String) (setlength_scalefactoruvmode : Int) (setlength_scalefactorrandmin : Float) (setlength_scalefactorrandminoverride : Int) (setlength_scalefactorrandminoptions : Int) (setlength_scalefactorrandmininrange : Vector2) (setlength_scalefactorrandminoutrange : Vector2) (setlength_scalefactorrandminremapramp : FloatRamp) (setlength_scalefactorrandmincurveattrib : String) (setlength_scalefactorrandminattrib : String) (setlength_scalefactorrandmintexture : String) (setlength_scalefactorrandmintextureprim : String) (setlength_scalefactorrandminuvmode : Int) (setlength_scalefactorrandmax : Float) (setlength_scalefactorrandmaxoverride : Int) (setlength_scalefactorrandmaxoptions : Int) (setlength_scalefactorrandmaxinrange : Vector2) (setlength_scalefactorrandmaxoutrange : Vector2) (setlength_scalefactorrandmaxremapramp : FloatRamp) (setlength_scalefactorrandmaxcurveattrib : String) (setlength_scalefactorrandmaxattrib : String) (setlength_scalefactorrandmaxtexture : String) (setlength_scalefactorrandmaxtextureprim : String) (setlength_scalefactorrandmaxuvmode : Int) (setlength_cullzerolen : Int) (setlength_cullthreshold : Float) (bend_blend : Float) (bend_axismode : Int) (bend_axis : Vector3) (bend_axiscurveattrib : String) (bend_axisskinattrib : String) (bend_dir : Vector3) (bend_dircurveattrib : String) (bend_dirskinattrib : String) (bend_dirtoaxis : Int) (bend_angle : Float) (bend_angleoverride : Int) (bend_angleoptions : Int) (bend_angleinrange : Vector2) (bend_angleoutrange : Vector2) (bend_anglemapramptobarbs : Int) (bend_angleremapramp : FloatRamp) (bend_anglecurveattrib : String) (bend_angleattrib : String) (bend_angletexture : String) (bend_angletextureprim : String) (bend_angleuvmode : Int) (bend_anglerand : Float) (bend_anglerandoverride : Int) (bend_anglerandoptions : Int) (bend_anglerandinrange : Vector2) (bend_anglerandoutrange : Vector2) (bend_anglerandremapramp : FloatRamp) (bend_anglerandcurveattrib : String) (bend_anglerandattrib : String) (bend_anglerandtexture : String) (bend_anglerandtextureprim : String) (bend_angleranduvmode : Int) (bend_anglerandbias : Float) (bend_anglerandbiasoverride : Int) (bend_anglerandbiasoptions : Int) (bend_anglerandbiasinrange : Vector2) (bend_anglerandbiasoutrange : Vector2) (bend_anglerandbiasremapramp : FloatRamp) (bend_anglerandbiascurveattrib : String) (bend_anglerandbiasattrib : String) (bend_anglerandbiastexture : String) (bend_anglerandbiastextureprim : String) (bend_anglerandbiasuvmode : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::hairclump" has_rundata]
opaque sop_hairclump (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (geo3 : Geometry) (legacymasking : Int) (uvattrib : String) (blend : Float) (blendoverride : Int) (blendoptions : Int) (blendinrange : Vector2) (blendoutrange : Vector2) (blendremapramp : FloatRamp) (blendcurveattrib : String) (blendattrib : String) (blendtexture : String) (blendtextureprim : String) (useskinmask : Int) (skinmaskattrib : String) (clumpsize : Float) (clumpsizeoverride : Int) (clumpsizeoptions : Int) (clumpsizeinrange : Vector2) (clumpsizeoutrange : Vector2) (clumpsizeremapramp : FloatRamp) (clumpsizeattrib : String) (clumpsizetexture : String) (clumpsizetextureprim : String) (searchbeyondradius : Int) (clumpwithinclumps : Int) (crossoverrate : Float) (seed : Float) (method : Int) (preservelength : Int) (extendtomatch : Float) (shortentomatch : Float) (accuratebundling : Int) (hairwidth : Float) (hairwidthscale : Float) (tightness : Float) (tightnessoverride : Int) (tightnessoptions : Int) (tightnessinrange : Vector2) (tightnessoutrange : Vector2) (tightnessremapramp : FloatRamp) (tightnesscurveattrib : String) (tightnessclumpattrib : String) (tightnessattrib : String) (tightnesstexture : String) (tightnesstextureprim : String) (strayamount : Float) (strayamountoverride : Int) (strayamountoptions : Int) (strayamountinrange : Vector2) (strayamountoutrange : Vector2) (strayamountremapramp : FloatRamp) (strayamountcurveattrib : String) (strayamountclumpattrib : String) (strayamountattrib : String) (strayamounttexture : String) (strayamounttextureprim : String) (strayrate : Float) (strayrateoverride : Int) (strayrateoptions : Int) (strayrateinrange : Vector2) (strayrateoutrange : Vector2) (strayrateremapramp : FloatRamp) (strayratecurveattrib : String) (strayrateclumpattrib : String) (strayrateattrib : String) (strayratetexture : String) (strayratetextureprim : String) (strayfalloff : Float) (strayfalloffoverride : Int) (strayfalloffoptions : Int) (strayfalloffinrange : Vector2) (strayfalloffoutrange : Vector2) (strayfalloffremapramp : FloatRamp) (strayfalloffcurveattrib : String) (strayfalloffclumpattrib : String) (strayfalloffattrib : String) (strayfallofftexture : String) (strayfallofftextureprim : String) (clumpprofile : FloatRamp) (iterations : Float) (iterationsoverride : Int) (iterationsoptions : Int) (iterationsinrange : Vector2) (iterationsoutrange : Vector2) (iterationsremapramp : FloatRamp) (iterationscurveattrib : String) (iterationsattrib : String) (iterationstexture : String) (iterationstextureprim : String) (sizereduction : Float) (sizereductionoverride : Int) (sizereductionoptions : Int) (sizereductioninrange : Vector2) (sizereductionoutrange : Vector2) (sizereductionremapramp : FloatRamp) (sizereductionattrib : String) (sizereductiontexture : String) (sizereductiontextureprim : String) (goalfeedback : Float) (goalfeedbackoverride : Int) (goalfeedbackoptions : Int) (goalfeedbackinrange : Vector2) (goalfeedbackoutrange : Vector2) (goalfeedbackremapramp : FloatRamp) (goalfeedbackattrib : String) (goalfeedbacktexture : String) (goalfeedbacktextureprim : String) (tightnessreduction : Float) (tightnessreductionoverride : Int) (tightnessreductionoptions : Int) (tightnessreductioninrange : Vector2) (tightnessreductionoutrange : Vector2) (tightnessreductionremapramp : FloatRamp) (tightnessreductioncurveattrib : String) (tightnessreductionattrib : String) (tightnessreductiontexture : String) (tightnessreductiontextureprim : String) (enablecurling : Int) (curlamp : Float) (curlampoverride : Int) (curlampoptions : Int) (curlampinrange : Vector2) (curlampoutrange : Vector2) (curlampremapramp : FloatRamp) (curlampcurveattrib : String) (curlampattrib : String) (curlamptexture : String) (curlamptextureprim : String) (curlampramp : FloatRamp) (curlfreq : Float) (curlfreqoverride : Int) (curlfreqoptions : Int) (curlfreqinrange : Vector2) (curlfreqoutrange : Vector2) (curlfreqremapramp : FloatRamp) (curlfreqcurveattrib : String) (curlfreqattrib : String) (curlfreqtexture : String) (curlfreqtextureprim : String) (curlfreqramp : FloatRamp) (clumpidinputattrib : String) (createclumpidattrib : Int) (clumpidoutputattrib : String) (useorientinputattrib : Int) (orientinputattrib : String) (createtightnessattrib : Int) (tightnessoutputattrib : String) (clumppointattribs : String) (clumpprimattribs : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::hairgencore" has_rundata]
opaque sop_hairgencore (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (geo3 : Geometry) (geo4 : Geometry) (mode : Int) (group : String) (uvattrib : String) (limittobbox : Int) (bboxsize : Vector3) (bboxcenter : Vector3) (prune : Int) (pruningratio : Float) (prunethicken : Int) (useguides : Int) (uniformguidesegments : Int) (skininterp : Int) (useinterpmesh : Int) (skininterpguidesattrib : String) (skininterpweightsattrib : String) (skinguidemode : Int) (guideblendmethod : Int) (guidegroup : String) (influenceradius : Float) (influenceradiusoverride : Int) (influenceradiusoptions : Int) (influenceradiusinrange : Vector2) (influenceradiusoutrange : Vector2) (influenceradiusattrib : String) (influenceradiustexture : String) (influenceradiustextureprim : String) (influencedecay : Float) (influencedecayoverride : Int) (influencedecayoptions : Int) (influencedecayinrange : Vector2) (influencedecayoutrange : Vector2) (influencedecayattrib : String) (influencedecaytexture : String) (influencedecaytextureprim : String) (maxguidecount : Int) (maxguidecountoverride : Int) (maxguidecountoptions : Int) (maxguidecountinrange : Vector2) (maxguidecountoutrange : Vector2) (maxguidecountattrib : String) (maxguidecounttexture : String) (maxguidecounttextureprim : String) (maxguideangle : Float) (maxguideangleoverride : Int) (maxguideangleoptions : Int) (maxguideangleinrange : Vector2) (maxguideangleoutrange : Vector2) (maxguideangleattrib : String) (maxguideangletexture : String) (maxguideangletextureprim : String) (clumpcrossover : Float) (clumpcrossoveroverride : Int) (clumpcrossoveroptions : Int) (clumpcrossoverinrange : Vector2) (clumpcrossoveroutrange : Vector2) (clumpcrossoverattrib : String) (clumpcrossovertexture : String) (clumpcrossovertextureprim : String) (growunguided : Int) (useinitdirattrib : Int) (initdirattrib : String) (unguidedsegments : Int) (unguidedlength : Float) (unguidedlengthoverride : Int) (unguidedlengthoptions : Int) (unguidedlengthinrange : Vector2) (unguidedlengthoutrange : Vector2) (unguidedlengthattrib : String) (unguidedlengthtexture : String) (unguidedlengthtextureprim : String) (unguidedminlength : Float) (createweightattribs : Int) (createrestrootattrib : Int) (clumpidattrib : String) (outputthicknessattrib : Int) (thicknessattrib : String) (thickness : Float) (thicknessoverride : Int) (thicknessskinattrib : String) (thicknesstexture : String) (hairprofile : FloatRamp) (pointattribs : String) (vertattribs : String) (primattribs : String) (detailattribs : String) (guidepointattribs : String) (guideprimattribs : String) (folder_weightarraypairs : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::heatgeodesic" has_rundata]
opaque sop_heatgeodesic (geo0 : Geometry) (group : String) (srcpoints : String) (attrib : String) (smoothing : Float) (boundarybalance : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::hole" has_rundata]
opaque sop_hole (geo0 : Geometry) (group : String) (break' : Int) (dist : Float) (angle : Float) (snap : Int) (removeunusedpoints : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::inflate" has_rundata]
opaque sop_inflate (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (group : String) (toolgroup : String) (centergroup : String) (centerprimgroup : String) (mask : Float) (enablemaskattrib : Int) (maskattrib : String) (toolidname : String) (centeroverlap : Float) (blendexponent : Float) (clampsurfdist : Int) (minsurfdist : Float) (tightness : Float) (inflatetofarthest : Int) (maxinflateenable : Int) (maxinflate : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::intersectionanalysis" has_rundata]
opaque sop_intersectionanalysis (geo0 : Geometry) (geo1 : Geometry) (agroup : String) (bgroup : String) (useproxtol : Int) (proxtol : Float) (detectverts : Int) (outputsegs : Int) (useinputnumattrib : Int) (inputnumattrib : String) (useprimnumattrib : Int) (primnumattrib : String) (useprimuvwattrib : Int) (primuvwattrib : String) (useptnumattrib : Int) (ptnumattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::intersectionstitch" has_rundata]
opaque sop_intersectionstitch (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (agroup : String) (bgroup : String) (useproxtol : Int) (proxtol : Float) (splitcurves : Int) (inputnumattrib : String) (primnumattrib : String) (primuvwattrib : String) (keeppointattribs : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::invoke" has_rundata]
opaque sop_invoke (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (geo3 : Geometry) (blockpath : String) (inputs : DictArray) (unload : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::invokegraph" has_rundata]
opaque sop_invokegraph (geo0 : Geometry) (auxgeo : GeometryArray) (method : Int) (outputtype : Int) (outputgroup : String) (output : String) (inputs : DictArray) (inputgroup : String) (unload : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::isooffset" has_rundata]
opaque sop_isooffset (geo0 : Geometry) (geo1 : Geometry) (output : Int) (mode : Int) (uniformsamples : Int) (divs : Vector3) (samplediv : Int) (divsize : Float) (overrideoutput : Int) (isodiv : Vector3) (padbounds : Int) (overridebounds : Int) (min : Vector3) (max : Vector3) (tol : Float) (offset : Float) (laserscan : Int) (fixsigns : Int) (forcebounds : Int) (invert : Int) (numneighbour : Int) (sweepcount : Int) (sweepalpha : Int) (tetratype : Int) (filemode : Int) (filename : String) (name : String) (buildpolysoup : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::join" has_rundata]
opaque sop_join (geo0 : Geometry) (group : String) (blend : Int) (tolerance : Float) (bias : Float) (knotmult : Int) (proximity : Int) (dir : Int) (joinop : Int) (inc : Int) (loop : Int) (prim : Int) (onlyconnected : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::agentfromrigcore" has_rundata]
opaque sop_kinefx_agentfromrigcore (geo0 : Geometry) (createagentname : Int) (agentname : String) (createlocomotionjoint : Int) (pointgroups : String) (pointattribs : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::attribtransformcompute" has_rundata]
opaque sop_kinefx_attribtransformcompute (geo0 : Geometry) (group : String) (decompmethod : Int) (xOrd : Int) (rOrd : Int) (attributes : String) (usetrnattrib : Int) (trnprefix : String) (trnsuffix : String) (userotattrib : Int) (rotprefix : String) (rotsuffix : String) (usescaleattrib : Int) (scaleprefix : String) (scalesuffix : String) (useshearattrib : Int) (shearprefix : String) (shearsuffix : String) (usestretchattrib : Int) (stretchprefix : String) (stretchsuffix : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::attribtransformextract" has_rundata]
opaque sop_kinefx_attribtransformextract (geo0 : Geometry) (group : String) (hiersource : Int) (usepieceattrib : Int) (pieceattrib : String) (hint : Vector3) (decompmethod : Int) (xOrd : Int) (rOrd : Int) (attributes : String) (usetrnattrib : Int) (trnprefix : String) (trnsuffix : String) (userotattrib : Int) (rotprefix : String) (rotsuffix : String) (usescaleattrib : Int) (scaleprefix : String) (scalesuffix : String) (useshearattrib : Int) (shearprefix : String) (shearsuffix : String) (usestretchattrib : Int) (stretchprefix : String) (stretchsuffix : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::characterblendshapesadd" has_rundata]
opaque sop_kinefx_characterblendshapesadd (geo0 : Geometry) (geo1 : Geometry) (removeunchanged : Int) (unpacked : Int) (unpackedname : String) (inbetween : Int) (heroshapename : String) (weightattrib : String) (weightunpacked : Float) (skin : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::characterblendshapescore" has_rundata]
opaque sop_kinefx_characterblendshapescore (geo0 : Geometry) (geo1 : Geometry) (group : String) (grouptype : Int) (attribs : String) (useopencl : Int) (openclmode : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::computemotionclipcreate" has_rundata]
opaque sop_kinefx_computemotionclipcreate (soppath : String) (samplerate : Float) (framerange : Vector2) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::computemotionclipretime" has_rundata]
opaque sop_kinefx_computemotionclipretime (geo0 : Geometry) (evalmode : Int) (trim : Int) (setanimstart : Int) (animstart : Float) (setanimend : Int) (animend : Float) (setshift : Int) (shift : Float) (speed : Float) (path : String) (usetimeparm : Int) (timeparm : String) (useframeparm : Int) (frameparm : String) (usespeedparm : Int) (speedparm : String) (useoutputrange : Int) (outputrange : Vector2) (useoutputsamplerate : Int) (outputsamplerate : Float) (repackattribs : Int) (restattribs : String) (animattribs : String) (setleftendbehavior : Int) (leftendbehavior : Int) (setrightendbehavior : Int) (rightendbehavior : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::computerigpose" has_rundata]
opaque sop_kinefx_computerigpose (geo0 : Geometry) (path : String) (parm : String) (restattrib : String) (worldspace : Int) (multithread : Int) (preserveshears : Int) (outputparmattribs : Int) (outputparmt : Int) (outputparmr : Int) (outputparms : Int) (outputparmp : Int) (outputparmpr : Int) (outputinternalattribs : Int) (outputlocaltransform : Int) (outputinputlocaltransform : Int) (outputeffectivelocaltransform : Int) (transformations : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::computetransform" has_rundata]
opaque sop_kinefx_computetransform (geo0 : Geometry) (mode : Int) (constrainedgroup : String) (outputlocaltransform : Int) (outputeffectivelocaltransform : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::configurerigvis::2.0" has_rundata]
opaque sop_kinefx_configurerigvis_2_0 (geo0 : Geometry) (configurations : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::ikchains::2.0" has_rundata]
opaque sop_kinefx_ikchains_2_0 (geo0 : Geometry) (geo1 : Geometry) (multithread : Int) (ikchains : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::motionclipextractkeyposes" has_rundata]
opaque sop_kinefx_motionclipextractkeyposes (geo0 : Geometry) (viewerstate : String) (outputmethod : Int) (repackattribs : Int) (restattribs : String) (animattribs : String) (method : Int) (target : Int) (percentage : Float) (keyposes : Int) (reducepasttarget : Int) (tolerance : Float) (usemaxstep : Int) (maxstep : Int) (trim : Int) (userange : Int) (range : Vector2) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::motionclipmerge" has_rundata]
opaque sop_kinefx_motionclipmerge (geo0 : Geometry) (geo1 : Geometry) (clipname : String) (merge1 : Int) (merge2 : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::motionclipposedelete::2.0" has_rundata]
opaque sop_kinefx_motionclipposedelete_2_0 (geo0 : Geometry) (jointgroup : String) (negatejoints : Int) (selectionmode : Int) (cliprangemode : Int) (framerange : Vector2) (selectoutsideframes : Int) (selectnthframe : Int) (framestep : Int) (frameoff : Int) (frameselect : Vector2) (includestart : Int) (includeend : Int) (negateframerange : Int) (framepattern : String) (negatepattern : Int) (useposerange : Int) (poserange : Vector2) (selectoutsideposes : Int) (selectnthpose : Int) (posestep : Int) (poseoff : Int) (poseselect : Vector2) (includefirst : Int) (includelast : Int) (negatposerange : Int) (posegroup : String) (negateposegroup : Int) (viewerstate : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::motionclipunpack" has_rundata]
opaque sop_kinefx_motionclipunpack (geo0 : Geometry) (jointnames : String) (samplemode : Int) (frame : Float) (interp : Int) (outputmode : Int) (unpackexisting : Int) (cliprangemode : Int) (framerange : Vector3) (useendbehavior : Int) (endbehavior : Int) (outputcom : Int) (isolatecom : Int) (comname : String) (useconfigattrib : Int) (configattrib : String) (useattribs : Int) (restattribs : String) (attribs : String) (skipnonanimattribs : Int) (outputtime : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::motionclipupdate" has_rundata]
opaque sop_kinefx_motionclipupdate (geo0 : Geometry) (geo1 : Geometry) (viewerstate : String) (overlapmode : Int) (newmode : Int) (uselocals : Int) (repackattribs : Int) (restattribs : String) (animattribs : String) (blendjoints : Int) (doptblend : Int) (ptblend : String) (weightmethod : Int) (numgroups : DictArray) (weightsource : Int) (weightattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::motionmixer" has_rundata]
opaque sop_kinefx_motionmixer (geo0 : Geometry) (characters : String) (displaycharacters : String) (outputmode : String) (animationclip : String) (outputlabels : Int) (mixerstart : Float) (mixerend : Float) (mixerfps : Float) (solotrack : String) (fxfiles : DictArray) (tracks : DictArray) (rOrd___ : Int) (t___ : Vector3) (r___ : Vector3) (s___ : Vector3) (p___ : Vector3) (pr___ : Vector3) (transit___ : String) (transit_inshape___ : String) (transit_in___ : Vector2) (transit_inz___ : Vector2) (transit_outshape___ : String) (transit_out___ : Vector2) (transit_outz___ : Vector2) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::pendulummotioncore" has_rundata]
opaque sop_kinefx_pendulummotioncore (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (geo3 : Geometry) (group : String) (pivotgroup : String) (usemotionpath : Int) (motionpathmode : Int) (rangetoreplace : Vector2) (usesourceframes : Int) (framesafterend : Float) (shiftanimbefore : Int) (shiftanimafter : Int) (inputinterp : Int) (transitioninterp : Int) (usetransitionin : Int) (transitionin : Vector2) (transitionincomp : Int) (usetransitionout : Int) (transitionout : Vector2) (transitionoutcomp : Int) (startframe : Float) (output : Int) (frame : Float) (samplerate : Float) (steps : Int) (prebehavior : Int) (postbehavior : Int) (useconfigattrib : Int) (configattrib : String) (mass : Float) (drag : Float) (gravity : Vector3) (startsource : Int) (startposattrib : String) (startpos : Vector3) (relstartpos : Vector3) (pivotsource : Int) (pivotattrib : String) (pivot : Vector3) (useworldpivot : Int) (pendtype : Int) (speedsource : Int) (speed : Float) (tiltsimple : Float) (reversedirsimple : Int) (tiltconical : Float) (reversedirconical : Int) (launchmethod : Int) (lifefree : Float) (targetsource : Int) (targetposattrib : String) (targetpos : Vector3) (reltargetpos : Vector3) (maxtargetlife : Float) (circletotarget : Int) (endattarget : Int) (lifetarget : Float) (velocitysource : Int) (velocityattrib : String) (velocity : Vector3) (relvelocity : Vector3) (endcondition : Int) (useendframevel : Int) (endframevel : Float) (lifevel : Float) (endheight : Float) (heightpassnum : Int) (endplanepos : Vector3) (endplanenormal : Vector3) (planepassnum : Int) (maxframe : Float) (createvattrib : Int) (vscale : Float) (createendtimeattrib : Int) (endtimeattrib : String) (createrelposattrib : Int) (relposattrib : String) (createoutpivotattrib : Int) (outpivotattrib : String) (createpathconfigattrib : Int) (pathconfigattrib : String) (createsourcetimeattrib : Int) (sourcetimeattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::poseweightinterp" has_rundata]
opaque sop_kinefx_poseweightinterp (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (poseweightattrib : String) (smoothingmode : Int) (clampinput : Int) (kerneltype : Int) (hyperplane : Int) (maxhyperplane : Int) (exponent : Int) (usefalloffattrib : Int) (falloffattrib : String) (positiveweights : Int) (damping : Float) (stash : Int) (interpmode : Int) (distancepow : Float) (resolution : Float) (passbandfreq : Float) (laplacianconstraints : Int) (boxconstraints : Int) (secondorder : Int) (numiterations : Int) (usecoordattrib : Int) (coordattrib : String) (ncoords : DictArray) (falloff : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::projectilemotioncore" has_rundata]
opaque sop_kinefx_projectilemotioncore (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (geo3 : Geometry) (group : String) (projectiles : Int) (usepieceattrib : Int) (pieceattrib : String) (usemotionpath : Int) (motionpathmode : Int) (rangetoreplace : Vector2) (usesourceframes : Int) (framesafterend : Float) (shiftanimbefore : Int) (shiftanimafter : Int) (inputinterp : Int) (transitioninterp : Int) (usetransitionin : Int) (transitionin : Vector2) (usetransitionout : Int) (transitionout : Vector2) (startframe : Float) (output : Int) (frame : Float) (prebehavior : Int) (postbehavior : Int) (samplerate : Int) (substeps : Int) (useconfigattrib : Int) (configattrib : String) (mass : Float) (drag : Float) (gravity : Vector3) (startsource : Int) (usestartgroup : Int) (startgroup : String) (startposattrib : String) (startpos : Vector3) (relstartpos : Vector3) (launchmethod : Int) (targetsource : Int) (usetargetgroup : Int) (targetgroup : String) (targetposattrib : String) (targetpos : Vector3) (reltargetpos : Vector3) (targetingmethod : Int) (useendframetarget : Int) (endframetarget : Float) (lifetarget : Float) (heighttarget : Float) (heightplanepos : Vector3) (heightplanenormal : Vector3) (initialangle : Float) (targetangle : Float) (speedsource : Int) (speedscale : Float) (speedattrib : String) (speed : Float) (prefershortpath : Int) (endattarget : Int) (velocitysource : Int) (velocityattrib : String) (velocity : Vector3) (relvelocity : Vector3) (endcondition : Int) (useendframe : Int) (endframe : Float) (life : Float) (endheight : Float) (preferfirstheight : Int) (endplanepos : Vector3) (endplanenormal : Vector3) (preferfirstplane : Int) (createvattrib : Int) (vscale : Float) (createpathnumattrib : Int) (pathnumattrib : String) (createpathpointsnumattrib : Int) (pathpointsnumattrib : String) (createpathpointidxattrib : Int) (pathpointidxattrib : String) (createlaunchspeedattrib : Int) (launchspeedattrib : String) (createendtimeattrib : Int) (endtimeattrib : String) (createrelposattrib : Int) (relposattrib : String) (createsourcetimeattrib : Int) (sourcetimeattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::resamplesplinetransforms" has_rundata]
opaque sop_kinefx_resamplesplinetransforms (geo0 : Geometry) (geo1 : Geometry) (mode : Int) (refaxis : Int) (primaryaxis : Int) (secondaryaxisx : Int) (secondaryaxisy : Int) (secondaryaxisz : Int) (numjoints : Int) (segmentlength : Float) (fixedlength : Int) (extrapolate : Int) (extrapolatealong : Int) (extrapolateaxis : Int) (sticky : Int) (twistattrib : String) (nameprefix : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::rigattribvop" has_rundata]
opaque sop_kinefx_rigattribvop (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (geo3 : Geometry) (vex_cwdpath : String) (vex_outputmask : String) (bindings : DictArray) (groupbindings : DictArray) (autobind : Int) (groupautobind : Int) (bindclass : Int) (bindgroup : String) (bindgrouptype : Int) (vex_multithread : Int) (vex_updatenmls : Int) (vex_numcount : Int) (vex_threadjobsize : Int) (vex_matchattrib : String) (vex_selectiongroup : String) (vex_inplace : Int) (vex_precision : String) (vexsrc : Int) (script : String) (vexsnippet : String) (vex_strict : Int) (vex_exportlist : String) (vex_strictvariables : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::skeletonblend::2.0" has_rundata]
opaque sop_kinefx_skeletonblend_2_0 (geo0 : Geometry) (geo1 : Geometry) (bindgroup : String) (matchattrib : Int) (attribtomatch : String) (blendtoattrib : Int) (poseattrib : String) (components : Int) (biasmode : Int) (bias : Float) (biasattribute : String) (worldspace : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::skeletonblend::3.0" has_rundata]
opaque sop_kinefx_skeletonblend_3_0 (geo0 : Geometry) (auxgeo : GeometryArray) (group : String) (blendtopose : Int) (poseattrib : String) (poseweight : Float) (posecomponents : Int) (differenceblend : Int) (worldspace : Int) (maskmode : Int) (maskattrib : String) (maskattribmode : Int) (nblends : DictArray) (matchattrib : Int) (attribtomatch : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::smoothmotioncore" has_rundata]
opaque sop_kinefx_smoothmotioncore (geo0 : Geometry) (jointsgroup : String) (rangemode : Int) (framerange : Vector2) (mode : Int) (customattributes : String) (crack : Int) (xOrd : Int) (rOrd : Int) (filtertype : Int) (filterorder : Int) (cutofffrequency : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::usdanimimport" has_rundata]
opaque sop_kinefx_usdanimimport (usdsource : Int) (loppath : String) (usdfile : String) (skelrootpath : String) (color : Vector3) (output : Int) (timeshiftmethod : Int) (time : Float) (useanimationstarttime : Int) (animationstarttime : Float) (useanimationendtime : Int) (animationendtime : Float) (useplaybackstarttime : Int) (playbackstarttime : Float) (frame : Float) (useanimationstartframe : Int) (animationstartframe : Float) (useanimationendframe : Int) (animationendframe : Float) (useplaybackstartframe : Int) (playbackstartframe : Float) (speed : Float) (clipname : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::kinefx::usdskinimport" has_rundata]
opaque sop_kinefx_usdskinimport (usdsource : Int) (loppath : String) (usdfile : String) (skelrootpath : String) (purpose : String) (shapeattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::knife" has_rundata]
opaque sop_knife (geo0 : Geometry) (group : String) (knifeop : Int) (origin : Vector3) (dist : Float) (dir : Vector3) (newg : Int) (above : String) (below : String) (clippts : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::lidarimport" has_rundata]
opaque sop_lidarimport (filename : String) (precision : String) (loadtype : Int) (filter_type : Int) (select_range : Vector2) (max_points : Int) (delete_invalid : Int) (color : Int) (intensity : Int) (ret_data : Int) (timestamp : Int) (normals : Int) (row_col : Int) (scanindex : Int) (ptnames : Int) (group_prefix : String) (scangroups : Int) (rigidtransforms : Int) (scannames : Int) (classindex : Int) (classname : Int) (classflags : Int) (scannerchannel : Int) (scanflags : Int) (userdata : Int) (scanangle : Int) (pointsourceid : Int) (nearinfrared : Int) (xOrd : Int) (rOrd : Int) (t : Vector3) (r : Vector3) (s : Vector3) (shear : Vector3) (scale : Float) (centroid : Int) (p : Vector3) (pr : Vector3) (prexform_xOrd : Int) (prexform_rOrd : Int) (prexform_t : Vector3) (prexform_r : Vector3) (prexform_s : Vector3) (prexform_shear : Vector3) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::line" has_rundata]
opaque sop_line (type : Int) (origin : Vector3) (dir : Vector3) (dist : Float) (points : Int) (order : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::linearsolver" has_rundata]
opaque sop_linearsolver (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (mode : Int) (cookinplace : Int) (precision : Int) (useiterativesolver : Int) (iterativesolver : Int) (solvewithguess : Int) (useeigensolver : Int) (spectraeigensolver : Int) (numeigenpairs : Int) (shift : Float) (solvertolerance : Float) (solvermaxiter : Int) (matrixstorage : Int) (volumeencoding : Int) (pointsprimsencoding : Int) (detailencoding : Int) (squarematrix : Int) (rows : Int) (cols : Int) (rowattr : String) (colattr : String) (matrixvalueattr : String) (vectorsrcstorage : Int) (vectorsrcattr : String) (vectordststorage : Int) (vectordstattr : String) (pinnedgroup : String) (reducerows : Int) (scale : Float) (accumulateresult : Int) (densedirectsolver : Int) (sparsedirectsolver : Int) (sparsedirectsolverbackend : Int) (densepreconditioner : Int) (sparsepreconditioner : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::lopimport::2.0" has_rundata]
opaque sop_lopimport_2_0 (loppath : String) (primpattern : String) (excludeinactiveprims : Int) (purpose : String) (importtraversal : String) (striplayers : Int) (timesample : Int) (importframe : Float) (staticimportframe : Float) (addpathattrib : Int) (pathattrib : String) (addnameattrib : Int) (nameattrib : String) (viewportlod : Int) (pivot : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::matchtopology" has_rundata]
opaque sop_matchtopology (geo0 : Geometry) (geo1 : Geometry) (trackpts : String) (refpts : String) (assumeprimmatch : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::material" has_rundata]
opaque sop_material (geo0 : Geometry) (style : String) (uselabels : Int) (createstylesheets : Int) (fullpath : Int) (num_materials : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::measure" has_rundata]
opaque sop_measure (geo0 : Geometry) (group : String) (type : Int) (override : Int) (attribname : String) (curvetype : Int) (curve : Vector2) (t : Vector3) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::measure::2.0" has_rundata]
opaque sop_measure_2_0 (geo0 : Geometry) (group : String) (grouptype : Int) (measure : Int) (curvaturetype : Int) (principaltype : Int) (principalsign : Int) (principalreportas : Int) (umbiliccutoff : Float) (integrationmode : Int) (srcattrib : String) (srccomp : Int) (scalenormalize : Int) (integrationdomain : Int) (refinetomanifold : Int) (pieceattrib : String) (usecustompos : Int) (posattrib : String) (userangemin : Int) (rangemin : Float) (userangemax : Int) (rangemax : Float) (usecenterwidth : Int) (width : Float) (widthscale : Int) (centertype : Int) (fixedcenter : Float) (colorramp : ColorRamp) (vectorscale : Float) (attribname : String) (usetotalattrib : Int) (totalattribname : String) (userangegroup : Int) (rangegroup : String) (bakeintooutput : Int) (useremaprange : Int) (remaprange : Vector2) (divideelementarea : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::merge" has_rundata]
opaque sop_merge (auxgeo : GeometryArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::mergepacked" has_rundata]
opaque sop_mergepacked (auxgeo : GeometryArray) (setprimname : Int) (primname : String) (setprimindex : Int) (primindex : String) (nameoverride : Int) (nameoverrideattrib : String) (pack : Int) (onlypackunpacked : Int) (ignoreempty : Int) (namingmethod : Int) (singlemode : Int) (singleindex : Int) (names : DictArray) (pivot : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::mirror" has_rundata]
opaque sop_mirror (geo0 : Geometry) (group : String) (operation : Int) (dirtype : Int) (t : Vector3) (r : Vector3) (origin : Vector3) (dir : Vector3) (dist : Float) (reversenml : Int) (keepOriginal : Int) (consolidatepts : Int) (consolidatetol : Float) (consolidateunshared : Int) (createoutputgroup : Int) (outputgroup : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::ml_examplecreatecore" has_rundata]
opaque sop_ml_examplecreatecore (geo0 : Geometry) (geo1 : Geometry) (usepackedinputcomponent : Int) (usepackedtargetcomponent : Int) (inputvalidityattribute : String) (targetvalidityattribute : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::ml_exampledecomposecore" has_rundata]
opaque sop_ml_exampledecomposecore (geo0 : Geometry) (componenttype : Int) (keepcomponentpacked : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::ml_exampledeserializepacked" has_rundata]
opaque sop_ml_exampledeserializepacked (geo0 : Geometry) (inputdimensionattribute : String) (targetdimensionattribute : String) (serialattribute : String) (inputs : DictArray) (targets : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::ml_exampledeserializepoint" has_rundata]
opaque sop_ml_exampledeserializepoint (geo0 : Geometry) (inputdimensionattribute : String) (targetdimensionattribute : String) (serialattribute : String) (inputpointattributes : String) (targetpointattributes : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::ml_exampleserializepacked" has_rundata]
opaque sop_ml_exampleserializepacked (geo0 : Geometry) (outputmode : Int) (inputdimensionattribute : String) (targetdimensionattribute : String) (serialattribute : String) (sopoutput : String) (inputs : DictArray) (targets : DictArray) (componentlayoutencoding : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::ml_exampleserializepoint" has_rundata]
opaque sop_ml_exampleserializepoint (geo0 : Geometry) (inputdimensionattribute : String) (targetdimensionattribute : String) (serialattribute : String) (inputpointattributes : String) (targetpointattributes : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::ml_extractexamplecore" has_rundata]
opaque sop_ml_extractexamplecore (geo0 : Geometry) (index : Int) (keeppacked : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::ml_posedeserializecore" has_rundata]
opaque sop_ml_posedeserializecore (geo0 : Geometry) (geo1 : Geometry) (jointgroup : String) (serialattribute : String) (mode : Int) (subskeletonincludeworldrotation : Int) (alllocalrotation : Int) (localrotationgroup : String) (alllocaltranslation : Int) (localtranslationgroup : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::ml_posegeneratecore" has_rundata]
opaque sop_ml_posegeneratecore (geo0 : Geometry) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::ml_poseserializecore" has_rundata]
opaque sop_ml_poseserializecore (geo0 : Geometry) (jointgroup : String) (serialattribute : String) (mode : Int) (subskeletonincludeworldrotation : Int) (alllocalrotation : Int) (localrotationgroup : String) (alllocaltranslation : Int) (localtranslationgroup : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::ml_regressioninferencecore" has_rundata]
opaque sop_ml_regressioninferencecore (geo0 : Geometry) (provider : Int) (batch : Int) (domaxbatch : Int) (maxbatch : Int) (modelfile : String) (inputs : DictArray) (outputs : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::ml_regressionkernelcore" has_rundata]
opaque sop_ml_regressionkernelcore (geo0 : Geometry) (geo1 : Geometry) (batch : Int) (weightdecay : Float) (errorthreshold : Float) (kerneltype : Int) (width : Float) (sigmoidscale : Float) (polynomialoffset : Float) (polynomialdegree : Int) (sigmoidoffset : Float) (modelmode : Int) (inputs : DictArray) (outputs : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::ml_regressionlinearcore" has_rundata]
opaque sop_ml_regressionlinearcore (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (batch : Int) (weightdecay : Float) (errorthreshold : Float) (inputs : DictArray) (outputs : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::ml_regressionproximitycore" has_rundata]
opaque sop_ml_regressionproximitycore (geo0 : Geometry) (geo1 : Geometry) (batch : Int) (inputs : DictArray) (outputs : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::name" has_rundata]
opaque sop_name (geo0 : Geometry) (attribname : String) (class' : Int) (donamefromgroup : Int) (namefromgroupmask : String) (numnames : DictArray) (numrenames : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::neighborsearchcl" has_rundata]
opaque sop_neighborsearchcl (geo0 : Geometry) (radiusscale : Float) (srcphase : Int) (dstphase : Int) (ignorepiece : Int) (leaveongpu : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::normal" has_rundata]
opaque sop_normal (geo0 : Geometry) (group : String) (grouptype : Int) (overridenormal : Int) (normalattrib : String) (docompute : Int) (type : Int) (cuspangle : Float) (method : Int) (origifzero : Int) (normalize : Int) (reverse : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::npspreprocessingcl" has_rundata]
opaque sop_npspreprocessingcl (geo0 : Geometry) (geo1 : Geometry) (dx : Float) (radiusscale : Float) (kerneloptions : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::null" has_rundata]
opaque sop_null (geo0 : Geometry) (copyinput : Int) (cacheinput : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::onnx" has_rundata]
opaque sop_onnx (geo0 : Geometry) (modelfile : String) (domaxbatch : Int) (maxbatch : Int) (provider : Int) (keepinput : Int) (inputs : DictArray) (outputs : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::opencl" has_rundata]
opaque sop_opencl (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (geo3 : Geometry) (kernelname : String) (usecode : Int) (kernelfile : String) (kernelcode : String) (atbinding : Int) (kerneloptions : String) (kerneloptionattrib : String) (usewritebackkernel : Int) (writebackkernelname : String) (recompile : Int) (runover : Int) (iterations : Int) (iteration : Int) (worksets_begin : String) (worksets_length : String) (singleworkgroup : Int) (finish : Int) (time : Int) (timeinc : Int) (timescale : Float) (timemethod : Int) (xnoise : Int) (precision : Int) (importprequel : Int) (bindings : DictArray) (generatedcode : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::orientalongcurve" has_rundata]
opaque sop_orientalongcurve (geo0 : Geometry) (geo1 : Geometry) (group : String) (tangenttype : Int) (continuousclosed : Int) (extrapolateendtangents : Int) (transformbyattribs : Int) (upvectortype : Int) (upvectoratstart : Int) (useendupvector : Int) (upvectorattrib : String) (endupvectorattrib : String) (upvector : Vector3) (endupvector : Vector3) (adjustupcurvature : Int) (curvaturescale : Float) (enablecurvaturescaleattrib : Int) (curvaturescaleattrib : String) (enablecurvatureattrib : Int) (curvatureattrib : String) (rOrd : Int) (applyroll : Int) (roll : Float) (rollper : Int) (fulltwists : Int) (incroll : Float) (rollattrib : String) (applyyaw : Int) (yaw : Float) (yawper : Int) (incyaw : Float) (yawattrib : String) (applypitch : Int) (pitch : Float) (pitchper : Int) (incpitch : Float) (pitchattrib : String) (normalize : Int) (scale : Float) (stretcharoundturns : Int) (maxstretcharoundturns : Float) (class' : Int) (outputxaxis : Int) (xaxisname : String) (outputyaxis : Int) (yaxisname : String) (outputzaxis : Int) (zaxisname : String) (outputtranslation : Int) (translationname : String) (outputquaternion : Int) (quaternionname : String) (outputtransform3 : Int) (transform3name : String) (outputtransform4 : Int) (transform4name : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::output" has_rundata]
opaque sop_output (geo0 : Geometry) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::pack" has_rundata]
opaque sop_pack (geo0 : Geometry) (viewportlod : String) (createpath : Int) (path : String) (packbyname : Int) (nameattribute : String) (packedfragments : Int) (pivot : Int) (transfer_attributes : String) (transfer_groups : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::packededit" has_rundata]
opaque sop_packededit (geo0 : Geometry) (vopcount : DictArray) (mopcount : DictArray) (lopcount : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::packfolder" has_rundata]
opaque sop_packfolder (geo0 : Geometry) (auxgeo : GeometryArray) (folder : String) (method : Int) (pack : Int) (onlypackunpacked : Int) (ignoreempty : Int) (names : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::packinject" has_rundata]
opaque sop_packinject (geo0 : Geometry) (geo1 : Geometry) (group : String) (method : Int) (collate : Int) (source : Int) (matchattrib : String) (srcattrib : String) (ignoremissing : Int) (preservetype : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::packpoints" has_rundata]
opaque sop_packpoints (geo0 : Geometry) (group : String) (tilesize : Float) (viewportlod : Int) (createpath : Int) (path : String) (pivot : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::pca" has_rundata]
opaque sop_pca (geo0 : Geometry) (geo1 : Geometry) (datatype : Int) (attribs : String) (stride : Int) (mode : Int) (includemeanweight : Int) (skip : Int) (comp : Int) (xscale : Float) (yscale : Float) (propcolor : Vector3) (cumcolor : Vector3) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::peak" has_rundata]
opaque sop_peak (geo0 : Geometry) (group : String) (grouptype : Int) (mask : Float) (enablemaskattrib : Int) (maskattrib : String) (usecustomattrib : Int) (customattrib : String) (normalizeattrib : Int) (dist : Float) (updatenmls : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::pointcapturecore" has_rundata]
opaque sop_pointcapturecore (geo0 : Geometry) (geo1 : Geometry) (group : String) (captureradius : Float) (kerneltype : Int) (smoothingmethod : Int) (smoothinglevel : Int) (tetmeshtreatment : Int) (outputcorrespondence : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::pointcloudnormal" has_rundata]
opaque sop_pointcloudnormal (geo0 : Geometry) (group : String) (normalattrib : String) (compute : Int) (radius : Float) (useneighbors : Int) (neighbors : Int) (origifzero : Int) (method : Int) (localneighbors : Int) (hint : Vector3) (reverse : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::pointcloudreduce" has_rundata]
opaque sop_pointcloudreduce (geo0 : Geometry) (geo1 : Geometry) (group : String) (voxelsize : Float) (samplemethod : Int) (seed : Float) (usepopulation : Int) (population : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::pointcloudsurface" has_rundata]
opaque sop_pointcloudsurface (geo0 : Geometry) (subdivisions : Int) (sampletarget : Float) (iterations : Int) (boundary : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::pointgenerate" has_rundata]
opaque sop_pointgenerate (geo0 : Geometry) (group : String) (keep : Int) (ptsperpt : Int) (npts : Int) (nptsperpt : Float) (doattrib : Int) (attrib : String) (seed : Float) (dogroup : Int) (ggroup : String) (dopointnum : Int) (spointnum : String) (dopointidx : Int) (spointidx : String) (docopyattribs : Int) (attribstocopy : String) (detailstocopy : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::polybevel::3.0" has_rundata]
opaque sop_polybevel_3_0 (geo0 : Geometry) (geo1 : Geometry) (group : String) (grouptype : Int) (ignorebridgededges : Int) (ignoresharededges : Int) (ignoreflatedges : Int) (ignoreflatpoints : Int) (ignoreinlinepoints : Int) (flatangle : Float) (offset : Float) (useoffsetscale : Int) (pointscaleattr : String) (sliding : Int) (asymtol : Float) (slideedges : String) (detectcollisions : Int) (restrictslides : Int) (limit : Int) (stopatslideend : Int) (stopatpinches : Int) (pinchangle : Float) (stopatcollisions : Int) (filletshape : Int) (convexity : Float) (profilesource : Int) (profilescale : Float) (reverseprofile : Int) (symmetrizeprofile : Int) (profiledirection : String) (profileramp : FloatRamp) (flatboost : Float) (useptfilletgroup : Int) (ptfilletgroup : String) (useptfilletedges : Int) (ptfilletedges : String) (useedgefilletgroup : Int) (edgeprims : String) (useoffsetedges : Int) (offsetedges : String) (useoffsetpoints : Int) (offsetpoints : String) (usemergedpoints : Int) (mergedpoints : String) (profilesampling : Int) (divisions : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::polycut" has_rundata]
opaque sop_polycut (geo0 : Geometry) (polygons : String) (type : Int) (cutpoints : String) (cutedges : String) (strategy : Int) (detectedgechanges : Int) (cutattrib : String) (cutvalue : Float) (cutstringvalue : String) (cutthreshold : Float) (keepclosed : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::polydoctor" has_rundata]
opaque sop_polydoctor (geo0 : Geometry) (group : String) (maxpasses : Int) (randomseed : Int) (illformed : Int) (manyedges : Int) (nonconvex : Int) (overlapping : Int) (pairoverlaps : Int) (intersect : Int) (thickness : Float) (glosmallarea : Int) (gloareathres : Float) (locsmallarea : Int) (locareathres : Float) (nonuni : Int) (unithres : Float) (glosmalledge : Int) (gloedgethres : Float) (locsmalledge : Int) (locedgethres : Float) (disconnectpt : Int) (nonmanifoldpt : Int) (ignorewindings : Int) (preferlargeangles : Int) (vismaxmanifold : Int) (exportmanifoldnumbers : Int) (fixwindings : Int) (deletesmallmanifolds : Int) (smallmanifoldsize : Int) (usevalidpoly : Int) (validpoly : String) (userepairedpoly : Int) (repairedpoly : String) (usevalidpts : Int) (validpts : String) (usemodifiedpts : Int) (modifiedpts : String) (creategrps : Int) (visinvalidpts : Int) (visinvalidptsfg : Vector3) (visrepairedpts : Int) (visrepairedptsfg : Vector3) (visinvalidpolys : Int) (visinvalidpolysfg : Vector3) (visrepairedpolys : Int) (visrepairedpolysfg : Vector3) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::polyexpand2d" has_rundata]
opaque sop_polyexpand2d (geo0 : Geometry) (group : String) (planepossrc : Int) (planeorigin : Vector3) (planedist : Float) (planenormal : Vector3) (output : Int) (offset : Float) (divs : Int) (sidedetermination : Int) (outputinside : Int) (outputoutside : Int) (keepinput : Int) (omitendcaps : Int) (uselocalinsidescale : Int) (localinsidescale : String) (uselocaloutsidescale : Int) (localoutsidescale : String) (newg : Int) (insidegroup : String) (outsidegroup : String) (doedgedistattrib : Int) (edgedistattrib : String) (doedgespeedattrib : Int) (edgespeedattrib : String) (coincidencetol : Float) (parallelismtol : Float) (skeletonfailure : Int) (cacheskeleton : Int) (updatenmls : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::polyextrude::2.0" has_rundata]
opaque sop_polyextrude_2_0 (geo0 : Geometry) (geo1 : Geometry) (group : String) (splittype : Int) (usesplitgroup : Int) (splitgroup : String) (extrusionmode : Int) (ptnormalsrc : Int) (ptnormalattrib : String) (dist : Float) (inset : Float) (twist : Float) (divs : Int) (spinetype : Int) (xformfront : Int) (xformspace : Int) (rst : Int) (xyz : Int) (translate : Vector3) (rotate : Vector3) (scale : Vector3) (shear : Vector3) (pivot : Vector3) (pivotrotate : Vector3) (prexform_rst : Int) (prexform_xyz : Int) (prexform_translate : Vector3) (prexform_rotate : Vector3) (prexform_scale : Vector3) (prexform_shear : Vector3) (outputfront : Int) (outputfrontgrp : Int) (frontgrp : String) (outputback : Int) (outputbackgrp : Int) (backgrp : String) (outputside : Int) (outputsidegrp : Int) (sidegrp : String) (outputfrontseamgrp : Int) (frontseamgrp : String) (outputbackseamgrp : Int) (backseamgrp : String) (preservegroups : Int) (limitinset : Int) (commonlimit : Int) (addvertexnomrals : Int) (cuspangle : Float) (cuspfront : Int) (cuspback : Int) (genuvs : Int) (uvstyle : Int) (uvscaling : Int) (frontmagnitude : Float) (backmagnitude : Float) (frontstiffness : Float) (backstiffness : Float) (interpolation : Int) (spacing : Int) (reversespinedirection : Int) (axialrotation : Float) (frontblend : Float) (backblend : Float) (thicknessscale : Float) (usethicknessattrib : Int) (thicknessattrib : String) (usethicknessramp : Int) (thicknessramp : FloatRamp) (usetwistattrib : Int) (twistattrib : String) (usetwistramp : Int) (twistramp : FloatRamp) (twistscale : Float) (uselocalzscaleattrib : Int) (localzscaleattrib : String) (uselocalinsetscaleattrib : Int) (localinsetscaleattrib : String) (uselocaltwistattrib : Int) (localtwistscaleattrib : String) (uselocaldivsattrib : Int) (locadivscaleattrib : String) (uselocalxattrib : Int) (localxattrib : String) (uselocalzattirb : Int) (localzattirb : String) (uselocalctrattrib : Int) (localctrattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::polyfill" has_rundata]
opaque sop_polyfill (geo0 : Geometry) (group : String) (fillmode : Int) (completeloops : Int) (reverse : Int) (uniquepoints : Int) (updatenorms : Int) (looptoggle : Int) (loopdistance : Float) (smoothtoggle : Int) (smoothstrength : Float) (corneroffset : Int) (customcornerstoggle : Int) (customcorners : String) (subdivtoggle : Int) (translate : Float) (tangentstrength : Float) (patchgrouptoggle : Int) (patchgroup : String) (loopgrouptoggle : Int) (loopgroup : String) (groupappend : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::polyframe" has_rundata]
opaque sop_polyframe (geo0 : Geometry) (group : String) (entity : Int) (style : String) (attribname : String) (Non : Int) (N : String) (tangentuon : Int) (tangentu : String) (tangentvon : Int) (tangentv : String) (signson : Int) (signs : String) (ortho : Int) (lefthanded : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::polypatch" has_rundata]
opaque sop_polypatch (geo0 : Geometry) (group : String) (basis : Int) (connecttype : Int) (closeu : Int) (closev : Int) (firstuclamp : Int) (lastuclamp : Int) (firstvclamp : Int) (lastvclamp : Int) (divisions : Vector2) (polys : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::polyreduce::2.0" has_rundata]
opaque sop_polyreduce_2_0 (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (group : String) (target : Int) (percentage : Float) (finalcount : Int) (reducepassedtarget : Int) (qualitytolerance : Float) (usenormaldevthreshold : Int) (normaldevthreshold : Float) (originalpoints : Int) (preservequads : Int) (equalizelengths : Float) (boundaryweight : Float) (vattribseamweight : Float) (seamattribs : String) (hardfeaturepoints : String) (hardfeatureedges : String) (softfeaturepoints : String) (softfeaturepointweight : Float) (softfeatureedges : String) (softfeatureedgeweight : Float) (useretainattrib : Int) (retainattrib : String) (retainattribweight : Float) (silhouetteweight : Float) (usesilhouettefalloff : Int) (silhouettefalloffdist : Float) (frontfacingweight : Float) (usefrontfacingfalloff : Int) (frontfacingfalloffdist : Float) (controlattribs : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::polysoup" has_rundata]
opaque sop_polysoup (geo0 : Geometry) (group : String) (ignoreattribs : Int) (ignoregroups : Int) (minpolys : Int) (convex : Int) (usemaxsides : Int) (maxsides : Int) (mergeverts : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::polysplit::2.0" has_rundata]
opaque sop_polysplit_2_0 (geo0 : Geometry) (geo1 : Geometry) (splitloc : String) (pathtype : Int) (close : Int) (allowfaces : Int) (quadcomplete : Int) (numloops : Int) (parallellooptoggle : Int) (parallelfliptoggle : Int) (edgepercenttoggle : Int) (edgepercent : Float) (updatenorms : Int) (grouptoggle : Int) (groupname : String) (groupappend : Int) (groupexclude : Int) (tolerance : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::polywire" has_rundata]
opaque sop_polywire (geo0 : Geometry) (group : String) (radius : Float) (usescaleattrib : Int) (scaleattrib : String) (jointcorrect : Int) (maxscale : Float) (smooth : Int) (usesmoothattrib : Int) (smoothattrib : String) (maxvalence : Int) (div : Int) (usedivattrib : Int) (divattrib : String) (segs : Int) (usesegsattrib : Int) (segsattrib : String) (usesegscale : Int) (segscale : Vector2) (dotexture : Int) (uoff : Float) (textu : Vector2) (usetextvattrib : Int) (textvattrib : String) (textv : Vector2) (upenable : Int) (upvector : Vector3) (upattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::primitive" has_rundata]
opaque sop_primitive (geo0 : Geometry) (geo1 : Geometry) (group : String) (doxform : Int) (dorot : Int) (xOrd : Int) (rOrd : Int) (t : Vector3) (r : Vector3) (s : Vector3) (shear : Vector3) (p : Vector3) (pr : Vector3) (lookatpath : String) (upvector : Vector3) (xformattribs : String) (doclr : Int) (diff : Vector3) (doalpha : Int) (alpha : Float) (docrease : Int) (crease : Float) (dotexture : Int) (texture : String) (closeu : Int) (closev : Int) (clampu : Int) (clampv : Int) (vtxsort : Int) (vtxuoff : Int) (vtxvoff : Int) (doweight : Int) (metaweight : Float) (doprender : Int) (prtype : Int) (prsize : Float) (prblur : Float) (dovolvis : Int) (volvis : Int) (volvisiso : Float) (volvisdensity : Float) (dotaper : Int) (taper : Vector2) (dovolume : Int) (volborder : Int) (volborderval : Float) (dovoltol : Int) (voltol : Float) (dovoltypeinfo : Int) (voltypeinfo : Int) (dovistile : Int) (vistile : Float) (dovdbclass : Int) (vdbclass : Int) (dovdbcreator : Int) (vdbcreator : String) (dovdbtransform : Int) (vdbtransform : Int) (dovdbvectype : Int) (vdbvectype : Int) (dovdbhalf : Int) (vdbhalf : Int) (templateGrp : String) (pshapeu : Int) (pshapev : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::primitivesplit" has_rundata]
opaque sop_primitivesplit (geo0 : Geometry) (group : String) (attribname : String) (tol : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::pythonsnippet" has_rundata]
opaque sop_pythonsnippet (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (geo3 : Geometry) (pythoncode : String) (options_maintainstate : Int) (bindings : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::quadremesh" has_rundata]
opaque sop_quadremesh (geo0 : Geometry) (geo1 : Geometry) (group : String) (decimation : Int) (decimationlevel : Int) (output : Int) (resolution : Int) (targetquadcount : Int) (targetquadarea : Float) (targettolerance : Float) (resolutionscale : Int) (resolutionsource : Int) (trianglearea : Float) (symmetrycenter : Vector3) (xaxis : Int) (xdirection : Int) (yaxis : Int) (ydirection : Int) (zaxis : Int) (zdirection : Int) (mirroroutput : Int) (enableadaptivity : Int) (adaptivityweight : Float) (adaptivitymaskattrib : String) (adaptivitysizing : Int) (adaptivitysizingattrib : String) (featureboundaries : Int) (field : Int) (globalweight : Float) (globalmask : String) (curvature : Int) (localcurvatureweight : Float) (curvaturemaskattrib : String) (curvaturerotation : Float) (boundary : Int) (boundarymode : Int) (localboundaryweight : Float) (boundarymaskattrib : String) (boundaryrotation : Float) (guide : Int) (guidemaskattrib : String) (guideattrib : String) (adaptivitysizingweight : Float) (guidemode : Int) (localguideweight : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::rawimport" has_rundata]
opaque sop_rawimport (file : String) (endian : Int) (pointcounttype : Int) (pointcount : Int) (pointcountattrib : String) (blocks : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::ray" has_rundata]
opaque sop_ray (geo0 : Geometry) (geo1 : Geometry) (group : String) (entity : Int) (collision : String) (method : Int) (dirmethod : Int) (dir : Vector3) (dirattrib : String) (reverserays : Int) (bidirectionalresult : Int) (dotrans : Int) (showguide : Int) (lookfar : Int) (rtolerance : Float) (scale : Float) (lift : Float) (bias : Float) (maxraydistcheck : Int) (maxraydist : Float) (sample : Int) (jitter : Float) (seed : Int) (combinetype : Int) (putnml : Int) (putdist : Int) (newgrp : Int) (hitgrp : String) (useprimnumattrib : Int) (primnumattrib : String) (useprimuvwattrib : Int) (primuvwattrib : String) (getptattribs : Int) (ptattribnames : String) (vertexattribnames : String) (primitiveattribnames : String) (detailattribnames : String) (gethitgroups : Int) (ptgroupnames : String) (vertexgroupnames : String) (primitivegroupnames : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::refine" has_rundata]
opaque sop_refine (geo0 : Geometry) (group : String) (firstu : Int) (domainu1 : Float) (secondu : Int) (domainu2 : Float) (firstv : Int) (domainv1 : Float) (secondv : Int) (domainv2 : Float) (refineu : Int) (refinev : Int) (refinespace : Int) (stdswitcher : Int) (unrefineu : Int) (unrefinev : Int) (tolu : Float) (tolv : Float) (subdivspace : Int) (divsu : Int) (divsv : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::relax" has_rundata]
opaque sop_relax (geo0 : Geometry) (geo1 : Geometry) (group : String) (influencegroup : String) (surfacegroup : String) (maxiterations : Int) (radius : Float) (useradiusattrib : Int) (radiusattrib : String) (relaxin3d : Int) (usenormals : Int) (usegeometricnormals : Int) (normal : Vector3) (usemaxstepattrib : Int) (maxstepattrib : String) (useprimnumattrib : Int) (primnumattrib : String) (useprimuvwattrib : Int) (primuvwattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::remesh" has_rundata]
opaque sop_remesh (geo0 : Geometry) (geo1 : Geometry) (group : String) (hard_edges : String) (iterations : Int) (recompute_normals : Int) (smoothing_level : Float) (target_edge : Float) (input_pts_only : Int) (element_sizing : Int) (gradation : Float) (density : Float) (limit_below : Int) (min_edge_length : Float) (limit_above : Int) (max_edge_length : Float) (lfs_only : Int) (visualize_lfs : Int) (lfs_ramp : ColorRamp) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::remesh::2.0" has_rundata]
opaque sop_remesh_2_0 (geo0 : Geometry) (geo1 : Geometry) (group : String) (hard_edges : String) (hard_points : String) (hardenuvseams : Int) (uvattriv : String) (iterations : Int) (smoothing : Float) (inputptsonly : Int) (detachfromnongroup : Int) (recomputenormals : Int) (sizing : Int) (targetsize : Float) (usemaxsize : Int) (maxsize : Float) (useminsize : Int) (minsize : Float) (density : Float) (gradation : Float) (usemeshsizeattrib : Int) (meshsizeattrib : String) (useminsizeattrib : Int) (minsizeattrib : String) (usemaxsizeattrib : Int) (maxsizeattrib : String) (useouthardedgesgroup : Int) (outhardedgesgroup : String) (useoutmeshsizeattrib : Int) (outmeshsizeattrib : String) (useoutmeshqualityattrib : Int) (outmeshqualityattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::repack" has_rundata]
opaque sop_repack (geo0 : Geometry) (group : String) (packedfragments : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::resample" has_rundata]
opaque sop_resample (geo0 : Geometry) (group : String) (maintainprimorder : Int) (lod : Float) (edge : Int) (method : Int) (measure : Int) (dolength : Int) (length : Float) (dosegs : Int) (segs : Int) (useattribs : Int) (allequal : Int) (last : Int) (randomshift : Int) (onlypoints : Int) (treatpolysas : Int) (outputsubdpoly : Int) (doptdistattr : Int) (ptdistattr : String) (dotangentattr : Int) (tangentattr : String) (docurveuattr : Int) (curveuattr : String) (docurvenumattr : Int) (curvenumattr : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::rest" has_rundata]
opaque sop_rest (geo0 : Geometry) (geo1 : Geometry) (mode : Int) (restattribname : String) (file : String) (nml : Int) (normalattribname : String) (xform : Int) (xformattribname : String) (quadric : Int) (quadricattribname : String) (precision : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::reverse" has_rundata]
opaque sop_reverse (geo0 : Geometry) (group : String) (vtxsort : Int) (vtxuoff : Int) (vtxvoff : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::revolve" has_rundata]
opaque sop_revolve (geo0 : Geometry) (group : String) (surftype : Int) (origin : Vector3) (dir : Vector3) (polys : Int) (imperfect : Int) (type : Int) (angle : Vector2) (divs : Int) (order : Int) (cap : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::rewire" has_rundata]
opaque sop_rewire (geo0 : Geometry) (group : String) (grouptype : Int) (attrib : String) (deletetargetpointattrib : Int) (recurse : Int) (keepunusedpoints : Int) (createorigattrib : Int) (origattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::sblend" has_rundata]
opaque sop_sblend (geo0 : Geometry) (auxgeo : GeometryArray) (blend : Float) (dopos : Int) (doclr : Int) (donml : Int) (douvw : Int) (dovoxel : Int) (doslerp : Int) (ptidattr : String) (primidattr : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::sblend::2.0" has_rundata]
opaque sop_sblend_2_0 (geo0 : Geometry) (auxgeo : GeometryArray) (blend : Float) (attribs : String) (ptidattr : String) (unmatchedpts : Int) (unmatchedgroup : String) (primidattr : String) (voxelblend : Int) (adt : Float) (velocity : String) (doslerp : Int) (interp : Int) (usevforpinterp : Int) (timestep : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::scatter::2.0" has_rundata]
opaque sop_scatter_2_0 (geo0 : Geometry) (group : String) (generateby : Int) (indepvoxel : Int) (forcetotal : Int) (npts : Int) (densityscale : Float) (usedensityattrib : Int) (densityattrib : String) (useareaattrib : Int) (areaattrib : String) (useareaforvolumes : Int) (usedensitytexture : Int) (densitytexture : String) (uvattrib : String) (primcountattrib : String) (useemergencylimit : Int) (emergencylimit : Int) (seed : Float) (overrideprimseed : Int) (primseedattrib : String) (randomizeorder : Int) (relaxpoints : Int) (relaxiterations : Int) (usegeometricnormals : Int) (useprimnumattrib : Int) (primnumattrib : String) (useprimuvwattrib : Int) (primuvwattrib : String) (useoutputdensityattrib : Int) (outputdensityattrib : String) (useoutputradiusattrib : Int) (outputradiusattrib : String) (useoutputidattrib : Int) (outputidattrib : String) (radiusintexturespace : Int) (pointattribs : String) (vertattribs : String) (primattribs : String) (detailattribs : String) (detailattribsasdetail : String) (scaleradiiby : Float) (usemaxradius : Int) (maxradius : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::shapediff" has_rundata]
opaque sop_shapediff (geo0 : Geometry) (geo1 : Geometry) (difftype : Int) (orientattrib : String) (skelrootpath : String) (bonetransformpath : String) (skintype : Int) (blendattrib : String) (donormal : Int) (dovattribs : Int) (vattribs : String) (doqattribs : Int) (qattribs : String) (maskattrib : String) (scalemask : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::shrinkwrap::2.0" has_rundata]
opaque sop_shrinkwrap_2_0 (geo0 : Geometry) (group : String) (type : Int) (planesrc : Int) (planeorigin : Vector3) (planedist : Float) (planenormal : Vector3) (shrinkamount : Float) (preserveattribs : Int) (removeinlinepoints : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::simplexrefine" has_rundata]
opaque sop_simplexrefine (geo0 : Geometry) (group : String) (smoothingmethod : Int) (smoothinglevel : Int) (tetmeshtreatment : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::skin" has_rundata]
opaque sop_skin (geo0 : Geometry) (geo1 : Geometry) (uprims : String) (vprims : String) (surftype : Int) (keepshape : Int) (closev : Int) (force : Int) (orderv : Int) (skinops : Int) (inc : Int) (prim : Int) (polys : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::smooth::2.0" has_rundata]
opaque sop_smooth_2_0 (geo0 : Geometry) (group : String) (contrainedboundary : Int) (constrainedpoints : String) (attributes : String) (useweightattribute : Int) (weightattribute : String) (method : Int) (strength : Float) (filterquality : Int) (updateaffectednmls : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::softpeak" has_rundata]
opaque sop_softpeak (geo0 : Geometry) (group : String) (mask : Float) (enablemaskattrib : Int) (maskattrib : String) (dist : Float) (distmetric : Int) (applyrolloff : Int) (distattr : String) (rad : Float) (type : Int) (tandeg : Vector2) (kernel : String) (leadnml : Int) (updatenmls : Int) (updateaffectednmls : Int) (visualizefalloff : Int) (leadptattr : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::softxform" has_rundata]
opaque sop_softxform (geo0 : Geometry) (group : String) (xOrd : Int) (rOrd : Int) (t : Vector3) (r : Vector3) (s : Vector3) (shear : Vector3) (p : Vector3) (pr : Vector3) (distmetric : Int) (applyrolloff : Int) (distattr : String) (rad : Float) (type : Int) (tandeg : Vector2) (kernel : String) (attribs : String) (updatenmls : Int) (updateaffectednmls : Int) (vlength : Int) (visualizefalloff : Int) (localspace : Int) (upvector : Vector3) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::solidify" has_rundata]
opaque sop_solidify (geo0 : Geometry) (tetgroup : String) (polygroup : String) (keeppolygons : Int) (solidbdry : Float) (outputsolidity : Int) (solidityattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::sort" has_rundata]
opaque sop_sort (geo0 : Geometry) (ptgroup : String) (ptsort : Int) (pointseed : Int) (pointoffset : Int) (pointprox : Vector3) (pointobjpath : String) (pointdir : Vector3) (pointexpr : Float) (pointattrib : String) (pointattribcomp : Int) (pointorder : String) (pointreverse : Int) (usepointindices : Int) (pointindices : String) (combinepointindices : Int) (primgroup : String) (primsort : Int) (primseed : Int) (primoffset : Int) (primprox : Vector3) (primobjpath : String) (primdir : Vector3) (primexpr : Float) (primattrib : String) (primattribcomp : Int) (primorder : String) (primreverse : Int) (useprimindices : Int) (primindices : String) (combineprimindices : Int) (vertexprimorder : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::sphere" has_rundata]
opaque sop_sphere (geo0 : Geometry) (type : Int) (surftype : Int) (rad : Vector3) (t : Vector3) (r : Vector3) (scale : Float) (orient : Int) (freq : Int) (orderu : Int) (orderv : Int) (imperfect : Int) (upole : Int) (accurate : Int) (triangularpoles : Int) (rows : Int) (cols : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::splitpoints" has_rundata]
opaque sop_splitpoints (geo0 : Geometry) (group : String) (grouptype : Int) (useattrib : Int) (attribname : String) (tol : Float) (promote : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::stash" has_rundata]
opaque sop_stash (geo0 : Geometry) (stash : DataItem) (stashfile : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::subdivide" has_rundata]
opaque sop_subdivide (geo0 : Geometry) (geo1 : Geometry) (subdivide : String) (creases : String) (iterations : Int) (overridecrease : Int) (creaseweight : Float) (outputcrease : Int) (outcreasegroup : String) (closeholes : Int) (surroundpoly : Int) (bias : Float) (smoothvertex : Int) (consisttopology : Int) (linearcreases : Int) (algorithm : Int) (buildpolysoups : Int) (indepcurves : Int) (updatenmls : Int) (removeholes : Int) (vtxboundary : String) (fvarlinear : String) (creasemethod : String) (trianglesubd : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::surfacesplat" has_rundata]
opaque sop_surfacesplat (geo0 : Geometry) (geo1 : Geometry) (bind_mask : String) (negatemask : Int) (bind_width : String) (bind_alpha : String) (bind_softedge : String) (bind_hit : String) (bind_hitprim : String) (bind_hituv : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::sweep::2.0" has_rundata]
opaque sop_sweep_2_0 (geo0 : Geometry) (geo1 : Geometry) (curvegroup : String) (crosssectiongroup : String) (surfaceshape : Int) (surfacetype : Int) (scale : Float) (cols : Int) (radius : Float) (width : Float) (reversecrosssections : Int) (stretcharoundturns : Int) (maxstretcharoundturns : Float) (endcaptype : Int) (capdivs : Int) (triangularpoles : Int) (capscale : Float) (caproundness : Float) (addendcapsgroup : Int) (endcapsgroup : String) (applyscale : Int) (scaleramp : FloatRamp) (rOrd : Int) (applyroll : Int) (roll : Float) (fulltwists : Int) (incroll : Float) (rollper : Int) (rollattrib : String) (applyyaw : Int) (yaw : Float) (incyaw : Float) (yawper : Int) (yawattrib : String) (applypitch : Int) (pitch : Float) (incpitch : Float) (pitchper : Int) (pitchattrib : String) (copyorder : Int) (crosssectionattrib : String) (primtype : Int) (unrollclosedrowcol : Int) (swaprowcol : Int) (closeifnocurveinput : Int) (tangenttype : Int) (continuousclosed : Int) (extrapolateendtangents : Int) (transformbyattribs : Int) (computeuvs : Int) (overrideexistinguvs : Int) (lengthweighteduvs : Int) (normalizeu : Int) (normalizev : Int) (flipu : Int) (uvscale : Vector2) (usemeshedgelengths : Int) (propscalepercurve : Int) (wrapu : Int) (wrapv : Int) (attribsfrombackbone : String) (attribsfromcrosssection : String) (addptrow : Int) (ptrowattrib : String) (addptcol : Int) (ptcolattrib : String) (addprimrow : Int) (primrowattrib : String) (addprimcol : Int) (primcolattrib : String) (addcrosssectionnum : Int) (crosssectionnumattrib : String) (addcurvenum : Int) (curvenumattrib : String) (upvectortype : Int) (upvectoratstart : Int) (useendupvector : Int) (upvectorattrib : String) (endupvectorattrib : String) (upvector : Vector3) (endupvector : Vector3) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::switch" has_rundata]
opaque sop_switch (geo0 : Geometry) (auxgeo : GeometryArray) (input : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::switchif" has_rundata]
opaque sop_switchif (geo0 : Geometry) (geo1 : Geometry) (mergecondition : Int) (testinput : Int) (tests : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::tangentfield" has_rundata]
opaque sop_tangentfield (geo0 : Geometry) (group : String) (carrier : Int) (directions : Int) (rotation : Float) (globalweight : Float) (globalmask : String) (curvature : Int) (localcurvatureweight : Float) (curvaturemaskattrib : String) (curvaturerotation : Float) (boundary : Int) (boundarymode : Int) (localboundaryweight : Float) (boundarymaskattrib : String) (boundaryrotation : Float) (guide : Int) (guidemode : Int) (localguideweight : Float) (guidemaskattrib : String) (guideattrib : String) (anisotropyweight : Float) (anisotropymask : String) (anisotropycurvature : Int) (anisotropycurvatureweight : Float) (anisotropycurvaturemask : String) (anisotropyguide : Int) (anisotropyguideweight : Float) (anisotropyguidemask : String) (anisotropyguideattrib : String) (anisotropysizing : Int) (anisotropysizingweight : Float) (anisotropysizingmask : String) (anisotropysizingattrib : String) (fieldattrib : String) (normalizefield : Int) (outputmode : Int) (usesingulargroup : Int) (singulargroup : String) (usepositivesingulargroup : Int) (positivesingulargroup : String) (usenegativesingulargroup : Int) (negativesingulargroup : String) (usediscontinuitiesgroup : Int) (discontinuities : String) (showfield : Int) (fieldscalemode : Int) (fieldscale : Float) (showsingularities : Int) (showguides : Int) (guidescalemode : Int) (guidescale : Float) (showanisotropy : Int) (anisotropyscale : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::tetcraft" has_rundata]
opaque sop_tetcraft (geo0 : Geometry) (group : String) (direction : Int) (createtets : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::tetlayer" has_rundata]
opaque sop_tetlayer (geo0 : Geometry) (group : String) (direction : Int) (thickness : Float) (enablethicknessmultiplierattribute : Int) (thicknessmultiplierattribute : String) (createboundarytriangles : Int) (createtets : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::tetpartition" has_rundata]
opaque sop_tetpartition (geo0 : Geometry) (geo1 : Geometry) (tetgroup : String) (polygroup : String) (attrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::tetrahedralize" has_rundata]
opaque sop_tetrahedralize (geo0 : Geometry) (geo1 : Geometry) (group : String) (batch : Int) (pieceattrib : String) (remove : Int) (mode : Int) (output : Int) (keepprims : Int) (noboundmod : Int) (onefacepertet : Int) (propnormal : Int) (internattrib : Int) (usequality : Int) (radedgetol : Float) (mindihedralang : Float) (usetargetsizeattrib : Int) (targetsizeattrib : String) (useuniformmaxsize : Int) (uniformmaxsize : Float) (usemaxsizeattrib : Int) (maxsizeattrib : String) (usemaxiter : Int) (maxiter : Int) (usemaxsteiner : Int) (maxsteiner : Int) (optiterations : Int) (optedgeface : Int) (optvtxsmooth : Int) (optvtxmod : Int) (useisectcolor : Int) (isectpolyclr : Vector3) (useisectgrp : Int) (isectpolygrp : String) (failures : Int) (randomseed : Int) (precisiontol : Float) (dihedralangtol : Float) (maxattempts : Int) (useinvalidcolor : Int) (invalidprimclr : Vector3) (useinvalidgrp : Int) (invalidprimgrp : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::tetrasurface" has_rundata]
opaque sop_tetrasurface (geo0 : Geometry) (keepprimitives : Int) (keeppoints : Int) (buildpolysoup : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::texture" has_rundata]
opaque sop_texture (geo0 : Geometry) (uvattrib : String) (group : String) (type : Int) (axis : Int) (campath : String) (coord : Int) (s : Vector3) (offset : Vector3) (angle : Float) (fixseams : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::texturefeature" has_rundata]
opaque sop_texturefeature (geo0 : Geometry) (geo1 : Geometry) (group : String) (maxfeaturesize : Int) (minimumspacing : Int) (qualitytolerance : Float) (blurringwindowradius : Int) (gradientwindowradius : Int) (method : Int) (cornerweight : Float) (output : Int) (outputname : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::textureopticalflow" has_rundata]
opaque sop_textureopticalflow (geo0 : Geometry) (geo1 : Geometry) (sourcegroup : String) (goalgroup : String) (outputname : String) (method : Int) (blurringwindowradius : Int) (usegaussianfilter : Int) (pyramidlevels : Int) (pyramidscale : Float) (iterations : Int) (approximationwindowradius : Float) (smoothnessdegree : Int) (patchsize : Int) (patchstride : Int) (finestscale : Int) (gradientdescentiterations : Int) (smoothnessweight : Float) (colorconstancyweight : Float) (gradientconstancyweight : Float) (variationalrefinementiterations : Int) (usemeannormalization : Int) (usespatialpropagation : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::topotransfer" has_rundata]
opaque sop_topotransfer (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (geo3 : Geometry) (enablesolve : Int) (enablegeometryconstraints : Int) (constraintselection : Int) (constraintsource : String) (iterations : Int) (reducedlevels : Int) (reductionpercentage : Vector2) (reductionpercentage_single : Float) (initialreductionpercentage : Float) (rigidweights : Vector2) (landmarkweights : Vector2) (maskmode : Int) (masktolerance : Float) (rigidprimitives : String) (disttolerance : Float) (normtolerance : Float) (solvertype : Int) (solveriterations : Int) (gradienttolerance : Vector2) (debug_menu : Int) (debug_coarse_lvl : Int) (debug_coarse_iteration : Int) (debug_dense_lvl : Int) (debug_dense_iteration : Int) (debug_hessian_scaling : Int) (debug_use_marquadt : Int) (parameter_tolerance : Float) (use_tau : Int) (tau : Float) (initial_damping : Float) (debug_save_meshes : Int) (uselandmarklabels : Int) (landmarkattrib : String) (enablelandmarks : Int) (numlandmarkgroups : DictArray) (rigidmask : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::torus" has_rundata]
opaque sop_torus (type : Int) (surftype : Int) (orient : Int) (rad : Vector2) (t : Vector3) (r : Vector3) (scale : Float) (imperfect : Int) (orderu : Int) (orderv : Int) (closeu : Int) (closev : Int) (capu : Int) (capv : Int) (rows : Int) (cols : Int) (angleu : Vector2) (anglev : Vector2) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::trace" has_rundata]
opaque sop_trace (geo0 : Geometry) (tracelayer : String) (t : Vector3) (r : Vector3) (s : Vector2) (thresh : Float) (addtexture : Int) (channel : Int) (file : String) (overridesize : Int) (imagesize : Vector2) (usecop : Int) (coppath : String) (copframe : Float) (copcolor : String) (copalpha : String) (delborder : Int) (bordwidth : Int) (doresample : Int) (step : Float) (dosmooth : Int) (corner : Float) (fitcurve : Int) (error : Float) (convpoly : Int) (lod : Float) (hole : Int) (boundary : Int) (boundaryvalue : Float) (missingframe : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::triangulate2d::2.0" has_rundata]
opaque sop_triangulate2d_2_0 (geo0 : Geometry) (points : String) (planepossrc : Int) (origin : Vector3) (dist : Float) (dir : Vector3) (pos2attrib : String) (restorepos : Int) (keepprims : Int) (updatenmls : Int) (removeunusedpoints : Int) (removeduplicatepoints : Int) (randseed : Int) (conprims : String) (conedges : String) (removeconvhull : Int) (removeoutcons : Int) (dontremoveincons : Int) (refine : Int) (maxarea : Float) (minangle : Float) (minedgelength : Float) (maxnewpts : Int) (nonewconpts : Int) (makenewconptgrp : Int) (newconptgrp : String) (makenewconedgegrp : Int) (outconedgegrp : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::triangulate2d::3.0" has_rundata]
opaque sop_triangulate2d_3_0 (geo0 : Geometry) (points : String) (planepossrc : Int) (origin : Vector3) (dist : Float) (dir : Vector3) (pos2attrib : String) (useconstredges : Int) (constredges : String) (useconstrpolys : Int) (constrpolys : String) (ignorepolybridges : Int) (usesilhouettepolys : Int) (silhouettepolys : String) (allowconstrsplit : Int) (useexactconstruction : Int) (ignorenonconstrpts : Int) (removefromconvexhull : Int) (removefromconstrpolys : Int) (removeoutsidesilhouette : Int) (refine : Int) (allowrefineonstrsplit : Int) (encroachangle : Float) (minangle : Float) (trianglesize : Int) (maxarea : Float) (targetedgelength : Float) (minedgelength : Float) (maxnewpts : Int) (lloydsteps : Int) (allowmovinginteriorpts : Int) (restorepos : Int) (keepprims : Int) (updatenmls : Int) (removeunusedpoints : Int) (removeduplicatepoints : Int) (usecontrsplitptgrp : Int) (constrsplitptgrp : String) (useconstrdedges : Int) (constrdedges : String) (randseed : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::tribez" has_rundata]
opaque sop_tribez (geo0 : Geometry) (group : String) (order : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::tridivide" has_rundata]
opaque sop_tridivide (geo0 : Geometry) (geo1 : Geometry) (group : String) (convex : Int) (root3iter : Int) (dominarea : Int) (minarea : Float) (doprojmatrix : Int) (projmatrix : Matrix4) (doboundingbox : Int) (size : Vector3) (t : Vector3) (doexpand : Int) (dominedge : Int) (minedge : Float) (donumsplits : Int) (numsplits : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::tristrip" has_rundata]
opaque sop_tristrip (geo0 : Geometry) (group : String) (constrainstriplength : Int) (maxstriplength : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::tube" has_rundata]
opaque sop_tube (geo0 : Geometry) (type : Int) (surftype : Int) (orient : Int) (cap : Int) (consolidatepts : Int) (vertexnormals : Int) (t : Vector3) (r : Vector3) (rad : Vector2) (radscale : Float) (height : Float) (rows : Int) (orderu : Int) (orderv : Int) (imperfect : Int) (cols : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::unpack" has_rundata]
opaque sop_unpack (geo0 : Geometry) (group : String) (limit_iterations : Int) (iterations : Int) (detail_attributes : String) (transfer_attributes : String) (transfer_groups : String) (apply_style_sheets : Int) (scene_style_sheet : String) (obj_style_sheet : String) (dotransform : Int) (convertpolysoup : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::unpackfolder" has_rundata]
opaque sop_unpackfolder (geo0 : Geometry) (pattern : String) (unpack : Int) (invert : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::unpackpoints" has_rundata]
opaque sop_unpackpoints (geo0 : Geometry) (geo1 : Geometry) (group : String) (fineculling : Int) (usebbox : Int) (size : Vector3) (t : Vector3) (scale : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::unpackusd::2.0" has_rundata]
opaque sop_unpackusd_2_0 (geo0 : Geometry) (group : String) (deleteorig : Int) (unpacktraversal : String) (output : Int) (limititerations : Int) (iterations : Int) (pivot : Int) (addpathattrib : Int) (pathattrib : String) (addnameattrib : Int) (nameattrib : String) (addfilepathattrib : Int) (filepathattrib : String) (addinstancelevelattrib : Int) (instancelevelattrib : String) (transferattributes : String) (transfergroups : String) (importprimvars : String) (importinheritedprimvars : Int) (importcomputedvisibility : Int) (importattributes : String) (nontransformingprimvars : String) (translatesttouv : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::unsubdivide" has_rundata]
opaque sop_unsubdivide (geo0 : Geometry) (group : String) (usepieceattrib : Int) (pieceattrib : String) (algorithm : Int) (useiter : Int) (iterations : Int) (tol : Float) (allowpartialunsub : Int) (repairmesh : Int) (strategy : Int) (preferuniform : Int) (seed : Int) (useconnectivityattribute : Int) (connectivityattribute : String) (connectivitytol : Float) (useoutputdepthattribute : Int) (outputdepthattribute : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::uvautoseam" has_rundata]
opaque sop_uvautoseam (geo0 : Geometry) (group : String) (avoidattr : String) (avoidanceweight : Float) (basesplit : Int) (graintol : Float) (mergethreshold : Float) (uvattrib : String) (uvtolerance : Float) (seamsgroupname : String) (useoutputislandattr : Int) (outputislandattr : String) (preseams : String) (islandattr : String) (nonseams : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::uvflatten::2.0" has_rundata]
opaque sop_uvflatten_2_0 (geo0 : Geometry) (group : String) (uvattrib : String) (method : Int) (keepexistingseams : Int) (keepexistinglayout : Int) (pinboundaries : Int) (seamgroup : String) (usepins : Int) (pins : DictArray) (usealignments : Int) (alignments : DictArray) (usestraightenings : Int) (straightenings : DictArray) (manuallayout : Int) (layoutseamgroup : String) (lpins : DictArray) (lalignments : DictArray) (lstraightenings : DictArray) (useoutputseams : Int) (outputseams : String) (useoutputconstrislands : Int) (outputconstrislands : String) (bboxcenter : Vector2) (bboxsize : Vector2) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::uvflatten::3.0" has_rundata]
opaque sop_uvflatten_3_0 (geo0 : Geometry) (group : String) (uvattrib : String) (method : Int) (keepexistingseams : Int) (keepexistinglayout : Int) (pinboundaries : Int) (seamgroup : String) (rectifygroup : String) (usepins : Int) (pins : DictArray) (usealignments : Int) (alignments : DictArray) (usestraightenings : Int) (straightenings : DictArray) (manuallayout : Int) (layoutseamgroup : String) (layoutrectifygroup : String) (lpins : DictArray) (lalignments : DictArray) (lstraightenings : DictArray) (useoutputseams : Int) (outputseams : String) (useoutputconstrislands : Int) (outputconstrislands : String) (bboxcenter : Vector2) (bboxsize : Vector2) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::uvflattenfrompoints" has_rundata]
opaque sop_uvflattenfrompoints (geo0 : Geometry) (geo1 : Geometry) (group : String) (uvattrib : String) (method : Int) (centerpointsource : Int) (srcpoints : String) (automaticmethod : Int) (addautomaticsources : Int) (nautomaticsources : Int) (automaticdistortionthreshold : Float) (useup : Int) (up : String) (usescale : Int) (scale : String) (usepscale : Int) (pscale : String) (packingmethod : Int) (bboxcenter : Vector2) (bboxsize : Vector2) (useudimtarget : Int) (udimtarget : String) (uvcenter : Vector2) (usedistortionpruning : Int) (distortionthreshold : Float) (useoutputdistortedprims : Int) (distortedprims : String) (preserveexistinguvs : Int) (useoutputunprocessedprims : Int) (unprocessedprims : String) (useoutputseams : Int) (outputseams : String) (useoutputislandnumber : Int) (outputislandnumber : String) (usecopypointattribs : Int) (copypointattribs : String) (createuvsetperpoint : Int) (uvsetprefix : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::uvfuse" has_rundata]
opaque sop_uvfuse (geo0 : Geometry) (uvattrib : String) (group : String) (grouptype : Int) (postype : Int) (usedist : Int) (dist : Float) (metric : Int) (manpos : Int) (uvw : Vector3) (gridtype : Int) (gridspacing : Vector3) (gridlines : Vector3) (gridpow2 : Vector3) (gridoffset : Vector3) (gridround : Int) (gridtol : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::uvlayout::2.0" has_rundata]
opaque sop_uvlayout_2_0 (geo0 : Geometry) (uvattr : String) (group : String) (packbetween : Int) (additionalseams : String) (packing : Int) (scale : Float) (padding : Int) (paddingboundary : Int) (packincavities : Int) (bboxcenter : Vector2) (bboxsize : Vector2) (correctareas : Int) (axisalignislands : Int) (rotations : Int) (iterations : Int) (randseed : Int) (resolution : Int) (uvtolerance : Float) (scaletolerance : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::uvlayout::3.0" has_rundata]
opaque sop_uvlayout_3_0 (geo0 : Geometry) (geo1 : Geometry) (uvattrib : String) (projplane : Int) (group : String) (separatoredges : String) (useislandattr : Int) (islandattr : String) (usetargetattr : Int) (targetattr : String) (targetoverrides : String) (useislandscaleattr : Int) (islandscaleattr : String) (scaleoverrides : String) (useislandsetattr : Int) (islandoffsetattr : String) (offsetoverrides : String) (correctareas : Int) (axisalignislands : Int) (scaling : Int) (scale : Float) (rotstep : Int) (circledivs : Int) (packbetween : Int) (packincavities : Int) (padding : Int) (paddingboundary : Int) (expandpadding : Int) (iterations : Int) (resolution : Int) (customresolution : Int) (uvtolerance : Float) (scaletolerance : Float) (randseed : Int) (targettype : Int) (udimtilemethod : Int) (usedefaulttarget : Int) (defaulttarget : Int) (usedefaultudimtarget : Int) (defaultudimtarget : Int) (rects : DictArray) (tilesize : Vector2) (numcolumns : Int) (startingudim : Int) (udimtilestart : Int) (udimextendfixedislands : Int) (targetuvattrib : String) (targetprojplane : Int) (targetgroup : String) (targetseparatoredges : String) (usetargetislandattr : Int) (targetislandattr : String) (stackislands : Int) (invertedoverlays : Int) (stackonnongroup : Int) (overlaytolerance : Float) (generatenonpackedpoly : Int) (nonpackedpolys : String) (generateislandattr : Int) (outputislandattr : String) (generatetargetattr : Int) (outputtargetattr : String) (udimtilecount : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::uvproject" has_rundata]
opaque sop_uvproject (geo0 : Geometry) (uvattrib : String) (group : String) (grouptype : Int) (projtype : Int) (torrad : Float) (xOrd : Int) (rOrd : Int) (t : Vector3) (r : Vector3) (s : Vector3) (p : Vector3) (inittype : Int) (urange : Vector2) (vrange : Vector2) (angle : Float) (fixseams : Int) (fixpolar : Int) (polerad : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::uvtransform::2.0" has_rundata]
opaque sop_uvtransform_2_0 (geo0 : Geometry) (group : String) (grouptype : Int) (uvattrib : String) (xOrd : Int) (rOrd : Int) (t : Vector3) (r : Vector3) (s : Vector3) (shear : Vector3) (p : Vector3) (rad : Float) (type : Int) (tandeg : Vector2) (kernel : String) (metric : Int) (global : Int) (uvglobal : Int) (visualizefalloff : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::uvunwrap" has_rundata]
opaque sop_uvunwrap (geo0 : Geometry) (geo1 : Geometry) (uvattrib : String) (group : String) (planegroup : String) (nplanes : Int) (layout : Int) (scale : Int) (spacing : Float) (rOrd : Int) (r : Vector3) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdb" has_rundata]
opaque sop_vdb (geo0 : Geometry) (ngrids : DictArray) (voxelsize : Float) (size : Vector3) (center : Vector3) (taper : Float) (camera : String) (zmin : Float) (zmax : Float) (usecamwindow : Int) (winx : Vector2) (winy : Vector2) (uniformsamples : Int) (samplediv : Int) (divs : Vector3) (divsize : Float) (zscale : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbactivate" has_rundata]
opaque sop_vdbactivate (geo0 : Geometry) (geo1 : Geometry) (group : String) (operation : Int) (setvalue : Int) (value : Float) (center : Vector3) (size : Vector3) (min : Vector3) (max : Vector3) (expand : Int) (expanddist : Float) (expansionpattern : String) (boundgroup : String) (usevdb : Int) (usehull : Int) (boundptgroup : String) (voxeloffset : Float) (worldoffset : Float) (bgtolerance : Float) (prune : Int) (tolerance : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbactivatesdf" has_rundata]
opaque sop_vdbactivatesdf (geo0 : Geometry) (group : String) (assumeuniformscale : Int) (useworldspaceunits : Int) (radius : Int) (radiusworld : Float) (iterations : Int) (halfwidth : Int) (halfwidthworld : Float) (voxeloffset : Float) (accuracy : String) (invert : Int) (minmask : Float) (maxmask : Float) (trim : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbadvectpoints" has_rundata]
opaque sop_vdbadvectpoints (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (advectvdbpoints : Int) (group : String) (vdbpointsgroups : String) (velgroup : String) (cptgroup : String) (operation : String) (integration : String) (iterations : Int) (timestep : Float) (steps : Int) (outputstreamlines : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbadvectsdf" has_rundata]
opaque sop_vdbadvectsdf (geo0 : Geometry) (geo1 : Geometry) (group : String) (velgroup : String) (respectclass : Int) (timestep : Float) (substeps : Int) (advection : String) (limiter : String) (advectspatial : String) (advecttemporal : String) (normsteps : Int) (renormspatial : String) (renormtemporal : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbanalysis" has_rundata]
opaque sop_vdbanalysis (geo0 : Geometry) (geo1 : Geometry) (group : String) (operator : Int) (maskname : String) (outputname : String) (customname : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbclip" has_rundata]
opaque sop_vdbclip (geo0 : Geometry) (geo1 : Geometry) (group : String) (inside : Int) (clipper : String) (mask : String) (camera : String) (setnear : Int) (near : Float) (setfar : Int) (far : Float) (setpadding : Int) (padding : Vector3) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbcombine" has_rundata]
opaque sop_vdbcombine (geo0 : Geometry) (geo1 : Geometry) (agroup : String) (bgroup : String) (collation : String) (operation : Int) (amult : Float) (bmult : Float) (resample : Int) (resampleinterp : Int) (deactivate : Int) (bgtolerance : Float) (prunedegenerate : Int) (prune : Int) (tolerance : Float) (flood : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbconvexclipsdf" has_rundata]
opaque sop_vdbconvexclipsdf (geo0 : Geometry) (geo1 : Geometry) (group : String) (ptgroup : String) (operation : Int) (voxeloffset : Float) (worldoffset : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbcreatecl" has_rundata]
opaque sop_vdbcreatecl (geo0 : Geometry) (geo1 : Geometry) (leafdilation : Int) (activateleaves : Int) (leaveongpu : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbdiagnostics" has_rundata]
opaque sop_vdbdiagnostics (geo0 : Geometry) (group : String) (usemask : Int) (usepoints : Int) (respectclass : Int) (verify_fogvolume : Int) (verify_csg : Int) (verify_filtering : Int) (verify_advection : Int) (test_finite : Int) (id_finite : Int) (fix_finite : Int) (test_background : Int) (id_background : Int) (fix_background : Int) (test_valrange : Int) (id_valrange : Int) (fix_valrange : Int) (valrange : Vector2) (test_symmetric : Int) (test_bandwidth : Int) (bandwidth : Int) (test_surface : Int) (test_gradient : Int) (id_gradient : Int) (fix_gradient : Int) (gradienttolerance : Float) (test_activetiles : Int) (id_activetiles : Int) (fix_activetiles : Int) (test_voxelsize : Int) (test_backgroundzero : Int) (id_backgroundzero : Int) (fix_backgroundzero : Int) (test_fogvalues : Int) (id_fogvalues : Int) (fix_fogvalues : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbextrapolate" has_rundata]
opaque sop_vdbextrapolate (geo0 : Geometry) (geo1 : Geometry) (group : String) (extfields : String) (mask : String) (mode : String) (sweepdomain : String) (convertorrenormalize : Int) (sweeps : Int) (sdfisovalue : Float) (fogisovalue : Float) (ignoretiles : Int) (dilate : Int) (pattern : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbfracture" has_rundata]
opaque sop_vdbfracture (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (group : String) (separatecutters : Int) (cutteroverlap : Int) (centercutter : Int) (randomizerotation : Int) (seed : Int) (segmentfragments : Int) (fragmentgroup : String) (visualizepieces : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbfromparticles" has_rundata]
opaque sop_vdbfromparticles (geo0 : Geometry) (geo1 : Geometry) (voxelsize : Float) (group : String) (builddistance : Int) (distancename : String) (buildfog : Int) (fogname : String) (buildmask : Int) (maskname : String) (radiusscale : Float) (minvoxelradius : Float) (prune : Int) (maskwidth : Float) (useworldspaceunits : Int) (bandwidthvoxels : Float) (bandwidth : Float) (writeintoref : Int) (footprint : Int) (velscale : Float) (velspace : Float) (numattrib : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbfrompolygons" has_rundata]
opaque sop_vdbfrompolygons (geo0 : Geometry) (geo1 : Geometry) (voxelsize : Float) (group : String) (builddistance : Int) (distancename : String) (buildfog : Int) (fogname : String) (useworldspaceunits : Int) (exteriorbandvoxels : Int) (interiorbandvoxels : Int) (exteriorband : Float) (interiorband : Float) (fillinterior : Int) (unsigneddist : Int) (preserveholes : Int) (numattrib : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdblod" has_rundata]
opaque sop_vdblod (geo0 : Geometry) (group : String) (lod : Int) (level : Float) (range : Vector3) (count : Int) (reuse : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbmerge" has_rundata]
opaque sop_vdbmerge (geo0 : Geometry) (auxgeo : GeometryArray) (group : String) (collation : String) (resampleinterp : Int) (op_fog : String) (op_sdf : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbmorphsdf" has_rundata]
opaque sop_vdbmorphsdf (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (sourcegroup : String) (targetgroup : String) (mask : Int) (maskname : String) (timestep : Float) (advectspatial : String) (advecttemporal : String) (normsteps : Int) (renormspatial : String) (renormtemporal : String) (invert : Int) (minmask : Float) (maxmask : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbocclusionmask" has_rundata]
opaque sop_vdbocclusionmask (geo0 : Geometry) (group : String) (camera : String) (voxelcount : Int) (voxeldepthsize : Float) (depth : Float) (erode : Int) (zoffset : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbpointsdelete" has_rundata]
opaque sop_vdbpointsdelete (geo0 : Geometry) (group : String) (vdbpointsgroups : String) (invert : Int) (dropgroups : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbpointsgroup" has_rundata]
opaque sop_vdbpointsgroup (geo0 : Geometry) (geo1 : Geometry) (group : String) (vdbpointsgroup : String) (enablecreate : Int) (groupname : String) (enablenumber : Int) (numbermode : Int) (pointpercent : Float) (pointcount : Int) (enablepercentattribute : Int) (percentattribute : String) (enableboundingbox : Int) (boundingmode : Int) (boundingname : String) (size : Vector3) (center : Vector3) (enablesdf : Int) (sdfname : String) (enablesdfmin : Int) (sdfmin : Float) (enablesdfmax : Int) (sdfmax : Float) (sdfinvert : Int) (deletegroups : String) (enableviewport : Int) (viewportoperation : Int) (viewportgroupname : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbpotentialflow" has_rundata]
opaque sop_vdbpotentialflow (geo0 : Geometry) (geo1 : Geometry) (group : String) (velocity : String) (maskvdbname : String) (masktype : Int) (useiterations : Int) (iterations : Int) (usetolerance : Int) (tolerance : Float) (useworldspace : Int) (dilationvoxels : Int) (dilation : Float) (usebackgroundvelocity : Int) (backgroundvelocity : Vector3) (applybackgroundvelocity : Int) (outputpotential : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbprojectnondivergent" has_rundata]
opaque sop_vdbprojectnondivergent (geo0 : Geometry) (geo1 : Geometry) (group : String) (useiterations : Int) (iterations : Int) (usetolerance : Int) (tolerance : Float) (usecollider : Int) (collidertype : String) (collider : String) (invertcollider : Int) (pressure : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbrenormalizesdf" has_rundata]
opaque sop_vdbrenormalizesdf (geo0 : Geometry) (group : String) (assumeuniformscale : Int) (useworldspaceunits : Int) (radius : Int) (radiusworld : Float) (iterations : Int) (halfwidth : Int) (halfwidthworld : Float) (voxeloffset : Float) (accuracy : String) (invert : Int) (minmask : Float) (maxmask : Float) (trim : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbresample" has_rundata]
opaque sop_vdbresample (geo0 : Geometry) (geo1 : Geometry) (group : String) (reference : String) (order : Int) (mode : Int) (xOrd : Int) (rOrd : Int) (t : Vector3) (r : Vector3) (s : Vector3) (p : Vector3) (voxelsize : Float) (voxelscale : Float) (linearxform : Int) (xformvectors : Int) (rebuild : Int) (prune : Int) (tolerance : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbreshapesdf" has_rundata]
opaque sop_vdbreshapesdf (geo0 : Geometry) (geo1 : Geometry) (group : String) (mask : Int) (maskname : String) (operation : String) (assumeuniformscale : Int) (useworldspaceunits : Int) (radius : Int) (radiusworld : Float) (iterations : Int) (halfwidth : Int) (halfwidthworld : Float) (voxeloffset : Float) (accuracy : String) (invert : Int) (minmask : Float) (maxmask : Float) (trim : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbsegmentbyconnectivity" has_rundata]
opaque sop_vdbsegmentbyconnectivity (geo0 : Geometry) (group : String) (colorsegments : Int) (appendnumber : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbsmooth" has_rundata]
opaque sop_vdbsmooth (geo0 : Geometry) (geo1 : Geometry) (group : String) (mask : Int) (maskname : String) (operation : String) (worldunits : Int) (radius : Int) (worldradius : Float) (iterations : Int) (invert : Int) (minmask : Float) (maxmask : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbsmoothsdf" has_rundata]
opaque sop_vdbsmoothsdf (geo0 : Geometry) (geo1 : Geometry) (group : String) (mask : Int) (maskname : String) (operation : String) (assumeuniformscale : Int) (useworldspaceunits : Int) (radius : Int) (radiusworld : Float) (iterations : Int) (halfwidth : Int) (halfwidthworld : Float) (voxeloffset : Float) (accuracy : String) (invert : Int) (minmask : Float) (maxmask : Float) (trim : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbtopologytosdf" has_rundata]
opaque sop_vdbtopologytosdf (geo0 : Geometry) (group : String) (outputname : String) (customname : String) (worldspaceunits : Int) (bandwidth : Int) (bandwidthws : Float) (dilation : Int) (closingwidth : Int) (smoothingsteps : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbtospheres" has_rundata]
opaque sop_vdbtospheres (geo0 : Geometry) (group : String) (isovalue : Float) (worldunits : Int) (useradiusmin : Int) (radiusmin : Float) (useradiusmax : Int) (radiusmax : Float) (usespheresmin : Int) (spheresmin : Int) (usespheresmax : Int) (spheresmax : Int) (scatter : Int) (overlapping : Int) (preserve : Int) (doid : Int) (dopscale : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbvectormerge" has_rundata]
opaque sop_vdbvectormerge (geo0 : Geometry) (xgroup : String) (ygroup : String) (zgroup : String) (usexname : Int) (merge_name : String) (vectype : Int) (remove_sources : Int) (copyinactive : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbvectorsplit" has_rundata]
opaque sop_vdbvectorsplit (geo0 : Geometry) (group : String) (remove_sources : Int) (copyinactive : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vdbvisualizetree" has_rundata]
opaque sop_vdbvisualizetree (geo0 : Geometry) (group : String) (addcolor : Int) (previewfrustum : Int) (drawleafnodes : Int) (leafmode : Int) (drawinternalnodes : Int) (internalmode : Int) (drawtiles : Int) (tilemode : Int) (drawvoxels : Int) (voxelmode : Int) (ignorestaggered : Int) (addindexcoord : Int) (addvalue : Int) (usegridname : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::vertexsplit" has_rundata]
opaque sop_vertexsplit (geo0 : Geometry) (group : String) (attribname : String) (tol : Float) (promote : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::visibility" has_rundata]
opaque sop_visibility (geo0 : Geometry) (group : String) (action : Int) (applyto : Int) (viewport : Int) (cumulative : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volume" has_rundata]
opaque sop_volume (geo0 : Geometry) (type : Int) (components : Int) (rank : Int) (name : String) (initialval : Vector3) (initialalpha : Float) (taper : Vector2) (dimensionsource : Int) (camera : String) (zmin : Float) (zmax : Float) (usecamwindow : Int) (winx : Vector2) (winy : Vector2) (uniformsamples : Int) (samplediv : Int) (divs : Vector3) (divsize : Float) (zscale : Float) (twod : Int) (voxelplane : Int) (volborder : Int) (volborderval : Vector3) (volborderalpha : Float) (voltol : Float) (quantizetol : Float) (dither : Int) (usefp16 : Int) (volvis : Int) (volvisiso : Float) (volvisdensity : Float) (voltypeinfo : Int) (volvistiles : Float) (size : Vector3) (t : Vector3) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumeambientocclusion" has_rundata]
opaque sop_volumeambientocclusion (geo0 : Geometry) (group : String) (occlusionname : String) (resolutionratio : Float) (expand : Int) (densityscale : Float) (transmissioncutoff : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumeanalysis" has_rundata]
opaque sop_volumeanalysis (geo0 : Geometry) (group : String) (analysis : Int) (edgedetectmode : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumearrivaltime" has_rundata]
opaque sop_volumearrivaltime (geo0 : Geometry) (geo1 : Geometry) (group : String) (name : String) (normalize : Int) (cutoff : Float) (tol : Float) (maxiter : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumebin" has_rundata]
opaque sop_volumebin (geo0 : Geometry) (volume : String) (stencil : String) (doignore : Int) (ignore : Float) (bins : Int) (rangemode : Int) (minmax : Vector2) (keepall : Int) (keeprange : Vector2) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumeblur" has_rundata]
opaque sop_volumeblur (geo0 : Geometry) (group : String) (usevoxelradius : Int) (radius : Float) (voxelradius : Vector3) (useopencl : Int) (reduction : Int) (passes : Int) (bordertype : Int) (borderval : Float) (enableblending : Int) (blurblend : Float) (originalblend : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumebound" has_rundata]
opaque sop_volumebound (geo0 : Geometry) (group : String) (comp : Int) (value : Float) (voxelpad : Vector3) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumebreak" has_rundata]
opaque sop_volumebreak (geo0 : Geometry) (geo1 : Geometry) (group : String) (breaktype : Int) (closeholes : Int) (closegeo : Int) (snapdistance : Float) (creategroups : Int) (insidegroup : String) (insideclosuregroup : String) (outsidegroup : String) (outsideclosuregroup : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumecombine" has_rundata]
opaque sop_volumecombine (geo0 : Geometry) (dstvolume : String) (numcombines : DictArray) (postscale : Float) (dothreshold : Int) (threshold : Float) (doclampmin : Int) (clampmin : Float) (doclampmax : Int) (clampmax : Float) (createmissing : Int) (forcescalar : Int) (deletesource : Int) (errormissing : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumecompress" has_rundata]
opaque sop_volumecompress (geo0 : Geometry) (geo1 : Geometry) (group : String) (compression : Int) (updatesettings : Int) (constanttol : Float) (quantizetol : Float) (dither : Int) (usefp16 : Int) (maskgrp : String) (domaskmin : Int) (maskmin : Float) (domaskmax : Int) (maskmax : Float) (invertmask : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumeconvolve3" has_rundata]
opaque sop_volumeconvolve3 (geo0 : Geometry) (group : String) (convolvezm : Matrix3) (convolvezz : Matrix3) (convolvezp : Matrix3) (normalize : Int) (operation : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumefeather" has_rundata]
opaque sop_volumefeather (geo0 : Geometry) (group : String) (decaymode : Int) (decay : Float) (decayvoxel : Float) (unitdist : Float) (unitvoxel : Float) (angle : Float) (outside : Int) (detect2d : Int) (bordertype : Int) (borderval : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumefft" has_rundata]
opaque sop_volumefft (geo0 : Geometry) (group : String) (centerdc : Int) (invert : Int) (normalize : Int) (slices : Int) (voxelplane : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumefromattrib" has_rundata]
opaque sop_volumefromattrib (geo0 : Geometry) (geo1 : Geometry) (group : String) (pointgrp : String) (useattrib : Int) (attrib : String) (disableonmissing : Int) (accumulate : Int) (extrapolate : Int) (usemaxextrapolate : Int) (maxextrapolate : Float) (usemaxextrapolatedist : Int) (maxextrapolatedist : Float) (threshold : Float) (bandwidth : Float) (calculationtype : Int) (dstpreadd : Float) (dstpremul : Float) (scalebyvolume : Int) (srcpreadd : Float) (srcpremul : Float) (postadd : Float) (postmul : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumemerge" has_rundata]
opaque sop_volumemerge (geo0 : Geometry) (geo1 : Geometry) (group : String) (mergegrp : String) (mergemethod : Int) (clampvolume : Int) (dstpreadd : Float) (dstpremul : Float) (srcpreadd : Float) (srcpremul : Float) (postadd : Float) (postmul : Float) (doclampmin : Int) (clampmin : Float) (doclampmax : Int) (clampmax : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumemix" has_rundata]
opaque sop_volumemix (geo0 : Geometry) (geo1 : Geometry) (group : String) (mixgrp : String) (mixmethod : Int) (range : Vector2) (blend : Float) (expr : Float) (dstpreadd : Float) (dstpremul : Float) (srcpreadd : Float) (srcpremul : Float) (postadd : Float) (postmul : Float) (doclampmin : Int) (clampmin : Float) (doclampmax : Int) (clampmax : Float) (expandvdb : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumenormalize" has_rundata]
opaque sop_volumenormalize (geo0 : Geometry) (numvolumes : DictArray) (norm : Int) (norm0tol : Float) (normalize : Int) (threshold : Float) (addremainder : Int) (abs : Int) (clampzero : Int) (clampone : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumeopticalflow" has_rundata]
opaque sop_volumeopticalflow (geo0 : Geometry) (geo1 : Geometry) (group : String) (goalgroup : String) (tolerance : Float) (winradius : Int) (gaussian : Int) (levels : Int) (pyramidscale : Float) (iterations : Int) (approxradius : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumepatch" has_rundata]
opaque sop_volumepatch (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (basegroup : String) (patchgroup : String) (maskgroup : String) (maskcutoff : Float) (patchislaplacian : Int) (tolerance : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumerasterizelattice" has_rundata]
opaque sop_volumerasterizelattice (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (group : String) (enable_preprocess : Int) (deactivate : Int) (mode : Int) (attrib : String) (sourcevols : String) (dosmoothing : Int) (scalecompressed : Int) (domaxdensityscale : Int) (maxdensityscale : Float) (prune : Int) (prunetolerance : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumerasterizeparticles" has_rundata]
opaque sop_volumerasterizeparticles (geo0 : Geometry) (geo1 : Geometry) (group : String) (points : String) (filter : String) (densityattrib : String) (densityscale : Float) (particlescale : Float) (minfilter : Float) (velocityblur : Int) (shutter : Float) (shutteroffset : Float) (blursamples : Int) (normalize : Int) (attribrules : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumereduce" has_rundata]
opaque sop_volumereduce (geo0 : Geometry) (group : String) (reduction : Int) (percentile : Float) (scaleby : Int) (result : Int) (resultattrib : String) (createvarmap : Int) (resultlvar : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumeresample" has_rundata]
opaque sop_volumeresample (geo0 : Geometry) (group : String) (filter : String) (filterscale : Float) (fixedresample : Int) (uniformsamples : Int) (samplediv : Int) (divs : Vector3) (divsize : Float) (scale : Float) (detect2d : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumeresize" has_rundata]
opaque sop_volumeresize (geo0 : Geometry) (geo1 : Geometry) (group : String) (extracttile : Int) (tilecount : Vector3) (tilenum : Int) (combine : Int) (size : Vector3) (t : Vector3) (tileminpad : Vector3) (tilemaxpad : Vector3) (voxelpad : Vector3) (usepoints : Int) (keepdata : Int) (allowextrap : Int) (useclipplane : Int) (clipcenter : Vector3) (clipdir : Vector3) (usemaxres : Int) (maxres : Vector3) (detect2d : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumesdf" has_rundata]
opaque sop_volumesdf (geo0 : Geometry) (group : String) (iso : Float) (invert : Int) (usemaxdist : Int) (maxdist : Float) (rebuildwithfim : Int) (fimtolerance : Float) (fimiterations : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumeslice" has_rundata]
opaque sop_volumeslice (geo0 : Geometry) (group : String) (method : Int) (plane : Int) (relative : Int) (planepos : Vector3) (planeoffset : Float) (voxelsnap : Int) (attrib : String) (createvarmap : Int) (lvar : String) (visualize : Int) (vismode : Int) (visrange : Vector2) (cdramp : ColorRamp) (keep : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumesplice" has_rundata]
opaque sop_volumesplice (geo0 : Geometry) (group : String) (deleteorig : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumetrail" has_rundata]
opaque sop_volumetrail (geo0 : Geometry) (geo1 : Geometry) (group : String) (velfield : String) (advectionchoice : Int) (traillen : Float) (usecfl : Int) (cfl : Float) (numsteps : Int) (usemaxsteps : Int) (maxsteps : Int) (keep : Int) (visenable : Int) (detectrange : Int) (vismax : Float) (visramp : Int) (cdramp : ColorRamp) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumevectorjoin" has_rundata]
opaque sop_volumevectorjoin (geo0 : Geometry) (setcomponents : Int) (components : Int) (xgroup : String) (ygroup : String) (zgroup : String) (wgroup : String) (keep : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumevectorsplit" has_rundata]
opaque sop_volumevectorsplit (geo0 : Geometry) (group : String) (keep : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumevisualization" has_rundata]
opaque sop_volumevisualization (geo0 : Geometry) (vismode : Int) (rangemin : Float) (rangemax : Float) (densityscale : Float) (shadowscale : Float) (setambientcolors : Int) (ambientexposed : Vector3) (ambientoccluded : Vector3) (setambientshadows : Int) (ambientshadows : Float) (setshadowcolor : Int) (shadowcolor : Vector3) (usedict : Int) (setmaxres : Int) (maxres : Int) (setambientmapsize : Int) (ambientmapsize : Float) (setambientsteprate : Int) (ambientsteprate : Float) (densityfield : String) (densityrampmode : Int) (densityramp : FloatRamp) (cdfield : String) (cdrangeoverride : Int) (cdrange : Vector2) (cdrampmode : Int) (cdramp : ColorRamp) (emitscale : Float) (emitfield : String) (emitrangeoverride : Int) (emitrange : Vector2) (emitrampmode : Int) (emitramp : FloatRamp) (emitcdfield : String) (emitcdrampmode : Int) (emitcdramp : ColorRamp) (emitcdfieldscale : Float) (emitcdtemperature0 : Float) (emitcdtemperature : Float) (emitcdtonemap : Int) (emitcdadaptation : Float) (emitcdburn : Float) (enablescatter : Int) (extinctionratio : Float) (scatteringiter : Int) (emitcdrangeoverride : Int) (emitcdrange : Vector2) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::volumevop" has_rundata]
opaque sop_volumevop (geo0 : Geometry) (geo1 : Geometry) (geo2 : Geometry) (geo3 : Geometry) (vexsrc : Int) (shoppath : String) (script : String) (vop_compiler : String) (vexsnippet : String) (vex_exportlist : String) (vex_strict : Int) (vex_strictvariables : Int) (vex_cwdpath : String) (vex_outputmask : String) (vex_multithread : Int) (prunevdbblocks : Int) (vex_geometrygenerator : Int) (vdb_signedflood : Int) (autobind : Int) (bindeach : Int) (bindings : DictArray) (vex_precision : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::voronoisplit" has_rundata]
opaque sop_voronoisplit (geo0 : Geometry) (geo1 : Geometry) (group : String) (clipptsgrp : String) (offset : Float) (interior : Int) (transferattribs : Int) (stamppieces : Int) (pieceattrib : String) (clipptattrib : String) (useweightattrib : Int) (weightattrib : String) (weightmethod : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::weightarraybiharmonic" has_rundata]
opaque sop_weightarraybiharmonic (geo0 : Geometry) (group : String) (primtype : Int) (maxiter : Int) (indexattrib : String) (weightattrib : String) (difftol : Float) (verbose : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::weightarrayinterpolate" has_rundata]
opaque sop_weightarrayinterpolate (geo0 : Geometry) (geo1 : Geometry) (primattrib : String) (primuvattrib : String) (indexattrib : String) (weightattrib : String) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::windingnumber" has_rundata]
opaque sop_windingnumber (geo0 : Geometry) (geo1 : Geometry) (querypoints : String) (meshprims : String) (type : Int) (attrib : String) (assolidangle : Int) (negate : Int) (fullaccuracy : Int) (accuracyscale : Float) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::wire" has_rundata]
opaque sop_wire (geo0 : Geometry) (group : String) (radius : Float) (usescaleattrib : Int) (scaleattrib : String) (corners : Int) (caps : Int) (remove : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::wireblend" has_rundata]
opaque sop_wireblend (geo0 : Geometry) (auxgeo : GeometryArray) (group : String) (diff : Int) (nblends : DictArray) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::xform" has_rundata]
opaque sop_xform (geo0 : Geometry) (group : String) (grouptype : Int) (xOrd : Int) (rOrd : Int) (t : Vector3) (r : Vector3) (s : Vector3) (shear : Vector3) (scale : Float) (p : Vector3) (pr : Vector3) (prexform_xOrd : Int) (prexform_rOrd : Int) (prexform_t : Vector3) (prexform_r : Vector3) (prexform_s : Vector3) (prexform_shear : Vector3) (attribs : String) (updatenmls : Int) (updateaffectednmls : Int) (vlength : Int) (invertxform : Int) (addattrib : Int) (outputattrib : String) (outputmerge : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::xformaxis" has_rundata]
opaque sop_xformaxis (geo0 : Geometry) (group : String) (grouptype : Int) (orig : Vector3) (dir : Vector3) (trans : Float) (rot : Float) (scale : Float) (updatenmls : Int) (updateaffectednmls : Int) (vlength : Int) (invertxform : Int) (addattrib : Int) (outputattrib : String) (outputmerge : Int) : Geometry

/-- outputs: (geo0) -/        
@[apex_node "sop::xformbyattrib" has_rundata]
opaque sop_xformbyattrib (geo0 : Geometry) (group : String) (grouptype : Int) (xformattrib : String) (invertxform : Int) (attribs : String) (updateaffectednmls : Int) (vlength : Int) (deletexform : Int) : Geometry

/-- outputs: (camelcase) -/        
@[apex_node "string::CamelCase"]
opaque string_CamelCase (string : String) (separators : String) (capitalizefirst : Bool) : String

/-- outputs: (result) -/        
@[apex_node "string::EndsWith"]
opaque string_EndsWith (string : String) (endswith : String) : Bool

/-- outputs: (index) -/        
@[apex_node "string::Find"]
opaque string_Find (string : String) (find : String) (casesensitive : Bool) : Int

/-- outputs: (result) -/        
@[apex_node "string::ForceValidName"]
opaque string_ForceValidName (input : String) (safechars : String) : String

/-- outputs: (result) -/        
@[apex_node "string::Format" has_rundata]
opaque string_Format (formatstring : String) {numargs: Nat} (args : VariadicArg Untyped numargs) : String

/-- outputs: (result) -/        
@[apex_node "string::FromInteger"]
opaque string_FromInteger (value : Int) : String

/-- outputs: (string) -/        
@[apex_node "string::FromRamp<ColorRamp>"]
opaque string_FromRampColorRamp (ramp : ColorRamp) : String

/-- outputs: (string) -/        
@[apex_node "string::FromRamp<FloatRamp>"]
opaque string_FromRampFloatRamp (ramp : FloatRamp) : String

/-- outputs: (result) -/        
@[apex_node "string::Join"]
opaque string_Join (delimiter : String) (array : StringArray) : String

/-- outputs: (length) -/        
@[apex_node "string::Length"]
opaque string_Length (str : String) : Int

/-- outputs: (lowercase) -/        
@[apex_node "string::Lower"]
opaque string_Lower (string : String) : String

/-- outputs: (before,after) -/        
@[apex_node "string::Partition"]
opaque string_Partition (string : String) (partition : String) (last : Bool) : String×String

-- special function not supported yet
-- opaque string::PathSplit (parm : String) (value : String) (value : String) (parm : String) (input : String) ...

/-- outputs: (result) -/        
@[apex_node "string::PatternRename"]
opaque string_PatternRename (input : String) (find : String) (replace : String) : String

/-- outputs: (success,captured) -/        
@[apex_node "string::RegexFind" has_rundata]
opaque string_RegexFind (regex : String) (input : String) (start : Int) (end' : Int) : Bool×StringArray

/-- outputs: (success,captured) -/        
@[apex_node "string::RegexFindAll" has_rundata]
opaque string_RegexFindAll (regex : String) (input : String) (start : Int) (end' : Int) : Bool×StringArray

/-- outputs: (success) -/        
@[apex_node "string::RegexMatch" has_rundata]
opaque string_RegexMatch (regex : String) (input : String) : Bool

/-- outputs: (success,result) -/        
@[apex_node "string::RegexReplace" has_rundata]
opaque string_RegexReplace (regex : String) (input : String) (replace : String) (numreplacements : Int) : Bool×String

/-- outputs: (success,results) -/        
@[apex_node "string::RegexSplit" has_rundata]
opaque string_RegexSplit (regex : String) (input : String) (maxsplits : Int) : Bool×StringArray

/-- outputs: (result) -/        
@[apex_node "string::Replace::2.0"]
opaque string_Replace_2_0 (input : String) (find : String) (replace : String) : String

/-- outputs: (reverse) -/        
@[apex_node "string::Reverse"]
opaque string_Reverse (string : String) : String

/-- outputs: (result) -/        
@[apex_node "string::Split"]
opaque string_Split (input : String) (separators : String) : StringArray

/-- outputs: (result) -/        
@[apex_node "string::StartsWith"]
opaque string_StartsWith (string : String) (startswith : String) : Bool

/-- outputs: (substring) -/        
@[apex_node "string::SubString"]
opaque string_SubString (string : String) (start : Int) (end' : Int) : String

/-- outputs: (result,success) -/        
@[apex_node "string::ToFloat"]
opaque string_ToFloat (value : String) : Float×Bool

/-- outputs: (result,success) -/        
@[apex_node "string::ToInteger"]
opaque string_ToInteger (value : String) : Int×Bool

/-- outputs: (ramp) -/        
@[apex_node "string::ToRamp<ColorRamp>"]
opaque string_ToRampColorRamp (string : String) : ColorRamp

/-- outputs: (ramp) -/        
@[apex_node "string::ToRamp<FloatRamp>"]
opaque string_ToRampFloatRamp (string : String) : FloatRamp

/-- outputs: (uppercase) -/        
@[apex_node "string::Upper"]
opaque string_Upper (string : String) : String

/-- outputs: (result) -/        
@[apex_node "transform::Blend"]
opaque transform_Blend (a : Matrix4) (b : Matrix4) (blend : Float) (components : Int) (accurate : Bool) : Matrix4

/-- outputs: (m) -/        
@[apex_node "transform::Build"]
opaque transform_Build (t : Vector3) (r : Vector3) (s : Vector3) (sh : Vector3) (p : Vector3) (pr : Vector3) (xOrd : Int) (rOrd : Int) : Matrix4

-- special function not supported yet
-- opaque transform::ClampedLookAtPorthole (parm : Float) (parm : Matrix4) (value : Matrix4) (value : Matrix4) ...

-- special function not supported yet
-- opaque transform::ClampedLookAtWindow (parm : Float) (parm : Matrix4) (value : Matrix4) (value : Matrix4) (v...

/-- outputs: (determinant) -/        
@[apex_node "transform::Determinant<Matrix3>"]
opaque transform_DeterminantMatrix3 (m : Matrix3) : Float

/-- outputs: (determinant) -/        
@[apex_node "transform::Determinant<Matrix4>"]
opaque transform_DeterminantMatrix4 (m : Matrix4) : Float

/-- outputs: (result) -/        
@[apex_node "transform::Dihedral<Matrix3>"]
opaque transform_DihedralMatrix3 (a : Vector3) (b : Vector3) : Matrix3

/-- outputs: (result) -/        
@[apex_node "transform::Dihedral<Matrix4>"]
opaque transform_DihedralMatrix4 (a : Vector3) (b : Vector3) : Matrix4

/-- outputs: (result) -/        
@[apex_node "transform::Dihedral<Vector4>"]
opaque transform_DihedralVector4 (a : Vector3) (b : Vector3) : Vector4

/-- outputs: (t,r,s,sh) -/        
@[apex_node "transform::Explode"]
opaque transform_Explode (m : Matrix4) (xOrd : Int) (rOrd : Int) (p : Vector3) (pr : Vector3) : Vector3×Vector3×Vector3×Vector3

/-- outputs: (xform) -/        
@[apex_node "transform::LookAt"]
opaque transform_LookAt (inxform : Matrix4) (lookat : Matrix4) (lookup : Matrix4) : Matrix4

/-- outputs: (xform) -/        
@[apex_node "transform::LookAt::2.0"]
opaque transform_LookAt_2_0 (xform : Matrix4) (lookat : Matrix4) (lookup : Matrix4) (lookataxis : Vector3) (lookupaxis : Vector3) : Matrix4

-- special function not supported yet
-- opaque transform::MirrorTransform (x : Float) (y : Float) (z : Float) (x : Float) (y : Float) (z : Float) (x...

/-- outputs: (result) -/        
@[apex_node "transform::MultiBlend<Matrix4>" has_rundata]
opaque transform_MultiBlendMatrix4 {numitems: Nat} (items : VariadicArg Matrix4 numitems) {numweights: Nat} (weights : VariadicArg Float numweights) (normalize : Bool) (accurate : Bool) : Matrix4

/-- outputs: (result) -/        
@[apex_node "transform::MultiBlend<Vector4>" has_rundata]
opaque transform_MultiBlendVector4 {numitems: Nat} (items : VariadicArg Vector4 numitems) {numweights: Nat} (weights : VariadicArg Float numweights) (normalize : Bool) (accurate : Bool) : Vector4

/-- outputs: (result) -/        
@[apex_node "transform::MultiBlendFromArray<Matrix4>" has_rundata]
opaque transform_MultiBlendFromArrayMatrix4 (items : Matrix4Array) (weights : FloatArray) (normalize : Bool) (accurate : Bool) : Matrix4

/-- outputs: (result) -/        
@[apex_node "transform::MultiBlendFromArray<Vector4>" has_rundata]
opaque transform_MultiBlendFromArrayVector4 (items : Vector4Array) (weights : FloatArray) (normalize : Bool) (accurate : Bool) : Vector4

/-- outputs: (rigid,stretch,success) -/        
@[apex_node "transform::PolarDecompose<Matrix3>"]
opaque transform_PolarDecomposeMatrix3 (m : Matrix3) : Matrix3×Matrix3×Bool

/-- outputs: (rigid,stretch,success) -/        
@[apex_node "transform::PolarDecompose<Matrix4>"]
opaque transform_PolarDecomposeMatrix4 (m : Matrix4) : Matrix4×Matrix3×Bool

/-- outputs: (result) -/        
@[apex_node "transform::Prerotate<Matrix3>"]
opaque transform_PrerotateMatrix3 (matrix : Matrix3) (radians : Vector3) (rord : Int) : Matrix3

/-- outputs: (result) -/        
@[apex_node "transform::Prerotate<Matrix4>"]
opaque transform_PrerotateMatrix4 (matrix : Matrix4) (radians : Vector3) (rord : Int) : Matrix4

/-- outputs: (result) -/        
@[apex_node "transform::Prescale<Matrix3>"]
opaque transform_PrescaleMatrix3 (matrix : Matrix3) (vector : Vector3) : Matrix3

/-- outputs: (result) -/        
@[apex_node "transform::Prescale<Matrix4>"]
opaque transform_PrescaleMatrix4 (matrix : Matrix4) (vector : Vector3) : Matrix4

/-- outputs: (result) -/        
@[apex_node "transform::Pretranslate<Matrix3>"]
opaque transform_PretranslateMatrix3 (matrix : Matrix3) (delta : Vector2) : Matrix3

/-- outputs: (result) -/        
@[apex_node "transform::Pretranslate<Matrix4>"]
opaque transform_PretranslateMatrix4 (matrix : Matrix4) (delta : Vector3) : Matrix4

-- special function not supported yet
-- opaque transform::ProjectOnSphere (value : Vector3) (value : Matrix4) (parm : Matrix4) (a : Matrix4) (a : Ve...

-- special function not supported yet
-- opaque transform::ProjectOnSphericalPorthole (parm : Float) (x : Float) (y : Float) (z : Float) (value : Vec...

-- special function not supported yet
-- opaque transform::ProjectOnSphericalWindow (parm : Float) (parm : Float) (value : Vector3) (value : Matrix4)...

/-- outputs: (result) -/        
@[apex_node "transform::Rotate<Matrix3>"]
opaque transform_RotateMatrix3 (matrix : Matrix3) (radians : Vector3) (rord : Int) : Matrix3

/-- outputs: (result) -/        
@[apex_node "transform::Rotate<Matrix4>"]
opaque transform_RotateMatrix4 (matrix : Matrix4) (radians : Vector3) (rord : Int) : Matrix4

/-- outputs: (result) -/        
@[apex_node "transform::RotateAboutAxis<Matrix3>"]
opaque transform_RotateAboutAxisMatrix3 (m : Matrix3) (axis : Vector3) (angle : Float) : Matrix3

/-- outputs: (result) -/        
@[apex_node "transform::RotateAboutAxis<Matrix4>"]
opaque transform_RotateAboutAxisMatrix4 (m : Matrix4) (axis : Vector3) (angle : Float) : Matrix4

/-- outputs: (result) -/        
@[apex_node "transform::Scale<Matrix3>"]
opaque transform_ScaleMatrix3 (matrix : Matrix3) (vector : Vector3) : Matrix3

/-- outputs: (result) -/        
@[apex_node "transform::Scale<Matrix4>"]
opaque transform_ScaleMatrix4 (matrix : Matrix4) (vector : Vector3) : Matrix4

/-- outputs: (result) -/        
@[apex_node "transform::Slerp<Matrix3>"]
opaque transform_SlerpMatrix3 (a : Matrix3) (b : Matrix3) (bias : Float) : Matrix3

/-- outputs: (result) -/        
@[apex_node "transform::Slerp<Matrix4>"]
opaque transform_SlerpMatrix4 (a : Matrix4) (b : Matrix4) (bias : Float) : Matrix4

/-- outputs: (result) -/        
@[apex_node "transform::Slerp<Vector4>"]
opaque transform_SlerpVector4 (a : Vector4) (b : Vector4) (bias : Float) : Vector4

/-- outputs: (dot) -/        
@[apex_node "transform::SmoothRotation"]
opaque transform_SmoothRotation (a : Vector3) (b : Vector3) (rord : Int) : Vector3

/-- outputs: (result) -/        
@[apex_node "transform::Translate<Matrix3>"]
opaque transform_TranslateMatrix3 (matrix : Matrix3) (delta : Vector2) : Matrix3

/-- outputs: (result) -/        
@[apex_node "transform::Translate<Matrix4>"]
opaque transform_TranslateMatrix4 (matrix : Matrix4) (delta : Vector3) : Matrix4

-- special function not supported yet
-- opaque transform::ViewScaling (value : Dict) (value : Matrix4) (value : Int) (value : Int) (value : Float) (...

/-- outputs: (value,value,success,value,success,value,success,uievent,xform,scale,mininverted) -/        
@[apex_node "uievent::CurViewport"]
opaque uievent_CurViewport (value : Dict) (dict : Dict) (key : String) (default : Matrix4) (dict : Dict) (key : String) (default : Float) (dict : Dict) (key : String) (default : Vector4) (xform : Matrix4) (scale : Float) (mininverted : Vector4) (uievent : Dict) : Dict×Matrix4×Bool×Float×Bool×Vector4×Bool×Dict×Matrix4×Float×Vector4

/-- outputs: (value,value,success,value,success,uievent,mousebutton,modifier) -/        
@[apex_node "uievent::Device"]
opaque uievent_Device (value : Dict) (dict : Dict) (key : String) (default : Int) (dict : Dict) (key : String) (default : Int) (mousebutton : Int) (modifier : Int) (uievent : Dict) : Dict×Int×Bool×Int×Bool×Dict×Int×Int

/-- outputs: (value,value,success,value,success,value,success,uievent,posstart,xstart,ystart) -/        
@[apex_node "uievent::Drag"]
opaque uievent_Drag (value : Dict) (dict : Dict) (key : String) (default : Vector3) (dict : Dict) (key : String) (default : Float) (dict : Dict) (key : String) (default : Float) (posstart : Vector3) (xstart : Float) (ystart : Float) (uievent : Dict) : Dict×Vector3×Bool×Float×Bool×Float×Bool×Dict×Vector3×Float×Float

/-- outputs: (value,value,success,value,success,value,success,value,success,uievent,x,y,xstart,ystart) -/        
@[apex_node "uievent::MousePosition"]
opaque uievent_MousePosition (value : Dict) (dict : Dict) (key : String) (default : Float) (dict : Dict) (key : String) (default : Float) (dict : Dict) (key : String) (default : Float) (dict : Dict) (key : String) (default : Float) (x : Float) (y : Float) (xstart : Float) (ystart : Float) (uievent : Dict) : Dict×Float×Bool×Float×Bool×Float×Bool×Float×Bool×Dict×Float×Float×Float×Float

/-- outputs: (value,value,success,value,success,value,success,value,success,value,success,uievent,x,y,xstart,ystart,xform) -/        
@[apex_node "uievent::Ndc"]
opaque uievent_Ndc (value : Dict) (dict : Dict) (key : String) (default : Matrix4) (dict : Dict) (key : String) (default : Float) (dict : Dict) (key : String) (default : Float) (dict : Dict) (key : String) (default : Float) (dict : Dict) (key : String) (default : Float) (x : Float) (y : Float) (xstart : Float) (ystart : Float) (xform : Matrix4) (uievent : Dict) : Dict×Matrix4×Bool×Float×Bool×Float×Bool×Float×Bool×Float×Bool×Dict×Float×Float×Float×Float×Matrix4

/-- outputs: (value,value,success,value,success,value,success,value,success,uievent,xform,xform_ctrl,xform_shift,xform_shiftctrl) -/        
@[apex_node "uievent::PrimaryXform"]
opaque uievent_PrimaryXform (value : Dict) (dict : Dict) (key : String) (default : Matrix4) (dict : Dict) (key : String) (default : Matrix4) (dict : Dict) (key : String) (default : Matrix4) (dict : Dict) (key : String) (default : Matrix4) (xform : Matrix4) (xform_ctrl : Matrix4) (xform_shift : Matrix4) (xform_shiftctrl : Matrix4) (uievent : Dict) : Dict×Matrix4×Bool×Matrix4×Bool×Matrix4×Bool×Matrix4×Bool×Dict×Matrix4×Matrix4×Matrix4×Matrix4

/-- outputs: (value,value,success,value,success,value,success,value,success,uievent,origin,direction,originstart,directionstart) -/        
@[apex_node "uievent::ScreenRay"]
opaque uievent_ScreenRay (value : Dict) (dict : Dict) (key : String) (default : Vector3) (dict : Dict) (key : String) (default : Vector3) (dict : Dict) (key : String) (default : Vector3) (dict : Dict) (key : String) (default : Vector3) (origin : Vector3) (direction : Vector3) (originstart : Vector3) (directionstart : Vector3) (uievent : Dict) : Dict×Vector3×Bool×Vector3×Bool×Vector3×Bool×Vector3×Bool×Dict×Vector3×Vector3×Vector3×Vector3

/-- outputs: (value,value,success,value,success,value,success,value,success,value,success,value,success,value,success,uievent,enable,xform,xformstart,xformparentstart,xformreference,xformcomponent,useworld) -/        
@[apex_node "uievent::XformHandle"]
opaque uievent_XformHandle (value : Dict) (dict : Dict) (key : String) (default : Bool) (dict : Dict) (key : String) (default : Matrix4) (dict : Dict) (key : String) (default : Matrix4) (dict : Dict) (key : String) (default : Matrix4) (dict : Dict) (key : String) (default : Matrix4) (dict : Dict) (key : String) (default : Int) (dict : Dict) (key : String) (default : Bool) (enable : Bool) (xform : Matrix4) (xformstart : Matrix4) (xformparentstart : Matrix4) (xformreference : Matrix4) (xformcomponent : Int) (useworld : Bool) (uievent : Dict) : Dict×Bool×Bool×Matrix4×Bool×Matrix4×Bool×Matrix4×Bool×Matrix4×Bool×Int×Bool×Bool×Bool×Dict×Bool×Matrix4×Matrix4×Matrix4×Matrix4×Int×Bool
