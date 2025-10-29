import HouLean.Apex.Basic

namespace HouLean.Apex

open Generated

-- ============================================================================
-- Geometry Interface
-- ============================================================================

-- Geometry attribute operations are type-specific and element-specific
-- We provide typeclasses for each element type (Detail, Point, Prim, Vertex)

-- Detail Attribute typeclass (single value per geometry)
class DetailAttrib (α : Type) where
  get : Geometry → String → α × Bool
  set : Geometry → String → α → Geometry × Bool

-- Point Attribute typeclass
class PointAttrib (α : Type) where
  get : Geometry → Int → String → α × Bool
  set : Geometry → Int → String → α → Geometry × Bool
  setAll : Geometry → String → String → α → Geometry × Bool  -- group version
  setByName : Geometry → String → String → {n : Nat} → VariadicArg α n → Geometry
  find : Geometry → String → α → IntArray × Int

-- Primitive Attribute typeclass
class PrimAttrib (α : Type) where
  get : Geometry → Int → String → α × Bool
  set : Geometry → Int → String → α → Geometry × Bool
  setAll : Geometry → String → String → α → Geometry × Bool  -- group version
  setByName : Geometry → String → String → {n : Nat} → VariadicArg α n → Geometry
  find : Geometry → String → α → IntArray × Int

-- Vertex Attribute typeclass
class VertexAttrib (α : Type) where
  get : Geometry → Int → String → α × Bool
  set : Geometry → Int → String → α → Geometry × Bool
  setAll : Geometry → String → String → α → Geometry × Bool  -- group version
  setByName : Geometry → String → String → {n : Nat} → VariadicArg α n → Geometry
  find : Geometry → String → α → IntArray × Int

-- Instances for all supported types

-- Int instances
instance : DetailAttrib Int where
  get := geo_DetailAttribValueInt
  set := geo_SetDetailAttribValueInt

instance : PointAttrib Int where
  get := geo_PointAttribValueInt
  set := geo_SetPointAttribValueInt
  setAll := geo_SetPointAttribValuesInt
  setByName := geo_SetPointAttribValuesByNameInt
  find := geo_FindPointAttribValueInt

instance : PrimAttrib Int where
  get := geo_PrimAttribValueInt
  set := geo_SetPrimAttribValueInt
  setAll := geo_SetPrimAttribValuesInt
  setByName := geo_SetPrimAttribValuesByNameInt
  find := geo_FindPrimAttribValueInt

instance : VertexAttrib Int where
  get := geo_VertexAttribValueInt
  set := geo_SetVertexAttribValueInt
  setAll := geo_SetVertexAttribValuesInt
  setByName := geo_SetVertexAttribValuesByNameInt
  find := geo_FindVertexAttribValueInt

-- Float instances
instance : DetailAttrib Float where
  get := geo_DetailAttribValueFloat
  set := geo_SetDetailAttribValueFloat

instance : PointAttrib Float where
  get := geo_PointAttribValueFloat
  set := geo_SetPointAttribValueFloat
  setAll := geo_SetPointAttribValuesFloat
  setByName := geo_SetPointAttribValuesByNameFloat
  find := apexPanic!"Find attribute is not supported for this type of attribute!"

instance : PrimAttrib Float where
  get := geo_PrimAttribValueFloat
  set := geo_SetPrimAttribValueFloat
  setAll := geo_SetPrimAttribValuesFloat
  setByName := geo_SetPrimAttribValuesByNameFloat
  find := apexPanic!"Find attribute is not supported for this type of attribute!"

instance : VertexAttrib Float where
  get := geo_VertexAttribValueFloat
  set := geo_SetVertexAttribValueFloat
  setAll := geo_SetVertexAttribValuesFloat
  setByName := geo_SetVertexAttribValuesByNameFloat
  find := apexPanic!"Find attribute is not supported for this type of attribute!"

-- String instances
instance : DetailAttrib String where
  get := geo_DetailAttribValueString
  set := geo_SetDetailAttribValueString

instance : PointAttrib String where
  get := geo_PointAttribValueString
  set := geo_SetPointAttribValueString
  setAll := geo_SetPointAttribValuesString
  setByName := geo_SetPointAttribValuesByNameString
  find := geo_FindPointAttribValueString

instance : PrimAttrib String where
  get := geo_PrimAttribValueString
  set := geo_SetPrimAttribValueString
  setAll := geo_SetPrimAttribValuesString
  setByName := geo_SetPrimAttribValuesByNameString
  find := geo_FindPrimAttribValueString

instance : VertexAttrib String where
  get := geo_VertexAttribValueString
  set := geo_SetVertexAttribValueString
  setAll := geo_SetVertexAttribValuesString
  setByName := geo_SetVertexAttribValuesByNameString
  find := geo_FindVertexAttribValueString

-- Vector2 instances
instance : DetailAttrib Vector2 where
  get := geo_DetailAttribValueVector2
  set := geo_SetDetailAttribValueVector2

instance : PointAttrib Vector2 where
  get := geo_PointAttribValueVector2
  set := geo_SetPointAttribValueVector2
  setAll := geo_SetPointAttribValuesVector2
  setByName := geo_SetPointAttribValuesByNameVector2
  find := apexPanic! "Find attribute is not supported for this type of attribute!"

instance : PrimAttrib Vector2 where
  get := geo_PrimAttribValueVector2
  set := geo_SetPrimAttribValueVector2
  setAll := geo_SetPrimAttribValuesVector2
  setByName := geo_SetPrimAttribValuesByNameVector2
  find := apexPanic! "Find attribute is not supported for this type of attribute!"

instance : VertexAttrib Vector2 where
  get := geo_VertexAttribValueVector2
  set := geo_SetVertexAttribValueVector2
  setAll := geo_SetVertexAttribValuesVector2
  setByName := geo_SetVertexAttribValuesByNameVector2
  find := apexPanic! "Find attribute is not supported for this type of attribute!"

-- Vector3 instances
instance : DetailAttrib Vector3 where
  get := geo_DetailAttribValueVector3
  set := geo_SetDetailAttribValueVector3

instance : PointAttrib Vector3 where
  get := geo_PointAttribValueVector3
  set := geo_SetPointAttribValueVector3
  setAll := geo_SetPointAttribValuesVector3
  setByName := geo_SetPointAttribValuesByNameVector3
  find := apexPanic! "Find attribute is not supported for this type of attribute!"

instance : PrimAttrib Vector3 where
  get := geo_PrimAttribValueVector3
  set := geo_SetPrimAttribValueVector3
  setAll := geo_SetPrimAttribValuesVector3
  setByName := geo_SetPrimAttribValuesByNameVector3
  find := apexPanic! "Find attribute is not supported for this type of attribute!"

instance : VertexAttrib Vector3 where
  get := geo_VertexAttribValueVector3
  set := geo_SetVertexAttribValueVector3
  setAll := geo_SetVertexAttribValuesVector3
  setByName := geo_SetVertexAttribValuesByNameVector3
  find := apexPanic! "Find attribute is not supported for this type of attribute!"

-- Vector4 instances
instance : DetailAttrib Vector4 where
  get := geo_DetailAttribValueVector4
  set := geo_SetDetailAttribValueVector4

instance : PointAttrib Vector4 where
  get := geo_PointAttribValueVector4
  set := geo_SetPointAttribValueVector4
  setAll := geo_SetPointAttribValuesVector4
  setByName := geo_SetPointAttribValuesByNameVector4
  find := apexPanic! "Find attribute is not supported for this type of attribute!"

instance : PrimAttrib Vector4 where
  get := geo_PrimAttribValueVector4
  set := geo_SetPrimAttribValueVector4
  setAll := geo_SetPrimAttribValuesVector4
  setByName := geo_SetPrimAttribValuesByNameVector4
  find := apexPanic! "Find attribute is not supported for this type of attribute!"

instance : VertexAttrib Vector4 where
  get := geo_VertexAttribValueVector4
  set := geo_SetVertexAttribValueVector4
  setAll := geo_SetVertexAttribValuesVector4
  setByName := geo_SetVertexAttribValuesByNameVector4
  find := apexPanic! "Find attribute is not supported for this type of attribute!"

-- Matrix3 instances
instance : DetailAttrib Matrix3 where
  get := geo_DetailAttribValueMatrix3
  set := geo_SetDetailAttribValueMatrix3

instance : PointAttrib Matrix3 where
  get := geo_PointAttribValueMatrix3
  set := geo_SetPointAttribValueMatrix3
  setAll := geo_SetPointAttribValuesMatrix3
  setByName := geo_SetPointAttribValuesByNameMatrix3
  find := apexPanic! "Find attribute is not supported for this type of attribute!"

instance : PrimAttrib Matrix3 where
  get := geo_PrimAttribValueMatrix3
  set := geo_SetPrimAttribValueMatrix3
  setAll := geo_SetPrimAttribValuesMatrix3
  setByName := geo_SetPrimAttribValuesByNameMatrix3
  find := apexPanic! "Find attribute is not supported for this type of attribute!"

instance : VertexAttrib Matrix3 where
  get := geo_VertexAttribValueMatrix3
  set := geo_SetVertexAttribValueMatrix3
  setAll := geo_SetVertexAttribValuesMatrix3
  setByName := geo_SetVertexAttribValuesByNameMatrix3
  find := apexPanic! "Find attribute is not supported for this type of attribute!"

-- Matrix4 instances
instance : DetailAttrib Matrix4 where
  get := geo_DetailAttribValueMatrix4
  set := geo_SetDetailAttribValueMatrix4

instance : PointAttrib Matrix4 where
  get := geo_PointAttribValueMatrix4
  set := geo_SetPointAttribValueMatrix4
  setAll := geo_SetPointAttribValuesMatrix4
  setByName := geo_SetPointAttribValuesByNameMatrix4
  find := apexPanic! "Find attribute is not supported for this type of attribute!"

instance : PrimAttrib Matrix4 where
  get := geo_PrimAttribValueMatrix4
  set := geo_SetPrimAttribValueMatrix4
  setAll := geo_SetPrimAttribValuesMatrix4
  setByName := geo_SetPrimAttribValuesByNameMatrix4
  find := apexPanic! "Find attribute is not supported for this type of attribute!"

instance : VertexAttrib Matrix4 where
  get := geo_VertexAttribValueMatrix4
  set := geo_SetVertexAttribValueMatrix4
  setAll := geo_SetVertexAttribValuesMatrix4
  setByName := geo_SetVertexAttribValuesByNameMatrix4
  find := apexPanic! "Find attribute is not supported for this type of attribute!"

-- Dict instances
instance : DetailAttrib Dict where
  get := geo_DetailAttribValueDict
  set := geo_SetDetailAttribValueDict

instance : PointAttrib Dict where
  get := geo_PointAttribValueDict
  set := geo_SetPointAttribValueDict
  setAll := geo_SetPointAttribValuesDict
  setByName := geo_SetPointAttribValuesByNameDict
  find := apexPanic! "Find attribute is not supported for this type of attribute!"

instance : PrimAttrib Dict where
  get := geo_PrimAttribValueDict
  set := geo_SetPrimAttribValueDict
  setAll := geo_SetPrimAttribValuesDict
  setByName := geo_SetPrimAttribValuesByNameDict
  find := apexPanic! "Find attribute is not supported for this type of attribute!"

instance : VertexAttrib Dict where
  get := geo_VertexAttribValueDict
  set := geo_SetVertexAttribValueDict
  setAll := geo_SetVertexAttribValuesDict
  setByName := geo_SetVertexAttribValuesByNameDict
  find := apexPanic! "Find attribute is not supported for this type of attribute!"

-- Array type instances (abbreviated - similar pattern for all array types)
instance : DetailAttrib IntArray where
  get := geo_DetailAttribValueIntArray
  set := geo_SetDetailAttribValueIntArray

instance : PointAttrib IntArray where
  get := geo_PointAttribValueIntArray
  set := geo_SetPointAttribValueIntArray
  setAll := geo_SetPointAttribValuesIntArray
  setByName := geo_SetPointAttribValuesByNameIntArray
  find := apexPanic! "Find attribute is not supported for this type of attribute!"

instance : PrimAttrib IntArray where
  get := geo_PrimAttribValueIntArray
  set := geo_SetPrimAttribValueIntArray
  setAll := geo_SetPrimAttribValuesIntArray
  setByName := geo_SetPrimAttribValuesByNameIntArray
  find := apexPanic! "Find attribute is not supported for type of attribute!"

instance : VertexAttrib IntArray where
  get := geo_VertexAttribValueIntArray
  set := geo_SetVertexAttribValueIntArray
  setAll := geo_SetVertexAttribValuesIntArray
  setByName := geo_SetVertexAttribValuesByNameIntArray
  find := apexPanic! "Find attribute is not supported for type of attribute!"

-- (Continue for FloatArray, StringArray, Vector2/3/4Array, Matrix3/4Array, DictArray, DictArray)
instance : DetailAttrib FloatArray where
  get := geo_DetailAttribValueFloatArray
  set := geo_SetDetailAttribValueFloatArray

instance : PointAttrib FloatArray where
  get := geo_PointAttribValueFloatArray
  set := geo_SetPointAttribValueFloatArray
  setAll := geo_SetPointAttribValuesFloatArray
  setByName := geo_SetPointAttribValuesByNameFloatArray
  find := apexPanic! "Find attribute is not supported for type of attribute!"

instance : PrimAttrib FloatArray where
  get := geo_PrimAttribValueFloatArray
  set := geo_SetPrimAttribValueFloatArray
  setAll := geo_SetPrimAttribValuesFloatArray
  setByName := geo_SetPrimAttribValuesByNameFloatArray
  find := apexPanic! "Find attribute is not supported for type of attribute!"

instance : VertexAttrib FloatArray where
  get := geo_VertexAttribValueFloatArray
  set := geo_SetVertexAttribValueFloatArray
  setAll := geo_SetVertexAttribValuesFloatArray
  setByName := geo_SetVertexAttribValuesByNameFloatArray
  find := apexPanic! "Find attribute is not supported for type of attribute!"

instance : DetailAttrib StringArray where
  get := geo_DetailAttribValueStringArray
  set := geo_SetDetailAttribValueStringArray

instance : PointAttrib StringArray where
  get := geo_PointAttribValueStringArray
  set := geo_SetPointAttribValueStringArray
  setAll := geo_SetPointAttribValuesStringArray
  setByName := geo_SetPointAttribValuesByNameStringArray
  find := apexPanic! "Find attribute is not supported for type of attribute!"

instance : PrimAttrib StringArray where
  get := geo_PrimAttribValueStringArray
  set := geo_SetPrimAttribValueStringArray
  setAll := geo_SetPrimAttribValuesStringArray
  setByName := geo_SetPrimAttribValuesByNameStringArray
  find := apexPanic! "Find attribute is not supported for type of attribute!"

instance : VertexAttrib StringArray where
  get := geo_VertexAttribValueStringArray
  set := geo_SetVertexAttribValueStringArray
  setAll := geo_SetVertexAttribValuesStringArray
  setByName := geo_SetVertexAttribValuesByNameStringArray
  find := apexPanic! "Find attribute is not supported for type of attribute!"

instance : DetailAttrib DictArray where
  get := geo_DetailAttribValueDictArray
  set := geo_SetDetailAttribValueDictArray

instance : PointAttrib DictArray where
  get := geo_PointAttribValueDictArray
  set := geo_SetPointAttribValueDictArray
  setAll := geo_SetPointAttribValuesDictArray
  setByName := geo_SetPointAttribValuesByNameDictArray
  find := apexPanic! "Find attribute is not supported for type of attribute!"

instance : PrimAttrib DictArray where
  get := geo_PrimAttribValueDictArray
  set := geo_SetPrimAttribValueDictArray
  setAll := geo_SetPrimAttribValuesDictArray
  setByName := geo_SetPrimAttribValuesByNameDictArray
  find := apexPanic! "Find attribute is not supported for type of attribute!"

instance : VertexAttrib DictArray where
  get := geo_VertexAttribValueDictArray
  set := geo_SetVertexAttribValueDictArray
  setAll := geo_SetVertexAttribValuesDictArray
  setByName := geo_SetVertexAttribValuesByNameDictArray
  find := apexPanic! "Find attribute is not supported for type of attribute!"

-- High-level Geometry API
namespace Geometry

-- Basic queries
@[apex_unfold] def numPoints (g : Geometry) : Int := geo_NumPoints g
@[apex_unfold] def numPrims (g : Geometry) : Int := geo_NumPrims g

-- Detail attributes
@[apex_unfold] def detailAttrib [DetailAttrib α] (g : Geometry) (name : String) : α :=
  (DetailAttrib.get g name).1

@[apex_unfold] def detailAttrib? [DetailAttrib α] (g : Geometry) (name : String) : Option α :=
  toOption (DetailAttrib.get g name)

@[apex_unfold] def setDetailAttrib [DetailAttrib α] (g : Geometry) (name : String) (val : α) : Geometry :=
  (DetailAttrib.set g name val).1

-- Point attributes
@[apex_unfold] def pointAttrib [PointAttrib α] (g : Geometry) (pt : Int) (name : String) : α :=
  (PointAttrib.get g pt name).1

@[apex_unfold] def pointAttrib? [PointAttrib α] (g : Geometry) (pt : Int) (name : String) : Option α :=
  toOption (PointAttrib.get g pt name)

@[apex_unfold] def setPointAttrib [PointAttrib α] (g : Geometry) (pt : Int) (name : String) (val : α) : Geometry :=
  (PointAttrib.set g pt name val).1

@[apex_unfold] def setPointAttribs [PointAttrib α] (g : Geometry) (group : String) (name : String) (val : α) : Geometry :=
  (PointAttrib.setAll g group name val).1

@[apex_unfold] def setPointAttribsByName [PointAttrib α] (g : Geometry) (nameAttrib : String) (attrib : String) {n : Nat} (vals : VariadicArg α n) : Geometry :=
  PointAttrib.setByName g nameAttrib attrib vals

@[apex_unfold] def findPointAttrib [PointAttrib α] (g : Geometry) (name : String) (val : α) : IntArray :=
  (PointAttrib.find g name val).1

-- Primitive attributes
@[apex_unfold] def primAttrib [PrimAttrib α] (g : Geometry) (prim : Int) (name : String) : α :=
  (PrimAttrib.get g prim name).1

@[apex_unfold] def primAttrib? [PrimAttrib α] (g : Geometry) (prim : Int) (name : String) : Option α :=
  toOption (PrimAttrib.get g prim name)

@[apex_unfold] def setPrimAttrib [PrimAttrib α] (g : Geometry) (prim : Int) (name : String) (val : α) : Geometry :=
  (PrimAttrib.set g prim name val).1

@[apex_unfold] def setPrimAttribs [PrimAttrib α] (g : Geometry) (group : String) (name : String) (val : α) : Geometry :=
  (PrimAttrib.setAll g group name val).1

@[apex_unfold] def setPrimAttribsByName [PrimAttrib α] (g : Geometry) (nameAttrib : String) (attrib : String) {n : Nat} (vals : VariadicArg α n) : Geometry :=
  PrimAttrib.setByName g nameAttrib attrib vals

@[apex_unfold] def findPrimAttrib [PrimAttrib α] (g : Geometry) (name : String) (val : α) : IntArray :=
  (PrimAttrib.find g name val).1

-- Vertex attributes
@[apex_unfold] def vertexAttrib [VertexAttrib α] (g : Geometry) (vtx : Int) (name : String) : α :=
  (VertexAttrib.get g vtx name).1

@[apex_unfold] def vertexAttrib? [VertexAttrib α] (g : Geometry) (vtx : Int) (name : String) : Option α :=
  toOption (VertexAttrib.get g vtx name)

@[apex_unfold] def setVertexAttrib [VertexAttrib α] (g : Geometry) (vtx : Int) (name : String) (val : α) : Geometry :=
  (VertexAttrib.set g vtx name val).1

@[apex_unfold] def setVertexAttribs [VertexAttrib α] (g : Geometry) (group : String) (name : String) (val : α) : Geometry :=
  (VertexAttrib.setAll g group name val).1

@[apex_unfold] def setVertexAttribsByName [VertexAttrib α] (g : Geometry) (nameAttrib : String) (attrib : String) {n : Nat} (vals : VariadicArg α n) : Geometry :=
  VertexAttrib.setByName g nameAttrib attrib vals

@[apex_unfold] def findVertexAttrib [VertexAttrib α] (g : Geometry) (name : String) (val : α) : IntArray :=
  (VertexAttrib.find g name val).1

-- Topology queries
@[apex_unfold] def pointPrims (g : Geometry) (pt : Int) : IntArray := geo_PointPrims g pt
@[apex_unfold] def primPoints (g : Geometry) (prim : Int) : IntArray := geo_PrimPoints g prim

-- Pattern matching / globbing
@[apex_unfold] def globPoints (g : Geometry) (pattern : String) (ordered : Bool := false) : IntArray :=
  geo_GlobPoints g pattern ordered

@[apex_unfold] def globPrims (g : Geometry) (pattern : String) (ordered : Bool := false) : IntArray :=
  geo_GlobPrims g pattern ordered

-- Geometry operations
@[apex_unfold] def merge (g : Geometry) {n : Nat} (sources : VariadicArg Geometry n) : Geometry :=
  geo_Merge g sources

@[apex_unfold] def mergePacked {n : Nat} (sources : VariadicArg Geometry n) : Geometry :=
  geo_MergePacked sources

@[apex_unfold] def replace (g : Geometry) (src : Geometry) : Geometry := geo_Replace g src

@[apex_unfold] def transform (g : Geometry) (xform : Matrix4) : Geometry := geo_Transform g xform

@[apex_unfold] def copyDetailAttrib (g : Geometry) (src : Geometry) (attrib : String) : Geometry :=
  (geo_CopyDetailAttrib g src attrib).1

-- Point manipulation
@[apex_unfold] def dragPoints (g : Geometry) (pts : IntArray) (weights : FloatArray) (delta : Vector3) : Geometry :=
  geo_DragPoints g pts weights delta

@[apex_unfold] def displacePoints (g : Geometry) (refGeo : Geometry) (pts : IntArray) (weights : FloatArray) (strength : Float) (normal : Vector3) : Geometry :=
  geo_DisplacePoints g refGeo pts weights strength normal

@[apex_unfold] def smoothPoints (g : Geometry) (pts : IntArray) (weights : FloatArray) (strength : Float) : Geometry :=
  geo_SmoothPoints g pts weights strength

-- Packed geometry
@[apex_unfold] def addPacked (g : Geometry) (targetPt : Int) (src : Geometry) : Geometry × Int × Int :=
  geo_AddPacked g targetPt src

@[apex_unfold] def extractPackedGeo (g : Geometry) (primNum : Int) : Geometry × Geometry × Int :=
  geo_ExtractPackedGeo g primNum

@[apex_unfold] def updatePackedGeo (g : Geometry) (embedded : Geometry) (primNum : Int) : Geometry :=
  geo_UpdatePackedGeo g embedded primNum

-- Bounding box
@[apex_unfold] def boundingBox (g : Geometry) (orient : Bool := false) (local' : Bool := false) : Vector3 × Vector3 × Vector3 × Vector3 × Matrix4 :=
  geo_BoundingBox g orient local'

-- Load from disk
@[apex_unfold] def fromDisk (filepath : String) (primname : String := "") : Geometry :=
  geo_FromDisk filepath primname

-- @[Apex_unfold] Deformation
@[apex_unfold] def lattice (g : Geometry) (restGeo : Geometry) (deformedGeo : Geometry) 
            (divsX divsY divsZ : Int) (group : String := "") 
            (interpType : Int := 0) (updateNormals : Bool := true) : Geometry :=
  geo_Lattice g restGeo deformedGeo divsX divsY divsZ group interpType updateNormals

-- Agent operations
@[apex_unfold] def setAgentTransforms (g : Geometry) (primNum : Int) {n : Nat} (transforms : VariadicArg Matrix4 n) : Geometry :=
  geo_SetAgentTransforms g primNum transforms

end Geometry
