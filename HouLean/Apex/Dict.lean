import HouLean.Apex.Basic

namespace HouLean.Apex

open Generated

-- ============================================================================
-- Dictionary Interface
-- ============================================================================

-- Dict operations use type-specific get/set functions
-- We provide separate typeclasses for basic and nested operations

class DictValue (α : Type) where
  get : Dict → String → α → α × Bool
  set : Dict → String → α → Dict

class DictNestedValue (α : Type) where
  getNested : Dict → StringArray → α → α × Bool
  setNested : Dict → StringArray → α → Dict

-- Instances for primitive types (both DictValue AND DictNestedValue)

instance : DictValue Bool where
  get := dict_GetBool
  set := dict_SetBool

instance : DictNestedValue Bool where
  getNested := dict_GetNestedBool
  setNested := dict_SetNestedBool

instance : DictValue Int where
  get := dict_GetInt
  set := dict_SetInt

instance : DictNestedValue Int where
  getNested := dict_GetNestedInt
  setNested := dict_SetNestedInt

instance : DictValue Float where
  get := dict_GetFloat
  set := dict_SetFloat

instance : DictNestedValue Float where
  getNested := dict_GetNestedFloat
  setNested := dict_SetNestedFloat

instance : DictValue String where
  get := dict_GetString
  set := dict_SetString

instance : DictNestedValue String where
  getNested := dict_GetNestedString
  setNested := dict_SetNestedString

instance : DictValue Vector2 where
  get := dict_GetVector2
  set := dict_SetVector2

instance : DictNestedValue Vector2 where
  getNested := dict_GetNestedVector2
  setNested := dict_SetNestedVector2

instance : DictValue Vector3 where
  get := dict_GetVector3
  set := dict_SetVector3

instance : DictNestedValue Vector3 where
  getNested := dict_GetNestedVector3
  setNested := dict_SetNestedVector3

instance : DictValue Vector4 where
  get := dict_GetVector4
  set := dict_SetVector4

instance : DictNestedValue Vector4 where
  getNested := dict_GetNestedVector4
  setNested := dict_SetNestedVector4

instance : DictValue Matrix3 where
  get := dict_GetMatrix3
  set := dict_SetMatrix3

instance : DictNestedValue Matrix3 where
  getNested := dict_GetNestedMatrix3
  setNested := dict_SetNestedMatrix3

instance : DictValue Matrix4 where
  get := dict_GetMatrix4
  set := dict_SetMatrix4

instance : DictNestedValue Matrix4 where
  getNested := dict_GetNestedMatrix4
  setNested := dict_SetNestedMatrix4

instance : DictValue Dict where
  get := dict_GetDict
  set := dict_SetDict

instance : DictNestedValue Dict where
  getNested := dict_GetNestedDict
  setNested := dict_SetNestedDict

instance : DictValue Geometry where
  get := dict_GetGeometry
  set := dict_SetGeometry

instance : DictNestedValue Geometry where
  getNested := dict_GetNestedGeometry
  setNested := dict_SetNestedGeometry

instance : DictValue DynamicPath where
  get := dict_GetDynamicPath
  set := dict_SetDynamicPath

instance : DictNestedValue DynamicPath where
  getNested := dict_GetNestedDynamicPath
  setNested := dict_SetNestedDynamicPath

instance : DictValue ApexNodeID where
  get := dict_GetApexNodeID
  set := dict_SetApexNodeID

instance : DictNestedValue ApexNodeID where
  getNested := dict_GetNestedApexNodeID
  setNested := dict_SetNestedApexNodeID

instance : DictValue ApexPortID where
  get := dict_GetApexPortID
  set := dict_SetApexPortID

instance : DictNestedValue ApexPortID where
  getNested := dict_GetNestedApexPortID
  setNested := dict_SetNestedApexPortID

instance : DictValue FBIKSkeleton where
  get := dict_GetFBIKSkeleton
  set := dict_SetFBIKSkeleton

instance : DictNestedValue FBIKSkeleton where
  getNested := dict_GetNestedFBIKSkeleton
  setNested := dict_SetNestedFBIKSkeleton

instance : DictValue FBIKSolver where
  get := dict_GetFBIKSolver
  set := dict_SetFBIKSolver

instance : DictNestedValue FBIKSolver where
  getNested := dict_GetNestedFBIKSolver
  setNested := dict_SetNestedFBIKSolver

instance : DictValue FBIKTarget where
  get := dict_GetFBIKTarget
  set := dict_SetFBIKTarget

instance : DictNestedValue FBIKTarget where
  getNested := dict_GetNestedFBIKTarget
  setNested := dict_SetNestedFBIKTarget

instance : DictValue AnimChannel where
  get := dict_GetAnimChannel
  set := dict_SetAnimChannel

instance : DictNestedValue AnimChannel where
  getNested := dict_GetNestedAnimChannel
  setNested := dict_SetNestedAnimChannel

instance : DictValue AnimChannelCollection where
  get := dict_GetAnimChannelCollection
  set := dict_SetAnimChannelCollection

instance : DictNestedValue AnimChannelCollection where
  getNested := dict_GetNestedAnimChannelCollection
  setNested := dict_SetNestedAnimChannelCollection

instance : DictValue AnimStack where
  get := dict_GetAnimStack
  set := dict_SetAnimStack

instance : DictNestedValue AnimStack where
  getNested := dict_GetNestedAnimStack
  setNested := dict_SetNestedAnimStack

instance : DictValue ApexGraphHandle where
  get := dict_GetApexGraphHandle
  set := dict_SetApexGraphHandle

instance : DictNestedValue ApexGraphHandle where
  getNested := dict_GetNestedApexGraphHandle
  setNested := dict_SetNestedApexGraphHandle

instance : DictValue ColorRamp where
  get := dict_GetColorRamp
  set := dict_SetColorRamp

instance : DictNestedValue ColorRamp where
  getNested := dict_GetNestedColorRamp
  setNested := dict_SetNestedColorRamp

instance : DictValue FloatRamp where
  get := dict_GetFloatRamp
  set := dict_SetFloatRamp

instance : DictNestedValue FloatRamp where
  getNested := dict_GetNestedFloatRamp
  setNested := dict_SetNestedFloatRamp

instance : DictValue SimRootDataId where
  get := dict_GetSimRootDataId
  set := dict_SetSimRootDataId

instance : DictNestedValue SimRootDataId where
  getNested := dict_GetNestedSimRootDataId
  setNested := dict_SetNestedSimRootDataId

-- Array types (ONLY DictValue, NO DictNestedValue instances)
instance : DictValue BoolArray where
  get := dict_GetBoolArray
  set := dict_SetBoolArray

instance : DictValue IntArray where
  get := dict_GetIntArray
  set := dict_SetIntArray

instance : DictValue FloatArray where
  get := dict_GetFloatArray
  set := dict_SetFloatArray

instance : DictValue StringArray where
  get := dict_GetStringArray
  set := dict_SetStringArray

instance : DictValue Vector2Array where
  get := dict_GetVector2Array
  set := dict_SetVector2Array

instance : DictValue Vector3Array where
  get := dict_GetVector3Array
  set := dict_SetVector3Array

instance : DictValue Vector4Array where
  get := dict_GetVector4Array
  set := dict_SetVector4Array

instance : DictValue Matrix3Array where
  get := dict_GetMatrix3Array
  set := dict_SetMatrix3Array

instance : DictValue Matrix4Array where
  get := dict_GetMatrix4Array
  set := dict_SetMatrix4Array

instance : DictValue GeometryArray where
  get := dict_GetGeometryArray
  set := dict_SetGeometryArray

instance : DictValue DictArray where
  get := dict_GetDictArray
  set := dict_SetDictArray

instance : DictValue DynamicPathArray where
  get := dict_GetDynamicPathArray
  set := dict_SetDynamicPathArray

instance : DictValue ApexNodeIDArray where
  get := dict_GetApexNodeIDArray
  set := dict_SetApexNodeIDArray

instance : DictValue ApexPortIDArray where
  get := dict_GetApexPortIDArray
  set := dict_SetApexPortIDArray

instance : DictValue FBIKSkeletonArray where
  get := dict_GetFBIKSkeletonArray
  set := dict_SetFBIKSkeletonArray

instance : DictValue FBIKSolverArray where
  get := dict_GetFBIKSolverArray
  set := dict_SetFBIKSolverArray

instance : DictValue FBIKTargetArray where
  get := dict_GetFBIKTargetArray
  set := dict_SetFBIKTargetArray

instance : DictValue AnimChannelArray where
  get := dict_GetAnimChannelArray
  set := dict_SetAnimChannelArray

instance : DictValue AnimChannelCollectionArray where
  get := dict_GetAnimChannelCollectionArray
  set := dict_SetAnimChannelCollectionArray

instance : DictValue AnimStackArray where
  get := dict_GetAnimStackArray
  set := dict_SetAnimStackArray

instance : DictValue ApexGraphHandleArray where
  get := dict_GetApexGraphHandleArray
  set := dict_SetApexGraphHandleArray

instance : DictValue ColorRampArray where
  get := dict_GetColorRampArray
  set := dict_SetColorRampArray

instance : DictValue FloatRampArray where
  get := dict_GetFloatRampArray
  set := dict_SetFloatRampArray

instance : DictValue SimRootDataIdArray where
  get := dict_GetSimRootDataIdArray
  set := dict_SetSimRootDataIdArray

-- High-level Dict API
namespace Dict

-- Get value with default
def get [DictValue α] (d : Dict) (key : String) (default : α) : α :=
  (DictValue.get d key default).1

-- Get value as Option
def get? [DictValue α] (d : Dict) (key : String) (default : α) : Option α :=
  toOption (DictValue.get d key default)

-- Get with full result (value and success flag)
def getChecked [DictValue α] (d : Dict) (key : String) (default : α) : α × Bool :=
  DictValue.get d key default

-- Set value
def set [DictValue α] (d : Dict) (key : String) (value : α) : Dict :=
  DictValue.set d key value

-- Get nested value with default (requires DictNestedValue)
def getNested [DictNestedValue α] (d : Dict) (keys : StringArray) (default : α) : α :=
  (DictNestedValue.getNested d keys default).1

-- Get nested value as Option (requires DictNestedValue)
def getNested? [DictNestedValue α] (d : Dict) (keys : StringArray) (default : α) : Option α :=
  toOption (DictNestedValue.getNested d keys default)

-- Set nested value (requires DictNestedValue)
def setNested [DictNestedValue α] (d : Dict) (keys : StringArray) (value : α) : Dict :=
  DictNestedValue.setNested d keys value

-- Check if key exists
def contains (d : Dict) (key : String) : Bool :=
  dict_Contains d key

-- Get all keys
def keys (d : Dict) : StringArray :=
  dict_Keys d

-- Build dict from variadic args
def build {n : Nat} (args : VariadicArg Untyped n) : Dict :=
  dict_Build default args

-- Insert multiple key-value pairs
def insert (d : Dict) {n : Nat} (args : VariadicArg Untyped n) : Dict :=
  dict_Insert d args

-- Remove keys matching pattern
def remove (d : Dict) (pattern : String) : Dict :=
  dict_Remove d pattern

-- Rename keys using a mapping dict
def renameKeys (d : Dict) (keymap : Dict) : Dict :=
  dict_RenameKeys d keymap

-- Rename keys using pattern
def patternRenameKeys (d : Dict) (keys : String) (pattern : String) : Dict :=
  dict_PatternRenameKeys d keys pattern

-- Transfer values from another dict using keymap
def transfer (d : Dict) (srcDict : Dict) (keymap : Dict) : Dict :=
  dict_Transfer d srcDict keymap

-- Update dict with other dicts
def update (d : Dict) {n : Nat} (others : VariadicArg Dict n) (addMissing : Bool := true) (changeType : Bool := true) : Dict :=
  dict_Update d others addMissing changeType

-- Debug data IDs
def debugDataIds (d : Dict) : Dict :=
  dict_DebugDataIds d

end Dict
