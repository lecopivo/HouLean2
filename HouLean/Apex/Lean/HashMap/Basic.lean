import HouLean.Apex.Lean.Array.ArrayType

namespace HouLean.Apex

open Generated Compiler Std

-- dict_GetAnimChannel dict_GetAnimChannelArray dict_GetAnimChannelCollection dict_GetAnimChannelCollectionArray dict_GetAnimStack dict_GetAnimStackArray dict_GetApexGraphHandle dict_GetApexGraphHandleArray dict_GetApexNodeID dict_GetApexNodeIDArray dict_GetApexPortID dict_GetApexPortIDArray dict_GetBool dict_GetBoolArray dict_GetColorRamp dict_GetColorRampArray dict_GetDict dict_GetDictArray dict_GetDynamicPath dict_GetDynamicPathArray dict_GetFBIKSkeleton dict_GetFBIKSkeletonArray dict_GetFBIKSolver dict_GetFBIKSolverArray dict_GetFBIKTarget dict_GetFBIKTargetArray dict_GetFloat dict_GetFloatArray dict_GetFloatRamp dict_GetFloatRampArray dict_GetGeometry dict_GetGeometryArray dict_GetInt dict_GetIntArray dict_GetMatrix3 dict_GetMatrix3Array dict_GetMatrix4 dict_GetMatrix4Array dict_GetSimRootDataId dict_GetSimRootDataIdArray dict_GetString dict_GetStringArray dict_GetVector2 dict_GetVector2Array dict_GetVector3 dict_GetVector3Array dict_GetVector4 dict_GetVector4Array

-- dict_GetNestedAnimChannel dict_GetNestedAnimChannelCollection dict_GetNestedAnimStack dict_GetNestedApexGraphHandle dict_GetNestedApexNodeID dict_GetNestedApexPortID dict_GetNestedBool dict_GetNestedColorRamp dict_GetNestedDict dict_GetNestedDynamicPath dict_GetNestedFBIKSkeleton dict_GetNestedFBIKSolver dict_GetNestedFBIKTarget dict_GetNestedFloat dict_GetNestedFloatRamp dict_GetNestedGeometry dict_GetNestedInt dict_GetNestedMatrix3 dict_GetNestedMatrix4 dict_GetNestedSimRootDataId dict_GetNestedString dict_GetNestedVector2 dict_GetNestedVector3 dict_GetNestedVector4

-- dict_Build dict_Contains dict_DebugDataIds dict_Insert dict_Keys dict_PatternRenameKeys dict_Remove dict_RenameKeys dict_Transfer dict_Update

-- dict_SetAnimChannel dict_SetAnimChannelArray dict_SetAnimChannelCollection dict_SetAnimChannelCollectionArray dict_SetAnimStack dict_SetAnimStackArray dict_SetApexGraphHandle dict_SetApexGraphHandleArray dict_SetApexNodeID dict_SetApexNodeIDArray dict_SetApexPortID dict_SetApexPortIDArray dict_SetBool dict_SetBoolArray dict_SetColorRamp dict_SetColorRampArray dict_SetDict dict_SetDictArray dict_SetDynamicPath dict_SetDynamicPathArray dict_SetFBIKSkeleton dict_SetFBIKSkeletonArray dict_SetFBIKSolver dict_SetFBIKSolverArray dict_SetFBIKTarget dict_SetFBIKTargetArray dict_SetFloat dict_SetFloatArray dict_SetFloatRamp dict_SetFloatRampArray dict_SetGeometry dict_SetGeometryArray dict_SetInt dict_SetIntArray dict_SetMatrix3 dict_SetMatrix3Array dict_SetMatrix4 dict_SetMatrix4Array dict_SetSimRootDataId dict_SetSimRootDataIdArray dict_SetString dict_SetStringArray dict_SetVector2 dict_SetVector2Array dict_SetVector3 dict_SetVector3Array dict_SetVector4 dict_SetVector4Array dict_SetNestedAnimChannel dict_SetNestedAnimChannelCollection dict_SetNestedAnimStack dict_SetNestedApexGraphHandle dict_SetNestedApexNodeID dict_SetNestedApexPortID dict_SetNestedBool dict_SetNestedColorRamp dict_SetNestedDict dict_SetNestedDynamicPath dict_SetNestedFBIKSkeleton dict_SetNestedFBIKSolver dict_SetNestedFBIKTarget dict_SetNestedFloat dict_SetNestedFloatRamp dict_SetNestedGeometry dict_SetNestedInt dict_SetNestedMatrix3 dict_SetNestedMatrix4 dict_SetNestedSimRootDataId dict_SetNestedString dict_SetNestedVector2 dict_SetNestedVector3 dict_SetNestedVector4

class DictType (α : Type u) extends ApexType (HashMap String α) Dict where
  get? (dict : Dict) (key : String) : α×Bool
  set (dict : Dict) (key : String) (value : α) : Dict

instance : DictType Bool where
  toApex := cast sorry_proof ()
  fromApex := cast sorry_proof ()
  get? dict key := dict_GetBool dict key default
  set dict key value := dict_SetBool dict key value

instance : DictType Int where
  toApex := cast sorry_proof ()
  fromApex := cast sorry_proof ()
  get? dict key := dict_GetInt dict key default
  set dict key value := dict_SetInt dict key value

instance : DictType Float where
  toApex := cast sorry_proof ()
  fromApex := cast sorry_proof ()
  get? dict key := dict_GetFloat dict key default
  set dict key value := dict_SetFloat dict key value

instance : DictType String where
  toApex := cast sorry_proof ()
  fromApex := cast sorry_proof ()
  get? dict key := dict_GetString dict key default
  set dict key value := dict_SetString dict key value

instance : DictType Vector2 where
  toApex := cast sorry_proof ()
  fromApex := cast sorry_proof ()
  get? dict key := dict_GetVector2 dict key default
  set dict key value := dict_SetVector2 dict key value

instance : DictType Vector3 where
  toApex := cast sorry_proof ()
  fromApex := cast sorry_proof ()
  get? dict key := dict_GetVector3 dict key default
  set dict key value := dict_SetVector3 dict key value

instance : DictType Vector4 where
  toApex := cast sorry_proof ()
  fromApex := cast sorry_proof ()
  get? dict key := dict_GetVector4 dict key default
  set dict key value := dict_SetVector4 dict key value

-- instance : DictType Matrix2 Matrix2 where
--   toApex := cast sorry_proof ()
--   fromApex := cast sorry_proof ()
--   get? dict key := dict_GetMatrix2 dict key default
--   set dict key value := dict_SetMatrix2 dict key value

instance : DictType Matrix3 where
  toApex := cast sorry_proof ()
  fromApex := cast sorry_proof ()
  get? dict key := dict_GetMatrix3 dict key default
  set dict key value := dict_SetMatrix3 dict key value

instance : DictType Matrix4 where
  toApex := cast sorry_proof ()
  fromApex := cast sorry_proof ()
  get? dict key := dict_GetMatrix4 dict key default
  set dict key value := dict_SetMatrix4 dict key value

instance : DictType Geometry where
  toApex := cast sorry_proof ()
  fromApex := cast sorry_proof ()
  get? dict key := dict_GetGeometry dict key default
  set dict key value := dict_SetGeometry dict key value

instance : DictType (Array Float) where
  toApex := cast sorry_proof ()
  fromApex := cast sorry_proof ()
  get? dict key := fromApex (dict_GetFloatArray dict key default)
  set dict key value := dict_SetFloatArray dict key (toApex value)

instance {α} [DictType α] : DictType (HashMap String α) where
  toApex := cast sorry_proof ()
  fromApex := cast sorry_proof ()
  get? dict key := fromApex (dict_GetDict dict key default)
  set dict key value := dict_SetDict dict key (toApex value)


-- Implementations

variable {α : Type u} {A : Type v} [ApexType α A] [DictType α] [Inhabited α]

def _root_.Std.HashMap.get?.apex_impl (m : HashMap String α) (key : String) : Option α :=
  Maybe.toOption (DictType.get? (α:=α) (toApex m) key)

run_meta compilerExt.add (.implementedByName ``HashMap.get? ``HashMap.get?.apex_impl
  #[some 1, none, some 4, some 5]) .global

def _root_.Std.HashMap.insert.apex_impl (m : HashMap String α) (key : String) (value : α) : HashMap String α :=
  fromApex (DictType.set (α:=α) (toApex m) key value)

run_meta compilerExt.add (.implementedByName ``HashMap.insert ``HashMap.insert.apex_impl
  #[some 1, none, some 4, some 5, some 6]) .global

def _root_.Std.HashMap.contains.apex_impl (m : HashMap String α) (key : String) : Bool :=
  dict_Contains (toApex m) key

run_meta compilerExt.add (.implementedByName ``HashMap.contains ``HashMap.contains.apex_impl
  #[some 1, none, some 4, some 5]) .global

def _root_.Std.HashMap.keys.apex_impl (m : HashMap String α) : Array String :=
  fromApex (dict_Keys (toApex m))

run_meta compilerExt.add (.implementedByName ``HashMap.keys ``HashMap.keys.apex_impl
  #[some 1, none, some 4, some 5]) .global

-- #check HashMap.alter
-- #check HashMap.modify
