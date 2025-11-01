import HouLean.Init
import HouLean.Apex.ApexType
import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Apex.Lean.Option

namespace HouLean.Apex

open Generated 

-- todo: remove GetElem and SetElem and just have internal get/set
--       the type As is not supposed to be used by users so
--       there is not point in using these classes, plus they will
--       likelly cause havoc on product types
/-- `α` is an arbitary Lean type and `As` is an array Apex type -/
class ArrayType (α : Type) (As : outParam Type) extends 
  GetElem? As Int α (fun _ _ => True),
  SetElem As Int α,
  ApexType (Array α) As where

  empty : As

  -- Basic operations
  length : As → Int
  null : As → As

  -- Array building and modification
  build : {n : Nat} → VariadicArg α n → As
  append : As → α → As × Int
  insert : As → α → Int → As
  remove : As → Int → As
  clear : As → As
  extend : As → As → As
  reverse : As → As

  
-- Array-specific operations as individual typeclasses

class ArrayFind (α : Type) (arr : outParam Type) where
  arrayFind : arr → α → Int

class ArraySum (α : Type) (arr : outParam Type) where
  arraySum : arr → α

class ArrayLerp (α : Type) (arr : outParam Type) where
  arrayLerp : arr → arr → FloatArray → arr

class ArrayMin (α : Type) (arr : outParam Type) where
  arrayMin : arr → α × Int

class ArrayMax (α : Type) (arr : outParam Type) where
  arrayMax : arr → α × Int

class ArraySort (α : Type) (arr : outParam Type) where
  arraySort : arr → arr
  arraySortAndRemoveDuplicates : arr → arr

-- Array instances for all APEX array types

private def toOption {α} (a : α × Bool) : Option α := Maybe.toOption a

unsafe instance : ArrayType Bool BoolArray where
  getElem a i _ := (array_GetBool a i default).1
  getElem? a i := toOption (array_GetBool a i default)
  setElem a i v := (array_SetBool a i v).1
  empty := .default
  length := array_LengthBool
  null := NullBoolArray
  build := array_BuildBool
  append := array_AppendBool
  insert a v i := array_InsertBool a v i
  remove := array_RemoveBool
  clear := array_ClearBool
  extend := array_ExtendBool
  reverse := array_ReverseBool
  toApex xs := cast silentSorry xs
  fromApex xs := cast silentSorry xs

instance : ArrayFind Bool BoolArray where
  arrayFind := array_FindBool

instance : ArrayMin Bool BoolArray where
  arrayMin := array_MinBool

instance : ArrayMax Bool BoolArray where
  arrayMax := array_MaxBool

instance : ArraySort Bool BoolArray where
  arraySort := array_SortBool
  arraySortAndRemoveDuplicates := array_SortAndRemoveDuplicatesBool

unsafe instance : ArrayType Int IntArray where
  getElem a i _ := (array_GetInt a i default).1
  getElem? a i := toOption (array_GetInt a i default)
  setElem a i v := (array_SetInt a i v).1
  empty := .default
  length := array_LengthInt
  null := NullIntArray
  build := array_BuildInt
  append := array_AppendInt
  insert a v i := array_InsertInt a v i
  remove := array_RemoveInt
  clear := array_ClearInt
  extend := array_ExtendInt
  reverse := array_ReverseInt
  toApex xs := cast silentSorry xs
  fromApex xs := cast silentSorry xs


instance : ArrayFind Int IntArray where
  arrayFind := array_FindInt

instance : Add IntArray where
  add := array_AddInt

instance : Sub IntArray where
  sub := array_SubtractInt

instance : Mul IntArray where
  mul := array_MultiplyInt

instance : Div IntArray where
  div := array_DivideInt

instance : HMul IntArray Int IntArray where
  hMul := array_ScaleInt

instance : ArraySum Int IntArray where
  arraySum := array_SumInt

instance : ArrayMin Int IntArray where
  arrayMin := array_MinInt

instance : ArrayMax Int IntArray where
  arrayMax := array_MaxInt

instance : ArraySort Int IntArray where
  arraySort := array_SortInt
  arraySortAndRemoveDuplicates := array_SortAndRemoveDuplicatesInt

unsafe instance : ArrayType Float FloatArray where
  getElem a i _ := (array_GetFloat a i default).1
  getElem? a i := toOption (array_GetFloat a i default)
  setElem a i v := (array_SetFloat a i v).1
  empty := FloatArray.empty
  length := array_LengthFloat
  null := NullFloatArray
  build := array_BuildFloat
  append := array_AppendFloat
  insert a v i := array_InsertFloat a v i
  remove := array_RemoveFloat
  clear := array_ClearFloat
  extend := array_ExtendFloat
  reverse := array_ReverseFloat
  toApex xs := cast silentSorry xs
  fromApex xs := cast silentSorry xs


instance : ArrayFind Float FloatArray where
  arrayFind := array_FindFloat

instance : Add FloatArray where
  add := array_AddFloat

instance : Sub FloatArray where
  sub := array_SubtractFloat

instance : Mul FloatArray where
  mul := array_MultiplyFloat

instance : Div FloatArray where
  div := array_DivideFloat

instance : HMul FloatArray Float FloatArray where
  hMul := array_ScaleFloat

instance : ArraySum Float FloatArray where
  arraySum := array_SumFloat

instance : ArrayLerp Float FloatArray where
  arrayLerp := array_LerpFloat

instance : ArrayMin Float FloatArray where
  arrayMin := array_MinFloat

instance : ArrayMax Float FloatArray where
  arrayMax := array_MaxFloat

instance : ArraySort Float FloatArray where
  arraySort := array_SortFloat
  arraySortAndRemoveDuplicates := array_SortAndRemoveDuplicatesFloat

unsafe instance : ArrayType String StringArray where
  getElem a i _ := (array_GetString a i default).1
  getElem? a i := toOption (array_GetString a i default)
  setElem a i v := (array_SetString a i v).1
  empty := .default
  length := array_LengthString
  null := NullStringArray
  build := array_BuildString
  append := array_AppendString
  insert a v i := array_InsertString a v i
  remove := array_RemoveString
  clear := array_ClearString
  extend := array_ExtendString
  reverse := array_ReverseString
  toApex xs := cast silentSorry xs
  fromApex xs := cast silentSorry xs


instance : ArrayFind String StringArray where
  arrayFind := array_FindString

instance : ArrayMin String StringArray where
  arrayMin := array_MinString

instance : ArrayMax String StringArray where
  arrayMax := array_MaxString

instance : ArraySort String StringArray where
  arraySort := array_SortString
  arraySortAndRemoveDuplicates := array_SortAndRemoveDuplicatesString

unsafe instance : ArrayType Vector2 Vector2Array where
  getElem a i _ := (array_GetVector2 a i default).1
  getElem? a i := toOption (array_GetVector2 a i default)
  setElem a i v := (array_SetVector2 a i v).1
  empty := .default
  length := array_LengthVector2
  null := NullVector2Array
  build := array_BuildVector2
  append := array_AppendVector2
  insert a v i := array_InsertVector2 a v i
  remove := array_RemoveVector2
  clear := array_ClearVector2
  extend := array_ExtendVector2
  reverse := array_ReverseVector2
  toApex xs := cast silentSorry xs
  fromApex xs := cast silentSorry xs

instance : ArrayFind Vector2 Vector2Array where
  arrayFind := array_FindVector2

instance : Add Vector2Array where
  add := array_AddVector2

instance : Sub Vector2Array where
  sub := array_SubtractVector2

instance : Mul Vector2Array where
  mul := array_MultiplyVector2

instance : Div Vector2Array where
  div := array_DivideVector2

instance : HMul Vector2Array Vector2 Vector2Array where
  hMul := array_ScaleVector2

instance : ArraySum Vector2 Vector2Array where
  arraySum := array_SumVector2

instance : ArrayLerp Vector2 Vector2Array where
  arrayLerp := array_LerpVector2

instance : ArrayMin Vector2 Vector2Array where
  arrayMin := array_MinVector2

instance : ArrayMax Vector2 Vector2Array where
  arrayMax := array_MaxVector2

instance : ArraySort Vector2 Vector2Array where
  arraySort := array_SortVector2
  arraySortAndRemoveDuplicates := array_SortAndRemoveDuplicatesVector2

unsafe instance : ArrayType Vector3 Vector3Array where
  getElem a i _ := (array_GetVector3 a i default).1
  getElem? a i := toOption (array_GetVector3 a i default)
  setElem a i v := (array_SetVector3 a i v).1
  empty := .default
  length := array_LengthVector3
  null := NullVector3Array
  build := array_BuildVector3
  append := array_AppendVector3
  insert a v i := array_InsertVector3 a v i
  remove := array_RemoveVector3
  clear := array_ClearVector3
  extend := array_ExtendVector3
  reverse := array_ReverseVector3
  toApex xs := cast silentSorry xs
  fromApex xs := cast silentSorry xs

instance : ArrayFind Vector3 Vector3Array where
  arrayFind := array_FindVector3

instance : Add Vector3Array where
  add := array_AddVector3

instance : Sub Vector3Array where
  sub := array_SubtractVector3

instance : Mul Vector3Array where
  mul := array_MultiplyVector3

instance : Div Vector3Array where
  div := array_DivideVector3

instance : HMul Vector3Array Vector3 Vector3Array where
  hMul := array_ScaleVector3

instance : ArraySum Vector3 Vector3Array where
  arraySum := array_SumVector3

instance : ArrayLerp Vector3 Vector3Array where
  arrayLerp := array_LerpVector3

instance : ArrayMin Vector3 Vector3Array where
  arrayMin := array_MinVector3

instance : ArrayMax Vector3 Vector3Array where
  arrayMax := array_MaxVector3

instance : ArraySort Vector3 Vector3Array where
  arraySort := array_SortVector3
  arraySortAndRemoveDuplicates := array_SortAndRemoveDuplicatesVector3

unsafe instance : ArrayType Vector4 Vector4Array where
  getElem a i _ := (array_GetVector4 a i default).1
  getElem? a i := toOption (array_GetVector4 a i default)
  setElem a i v := (array_SetVector4 a i v).1
  empty := .default
  length := array_LengthVector4
  null := NullVector4Array
  build := array_BuildVector4
  append := array_AppendVector4
  insert a v i := array_InsertVector4 a v i
  remove := array_RemoveVector4
  clear := array_ClearVector4
  extend := array_ExtendVector4
  reverse := array_ReverseVector4
  toApex xs := cast silentSorry xs
  fromApex xs := cast silentSorry xs

instance : ArrayFind Vector4 Vector4Array where
  arrayFind := array_FindVector4

instance : Add Vector4Array where
  add := array_AddVector4

instance : Sub Vector4Array where
  sub := array_SubtractVector4

instance : Mul Vector4Array where
  mul := array_MultiplyVector4

instance : Div Vector4Array where
  div := array_DivideVector4

instance : HMul Vector4Array Vector4 Vector4Array where
  hMul := array_ScaleVector4

instance : ArraySum Vector4 Vector4Array where
  arraySum := array_SumVector4

instance : ArrayLerp Vector4 Vector4Array where
  arrayLerp := array_LerpVector4

instance : ArrayMin Vector4 Vector4Array where
  arrayMin := array_MinVector4

instance : ArrayMax Vector4 Vector4Array where
  arrayMax := array_MaxVector4

instance : ArraySort Vector4 Vector4Array where
  arraySort := array_SortVector4
  arraySortAndRemoveDuplicates := array_SortAndRemoveDuplicatesVector4

unsafe instance : ArrayType Matrix3 Matrix3Array where
  getElem a i _ := (array_GetMatrix3 a i default).1
  getElem? a i := toOption (array_GetMatrix3 a i default)
  setElem a i v := (array_SetMatrix3 a i v).1
  empty := .default
  length := array_LengthMatrix3
  null := NullMatrix3Array
  build := array_BuildMatrix3
  append := array_AppendMatrix3
  insert a v i := array_InsertMatrix3 a v i
  remove := array_RemoveMatrix3
  clear := array_ClearMatrix3
  extend := array_ExtendMatrix3
  reverse := array_ReverseMatrix3
  toApex xs := cast silentSorry xs
  fromApex xs := cast silentSorry xs

instance : ArrayFind Matrix3 Matrix3Array where
  arrayFind := array_FindMatrix3

instance : Add Matrix3Array where
  add := array_AddMatrix3

instance : Sub Matrix3Array where
  sub := array_SubtractMatrix3

instance : HMul Matrix3Array Matrix3 Matrix3Array where
  hMul := array_ScaleMatrix3

instance : ArraySum Matrix3 Matrix3Array where
  arraySum := array_SumMatrix3

instance : ArrayLerp Matrix3 Matrix3Array where
  arrayLerp := array_LerpMatrix3

unsafe instance : ArrayType Matrix4 Matrix4Array where
  getElem a i _ := (array_GetMatrix4 a i default).1
  getElem? a i := toOption (array_GetMatrix4 a i default)
  setElem a i v := (array_SetMatrix4 a i v).1
  empty := .default
  length := array_LengthMatrix4
  null := NullMatrix4Array
  build := array_BuildMatrix4
  append := array_AppendMatrix4
  insert a v i := array_InsertMatrix4 a v i
  remove := array_RemoveMatrix4
  clear := array_ClearMatrix4
  extend := array_ExtendMatrix4
  reverse := array_ReverseMatrix4
  toApex xs := cast silentSorry xs
  fromApex xs := cast silentSorry xs

instance : ArrayFind Matrix4 Matrix4Array where
  arrayFind := array_FindMatrix4

instance : Add Matrix4Array where
  add := array_AddMatrix4

instance : Sub Matrix4Array where
  sub := array_SubtractMatrix4

instance : HMul Matrix4Array Matrix4 Matrix4Array where
  hMul := array_ScaleMatrix4

instance : ArraySum Matrix4 Matrix4Array where
  arraySum := array_SumMatrix4

instance : ArrayLerp Matrix4 Matrix4Array where
  arrayLerp := array_LerpMatrix4

-- Simple arrays without arithmetic
unsafe instance : ArrayType Geometry GeometryArray where
  getElem a i _ := (array_GetGeometry a i default).1
  getElem? a i := toOption (array_GetGeometry a i default)
  setElem a i v := (array_SetGeometry a i v).1
  empty := .default
  length := array_LengthGeometry
  null := NullGeometryArray
  build := array_BuildGeometry
  append := array_AppendGeometry
  insert a v i := array_InsertGeometry a v i
  remove := array_RemoveGeometry
  clear := array_ClearGeometry
  extend := array_ExtendGeometry
  reverse := array_ReverseGeometry
  toApex xs := cast silentSorry xs
  fromApex xs := cast silentSorry xs

unsafe instance : ArrayType Dict DictArray where
  getElem a i _ := (array_GetDict a i default).1
  getElem? a i := toOption (array_GetDict a i default)
  setElem a i v := (array_SetDict a i v).1
  empty := .default
  length := array_LengthDict
  null := NullDictArray
  build := array_BuildDict
  append := array_AppendDict
  insert a v i := array_InsertDict a v i
  remove := array_RemoveDict
  clear := array_ClearDict
  extend := array_ExtendDict
  reverse := array_ReverseDict
  toApex xs := cast silentSorry xs
  fromApex xs := cast silentSorry xs

unsafe instance : ArrayType DynamicPath DynamicPathArray where
  getElem a i _ := (array_GetDynamicPath a i default).1
  getElem? a i := toOption (array_GetDynamicPath a i default)
  setElem a i v := (array_SetDynamicPath a i v).1
  empty := .default
  length := array_LengthDynamicPath
  null := NullDynamicPathArray
  build := array_BuildDynamicPath
  append := array_AppendDynamicPath
  insert a v i := array_InsertDynamicPath a v i
  remove := array_RemoveDynamicPath
  clear := array_ClearDynamicPath
  extend := array_ExtendDynamicPath
  reverse := array_ReverseDynamicPath
  toApex xs := cast silentSorry xs
  fromApex xs := cast silentSorry xs

unsafe instance : ArrayType ApexNodeID ApexNodeIDArray where
  getElem a i _ := (array_GetApexNodeID a i default).1
  getElem? a i := toOption (array_GetApexNodeID a i default)
  setElem a i v := (array_SetApexNodeID a i v).1
  empty := .default
  length := array_LengthApexNodeID
  null := NullApexNodeIDArray
  build := array_BuildApexNodeID
  append := array_AppendApexNodeID
  insert a v i := array_InsertApexNodeID a v i
  remove := array_RemoveApexNodeID
  clear := array_ClearApexNodeID
  extend := array_ExtendApexNodeID
  reverse := array_ReverseApexNodeID
  toApex xs := cast silentSorry xs
  fromApex xs := cast silentSorry xs

instance : ArrayFind ApexNodeID ApexNodeIDArray where
  arrayFind := array_FindApexNodeID

unsafe instance : ArrayType ApexPortID ApexPortIDArray where
  getElem a i _ := (array_GetApexPortID a i default).1
  getElem? a i := toOption (array_GetApexPortID a i default)
  setElem a i v := (array_SetApexPortID a i v).1
  empty := .default
  length := array_LengthApexPortID
  null := NullApexPortIDArray
  build := array_BuildApexPortID
  append := array_AppendApexPortID
  insert a v i := array_InsertApexPortID a v i
  remove := array_RemoveApexPortID
  clear := array_ClearApexPortID
  extend := array_ExtendApexPortID
  reverse := array_ReverseApexPortID
  toApex xs := cast silentSorry xs
  fromApex xs := cast silentSorry xs

instance : ArrayFind ApexPortID ApexPortIDArray where
  arrayFind := array_FindApexPortID

unsafe instance : ArrayType FBIKSkeleton FBIKSkeletonArray where
  getElem a i _ := (array_GetFBIKSkeleton a i default).1
  getElem? a i := toOption (array_GetFBIKSkeleton a i default)
  setElem a i v := (array_SetFBIKSkeleton a i v).1
  empty := .default
  length := array_LengthFBIKSkeleton
  null := NullFBIKSkeletonArray
  build := array_BuildFBIKSkeleton
  append := array_AppendFBIKSkeleton
  insert a v i := array_InsertFBIKSkeleton a v i
  remove := array_RemoveFBIKSkeleton
  clear := array_ClearFBIKSkeleton
  extend := array_ExtendFBIKSkeleton
  reverse := array_ReverseFBIKSkeleton
  toApex xs := cast silentSorry xs
  fromApex xs := cast silentSorry xs

unsafe instance : ArrayType FBIKSolver FBIKSolverArray where
  getElem a i _ := (array_GetFBIKSolver a i default).1
  getElem? a i := toOption (array_GetFBIKSolver a i default)
  setElem a i v := (array_SetFBIKSolver a i v).1
  empty := .default
  length := array_LengthFBIKSolver
  null := NullFBIKSolverArray
  build := array_BuildFBIKSolver
  append := array_AppendFBIKSolver
  insert a v i := array_InsertFBIKSolver a v i
  remove := array_RemoveFBIKSolver
  clear := array_ClearFBIKSolver
  extend := array_ExtendFBIKSolver
  reverse := array_ReverseFBIKSolver
  toApex xs := cast silentSorry xs
  fromApex xs := cast silentSorry xs

unsafe instance : ArrayType FBIKTarget FBIKTargetArray where
  getElem a i _ := (array_GetFBIKTarget a i default).1
  getElem? a i := toOption (array_GetFBIKTarget a i default)
  setElem a i v := (array_SetFBIKTarget a i v).1
  empty := .default
  length := array_LengthFBIKTarget
  null := NullFBIKTargetArray
  build := array_BuildFBIKTarget
  append := array_AppendFBIKTarget
  insert a v i := array_InsertFBIKTarget a v i
  remove := array_RemoveFBIKTarget
  clear := array_ClearFBIKTarget
  extend := array_ExtendFBIKTarget
  reverse := array_ReverseFBIKTarget
  toApex xs := cast silentSorry xs
  fromApex xs := cast silentSorry xs

unsafe instance : ArrayType SimRootDataId SimRootDataIdArray where
  getElem a i _ := (array_GetSimRootDataId a i default).1
  getElem? a i := toOption (array_GetSimRootDataId a i default)
  setElem a i v := (array_SetSimRootDataId a i v).1
  empty := .default
  length := array_LengthSimRootDataId
  null := NullSimRootDataIdArray
  build := array_BuildSimRootDataId
  append := array_AppendSimRootDataId
  insert a v i := array_InsertSimRootDataId a v i
  remove := array_RemoveSimRootDataId
  clear := array_ClearSimRootDataId
  extend := array_ExtendSimRootDataId
  reverse := array_ReverseSimRootDataId
  toApex xs := cast silentSorry xs
  fromApex xs := cast silentSorry xs

instance : ArrayFind SimRootDataId SimRootDataIdArray where
  arrayFind := array_FindSimRootDataId
