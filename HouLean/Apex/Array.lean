import HouLean.Apex.Basic

namespace HouLean.Apex

open Generated

-- ============================================================================
-- Generic Array Interface
-- ============================================================================

-- Note: APEX array Get/Set operations return tuples where Bool indicates success
-- We provide both unsafe indexing (GetElem) and safe indexing (GetElem?)

class ApexArray (α : outParam Type) (arr : Type) extends 
  GetElem arr Int α (fun _ _ => True),
  GetElem? arr Int α (fun _ _ => True) where
  -- Basic operations
  set : arr → Int → α → arr × Bool
  length : arr → Int
  null : arr → arr
  
  -- Array building and modification
  build : {n : Nat} → VariadicArg α n → arr
  append : arr → α → arr × Int
  insert : arr → α → Int → arr
  remove : arr → Int → arr
  clear : arr → arr
  extend : arr → arr → arr
  reverse : arr → arr

-- Array-specific operations as individual typeclasses

class ArrayFind (α : outParam Type) (arr : Type) where
  find : arr → α → Int

export ArrayFind (find)

class ArraySum (α : outParam Type) (arr : Type) where
  arraySum : arr → α

export ArraySum (arraySum)

class ArrayLerp (arr : Type) where
  lerp : arr → arr → FloatArray → arr

export ArrayLerp (lerp)

class ArrayMin (α : outParam Type) (arr : Type) where
  min : arr → α × Int

export ArrayMin (min)

class ArrayMax (α : outParam Type) (arr : Type) where
  max : arr → α × Int

export ArrayMax (max)

class ArraySort (arr : Type) where
  sort : arr → arr
  sortAndRemoveDuplicates : arr → arr

export ArraySort (sort sortAndRemoveDuplicates)

-- Array instances for all APEX array types

instance : ApexArray Bool BoolArray where
  getElem a i _ := (array_GetBool a i default).1
  getElem? a i := toOption (array_GetBool a i default)
  set a i v := array_SetBool a i v
  length := array_LengthBool
  null := NullBoolArray
  build := array_BuildBool
  append := array_AppendBool
  insert a v i := array_InsertBool a v i
  remove := array_RemoveBool
  clear := array_ClearBool
  extend := array_ExtendBool
  reverse := array_ReverseBool

instance : ArrayFind Bool BoolArray where
  find := array_FindBool

instance : ArrayMin Bool BoolArray where
  min := array_MinBool

instance : ArrayMax Bool BoolArray where
  max := array_MaxBool

instance : ArraySort BoolArray where
  sort := array_SortBool
  sortAndRemoveDuplicates := array_SortAndRemoveDuplicatesBool

instance : ApexArray Int IntArray where
  getElem a i _ := (array_GetInt a i default).1
  getElem? a i := toOption (array_GetInt a i default)
  set a i v := array_SetInt a i v
  length := array_LengthInt
  null := NullIntArray
  build := array_BuildInt
  append := array_AppendInt
  insert a v i := array_InsertInt a v i
  remove := array_RemoveInt
  clear := array_ClearInt
  extend := array_ExtendInt
  reverse := array_ReverseInt

instance : ArrayFind Int IntArray where
  find := array_FindInt

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
  min := array_MinInt

instance : ArrayMax Int IntArray where
  max := array_MaxInt

instance : ArraySort IntArray where
  sort := array_SortInt
  sortAndRemoveDuplicates := array_SortAndRemoveDuplicatesInt

instance : ApexArray Float FloatArray where
  getElem a i _ := (array_GetFloat a i default).1
  getElem? a i := toOption (array_GetFloat a i default)
  set a i v := array_SetFloat a i v
  length := array_LengthFloat
  null := NullFloatArray
  build := array_BuildFloat
  append := array_AppendFloat
  insert a v i := array_InsertFloat a v i
  remove := array_RemoveFloat
  clear := array_ClearFloat
  extend := array_ExtendFloat
  reverse := array_ReverseFloat

instance : ArrayFind Float FloatArray where
  find := array_FindFloat

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

instance : ArrayLerp FloatArray where
  lerp := array_LerpFloat

instance : ArrayMin Float FloatArray where
  min := array_MinFloat

instance : ArrayMax Float FloatArray where
  max := array_MaxFloat

instance : ArraySort FloatArray where
  sort := array_SortFloat
  sortAndRemoveDuplicates := array_SortAndRemoveDuplicatesFloat

instance : ApexArray String StringArray where
  getElem a i _ := (array_GetString a i default).1
  getElem? a i := toOption (array_GetString a i default)
  set a i v := array_SetString a i v
  length := array_LengthString
  null := NullStringArray
  build := array_BuildString
  append := array_AppendString
  insert a v i := array_InsertString a v i
  remove := array_RemoveString
  clear := array_ClearString
  extend := array_ExtendString
  reverse := array_ReverseString

instance : ArrayFind String StringArray where
  find := array_FindString

instance : ArrayMin String StringArray where
  min := array_MinString

instance : ArrayMax String StringArray where
  max := array_MaxString

instance : ArraySort StringArray where
  sort := array_SortString
  sortAndRemoveDuplicates := array_SortAndRemoveDuplicatesString

instance : ApexArray Vector2 Vector2Array where
  getElem a i _ := (array_GetVector2 a i default).1
  getElem? a i := toOption (array_GetVector2 a i default)
  set a i v := array_SetVector2 a i v
  length := array_LengthVector2
  null := NullVector2Array
  build := array_BuildVector2
  append := array_AppendVector2
  insert a v i := array_InsertVector2 a v i
  remove := array_RemoveVector2
  clear := array_ClearVector2
  extend := array_ExtendVector2
  reverse := array_ReverseVector2

instance : ArrayFind Vector2 Vector2Array where
  find := array_FindVector2

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

instance : ArrayLerp Vector2Array where
  lerp := array_LerpVector2

instance : ArrayMin Vector2 Vector2Array where
  min := array_MinVector2

instance : ArrayMax Vector2 Vector2Array where
  max := array_MaxVector2

instance : ArraySort Vector2Array where
  sort := array_SortVector2
  sortAndRemoveDuplicates := array_SortAndRemoveDuplicatesVector2

instance : ApexArray Vector3 Vector3Array where
  getElem a i _ := (array_GetVector3 a i default).1
  getElem? a i := toOption (array_GetVector3 a i default)
  set a i v := array_SetVector3 a i v
  length := array_LengthVector3
  null := NullVector3Array
  build := array_BuildVector3
  append := array_AppendVector3
  insert a v i := array_InsertVector3 a v i
  remove := array_RemoveVector3
  clear := array_ClearVector3
  extend := array_ExtendVector3
  reverse := array_ReverseVector3

instance : ArrayFind Vector3 Vector3Array where
  find := array_FindVector3

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

instance : ArrayLerp Vector3Array where
  lerp := array_LerpVector3

instance : ArrayMin Vector3 Vector3Array where
  min := array_MinVector3

instance : ArrayMax Vector3 Vector3Array where
  max := array_MaxVector3

instance : ArraySort Vector3Array where
  sort := array_SortVector3
  sortAndRemoveDuplicates := array_SortAndRemoveDuplicatesVector3

instance : ApexArray Vector4 Vector4Array where
  getElem a i _ := (array_GetVector4 a i default).1
  getElem? a i := toOption (array_GetVector4 a i default)
  set a i v := array_SetVector4 a i v
  length := array_LengthVector4
  null := NullVector4Array
  build := array_BuildVector4
  append := array_AppendVector4
  insert a v i := array_InsertVector4 a v i
  remove := array_RemoveVector4
  clear := array_ClearVector4
  extend := array_ExtendVector4
  reverse := array_ReverseVector4

instance : ArrayFind Vector4 Vector4Array where
  find := array_FindVector4

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

instance : ArrayLerp Vector4Array where
  lerp := array_LerpVector4

instance : ArrayMin Vector4 Vector4Array where
  min := array_MinVector4

instance : ArrayMax Vector4 Vector4Array where
  max := array_MaxVector4

instance : ArraySort Vector4Array where
  sort := array_SortVector4
  sortAndRemoveDuplicates := array_SortAndRemoveDuplicatesVector4

instance : ApexArray Matrix3 Matrix3Array where
  getElem a i _ := (array_GetMatrix3 a i default).1
  getElem? a i := toOption (array_GetMatrix3 a i default)
  set a i v := array_SetMatrix3 a i v
  length := array_LengthMatrix3
  null := NullMatrix3Array
  build := array_BuildMatrix3
  append := array_AppendMatrix3
  insert a v i := array_InsertMatrix3 a v i
  remove := array_RemoveMatrix3
  clear := array_ClearMatrix3
  extend := array_ExtendMatrix3
  reverse := array_ReverseMatrix3

instance : ArrayFind Matrix3 Matrix3Array where
  find := array_FindMatrix3

instance : Add Matrix3Array where
  add := array_AddMatrix3

instance : Sub Matrix3Array where
  sub := array_SubtractMatrix3

instance : HMul Matrix3Array Matrix3 Matrix3Array where
  hMul := array_ScaleMatrix3

instance : ArraySum Matrix3 Matrix3Array where
  arraySum := array_SumMatrix3

instance : ArrayLerp Matrix3Array where
  lerp := array_LerpMatrix3

instance : ApexArray Matrix4 Matrix4Array where
  getElem a i _ := (array_GetMatrix4 a i default).1
  getElem? a i := toOption (array_GetMatrix4 a i default)
  set a i v := array_SetMatrix4 a i v
  length := array_LengthMatrix4
  null := NullMatrix4Array
  build := array_BuildMatrix4
  append := array_AppendMatrix4
  insert a v i := array_InsertMatrix4 a v i
  remove := array_RemoveMatrix4
  clear := array_ClearMatrix4
  extend := array_ExtendMatrix4
  reverse := array_ReverseMatrix4

instance : ArrayFind Matrix4 Matrix4Array where
  find := array_FindMatrix4

instance : Add Matrix4Array where
  add := array_AddMatrix4

instance : Sub Matrix4Array where
  sub := array_SubtractMatrix4

instance : HMul Matrix4Array Matrix4 Matrix4Array where
  hMul := array_ScaleMatrix4

instance : ArraySum Matrix4 Matrix4Array where
  arraySum := array_SumMatrix4

instance : ArrayLerp Matrix4Array where
  lerp := array_LerpMatrix4

-- Simple arrays without arithmetic
instance : ApexArray Geometry GeometryArray where
  getElem a i _ := (array_GetGeometry a i default).1
  getElem? a i := toOption (array_GetGeometry a i default)
  set a i v := array_SetGeometry a i v
  length := array_LengthGeometry
  null := NullGeometryArray
  build := array_BuildGeometry
  append := array_AppendGeometry
  insert a v i := array_InsertGeometry a v i
  remove := array_RemoveGeometry
  clear := array_ClearGeometry
  extend := array_ExtendGeometry
  reverse := array_ReverseGeometry

instance : ApexArray Dict DictArray where
  getElem a i _ := (array_GetDict a i default).1
  getElem? a i := toOption (array_GetDict a i default)
  set a i v := array_SetDict a i v
  length := array_LengthDict
  null := NullDictArray
  build := array_BuildDict
  append := array_AppendDict
  insert a v i := array_InsertDict a v i
  remove := array_RemoveDict
  clear := array_ClearDict
  extend := array_ExtendDict
  reverse := array_ReverseDict

instance : ApexArray DynamicPath DynamicPathArray where
  getElem a i _ := (array_GetDynamicPath a i default).1
  getElem? a i := toOption (array_GetDynamicPath a i default)
  set a i v := array_SetDynamicPath a i v
  length := array_LengthDynamicPath
  null := NullDynamicPathArray
  build := array_BuildDynamicPath
  append := array_AppendDynamicPath
  insert a v i := array_InsertDynamicPath a v i
  remove := array_RemoveDynamicPath
  clear := array_ClearDynamicPath
  extend := array_ExtendDynamicPath
  reverse := array_ReverseDynamicPath

instance : ApexArray ApexNodeID ApexNodeIDArray where
  getElem a i _ := (array_GetApexNodeID a i default).1
  getElem? a i := toOption (array_GetApexNodeID a i default)
  set a i v := array_SetApexNodeID a i v
  length := array_LengthApexNodeID
  null := NullApexNodeIDArray
  build := array_BuildApexNodeID
  append := array_AppendApexNodeID
  insert a v i := array_InsertApexNodeID a v i
  remove := array_RemoveApexNodeID
  clear := array_ClearApexNodeID
  extend := array_ExtendApexNodeID
  reverse := array_ReverseApexNodeID

instance : ArrayFind ApexNodeID ApexNodeIDArray where
  find := array_FindApexNodeID

instance : ApexArray ApexPortID ApexPortIDArray where
  getElem a i _ := (array_GetApexPortID a i default).1
  getElem? a i := toOption (array_GetApexPortID a i default)
  set a i v := array_SetApexPortID a i v
  length := array_LengthApexPortID
  null := NullApexPortIDArray
  build := array_BuildApexPortID
  append := array_AppendApexPortID
  insert a v i := array_InsertApexPortID a v i
  remove := array_RemoveApexPortID
  clear := array_ClearApexPortID
  extend := array_ExtendApexPortID
  reverse := array_ReverseApexPortID

instance : ArrayFind ApexPortID ApexPortIDArray where
  find := array_FindApexPortID

instance : ApexArray FBIKSkeleton FBIKSkeletonArray where
  getElem a i _ := (array_GetFBIKSkeleton a i default).1
  getElem? a i := toOption (array_GetFBIKSkeleton a i default)
  set a i v := array_SetFBIKSkeleton a i v
  length := array_LengthFBIKSkeleton
  null := NullFBIKSkeletonArray
  build := array_BuildFBIKSkeleton
  append := array_AppendFBIKSkeleton
  insert a v i := array_InsertFBIKSkeleton a v i
  remove := array_RemoveFBIKSkeleton
  clear := array_ClearFBIKSkeleton
  extend := array_ExtendFBIKSkeleton
  reverse := array_ReverseFBIKSkeleton

instance : ApexArray FBIKSolver FBIKSolverArray where
  getElem a i _ := (array_GetFBIKSolver a i default).1
  getElem? a i := toOption (array_GetFBIKSolver a i default)
  set a i v := array_SetFBIKSolver a i v
  length := array_LengthFBIKSolver
  null := NullFBIKSolverArray
  build := array_BuildFBIKSolver
  append := array_AppendFBIKSolver
  insert a v i := array_InsertFBIKSolver a v i
  remove := array_RemoveFBIKSolver
  clear := array_ClearFBIKSolver
  extend := array_ExtendFBIKSolver
  reverse := array_ReverseFBIKSolver

instance : ApexArray FBIKTarget FBIKTargetArray where
  getElem a i _ := (array_GetFBIKTarget a i default).1
  getElem? a i := toOption (array_GetFBIKTarget a i default)
  set a i v := array_SetFBIKTarget a i v
  length := array_LengthFBIKTarget
  null := NullFBIKTargetArray
  build := array_BuildFBIKTarget
  append := array_AppendFBIKTarget
  insert a v i := array_InsertFBIKTarget a v i
  remove := array_RemoveFBIKTarget
  clear := array_ClearFBIKTarget
  extend := array_ExtendFBIKTarget
  reverse := array_ReverseFBIKTarget

instance : ApexArray SimRootDataId SimRootDataIdArray where
  getElem a i _ := (array_GetSimRootDataId a i default).1
  getElem? a i := toOption (array_GetSimRootDataId a i default)
  set a i v := array_SetSimRootDataId a i v
  length := array_LengthSimRootDataId
  null := NullSimRootDataIdArray
  build := array_BuildSimRootDataId
  append := array_AppendSimRootDataId
  insert a v i := array_InsertSimRootDataId a v i
  remove := array_RemoveSimRootDataId
  clear := array_ClearSimRootDataId
  extend := array_ExtendSimRootDataId
  reverse := array_ReverseSimRootDataId

instance : ArrayFind SimRootDataId SimRootDataIdArray where
  find := array_FindSimRootDataId

-- Convenience functions using the typeclass
namespace ApexArray

def build' [ApexArray α arr] {n : Nat} (values : VariadicArg α n) : arr := 
  ApexArray.build values

def append' [ApexArray α arr] (a : arr) (val : α) : arr := 
  (ApexArray.append a val).1

def appendWithIndex [ApexArray α arr] (a : arr) (val : α) : arr × Int := 
  ApexArray.append a val

def length' [ApexArray α arr] (a : arr) : Int := 
  ApexArray.length a

def set' [ApexArray α arr] (a : arr) (i : Int) (val : α) : arr := 
  (ApexArray.set a i val).1

def setChecked [ApexArray α arr] (a : arr) (i : Int) (val : α) : arr × Bool := 
  ApexArray.set a i val

end ApexArray

-- Dot notation convenience functions for arrays
namespace BoolArray
abbrev set (a : BoolArray) (i : Int) (v : Bool) : BoolArray := (ApexArray.set a i v).1
abbrev length (a : BoolArray) : Int := ApexArray.length a
abbrev append (a : BoolArray) (v : Bool) : BoolArray := (ApexArray.append a v).1
abbrev insert (a : BoolArray) (v : Bool) (i : Int) : BoolArray := ApexArray.insert a v i
abbrev remove (a : BoolArray) (i : Int) : BoolArray := ApexArray.remove a i
abbrev clear (a : BoolArray) : BoolArray := ApexArray.clear a
abbrev extend (a b : BoolArray) : BoolArray := ApexArray.extend a b
abbrev reverse (a : BoolArray) : BoolArray := ApexArray.reverse a
abbrev find (a : BoolArray) (v : Bool) : Int := ArrayFind.find a v
abbrev min (a : BoolArray) : Bool × Int := ArrayMin.min a
abbrev max (a : BoolArray) : Bool × Int := ArrayMax.max a
abbrev sort (a : BoolArray) : BoolArray := ArraySort.sort a
abbrev sortAndRemoveDuplicates (a : BoolArray) : BoolArray := ArraySort.sortAndRemoveDuplicates a
end BoolArray

namespace IntArray
abbrev set (a : IntArray) (i : Int) (v : Int) : IntArray := (ApexArray.set a i v).1
abbrev length (a : IntArray) : Int := ApexArray.length a
abbrev append (a : IntArray) (v : Int) : IntArray := (ApexArray.append a v).1
abbrev insert (a : IntArray) (v : Int) (i : Int) : IntArray := ApexArray.insert a v i
abbrev remove (a : IntArray) (i : Int) : IntArray := ApexArray.remove a i
abbrev clear (a : IntArray) : IntArray := ApexArray.clear a
abbrev extend (a b : IntArray) : IntArray := ApexArray.extend a b
abbrev reverse (a : IntArray) : IntArray := ApexArray.reverse a
abbrev find (a : IntArray) (v : Int) : Int := ArrayFind.find a v
abbrev sum (a : IntArray) : Int := ArraySum.arraySum a
abbrev min (a : IntArray) : Int × Int := ArrayMin.min a
abbrev max (a : IntArray) : Int × Int := ArrayMax.max a
abbrev sort (a : IntArray) : IntArray := ArraySort.sort a
abbrev sortAndRemoveDuplicates (a : IntArray) : IntArray := ArraySort.sortAndRemoveDuplicates a
end IntArray

namespace FloatArray
abbrev set (a : FloatArray) (i : Int) (v : Float) : FloatArray := (ApexArray.set a i v).1
abbrev length (a : FloatArray) : Int := ApexArray.length a
abbrev append (a : FloatArray) (v : Float) : FloatArray := (ApexArray.append a v).1
abbrev insert (a : FloatArray) (v : Float) (i : Int) : FloatArray := ApexArray.insert a v i
abbrev remove (a : FloatArray) (i : Int) : FloatArray := ApexArray.remove a i
abbrev clear (a : FloatArray) : FloatArray := ApexArray.clear a
abbrev extend (a b : FloatArray) : FloatArray := ApexArray.extend a b
abbrev reverse (a : FloatArray) : FloatArray := ApexArray.reverse a
abbrev find (a : FloatArray) (v : Float) : Int := ArrayFind.find a v
abbrev sum (a : FloatArray) : Float := ArraySum.arraySum a
abbrev lerp (a b : FloatArray) (bias : FloatArray) : FloatArray := ArrayLerp.lerp a b bias
abbrev min (a : FloatArray) : Float × Int := ArrayMin.min a
abbrev max (a : FloatArray) : Float × Int := ArrayMax.max a
abbrev sort (a : FloatArray) : FloatArray := ArraySort.sort a
abbrev sortAndRemoveDuplicates (a : FloatArray) : FloatArray := ArraySort.sortAndRemoveDuplicates a
end FloatArray

namespace StringArray
abbrev set (a : StringArray) (i : Int) (v : String) : StringArray := (ApexArray.set a i v).1
abbrev length (a : StringArray) : Int := ApexArray.length a
abbrev append (a : StringArray) (v : String) : StringArray := (ApexArray.append a v).1
abbrev insert (a : StringArray) (v : String) (i : Int) : StringArray := ApexArray.insert a v i
abbrev remove (a : StringArray) (i : Int) : StringArray := ApexArray.remove a i
abbrev clear (a : StringArray) : StringArray := ApexArray.clear a
abbrev extend (a b : StringArray) : StringArray := ApexArray.extend a b
abbrev reverse (a : StringArray) : StringArray := ApexArray.reverse a
abbrev find (a : StringArray) (v : String) : Int := ArrayFind.find a v
abbrev min (a : StringArray) : String × Int := ArrayMin.min a
abbrev max (a : StringArray) : String × Int := ArrayMax.max a
abbrev sort (a : StringArray) : StringArray := ArraySort.sort a
abbrev sortAndRemoveDuplicates (a : StringArray) : StringArray := ArraySort.sortAndRemoveDuplicates a
end StringArray

namespace Vector2Array
abbrev set (a : Vector2Array) (i : Int) (v : Vector2) : Vector2Array := (ApexArray.set a i v).1
abbrev length (a : Vector2Array) : Int := ApexArray.length a
abbrev append (a : Vector2Array) (v : Vector2) : Vector2Array := (ApexArray.append a v).1
abbrev insert (a : Vector2Array) (v : Vector2) (i : Int) : Vector2Array := ApexArray.insert a v i
abbrev remove (a : Vector2Array) (i : Int) : Vector2Array := ApexArray.remove a i
abbrev clear (a : Vector2Array) : Vector2Array := ApexArray.clear a
abbrev extend (a b : Vector2Array) : Vector2Array := ApexArray.extend a b
abbrev reverse (a : Vector2Array) : Vector2Array := ApexArray.reverse a
abbrev find (a : Vector2Array) (v : Vector2) : Int := ArrayFind.find a v
abbrev sum (a : Vector2Array) : Vector2 := ArraySum.arraySum a
abbrev lerp (a b : Vector2Array) (bias : FloatArray) : Vector2Array := ArrayLerp.lerp a b bias
abbrev min (a : Vector2Array) : Vector2 × Int := ArrayMin.min a
abbrev max (a : Vector2Array) : Vector2 × Int := ArrayMax.max a
abbrev sort (a : Vector2Array) : Vector2Array := ArraySort.sort a
abbrev sortAndRemoveDuplicates (a : Vector2Array) : Vector2Array := ArraySort.sortAndRemoveDuplicates a
end Vector2Array

namespace Vector3Array
abbrev set (a : Vector3Array) (i : Int) (v : Vector3) : Vector3Array := (ApexArray.set a i v).1
abbrev length (a : Vector3Array) : Int := ApexArray.length a
abbrev append (a : Vector3Array) (v : Vector3) : Vector3Array := (ApexArray.append a v).1
abbrev insert (a : Vector3Array) (v : Vector3) (i : Int) : Vector3Array := ApexArray.insert a v i
abbrev remove (a : Vector3Array) (i : Int) : Vector3Array := ApexArray.remove a i
abbrev clear (a : Vector3Array) : Vector3Array := ApexArray.clear a
abbrev extend (a b : Vector3Array) : Vector3Array := ApexArray.extend a b
abbrev reverse (a : Vector3Array) : Vector3Array := ApexArray.reverse a
abbrev find (a : Vector3Array) (v : Vector3) : Int := ArrayFind.find a v
abbrev sum (a : Vector3Array) : Vector3 := ArraySum.arraySum a
abbrev lerp (a b : Vector3Array) (bias : FloatArray) : Vector3Array := ArrayLerp.lerp a b bias
abbrev min (a : Vector3Array) : Vector3 × Int := ArrayMin.min a
abbrev max (a : Vector3Array) : Vector3 × Int := ArrayMax.max a
abbrev sort (a : Vector3Array) : Vector3Array := ArraySort.sort a
abbrev sortAndRemoveDuplicates (a : Vector3Array) : Vector3Array := ArraySort.sortAndRemoveDuplicates a
end Vector3Array

namespace Vector4Array
abbrev set (a : Vector4Array) (i : Int) (v : Vector4) : Vector4Array := (ApexArray.set a i v).1
abbrev length (a : Vector4Array) : Int := ApexArray.length a
abbrev append (a : Vector4Array) (v : Vector4) : Vector4Array := (ApexArray.append a v).1
abbrev insert (a : Vector4Array) (v : Vector4) (i : Int) : Vector4Array := ApexArray.insert a v i
abbrev remove (a : Vector4Array) (i : Int) : Vector4Array := ApexArray.remove a i
abbrev clear (a : Vector4Array) : Vector4Array := ApexArray.clear a
abbrev extend (a b : Vector4Array) : Vector4Array := ApexArray.extend a b
abbrev reverse (a : Vector4Array) : Vector4Array := ApexArray.reverse a
abbrev find (a : Vector4Array) (v : Vector4) : Int := ArrayFind.find a v
abbrev sum (a : Vector4Array) : Vector4 := ArraySum.arraySum a
abbrev lerp (a b : Vector4Array) (bias : FloatArray) : Vector4Array := ArrayLerp.lerp a b bias
abbrev min (a : Vector4Array) : Vector4 × Int := ArrayMin.min a
abbrev max (a : Vector4Array) : Vector4 × Int := ArrayMax.max a
abbrev sort (a : Vector4Array) : Vector4Array := ArraySort.sort a
abbrev sortAndRemoveDuplicates (a : Vector4Array) : Vector4Array := ArraySort.sortAndRemoveDuplicates a
end Vector4Array

namespace Matrix3Array
abbrev set (a : Matrix3Array) (i : Int) (v : Matrix3) : Matrix3Array := (ApexArray.set a i v).1
abbrev length (a : Matrix3Array) : Int := ApexArray.length a
abbrev append (a : Matrix3Array) (v : Matrix3) : Matrix3Array := (ApexArray.append a v).1
abbrev insert (a : Matrix3Array) (v : Matrix3) (i : Int) : Matrix3Array := ApexArray.insert a v i
abbrev remove (a : Matrix3Array) (i : Int) : Matrix3Array := ApexArray.remove a i
abbrev clear (a : Matrix3Array) : Matrix3Array := ApexArray.clear a
abbrev extend (a b : Matrix3Array) : Matrix3Array := ApexArray.extend a b
abbrev reverse (a : Matrix3Array) : Matrix3Array := ApexArray.reverse a
abbrev find (a : Matrix3Array) (v : Matrix3) : Int := ArrayFind.find a v
abbrev sum (a : Matrix3Array) : Matrix3 := ArraySum.arraySum a
abbrev lerp (a b : Matrix3Array) (bias : FloatArray) : Matrix3Array := ArrayLerp.lerp a b bias
end Matrix3Array

namespace Matrix4Array
abbrev set (a : Matrix4Array) (i : Int) (v : Matrix4) : Matrix4Array := (ApexArray.set a i v).1
abbrev length (a : Matrix4Array) : Int := ApexArray.length a
abbrev append (a : Matrix4Array) (v : Matrix4) : Matrix4Array := (ApexArray.append a v).1
abbrev insert (a : Matrix4Array) (v : Matrix4) (i : Int) : Matrix4Array := ApexArray.insert a v i
abbrev remove (a : Matrix4Array) (i : Int) : Matrix4Array := ApexArray.remove a i
abbrev clear (a : Matrix4Array) : Matrix4Array := ApexArray.clear a
abbrev extend (a b : Matrix4Array) : Matrix4Array := ApexArray.extend a b
abbrev reverse (a : Matrix4Array) : Matrix4Array := ApexArray.reverse a
abbrev find (a : Matrix4Array) (v : Matrix4) : Int := ArrayFind.find a v
abbrev sum (a : Matrix4Array) : Matrix4 := ArraySum.arraySum a
abbrev lerp (a b : Matrix4Array) (bias : FloatArray) : Matrix4Array := ArrayLerp.lerp a b bias
end Matrix4Array

namespace GeometryArray
abbrev set (a : GeometryArray) (i : Int) (v : Geometry) : GeometryArray := (ApexArray.set a i v).1
abbrev length (a : GeometryArray) : Int := ApexArray.length a
abbrev append (a : GeometryArray) (v : Geometry) : GeometryArray := (ApexArray.append a v).1
abbrev insert (a : GeometryArray) (v : Geometry) (i : Int) : GeometryArray := ApexArray.insert a v i
abbrev remove (a : GeometryArray) (i : Int) : GeometryArray := ApexArray.remove a i
abbrev clear (a : GeometryArray) : GeometryArray := ApexArray.clear a
abbrev extend (a b : GeometryArray) : GeometryArray := ApexArray.extend a b
abbrev reverse (a : GeometryArray) : GeometryArray := ApexArray.reverse a
end GeometryArray

namespace DictArray
abbrev set (a : DictArray) (i : Int) (v : Dict) : DictArray := (ApexArray.set a i v).1
abbrev length (a : DictArray) : Int := ApexArray.length a
abbrev append (a : DictArray) (v : Dict) : DictArray := (ApexArray.append a v).1
abbrev insert (a : DictArray) (v : Dict) (i : Int) : DictArray := ApexArray.insert a v i
abbrev remove (a : DictArray) (i : Int) : DictArray := ApexArray.remove a i
abbrev clear (a : DictArray) : DictArray := ApexArray.clear a
abbrev extend (a b : DictArray) : DictArray := ApexArray.extend a b
abbrev reverse (a : DictArray) : DictArray := ApexArray.reverse a
end DictArray

namespace DynamicPathArray
abbrev set (a : DynamicPathArray) (i : Int) (v : DynamicPath) : DynamicPathArray := (ApexArray.set a i v).1
abbrev length (a : DynamicPathArray) : Int := ApexArray.length a
abbrev append (a : DynamicPathArray) (v : DynamicPath) : DynamicPathArray := (ApexArray.append a v).1
abbrev insert (a : DynamicPathArray) (v : DynamicPath) (i : Int) : DynamicPathArray := ApexArray.insert a v i
abbrev remove (a : DynamicPathArray) (i : Int) : DynamicPathArray := ApexArray.remove a i
abbrev clear (a : DynamicPathArray) : DynamicPathArray := ApexArray.clear a
abbrev extend (a b : DynamicPathArray) : DynamicPathArray := ApexArray.extend a b
abbrev reverse (a : DynamicPathArray) : DynamicPathArray := ApexArray.reverse a
end DynamicPathArray

namespace ApexNodeIDArray
abbrev set (a : ApexNodeIDArray) (i : Int) (v : ApexNodeID) : ApexNodeIDArray := (ApexArray.set a i v).1
abbrev length (a : ApexNodeIDArray) : Int := ApexArray.length a
abbrev append (a : ApexNodeIDArray) (v : ApexNodeID) : ApexNodeIDArray := (ApexArray.append a v).1
abbrev insert (a : ApexNodeIDArray) (v : ApexNodeID) (i : Int) : ApexNodeIDArray := ApexArray.insert a v i
abbrev remove (a : ApexNodeIDArray) (i : Int) : ApexNodeIDArray := ApexArray.remove a i
abbrev clear (a : ApexNodeIDArray) : ApexNodeIDArray := ApexArray.clear a
abbrev extend (a b : ApexNodeIDArray) : ApexNodeIDArray := ApexArray.extend a b
abbrev reverse (a : ApexNodeIDArray) : ApexNodeIDArray := ApexArray.reverse a
abbrev find (a : ApexNodeIDArray) (v : ApexNodeID) : Int := ArrayFind.find a v
end ApexNodeIDArray

namespace ApexPortIDArray
abbrev set (a : ApexPortIDArray) (i : Int) (v : ApexPortID) : ApexPortIDArray := (ApexArray.set a i v).1
abbrev length (a : ApexPortIDArray) : Int := ApexArray.length a
abbrev append (a : ApexPortIDArray) (v : ApexPortID) : ApexPortIDArray := (ApexArray.append a v).1
abbrev insert (a : ApexPortIDArray) (v : ApexPortID) (i : Int) : ApexPortIDArray := ApexArray.insert a v i
abbrev remove (a : ApexPortIDArray) (i : Int) : ApexPortIDArray := ApexArray.remove a i
abbrev clear (a : ApexPortIDArray) : ApexPortIDArray := ApexArray.clear a
abbrev extend (a b : ApexPortIDArray) : ApexPortIDArray := ApexArray.extend a b
abbrev reverse (a : ApexPortIDArray) : ApexPortIDArray := ApexArray.reverse a
abbrev find (a : ApexPortIDArray) (v : ApexPortID) : Int := ArrayFind.find a v
end ApexPortIDArray

namespace FBIKSkeletonArray
abbrev set (a : FBIKSkeletonArray) (i : Int) (v : FBIKSkeleton) : FBIKSkeletonArray := (ApexArray.set a i v).1
abbrev length (a : FBIKSkeletonArray) : Int := ApexArray.length a
abbrev append (a : FBIKSkeletonArray) (v : FBIKSkeleton) : FBIKSkeletonArray := (ApexArray.append a v).1
abbrev insert (a : FBIKSkeletonArray) (v : FBIKSkeleton) (i : Int) : FBIKSkeletonArray := ApexArray.insert a v i
abbrev remove (a : FBIKSkeletonArray) (i : Int) : FBIKSkeletonArray := ApexArray.remove a i
abbrev clear (a : FBIKSkeletonArray) : FBIKSkeletonArray := ApexArray.clear a
abbrev extend (a b : FBIKSkeletonArray) : FBIKSkeletonArray := ApexArray.extend a b
abbrev reverse (a : FBIKSkeletonArray) : FBIKSkeletonArray := ApexArray.reverse a
end FBIKSkeletonArray

namespace FBIKSolverArray
abbrev set (a : FBIKSolverArray) (i : Int) (v : FBIKSolver) : FBIKSolverArray := (ApexArray.set a i v).1
abbrev length (a : FBIKSolverArray) : Int := ApexArray.length a
abbrev append (a : FBIKSolverArray) (v : FBIKSolver) : FBIKSolverArray := (ApexArray.append a v).1
abbrev insert (a : FBIKSolverArray) (v : FBIKSolver) (i : Int) : FBIKSolverArray := ApexArray.insert a v i
abbrev remove (a : FBIKSolverArray) (i : Int) : FBIKSolverArray := ApexArray.remove a i
abbrev clear (a : FBIKSolverArray) : FBIKSolverArray := ApexArray.clear a
abbrev extend (a b : FBIKSolverArray) : FBIKSolverArray := ApexArray.extend a b
abbrev reverse (a : FBIKSolverArray) : FBIKSolverArray := ApexArray.reverse a
end FBIKSolverArray

namespace FBIKTargetArray
abbrev set (a : FBIKTargetArray) (i : Int) (v : FBIKTarget) : FBIKTargetArray := (ApexArray.set a i v).1
abbrev length (a : FBIKTargetArray) : Int := ApexArray.length a
abbrev append (a : FBIKTargetArray) (v : FBIKTarget) : FBIKTargetArray := (ApexArray.append a v).1
abbrev insert (a : FBIKTargetArray) (v : FBIKTarget) (i : Int) : FBIKTargetArray := ApexArray.insert a v i
abbrev remove (a : FBIKTargetArray) (i : Int) : FBIKTargetArray := ApexArray.remove a i
abbrev clear (a : FBIKTargetArray) : FBIKTargetArray := ApexArray.clear a
abbrev extend (a b : FBIKTargetArray) : FBIKTargetArray := ApexArray.extend a b
abbrev reverse (a : FBIKTargetArray) : FBIKTargetArray := ApexArray.reverse a
end FBIKTargetArray

namespace SimRootDataIdArray
abbrev set (a : SimRootDataIdArray) (i : Int) (v : SimRootDataId) : SimRootDataIdArray := (ApexArray.set a i v).1
abbrev length (a : SimRootDataIdArray) : Int := ApexArray.length a
abbrev append (a : SimRootDataIdArray) (v : SimRootDataId) : SimRootDataIdArray := (ApexArray.append a v).1
abbrev insert (a : SimRootDataIdArray) (v : SimRootDataId) (i : Int) : SimRootDataIdArray := ApexArray.insert a v i
abbrev remove (a : SimRootDataIdArray) (i : Int) : SimRootDataIdArray := ApexArray.remove a i
abbrev clear (a : SimRootDataIdArray) : SimRootDataIdArray := ApexArray.clear a
abbrev extend (a b : SimRootDataIdArray) : SimRootDataIdArray := ApexArray.extend a b
abbrev reverse (a : SimRootDataIdArray) : SimRootDataIdArray := ApexArray.reverse a
abbrev find (a : SimRootDataIdArray) (v : SimRootDataId) : Int := ArrayFind.find a v
end SimRootDataIdArray
