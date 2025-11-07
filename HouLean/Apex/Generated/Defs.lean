import HouLean.Init
import HouLean.Data.Defs

namespace HouLean.Apex


opaque AnimChannel : Type := Unit
def AnimChannel.default : AnimChannel := cast sorry_proof ()
instance : Inhabited AnimChannel := ⟨AnimChannel.default⟩    

opaque AnimChannelCollection : Type := Unit
def AnimChannelCollection.default : AnimChannelCollection := cast sorry_proof ()
instance : Inhabited AnimChannelCollection := ⟨AnimChannelCollection.default⟩    

opaque AnimStack : Type := Unit
def AnimStack.default : AnimStack := cast sorry_proof ()
instance : Inhabited AnimStack := ⟨AnimStack.default⟩    

opaque ApexGraphHandle : Type := Unit
def ApexGraphHandle.default : ApexGraphHandle := cast sorry_proof ()
instance : Inhabited ApexGraphHandle := ⟨ApexGraphHandle.default⟩    

opaque ApexNodeID : Type := Unit
def ApexNodeID.default : ApexNodeID := cast sorry_proof ()
instance : Inhabited ApexNodeID := ⟨ApexNodeID.default⟩    

opaque ApexPortID : Type := Unit
def ApexPortID.default : ApexPortID := cast sorry_proof ()
instance : Inhabited ApexPortID := ⟨ApexPortID.default⟩    

opaque ColorRamp : Type := Unit
def ColorRamp.default : ColorRamp := cast sorry_proof ()
instance : Inhabited ColorRamp := ⟨ColorRamp.default⟩    

opaque DataItem : Type := Unit
def DataItem.default : DataItem := cast sorry_proof ()
instance : Inhabited DataItem := ⟨DataItem.default⟩    

opaque Dict : Type := Unit
def Dict.default : Dict := cast sorry_proof ()
instance : Inhabited Dict := ⟨Dict.default⟩    

opaque DynamicPath : Type := Unit
def DynamicPath.default : DynamicPath := cast sorry_proof ()
instance : Inhabited DynamicPath := ⟨DynamicPath.default⟩    

opaque FBIKSkeleton : Type := Unit
def FBIKSkeleton.default : FBIKSkeleton := cast sorry_proof ()
instance : Inhabited FBIKSkeleton := ⟨FBIKSkeleton.default⟩    

opaque FBIKSolver : Type := Unit
def FBIKSolver.default : FBIKSolver := cast sorry_proof ()
instance : Inhabited FBIKSolver := ⟨FBIKSolver.default⟩    

opaque FBIKTarget : Type := Unit
def FBIKTarget.default : FBIKTarget := cast sorry_proof ()
instance : Inhabited FBIKTarget := ⟨FBIKTarget.default⟩    

opaque FloatRamp : Type := Unit
def FloatRamp.default : FloatRamp := cast sorry_proof ()
instance : Inhabited FloatRamp := ⟨FloatRamp.default⟩    

opaque Geometry : Type := Unit
def Geometry.default : Geometry := cast sorry_proof ()
instance : Inhabited Geometry := ⟨Geometry.default⟩    

opaque ImageLayer : Type := Unit
def ImageLayer.default : ImageLayer := cast sorry_proof ()
instance : Inhabited ImageLayer := ⟨ImageLayer.default⟩    

opaque NanoVDB : Type := Unit
def NanoVDB.default : NanoVDB := cast sorry_proof ()
instance : Inhabited NanoVDB := ⟨NanoVDB.default⟩    

opaque SimEngine : Type := Unit
def SimEngine.default : SimEngine := cast sorry_proof ()
instance : Inhabited SimEngine := ⟨SimEngine.default⟩    

opaque SimRootDataId : Type := Unit
def SimRootDataId.default : SimRootDataId := cast sorry_proof ()
instance : Inhabited SimRootDataId := ⟨SimRootDataId.default⟩    

opaque VerbContext : Type := Unit
def VerbContext.default : VerbContext := cast sorry_proof ()
instance : Inhabited VerbContext := ⟨VerbContext.default⟩    

opaque AnimChannelArray : Type := Unit
def AnimChannelArray.default : AnimChannelArray := cast sorry_proof ()
instance : Inhabited AnimChannelArray := ⟨AnimChannelArray.default⟩    

opaque AnimChannelCollectionArray : Type := Unit
def AnimChannelCollectionArray.default : AnimChannelCollectionArray := cast sorry_proof ()
instance : Inhabited AnimChannelCollectionArray := ⟨AnimChannelCollectionArray.default⟩    

opaque AnimStackArray : Type := Unit
def AnimStackArray.default : AnimStackArray := cast sorry_proof ()
instance : Inhabited AnimStackArray := ⟨AnimStackArray.default⟩    

opaque ApexGraphHandleArray : Type := Unit
def ApexGraphHandleArray.default : ApexGraphHandleArray := cast sorry_proof ()
instance : Inhabited ApexGraphHandleArray := ⟨ApexGraphHandleArray.default⟩    

opaque ApexNodeIDArray : Type := Unit
def ApexNodeIDArray.default : ApexNodeIDArray := cast sorry_proof ()
instance : Inhabited ApexNodeIDArray := ⟨ApexNodeIDArray.default⟩    

opaque ApexPortIDArray : Type := Unit
def ApexPortIDArray.default : ApexPortIDArray := cast sorry_proof ()
instance : Inhabited ApexPortIDArray := ⟨ApexPortIDArray.default⟩    

opaque BoolArray : Type := Unit
def BoolArray.default : BoolArray := cast sorry_proof ()
instance : Inhabited BoolArray := ⟨BoolArray.default⟩    

opaque ColorRampArray : Type := Unit
def ColorRampArray.default : ColorRampArray := cast sorry_proof ()
instance : Inhabited ColorRampArray := ⟨ColorRampArray.default⟩    

opaque DictArray : Type := Unit
def DictArray.default : DictArray := cast sorry_proof ()
instance : Inhabited DictArray := ⟨DictArray.default⟩    

opaque DynamicPathArray : Type := Unit
def DynamicPathArray.default : DynamicPathArray := cast sorry_proof ()
instance : Inhabited DynamicPathArray := ⟨DynamicPathArray.default⟩    

opaque FBIKSkeletonArray : Type := Unit
def FBIKSkeletonArray.default : FBIKSkeletonArray := cast sorry_proof ()
instance : Inhabited FBIKSkeletonArray := ⟨FBIKSkeletonArray.default⟩    

opaque FBIKSolverArray : Type := Unit
def FBIKSolverArray.default : FBIKSolverArray := cast sorry_proof ()
instance : Inhabited FBIKSolverArray := ⟨FBIKSolverArray.default⟩    

opaque FBIKTargetArray : Type := Unit
def FBIKTargetArray.default : FBIKTargetArray := cast sorry_proof ()
instance : Inhabited FBIKTargetArray := ⟨FBIKTargetArray.default⟩    

opaque FloatRampArray : Type := Unit
def FloatRampArray.default : FloatRampArray := cast sorry_proof ()
instance : Inhabited FloatRampArray := ⟨FloatRampArray.default⟩    

opaque GeometryArray : Type := Unit
def GeometryArray.default : GeometryArray := cast sorry_proof ()
instance : Inhabited GeometryArray := ⟨GeometryArray.default⟩    

opaque IntArray : Type := Unit
def IntArray.default : IntArray := cast sorry_proof ()
instance : Inhabited IntArray := ⟨IntArray.default⟩    

opaque Matrix3Array : Type := Unit
def Matrix3Array.default : Matrix3Array := cast sorry_proof ()
instance : Inhabited Matrix3Array := ⟨Matrix3Array.default⟩    

opaque Matrix4Array : Type := Unit
def Matrix4Array.default : Matrix4Array := cast sorry_proof ()
instance : Inhabited Matrix4Array := ⟨Matrix4Array.default⟩    

opaque SimRootDataIdArray : Type := Unit
def SimRootDataIdArray.default : SimRootDataIdArray := cast sorry_proof ()
instance : Inhabited SimRootDataIdArray := ⟨SimRootDataIdArray.default⟩    

opaque StringArray : Type := Unit
def StringArray.default : StringArray := cast sorry_proof ()
instance : Inhabited StringArray := ⟨StringArray.default⟩    

opaque Vector2Array : Type := Unit
def Vector2Array.default : Vector2Array := cast sorry_proof ()
instance : Inhabited Vector2Array := ⟨Vector2Array.default⟩    

opaque Vector3Array : Type := Unit
def Vector3Array.default : Vector3Array := cast sorry_proof ()
instance : Inhabited Vector3Array := ⟨Vector3Array.default⟩    

opaque Vector4Array : Type := Unit
def Vector4Array.default : Vector4Array := cast sorry_proof ()
instance : Inhabited Vector4Array := ⟨Vector4Array.default⟩    

inductive ApexTypeTag where
  | animChannel
  | animChannelCollection
  | animStack
  | apexGraphHandle
  | apexNodeID
  | apexPortID
  | bool
  | colorRamp
  | dataItem
  | dict
  | dynamicPath
  | fBIKSkeleton
  | fBIKSolver
  | fBIKTarget
  | float
  | floatRamp
  | geometry
  | imageLayer
  | int
  | matrix3
  | matrix4
  | nanoVDB
  | simEngine
  | simRootDataId
  | string
  | vector2
  | vector3
  | vector4
  | verbContext
  | animChannelArray
  | animChannelCollectionArray
  | animStackArray
  | apexGraphHandleArray
  | apexNodeIDArray
  | apexPortIDArray
  | boolArray
  | colorRampArray
  | dictArray
  | dynamicPathArray
  | fBIKSkeletonArray
  | fBIKSolverArray
  | fBIKTargetArray
  | floatArray
  | floatRampArray
  | geometryArray
  | intArray
  | matrix3Array
  | matrix4Array
  | simRootDataIdArray
  | stringArray
  | vector2Array
  | vector3Array
  | vector4Array
deriving Inhabited, BEq

def ApexTypeTag.toType : ApexTypeTag → Type
  | animChannel => AnimChannel
  | animChannelCollection => AnimChannelCollection
  | animStack => AnimStack
  | apexGraphHandle => ApexGraphHandle
  | apexNodeID => ApexNodeID
  | apexPortID => ApexPortID
  | bool => Bool
  | colorRamp => ColorRamp
  | dataItem => DataItem
  | dict => Dict
  | dynamicPath => DynamicPath
  | fBIKSkeleton => FBIKSkeleton
  | fBIKSolver => FBIKSolver
  | fBIKTarget => FBIKTarget
  | float => Float
  | floatRamp => FloatRamp
  | geometry => Geometry
  | imageLayer => ImageLayer
  | int => Int
  | matrix3 => Matrix3
  | matrix4 => Matrix4
  | nanoVDB => NanoVDB
  | simEngine => SimEngine
  | simRootDataId => SimRootDataId
  | string => String
  | vector2 => Vector2
  | vector3 => Vector3
  | vector4 => Vector4
  | verbContext => VerbContext
  | animChannelArray => AnimChannelArray
  | animChannelCollectionArray => AnimChannelCollectionArray
  | animStackArray => AnimStackArray
  | apexGraphHandleArray => ApexGraphHandleArray
  | apexNodeIDArray => ApexNodeIDArray
  | apexPortIDArray => ApexPortIDArray
  | boolArray => BoolArray
  | colorRampArray => ColorRampArray
  | dictArray => DictArray
  | dynamicPathArray => DynamicPathArray
  | fBIKSkeletonArray => FBIKSkeletonArray
  | fBIKSolverArray => FBIKSolverArray
  | fBIKTargetArray => FBIKTargetArray
  | floatArray => FloatArray
  | floatRampArray => FloatRampArray
  | geometryArray => GeometryArray
  | intArray => IntArray
  | matrix3Array => Matrix3Array
  | matrix4Array => Matrix4Array
  | simRootDataIdArray => SimRootDataIdArray
  | stringArray => StringArray
  | vector2Array => Vector2Array
  | vector3Array => Vector3Array
  | vector4Array => Vector4Array

def ApexTypeTag.toString : ApexTypeTag → String
  | animChannel => "AnimChannel"
  | animChannelCollection => "AnimChannelCollection"
  | animStack => "AnimStack"
  | apexGraphHandle => "ApexGraphHandle"
  | apexNodeID => "ApexNodeID"
  | apexPortID => "ApexPortID"
  | bool => "Bool"
  | colorRamp => "ColorRamp"
  | dataItem => "DataItem"
  | dict => "Dict"
  | dynamicPath => "DynamicPath"
  | fBIKSkeleton => "FBIKSkeleton"
  | fBIKSolver => "FBIKSolver"
  | fBIKTarget => "FBIKTarget"
  | float => "Float"
  | floatRamp => "FloatRamp"
  | geometry => "Geometry"
  | imageLayer => "ImageLayer"
  | int => "Int"
  | matrix3 => "Matrix3"
  | matrix4 => "Matrix4"
  | nanoVDB => "NanoVDB"
  | simEngine => "SimEngine"
  | simRootDataId => "SimRootDataId"
  | string => "String"
  | vector2 => "Vector2"
  | vector3 => "Vector3"
  | vector4 => "Vector4"
  | verbContext => "VerbContext"
  | animChannelArray => "AnimChannelArray"
  | animChannelCollectionArray => "AnimChannelCollectionArray"
  | animStackArray => "AnimStackArray"
  | apexGraphHandleArray => "ApexGraphHandleArray"
  | apexNodeIDArray => "ApexNodeIDArray"
  | apexPortIDArray => "ApexPortIDArray"
  | boolArray => "BoolArray"
  | colorRampArray => "ColorRampArray"
  | dictArray => "DictArray"
  | dynamicPathArray => "DynamicPathArray"
  | fBIKSkeletonArray => "FBIKSkeletonArray"
  | fBIKSolverArray => "FBIKSolverArray"
  | fBIKTargetArray => "FBIKTargetArray"
  | floatArray => "FloatArray"
  | floatRampArray => "FloatRampArray"
  | geometryArray => "GeometryArray"
  | intArray => "IntArray"
  | matrix3Array => "Matrix3Array"
  | matrix4Array => "Matrix4Array"
  | simRootDataIdArray => "SimRootDataIdArray"
  | stringArray => "StringArray"
  | vector2Array => "Vector2Array"
  | vector3Array => "Vector3Array"
  | vector4Array => "Vector4Array"

open Lean in
def ApexTypeTag.fromName : Name → Option ApexTypeTag
  | ``AnimChannel => some .animChannel
  | ``AnimChannelCollection => some .animChannelCollection
  | ``AnimStack => some .animStack
  | ``ApexGraphHandle => some .apexGraphHandle
  | ``ApexNodeID => some .apexNodeID
  | ``ApexPortID => some .apexPortID
  | ``Bool => some .bool
  | ``ColorRamp => some .colorRamp
  | ``DataItem => some .dataItem
  | ``Dict => some .dict
  | ``DynamicPath => some .dynamicPath
  | ``FBIKSkeleton => some .fBIKSkeleton
  | ``FBIKSolver => some .fBIKSolver
  | ``FBIKTarget => some .fBIKTarget
  | ``Float => some .float
  | ``FloatRamp => some .floatRamp
  | ``Geometry => some .geometry
  | ``ImageLayer => some .imageLayer
  | ``Int => some .int
  | ``Matrix3 => some .matrix3
  | ``Matrix4 => some .matrix4
  | ``NanoVDB => some .nanoVDB
  | ``SimEngine => some .simEngine
  | ``SimRootDataId => some .simRootDataId
  | ``String => some .string
  | ``Vector2 => some .vector2
  | ``Vector3 => some .vector3
  | ``Vector4 => some .vector4
  | ``VerbContext => some .verbContext
  | ``AnimChannelArray => some .animChannelArray
  | ``AnimChannelCollectionArray => some .animChannelCollectionArray
  | ``AnimStackArray => some .animStackArray
  | ``ApexGraphHandleArray => some .apexGraphHandleArray
  | ``ApexNodeIDArray => some .apexNodeIDArray
  | ``ApexPortIDArray => some .apexPortIDArray
  | ``BoolArray => some .boolArray
  | ``ColorRampArray => some .colorRampArray
  | ``DictArray => some .dictArray
  | ``DynamicPathArray => some .dynamicPathArray
  | ``FBIKSkeletonArray => some .fBIKSkeletonArray
  | ``FBIKSolverArray => some .fBIKSolverArray
  | ``FBIKTargetArray => some .fBIKTargetArray
  | ``FloatArray => some .floatArray
  | ``FloatRampArray => some .floatRampArray
  | ``GeometryArray => some .geometryArray
  | ``IntArray => some .intArray
  | ``Matrix3Array => some .matrix3Array
  | ``Matrix4Array => some .matrix4Array
  | ``SimRootDataIdArray => some .simRootDataIdArray
  | ``StringArray => some .stringArray
  | ``Vector2Array => some .vector2Array
  | ``Vector3Array => some .vector3Array
  | ``Vector4Array => some .vector4Array
  | _ => none

inductive Untyped where
  | animChannel (x : AnimChannel)
  | animChannelCollection (x : AnimChannelCollection)
  | animStack (x : AnimStack)
  | apexGraphHandle (x : ApexGraphHandle)
  | apexNodeID (x : ApexNodeID)
  | apexPortID (x : ApexPortID)
  | bool (x : Bool)
  | colorRamp (x : ColorRamp)
  | dataItem (x : DataItem)
  | dict (x : Dict)
  | dynamicPath (x : DynamicPath)
  | fBIKSkeleton (x : FBIKSkeleton)
  | fBIKSolver (x : FBIKSolver)
  | fBIKTarget (x : FBIKTarget)
  | float (x : Float)
  | floatRamp (x : FloatRamp)
  | geometry (x : Geometry)
  | imageLayer (x : ImageLayer)
  | int (x : Int)
  | matrix3 (x : Matrix3)
  | matrix4 (x : Matrix4)
  | nanoVDB (x : NanoVDB)
  | simEngine (x : SimEngine)
  | simRootDataId (x : SimRootDataId)
  | string (x : String)
  | vector2 (x : Vector2)
  | vector3 (x : Vector3)
  | vector4 (x : Vector4)
  | verbContext (x : VerbContext)
  | animChannelArray (x : AnimChannelArray)
  | animChannelCollectionArray (x : AnimChannelCollectionArray)
  | animStackArray (x : AnimStackArray)
  | apexGraphHandleArray (x : ApexGraphHandleArray)
  | apexNodeIDArray (x : ApexNodeIDArray)
  | apexPortIDArray (x : ApexPortIDArray)
  | boolArray (x : BoolArray)
  | colorRampArray (x : ColorRampArray)
  | dictArray (x : DictArray)
  | dynamicPathArray (x : DynamicPathArray)
  | fBIKSkeletonArray (x : FBIKSkeletonArray)
  | fBIKSolverArray (x : FBIKSolverArray)
  | fBIKTargetArray (x : FBIKTargetArray)
  | floatArray (x : FloatArray)
  | floatRampArray (x : FloatRampArray)
  | geometryArray (x : GeometryArray)
  | intArray (x : IntArray)
  | matrix3Array (x : Matrix3Array)
  | matrix4Array (x : Matrix4Array)
  | simRootDataIdArray (x : SimRootDataIdArray)
  | stringArray (x : StringArray)
  | vector2Array (x : Vector2Array)
  | vector3Array (x : Vector3Array)
  | vector4Array (x : Vector4Array)

instance : Inhabited Untyped := ⟨.float 0⟩

