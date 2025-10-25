import HouLean.Apex.Compile.ExprType

namespace HouLean.Apex

axiom silentSorry {α : Sort u} : α

attribute [apex_type "Float"] Float
attribute [apex_type "Bool"] Bool
attribute [apex_type "Int"] Int
attribute [apex_type "String"] String
attribute [apex_type "FloatArray"] FloatArray
attribute [apex_type "Vector2"] Vector2
attribute [apex_type "Vector3"] Vector3
attribute [apex_type "Vector4"] Vector4
attribute [apex_type "Matrix2"] Matrix2
attribute [apex_type "Matrix3"] Matrix3
attribute [apex_type "Matrix4"] Matrix4


    
@[apex_type "AnimChannel"]
opaque AnimChannel : Type := Unit

def AnimChannel.default : AnimChannel := cast silentSorry ()
instance : Inhabited AnimChannel := ⟨AnimChannel.default⟩
    

    
@[apex_type "AnimChannelCollection"]
opaque AnimChannelCollection : Type := Unit

def AnimChannelCollection.default : AnimChannelCollection := cast silentSorry ()
instance : Inhabited AnimChannelCollection := ⟨AnimChannelCollection.default⟩
    

    
@[apex_type "AnimStack"]
opaque AnimStack : Type := Unit

def AnimStack.default : AnimStack := cast silentSorry ()
instance : Inhabited AnimStack := ⟨AnimStack.default⟩
    

    
@[apex_type "ApexGraphHandle"]
opaque ApexGraphHandle : Type := Unit

def ApexGraphHandle.default : ApexGraphHandle := cast silentSorry ()
instance : Inhabited ApexGraphHandle := ⟨ApexGraphHandle.default⟩
    

    
@[apex_type "ApexNodeID"]
opaque ApexNodeID : Type := Unit

def ApexNodeID.default : ApexNodeID := cast silentSorry ()
instance : Inhabited ApexNodeID := ⟨ApexNodeID.default⟩
    

    
@[apex_type "ApexPortID"]
opaque ApexPortID : Type := Unit

def ApexPortID.default : ApexPortID := cast silentSorry ()
instance : Inhabited ApexPortID := ⟨ApexPortID.default⟩
    

    
@[apex_type "ColorRamp"]
opaque ColorRamp : Type := Unit

def ColorRamp.default : ColorRamp := cast silentSorry ()
instance : Inhabited ColorRamp := ⟨ColorRamp.default⟩
    

    
@[apex_type "DataItem"]
opaque DataItem : Type := Unit

def DataItem.default : DataItem := cast silentSorry ()
instance : Inhabited DataItem := ⟨DataItem.default⟩
    

    
@[apex_type "Dict"]
opaque Dict : Type := Unit

def Dict.default : Dict := cast silentSorry ()
instance : Inhabited Dict := ⟨Dict.default⟩
    

    
@[apex_type "DynamicPath"]
opaque DynamicPath : Type := Unit

def DynamicPath.default : DynamicPath := cast silentSorry ()
instance : Inhabited DynamicPath := ⟨DynamicPath.default⟩
    

    
@[apex_type "FBIKSkeleton"]
opaque FBIKSkeleton : Type := Unit

def FBIKSkeleton.default : FBIKSkeleton := cast silentSorry ()
instance : Inhabited FBIKSkeleton := ⟨FBIKSkeleton.default⟩
    

    
@[apex_type "FBIKSolver"]
opaque FBIKSolver : Type := Unit

def FBIKSolver.default : FBIKSolver := cast silentSorry ()
instance : Inhabited FBIKSolver := ⟨FBIKSolver.default⟩
    

    
@[apex_type "FBIKTarget"]
opaque FBIKTarget : Type := Unit

def FBIKTarget.default : FBIKTarget := cast silentSorry ()
instance : Inhabited FBIKTarget := ⟨FBIKTarget.default⟩
    

    
@[apex_type "FloatRamp"]
opaque FloatRamp : Type := Unit

def FloatRamp.default : FloatRamp := cast silentSorry ()
instance : Inhabited FloatRamp := ⟨FloatRamp.default⟩
    

    
@[apex_type "Geometry"]
opaque Geometry : Type := Unit

def Geometry.default : Geometry := cast silentSorry ()
instance : Inhabited Geometry := ⟨Geometry.default⟩
    

    
@[apex_type "ImageLayer"]
opaque ImageLayer : Type := Unit

def ImageLayer.default : ImageLayer := cast silentSorry ()
instance : Inhabited ImageLayer := ⟨ImageLayer.default⟩
    

    
@[apex_type "NanoVDB"]
opaque NanoVDB : Type := Unit

def NanoVDB.default : NanoVDB := cast silentSorry ()
instance : Inhabited NanoVDB := ⟨NanoVDB.default⟩
    

    
@[apex_type "SimEngine"]
opaque SimEngine : Type := Unit

def SimEngine.default : SimEngine := cast silentSorry ()
instance : Inhabited SimEngine := ⟨SimEngine.default⟩
    

    
@[apex_type "SimRootDataId"]
opaque SimRootDataId : Type := Unit

def SimRootDataId.default : SimRootDataId := cast silentSorry ()
instance : Inhabited SimRootDataId := ⟨SimRootDataId.default⟩
    

    
@[apex_type "VerbContext"]
opaque VerbContext : Type := Unit

def VerbContext.default : VerbContext := cast silentSorry ()
instance : Inhabited VerbContext := ⟨VerbContext.default⟩
    

    
@[apex_type "AnimChannelArray"]
opaque AnimChannelArray : Type := Unit

def AnimChannelArray.default : AnimChannelArray := cast silentSorry ()
instance : Inhabited AnimChannelArray := ⟨AnimChannelArray.default⟩
    

    
@[apex_type "AnimChannelCollectionArray"]
opaque AnimChannelCollectionArray : Type := Unit

def AnimChannelCollectionArray.default : AnimChannelCollectionArray := cast silentSorry ()
instance : Inhabited AnimChannelCollectionArray := ⟨AnimChannelCollectionArray.default⟩
    

    
@[apex_type "AnimStackArray"]
opaque AnimStackArray : Type := Unit

def AnimStackArray.default : AnimStackArray := cast silentSorry ()
instance : Inhabited AnimStackArray := ⟨AnimStackArray.default⟩
    

    
@[apex_type "ApexGraphHandleArray"]
opaque ApexGraphHandleArray : Type := Unit

def ApexGraphHandleArray.default : ApexGraphHandleArray := cast silentSorry ()
instance : Inhabited ApexGraphHandleArray := ⟨ApexGraphHandleArray.default⟩
    

    
@[apex_type "ApexNodeIDArray"]
opaque ApexNodeIDArray : Type := Unit

def ApexNodeIDArray.default : ApexNodeIDArray := cast silentSorry ()
instance : Inhabited ApexNodeIDArray := ⟨ApexNodeIDArray.default⟩
    

    
@[apex_type "ApexPortIDArray"]
opaque ApexPortIDArray : Type := Unit

def ApexPortIDArray.default : ApexPortIDArray := cast silentSorry ()
instance : Inhabited ApexPortIDArray := ⟨ApexPortIDArray.default⟩
    

    
@[apex_type "BoolArray"]
opaque BoolArray : Type := Unit

def BoolArray.default : BoolArray := cast silentSorry ()
instance : Inhabited BoolArray := ⟨BoolArray.default⟩
    

    
@[apex_type "ColorRampArray"]
opaque ColorRampArray : Type := Unit

def ColorRampArray.default : ColorRampArray := cast silentSorry ()
instance : Inhabited ColorRampArray := ⟨ColorRampArray.default⟩
    

    
@[apex_type "DictArray"]
opaque DictArray : Type := Unit

def DictArray.default : DictArray := cast silentSorry ()
instance : Inhabited DictArray := ⟨DictArray.default⟩
    

    
@[apex_type "DynamicPathArray"]
opaque DynamicPathArray : Type := Unit

def DynamicPathArray.default : DynamicPathArray := cast silentSorry ()
instance : Inhabited DynamicPathArray := ⟨DynamicPathArray.default⟩
    

    
@[apex_type "FBIKSkeletonArray"]
opaque FBIKSkeletonArray : Type := Unit

def FBIKSkeletonArray.default : FBIKSkeletonArray := cast silentSorry ()
instance : Inhabited FBIKSkeletonArray := ⟨FBIKSkeletonArray.default⟩
    

    
@[apex_type "FBIKSolverArray"]
opaque FBIKSolverArray : Type := Unit

def FBIKSolverArray.default : FBIKSolverArray := cast silentSorry ()
instance : Inhabited FBIKSolverArray := ⟨FBIKSolverArray.default⟩
    

    
@[apex_type "FBIKTargetArray"]
opaque FBIKTargetArray : Type := Unit

def FBIKTargetArray.default : FBIKTargetArray := cast silentSorry ()
instance : Inhabited FBIKTargetArray := ⟨FBIKTargetArray.default⟩
    

    
@[apex_type "FloatRampArray"]
opaque FloatRampArray : Type := Unit

def FloatRampArray.default : FloatRampArray := cast silentSorry ()
instance : Inhabited FloatRampArray := ⟨FloatRampArray.default⟩
    

    
@[apex_type "GeometryArray"]
opaque GeometryArray : Type := Unit

def GeometryArray.default : GeometryArray := cast silentSorry ()
instance : Inhabited GeometryArray := ⟨GeometryArray.default⟩
    

    
@[apex_type "IntArray"]
opaque IntArray : Type := Unit

def IntArray.default : IntArray := cast silentSorry ()
instance : Inhabited IntArray := ⟨IntArray.default⟩
    

    
@[apex_type "Matrix3Array"]
opaque Matrix3Array : Type := Unit

def Matrix3Array.default : Matrix3Array := cast silentSorry ()
instance : Inhabited Matrix3Array := ⟨Matrix3Array.default⟩
    

    
@[apex_type "Matrix4Array"]
opaque Matrix4Array : Type := Unit

def Matrix4Array.default : Matrix4Array := cast silentSorry ()
instance : Inhabited Matrix4Array := ⟨Matrix4Array.default⟩
    

    
@[apex_type "SimRootDataIdArray"]
opaque SimRootDataIdArray : Type := Unit

def SimRootDataIdArray.default : SimRootDataIdArray := cast silentSorry ()
instance : Inhabited SimRootDataIdArray := ⟨SimRootDataIdArray.default⟩
    

    
@[apex_type "StringArray"]
opaque StringArray : Type := Unit

def StringArray.default : StringArray := cast silentSorry ()
instance : Inhabited StringArray := ⟨StringArray.default⟩
    

    
@[apex_type "Vector2Array"]
opaque Vector2Array : Type := Unit

def Vector2Array.default : Vector2Array := cast silentSorry ()
instance : Inhabited Vector2Array := ⟨Vector2Array.default⟩
    

    
@[apex_type "Vector3Array"]
opaque Vector3Array : Type := Unit

def Vector3Array.default : Vector3Array := cast silentSorry ()
instance : Inhabited Vector3Array := ⟨Vector3Array.default⟩
    

    
@[apex_type "Vector4Array"]
opaque Vector4Array : Type := Unit

def Vector4Array.default : Vector4Array := cast silentSorry ()
instance : Inhabited Vector4Array := ⟨Vector4Array.default⟩
    

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
