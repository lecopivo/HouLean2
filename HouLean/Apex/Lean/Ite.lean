import HouLean.Apex.ApexType
import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy

open HouLean Apex Compiler

/-- `TwoWaySwitch α` provides Apex implementation of `if _ then _ else _` statement
through `TwoWaySwitch` Apex node. -/
class TwoWaySwitch (α : Type) where
  twoWaySwitch (falseCase trueCase : α) (c : Bool) : α

instance : TwoWaySwitch Int where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchInt falseCase trueCase cond

instance : TwoWaySwitch Bool where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchBool falseCase trueCase cond

instance : TwoWaySwitch Dict where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchDict falseCase trueCase cond

instance : TwoWaySwitch Float where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchFloat falseCase trueCase cond

instance : TwoWaySwitch String where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchString falseCase trueCase cond

instance : TwoWaySwitch Matrix3 where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchMatrix3 falseCase trueCase cond

instance : TwoWaySwitch Matrix4 where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchMatrix4 falseCase trueCase cond

instance : TwoWaySwitch Vector2 where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchVector2 falseCase trueCase cond

instance : TwoWaySwitch Vector3 where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchVector3 falseCase trueCase cond

instance : TwoWaySwitch Vector4 where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchVector4 falseCase trueCase cond

instance : TwoWaySwitch DataItem where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchDataItem falseCase trueCase cond

instance : TwoWaySwitch Geometry where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchGeometry falseCase trueCase cond

instance : TwoWaySwitch IntArray where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchIntArray falseCase trueCase cond

instance : TwoWaySwitch AnimStack where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchAnimStack falseCase trueCase cond

instance : TwoWaySwitch BoolArray where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchBoolArray falseCase trueCase cond

instance : TwoWaySwitch ColorRamp where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchColorRamp falseCase trueCase cond

instance : TwoWaySwitch DictArray where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchDictArray falseCase trueCase cond

instance : TwoWaySwitch FloatRamp where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchFloatRamp falseCase trueCase cond

instance : TwoWaySwitch ApexNodeID where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchApexNodeID falseCase trueCase cond

instance : TwoWaySwitch ApexPortID where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchApexPortID falseCase trueCase cond

instance : TwoWaySwitch FBIKSolver where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchFBIKSolver falseCase trueCase cond

instance : TwoWaySwitch FBIKTarget where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchFBIKTarget falseCase trueCase cond

instance : TwoWaySwitch FloatArray where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchFloatArray falseCase trueCase cond

instance : TwoWaySwitch AnimChannel where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchAnimChannel falseCase trueCase cond

instance : TwoWaySwitch DynamicPath where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchDynamicPath falseCase trueCase cond

instance : TwoWaySwitch StringArray where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchStringArray falseCase trueCase cond

instance : TwoWaySwitch FBIKSkeleton where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchFBIKSkeleton falseCase trueCase cond

instance : TwoWaySwitch Matrix3Array where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchMatrix3Array falseCase trueCase cond

instance : TwoWaySwitch Matrix4Array where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchMatrix4Array falseCase trueCase cond

instance : TwoWaySwitch Vector2Array where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchVector2Array falseCase trueCase cond

instance : TwoWaySwitch Vector3Array where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchVector3Array falseCase trueCase cond

instance : TwoWaySwitch Vector4Array where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchVector4Array falseCase trueCase cond

instance : TwoWaySwitch GeometryArray where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchGeometryArray falseCase trueCase cond

instance : TwoWaySwitch SimRootDataId where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchSimRootDataId falseCase trueCase cond

instance : TwoWaySwitch ApexGraphHandle where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchApexGraphHandle falseCase trueCase cond

instance : TwoWaySwitch ApexNodeIDArray where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchApexNodeIDArray falseCase trueCase cond

instance : TwoWaySwitch ApexPortIDArray where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchApexPortIDArray falseCase trueCase cond

instance : TwoWaySwitch FBIKSolverArray where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchFBIKSolverArray falseCase trueCase cond

instance : TwoWaySwitch FBIKTargetArray where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchFBIKTargetArray falseCase trueCase cond

instance : TwoWaySwitch DynamicPathArray where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchDynamicPathArray falseCase trueCase cond

instance : TwoWaySwitch FBIKSkeletonArray where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchFBIKSkeletonArray falseCase trueCase cond

instance : TwoWaySwitch SimRootDataIdArray where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchSimRootDataIdArray falseCase trueCase cond

instance : TwoWaySwitch AnimChannelCollection where
  twoWaySwitch falseCase trueCase cond := Generated.TwoWaySwitchAnimChannelCollection falseCase trueCase cond


open TwoWaySwitch in
-- this might be dangerous and cause diamonds ...
instance (priority:=low) {α A} [ApexType α A] [TwoWaySwitch A] : TwoWaySwitch α where
  twoWaySwitch falseCase trueCase cond := 
    fromApex (twoWaySwitch (toApex falseCase) (toApex trueCase) cond)

open TwoWaySwitch in
instance [TwoWaySwitch α] [TwoWaySwitch β] : TwoWaySwitch (α×β) where
  twoWaySwitch falseCase trueCase cond := 
    (twoWaySwitch falseCase.1 trueCase.1 cond, twoWaySwitch falseCase.2 trueCase.2 cond)

def ite.apex_impl {α} [TwoWaySwitch α] (c : Prop) [h : Decidable c] (t e : α) :=
  TwoWaySwitch.twoWaySwitch e t (decide c)

unsafe def dite.apex_impl {α} [TwoWaySwitch α] (c : Prop) [h : Decidable c] (t : c → α) (e : ¬c → α) :=
  TwoWaySwitch.twoWaySwitch (e (unsafeCast ())) (t (unsafeCast ())) (decide c)

run_meta compilerExt.add (.implementedByName ``ite ``ite.apex_impl
  #[some 0, none, some 1, some 2, some 3, some 4]) default

run_meta compilerExt.add (.implementedByName ``dite ``dite.apex_impl
  #[some 0, none, some 1, some 2, some 3, some 4]) default
