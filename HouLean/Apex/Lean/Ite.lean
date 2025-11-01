import HouLean.Apex.ApexType
import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy

open HouLean Apex Compiler

/-- `TwoWaySwitch α` provides Apex implementation of `if _ then _ else _` statement
through `TwoWaySwitch` Apex node. -/
class TwoWaySwitch (α : Type) where
  twoWaySwitch (c : Prop) [h : Decidable c] (t e : α) : α

instance : TwoWaySwitch Int where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchInt t e (decide c)

instance : TwoWaySwitch Bool where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchBool t e (decide c)

instance : TwoWaySwitch Dict where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchDict t e (decide c)

instance : TwoWaySwitch Float where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchFloat t e (decide c)

instance : TwoWaySwitch String where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchString t e (decide c)

instance : TwoWaySwitch Matrix3 where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchMatrix3 t e (decide c)

instance : TwoWaySwitch Matrix4 where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchMatrix4 t e (decide c)

instance : TwoWaySwitch Vector2 where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchVector2 t e (decide c)

instance : TwoWaySwitch Vector3 where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchVector3 t e (decide c)

instance : TwoWaySwitch Vector4 where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchVector4 t e (decide c)

instance : TwoWaySwitch DataItem where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchDataItem t e (decide c)

instance : TwoWaySwitch Geometry where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchGeometry t e (decide c)

instance : TwoWaySwitch IntArray where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchIntArray t e (decide c)

instance : TwoWaySwitch AnimStack where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchAnimStack t e (decide c)

instance : TwoWaySwitch BoolArray where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchBoolArray t e (decide c)

instance : TwoWaySwitch ColorRamp where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchColorRamp t e (decide c)

instance : TwoWaySwitch DictArray where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchDictArray t e (decide c)

instance : TwoWaySwitch FloatRamp where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchFloatRamp t e (decide c)

instance : TwoWaySwitch ApexNodeID where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchApexNodeID t e (decide c)

instance : TwoWaySwitch ApexPortID where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchApexPortID t e (decide c)

instance : TwoWaySwitch FBIKSolver where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchFBIKSolver t e (decide c)

instance : TwoWaySwitch FBIKTarget where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchFBIKTarget t e (decide c)

instance : TwoWaySwitch FloatArray where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchFloatArray t e (decide c)

instance : TwoWaySwitch AnimChannel where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchAnimChannel t e (decide c)

instance : TwoWaySwitch DynamicPath where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchDynamicPath t e (decide c)

instance : TwoWaySwitch StringArray where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchStringArray t e (decide c)

instance : TwoWaySwitch FBIKSkeleton where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchFBIKSkeleton t e (decide c)

instance : TwoWaySwitch Matrix3Array where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchMatrix3Array t e (decide c)

instance : TwoWaySwitch Matrix4Array where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchMatrix4Array t e (decide c)

instance : TwoWaySwitch Vector2Array where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchVector2Array t e (decide c)

instance : TwoWaySwitch Vector3Array where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchVector3Array t e (decide c)

instance : TwoWaySwitch Vector4Array where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchVector4Array t e (decide c)

instance : TwoWaySwitch GeometryArray where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchGeometryArray t e (decide c)

instance : TwoWaySwitch SimRootDataId where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchSimRootDataId t e (decide c)

instance : TwoWaySwitch ApexGraphHandle where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchApexGraphHandle t e (decide c)

instance : TwoWaySwitch ApexNodeIDArray where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchApexNodeIDArray t e (decide c)

instance : TwoWaySwitch ApexPortIDArray where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchApexPortIDArray t e (decide c)

instance : TwoWaySwitch FBIKSolverArray where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchFBIKSolverArray t e (decide c)

instance : TwoWaySwitch FBIKTargetArray where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchFBIKTargetArray t e (decide c)

instance : TwoWaySwitch DynamicPathArray where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchDynamicPathArray t e (decide c)

instance : TwoWaySwitch FBIKSkeletonArray where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchFBIKSkeletonArray t e (decide c)

instance : TwoWaySwitch SimRootDataIdArray where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchSimRootDataIdArray t e (decide c)

instance : TwoWaySwitch AnimChannelCollection where
  twoWaySwitch c _ t e := Generated.TwoWaySwitchAnimChannelCollection t e (decide c)


open TwoWaySwitch in
-- this might be dangerous and cause diamonds ...
instance (priority:=low) {α A} [ApexType α A] [TwoWaySwitch A] : TwoWaySwitch α where
  twoWaySwitch c _ t e := fromApex (twoWaySwitch c (toApex t) (toApex e))

open TwoWaySwitch in
instance [TwoWaySwitch α] [TwoWaySwitch β] : TwoWaySwitch (α×β) where
  twoWaySwitch c _ t e := (twoWaySwitch c t.1 e.1, twoWaySwitch c t.2 e.2)


/- We replace any `Decidable P` with `Bool` at APEX runtime. -/
run_meta compilerExt.add (.implementedByName ``ite ``TwoWaySwitch.twoWaySwitch 
  #[some 0, none, some 1, some 2, some 3, some 4]) default

  
