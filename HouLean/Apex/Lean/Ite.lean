import HouLean.Apex.ApexType
import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy

open HouLean Apex Compiler

/-- `TwoWaySwitch α` provides Apex implementation of `if _ then _ else _` statement
through `TwoWaySwitch` Apex node. -/
class TwoWaySwitch (α : Type) where
  twoWaySwitch (t e : α) (c : Bool) : α

instance : TwoWaySwitch Int where
  twoWaySwitch t e c := Generated.TwoWaySwitchInt t e c

instance : TwoWaySwitch Bool where
  twoWaySwitch t e c := Generated.TwoWaySwitchBool t e c

instance : TwoWaySwitch Dict where
  twoWaySwitch t e c := Generated.TwoWaySwitchDict t e c

instance : TwoWaySwitch Float where
  twoWaySwitch t e c := Generated.TwoWaySwitchFloat t e c

instance : TwoWaySwitch String where
  twoWaySwitch t e c := Generated.TwoWaySwitchString t e c

instance : TwoWaySwitch Matrix3 where
  twoWaySwitch t e c := Generated.TwoWaySwitchMatrix3 t e c

instance : TwoWaySwitch Matrix4 where
  twoWaySwitch t e c := Generated.TwoWaySwitchMatrix4 t e c

instance : TwoWaySwitch Vector2 where
  twoWaySwitch t e c := Generated.TwoWaySwitchVector2 t e c

instance : TwoWaySwitch Vector3 where
  twoWaySwitch t e c := Generated.TwoWaySwitchVector3 t e c

instance : TwoWaySwitch Vector4 where
  twoWaySwitch t e c := Generated.TwoWaySwitchVector4 t e c

instance : TwoWaySwitch DataItem where
  twoWaySwitch t e c := Generated.TwoWaySwitchDataItem t e c

instance : TwoWaySwitch Geometry where
  twoWaySwitch t e c := Generated.TwoWaySwitchGeometry t e c

instance : TwoWaySwitch IntArray where
  twoWaySwitch t e c := Generated.TwoWaySwitchIntArray t e c

instance : TwoWaySwitch AnimStack where
  twoWaySwitch t e c := Generated.TwoWaySwitchAnimStack t e c

instance : TwoWaySwitch BoolArray where
  twoWaySwitch t e c := Generated.TwoWaySwitchBoolArray t e c

instance : TwoWaySwitch ColorRamp where
  twoWaySwitch t e c := Generated.TwoWaySwitchColorRamp t e c

instance : TwoWaySwitch DictArray where
  twoWaySwitch t e c := Generated.TwoWaySwitchDictArray t e c

instance : TwoWaySwitch FloatRamp where
  twoWaySwitch t e c := Generated.TwoWaySwitchFloatRamp t e c

instance : TwoWaySwitch ApexNodeID where
  twoWaySwitch t e c := Generated.TwoWaySwitchApexNodeID t e c

instance : TwoWaySwitch ApexPortID where
  twoWaySwitch t e c := Generated.TwoWaySwitchApexPortID t e c

instance : TwoWaySwitch FBIKSolver where
  twoWaySwitch t e c := Generated.TwoWaySwitchFBIKSolver t e c

instance : TwoWaySwitch FBIKTarget where
  twoWaySwitch t e c := Generated.TwoWaySwitchFBIKTarget t e c

instance : TwoWaySwitch FloatArray where
  twoWaySwitch t e c := Generated.TwoWaySwitchFloatArray t e c

instance : TwoWaySwitch AnimChannel where
  twoWaySwitch t e c := Generated.TwoWaySwitchAnimChannel t e c

instance : TwoWaySwitch DynamicPath where
  twoWaySwitch t e c := Generated.TwoWaySwitchDynamicPath t e c

instance : TwoWaySwitch StringArray where
  twoWaySwitch t e c := Generated.TwoWaySwitchStringArray t e c

instance : TwoWaySwitch FBIKSkeleton where
  twoWaySwitch t e c := Generated.TwoWaySwitchFBIKSkeleton t e c

instance : TwoWaySwitch Matrix3Array where
  twoWaySwitch t e c := Generated.TwoWaySwitchMatrix3Array t e c

instance : TwoWaySwitch Matrix4Array where
  twoWaySwitch t e c := Generated.TwoWaySwitchMatrix4Array t e c

instance : TwoWaySwitch Vector2Array where
  twoWaySwitch t e c := Generated.TwoWaySwitchVector2Array t e c

instance : TwoWaySwitch Vector3Array where
  twoWaySwitch t e c := Generated.TwoWaySwitchVector3Array t e c

instance : TwoWaySwitch Vector4Array where
  twoWaySwitch t e c := Generated.TwoWaySwitchVector4Array t e c

instance : TwoWaySwitch GeometryArray where
  twoWaySwitch t e c := Generated.TwoWaySwitchGeometryArray t e c

instance : TwoWaySwitch SimRootDataId where
  twoWaySwitch t e c := Generated.TwoWaySwitchSimRootDataId t e c

instance : TwoWaySwitch ApexGraphHandle where
  twoWaySwitch t e c := Generated.TwoWaySwitchApexGraphHandle t e c

instance : TwoWaySwitch ApexNodeIDArray where
  twoWaySwitch t e c := Generated.TwoWaySwitchApexNodeIDArray t e c

instance : TwoWaySwitch ApexPortIDArray where
  twoWaySwitch t e c := Generated.TwoWaySwitchApexPortIDArray t e c

instance : TwoWaySwitch FBIKSolverArray where
  twoWaySwitch t e c := Generated.TwoWaySwitchFBIKSolverArray t e c

instance : TwoWaySwitch FBIKTargetArray where
  twoWaySwitch t e c := Generated.TwoWaySwitchFBIKTargetArray t e c

instance : TwoWaySwitch DynamicPathArray where
  twoWaySwitch t e c := Generated.TwoWaySwitchDynamicPathArray t e c

instance : TwoWaySwitch FBIKSkeletonArray where
  twoWaySwitch t e c := Generated.TwoWaySwitchFBIKSkeletonArray t e c

instance : TwoWaySwitch SimRootDataIdArray where
  twoWaySwitch t e c := Generated.TwoWaySwitchSimRootDataIdArray t e c

instance : TwoWaySwitch AnimChannelCollection where
  twoWaySwitch t e c := Generated.TwoWaySwitchAnimChannelCollection t e c


open TwoWaySwitch in
-- this might be dangerous and cause diamonds ...
instance (priority:=low) {α A} [ApexType α A] [TwoWaySwitch A] : TwoWaySwitch α where
  twoWaySwitch t e c := fromApex (twoWaySwitch (toApex t) (toApex e) c)

open TwoWaySwitch in
instance [TwoWaySwitch α] [TwoWaySwitch β] : TwoWaySwitch (α×β) where
  twoWaySwitch t e c := (twoWaySwitch t.1 e.1 c, twoWaySwitch t.2 e.2 c)

def ite.apex_impl {α} [TwoWaySwitch α] (c : Prop) [h : Decidable c] (t e : α) :=
  TwoWaySwitch.twoWaySwitch t e (decide c)

unsafe def dite.apex_impl {α} [TwoWaySwitch α] (c : Prop) [h : Decidable c] (t : c → α) (e : ¬c → α) :=
  TwoWaySwitch.twoWaySwitch (t (unsafeCast ())) (e (unsafeCast ())) (decide c)

run_meta compilerExt.add (.implementedByName ``ite ``ite.apex_impl
  #[some 0, none, some 1, some 2, some 3, some 4]) default

run_meta compilerExt.add (.implementedByName ``dite ``dite.apex_impl
  #[some 0, none, some 1, some 2, some 3, some 4]) default
