-- import HouLean.Init
import HouLean.Apex.Compile.Extension
import HouLean.Apex.Generated.Defs

namespace HouLean.Apex

-- Instruct the compiler to erase toApex and fromApex calls
open Compiler
run_meta compilerExt.add (.implementedByName ``ApexType.toApex ``id' #[none, some 3]) default
run_meta compilerExt.add (.implementedByName ``ApexType.fromApex ``id' #[none, some 3]) default

export ApexType (toApex fromApex)

instance {α β A B} [ApexType α A] [ApexType β B] : ApexType (α×β) (A×B) where
  toApex := fun (x,y) => (toApex x, toApex y)
  fromApex := fun (x,y) => (fromApex x, fromApex y)

-- todo: update generate.py
instance : ApexType Float Float where
  toApex x := x
  fromApex x := x

instance : ApexType Bool Bool where
  toApex x := x
  fromApex x := x

instance : ApexType Int Int where
  toApex x := x
  fromApex x := x

instance : ApexType String String where
  toApex x := x
  fromApex x := x

instance : ApexType FloatArray FloatArray where
  toApex x := x
  fromApex x := x

instance : ApexType Vector2 Vector2 where
  toApex x := x
  fromApex x := x

instance : ApexType Vector3 Vector3 where
  toApex x := x
  fromApex x := x

instance : ApexType Vector4 Vector4 where
  toApex x := x
  fromApex x := x

instance : ApexType Matrix2 Matrix2 where
  toApex x := x
  fromApex x := x

instance : ApexType Matrix3 Matrix3 where
  toApex x := x
  fromApex x := x

instance : ApexType Matrix4 Matrix4 where
  toApex x := x
  fromApex x := x

instance : ApexType AnimChannel AnimChannel where
  toApex x := x
  fromApex x := x

instance : ApexType AnimChannelCollection AnimChannelCollection where
  toApex x := x
  fromApex x := x

instance : ApexType AnimStack AnimStack where
  toApex x := x
  fromApex x := x

instance : ApexType ApexGraphHandle ApexGraphHandle where
  toApex x := x
  fromApex x := x

instance : ApexType ApexNodeID ApexNodeID where
  toApex x := x
  fromApex x := x

instance : ApexType ApexPortID ApexPortID where
  toApex x := x
  fromApex x := x

instance : ApexType ColorRamp ColorRamp where
  toApex x := x
  fromApex x := x

instance : ApexType DataItem DataItem where
  toApex x := x
  fromApex x := x

instance : ApexType Dict Dict where
  toApex x := x
  fromApex x := x

instance : ApexType DynamicPath DynamicPath where
  toApex x := x
  fromApex x := x

instance : ApexType FBIKSkeleton FBIKSkeleton where
  toApex x := x
  fromApex x := x

instance : ApexType FBIKSolver FBIKSolver where
  toApex x := x
  fromApex x := x

instance : ApexType FBIKTarget FBIKTarget where
  toApex x := x
  fromApex x := x

instance : ApexType FloatRamp FloatRamp where
  toApex x := x
  fromApex x := x

instance : ApexType Geometry Geometry where
  toApex x := x
  fromApex x := x

instance : ApexType ImageLayer ImageLayer where
  toApex x := x
  fromApex x := x

instance : ApexType NanoVDB NanoVDB where
  toApex x := x
  fromApex x := x

instance : ApexType SimEngine SimEngine where
  toApex x := x
  fromApex x := x

instance : ApexType SimRootDataId SimRootDataId where
  toApex x := x
  fromApex x := x

instance : ApexType VerbContext VerbContext where
  toApex x := x
  fromApex x := x

instance : ApexType AnimChannelArray AnimChannelArray where
  toApex x := x
  fromApex x := x

instance : ApexType AnimChannelCollectionArray AnimChannelCollectionArray where
  toApex x := x
  fromApex x := x

instance : ApexType AnimStackArray AnimStackArray where
  toApex x := x
  fromApex x := x

instance : ApexType ApexGraphHandleArray ApexGraphHandleArray where
  toApex x := x
  fromApex x := x

instance : ApexType ApexNodeIDArray ApexNodeIDArray where
  toApex x := x
  fromApex x := x

instance : ApexType ApexPortIDArray ApexPortIDArray where
  toApex x := x
  fromApex x := x

instance : ApexType BoolArray BoolArray where
  toApex x := x
  fromApex x := x

instance : ApexType ColorRampArray ColorRampArray where
  toApex x := x
  fromApex x := x

instance : ApexType DictArray DictArray where
  toApex x := x
  fromApex x := x

instance : ApexType DynamicPathArray DynamicPathArray where
  toApex x := x
  fromApex x := x

instance : ApexType FBIKSkeletonArray FBIKSkeletonArray where
  toApex x := x
  fromApex x := x

instance : ApexType FBIKSolverArray FBIKSolverArray where
  toApex x := x
  fromApex x := x

instance : ApexType FBIKTargetArray FBIKTargetArray where
  toApex x := x
  fromApex x := x

instance : ApexType FloatRampArray FloatRampArray where
  toApex x := x
  fromApex x := x

instance : ApexType GeometryArray GeometryArray where
  toApex x := x
  fromApex x := x

instance : ApexType IntArray IntArray where
  toApex x := x
  fromApex x := x

instance : ApexType Matrix3Array Matrix3Array where
  toApex x := x
  fromApex x := x

instance : ApexType Matrix4Array Matrix4Array where
  toApex x := x
  fromApex x := x

instance : ApexType SimRootDataIdArray SimRootDataIdArray where
  toApex x := x
  fromApex x := x

instance : ApexType StringArray StringArray where
  toApex x := x
  fromApex x := x

instance : ApexType Vector2Array Vector2Array where
  toApex x := x
  fromApex x := x

instance : ApexType Vector3Array Vector3Array where
  toApex x := x
  fromApex x := x

instance : ApexType Vector4Array Vector4Array where
  toApex x := x
  fromApex x := x


-- /-- Every ApexType should also be able to implement this function that just takes -/
-- class ToVariadicUntyped (α : Type u) (n : outParam Nat) where
--   toVarUntyped : α → VariadicArg Untyped n
--   fromVarUntyped : VariadicArg Untyped n → α

-- open ToVariadicUntyped

-- instance : ToVariadicUntyped Float 1 where
--   toVarUntyped x := #a[.float x]
--   fromVarUntyped x := 
--     match x with
--     | #a[.float x] => x
--     | _ => panic! "invalid extraction of variadic untyped!"


-- instance [ToVariadicUntyped α m] [ToVariadicUntyped β n] : ToVariadicUntyped (α × β) (m + n) where
--   toVarUntyped := fun (x,y) => (toVarUntyped x).append (toVarUntyped y)
--   fromVarUntyped := fun xy =>
--     let x : VariadicArg Untyped m := ⟨xy.1[0:m].toArray, sorry_proof⟩
--     let y : VariadicArg Untyped n := ⟨xy.1[m:m+n].toArray, sorry_proof⟩
--     (fromVarUntyped x, fromVarUntyped y)
