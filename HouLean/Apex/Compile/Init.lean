import HouLean.Apex.Generated.Defs
import HouLean.Meta.ProdLike

namespace HouLean

open Apex

/-- List of element of type `α` whose length `n` has to be known at the APEX compile time. -/
inductive Apex.VariadicArg (α : Type u) : Nat → Type u where
  | nil : VariadicArg α 0
  | cons {n} (head : α) (tail : VariadicArg α n) : VariadicArg α (n+1)
-- def VariadicArg (α : Type u) (n : Nat) := Vector α n

protected def VariadicArg.default [Inhabited α] : (n : Nat) → VariadicArg α n
  | 0 => .nil
  | n+1 => .cons default (VariadicArg.default n)

instance {α} [Inhabited α] : Inhabited (VariadicArg α n) := ⟨VariadicArg.default n⟩

syntax "#a[" term,* "]" : term
macro_rules 
| `(#a[]) => `(VariadicArg.nil)
| `(#a[$x:term]) => `(VariadicArg.cons $x .nil)
| `(#a[$x:term,$xs:term,*]) => `(VariadicArg.cons $x #a[$xs:term,*])


inductive Apex.VariadicArg' : List ApexTypeTag  → Type u where
  | nil : VariadicArg' []
  | cons {t : ApexTypeTag} {ts : List ApexTypeTag} (head : t.toType) (tail : VariadicArg' ts) : VariadicArg' (t::ts)

namespace Apex.VariadicArg'
def append {ts ts'} (xs : VariadicArg' ts) (ys : VariadicArg' ts') : VariadicArg' (ts ++ ts') := 
  match ts, xs with
  | [], _ => ys
  | _::_, .cons x xs => .cons x (xs.append ys)

def split {ts ts'} (xs : VariadicArg' (ts++ts')) : VariadicArg' ts × VariadicArg' ts' := 
  match ts, xs with
  | [], xs => (.nil, xs)
  | _::_, .cons x xys => 
    let (xs,y) := xys.split
    (.cons x xs, y)

end Apex.VariadicArg'

/-- This class maps Lean type `α` to Apex compatible type `β`. 

This extensible type level function mapping α to β is used to 
transform Lean code to a smaller subset of Lean which is supported
by Apex compiler. Therefore many APEX compiler extensions can be done
through providing instances like `ApexType` and one does not have to
touch the compiler!.
-/
class ApexType (α : Type u) (β : outParam (Type v)) where
  toApex : α → β
  fromApex : β → α

export ApexType (toApex fromApex)

class ApexTypeFlatten (α : Type u) (ts : outParam (List ApexTypeTag)) : Type u where
  apexFlatten : α → VariadicArg' ts
  apexUnflatten : VariadicArg' ts → α

export ApexTypeFlatten (apexFlatten apexUnflatten)


----------------------------------------------------------------------------------------------------
-- Instances for ApexType

instance {α β A B} [ApexType α A] [ApexType β B] : ApexType (α×β) (A×B) where
  toApex := fun (x,y) => (toApex x, toApex y)
  fromApex := fun (x,y) => (fromApex x, fromApex y)

instance {α P AP} [ProdLike α P] [ApexType P AP] : ApexType α AP where
  toApex := fun x => toApex (toProdType x)
  fromApex := fun x => fromProdType (fromApex x)

instance {α} [ApexType α A] : ApexType (Id α) A where
  toApex := fun x => toApex (α:=α) x
  fromApex := fun x => fromApex (α:=α) x


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


----------------------------------------------------------------------------------------------------
-- Instances for ApexTypeFlatten

instance {α β ts ts'} [ApexTypeFlatten α ts] [ApexTypeFlatten β ts'] : ApexTypeFlatten (α×β) (ts++ts') where
  apexFlatten := fun (x,y) => (apexFlatten x).append (apexFlatten y)
  apexUnflatten := fun xy => 
    let (x,y) := xy.split
    (apexUnflatten x, apexUnflatten y)

unsafe instance [ApexTypeFlatten α ts] [ApexTypeFlatten β ss] : ApexTypeFlatten (MProd α β) (ts ++ ss) where
  apexFlatten := fun ⟨x,y⟩ => (apexFlatten x).append (apexFlatten y)
  apexUnflatten := fun xy => 
    let (x,y) := xy.split
    ⟨apexUnflatten x, apexUnflatten y⟩

instance {α P AP} [ProdLike α P] [ApexTypeFlatten P AP] : ApexTypeFlatten α AP where
  apexFlatten x := apexFlatten (toProdType x)
  apexUnflatten x := fromProdType (apexUnflatten x)

instance {α A} [ApexType α A] [ApexTypeFlatten A ts] : ApexTypeFlatten α ts where
  apexFlatten x := apexFlatten (toApex x)
  apexUnflatten x := fromApex (apexUnflatten x)


-- todo: update generate.py
instance : ApexTypeFlatten Float [.float] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten Bool [.bool] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten Int [.int] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten String [.string] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten FloatArray [.floatArray] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten Vector2 [.vector2] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten Vector3 [.vector3] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten Vector4 [.vector4] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

-- instance : ApexTypeFlatten Matrix2 [.matrix2] where
--   apexFlatten x := .cons x .nil
--   apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten Matrix3 [.matrix3] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten Matrix4 [.matrix4] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten AnimChannel [.animChannel] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten AnimChannelCollection [.animChannelCollection] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten AnimStack [.animStack] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten ApexGraphHandle [.apexGraphHandle] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten ApexNodeID [.apexNodeID] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten ApexPortID [.apexPortID] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten ColorRamp [.colorRamp] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten DataItem [.dataItem] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten Dict [.dict] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten DynamicPath [.dynamicPath] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten FBIKSkeleton [.fBIKSkeleton] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten FBIKSolver [.fBIKSolver] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten FBIKTarget [.fBIKTarget] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten FloatRamp [.floatRamp] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten Geometry [.geometry] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten ImageLayer [.imageLayer] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten NanoVDB [.nanoVDB] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten SimEngine [.simEngine] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten SimRootDataId [.simRootDataId] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten VerbContext [.verbContext] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten AnimChannelArray [.animChannelArray] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten AnimChannelCollectionArray [.animChannelCollectionArray] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten AnimStackArray [.animStackArray] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten ApexGraphHandleArray [.apexGraphHandleArray] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten ApexNodeIDArray [.apexNodeIDArray] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten ApexPortIDArray [.apexPortIDArray] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten BoolArray [.boolArray] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten ColorRampArray [.colorRampArray] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten DictArray [.dictArray] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten DynamicPathArray [.dynamicPathArray] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten FBIKSkeletonArray [.fBIKSkeletonArray] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten FBIKSolverArray [.fBIKSolverArray] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten FBIKTargetArray [.fBIKTargetArray] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten FloatRampArray [.floatRampArray] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten GeometryArray [.geometryArray] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten IntArray [.intArray] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten Matrix3Array [.matrix3Array] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten Matrix4Array [.matrix4Array] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten SimRootDataIdArray [.simRootDataIdArray] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten StringArray [.stringArray] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten Vector2Array [.vector2Array] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten Vector3Array [.vector3Array] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten Vector4Array [.vector4Array] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x
