import HouLean.Meta.ProdLike
import Lean.Elab.GuardMsgs

open HouLean

namespace Tests.Meta.ProdLike

structure Foo (α : Type u) where
  a : Float
  b : Float
  s : String
  q : Array α
  e : Unit
deriving ProdLike


/-- info: (1.000000, (2.000000, "adsf"), #[1, 2, 3], ()) -/
#guard_msgs in
#eval (toProdType (Foo.mk 1.0 2.0 "adsf" #[1,2,3] ()))


/-- info: { a := 1.000000, b := 2.000000, s := "adsf", q := #[1, 2, 3], e := () } -/
#guard_msgs in
#eval fromProdType (A := Foo _) (toProdType (Foo.mk 1.0 2.0 "adsf" #[1,2,3] ()))


instance [Add A] [Add B] : Add (A×B) := ⟨fun (x,x') (y,y') => (x+y, x'+y')⟩
instance [ProdLike A P] [Add P] : Add A := ⟨fun x y => fromProdType (toProdType x + toProdType y)⟩

structure Float3 where
  x : Float
  y : Float
  z : Float
deriving ProdLike

/-- info: inferInstance : ProdLike Float3 (Float × Float × Float) -/
#guard_msgs in
#check (by infer_instance : ProdLike Float3 _)

variable (x y : Float3)

/-- info: Add.add x y : Float3 -/
#guard_msgs in
#check Add.add x y


set_option pp.funBinderTypes true


/-- info: fun (x : Float3) => toProdType x : Float3 → Float × Float × Float -/
#guard_msgs in
#check (fun x : Float3 => toProdType x)

/-- info: fun (x : Float × Float × Float) => fromProdType x : Float × Float × Float → Float3 -/
#guard_msgs in
#check (fun x : _ => (fromProdType x : Float3))


structure Single where
  x : Float
deriving ProdLike
