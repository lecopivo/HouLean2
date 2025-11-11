import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy

open HouLean Apex Compiler

unsafe def Prod.rec.apex_impl {α β} {motive : α × β → Sort u_1}
  (mk : (fst : α) → (snd : β) → motive (fst, snd)) (t : α × β) : motive t := 
  let s := t
  mk s.1 s.2

run_meta compilerExt.add (.implementedByName ``Prod.rec ``Prod.rec.apex_impl #[some 0, some 1, some 2, some 3, some 4]) default

instance [ApexType α A] [ApexType β B] : ApexType (MProd α β) (A × B) where
  toApex := fun ⟨x,y⟩ => (toApex x, toApex y)
  fromApex := fun ⟨x,y⟩ => ⟨fromApex x, fromApex y⟩

unsafe def MProd.rec.apex_impl {α β} {motive : MProd α β → Sort u_1}
  (mk : (fst : α) → (snd : β) → motive (MProd.mk fst snd)) (t : MProd α β) : motive t := 
  let s := t
  mk s.1 s.2

run_meta compilerExt.add (.implementedByName ``MProd.rec ``MProd.rec.apex_impl #[some 0, some 1, some 2, some 3, some 4]) default



namespace HouLean

class ProdMap (A B : Type) (As : Type) (Bs : outParam Type) where
  prodMap : (A → B) → As → Bs

instance {A B : Type} : ProdMap A B A B where
  prodMap f := f

open ProdMap in
instance {A B : Type} [ProdMap A B As Bs] [ProdMap A B As' Bs'] : ProdMap A B (As×As') (Bs×Bs') where
  prodMap f := fun (a,a') =>
    (prodMap f a, prodMap f a')

def prodMap {A B As Bs} [ProdMap A B As Bs] (as : As) (f : A → B) := ProdMap.prodMap f as 



class ProdFold (A B : Type) (As : Type) where
  prodFold : (B → A → B) → B → As → B

instance {A B : Type} : ProdFold A B A where
  prodFold op init a := op init a

open ProdFold in
instance {A B : Type} [ProdFold A B As] [ProdFold A B As'] : ProdFold A B (As×As') where
  prodFold := fun op init (a,a') =>
    prodFold op (prodFold op init a) a'

def prodFold {A B As} [ProdFold A B As] (as : As) (init : B) (op : B → A → B) := ProdFold.prodFold op init as
