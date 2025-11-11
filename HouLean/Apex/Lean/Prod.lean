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
