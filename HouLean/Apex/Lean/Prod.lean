import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy

open HouLean Apex Compiler

unsafe def Prod.rec.apex_impl {α β} {motive : α × β → Sort u_1}
  (mk : (fst : α) → (snd : β) → motive (fst, snd)) (t : α × β) : motive t := 
  mk t.1 t.2

run_meta compilerExt.add (.implementedByName ``Prod.rec ``Prod.rec.apex_impl #[some 0, some 1, some 2, some 3, some 4]) default
