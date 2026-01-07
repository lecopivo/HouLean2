import HouLean.Meta.SpecializeAndSimp2.Main
import HouLean.Meta.SpecializeAndSimp2.Command
import HouLean.Meta.FloatPolynomial
import HouLean.Data.Vector
import HouLean.Data.Matrix
import HouLean.Meta.RewriteBy

namespace Test.Meta.Sas2

open Lean Elab Command Term

open HouLean.Meta.Sas


-- @[simp]
-- theorem hihi (f : α → Fin 0 → α) (init : α) : Fin.foldl 0 f init = init := by simp

-- todo: change to simproc to make faster
@[simp]
theorem Fin.foldl_inline (n) (f : α → Fin (n+1) → α) (init : α) : Fin.foldl (n+1) f init = Fin.foldl n (fun x i => f x ⟨i.1+1, by omega⟩) (f init 0) := by
  sorry_proof

-- todo: change to simproc to make faster
@[simp]
theorem Fin.foldr_inline (n) (f : Fin (n+1) → α → α) (init : α) : Fin.foldr (n+1) f init = Fin.foldr n (fun i x => f ⟨i.1, by omega⟩ x) (f ⟨n, by omega⟩ init) := by
  sorry_proof

attribute [simp] HouLean.sum -- remove this as it should be inlined automatically

/-- info: fun x => 0 + x.x0 + x.x1 + x.x2 -/
#guard_msgs in
#sas (fun x : Vector Float 3 => ∑ (i : Fin 3), x[i])

/-- info: fun f => 0 + f 0 + f 1 + f 2 + f 3 + f 4 + f 5 + f 6 + f 7 + f 8 + f 9 -/
#guard_msgs in
#sas (fun f : Fin 10 → Float => ∑ i, f i)
