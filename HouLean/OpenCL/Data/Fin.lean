import HouLean.OpenCL.Compiler.Main
import HouLean.OpenCL.Data.Init
import HouLean.OpenCL.Data.InlinedLoop

namespace HouLean.OpenCL

variable {m} [Monad m]

opencl_rewrite (f : α → Fin n → α) (init : α) :
  Fin.foldl n f init
  =
  inlinedLoop n (fun i s => f s i) init

opencl_rewrite (f : α → Fin n → m α) (init : α) :
  Fin.foldlM n f init
  =
  inlinedLoopM n (fun i s => f s i) init

opencl_rewrite (f : Fin n → α → α) (init : α) :
  Fin.foldr n f init
  =
  inlinedLoop n (fun i s => f ⟨n-i.1-1, by omega⟩ s) init

opencl_rewrite (f : Fin n → α → m α) (init : α) :
  Fin.foldrM n f init
  =
  inlinedLoopM n (fun i s => f ⟨n-i.1-1, by omega⟩ s) init

opencl_rewrite (P : Fin n → Prop) [∀ i, Decidable (P i)] :
  decide (∀ i, P i) = inlinedLoop n (fun i r => r && decide (P i)) (init:=true)
