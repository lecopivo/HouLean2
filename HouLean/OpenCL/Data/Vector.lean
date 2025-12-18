import HouLean.OpenCL.Compiler.SpecAndSimp
import HouLean.OpenCL.Basic
import HouLean.Data.Vector

namespace HouLean.OpenCL

open Compiler Meta

@[opencl_csimp]
theorem vector_ofFn {α} {n : Nat} (f : Fin n → α) :
    Vector.ofFn f = Vector.mk (unroll (List.ofFn f)).toArray (by simp[unroll]) := by simp[unroll]

@[opencl_csimp]
theorem vector_map {α β} {n : Nat} (f : α → β) (u : Vector α n) :
    u.map f = .ofFn fun i => f u[i] := by ext i; simp

@[opencl_csimp]
theorem vector_mapIdx {α β} {n : Nat} (f : Nat → α → β) (u : Vector α n) :
    u.mapIdx f = .ofFn fun i => f i u[i] := by ext i; simp

@[opencl_csimp]
theorem vector_mapFinIdx {α β} {n : Nat} (f : (i : Nat) → α → i < n → β) (u : Vector α n) :
    u.mapFinIdx f = .ofFn fun i => f i u[i] (i.2) := by ext i; simp

@[opencl_csimp]
theorem vector_foldl {α β} {n : Nat} (f : β → α → β) (b : β) (u : Vector α n) :
    u.foldl f b = Fin.foldl n (fun b i => f b u[i]) b := sorry_proof

@[opencl_csimp]
theorem vector_foldr {α β} {n : Nat} (f : α → β → β) (b : β) (u : Vector α n) :
    u.foldr f b = Fin.foldr n (fun i b => f u[i] b) b := sorry_proof

@[opencl_csimp]
theorem vector_sum [Add α] [Zero α] (u : Vector α n) :
    u.sum = ∑ (i : Fin n), u[i] := sorry_proof

@[opencl_csimp]
theorem vector_replicate {α} {n : Nat} (a : α) :
    Vector.replicate n a = .ofFn fun _ => a := by ext i; simp


-- how can we generalize this?
@[opencl_csimp]
theorem vector_ofFnM1 {m} [Monad m] {α} (f : Fin 1 → m α) :
    Vector.ofFnM f = do return #v[← f 0] := sorry_proof

@[opencl_csimp]
theorem vector_ofFnM2 {m} [Monad m] {α} (f : Fin 2 → m α) :
    Vector.ofFnM f = do return #v[← f 0, ← f 1] := sorry_proof

@[opencl_csimp]
theorem vector_ofFnM3 {m} [Monad m] {α} (f : Fin 3 → m α) :
    Vector.ofFnM f = do return #v[← f 0, ← f 1, ← f 2] := sorry_proof

@[opencl_csimp]
theorem vector_ofFnM4 {m} [Monad m] {α} (f : Fin 4 → m α) :
    Vector.ofFnM f = do return #v[← f 0, ← f 1, ← f 2, ← f 3] := sorry_proof
