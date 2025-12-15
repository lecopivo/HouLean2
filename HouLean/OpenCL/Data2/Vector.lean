import HouLean.OpenCL.Compiler.RewriteRules
import HouLean.OpenCL.Compiler.SpecAndSimp
import HouLean.OpenCL.Reference
import HouLean.Data.Vector

namespace HouLean.OpenCL

open Compiler3 Meta


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
theorem vector_replicate {α} {n : Nat} (a : α) :
    Vector.replicate n a = .ofFn fun i => a := by ext i; simp
