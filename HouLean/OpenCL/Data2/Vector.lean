import HouLean.OpenCL.Compiler.RewriteRules
import HouLean.OpenCL.Compiler.SpecAndSimp
import HouLean.OpenCL.Reference
import HouLean.Data.Vector

namespace HouLean.OpenCL

open Compiler3 Meta


@[opencl_csimp]
theorem vector_ofFn {α} {n : Nat} (f : Fin n → α) :
    Vector.ofFn f = Vector.mk (unroll (List.ofFn f)).toArray (by simp[unroll]) := by simp[unroll]
