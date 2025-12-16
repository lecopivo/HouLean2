import HouLean.OpenCL.Compiler.RewriteRules
import HouLean.OpenCL.Reference.Operators
import HouLean.OpenCL.Reference.DataTypes
import HouLean.OpenCL.Reference.VectorComponentAddressing
import HouLean.Meta.RewriteBy

open HouLean OpenCL Math Compiler2

variable {R : Type} {m n : Nat}

open Qq


open Lean Elab Term Meta in
elab c:"#ocl_compile_expr" f:term : command => do
  Command.runTermElabM fun _ => do
    let f ← elabTermAndSynthesize f none
    lambdaTelescope f fun _ b => do
      let stx ← compileExpr b
      logInfoAt c stx


open Lean Elab Term Meta in
macro "#ocl_compile_expr_with_csimp" f:term : command =>
 `(#ocl_compile_expr ($f) rewrite_by simp only [opencl_csimp])



/-- info: x + y -/
#guard_msgs in
#ocl_compile_expr fun x y : Float => x + y


/-- info: (float2){x, y + x} -/
#guard_msgs in
#ocl_compile_expr fun x y : Float32 => #v[x,y + x]

/-- info: (double3){x, x * y, z} -/
#guard_msgs in
#ocl_compile_expr fun x y z : Float => #v[x,x * y,z]

/-- info: (uint3){x, y, z} -/
#guard_msgs in
#ocl_compile_expr fun x y z : Nat => #v[x,y,z]

/-- info: v.x -/
#guard_msgs in
#ocl_compile_expr fun v : Vector Float 4 => v[0]

/-- info: v.y -/
#guard_msgs in
#ocl_compile_expr fun v : Vector Float 4 => v[1]

/-- info: v.z -/
#guard_msgs in
#ocl_compile_expr fun v : Vector Float 4 => v[2]

/-- info: v.w -/
#guard_msgs in
#ocl_compile_expr fun v : Vector Float 4 => v[3]

/-- info: v.x -/
#guard_msgs in
#ocl_compile_expr_with_csimp fun v : Vector Float 4 => v.x

/-- info: v.y -/
#guard_msgs in
#ocl_compile_expr_with_csimp fun v : Vector Float 4 => v.y

/-- info: v.z -/
#guard_msgs in
#ocl_compile_expr_with_csimp fun v : Vector Float 4 => v.z

/-- info: v.w -/
#guard_msgs in
#ocl_compile_expr_with_csimp fun v : Vector Float 4 => v.w
