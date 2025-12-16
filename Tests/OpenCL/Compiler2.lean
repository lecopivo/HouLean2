import HouLean.OpenCL.Compiler
import HouLean.OpenCL.Reference

open HouLean.OpenCL Compiler2

namespace Test.OpenCL.Compiler2



open Lean Elab Term Meta in
elab c:"#ocl_compile" f:term : command => do
  Command.runTermElabM fun _ => do
    let f ← elabTermAndSynthesize f none
    lambdaTelescope f fun _ b => do
      let (_,s) ← compileBlock b {} {}
      let stx ← `(clCompStmt| { $s.statements* })
      logInfoAt c stx



/--
info: {
      double a = x * y;
      return a + x * y;
}
-/
#guard_msgs in
#ocl_compile fun x y : Float =>
  let a := x * y
  a + x * y

/--
info: {
      double a = x * y;
      double b = a - x + y;
      return a + x * y / b;
}
-/
#guard_msgs in
#ocl_compile fun x y : Float =>
  let a := x * y
  let b := a - x + y
  a + x * y / b


def foo (x y : Float) :=
  let a := x * y
  let b := a - x + y
  a + x * y / b

run_meta
  compileDecl ``foo

#ocl_compile fun x y : Float => foo x y


-- open Lean Meta Qq
-- run_meta
--   let t ← compileType q(Nat × Float)
--   logInfo m!"{t}"

-- open Lean Meta Qq
-- run_meta
--   let t ← compileType q(Vector (Vector (Vector Float32 4) 4) 4)
--   logInfo m!"{t}"
