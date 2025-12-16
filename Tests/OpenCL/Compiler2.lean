import HouLean.OpenCL.Compiler
import HouLean.OpenCL.Reference
import HouLean.Data.Float

open HouLean OpenCL Compiler Lean

namespace Test.OpenCL.Compiler


open Lean Elab Term Meta in
elab c:"#ocl_compile" f:term : command => do
  withoutModifyingEnv do
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

/--
info: {
      Prod_double_Prod_double_double p = (Prod_double_Prod_double_double){x, (Prod_double_double){x * y, x + x}};
      double a = p.snd.fst;
      double b = p.snd.snd;
      return a + b;
}
-/
#guard_msgs in
#ocl_compile fun x y : Float =>
  let p := (x,x*y,x+x)
  let a := p.2.1
  let b := p.2.2
  a + b


def foo (x y : Float) :=
  let a := x * y
  let b := a - x + y
  a + x * y / b

/--
info: double test_opencl_compiler_foo(double x, double y){
      double a = x * y;
      double b = a - x + y;
      return a + x * y / b;
}
-/
#guard_msgs in
run_meta
  let f ← compileDecl ``foo
  logInfo m!"{f}"

/--
info: {
      return test_opencl_compiler_foo(x, y);
}
-/
#guard_msgs in
#ocl_compile fun x y : Float => foo x y


#exit

inductive AttrClass where
  | point | primitive


#ocl_compile fun x y : Float => (x,y)

#ocl_compile fun x y : Float => AttrClass.point

#ocl_compile fun (x y : Float) (cls : AttrClass) => AttrClass.rec x y cls

#ocl_compile fun (x y : Float) (cls : AttrClass) => match cls with | .point => x | .primitive => y




def bar {R} [FloatType R] (x y : R) :=
  let a := x * y
  let b := a - x + y
  a + x * y / b

variable (x y : Float)

#opencl_sas (bar x y)


set_option pp.funBinderTypes true

open Lean Meta Qq
run_meta
  let (t,_) ← compileType q(Nat × (Float × Float) × (Float × Float)) {} {}
  logInfo m!"{t}"


#ocl_compile fun x y : Float × Float =>
  let p := x
  let q := (x.1,x.1)
  x.1 + x.2


open Lean Meta Qq
run_meta
  let (t,_) ← compileType q(MProd Nat (Float × Float) × (Float × Float)) {} {}
  logInfo m!"{t}"

open Lean Meta Qq
run_meta
  let (t,_) ← compileType q(Vector (Vector (Vector Float32 4) 4) 4) {} {}
  logInfo m!"{t}"
