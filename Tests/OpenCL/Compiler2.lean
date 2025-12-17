import HouLean.OpenCL.Compiler
import HouLean.OpenCL.Reference
import HouLean.Data.Float
import HouLean.Meta.RewriteBy

open HouLean OpenCL Compiler Lean

namespace Test.OpenCL.Compiler


open Lean Elab Term Meta in
elab c:"#ocl_compile" f:term : command => do
  withoutModifyingEnv do
  Command.runTermElabM fun _ => do
    let f ← elabTermAndSynthesize f none
    lambdaTelescope f fun xs b => do
      let go : CompileM Unit := do
        withFVars xs fun _ => do
          compileBlock b
      let (_,s) ← go {} {}
      let stx ← `(clCompStmt| { $s.statements* })
      logInfoAt c stx

/--
info: {
      const double a = x * y;
      return a + x * y;
}
-/
#guard_msgs in
#ocl_compile fun x y : Float =>
  let a := x * y
  a + x * y

/--
info: {
      const double a = x * y;
      const double b = a - x + y;
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
      const Prod_double_Prod_double_double p = (Prod_double_Prod_double_double){x, (Prod_double_double){x * y, x + x}};
      const double a = p.snd.fst;
      const double b = p.snd.snd;
      return a + b;
}
-/
#guard_msgs in
#ocl_compile fun x y : Float =>
  let p := (x,x*y,x+x)
  let a := p.2.1
  let b := p.2.2
  a + b


/--
info: {
      if (y >= x)
        {
              return x + y;
        } else
        {
              return x * y;
        }
}
-/
#guard_msgs in
#ocl_compile fun x y : Float =>
  if x ≤ y then
    x + y
  else
    x * y

impl_by {α : Type} (m : Type → Type) [Monad m] (x : α) : pure (f:=m) x ==> x

/--
info: {
      const uint id = get_global_id(n);
      if (10 > id)
        {
              return x;
        } else
        {
              return y;
        }
}
-/
#guard_msgs in
#ocl_compile (fun x y : Float => do
  let id ← getGlobalId 0
  if id < 10 then
    return x
  return y)
  rewrite_by
    simp -zeta only [pure_bind, bind_pure, bind_assoc]


/--
info: {
      const uint id = get_global_id(n);
      const double w = x;
      if (10 > id)
        {
              const double w1 = w + y;
              return w1;
        } else
        {
              return w;
        }
}
-/
#guard_msgs in
#ocl_compile (fun x y : Float => do
  let id ← getGlobalId 0
  let mut w := x
  if id < 10 then
    w := w + y
  return w)

impl_by {α : Type} (x : α) : ForInStep.yield x ==> x

set_option trace.HouLean.OpenCL.compiler true in
#ocl_compile (fun x : Float => do
  let id ← getGlobalId 0
  let mut x := x
  let mut y := x
  for i in [0:id.toNat] do
    x := x + x*x + i.toFloat
    y := x + y
  return x*y)
  rewrite_by
    simp -zeta only [pure_bind, bind_pure, bind_assoc]


def foo (x y : Float) :=
  let a := x * y
  let b := a - x + y
  a + x * y / b

/--
info: double test_opencl_compiler_foo(double x, double y){
      const double a = x * y;
      const double b = a - x + y;
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
