import HouLean.OpenCL.Compiler.Main
import HouLean.OpenCL.Data.Int
import HouLean.OpenCL.Data.Vector

open HouLean.OpenCL

instance [t : OpenCLType α] : OpenCLType (Id α) := t
instance [t : OpenCLType α] : OpenCLType (ForInStep α) where
  name := t.name
  shortName := t.shortName

implemented_by : Nat.toFloat = oclFunction _ "(float)"

attribute [opencl_csimp] pure_bind bind_pure bind_assoc Id.run

/--
info:
double3 (anonymous)(double a)
{
    double state = a;
    for (uint i = 0; i < 10; i += 2)
    {
        double x = (state * state) * (float)(i);
        double x1 = x + x;
        state = x1;
    }
    double r = state;
    return (double3){(r + r), (r * r), (5.0d * r)};
}
-/
#guard_msgs in
#opencl_compile (fun a : Float =>
  Id.run do
    let mut x : Float := a
    for i in [0:10:2] do
      x := x * x * i.toFloat
      x := x+x
    #v[x + x, x*x, 5*x])

/--
info:
double3 (anonymous)(double a)
{
    double state = a;
    for (uint i = 0; i < 10; i += 2)
    {
        double state1 = state;
        for (uint j = 5; j < 20; j += 1)
        {
            double x = ((state1 * state1) * (float)(i)) * (float)(j);
            double x1 = x + x;
            state1 = x1;
        }
        double r = state1;
        state = r;
    }
    double r = state;
    return (double3){(r + r), (r * r), (5.0d * r)};
}
-/
#guard_msgs in
#opencl_compile (fun a : Float =>
  Id.run do
    let mut x : Float := a
    for i in [0:10:2] do
      for j in [5:20] do
        x := x * x * i.toFloat * j.toFloat
        x := x+x
    #v[x + x, x*x, 5*x])

#exit

noncomputable
abbrev foo := Id.run do
    let mut x : Float := oclFunction _ "xx"
    let mut y : Float := oclFunction _ "yy"
    for _ in [0:10] do
      let tmp := y
      y := x + y
      x := tmp
    y

attribute [opencl_csimp] foo

instance [a : OpenCLType α] [b : OpenCLType β] : OpenCLType (MProd α β) where
  name := s!"prod{a.shortName}{b.shortName}"
  shortName := s!"p{a.shortName}{b.shortName}"
  definition? :=
    s!"struct prod{a.shortName}{b.shortName}\n\
       \{\n\
         {a.name} fst;\n\
         {b.name} snd;\n\
       };"


implemented_by [Inhabited α] [Inhabited β] [t : OpenCLType (MProd α β)] :
  MProd.mk (α:=α) (β:=β)
  =
  oclFunction _ s!"({t.name})" .constructor

implemented_by [Inhabited α] :
  MProd.fst (α:=α) (β:=β)
  =
  oclFunction _ ".fst" .postfix

implemented_by [Inhabited α] (x : MProd α β) :
  x.1
  =
  (oclFunction (MProd α β → α) ".fst" .postfix) x

implemented_by [Inhabited β] (x : MProd α β) :
  x.2
  =
  (oclFunction (MProd α β → β) ".snd" .postfix) x

implemented_by [Inhabited β] :
  MProd.snd (α:=α) (β:=β)
  =
  oclFunction _ s!".snd" .postfix

set_option trace.HouLean.OpenCL.compiler true in
open Qq Compiler Lean Meta in
run_meta
  let info ← getConstInfo ``foo
  let e := info.value!
  let stmts ← compileExpr'' e
  let cs ← stmts.mapM (·.toString)
  logInfo m!"{cs}"
