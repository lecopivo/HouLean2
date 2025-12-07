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

-- set_option trace.HouLean.OpenCL.compiler true in
-- #opencl_compile (fun x : Float => Id.run do
--   let mut x := x
--   for i in [0:5] do
--     x := x * x * i.toFloat
--   x + x)


set_option trace.HouLean.OpenCL.compiler true in
open Qq Compiler Lean Meta in
run_meta

  let e := q(Id.run do
    let mut x : Float := oclFunction _ "asdf"
    for i in [0:10:2] do
      x := x * x * i.toFloat
      x := x+x
    #v[x + x, x*x, 5*x])

  let stmts ← compileExpr'' e

  let cs ← stmts.mapM (·.toString)

  logInfo m!"{cs}"

attribute [opencl_csimp] pure_bind bind_pure bind_assoc bind_map

open Qq Compiler Lean Meta in
run_meta
  let e := q(Id.run do
    let mut x : Float := oclFunction _ "asdf"
    for i in [0:10:2] do
      x := 10*x
      for j in [5:20] do
        x := x * x * i.toFloat * j.toFloat
        x := x+x
      x := x + 42
    x + x)

  let stmts ← compileExpr'' e

  let cs ← stmts.mapM (·.toString)

  logInfo m!"{cs}"


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
