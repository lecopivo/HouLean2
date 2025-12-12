import HouLean.Meta.SpecializeAndSimp.Main
import HouLean.OpenCL.Basic
import HouLean.OpenCL.Reference.Operators
import HouLean.Data.Matrix
import HouLean.Data.Vector
-- import HouLean.OpenCL.Data.Matrix

import Qq

open Lean Meta Qq HouLean Meta

namespace Test.OpenCL.SpecAndSimp

open OpenCL.Compiler3
def skipImplementedBy (e : Expr) : MetaM Bool := do
  let s := (compilerExt.getState (← getEnv)).implementedBy
  let candidates ← s.getMatch e
  for c in candidates do
    let (xs,_,_) ← forallMetaTelescope (← inferType c.lhs)
    let b := c.lhs.beta xs
    if ← isDefEq e b then
      let mut skip := false
      for x in xs do
        let t ← inferType x
        if (← isClass? t).isSome then
          if (← synthInstance? t).isNone then
            skip := true
      if skip then
        continue
      -- logInfo m!"skipping specialization of {e}"
      return true
  return false


def foo {α} [Add α] [Mul α] (x : α) :=
  let a := x*x
  let b := x+x
  a + b + x

-- set_option trace.Meta.Tactic.simp true

#eval foo 10

attribute [opencl_csimp] Nat.add_zero Nat.zero_add

run_meta
  let e :=
    q(fun x y : Nat =>
      let a :=
        let a1 := x + foo y
        let a2 := 0 + foo (a1 + x)
        a1 * a2
      let b :=
        let b1 := y + 3
        let b2 := 0 + foo a + 0 + foo 10
        b1 * a * b2
      a + b)

  lambdaTelescope e fun _ e => do
  let e' ← specializeAndSimp e {} { zeta := false } #[`opencl_csimp] skipImplementedBy

  logInfo e'


impl_by (x y z : Float) : #v[x,y,z] ==> (float3){x,y,z}

impl_by {α : Type} (u : Vector α 3) (h : 0 < 3) : getElem u 0 h ==> u.x
impl_by {α : Type} (u : Vector α 3) (h : 1 < 3) : u[1]'h ==> u.y
impl_by {α : Type} (u : Vector α 3) (h : 2 < 3) : u[2]'h ==> u.z

@[opencl_csimp]
theorem vector_map (v : Vector α n) (f : α → β) : v.map f = .ofFn (fun i => f v[i]) := sorry_proof

@[opencl_csimp]
theorem vector_ofFn (f : Fin 3 → α) : Vector.ofFn f = #v[f 0, f 1, f 2] := sorry_proof


-- variable (x : Vector Float 3)
-- #check (Vector.map Math.sin x) rewrite_by
macro_rules | `(tactic| get_elem_tactic_extensible) => `(tactic| sorry_proof)


-- set_option trace.HouLean.specialize.detail true
-- run_meta
--   let e :=
--     q(fun x y z : Float => #v[x,y,z])

--   lambdaTelescope e fun _ e => do
--   let e' ← specializeAndSimp e {} { zeta := false } #[`opencl_csimp] skipImplementedBy

--   logInfo e'
