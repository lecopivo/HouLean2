import HouLean.Meta.SpecializeAndSimp.Main
import HouLean.OpenCL.Basic
-- import HouLean.OpenCL.Data.Fin
import HouLean.OpenCL.Data.InlinedLoop
import HouLean.OpenCL.Reference.Operators
import HouLean.Data.Matrix
import HouLean.Data.Vector
-- import HouLean.OpenCL.Data.Matrix

import Qq

open Lean Meta Qq HouLean Meta

namespace Test.OpenCL.SpecAndSimp


open OpenCL.Compiler3 SpecializeAndSimp in
def specializeImplementedBy (e : Expr) : M (Option Expr) := do
  let s := (compilerExt.getState (← getEnv)).implementedBy
  let candidates ← s.getMatch e
  for c in candidates do
    let (xs,_,_) ← forallMetaTelescope (← inferType c.lhs)

    -- is 'implemented_by' applicable?
    -- we have to go over all arguments and check if they really apply/classes have instances
    let b := c.lhs.beta xs
    if ← isDefEq e b then
      let mut skip := false
      for x in xs do
        let x ← instantiateMVars x
        let t ← inferType x
        if (← isClass? t).isSome && x.isMVar then
          if let some inst ← synthInstance? t then
            x.mvarId!.assign inst
          else
            skip := true
            break
      if skip then
        continue

      let xs' ← xs.mapM specializeExpr
      let e' := c.lhs.beta xs'
      -- logInfo m!"{xs} ==> {xs'}"
      -- logInfo m!"custom spec{indentExpr e}==>{indentExpr e'}"
      return e'
  return none


def foo {α} [Add α] [Mul α] (x : α) :=
  let a := x*x
  let b := x+x
  a + b + x

-- set_option trace.Meta.Tactic.simp true

/-- Gadget identity map that will unroll loops at compile time. -/
def unroll {α : Sort u} (a : α) : α := a

def evalNat' (e : Expr) : MetaM (Option Nat) := do
  try
    let n ← unsafe evalExpr' Nat ``Nat e
    return some n
  catch _ =>
    return none

simproc_decl list_ofFn_unroll (unroll (List.ofFn _)) := fun e => do
  let e ← instantiateMVars e.appArg!
  if let some (t, n, f) := e.app3? ``List.ofFn then
    if let some n ← evalNat' n then
      let mut e ← mkAppOptM ``List.nil #[t]
      for h : i in [0:n] do
        have ⟨_,_⟩ := h
        let i : Fin n := ⟨n-i-1, by grind⟩
        let i := toExpr i
        e ← mkAppM ``List.cons #[f.app i, e]
      return .visit { expr := e}
  return .continue


@[opencl_csimp]
theorem vector_mk_ofFn {α : Type} {n : Nat} (f : Fin n → α) (h) :
  Vector.mk (n:=n) (List.ofFn f).toArray h
  =
  Vector.mk (n:=n) (unroll (List.ofFn f)).toArray h  := sorry

attribute [opencl_csimp] Nat.add_zero Nat.zero_add list_ofFn_unroll

#check (Vector.mk (n:=5) (List.ofFn fun i : Fin 5 => 3*i.1).toArray rfl)
  rewrite_by
    simp only [opencl_csimp]

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
  let e' ← specializeAndSimp e {} { zeta := false, zetaDelta := false }
    #[`opencl_csimp] specializeImplementedBy

  logInfo e'

impl_by (x y z : Float) : #v[x,y,z] ==> (float3){x,y,z}

set_option pp.funBinderTypes true in
-- todo: turn this into a `impl_by` rule that creates the rhs
impl_by {α : Type} {n : Nat} (u : Vector α n) (i : Nat) (h) : getElem u i h ==> getElem(u,i)
impl_by {α : Type} {n : Nat} (u : Vector α n) (i : Fin n) (h) : getElem u i (h) ==> getElem(u,i)

@[opencl_csimp]
theorem vector_map (v : Vector α n) (f : α → β) : v.map f = .ofFn (fun i => f v[i]) := sorry_proof

@[opencl_csimp]
theorem vector_mapFinIdx (v : Vector α n) (f : (i : Nat) → α → (h : i < n) → β) :
  v.mapFinIdx f = .ofFn (fun i => f i.1 v[i.1] i.2) := sorry_proof

@[opencl_csimp]
theorem inline_foldl (f : α → Fin n → α) (init : α) :
  Fin.foldl n f init
  =
  OpenCL.inlinedLoop n (fun i s => f s i) init := sorry_proof


-- variable (x : Vector Float 3)
-- #check (Vector.map Math.sin x) rewrite_by
macro_rules | `(tactic| get_elem_tactic_extensible) => `(tactic| sorry_proof)


-- set_option trace.HouLean.specialize.detail true
run_meta
  let e :=
    q(fun x y z : Float =>
        let a := foo x
        let b := y
        let c := foo (z + foo x)
        #v[a, b, c])

  lambdaTelescope e fun _ e => do
  let e' ← specializeAndSimp e {} { zeta := false } #[`opencl_csimp] specializeImplementedBy

  logInfo e'


set_option trace.HouLean.specialize true
run_meta
  let e :=
    q(fun x : Vector Float 3 => x * x)

  lambdaTelescope e fun _ e => do
  let e' ← specializeAndSimp e {} { zeta := false } #[`opencl_csimp] specializeImplementedBy
  logInfo e'

set_option trace.HouLean.specialize true
run_meta
  let e :=
    q(fun x : Vector Float 3 => x[0])

  lambdaTelescope e fun _ e => do
  let e' ← specializeAndSimp e {} { zeta := false } #[`opencl_csimp] specializeImplementedBy
  logInfo e'

attribute [opencl_csimp] sum Fin.getElem_fin

@[opencl_csimp]
theorem Float.zero_add (x : Float) : 0 + x = x := sorry_proof

@[opencl_csimp]
theorem Float.add_zero (x : Float) : x + 0 = x := sorry_proof


set_option trace.HouLean.specialize true
set_option pp.proofs false
set_option trace.Meta.Tactic.simp.rewrite true in
run_meta
  let e :=
    q(fun x : Vector Float 3 => x.dot x)

  lambdaTelescope e fun _ e => do
  let e' ← specializeAndSimp e {} { zeta := false } #[`opencl_csimp] specializeImplementedBy
  logInfo e'


#check isRecursiveDefinition
set_option trace.HouLean.specialize true
run_meta
  let e :=
    q(fun x : Matrix Float 3 3 => x + x)

  lambdaTelescope e fun _ e => do
  let e' ← specializeAndSimp e {} { zeta := false } #[`opencl_csimp] specializeImplementedBy

  logInfo e'
