import HouLean.OpenCL.Compiler.RewriteRules
import HouLean.Meta.SpecializeAndSimp.Main

namespace HouLean.OpenCL.Compiler3

open Lean Meta

open OpenCL.Compiler3 SpecializeAndSimp in
def specializeImplementedBy (e : Expr) : M (Option Expr) := do
  let s := (compilerExt.getState (← getEnv)).implementedBy
  let candidates := (← s.getMatch e).map (·.lhs)
  let s := (compilerExt.getState (← getEnv)).implementedByBuilders
  let candidates := candidates ++ (← s.getMatch e).map (·.lhs)
  for c in candidates do
    let (xs,_,_) ← forallMetaTelescope (← inferType c)

    -- is 'implemented_by' applicable?
    -- we have to go over all arguments and check if they really apply/classes have instances
    let b := c.beta xs
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
      let e' := c.beta xs'
      return e'
  return none


/-- Gadget identity map that will unroll loops at compile time. -/
def unroll {α : Sort u} (a : α) : α := a

simproc_decl list_ofFn_unroll (unroll (List.ofFn _)) := fun e => do
  let e ← instantiateMVars e.appArg!
  if let some (t, n, f) := e.app3? ``List.ofFn then
    if let some n ← runInterpreter? Nat n then
      let mut e ← mkAppOptM ``List.nil #[t]
      for h : i in [0:n] do
        have ⟨_,_⟩ := h
        let i : Fin n := ⟨n-i-1, by grind⟩
        let i := toExpr i
        e ← mkAppM ``List.cons #[f.app i, e]
      return .visit { expr := e}
    else
      throwError m!"Expected `{n}` to be value known at compile time!"
  return .continue


attribute [opencl_csimp] list_ofFn_unroll



open Elab Command Term SpecializeAndSimp in
/-- OpenCL 'Specialize and Simplify' form of an expression.

The is a preview of the first stage of the OpenCL compiler which specializes functions and runs
simplifier. -/
elab c:"#opencl_sas " e:term : command => do
  withoutModifyingEnv do
  runTermElabM fun _ => do

    let e ← elabTermAndSynthesize e none
    let (e', s) ← (SpecializeAndSimp.specializeAndSimp e).runInMeta
      {} { zeta := false } #[`opencl_csimp] specializeImplementedBy


    let mut m : MessageData := ""
    for s in s.specOrder do
      let info ← getConstInfo s
      m := m ++ m!"{s}:\n{info.value!}\n\n"
    m := m ++ m!"Resulting specialization:{indentExpr e'}"

    logInfoAt c m
