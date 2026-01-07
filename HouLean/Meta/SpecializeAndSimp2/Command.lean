import HouLean.Meta.SpecializeAndSimp2.Main

namespace HouLean.Meta.Sas

open Lean Meta Elab Term Command

elab "#sas " e:term : command =>
  withoutModifyingEnv do
  runTermElabM fun _ctx => do
    let e ← elabTermAndSynthesize e none
    let (e,specs) ← sas e #[`simp]

    let mut m : MessageData := m!""
    for s in specs.reverse do
      let info ← getConstInfo s.implName
      m := m ++ m!"def {s.req.specName} := {indentExpr info.value!}\n\n"
    m := m ++ m!"{e}"
    logInfo m
