import HouLean.Meta.SpecializeAndSimp2.Main

namespace HouLean.Meta.Sas

open Lean Meta Elab Term Command

elab "#sas " e:term : command =>
  withoutModifyingEnv do
  runTermElabM fun _ctx => do
    let e ← elabTermAndSynthesize e none
    let e ← sas e #[`simp]
    logInfo e
    pure ()
