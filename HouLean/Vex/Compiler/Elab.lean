import HouLean.Vex.Compiler.RunVcc

open Lean

namespace HouLean.Vex.Compiler

elab c:"cvex%" "{" code:cvexProgram "}" : term => do
  let s := code.raw.prettyPrint
  let e := Lean.mkStrLit (toString s)
  let errors ← compileCVex ⟨code.raw⟩
  for err in errors do
    match err.severity with
    | .error | .unknown =>
      logErrorAt c err.msg
    | .warning =>
      logWarningAt c err.msg
  pure e

elab c:"vex%" "{" code:vexSnippet "}" : term => do
  let s := toString code.raw.prettyPrint
  let e := Lean.mkStrLit s
  let errors ← compileVexSnippet ⟨code.raw⟩
  for err in errors do
    match err.severity with
    | .error | .unknown =>
      logErrorAt c err.msg
    | .warning =>
      logWarningAt c err.msg
  pure e
