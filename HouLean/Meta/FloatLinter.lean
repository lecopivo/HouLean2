import Lean

open Lean Elab Command Meta

namespace FloatLinter

partial def containsFloat (e : Expr) : Bool :=
  e.isConstOf ``Float ||
  match e with
  | .forallE _ d b _    => containsFloat d || containsFloat b
  | .lam _ d b _        => containsFloat d || containsFloat b
  | .app f a            => containsFloat f || containsFloat a
  | .mdata _ e          => containsFloat e
  | .letE _ t v b _     => containsFloat t || containsFloat v || containsFloat b
  | .proj _ _ e         => containsFloat e
  | _                   => false

register_option linter.floatUsage : Bool := {
  defValue := true
  descr := "Warn when using `Float` instead of explicit `Float32` or `Float64`"
}

def getDefDeclName? (stx : Syntax) : Option Name :=
  if stx.isOfKind ``Lean.Parser.Command.declaration then
    let inner := stx[1]
    let kind := inner.getKind
    if kind == `Lean.Parser.Command.definition ||
       kind == `Lean.Parser.Command.theorem ||
       kind == `Lean.Parser.Command.abbrev then
      let declId := inner[1]
      some declId[0].getId
    else
      none
  else
    none

def floatUsageLinter : Linter where
  run := fun stx => do
    unless linter.floatUsage.get (← getOptions) do return
    let some baseName := getDefDeclName? stx | return
    let ns ← getCurrNamespace
    let declName := ns ++ baseName
    let env ← getEnv
    let some info := env.find? declName | return

    if containsFloat info.type then
      Linter.logLint linter.floatUsage stx
        m!"Use of `Float` detected in type signature. Consider `Float32` or `Float64` for explicit precision."

    if let some val := info.value? then
      if containsFloat val then
        Linter.logLint linter.floatUsage stx
          m!"Use of `Float` detected in definition body. Consider `Float32` or `Float64`."

initialize addLinter floatUsageLinter

end FloatLinter
