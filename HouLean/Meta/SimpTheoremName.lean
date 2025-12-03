import Lean
import HouLean.Init
import Qq

open Lean Meta Elab

namespace HouLean.Meta

open Qq in
private def evalExpr? (e : Expr) : MetaM (Option String) := do
  let t ← inferType e
  try
    if (← isDefEq t q(Nat)) then
      let n ← unsafe evalExpr Nat q(Nat) e
      return some (toString n)

    if (← isDefEq t q(Int)) then
      let n ← unsafe evalExpr Int q(Int) e
      return some (toString n)

    if (← isDefEq t q(Float)) then
      let n ← unsafe evalExpr Float q(Float) e
      return some ((toString n).dropRightWhile (· == '0') |>.dropRightWhile (· == '.') |>.replace "." "_")

    if (← isDefEq t q(String)) then
      let n ← unsafe evalExpr String q(String) e
      return some n

    return none
  catch _ =>
    return none

private def nameToString (n : Name) : String := Id.run do
  let parts := n.components
  if parts.length == 2 &&
     parts[0]!.toString.toLower == parts[1]!.toString.toLower then
     return parts[1]!.toString

  return n.eraseMacroScopes.toString |>.replace "." "_"

/-- Converts an expression to a string that roughly represent that expression. Used for
automatically generating theorem names. -/
partial def exprToString (e : Expr) : MetaM String := do

  let t ← inferType e

  if e.isFVar && !t.isSort then
    return s!"{← ppExpr e}"

  unless e.getAppFn'.isConst do
    return ""


  -- ignore instances
  if (← isClass? t).isSome then
    return ""

  if let some val ← evalExpr? e then
    return val

  -- -- ignore type parameters
  if t.isSort then
    return ""

  let (fn, args) := e.getAppFnArgs

  let fnName := nameToString fn

  let argNames ←  args.mapM exprToString
  let argNames := argNames.filter (· != "")

  if argNames.size == 0 then
    return fnName
  else
    return s!"{fnName}_{argNames.joinl (map:=fun x => x) (·++"_"++·)}"


/-- Generate simplifier theorem name from an expression of form A₁ → ... → Aₙ → lhs = rhs -/
def generateSimpTheoremName (e : Expr) : MetaM String := do
  forallTelescope e fun _hyps body => do
    let some (_, lhs, _) := body.eq?
      | throwError m!"{decl_name%} expectes equality theorem!"
    return ← exprToString lhs
