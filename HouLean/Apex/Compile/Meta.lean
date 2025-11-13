import Lean
import Qq
import HouLean.Init
import HouLean.Meta.Basic

namespace Lean.Meta

open HouLean Meta

/-- Take an expression `e` and abstract all free variables, turning `e` into a closed term.

Returns a function that accepts all the removed free variables and a product of all the free variables.

For example calling it on 
`(fun a b c => a + b + c + x + y + z)`
will product
`((fun ctx a b c => a + b + c + ctx.1 + ctx.2.1 + ctx.2.2),
  (x, y, z))` -/
def abstractAllFVarsMany (es : Array Expr) : MetaM (Expr × Array Expr) := do

  let go := do
    for e in es do
      e.collectFVars
  let (_,s) ← go.run {}

  let ids := s.fvarIds
  let ctxVals := ids.map Expr.fvar
  let ctx ← mkProdElem ctxVals
  let ctx := if ctxVals.size != 0 then ctx else (.const ``Unit.unit [])
  let ctxType ← inferType ctx

  let es' ← withLocalDeclD `ctx ctxType fun ctxVar => do
    let ctxVars ← mkProdSplitElem ctxVar (max 1 ctxVals.size)
    let es := es.map (fun e => e.replaceFVars ctxVals ctxVars)
    let es ← es.mapM (mkLambdaFVars #[ctxVar])
    return es

  return (ctx, es')


@[inherit_doc abstractAllFVarsMany]
def abstractAllFVars (e : Expr) : MetaM (Expr × Expr) := do
  let (ctx, e) ← abstractAllFVarsMany #[e]
  return (ctx, e[0]!)


def asContextChange (es : Array Expr) : MetaM (Option (Expr × Expr × Array Expr)) := do

  if es.size = 0 then
    return none

  let go := do
    for e in es do
      e.collectFVars
  let (_,s) ← go.run {}

  let ids := s.fvarIds
  let ctxVals := ids.map Expr.fvar
  let ctx ← mkProdElem ctxVals
  let ctx := if ctxVals.size != 0 then ctx else (.const ``Unit.unit [])
  let ctxType ← inferType ctx

  let type ← inferType es[0]!

  -- check all types are the same
  unless ← es.allM (fun e => do isDefEq type (← inferType e)) do
    return none

  -- find context value we can use to pass on
  -- todo: some smarter heuristic on which variable should be devaured would be nice
  --       nothing will be perfect, this should be handled by reference counting anyway
  let some i ← ctxVals.findIdxM? (fun val => do isDefEq type (← inferType val))
    | return none

  let (es',proj) ← withLocalDeclD `ctx ctxType fun ctxVar => do
    let ctxVars ← mkProdSplitElem ctxVar (max 1 ctxVals.size)
    let es := es.map (fun e => e.replaceFVars ctxVals ctxVars)
    -- let ctxVars := ctxVars.set! i 
    let proj ← mkLambdaFVars #[ctxVar] (ctxVars[i]!)
    let es ← es.mapM (fun e => do
      mkLambdaFVars #[ctxVar] (← mkProdElem (ctxVars.set! i e)))
    return (es,proj)

  return (ctx, proj, es')

