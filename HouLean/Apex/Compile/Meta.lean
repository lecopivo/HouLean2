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




/-- Takes an array of expressions `es := #[e₁, ..., eₙ]` of the same type and expresses them as
context modifications.

This function analyzes the free variables in the expressions and determines which variables are
mutated (modified) by the computation. It transforms each expression into a function that takes
a context (containing all free variables) and returns a new context with mutated variables updated.

## Returns
- `ctx`: Context type (product of all free variables found in the expressions)
- `ctx'`: Unmutated context type (product of variables that are not modified)
- `split`: Function of type `Context → α × Context'` that splits a context into:
  - The mutated values (as a product matching the expression type)
  - The unmutated context variables
- `es'`: Array of context update functions, each of type `Context → Context`

## How it works
1. Collects all free variables from the input expressions
2. Determines which variables are mutated by matching the expression's result type components
   with the free variable types
3. Transforms each expression `eᵢ` into a lambda `fun ctx => new_ctx` where the result is
   packaged back into a full context with updated mutated variables

## Example
Given expressions:
- `e₁ = (a * b, x + y)`  where `a, b : Int` and `x, y : Float`
- `e₂ = (a + b, x * y)`

The function identifies:
- Context variables: `[a, b, x, y]`
- Mutated variables: `[a, x]` (indices 0 and 2)
- Unmutated variables: `[b, y]`

Returns:
- `ctx = (a, b, x, y)`
- `ctx' = (b, y)`
- `split = fun ctx => ((ctx.1, ctx.2.2.1), (ctx.2.1, ctx.2.2.2))`
  - First component: `(a, x)` - the mutated values
  - Second component: `(b, y)` - the unmutated context
- `es' = [fun ctx => (a*b, b, x+y, y), fun ctx => (a+b, b, x*y, y)]`

This allows tracking which variables are modified vs preserved across computations.
-/
partial def asContextChange' (es : Array Expr) : MetaM (Option (Expr × Expr × Expr × Array Expr)) := do
  if es.size = 0 then
    return none

  -- Collect all free variables from the expressions
  let go := do
    for e in es do
      e.collectFVars
  let (_,s) ← go.run {}
  let fvarIds := s.fvarIds

  let ctxVals := fvarIds.map Expr.fvar

  trace[HouLean.Apex.compiler] m!"Trying to express {es} as context changes"
  trace[HouLean.Apex.compiler] m!"Context variables {ctxVals}"

  -- Build context type from free variables
  let ctx ← mkProdElem ctxVals
  let ctx := if ctxVals.size != 0 then ctx else (.const ``Unit.unit [])
  let ctxType ← inferType ctx

  -- Verify all expressions have the same type
  let type ← inferType es[0]!
  unless ← es.allM (fun e => do isDefEq type (← inferType e)) do
    return none


  -- Get product constructors and projections for value and context types
  let (mk, projs) ← mkProdMkProjs type
  let (ctxMk, ctxProjs) ← mkProdMkProjs ctxType

  let types := (← projs.mapM fun proj => inferType proj) |>.map Expr.getForallBody
  let ctxTypes ← ctxVals.mapM inferType

  -- Find which context variables are mutated by matching value component types
  let mut mutatedIndices : Array Nat := #[]
  for type in types do
    let some j ← findFreeCtxVar 0 type mutatedIndices ctxTypes
      | return none
    mutatedIndices := mutatedIndices.push j


  trace[HouLean.Apex.compiler] m!"Mutating variables {mutatedIndices.map (fun i => ctxVals[i]!)}"

  -- Build ctx' containing only non-mutated variables
  let ctx' ← mkProdElem <|
    (ctxVals.mapIdx fun i var => if mutatedIndices.contains i then none else some var).filterMap id


  -- Transform expressions into context update functions
  let (es', split) ← withLocalDeclD `ctx ctxType fun ctxVar => do
    let ctxVars ← mkProdSplitElem ctxVar (max 1 ctxVals.size)

    let mut es' : Array Expr := #[]
    for e in es do
      trace[HouLean.Apex.compiler] m!"oo {e}"
      -- Replace free variables with context projections
      let e := e.replaceFVars ctxVals ctxVars

      -- Build new context with mutated values
      -- Start with all original context projections
      let bs := ctxProjs.map fun p => p.beta #[ctxVar]

      -- Replace mutated variables with new computed values (from bvar 0, the let-bound result)
      let bs := (projs.zip mutatedIndices).foldl (init := bs) fun bs (p, j) =>
        bs.set! j (p.beta #[.bvar 0])

      let b := ctxMk.beta bs
      let e := Expr.letE `r type e b false

      es' := es'.push (← mkLambdaFVars #[ctxVar] e)

    trace[HouLean.Apex.compiler] m!"aa"

    -- Build split function: mutated values × unmutated context
    let mutatedValues := mk.beta (mutatedIndices.map fun j => ctxProjs[j]!.beta #[ctxVar])
    let unmutatedCtx := ctx'.replaceFVars ctxVals ctxVars

    trace[HouLean.Apex.compiler] m!"bb"

    trace[HouLean.Apex.compiler] m!"mutatedValues {mutatedValues}"
    trace[HouLean.Apex.compiler] m!"unmutatedCtx {unmutatedCtx}"

    let split ← mkLambdaFVars #[ctxVar] (← mkAppM ``Prod.mk #[mutatedValues, unmutatedCtx])
    trace[HouLean.Apex.compiler] m!"cc"
    return (es', split)

  return (ctx, ctx', split, es')

where
  /-- Find the index of the first unused context variable that matches type `t`.

  Searches through context types `ts` starting at index `j`, skipping any indices
  already in `usedIndices`. Returns the first matching index or `none` if no match found.
  -/
  findFreeCtxVar (j : Nat) (t : Expr) (usedIndices : Array Nat) (ts : Array Expr) :
      MetaM (Option Nat) := do
    unless j < ts.size do
      return none

    if usedIndices.contains j then
      return ← findFreeCtxVar (j+1) t usedIndices ts

    if ← isDefEq t ts[j]! then
      return j

    findFreeCtxVar (j+1) t usedIndices ts
