import HouLean.LeanGraph.Scopes
import HouLean.LeanGraph.Linearization

open Lean Meta Elab Term Std

namespace HouLean
namespace LeanGraph

open Traverse

partial def graphToCode (graph : LeanGraph) : TermElabM Unit := do
  let ctx ← buildContext graph

  let subgraph ← extractInputOutputSubgraph graph ctx
  let idom := subgraph.computePostDominators

  let scopeHierachy ← buildScopeHierarchy graph idom ctx
  -- logInfo m!"{scopeHierachy}"

  let scopes ← assignNodeScopes graph ctx scopeHierachy
  -- logInfo m!"{scopes.toList}"

  let scopeSubgraphs ← extractScopeSubgraph graph scopes scopeHierachy

  let mut subgraphOrders : HashMap Scope (List String) := {}

  -- sort all subgraph
  for (scope,graph) in scopeSubgraphs do
    let some order := DiGraph.kahnSort ⟨graph⟩
      | throwError m!"Failed to sort subgraph of {scope}"
    subgraphOrders := subgraphOrders.insert scope order

  let some groundOrder := subgraphOrders[Scope.ground]?
    | throwError "Bug in {decl_name%}, no order for ground scope"

  go 0 Scope.ground groundOrder subgraphOrders

where
  go (depth : Nat) (scope : Scope) (order : List String) (orders : HashMap Scope (List String)) : TermElabM Unit := do
    if depth > 10 then
      throwError "asdf"

    for node in order do

      let indent := String.mk (List.replicate (2*depth) ' ')
      IO.println s!"{indent}{node}"
      if scope == .node node then
        IO.println s!"{indent}return {Scope.node node}"
      else
        if let some subscope := orders[Scope.node node]? then
          go (depth+1) (.node node) subscope orders
