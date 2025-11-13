import HouLean.LeanGraph.Scopes
import HouLean.LeanGraph.Init

open Lean HouLean LeanGraph Meta Traverse Std


/--
info: Scope Hierachy:
  root: scope(output)

  ground[]  ⊆  none
     [ground, scope(output_1), scope(output), scope(output_2), scope(output_3)]
  scope(output_1)[index, zgeometry]  ⊆  (some scope(output))
     [scope(output_1), scope(output_2), scope(output_3)]
  scope(output)[input]  ⊆  (some ground)
     [scope(output_1), scope(output), scope(output_2), scope(output_3)]
  scope(output_2)[input_1]  ⊆  (some scope(output_1))
     [scope(output_2)]
  scope(output_3)[input_2]  ⊆  (some scope(output_1))
     [scope(output_3)]
---
info: [(Arith_sub, scope(output_1)),
 (eval_1, scope(output_1)),
 (Geometry_numPoints, scope(output)),
 (Geometry_setPointAttrib, scope(output_1)),
 (input_1, scope(output_2)),
 (input_2, scope(output_3)),
 (output_3, scope(output_1)),
 (Arith_div, scope(output_1)),
 (output, ground),
 (Arith_add, scope(output_3)),
 (Geometry_boundingBox, scope(output)),
 (eval, scope(output_1)),
 (forLoop, scope(output)),
 (Vector3_add, scope(output_2)),
 (Geometry_pointAttrib, scope(output_1)),
 (index, scope(output_1)),
 (output_1, scope(output)),
 (input, scope(output)),
 (output_2, scope(output_1)),
 (zgeometry, scope(output_1))]
-/
#guard_msgs in
run_elab
  let s ← IO.FS.withFile "Tests/LeanGraph/scope_test.json" .read fun file => do
    file.readToEnd

  let .ok json := Json.parse s
    | throwError "failed parsing json!"
  let .ok graph := fromJson? (α:=LeanGraph) json 
    | throwError "failed loading graph!"

  let ctx ← buildContext graph

  let subgraph ← extractInputOutputSubgraph graph ctx
  let idom := subgraph.computePostDominators

  let scopeHierachy ← buildScopeHierarchy graph idom ctx
  logInfo m!"{scopeHierachy}"

  let scopes ← assignNodeScopes graph ctx scopeHierachy
  logInfo m!"{scopes.toList}"


