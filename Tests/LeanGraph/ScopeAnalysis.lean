import HouLean.LeanGraph.Scopes
import HouLean.LeanGraph.Linearization
import HouLean.LeanGraph.Init

open Lean HouLean LeanGraph Meta Traverse Std

-- TODO: the node `eval` and `output_2` has incorrect scope. It should be in `output` scope 
--       probably the scope of an output node should not be 
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
 (Geometry_setPointAttrib, scope(output_1)),
 (Geometry_numPoints, scope(output)),
 (input_1, scope(output_2)),
 (input_2, scope(output_3)),
 (output_3, scope(output_1)),
 (Arith_div, scope(output_1)),
 (output, ground),
 (Arith_add, scope(output_3)),
 (Geometry_boundingBox, scope(output)),
 (eval, scope(output)),
 (forLoop, scope(output)),
 (Vector3_add, scope(output_2)),
 (Geometry_pointAttrib, scope(output_1)),
 (index, scope(output_1)),
 (output_1, scope(output)),
 (output_2, ground),
 (input, scope(output)),
 (zgeometry, scope(output_1))]
---
info: Direct child of scope(output) startring from scope(output_3) is some (scope(output_1))
---
info: Subgraph of ground: [(output_2, [output])]
---
info: Subgraph of scope(output_1): [(Arith_sub, [Arith_div]),
 (eval_1, [Geometry_setPointAttrib]),
 (Geometry_setPointAttrib, [output_1]),
 (Geometry_pointAttrib, [Arith_sub]),
 (index, [eval_1, Geometry_pointAttrib, output_3]),
 (output_3, [eval_1]),
 (Arith_div, [Geometry_setPointAttrib]),
 (zgeometry, [Geometry_setPointAttrib, Geometry_pointAttrib])]
---
info: Subgraph of scope(output): [(Geometry_boundingBox, [eval, output_1]),
 (eval, [output_1]),
 (forLoop, [output]),
 (Geometry_numPoints, [forLoop]),
 (output_1, [forLoop]),
 (input, [Geometry_boundingBox, forLoop, Geometry_numPoints])]
---
info: Subgraph of scope(output_2): [(Vector3_add, [output_2]), (input_1, [Vector3_add])]
---
info: Subgraph of scope(output_3): [(input_2, [Arith_add]), (Arith_add, [output_3])]
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

  let srcScope := Scope.node "output"
  let trgScope := Scope.node "output_3"
  logInfo m!"Direct child of {srcScope} startring from {trgScope} is {scopeHierachy.directChild srcScope trgScope}"

  let scopeSubgraphs ← extractScopeSubgraph graph scopes scopeHierachy
  for (scope,graph) in scopeSubgraphs do
    logInfo m!"Subgraph of {scope}: {graph.toList.map (fun (a,b) => (a,b.toList))}"
    
    let order := DiGraph.kahnSort ⟨graph⟩
    logInfo m!"Order: {order}"
  
