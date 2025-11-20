import HouLean.LeanGraph.GraphToCode

open Lean HouLean LeanGraph Meta Traverse Std


set_option trace.HouLean.LeanGraph.typecheck true



-- #guard_msgs in
run_elab
  let s ← IO.FS.withFile "Tests/LeanGraph/graph_to_code_test3.json" .read fun file => do
    file.readToEnd

  let .ok json := Json.parse s
    | throwError "failed parsing json!"
  let .ok graph := fromJson? (α:=LeanGraph) json
    | throwError "failed loading graph!"

  let ctx ← buildContext graph
  let (e,_) ← graphToCode graph {nodeMap := ctx.nodeMap, inputConnections := ctx.inputConnections} {}

  logInfo e
