import HouLean.LeanGraph.Traverse

open Lean HouLean LeanGraph

run_elab 
  let s ← IO.FS.withFile "Tests/LeanGraph/graph.json" .read fun file => do
    file.readToEnd

  let .ok json := Json.parse s
    | throwError "failed parsing json!"
  let .ok graph := fromJson? (α:=LeanGraph) json 
    | throwError "failed loading graph!"

  traverseGraph' graph

open Lean

