import HouLean.LeanGraph.GraphToCode

open Lean HouLean LeanGraph Meta Traverse Std

-- #guard_msgs in
run_elab
  let s ← IO.FS.withFile "Tests/LeanGraph/scope_test.json" .read fun file => do
    file.readToEnd

  let .ok json := Json.parse s
    | throwError "failed parsing json!"
  let .ok graph := fromJson? (α:=LeanGraph) json
    | throwError "failed loading graph!"

  graphToCode graph
