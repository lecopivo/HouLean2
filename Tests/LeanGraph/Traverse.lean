import HouLean.LeanGraph.Traverse
import HouLean.LeanGraph.Init

open Lean HouLean LeanGraph Meta

-- set_option trace.HouLean.lean_graph true 
instance : Zero Vector3 := ⟨⟨0,0,0⟩⟩

run_elab
  let s ← IO.FS.withFile "Tests/LeanGraph/graph.json" .read fun file => do
    file.readToEnd

  let .ok json := Json.parse s
    | throwError "failed parsing json!"
  let .ok graph := fromJson? (α:=LeanGraph) json 
    | throwError "failed loading graph!"

  let s ← traverseGraph' graph
  -- logInfo m!"{s.code}"
  
  withLCtx s.lctx s.insts do
    let mut nodes := graph.nodes
    for node in nodes, i in [0:nodes.size] do
      let some var := s.nodeNameToVar[node.name]?
        | logInfo m!"missing info on {node.name}"
          continue
      let t ← mkNodeType (← instantiateMVars (← var.fvarId!.getValue?).get!)
      nodes := nodes.set! i {node with type := t}
      -- logInfo m!"node {node.name}, value {← var.fvarId!.getValue?}\n\n\
      --            old type:\n{node.type}\n\n\"
      --            new type:\n{t}"
    let graph := {graph with nodes := nodes}

    IO.println (toJson graph)
    

#exit



#check
  let Arith_zero_1 := Zero.zero
  let Arith_zero := Zero.zero
  let Arith_one := One.one
  let HouLean_Vector3_add := HouLean.Vector3.add ⟨Arith_one, Arith_one, Arith_one⟩ ⟨Arith_zero, Arith_one, Arith_zero⟩
  let HouLean_Vector3_fromGeodetic := HouLean.Vector3.fromGeodetic ⟨Arith_one, Arith_zero, Arith_one⟩
  let Arith_add := HAdd.hAdd HouLean_Vector3_add HouLean_Vector3_fromGeodetic
  let HouLean_Vector3_add_1 := HouLean.Vector3.add Arith_add Arith_zero_1
  let Arith_add_1 := HAdd.hAdd HouLean_Vector3_add_1.x HouLean_Vector3_add_1.y
  Arith_add_1
