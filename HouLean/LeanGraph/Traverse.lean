import HouLean.LeanGraph.LeanGraph
import HouLean.LeanGraph.Extension

open Lean Meta

namespace HouLean

namespace LeanGraph.Traverse

structure State where
  unvisitedNodes : List String := []
  nodesToVisit : List String := []
  visitedNodes : Std.HashMap String FVarId := {} -- node name to fvar
  currentTopNode : String -- we keep track of the current top node to detect loops
  code : String := ""
  graph : LeanGraph -- graph to update types and implicit wires on
  lctx : LocalContext
  insts : LocalInstances
  vars : Array Expr
  nodeNameToVar : Std.HashMap String Expr
deriving Inhabited

structure Context where
  nodeMap : Std.HashMap String Node
  portTypes : Std.HashMap String PortType
  inputConnections : Std.HashMap String (Array Connection)
  outputConnections : Std.HashMap String (Array Connection)

open Lean Elab Term Meta
abbrev TraverseM := ReaderT Context <| StateT State <| TermElabM 

def addVariable (name : Name) (type val : Expr) (nodeName : String) : TraverseM Unit := do
  withLCtx (← get).lctx (← get).insts do
    withLetDecl name type val fun var => do
      let lctx ← getLCtx
      let insts ← getLocalInstances
      modify (fun s => {s with 
        lctx, insts, 
        vars := s.vars.push var
        nodeNameToVar := s.nodeNameToVar.insert nodeName var})


def withContext (go : TraverseM α) : TraverseM α := do
  withLCtx (← get).lctx (← get).insts go

partial def getNodeToVisit : TraverseM (Option Node) := do
  let s ← get
  
  match s.nodesToVisit with
  | [] => return none
  | n :: ns => 
    set {s with nodesToVisit := ns}
    if s.visitedNodes.contains n then
      getNodeToVisit
    else
      let ctx ← read
      let some node := ctx.nodeMap[n]?
        | throwError "getNodeToVisit: invalid node name. This is a bug!"
      return node

def getInputConnections (nodeName : String) : TraverseM (Array Connection) := do
  if let some wires := (← read).inputConnections[nodeName]? then
    return wires
  else
    return #[]

def pushNode (nodeName : String) : TraverseM Unit := do
  if (← get).visitedNodes.contains nodeName then
    pure ()
  else
    modify (fun s => {s with nodesToVisit := nodeName :: s.nodesToVisit})

def addLine (code : String) : TraverseM Unit := do
  modify (fun s => {s with code := s.code ++ code ++ "\n"})

partial def getSubports (port : PortType) : TraverseM (Array PortType) := 
  match port with
  | .struct _ _ subports => return subports
  | .builtin _ typeName => do
    let ctx ← read
    if let some port' := ctx.portTypes[typeName]? then
      getSubports port'
    else
      return #[]

partial def buildOutputProj (node : Node) (index : List Nat) : TraverseM String := do
  match index with
  | [] => return "invalid_index"
  | i :: is =>
    let some port := node.type.outputs[i]?
      | return "proj_out_of_bounds"
    go port node.name is
where
  go (port : PortType) (s : String) (index : List Nat) : TraverseM String := do
    match index with
    | [] => return s
    | i :: is =>
      let subports ← getSubports port
      let some subport := subports[i]?
        | return "proj_out_of_bounds"
      go subport (s ++ "." ++ subport.name) is

partial def buildOutputProj' (node : Node) (index : List Nat) : TraverseM Term := do
  match index with
  | [] => throwError m!"invalud projection {index} of {node.type}"
  | i :: is =>
    let some port := node.type.outputs[i]?
      | throwError m!"projection out of bounds, {index} in {node.type}"
    let nodeId := mkIdent (Name.mkSimple node.name)
    go port nodeId is
where
  go (port : PortType) (s : Term) (index : List Nat) : TraverseM Term := do
    match index with
    | [] => return s
    | i :: is =>
      let subports ← getSubports port
      let some subport := subports[i]?
        | throwError m!"projection out of bounds, {index} in {port}"
      let subportId := mkIdent (Name.mkSimple subport.name)
      go subport (← `($s.$subportId)) is


partial def buildArg (drop : Nat) (port : PortType) (wires : Array Connection) : TraverseM String := do
  if wires.size = 0 then
    return "?_"
  else 
    -- only one connection and at the right level!
    if wires.size = 1 then
      let w := wires[0]!
      if w.inputIndex.length = drop then
        let proj ← buildOutputProj (← read).nodeMap[w.outputNodeName]! w.outputIndex
        return proj
    
    let mut args : Array String := #[]
    let subports ← getSubports port
    for subport in subports, i in [0:subports.size] do
      let wires' := wires.filter (fun w => (w.inputIndex.drop drop).head? == some i)
      args := args.push (← buildArg (drop+1) subport wires')
    return "⟨" ++ args.joinl (map := id) (· ++ ", " ++ ·) ++ "⟩"


partial def buildArg' (drop : Nat) (port : PortType) (wires : Array Connection) : TraverseM Term := do
  if wires.size = 0 then
    return ← `(?_)
  else 
    -- only one connection and at the right level!
    if wires.size = 1 then
      let w := wires[0]!
      if w.inputIndex.length = drop then
        let proj ← buildOutputProj' (← read).nodeMap[w.outputNodeName]! w.outputIndex
        return proj
    
    let mut args : Array Term := #[]
    let subports ← getSubports port
    for subport in subports, i in [0:subports.size] do
      let wires' := wires.filter (fun w => (w.inputIndex.drop drop).head? == some i)
      args := args.push (← buildArg' (drop+1) subport wires')
    return ← `(⟨$args,*⟩)
  

partial def traverseGraphCore (node : Node) : TraverseM Unit := do
  Meta.withIncRecDepth do
  let s ← get
  if s.visitedNodes.contains node.name then
    return ()

  let ctx ← read
  let wires ← getInputConnections node.name
  let wires := wires.qsort (fun w w' => w.inputIndex < w'.inputIndex)
  -- todo: somehow sort int inputs
  for wire in wires do
    let some outputNode := ctx.nodeMap[wire.outputNodeName]?
      | throwError "invalid node!"
    traverseGraphCore outputNode

  let wires := wires.filter (fun w => ¬w.isImplicit)
  -- build application
  let mut inputs : Array (String) := #[]
  for inputPort in node.type.inputs, i in [0:node.type.inputs.size], val? in node.portValues do
    let wires' := wires.filter (fun w => w.inputIndex.head? == some i)

    -- no connected wire but we have a value
    if wires'.size = 0 then
      if let some val := val? then
        if val != "" then 
          inputs := inputs.push s!"{val}"
          continue

    inputs := inputs.push (← buildArg 1 inputPort wires')

  addLine s!"let {node.name} := {node.type.leanConstant} {inputs.joinl (map:= ("("++·++")")) (· ++ " " ++ ·)}"

  modify (fun s => {s with visitedNodes := s.visitedNodes.insert node.name default})

partial def traverseGraphCore' (node : Node) : TraverseM Unit := do
  Meta.withIncRecDepth do
  let s ← get
  if s.visitedNodes.contains node.name then
    return ()

  trace[HouLean.lean_graph] m!"visiting node: {node.name}"

  let ctx ← read
  let wires ← getInputConnections node.name
  let wires := wires.qsort (fun w w' => w.inputIndex < w'.inputIndex)

  trace[HouLean.lean_graph] m!"number of inpute wires: {wires.size}"
  -- todo: somehow sort int inputs
  for wire in wires do
    let some outputNode := ctx.nodeMap[wire.outputNodeName]?
      | throwError "invalid node!"
    traverseGraphCore' outputNode

  let wires := wires.filter (fun w => ¬w.isImplicit)
  -- build application
  let mut inputs : Array Term := #[]
  let portValues := node.portValues ++ Array.replicate (node.type.inputs.size - node.portValues.size) none
  for inputPort in node.type.inputs, i in [0:node.type.inputs.size], val? in portValues do
    let wires' := wires.filter (fun w => w.inputIndex.head? == some i)
    trace[HouLean.lean_graph] m!"port: {inputPort.name}, wires: {wires'.size}}}"
    -- no connected wire but we have a value
    if wires'.size = 0 then
      if let some val := val? then
        if val != "" then 
          match Parser.runParserCategory (← getEnv) `term val "<code>" with
          | .error e => 
            throwError m!"Failed parsing expression value: {val}\n{e}"
          | .ok s =>
            inputs := inputs.push ⟨s⟩
            continue

    inputs := inputs.push (← buildArg' 1 inputPort wires')


  let nodeId := mkIdent node.type.leanConstant
  let nodeStx ← `(term| $nodeId $inputs*)
  withContext do
    let val ← elabTerm nodeStx none >>= instantiateMVars
    let type ← inferType val
    addVariable (Name.mkSimple node.name) type val node.name
  addLine s!"let {node.name} := {toString (← PrettyPrinter.ppTerm nodeStx)}"
  modify (fun s => {s with visitedNodes := s.visitedNodes.insert node.name default})


def buildContext (graph : LeanGraph) : Context := Id.run do
  let nodeMap := Std.HashMap.ofList (graph.nodes.map (fun n => (n.name, n)) |>.toList)
  let portTypes := Std.HashMap.ofList (graph.portTypes.map (fun n => (n.typeName, n)) |>.toList)
  let mut inputConnections : Std.HashMap String (Array Connection) := {}
  let mut outputConnections : Std.HashMap String (Array Connection) := {}
  
  
  for wire in graph.connections do
    inputConnections := inputConnections.alter wire.inputNodeName
      fun wires? => 
        match wires? with
        | some wires => some (wires.push wire)
        | none => some #[wire]
    outputConnections := outputConnections.alter wire.outputNodeName
      fun wires? => 
        match wires? with
        | some wires => some (wires.push wire)
        | none => some #[wire]
  return {
    nodeMap
    portTypes
    inputConnections
    outputConnections
  }

end LeanGraph.Traverse

open Elab Term LeanGraph Traverse in
def traverseGraph (graph : LeanGraph) : TermElabM Unit := do
  let ctx := buildContext graph
  let go : TraverseM Unit := do
    for node in graph.nodes.reverse do
      traverseGraphCore node
  let (_,s) ← go ctx default


open Elab Term LeanGraph Traverse in
def traverseGraph' (graph : LeanGraph) : TermElabM Traverse.State := do
  let ctx := buildContext graph
  let go : TraverseM Unit := do
    for node in graph.nodes.reverse do
      traverseGraphCore' node
  let (_,s) ← go ctx default
  return s
