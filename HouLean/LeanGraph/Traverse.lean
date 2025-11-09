import HouLean.LeanGraph.LeanGraph
import HouLean.LeanGraph.Extension
import HouLean.LeanGraph.Init
import HouLean.LeanGraph.LambdaNodes
import HouLean.Meta.Exact

open Lean Meta Elab Term 

namespace HouLean
namespace LeanGraph.Traverse

/-- Trace class for LeanGraph elaboration -/
register_option trace.HouLean.LeanGraph.elab : Bool := {
  defValue := false
  descr := "trace elaboration of LeanGraph nodes to Lean expressions"
}

/-- State maintained during graph traversal -/
structure State where
  /-- Nodes that have been fully processed, mapping to their fvar -/
  visitedNodes : Std.HashMap String FVarId := {}
  /-- Let-bound variables that have been introduced -/
  vars : Array Expr := #[]
  deriving Inhabited

/-- Context for graph traversal -/
structure Context where
  /-- All nodes in the graph by name -/
  nodeMap : Std.HashMap String Node
  /-- Port type definitions by type name -/
  portTypes : NameMap PortType
  /-- Connections indexed by input node name -/
  inputConnections : Std.HashMap String (Array Connection)
  /-- Connections indexed by output node name -/
  outputConnections : Std.HashMap String (Array Connection)
  /-- Nodes currently being processed (for cycle detection) -/
  nodesBeingProcessed : Lean.PersistentHashSet String


abbrev TraverseM := ReaderT Context <| StateT State <| TermElabM

/-- Mark `nodeName` as being processed. Throws error if already being processed (cycle detection). -/
def withProcessingNode (nodeName : String) (go : TraverseM α) : TraverseM α := do
  if (← read).nodesBeingProcessed.contains nodeName then
    throwError "Loop detected! Trying to compile {nodeName} while compiling its dependencies!"
  trace[HouLean.LeanGraph.elab] "Processing node: {nodeName}"
  withReader (fun ctx => {ctx with nodesBeingProcessed := ctx.nodesBeingProcessed.insert nodeName}) go

/-- Recursively extract subports from a port type -/
partial def getSubports (port : PortType) : CoreM (Array PortType) := do
  match port with
  | .struct _ _ subports => 
    trace[HouLean.LeanGraph.elab] "Port has {subports.size} subports"
    return subports
  | .builtin _ typeName => do
    -- let ctx ← read
    let s := leanGraphExt.getState (← getEnv) 
    let portTypes := s.portTypes
    if let some port' := portTypes.get? typeName.toName then
      trace[HouLean.LeanGraph.elab] "Resolving builtin type: {typeName}"
      match port' with
      | .builtin .. => return #[]
      | _ => getSubports port'
    else
      trace[HouLean.LeanGraph.elab] "Builtin type {typeName} has no subports"
      return #[]


/-- Build a projection expression for accessing a node's output port.
    `index` is a hierarchical path through nested struct ports. -/
partial def buildOutputProj (node : Node) (index : List Nat) : TraverseM Term := do
  match index with
  | [] => throwError "Invalid projection: empty index for {node.type}"
  | i :: is =>
    let some port := node.type.outputs[i]?
      | throwError "Projection out of bounds: index {i} not found in {node.type}"
    let nodeId := mkIdent (Name.mkSimple node.name)
    trace[HouLean.LeanGraph.elab] "Building projection: {node.name}.{index}"
    go port nodeId is
where
  go (port : PortType) (s : Term) (index : List Nat) : TraverseM Term := do
    match index with
    | [] => return s
    | i :: is =>
      let subports ← getSubports port
      let some subport := subports[i]?
        | throwError "Subport projection out of bounds: index {i} not found in {port}"
      let subportId := mkIdent (Name.mkSimple subport.name)
      go subport (← `($s.$subportId)) is

/-- Build argument expression for a node input port.
    Handles both direct connections and nested struct constructions.
    `drop` indicates the current depth in the port hierarchy. -/
partial def buildArg (drop : Nat) (port : PortType) (wires : Array Connection) : TraverseM Term := do
  if wires.size = 0 then
    trace[HouLean.LeanGraph.elab] "No wires connected, using hole"
    return ← `(?_)
  
  -- Single wire at the correct level - direct connection
  if wires.size = 1 then
    let w := wires[0]!
    if w.inputIndex.length = drop then
      trace[HouLean.LeanGraph.elab] "Direct connection from {w.outputNodeName}"
      let proj ← buildOutputProj (← read).nodeMap[w.outputNodeName]! w.outputIndex
      return proj
  
  -- Multiple wires or nested connection - build struct
  trace[HouLean.LeanGraph.elab] "Building struct with {wires.size} connections at depth {drop}"
  let mut args : Array Term := #[]
  let subports ← getSubports port
  for subport in subports, i in [0:subports.size] do
    let wires' := wires.filter (fun w => (w.inputIndex.drop drop).head? == some i)
    args := args.push (← buildArg (drop+1) subport wires')
  let typeId := mkIdent port.typeName.toName
  return ← `((⟨$args,*⟩ : $typeId))

/-- Process a node in the graph, generating a let-binding.
    
    Assigns the elaborated node to a let-binding and returns a new metavariable
    for the continuation. Performs depth-first traversal to ensure dependencies
    are processed first. -/
partial def processNode (node : Node) (rest : MVarId) : TraverseM MVarId := 
  withProcessingNode node.name do
    -- Skip if already visited
    if (← get).visitedNodes.contains node.name then
      trace[HouLean.LeanGraph.elab] "Node {node.name} already visited, skipping"
      return rest

    -- Depth-first search: process all dependencies first
    let inputConnections := (← read).inputConnections[node.name]?.getD #[]
    let nodeMap := (← read).nodeMap
    let deps := inputConnections.filterMap (fun wire => nodeMap[wire.outputNodeName]?)
    
    trace[HouLean.LeanGraph.elab] "Node {node.name} has {deps.size} dependencies"
    let rest ← deps.foldlM (init := rest) (fun (r : MVarId) node => processNode node r)

    -- Work in context where all previous nodes exist as free variables
    rest.withContext do
      let fName := node.type.leanConstant
      trace[HouLean.LeanGraph.elab] "Elaborating node {node.name} as {fName}"
      
      let fn ← mkConstWithFreshMVarLevels fName
      let (allArgs, _, _) ← forallMetaTelescope (← inferType fn)

      let fInfo ← getFunInfo fn
      let args := (fInfo.paramInfo.zip allArgs).filterMap 
        (fun (info, arg) => if info.isExplicit then some arg else none)

      let mut argsStx : Array Term := #[]
      
      -- Build argument expressions
      for _arg in args, i in [0:args.size], port in node.type.inputs do
        let wires := inputConnections.filter (fun w => w.inputIndex.head? == some i)

        -- Check for default value if no wires connected
        if wires.size = 0 then
          if let some val := node.portValues[i]?.join then
            if val != "" then 
              trace[HouLean.LeanGraph.elab] "Using default value for port {i}: {val}"
              match Parser.runParserCategory (← getEnv) `term val "<default_value>" with
              | .error .. => pure ()
              | .ok stx => 
                argsStx := argsStx.push ⟨stx⟩
                continue

        -- Build argument from connections
        let argStx ← buildArg 1 port wires
        argsStx := argsStx.push argStx

      -- Elaborate the complete node expression
      let fnIdent := mkIdent node.type.leanConstant
      let nodeVal ← elabTerm (← `(term| $fnIdent:ident $argsStx*)) none

      let nodeId := Name.mkSimple node.name
      let nodeType ← inferType nodeVal >>= instantiateMVars
      
      trace[HouLean.LeanGraph.elab] "Node type: {nodeType}"
      
      withLetDecl nodeId nodeType nodeVal fun nodeVar => do
        -- Register the new variable
        modify (fun s => {s with 
          vars := s.vars.push nodeVar, 
          -- nodeNameToVar := s.nodeNameToVar.insert node.name nodeVar
          visitedNodes := s.visitedNodes.insert node.name nodeVar.fvarId!})
        
        let rest2 ← mkFreshExprMVar none
        rest.assign (← mkLambdaFVars (←get).vars rest2)

        return rest2.mvarId!

/-- Build the traversal context from a LeanGraph -/
def buildContext (graph : LeanGraph) : CoreM Context := do
  let nodeMap := Std.HashMap.ofList (graph.nodes.map (fun n => (n.name, n)) |>.toList)

  let s := leanGraphExt.getState (← getEnv) 
  let portTypes := s.portTypes  -- Std.HashMap.ofList (graph.portTypes.map (fun n => (n.typeName, n)) |>.toList)
  
  let mut inputConnections : Std.HashMap String (Array Connection) := {}
  let mut outputConnections : Std.HashMap String (Array Connection) := {}
  
  for wire in graph.connections do
    inputConnections := inputConnections.alter wire.inputNodeName fun wires? => 
      some (wires?.getD #[] |>.push wire)
    outputConnections := outputConnections.alter wire.outputNodeName fun wires? => 
      some (wires?.getD #[] |>.push wire)
  
  return {
    nodeMap
    portTypes
    inputConnections
    outputConnections
    nodesBeingProcessed := {}
  }

end LeanGraph.Traverse



open LeanGraph Traverse in
def LeanGraph.typeCheck (graph : LeanGraph) : TermElabM LeanGraph := do

  let go : TraverseM MVarId := do
  
    -- process all nodes
    let mut rest := (← mkFreshExprMVar none).mvarId!
    for node in graph.nodes.reverse do
      rest ← processNode node rest

    return rest

  let ctx ← buildContext graph
  let (rest,⟨nodeToFVar,_⟩) ← go ctx {}

  
  -- update Node Types
  rest.withContext do
    let mut nodes := graph.nodes
    for i in [0:nodes.size] do
      let mut node := nodes[i]!
      nodes := nodes.set! i node

      let var := nodeToFVar[node.name]?.get!
      let val := (← var.getValue?).get!
      
      let (fn, args) := val.withApp (fun fn args => (fn,args))

      let (ins, out) ← 
        forallTelescope (← inferType fn) fun xs _ => do

        let mut inputPorts : Array PortType := #[]

        for arg in args, x in xs do
          let name ← x.fvarId!.getUserName
          let bi ← x.fvarId!.getBinderInfo

          -- skip non-explicit arguments
          unless bi.isExplicit do continue

          let inputPort ← mkPortType (← instantiateMVars (← inferType arg)) false (toString name.eraseMacroScopes)
          inputPorts := inputPorts.push inputPort

        let outputPort ← mkPortType (← instantiateMVars (← inferType val)) false "output"
          
        return (inputPorts, outputPort)

      nodes := nodes.set! i {node with type := {node.type with inputs := ins, outputs := #[out]}}

    return {graph with nodes := nodes}
        
end HouLean
