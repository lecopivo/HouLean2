import HouLean.LeanGraph.LeanGraph
import HouLean.LeanGraph.Extension
import HouLean.LeanGraph.Init
import HouLean.LeanGraph.LambdaNodes
import HouLean.LeanGraph.DirectedGraph
import HouLean.Meta.Exact

open Lean Meta Elab Term Std

namespace HouLean
namespace LeanGraph.Traverse

/-- Trace class for LeanGraph elaboration -/
register_option trace.HouLean.LeanGraph.typecheck : Bool := {
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
  nodeMap : HashMap String Node
  /-- Port type definitions by type name -/
  portTypes : NameMap PortType
  /-- Connections indexed by input node name -/
  inputConnections : HashMap String (Array Connection)
  /-- Connections indexed by output node name -/
  outputConnections : HashMap String (Array Connection)
  /-- Nodes currently being processed (for cycle detection) -/
  nodesBeingProcessed : Lean.PersistentHashSet String
  /-- For every output node we store all the input nodes this node dominates
  such output node should abstract over the variables corresponding tho those
  input nodes -/
  scopes : HashMap String (HashSet String)

open DAG
structure AnalyzeState where
  cache : HashMap String (HashSet String)
  graph : DiGraph String

abbrev TraverseM := ReaderT Context <| StateT State <| TermElabM

abbrev AnalyzeM := ReaderT Context <| MonadCacheT String (HashSet String) <| StateT (DiGraph String) <| TermElabM

/-- Get all nodes that are connected directly to `nodeName` -/
def getNodeBackDeps (nodeName : String) (ctx : Context) : Array Node := 
  let inputConnections := ctx.inputConnections[nodeName]?.getD #[]
  let nodeMap := ctx.nodeMap
  let deps := inputConnections.filterMap (fun wire => nodeMap[wire.outputNodeName]?)
  deps

/-- Extract subnetwork consisting only of input/output nodes which is then used to compute immediate post-dominators.
-/
partial def extractInputOutputSubgraphCore (node : Node) : AnalyzeM (HashSet String) := 
  checkCache node.name fun _ => do

  withTraceNode `HouLean.LeanGraph.typecheck
     (fun _ => return m!"Processing node {node.name}") do

  let mut r : HashSet String := ∅

  if node.type.leanConstant == ``HouLean.input then
    trace[HouLean.LeanGraph.typecheck] m!"Adding input node {node.name}"
    modify (fun g => {g with nodes := g.nodes.insert node.name})
    r := r.insert node.name
    -- no return as there might be dependently typed input

  -- Depth-first search: process all dependencies first
  let deps := getNodeBackDeps node.name (← read)
  r ← deps.foldlM (init := r) (fun r node => do
    let r' ← extractInputOutputSubgraphCore node
    return r.union r')

  if node.type.leanConstant == ``HouLean.output then
    trace[HouLean.LeanGraph.typecheck] m!"Adding output node {node.name}"
    for src in r do
      trace[HouLean.LeanGraph.typecheck] m!"Adding connection node {src} -> {node.name}"
      modify (fun g => g.addEdge src node.name)

    -- any subsequent calculation gets choked throught this node
    r := (∅ : HashSet String).insert node.name
  
  return r


def extractInputOutputSubgraph (graph : LeanGraph) (ctx : Context) : TermElabM (DiGraph String) := do
  withTraceNode `HouLean.LeanGraph.typecheck
     (fun _ => return m!"Extracting subgraph for flow analysis.") do


  let go : AnalyzeM Unit := do
    for node in graph.nodes.reverse do
      let _ ← extractInputOutputSubgraphCore node
    pure ()
  let (_,graph) ← (go ctx).run default

  return graph

/-- Update context about output node's scopes i.e. which input nodes belong to which output  -/
def analyzeInputOutputFlow (graph : LeanGraph) (ctx : Context) : TermElabM Context := do
  let subgraph ← extractInputOutputSubgraph graph ctx

  withTraceNode `HouLean.LeanGraph.typecheck
    (fun _ => return "Extracted subgraph") do
    for (node,succs) in subgraph.successors do
      trace[HouLean.LeanGraph.typecheck] m!"{node} -> {succs.toList}"

  let idom := subgraph.computePostDominators

  -- For every output node we store all the input nodes this node dominates
  -- such output node should abstract over the variables corresponding tho those
  -- input nodes
  let mut scopes : HashMap String (HashSet String) := ∅

  for node in graph.nodes do

    if node.type.leanConstant == ``HouLean.input then
      if let some dom := idom[node.name]? then
        scopes := scopes.alter dom (fun set? => set?.getD {} |>.insert node.name)
      else 
        -- not dominated, if it is connected then we have an invalid graph
        let directOuts := ctx.outputConnections[node.name]?.getD #[]
        let outs := subgraph.successors[node.name]?.getD ∅
        if directOuts.size > 0 then
          throwError m!"Input node {node.name} can't determine its corresponding output node!\
                        Plese ensure that the node {node.name} flows to only one of there nodes {outs.toList}" 

    if node.type.leanConstant == ``HouLean.input ||
       node.type.leanConstant == ``HouLean.output then
       
       let dom? := idom[node.name]?
       trace[HouLean.LeanGraph.typecheck] "Immediate post-dominator for {node.name} is {dom?}"
  
  for (outputNode, inputs) in scopes.toList do
    trace[HouLean.LeanGraph.typecheck] "Output nodes {outputNode} should abstract over {inputs.toList}"

  return { ctx with scopes := scopes }


/-- Mark `nodeName` as being processed. Throws error if already being processed (cycle detection). -/
def withProcessingNode (nodeName : String) (go : TraverseM α) : TraverseM α := do
  if (← read).nodesBeingProcessed.contains nodeName then
    throwError "Loop detected! Trying to compile {nodeName} while compiling its dependencies!"
  withTraceNode `HouLean.LeanGraph.typecheck
     (fun _ => return m!"Processing Node {nodeName}") do
    withReader (fun ctx => {ctx with nodesBeingProcessed := ctx.nodesBeingProcessed.insert nodeName}) go

/-- Recursively extract subports from a port type -/
partial def getSubports (port : PortType) : CoreM (Array PortType) := do
  match port with
  | .struct _ _ subports => 
    trace[HouLean.LeanGraph.typecheck] "Port has {subports.size} subports"
    return subports
  | .builtin _ typeName => do
    -- let ctx ← read
    let s := leanGraphExt.getState (← getEnv) 
    let portTypes := s.portTypes
    if let some port' := portTypes.get? typeName.toName then
      trace[HouLean.LeanGraph.typecheck] "Resolving builtin type: {typeName}"
      match port' with
      | .builtin .. => return #[]
      | _ => getSubports port'
    else
      trace[HouLean.LeanGraph.typecheck] "Builtin type {typeName} has no subports"
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
    trace[HouLean.LeanGraph.typecheck] "Building projection: {node.name}.{index}"
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
    trace[HouLean.LeanGraph.typecheck] "No wires connected, using hole"
    return ← `(default)
  
  -- Single wire at the correct level - direct connection
  if wires.size = 1 then
    let w := wires[0]!
    if w.inputIndex.length = drop then
      trace[HouLean.LeanGraph.typecheck] "Direct connection from {w.outputNodeName}"
      let proj ← buildOutputProj (← read).nodeMap[w.outputNodeName]! w.outputIndex
      return proj
  
  -- Multiple wires or nested connection - build struct
  trace[HouLean.LeanGraph.typecheck] "Building struct with {wires.size} connections at depth {drop}"
  let mut args : Array Term := #[]
  let subports ← getSubports port
  for subport in subports, i in [0:subports.size] do
    let wires' := wires.filter (fun w => (w.inputIndex.drop drop).head? == some i)
    args := args.push (← buildArg (drop+1) subport wires')
  let typeId := mkIdent port.typeName.toName
  return ← `((⟨$args,*⟩ : $typeId))

def hasNoOutputConnection (nodeName : String) : TraverseM Bool := do
  match (← read).outputConnections[nodeName]? with
  | none => return true
  | some wires => return (wires.size = 0)
  
/-- Process a node in the graph, generating a let-binding.
    
    Assigns the elaborated node to a let-binding and returns a new metavariable
    for the continuation. Performs depth-first traversal to ensure dependencies
    are processed first. -/
partial def processNode (node : Node) (rest : MVarId) : TraverseM MVarId := 
  withProcessingNode node.name do

    -- Skip if already visited
    if let some _ := (← get).visitedNodes[node.name]? then
      trace[HouLean.LeanGraph.typecheck] "Node {node.name} already visited, skipping"
      return rest

    -- Depth-first search: process all dependencies first
    let inputConnections := (← read).inputConnections[node.name]?.getD #[]
    -- let outputConnections := (← read).outputConnections[node.name]?.getD #[]        

    let nodeMap := (← read).nodeMap
    let deps := inputConnections.filterMap (fun wire => nodeMap[wire.outputNodeName]?)
    
    trace[HouLean.LeanGraph.typecheck] "Node {node.name} has {deps.size} dependencies"
    let rest ← deps.foldlM (init := rest) (fun r node => do
      let r ← processNode node r
      return r)

    -- Work in context where all previous nodes exist as free variables
    rest.withContext do
      let fName := node.type.leanConstant
      trace[HouLean.LeanGraph.typecheck] "Elaborating node {node.name} as {fName}"
      
      let fn ← mkConstWithFreshMVarLevels fName
      let (allArgs, _, _) ← forallMetaTelescope (← inferType fn)

      let fInfo ← getFunInfo fn
      let args := (fInfo.paramInfo.zip allArgs).filterMap 
        (fun (info, arg) => if info.isExplicit then some arg else none)

      let mut argsStx : Array Term := #[]
      

      -- complete ports at least full arity of the function
      let missingPorts ← args[node.type.inputs.size:].toArray.mapM (fun arg => do
        let name := (← arg.mvarId!.getDecl).userName
        let port ← mkPortType (← inferType arg) false name.eraseMacroScopes.toString
        return port)
      let inputPorts := node.type.inputs ++ missingPorts

      -- Build argument expressions
      for i in [0:max node.type.inputs.size args.size], port in inputPorts do
        let wires := inputConnections.filter (fun w => w.inputIndex.head? == some i)

        -- Check for default value if no wires connected
        if wires.size = 0 then
          if let some val := node.portValues[i]?.join then
            if val != "" then 
              trace[HouLean.LeanGraph.typecheck] "Using default value for port {i}: {val}"
              match Parser.runParserCategory (← getEnv) `term val "<default_value>" with
              | .error .. => 
                argsStx := argsStx.push (← `(term|default))
                continue
              | .ok stx => 
                argsStx := argsStx.push ⟨stx⟩
                continue

        -- Build argument from connections
        let argStx ← buildArg 1 port wires
        argsStx := argsStx.push argStx

      -- Elaborate the complete node expression
      let fnIdent := mkIdent node.type.leanConstant
      let mut nodeVal ← elabTerm (← `(term| $fnIdent:ident $argsStx*)) none
      
      -- trace[HouLean.LeanGraph.typecheck] "Node type: {nodeType}"


      -- -- special case for input node
      if node.type.leanConstant == ``HouLean.input then
        let varName := node.name.toName
        let type ← inferType nodeVal
        return ← withLocalDeclD varName type fun inputVar => do
          trace[HouLean.LeanGraph.typecheck] m!"fun ({node.name} : {type}) =>"
          modify (fun s => {s with 
            visitedNodes := s.visitedNodes.insert node.name inputVar.fvarId!})

          let rest2 ← mkFreshExprMVar none
          rest.assign (← mkLambdaFVars (←get).vars rest2)
          return rest2.mvarId!


      -- special case of output node/lambda abstraction
      if node.type.leanConstant == ``HouLean.output then
        let scopeVarNames := (← read).scopes[node.name]?.getD ∅ |>.toArray
        let visitedNodes := (← get).visitedNodes
        let scopeVars := scopeVarNames.filterMap (fun n => visitedNodes[n]?) |>.map Expr.fvar
        if ← hasNoOutputConnection node.name then
          nodeVal ← mkLambdaFVars (scopeVars ++ (←get).vars) (nodeVal.getArg! 1)
        else
          trace[HouLean.LeanGraph.typecheck] m!"local functions with scope vars: {scopeVars}"
          let deps ← collectForwardDeps scopeVars true
          let deps ← deps.filterM (fun x => x.fvarId!.isLetVar)
          trace[HouLean.LeanGraph.typecheck] m!"their forward let dependencies: {deps}"
          nodeVal ← mkLambdaFVars (scopeVars ++ deps) (nodeVal.getArg! 1)


      let nodeId := Name.mkSimple node.name
      let nodeType ← inferType nodeVal >>= instantiateMVars
      
      withLetDecl nodeId nodeType nodeVal fun nodeVar => do

        trace[HouLean.LeanGraph.typecheck] m!"let {node.name} := {nodeVal}"

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
  let nodeMap := HashMap.ofList (graph.nodes.map (fun n => (n.name, n)) |>.toList)

  let s := leanGraphExt.getState (← getEnv) 
  let portTypes := s.portTypes  -- HashMap.ofList (graph.portTypes.map (fun n => (n.typeName, n)) |>.toList)
  
  let mut inputConnections : HashMap String (Array Connection) := {}
  let mut outputConnections : HashMap String (Array Connection) := {}
  
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
    scopes := {}
  }

end LeanGraph.Traverse


structure TypeCheckResult where
  graph : LeanGraph
  code : String
  mainProgram : Expr

open LeanGraph Traverse PrettyPrinter in
def LeanGraph.typeCheck (graph : LeanGraph) : TermElabM TypeCheckResult := do

  let go : TraverseM MVarId := do

    withTraceNode `HouLean.LeanGraph.typecheck
     (fun _ => return m!"Elaborating Lean Network") do
  
    -- process all nodes
    let mut rest := (← mkFreshExprMVar none).mvarId!
    for node in graph.nodes.reverse do
      rest ← processNode node rest

    return rest

  let ctx ← buildContext graph
  let ctx ← analyzeInputOutputFlow graph ctx
  let (rest,s) ← go ctx {}
  
  withTraceNode `HouLean.LeanGraph.typecheck
     (fun _ => return m!"Gethering Node Types!") do
  

  -- update Node Types
  rest.withContext do

    let mut outputs : Array (Node × Expr) := #[]

    let mut nodes := graph.nodes
    for i in [0:nodes.size] do
      let mut node := nodes[i]!

      let var := s.visitedNodes[node.name]?.get!
       
      if node.type.leanConstant == ``HouLean.input then
        let type ← var.getType
        let inputPort ← mkPortType (← instantiateMVars (← inferType type)) false "type"
        let outputPort ← mkPortType (← instantiateMVars type) false node.name
        let typeStr := toString (← Meta.ppExpr type)
        trace[HouLean.LeanGraph.typecheck] m!"{node.name} : ({inputPort.toString}) → ({outputPort.toString})"
        nodes := nodes.set! i 
          {node with
            type := {node.type with
              inputs := #[inputPort],
              outputs := #[outputPort]}
            portValues := #[some typeStr]}
        continue

      -- only `HouLean.input` should be fvars without values
      let some val ← var.getValue?
        | throwError m!"Can't recover the type of {node.name}, the associated fvar does not have a value!"

      -- special case for output nodes
      if node.type.leanConstant == ``HouLean.output then
        let type ← inferType val
        let inputPort ← forallTelescope type fun _ returnType => do
            mkPortType (← instantiateMVars returnType) false "output"
        let outputPort ← mkPortType (← instantiateMVars type) false "function"
        trace[HouLean.LeanGraph.typecheck] m!"{node.name} : ({inputPort.toString}) → ({outputPort.toString})"
        let node' : Node := {node with
            type := {node.type with
              inputs := #[inputPort],
              outputs := #[outputPort]}}

        nodes := nodes.set! i node'

        -- consider this node as main output node if there are no outgoing connections 
        if ctx.outputConnections[node.name]?.isNone then
          outputs := outputs.push (node, val)
        continue

      -- special case for eval nodes
      if node.type.leanConstant == ``HouLean.eval then
        let fn := val.getArg! 1
        let type ← instantiateMVars (← inferType fn)
        let (inputPorts,outputPort) ← forallTelescope type fun xs r => do
            let funPort ← mkPortType type false "function"
            let args ← xs.mapM (fun x => do mkPortType (← inferType x) false (← x.fvarId!.getUserName).eraseMacroScopes.toString)
            let resultPort ← mkPortType r false "output"
            return (#[funPort] ++ args, resultPort)
        trace[HouLean.LeanGraph.typecheck] m!"{node.name} : ({inputPorts.map (·.toString)}) → ({outputPort.toString})"
        nodes := nodes.set! i 
          {node with
            type := {node.type with
              inputs := inputPorts,
              outputs := #[outputPort]}}
        continue

      
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

        trace[HouLean.LeanGraph.typecheck] m!"{node.name} : {inputPorts.map (·.toString)} → ({outputPort.toString})}"
          
        return (inputPorts, outputPort)

      nodes := nodes.set! i {node with type := {node.type with inputs := ins, outputs := #[out]}}

    let graph := {graph with nodes := nodes}
    let mut mainProgram : Expr := default
    -- let mut mainProgram? : Option Expr ← outputs[0]?.mapM (fun e => instantiateMVars e.2)
    let codes : Array String ← outputs.mapM (fun (node, function) => do
      let body ← withOptions (fun opts => opts 
          |>.set `pp.structureInstanceTypes true
          |>.set `pp.funBinderTypes true) <| 
        Meta.ppExpr function
      let body := s!"{body}".replace "\n" "\n  "
      let code := s!"def {node.name} :=\n  {body}"
      return code)

    let mut code := "import HouLean\n\n" ++ codes.joinl (map:=id) (·++"\n\n"++·)
    
    if outputs.size == 0 then
      mainProgram ← elabTerm (← `(term| HouLean.Apex.SOP.font { text := "Please add an output node!" })) none
      code := code ++ "\n\n" ++ "def run := HouLean.Apex.SOP.font { text := \"Please add an output node!\" }"
    else if outputs.size == 1 then
      mainProgram ← instantiateMVars outputs[0]!.2
      code := code ++ "\n\n" ++ s!"def run := {outputs[0]!.1.name}"
    else
      mainProgram ← elabTerm (← `(term| HouLean.Apex.SOP.font { text := "There should be only one unconnected output node!" })) none
      code := code ++ "\n\n" ++ "def run := HouLean.Apex.SOP.font { text := \"There should be only one unconnected output node!\" }"
      
    return {
      graph := graph
      mainProgram := mainProgram
      code := code
    }

end HouLean

