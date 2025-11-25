import HouLean.LeanGraph.Scopes
import HouLean.LeanGraph.Linearization
import HouLean.LeanGraph.GraphToCodeInit


open Lean Meta Elab Term Std Qq

namespace HouLean
namespace LeanGraph

-- open Traverse

namespace GraphToCode

structure Context where
  nodeMap : HashMap String Node
  /-- Connections indexed by input node name -/
  inputConnections : HashMap String (Array Connection)

structure State where
  nodeValues : HashMap String MVarId := {}
  -- nodeToFVar : HashMap String FVarId

end GraphToCode

abbrev CompileM := ReaderT GraphToCode.Context <| StateT GraphToCode.State <| TermElabM

/-- Recursively exract subports from a port type -/
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
partial def buildOutputProj (node : Node) (index : List Nat) : CompileM Term := do
  match index with
  | [] => throwError "Invalid projection: empty index for {node.type}"
  | i :: is =>
    let some port := node.type.outputs[i]?
      | throwError "Projection out of bounds: index {i} not found in {node.type}"
    let nodeId := mkIdent (Name.mkSimple node.name)
    trace[HouLean.LeanGraph.typecheck] "Building projection: {node.name}.{index}"
    go port nodeId is
where
  go (port : PortType) (s : Term) (index : List Nat) : CompileM Term := do
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
partial def buildArgAux (drop : Nat) (port : PortType) (wires : Array Connection) : CompileM (Option Term) := do
  if wires.size = 0 then
    trace[HouLean.LeanGraph.typecheck] "No wires connected, using hole"
    return none

  -- Single wire at the correct level - direct connection
  if wires.size = 1 then
    let w := wires[0]!
    if w.inputIndex.length = drop then
      trace[HouLean.LeanGraph.typecheck] "Direct connection from {w.outputNodeName}"
      let proj ← buildOutputProj (← read).nodeMap[w.outputNodeName]! w.outputIndex
      return proj

  -- Multiple wires or nested connection - build struct
  trace[HouLean.LeanGraph.typecheck] "Building struct with {wires.size} connections at depth {drop}"
  let mut args : Array (Option (Ident × Term)) := #[]
  let subports ← getSubports port
  for subport in subports, i in [0:subports.size] do
    let wires' := wires.filter (fun w => (w.inputIndex.drop drop).head? == some i)
    let arg? ← buildArgAux (drop+1) subport wires'
    let nameAndArg? := arg?.map (fun arg => (mkIdent subport.name.toName, arg))
    args := args.push nameAndArg?
  let (names, vals) := args.filterMap id |>.unzip

  let typeName := port.typeName.toName
  if typeName.isAnonymous then
    return ← `({ $[ $names:ident := $vals],*})
  else
    let typeId := mkIdent port.typeName.toName
    return ← `({ $[ $names:ident := $vals],* : $typeId})


def buildArg (port : PortType) (wires : Array Connection) : CompileM Term := do
  let t? ← buildArgAux 1 port wires
  -- let typeId := mkIdent port.typeName.toName
  let default : Term ← `((default))
  return t?.getD default


def buildArgs (node : Node) : CompileM (Array Term) := do

  let inputConnections := (← read).inputConnections[node.name]?.getD #[]

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
            let stx : Term := ⟨stx⟩
            argsStx := argsStx.push (← `(term| ($stx)))
            continue

    -- Build argument from connections
    let argStx ← buildArg port wires
    argsStx := argsStx.push argStx

  return argsStx

/-- Is `type` a type in a monad? Return the monad and the type.

`isMonadApp? q(StateM Float (Array Int))` returns `(q(StateM Float), q(Array Int))`.
 -/
def isMonadApp? (type : Expr) : MetaM (Option (Expr×Expr)) := do
  let Expr.app f x := type | return none
  let monad ← mkAppM ``Monad #[f]
  if (← synthInstance? monad).isSome then
    return (f,x)
  return none

def hasAnyInputConnection (node : Node) : CompileM Bool := do
  let inputConnections := (← read).inputConnections[node.name]?.getD #[]
  return !inputConnections.isEmpty


/-- Return the join of two monads.

If `m` is liftable to `n` then the join is `n`.

If `n` is liftable to `m` then the join is `m`.

Error is thrown if neither apply.-/
def monadJoin (m : Expr) (n? : Option Expr) : MetaM Expr :=
  match n? with
  | none => return m
  | some n => do
    let liftMtoN ← mkAppM ``MonadLiftT #[m, n]
    if (← synthInstance? liftMtoN).isSome then
      return n

    let liftNtoM ← mkAppM ``MonadLiftT #[n, m]
    if (← synthInstance? liftNtoM).isSome then
      return m

    throwError m!"Trying to build code for two incompatible monads {m} and {n}!"

def getNodeValue (node : Node) : CompileM Expr := do

  if node.type.leanConstant == ``HouLean.output then
    return (Expr.const ``Unit.unit [])

  if node.type.leanConstant == ``HouLean.input then
    let type := node.portValues[0]? |>.join |>.getD "?_"
    match Parser.runParserCategory (← getEnv) `term type with
    | .ok t =>
      let t : Term := ⟨t⟩
      return (← elabTerm (← `(HouLean.input $t)) q(Type))
    | .error _ => return (← elabTerm (← `(HouLean.input ?_)) q(Type))

  -- todo: add support for macro nodes like
  --    - `struct s merge% s'`
  --    - `struct s push% pos {0,1, 0.2, 0.3}`
  --    - `struct s pop% vel`
  --    - `struct s modify_fun% x (fun x => 10 * x)`

  let fn : Ident := mkIdent node.type.leanConstant
  let args ← buildArgs node

  let stx ← `($fn $args*)

  trace[HouLean.LeanGraph.typecheck] "Elaborating: {stx}"
  let value ← withCurrentNode node.name (elabTerm stx none)
  trace[HouLean.LeanGraph.typecheck] "Elaborated: {value}"

  let msgLog ← Core.getMessageLog
  if msgLog.hasUnreported then
    trace[HouLean.LeanGraph.typecheck] "New messages: {← msgLog.unreported.toArray.mapM (fun m => m.toString)}"
    Core.setMessageLog msgLog.markAllReported

  return value

def storeNodeValue (name : String) (val : Expr) : CompileM Unit := do
  let mvar ← mkFreshExprMVar (← inferType val)
  mvar.mvarId!.assign val
  trace[HouLean.LeanGraph.typecheck] m!"storing {name}'s value: {val}"
  modify (fun s => {s with nodeValues := s.nodeValues.insert name mvar.mvarId!})

partial def graphToCode (graph : LeanGraph) : CompileM Expr := do
  let ctx ← buildContext graph

  let subgraph ← extractInputOutputSubgraph graph ctx
  let idom := subgraph.computePostDominators

  let scopeHierachy ← buildScopeHierarchy graph idom ctx
  -- logInfo m!"{scopeHierachy}"

  let scopes ← assignNodeScopes graph ctx scopeHierachy
  trace[HouLean.LeanGraph.typecheck] "Node scopes: {scopes.toArray}"

  let scopeSubgraphs ← extractScopeSubgraph graph scopes scopeHierachy

  let mut subgraphOrders : HashMap Scope (List String) := {}

  -- sort all subgraph
  for (scope,graph) in scopeSubgraphs do
    trace[HouLean.LeanGraph.typecheck] "Scope nodes {scope}: {graph.toArray.map Prod.fst}"
    let some order := DiGraph.kahnSort ⟨graph⟩
      | throwError m!"Failed to sort subgraph of {scope}"
    subgraphOrders := subgraphOrders.insert scope order
    trace[HouLean.LeanGraph.typecheck] "Order of scope {scope}: {order}"

  let some groundOrder := subgraphOrders[Scope.ground]?
    | throwError "Bug in {decl_name%}, no order for ground scope"


  go 0 Scope.ground groundOrder subgraphOrders none

where
  go (depth : Nat) (scope : Scope) (order : List String) (orders : HashMap Scope (List String)) (monad? : Option Expr) : CompileM Expr := do
    if depth > 10 then
      throwError "Too deep scope, likely there is a bug in {decl_name%}"

    match order with
    | [] => return ← mkFreshExprMVar none
    | nodeName :: ns =>

      withTraceNode `HouLean.LeanGraph.typecheck (fun r =>
        match r with
        | .ok e => return m!"[{exceptEmoji r}] {nodeName}: {e}"
        | .error e => return m!"[{exceptEmoji r}] {nodeName}: {e.toMessageData}") do

      let node := (← read).nodeMap[nodeName]?.get!

      let mut value ← getNodeValue node
      let mut type ← inferType value

      -- input node --
      -----------------
      -- fun nodeName =>
      --   ?rest
      if node.type.leanConstant == ``HouLean.input then
         return ← withLocalDeclD nodeName.toName type fun var => do
           let rest ← go depth scope ns orders monad?
           storeNodeValue nodeName var
           mkLambdaFVars #[var] rest

      -- output node --
      -----------------
      if node.type.leanConstant == ``HouLean.output then
        if scope == Scope.node nodeName then
          if ¬ns.isEmpty then
            let _← go depth scope ns orders monad?
          let args ← buildArgs node
          let arg := args[0]!
          let r ← elabTerm arg none
          let val ←
            if let some monad := monad? then
              mkAppOptM ``pure #[monad, none, none, r]
            else
              pure r
          storeNodeValue nodeName val
          return val
        else
          -- we found an output node of deeper scope
          -- so create a new meta variable for the subscope
          -- let nodeName := ?restSubscope
          --   ?rest
          -- create value for subsocpe
          let some subscope := orders[Scope.node nodeName]?
            | throwError "Can't find order for subscope {Scope.node nodeName}"
          value ← go (depth+1) (.node nodeName) subscope orders monad?
          type ← inferType value

      storeNodeValue nodeName value

      if scope == Scope.ground && ns == [] then
        return ← withLetDecl nodeName.toName type value fun var => do
          mkLetFVars #[var] var

      -- other nodes --
      -----------------
      -- let nodeName ← nodeValue
      -- rest
      if let some (monad, type') ← isMonadApp? type then
        return ← withLocalDeclD nodeName.toName type' fun var => do
          let monad' ← monadJoin monad monad?
          let rest ← go depth scope ns orders monad'
          let mut value := value
          if monad' != monad then
            value ← mkAppOptM ``liftM #[monad, monad', none, none, value]
          mkAppM ``Bind.bind #[value, ← mkLambdaFVars #[var] rest]

      -- let nodeName := nodeValue
      -- rest
      else
        return ← withLetDecl nodeName.toName type value fun var => do
          let rest ← go depth scope ns orders monad?
          mkLetFVars #[var] rest


partial def updateNodeTypesCore : StateT (HashMap String NodeType) CompileM Unit := do

  for (nodeName, mvarId) in (← getThe GraphToCode.State).nodeValues do
    trace[HouLean.LeanGraph.typecheck] m!"upsating types of {nodeName}"
    let some node := (← read).nodeMap[nodeName]?
      | throwError m!"Bug in {decl_name%}, invalid node {nodeName}!"
    mvarId.withContext do
      let value ← instantiateMVars (.mvar mvarId)
      let type ← inferType value

      if node.type.leanConstant == ``HouLean.input then
        updateInputNode node type
      else
        updateNode node value

where
  updateInputNode (node : Node) (type : Expr) : StateT (HashMap String NodeType) CompileM Unit := do
    let inputPort ← mkPortType (← instantiateMVars (← inferType type)) false "type"
    let outputPort ← mkPortType (← instantiateMVars type) false node.name
    trace[HouLean.LeanGraph.typecheck] m!"{node.name} : ({inputPort.toString}) → ({outputPort.toString})"
    let nodeType : NodeType := { node.type with
      inputs := #[inputPort]
      outputs := #[outputPort]
    }
    modify (fun s => s.insert node.name nodeType)

  updateNode (node : Node) (value : Expr) : StateT (HashMap String NodeType) CompileM Unit := do
    if node.type.leanConstant  == ``HouLean.output then
      return ← updateOutputNode node value

    if node.type.leanConstant  == ``HouLean.eval then
      return ← updateEvalNode node value

    updateNormalNode node value

  updateOutputNode (node : Node) (value : Expr) : StateT (HashMap String NodeType) CompileM Unit := do
    let type ← inferType value
    let inputPort ← forallTelescope type fun _ returnType => do
        mkPortType (← instantiateMVars returnType) false "output"
    let outputPort ← mkPortType (← instantiateMVars type) false "function"
    trace[HouLean.LeanGraph.typecheck] m!"{node.name} : ({inputPort.toString}) → ({outputPort.toString})"
    let nodeType : NodeType := { node.type with
      inputs := #[inputPort]
      outputs := #[outputPort]
    }
    modify (fun s => s.insert node.name nodeType)

  updateEvalNode (node : Node) (value : Expr) : StateT (HashMap String NodeType) CompileM Unit := do
    let fn := value.getArg! 1
    let type ← instantiateMVars (← inferType fn)
    let (inputPorts,outputPort) ← forallTelescope type fun xs r => do
        let funPort ← mkPortType type false "function"
        let args ← xs.mapM (fun x => do mkPortType (← inferType x) false (← x.fvarId!.getUserName).eraseMacroScopes.toString)
        let resultPort ← mkPortType r false "output"
        return (#[funPort] ++ args, resultPort)
    trace[HouLean.LeanGraph.typecheck] m!"{node.name} : ({inputPorts.map (·.toString)}) → ({outputPort.toString})"
    let nodeType : NodeType := { node.type with
      inputs := inputPorts
      outputs := #[outputPort]
    }
    modify (fun s => s.insert node.name nodeType)

  updateNormalNode (node : Node) (value : Expr) : StateT (HashMap String NodeType) CompileM Unit := do

    let (fn, args) := value.withApp (fun fn args => (fn,args))

    forallTelescope (← inferType fn) fun xs _ => do

      let mut inputPorts : Array PortType := #[]

      for arg in args, x in xs do
        let name ← x.fvarId!.getUserName
        let bi ← x.fvarId!.getBinderInfo

        -- skip non-explicit arguments
        unless bi.isExplicit do continue

        let inputPort ← mkPortType (← instantiateMVars (← inferType arg)) false (toString name.eraseMacroScopes)
        inputPorts := inputPorts.push inputPort

      let outputPort ← mkPortType (← instantiateMVars (← inferType value)) false "output"

      trace[HouLean.LeanGraph.typecheck] m!"{node.name} : {inputPorts.map (·.toString)} → ({outputPort.toString})}"

      let nodeType : NodeType := { node.type with
        inputs := inputPorts
        outputs := #[outputPort]
      }
      modify (fun s => s.insert node.name nodeType)

  -- todo: add support for macro nodes like
  --    - `struct s merge% s'`
  --    - `struct s push% pos {0,1, 0.2, 0.3}`
  --    - `struct s pop% vel`
  --    - `struct s modify_fun% x (fun x => 10 * x)`

def updateNodeTypes (graph : LeanGraph) : CompileM LeanGraph := do

  let (_,s) ← updateNodeTypesCore {}

  let mut nodes := graph.nodes
  for i in [0:nodes.size] do
    let node := nodes[i]!
    if let some newType := s[node.name]? then
      nodes := nodes.set! i { node with type := newType }

  return { graph with nodes := nodes }


structure TypeCheckResult where
  graph : LeanGraph
  code : String
  mainProgram : Expr

open Qq Apex in
def maybeAddWithVisualizer (leanCode : Expr) : TermElabM Expr := do
  forallTelescope (← inferType leanCode) fun xs r => do
    unless ← isDefEq r q(VisualizeM Geometry) do
      return leanCode
    let e ← mkAppM ``Apex.withVisualizer #[← Apex.Compiler.betaThroughLet (leanCode.beta xs)]
    return ← mkLambdaFVars xs e

open LeanGraph PrettyPrinter in
def typeCheck (graph : LeanGraph) : TermElabM TypeCheckResult := do

  let ctx ← buildContext graph
  let ctx : GraphToCode.Context := {
    nodeMap := ctx.nodeMap
    inputConnections := ctx.inputConnections
  }
  let (leanCode,s) ← withSynthesizeLight (graphToCode graph ctx {})
  let (graph,_s) ← updateNodeTypes graph ctx s

  let leanCode ← instantiateMVars (← maybeAddWithVisualizer leanCode)

  -- let code ← delab leanCode
  let codeBody ← withOptions (fun opts => opts
      |>.set `pp.structureInstanceTypes true
      |>.set `pp.funBinderTypes true) <|
     Meta.ppExpr leanCode
  let codeBody := s!"{codeBody}".replace "\n" "\n  "
  let code := s!"import HouLean\n\n\
  def run :=\n  {codeBody}"

  return {
    graph := graph
    code := code
    mainProgram := leanCode
  }
