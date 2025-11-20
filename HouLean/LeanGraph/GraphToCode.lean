import HouLean.LeanGraph.Scopes
import HouLean.LeanGraph.Linearization

open Lean Meta Elab Term Std Qq

namespace HouLean
namespace LeanGraph

open Traverse


namespace GraphToCode

structure Context where
  nodeMap : HashMap String Node
  /-- Connections indexed by input node name -/
  inputConnections : HashMap String (Array Connection)


structure State where
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
  let typeId := mkIdent port.typeName.toName
  return ← `({ $[ $names:ident := $vals],* : $typeId})

def buildArg (port : PortType) (wires : Array Connection) : CompileM Term := do
  let t? ← buildArgAux 1 port wires
  let typeId := mkIdent port.typeName.toName
  let default : Term ← `((default : $typeId))
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


partial def graphToCode (graph : LeanGraph) : CompileM Expr := do
  let ctx ← buildContext graph

  let subgraph ← extractInputOutputSubgraph graph ctx
  let idom := subgraph.computePostDominators

  let scopeHierachy ← buildScopeHierarchy graph idom ctx
  -- logInfo m!"{scopeHierachy}"

  let scopes ← assignNodeScopes graph ctx scopeHierachy
  -- logInfo m!"{scopes.toList}"

  let scopeSubgraphs ← extractScopeSubgraph graph scopes scopeHierachy

  let mut subgraphOrders : HashMap Scope (List String) := {}

  -- sort all subgraph
  for (scope,graph) in scopeSubgraphs do
    let some order := DiGraph.kahnSort ⟨graph⟩
      | throwError m!"Failed to sort subgraph of {scope}"
    subgraphOrders := subgraphOrders.insert scope order

  let some groundOrder := subgraphOrders[Scope.ground]?
    | throwError "Bug in {decl_name%}, no order for ground scope"

  let rest ← mkFreshExprMVar none

  go 0 Scope.ground rest groundOrder subgraphOrders

  return ← instantiateMVars rest

where
  nodeValue (node : Node) : CompileM Expr :=
    return mkStrLit node.name

  go (depth : Nat) (scope : Scope) (rest : Expr) (order : List String) (orders : HashMap Scope (List String)) : CompileM Unit := do
    if depth > 10 then
      throwError "Too deep scope, likely there is a bug in {decl_name%}"

    let indent := String.mk (List.replicate (2*depth) ' ')

    let orgRest := rest

    let mut rest := rest

    let mut funVars : Array Expr := #[]
    let mut letVars : Array Expr := #[]

    for nodeName in order do

      logInfo nodeName

      let node := (← read).nodeMap[nodeName]?.get!
      let args ← buildArgs node

      let fn : Ident := mkIdent node.type.leanConstant
      let mut value ← rest.mvarId!.withContext do
        if node.type.leanConstant != ``HouLean.output then
          elabTerm (← `($fn $args*)) none
        else
          pure (.const ``Unit.unit [])
      let mut type ← rest.mvarId!.withContext do inferType value

      -- logInfo m!"{indent}{node.type.leanConstant} {args}"
      orgRest.mvarId!.withContext do (logInfo (← instantiateMVars orgRest))

      -- input node --
      -----------------
      -- ?rest ← fun nodeName =>
      --         ?rest2
      if node.type.leanConstant == ``HouLean.input then

        let (rest',var) ← rest.mvarId!.withContext do
          withLocalDeclD nodeName.toName type fun var => do
            let rest2 ← mkFreshExprMVar none
            let restVal ← mkLambdaFVars #[var] rest2 (usedLetOnly:=false)
            rest.mvarId!.assign restVal
            return (rest2,var)

        rest := rest'
        funVars := funVars.push var

        continue


      -- output node --
      -----------------
      if node.type.leanConstant == ``HouLean.output then

        if scope == .node nodeName then
          -- end of scope, we should return the input to the
          let arg := args[0]!
          let val ← rest.mvarId!.withContext do
            let r ← elabTerm arg none
            mkLambdaFVars funVars (← mkLetFVars letVars r)
          rest.mvarId!.withContext do
            logInfo m!"return value of {nodeName}: {val}"
          rest.mvarId!.assign val
          continue
        else
          -- we found an output node of deeper scope
          -- so create a new meta variable for the subscope
          -- ?rest ← let nodeName := ?restSubscope
          --         ?rest2
          let (value',type') ← rest.mvarId!.withContext do
            let restSubscope ← mkFreshExprMVar none

            -- create value for subsocpe
            let some subscope := orders[Scope.node nodeName]?
              | throwError "Can't find order for subscope {Scope.node nodeName}"
            go (depth+1) (.node nodeName) restSubscope subscope orders

            -- `value` and `type` do not hold valid values
            let value' ← instantiateMVars restSubscope
            let type' ← inferType value'
            return (value', type')

          -- update value and type with proper values, they were invalid originally
          value := value'
          type := type'


      rest.mvarId!.withContext do logInfo m!"binding value: let {nodeName} := {value}"
      -- other nodes --
      -----------------
      -- ?rest ← let nodeName := nodeValue
      --         ?rest2
      let (rest',var) ← rest.mvarId!.withContext do
        withLetDecl nodeName.toName type value fun var => do
          let rest2 ← mkFreshExprMVar none
          let restVal ← mkLetFVars #[var] rest2 (usedLetOnly:=false)
          rest.mvarId!.assign restVal
          return (rest2,var)
      rest := rest'
      letVars := letVars.push var

    if scope == Scope.ground then

      rest.mvarId!.withContext do
        let out : Ident := mkIdent `output
        let var ← elabTermAndSynthesize out none
        let result ← mkLetFVars #[var] var
        logInfo m!"final result:\n{← instantiateMVars result}"
