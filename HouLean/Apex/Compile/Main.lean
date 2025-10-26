import HouLean.Apex.Compile.Graph

open Lean Meta Std

namespace HouLean.Apex.Compiler

structure GraphState where
  /-- Currently built graph -/
  graph : ApexGraph
  /-- Mapping from local fvars to ports -/
  fvarToPort : HashMap FVarId (ArrayTree PortId)
  /-- Counts how many times we added a node with particular name -/
  nameCounter : HashMap String Nat
deriving Inhabited

abbrev GraphCompileM := StateT GraphState MetaM

def modifyGraph {α} (f : ApexGraph → MetaM (ApexGraph × α)) : GraphCompileM α := do
  let ⟨g, map, names⟩ ← get
  let (g, val) ← f g
  set { graph := g, fvarToPort := map, nameCounter := names : GraphState}
  return val

def readGraph {α} (f : ApexGraph → MetaM α) : GraphCompileM α := do
  let ⟨g, _, _⟩ ← get
  return (← f g)

def getPortName (portId : Nat) : GraphCompileM String := do
  readGraph (fun g => return g.ports[portId]!.name)

def getPortType (portId : Nat) : GraphCompileM PortType := do
  readGraph (fun g => return g.ports[portId]!.type)

def getFreshNodeName (name : String) : GraphCompileM String := do 
  let nameCounter := (← get).nameCounter
  if let some i := nameCounter[name]? then
    modify (fun s => {s with nameCounter := nameCounter.insert name (i+1)})
    return name ++ toString i
  else
    modify (fun s => {s with nameCounter := nameCounter.insert name 1})
    return name

def arrayToBundle (ps : Array PortBundle) : PortBundle :=
  if ps.size = 1 then
    ps[0]!
  else
    .node ps

/-- Add a node to the graph with optional subports -/
def addNode (nodeType : NodeType) (subports : Array AddSubPortSpec) 
    (userName? : Option String := none) : 
    GraphCompileM (Nat × Array PortBundle × PortBundle) := do
  let nodeName := (userName?.getD (← getFreshNodeName nodeType.name))
    |>.replace "<" "_" |>.replace ">" "" |>.replace "." "_" |>.toLower
  modifyGraph (fun g => do

    let nodeOff := g.nodes.size
    let mut portOff := g.ports.size

    let mut ports : Array Port := nodeType.ports.mapIdx (fun i p => {
      p with globalId := portOff + i
             nodeId := nodeOff })

    let mut bundles : Array PortBundle := ports.map (fun p =>
      .leaf p.globalId)
    
    -- Add all subports
    for ⟨i, j?, names⟩ in subports do
      let p := ports[i]!
      let mut inputPorts : Array PortBundle := #[]
      let mut outputPorts : Array PortBundle := #[]
      for n in names do
        inputPorts := inputPorts.push (.leaf (ports.size + portOff))
        ports := ports.push {
          localId := ports.size
          name := n
          type := p.type
          dir := .input
          globalId := ports.size + portOff
          nodeId := nodeOff : Port
        }
        if j?.isSome then
          outputPorts := outputPorts.push (.leaf (ports.size + portOff))
          ports := ports.push {
            localId := ports.size
            name := n
            type := p.type
            dir := .output
            globalId := ports.size + portOff
            nodeId := nodeOff : Port
          }

      bundles := bundles.set! i (.node inputPorts)
      if let some j := j? then
        bundles := bundles.set! j (.node outputPorts)

    -- Filter out input and output port bundles
    let inputs := ports.zip bundles |>.filterMap (fun (p, b) =>
      if p.dir == .input then some b else none)
    let outputs := ports.zip bundles |>.filterMap (fun (p, b) =>
      if p.dir == .output && p.type != .rundata then some b else none)

    let portIds := ports.map (fun p => p.globalId)

    let node : Node := {
      name := nodeName
      type := nodeType
      globalId := nodeOff
      ports := portIds
      subPorts := subports.map (fun ⟨i,j?,n⟩ => ⟨i+portOff, j?.map (·+portOff), n⟩)
    }
    
    let g := {g with
      nodes := g.nodes.push node
      ports := g.ports ++ ports
    }

    return (g, nodeOff, inputs, arrayToBundle outputs))

def setIntPort (val : Int) (id : PortId) : GraphCompileM Unit := do
  modifyGraph (fun g => return ({g with literals := g.literals.push (.int val, id)}, ()))

def setFloatPort (val : Float) (id : PortId) : GraphCompileM Unit := do
  modifyGraph (fun g => return ({g with literals := g.literals.push (.float val, id)}, ()))

def setStringPort (val : String) (id : PortId) : GraphCompileM Unit := do
  modifyGraph (fun g => return ({g with literals := g.literals.push (.str val, id)}, ()))

/-- Add an integer constant node -/
def addInt (val : Int) : GraphCompileM PortBundle := do
  let nodeType : NodeType := {
    name := "Value<Int>"
    ports := #[
      {localId := 0, dir := .input, name := "val", type := .builtin "Int"},
      {localId := 1, dir := .output, name := "val", type := .builtin "Int"}
    ]
  }
  let (_, #[.leaf input], output) ← addNode nodeType #[]
    | throwError "Failed to add Value<Int> node"
  setIntPort val input
  return output

/-- Add a float constant node -/
def addFloat (val : Float) : GraphCompileM PortBundle := do
  let nodeType : NodeType := {
    name := "Value<Float>"
    ports := #[
      {localId := 0, dir := .input, name := "val", type := .builtin "Float"},
      {localId := 1, dir := .output, name := "val", type := .builtin "Float"}
    ]
  }
  let (_, #[.leaf input], output) ← addNode nodeType #[]
    | throwError "Failed to add Value<Float> node"
  setFloatPort val input
  return output

/-- Add a string constant node -/
def addString (val : String) : GraphCompileM PortBundle := do
  let nodeType : NodeType := {
    name := "Value<String>"
    ports := #[
      {localId := 0, dir := .input, name := "val", type := .builtin "String"},
      {localId := 1, dir := .output, name := "val", type := .builtin "String"}
    ]
  }
  let (_, #[.leaf input], output) ← addNode nodeType #[]
    | throwError "Failed to add Value<String> node"
  setStringPort val input
  return output

def addForBegin (state : ApexStaticType) : GraphCompileM ForBeginPorts := do
  modifyGraph (fun g => return g.addForBegin state)

def addForEnd (state : ApexStaticType) : GraphCompileM ForEndPorts := do
  modifyGraph (fun g => return g.addForEnd state)
    
def makeConnection (src trg : PortBundle) : GraphCompileM Bool := do
  let _ ← modifyGraph (fun g => return (g.addConnections src trg, ()))
  return true

def makeConnections (src trg : Array PortBundle) : GraphCompileM Bool := do
  if src.size != trg.size then
    return false

  for s in src, t in trg do
    unless ← makeConnection s t do
      return false

  return true

/-- Add identity node for a given type, returns input and output port bundle -/
def addIdentity (type : Expr) (userName? : Option String := none) : 
    GraphCompileM (PortBundle × PortBundle) := do
  let some apexType ← getApexType? type
    | throwError m!"Cannot get APEX type for {type}"

  match ← enforceStaticSize apexType with
  | .error n => 
    throwError m!"Cannot determine static size of {type}, the number {n} should be a compile time constant"
  | .ok type => 
    -- Offset by two for default `__spare__` ports
    let nodeType : NodeType := {
      name := "__null__"
      ports := #[
        {localId := 0, name := "__spare__", type := .undefined, dir := .input},
        {localId := 1, name := "__spare__", type := .undefined, dir := .output}
      ]
    }
    
    let subport : AddSubPortSpec := {
      variadicPortLocalId := 0
      outputPortId := some 1
      subports := type.flatten.map (fun (n, _) => n)
    }
    let (_, inputs, output) ← addNode nodeType #[subport] userName?

    return (inputs[0]!, output)

/-- Add a free variable node -/
def addFVar (var : FVarId) : GraphCompileM (PortBundle × PortBundle) := do
  let (input, output) ← addIdentity (← var.getType) (← var.getUserName).eraseMacroScopes.toString
  modify (fun s => {s with fvarToPort := s.fvarToPort.insert var output})
  return (input, output)

/-- Check if expression is a variadic argument and return its elements -/
partial def isVariadicArg (e : Expr) : MetaM (Option (Array Expr)) := do
  let t ← inferType e
  unless t.isAppOfArity ``Vector 2 do
    return none

  let n := (t.getArg! 1)
  let .lit (.natVal n) ← whnfD n
    | throwError m!"Size {n} of variadic argument must be known at compile time: {e}"

  unless e.isAppOfArity ``Vector.mk 4 do
    throwError m!"Variadic argument must be explicit: {e}"

  let arr ← whnfI (e.getArg! 2)
  unless arr.isAppOfArity ``Array.mk 2 do
    throwError m!"Variadic argument must be explicit: {e}"

  let l ← whnfI (arr.getArg! 1)
  let xs := (← splitList l).toArray

  unless xs.size == n do
    throwError m!"All elements of variadic argument must be known at compile time"
  
  return xs
where 
  splitList (l : Expr) : MetaM (List Expr) := do
    match (← whnf l) with
    | .app (.const ``List.nil _) _ => return []
    | mkApp3 (.const ``List.cons _) _ x xs => return x :: (← splitList xs)
    | l => throwError m!"Invalid variadic argument {l} {l.getAppFn}"

open Qq in
mutual

/-- Compile Lean expression to APEX graph -/
partial def toApexGraph (e : Expr) (userName? : Option String := none) : 
    GraphCompileM PortBundle := do
  let e ← withConfig (fun cfg => {cfg with iota := true, zeta := false}) <| whnfI e

  match e with
  | .bvar _ => 
    throwError m!"Cannot compile bound variable: {e}"
  | .fvar id => 
    let some portBundle := (← get).fvarToPort[id]?
      | throwError m!"Unrecognized free variable: {e}"
    return portBundle
  | .lit (.natVal n) =>
    addInt n
  | .lit (.strVal s) => 
    addString s
  | mkApp3 (.const ``Float.ofScientific []) m s e => 
    let .lit (.natVal m) := m | throwError s!"Invalid literal for mantissa: {m}"
    let .some e := e.nat? | throwError s!"Invalid literal for exponent: {e}"
    let s := if ← isDefEq s q(true) then true else false
    addFloat (Float.ofScientific m s e)
  | .sort _ => 
    throwError m!"Cannot compile sort: {e}"
  | .mvar _ => 
    let e ← instantiateMVars e
    if e.isMVar then 
      throwError m!"Cannot compile metavariable: {e}"
    toApexGraph (← instantiateMVars e)
  | .app .. =>
    let (fn, args) := e.withApp fun fn args => (fn, args)

    -- implemented_by -- so something more clever here
    let s := compilerExt.getState (← getEnv)
    if let some fn' := s.implementedBy[fn]? then
      return ← toApexGraph (fn'.beta args)

    let .const fname _ := fn 
      | throwError m!"Unexpected function application: {e}"

    -- Detect variadic argument
    if let some xs ← isVariadicArg e then
      let xs ← xs.mapM toApexGraph 
      return arrayToBundle xs

    -- Special case: constructor
    if let some (info, args) ← constructorApp? e then
      let args := args[info.numParams:].toArray
      let args ← args.mapM toApexGraph
      return .node args

    -- Special case: projection
    if let some info ← getProjectionFnInfo? fname then
      let x := args[args.size - 1]!
      let x ← toApexGraph x
      let some si := x.child? info.i
        | throwError m!"Invalid projection {info.i} of {args[args.size - 1]!}: {x.toString}"
      return si

    -- Special case: for-loop
    if fname = ``Std.Range.forIn'.loop then
      let monad := args[0]!
      let range ← whnf args[3]!

      unless ← isDefEq monad q(@Id : Type → Type) do
        throwError m!"Unsupported monad {monad} in for loop: {e}"

      unless range.isAppOfArity' ``Std.Range.mk 4 do
        throwError m!"Unsupported range {range} in for loop: {e}"

      let b ← whnf (range.getArg! 0)
      let s ← whnf (range.getArg! 2)
      unless ← isDefEq b q(0 : Nat) do
        throwError m!"Invalid range start: {b}"
      unless ← isDefEq s q(1 : Nat) do
        throwError m!"Invalid range step: {s}"

      let iterations ← toApexGraph (range.getArg! 1)
      let (loopIns, loopOut) ← functionToApexGraph args[4]!
      let input ← toApexGraph args[5]!

      let loopIndexIn := loopIns[0]!
      let loopIn := loopIns[2]!

      let stateShape ← input.mapIdxM (fun i p => do 
        let p ← readGraph (fun g => pure g.ports[i]!)
        match p.type with
        | .builtin typeName => return (p.name, typeName)
        | _ => throwError m!"Invalid state {← inferType args[5]!} in for loop")
      
      let bp ← addForBegin stateShape
      let ep ← addForEnd stateShape

      let _ ← makeConnection input bp.stateIn
      let _ ← makeConnection iterations bp.iterations
      let _ ← makeConnection bp.scope ep.scope
      let _ ← makeConnection bp.index loopIndexIn
      let _ ← makeConnection bp.stateOut loopIn
      let _ ← makeConnection loopOut ep.stateIn

      return ep.stateOut

    -- Normal application
    let args ← getExplicitArgs fn args
    let some nodeType ← getNodeType (.const fname []) 
      | throwError m!"No APEX node for {fname}"

    let argPorts ← args.mapM toApexGraph
    let fnInputs := nodeType.inputs
    
    logInfo m!"application {e}"
    -- Find variadic arguments
    let mut subPorts : Array AddSubPortSpec := #[]
    for arg in args, i in [0:args.size] do
      let type ← inferType arg
      if type.isAppOfArity' ``Vector 2 then
        logInfo m!"variadic argument {i} {arg} in {e}"
        let id := fnInputs[i]!.localId
        let names ← argPorts[i]!.flatten.mapM getPortName
        subPorts := subPorts.push ⟨id, none, names⟩
      
    let (_, inputBundle, outputBundle) ← addNode nodeType subPorts userName?
    let _ ← makeConnections argPorts inputBundle
    return outputBundle
    
  | .letE n t v b _ => 
    withLocalDeclD n t fun x => do
      let letOutput ← toApexGraph v
      let (input, _) ← addFVar x.fvarId!
      let _ ← makeConnections #[letOutput] #[input]
      let b := b.instantiate1 x
      toApexGraph b

  | .proj _ i x => 
    let x' := x
    let x ← toApexGraph x
    let some si := x.child? i
      | throwError m!"Invalid projection {i} of {x'}: {x.toString}"
    return si

  | _ => throwError m!"Cannot compile expression: {e}"

/-- Compile function to APEX graph, returning input and output bundles -/  
partial def functionToApexGraph (e : Expr) : 
    GraphCompileM (Array PortBundle × PortBundle) := do
  forallTelescope (← inferType e) fun xs _ => do
    if xs.size = 0 then
      throwError m!"Function expected: {e}"

    let b ← withConfig (fun cfg => {cfg with iota := false, zeta := false}) <|
      whnfI (e.beta xs)

    let mut inputs : Array PortBundle := #[]
    for x in xs do
      let (input, _) ← addFVar x.fvarId!
      inputs := inputs.push input
    let output ← toApexGraph b
    return (inputs, output)

end

end HouLean.Apex.Compiler
