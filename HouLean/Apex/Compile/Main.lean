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

partial def arrayToProdBundle (ps : Array PortBundle) : PortBundle :=
  if ps.size = 0 then
    .node #[]
  else if ps.size = 1 then
    ps[0]!
  else
    .node #[ps[0]!, arrayToProdBundle ps[1:].toArray]

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
    for ⟨i?, j?, namesAndTypes⟩ in subports do
      let mut inputPorts : Array PortBundle := #[]
      let mut outputPorts : Array PortBundle := #[]
      for (n,t) in namesAndTypes do
        if i?.isSome then
          inputPorts := inputPorts.push (.leaf (ports.size + portOff))
          ports := ports.push {
            localId := ports.size
            name := n
            type := .builtin t
            dir := .input
            globalId := ports.size + portOff
            nodeId := nodeOff : Port
          }
        if j?.isSome then
          outputPorts := outputPorts.push (.leaf (ports.size + portOff))
          ports := ports.push {
            localId := ports.size
            name := n
            type := .builtin t
            dir := .output
            globalId := ports.size + portOff
            nodeId := nodeOff : Port
          }

      if let some i := i? then
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
      subPorts := subports.map (fun ⟨i?,j?,n⟩ => ⟨i?.map (·+portOff), j?.map (·+portOff), n⟩)
    }
    
    let g := {g with
      nodes := g.nodes.push node
      ports := g.ports ++ ports
    }

    return (g, nodeOff, inputs, arrayToProdBundle outputs))

def setIntPort (val : Int) (id : PortId) : GraphCompileM Unit := do
  modifyGraph (fun g => return ({g with literals := g.literals.push (.int val, id)}, ()))

def setFloatPort (val : Float) (id : PortId) : GraphCompileM Unit := do
  modifyGraph (fun g => return ({g with literals := g.literals.push (.float val, id)}, ()))

def setStringPort (val : String) (id : PortId) : GraphCompileM Unit := do
  modifyGraph (fun g => return ({g with literals := g.literals.push (.str val, id)}, ()))

def setBoolPort (val : Bool) (id : PortId) : GraphCompileM Unit := do
  modifyGraph (fun g => return ({g with literals := g.literals.push (.bool val, id)}, ()))



/-- Add an integer constant node -/
def addInt (val : Int) : GraphCompileM PortBundle := do
  let nodeType : NodeType := {
    name := "Value<Int>"
    ports := #[
      {localId := 0, dir := .input, name := "parm", type := .builtin "Int"},
      {localId := 1, dir := .output, name := "value", type := .builtin "Int"}
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
      {localId := 0, dir := .input, name := "parm", type := .builtin "Float"},
      {localId := 1, dir := .output, name := "value", type := .builtin "Float"}
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
      {localId := 0, dir := .input, name := "parm", type := .builtin "String"},
      {localId := 1, dir := .output, name := "value", type := .builtin "String"}
    ]
  }
  let (_, #[.leaf input], output) ← addNode nodeType #[]
    | throwError "Failed to add Value<String> node"
  setStringPort val input
  return output

/-- Add a bool constant node -/
def addBool (val : Bool) : GraphCompileM PortBundle := do
  let nodeType : NodeType := {
    name := "Value<Bool>"
    ports := #[
      {localId := 0, dir := .input, name := "parm", type := .builtin "Float"},
      {localId := 1, dir := .output, name := "value", type := .builtin "Float"}
    ]
  }
  let (_, #[.leaf input], output) ← addNode nodeType #[]
    | throwError "Failed to add Value<Bool> node"
  setBoolPort val input
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

/-- Add identit ynode for a given type, returns input and output port bundle -/
def addIdentity (type : Expr) (userName? : Option String := none) : 
    GraphCompileM (PortBundle × PortBundle) := do
  let some apexType ← getApexType? type
    | throwError m!"Cannot get APEX type for {type}"

  match ← enforceStaticSize apexType with
  | .error n => 
    throwError m!"Cannot determine static size of {type}, the number {n} should be a compile time constant"
  | .ok type => 

    let mut g := (← get).graph

    let localPorts : Array LocalPort := #[
      { localId := 0, name := "__spare__", type := .undefined, dir := .input},
      { localId := 1, name := "__spare__", type := .undefined, dir := .output}
    ]

    let nodeType : NodeType := {
      name := "__null__"
      ports := localPorts
    }

    let nodeOff := g.nodes.size
    let portOff := g.ports.size

    let ports := localPorts.map (fun p => 
      {p with globalId := portOff + p.localId, nodeId := nodeOff : Port})

    let stateIn : ArrayTree Port := type.mapIdx fun i (name,typeName) => 
      { localId  := 2*i + localPorts.size, 
        globalId := 2*i + localPorts.size + portOff
        nodeId := nodeOff
        name := name
        type := .builtin typeName
        dir := .input : Port}

    let stateOut : ArrayTree Port := type.mapIdx fun i (name,typeName) => 
      { localId  := 2*i + 1 + localPorts.size, 
        globalId := 2*i + 1 + localPorts.size + portOff
        nodeId := nodeOff
        name := name
        type := .builtin typeName
        dir := .output : Port}

    let statePorts := stateIn.flatten.zip stateOut.flatten |>.map (fun (x,y) => #[x,y]) |>.flatten
    let ports := ports ++ statePorts

    let node : Node := {
      type := nodeType
      globalId := nodeOff
      name := (← getFreshNodeName (userName?.getD "null"))
      ports := ports.map (fun p => p.globalId)
      subPorts := #[{
        inputPortId  := some (portOff + 0)
        outputPortId := some (portOff + 1)
        subports := stateIn.flatten.map (fun p => (p.name, p.type.builtin!))
        }]
    }

    g := {g with
      nodes := g.nodes.push node
      ports := g.ports ++ ports
    }

    modify (fun s => {s with graph := g})

    return (stateIn.mapIdx (fun _ p => p.globalId), 
            stateOut.mapIdx (fun _ p => p.globalId))


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

instance : ExceptToEmoji Exception PortBundle where
  toEmoji r :=
    match r with
    | .ok p => s!"✅️ {p.toString}"
    | .error _ => "💥️"


open Qq in
mutual

/-- Compile Lean expression to APEX graph -/
partial def toApexGraph (e : Expr) (userName? : Option String := none) : 
    GraphCompileM PortBundle := do
  
  withTraceNode `HouLean.Apex.compiler (fun r => return m!"[{ExceptToEmoji.toEmoji r}] compiling expression\n{e}") do
  let e ← withConfig (fun cfg => {cfg with iota := false, zeta := false}) <| whnfI e

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
    let .lit (.natVal m) ← whnf m | throwError s!"Invalid literal for mantissa: {m}"
    let .lit (.natVal e) ← whnf e | throwError s!"Invalid literal for exponent: {e}"
    let s := if ← isDefEq s q(true) then true else false
    addFloat (Float.ofScientific m s e)
  | .const _ _ =>
    -- hard code Bool.true/Bool.false
    if (← isDefEq e q(true)) then
      return ← addBool true
    if (← isDefEq e q(false)) then
      return ← addBool false

    throwError m!"Cannot compile constant: {e}"
  | .sort _ => 
    throwError m!"Cannot compile sort: {e}"
  | .mvar _ => 
    let e ← instantiateMVars e
    if e.isMVar then 
      throwError m!"Cannot compile metavariable: {e}"
    toApexGraph (← instantiateMVars e)
  | .app .. =>
    let (fn, args) := e.withApp fun fn args => (fn, args)

    -- blast match statement
    -- todo: add let binding ...
    if ← isMatcherApp e then
      let body := e.appArg!
      let x := e.appFn!.appArg!
      trace[HouLean.Apex.compiler] m!"match expression for {x}"
      if x.isFVar then
        trace[HouLean.Apex.compiler] m!"fvar case, reducing match"
        return ← toApexGraph (← whnf e)
      else
        trace[HouLean.Apex.compiler] m!"nontrivial match"
        let fn := e.appFn!.appFn!
        return ← toApexGraph (Expr.letE `r (← inferType x) x ((fn.app (.bvar 0)).app body) false)
        

    -- implemented_by -- so something more clever here
    let s := compilerExt.getState (← getEnv)
    if let some fn' := s.implementedBy[fn]? then
      return ← toApexGraph (fn'.beta args)

    let .const fname _ := fn 
      | throwError m!"Unexpected function application: {e}"

    if (compilerExt.getState (← getEnv)).toUnfold.contains fname then
      let r ← Meta.unfold e fname
      return ← toApexGraph r.expr

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
      trace[HouLean.Apex.compiler] m!"projection function {info.ctorName}"
      let x := args[args.size - 1]!
      let x ← toApexGraph x
      let some si := x.child? info.i
        | throwError m!"Invalid projection {info.i} of {args[args.size - 1]!}: {x.toString}"
      return si

    -- Special case: for-loop
    if fname = ``Std.Range.forIn'.loop then
      trace[HouLean.Apex.compiler] m!"compiling for loop"
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

      trace[HouLean.Apex.compiler] m!"iterations"
      let iterations ← toApexGraph (range.getArg! 1)
      trace[HouLean.Apex.compiler] m!"inner loop"
      let (loopIns, loopOut) ← functionToApexGraph args[4]!
      trace[HouLean.Apex.compiler] m!"initial state"
      let input ← toApexGraph args[5]!

      let loopIndexIn := loopIns[0]!
      let loopIn := loopIns[2]!

      let stateShape ← input.mapIdxM (fun _ p => do 
        let p ← readGraph (fun g => pure g.ports[p]!)
        match p.type with
        | .builtin typeName => return (p.name, typeName)
        | _ => throwError m!"Invalid state {args[5]!} : {← inferType args[5]!} in for loop")
      
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
    
    -- Find variadic arguments
    let mut subPorts : Array AddSubPortSpec := #[]
    for arg in args, i in [0:args.size] do
      let type ← inferType arg
      if type.isAppOfArity' ``Vector 2 then
        let id := fnInputs[i]!.localId
        let namesAndTypes ← argPorts[i]!.flatten.mapM (fun pId => do 
          let n ← getPortName pId
          let t ← getPortType pId
          let t := t.builtin!
          return (n,t))
        subPorts := subPorts.push ⟨id, none, namesAndTypes⟩
      
    let (_, inputBundle, outputBundle) ← addNode nodeType subPorts userName?
    let _ ← makeConnections argPorts inputBundle
    return outputBundle
    
  | .letE n t v b _ => 
    if t.isForall then
      return ← toApexGraph (b.instantiate1 v)
    withLocalDeclD n t fun x => do
      let letOutput ← toApexGraph v
      modify (fun s => {s with fvarToPort := s.fvarToPort.insert x.fvarId! letOutput})
      let b := b.instantiate1 x
      toApexGraph b

  | .proj s i x => 
    trace[HouLean.Apex.compiler] m!"projection {i}"
    let t ← inferType x
    if ← isStructureType t then
      let x' := x
      let x ← toApexGraph x
      let some si := x.child? i
        | throwError m!"Invalid projection {i} of {x'}: {x.toString}"
      return si
    else
      let info := getStructureInfo (← getEnv) s
      -- revert to projection fn application
      let some fn := info.getProjFn? i
        | throwError m!"Invalid projection {i} for {t}"
      let fn := (← mkAppM fn #[x]).appFn!
      let s := compilerExt.getState (← getEnv)
      if let some fn' := s.implementedBy[fn]? then
        return ← toApexGraph (fn'.beta #[x])
      else
        throwError m!"Don't know how to compiler projection {e}"

  | _ => 
    throwError m!"Cannot compile expression: {e}. Constructor {e.ctorName}"

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


def programToApexGraph (e : Expr) : MetaM ApexGraph := do
  return (← go default).2.graph
where 
  go : GraphCompileM Unit := do
    let (inputs, output) ← functionToApexGraph e

    let inputs := inputs.map (·.flatten) |>.flatten
    let output := output.flatten

    modifyGraph (fun g => pure ({g with 
      inputPorts := inputs
      outputPorts := output},()))

end HouLean.Apex.Compiler
