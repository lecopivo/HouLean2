import HouLean.Apex.Compile.Graph

open Lean Meta Std

namespace HouLean.Apex.Compiler

structure Context where
  -- recDepth : Nat
  -- maxRecDepth : Nat := 100
  unfoldStack : List Name -- todo: store recursion depth too and probably use it to report errors
deriving Inhabited

structure GraphState where
  /-- Currently built graph -/
  graph : ApexGraph
  /-- Mapping from local fvars to ports -/
  fvarToPort : HashMap FVarId (ArrayTree PortId)
  /-- Counts how many times we added a node with particular name -/
  nameCounter : HashMap String Nat
  /-- Cache output ports for expressions -/
  cache : ExprMap (Array PortBundle × PortBundle)
deriving Inhabited

abbrev GraphCompileM := ReaderT Context <| StateT GraphState MetaM

-- def withIncRecDepth {α} (go : GraphCompileM α) : GraphCompileM α := do
--   throwMaxRecDepthAt (← getRef)


def withUnfolded {α} (name : Name) (go : GraphCompileM α) : GraphCompileM α := do
--   Meta.withIncRecDepth do
--     sorry
  fun ctx => go {ctx with unfoldStack := name :: ctx.unfoldStack}

def GraphCompileM.run {α} (go : GraphCompileM α) 
    (c : Context := default) (s : GraphState := default) : MetaM α := do
  return (← go c s).1

def GraphCompileM.run' {α} (go : GraphCompileM α) (c : Context := default) 
    (s : GraphState := default) : MetaM ApexGraph := do
  return (← go c s).2.graph

def modifyGraph {α} (f : ApexGraph → MetaM (ApexGraph × α)) : GraphCompileM α := do
  let s ← get
  let (g, val) ← f s.graph
  set {s with graph := g}
  return val

def readGraph {α} (f : ApexGraph → MetaM α) : GraphCompileM α := do
  return (← f (← get).graph)

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

def getUnfoldStackMsg : GraphCompileM MessageData := do
  let stack := (← read).unfoldStack
  if stack.isEmpty then
    return m!""
  else
    return m!", unfolded: {stack.head!} → ... → {stack.getLast!}"

def throwErrorWithStack (msg : MessageData) : GraphCompileM α := do
  throwErrorAt (← getRef) m!"{msg}{← getUnfoldStackMsg}"

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
    
    let g := { g with
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
    | throwErrorWithStack m!"Failed to add Value<Int> node"
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
    | throwErrorWithStack m!"Failed to add Value<Float> node"
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
    | throwErrorWithStack m!"Failed to add Value<String> node"
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
    | throwErrorWithStack m!"Failed to add Value<Bool> node"
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

/-- Add identity node for a given type, returns input and output port bundle -/
def addIdentity (type : Expr) (userName? : Option String := none) : 
    GraphCompileM (PortBundle × PortBundle) := do
  let some apexType ← getApexType? type
    | throwErrorWithStack m!"Cannot get APEX type for {type}"

  match ← enforceStaticSize apexType with
  | .error n => 
    throwErrorWithStack m!"Cannot determine static size of {type}, the number {n} should be a compile time constant"
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

instance : ExceptToEmoji Exception (Array PortBundle × PortBundle) where
  toEmoji r :=
    match r with
    | .ok p => 
      if p.1.size = 0 then
        s!"✅️ {p.2.toString}"
      else
        s!"✅️ {p.1.map (·.toString)} -> {p.2.toString}"
    | .error _ => "💥️"

private def _root_.Lean.isInstanceProjection (name : Name) : MetaM Bool := do
  unless ← isProjectionFn name do return false
  let some info ← getProjectionFnInfo? name | return false
  unless info.fromClass do return false
  return true


/-- If `e` is a match expression then all discriminants are turned into a let bindings
if they are not free variable already.

For example the expression
```
let (x,y) := f x
x + y
```
gets elaborated to
```
match f x with
| (x,y) => x + y
```
and running `whnf` with `iota := true` and `zeta := false` turns it into
```
(f x).1 + (f x).2
```
which is undesirable. Therefore this function turns
```
match f x with
| (x,y) => x + y
```
into
```
let r := f x
match r with
| (x,y) => x + y
```
and now running whnf on the let body will produce
```
let r := f x
r.1 + r.2
```
which preserves the let binding of the original expression

 -/
partial def letBindMatchDiscrs (e : Expr) (doUnfold := false) : MetaM Expr := do
  let some info := isMatcherAppCore? (← getEnv) e 
    | return e
  let (fn, args) := e.withApp (fun fn args => (fn,args))
  go fn args info 0 #[]
where
  go (fn : Expr) (args : Array Expr) (info : MatcherInfo) (i : Nat) (xs : Array Expr) : MetaM Expr := do
    if i = info.numDiscrs then
      let mut e := fn.beta args
      if doUnfold then
        -- todo: maybe check that the proof of unfolding is rfl as we use it int whnf
        e := (← unfold e fn.constName!).expr
      return ← mkLambdaFVars xs e
    else
      let xi := args[i + info.numParams + 1]!
      if xi.isFVar then
        -- do nothing if `xi` is already fvar
        go fn args info (i+1) xs
      else
        let name := Name.appendAfter `r (toString i)
        withLetDecl name (← inferType xi) xi fun xvar => do
          let args := args.set! (i + info.numParams + 1) xvar
          let xs := xs.push xvar
          go fn args info (i+1) xs

def doUnfold (e : Expr) : MetaM Bool := do
  let .const name _ := e.getAppFn | return false
  if ← isReducible name then return true
  unless ← isInstanceProjection name do return false
  let s := compilerExt.getState (← getEnv)
  if s.implementedByName.contains name then
    return false
  else
    return true

/-- Specially configured `whnf` for APEX compilation. 

At its core it is whnf at reducible and instances with `iota := true` and `zeta := false`

But certain instance projections are overriden by `apex_implemented_by` so those do not 
get reduced. Also match statements hoist their discriminants into let binders before
reducing them.
-/
partial def whnfC (e : Expr) : MetaM Expr :=
  withConfig (fun cfg => {cfg with iota := false, zeta := true}) do
    let e' ← whnfHeadPred e doUnfold
    let e' ← letBindMatchDiscrs e' true
    if e.equal e' then
      return e'
    else
      whnfC e'

open Qq in
mutual

partial def compile (e : Expr) : GraphCompileM (Array PortBundle × PortBundle) := do
  let s ← get
  if let some output := s.cache[e]? then
    return output
  else
    let output ← toApexGraph e
    modify (fun s => {s with cache := s.cache.insert e output})
    return output

/-- Compile a free variable reference -/
partial def compileVariable (id : FVarId) (e : Expr) : 
    GraphCompileM (Array PortBundle × PortBundle) := do
  let some portBundle := (← get).fvarToPort[id]?
    | throwErrorWithStack m!"Unrecognized free variable: {e}"
  return (#[], portBundle)

/-- Compile literal values (Nat, String) -/
partial def compileLiteral (lit : Literal) : 
    GraphCompileM (Array PortBundle × PortBundle) := do
  match lit with
  | .natVal n => return (#[], ← addInt n)
  | .strVal s => return (#[], ← addString s)

/-- Compile Float literals -/
partial def compileFloat (m s e : Expr) : 
    GraphCompileM (Array PortBundle × PortBundle) := do
  let .lit (.natVal m) ← whnf m 
    | throwErrorWithStack s!"Invalid literal for mantissa: {m}"
  let .lit (.natVal e) ← whnf e 
    | throwErrorWithStack s!"Invalid literal for exponent: {e}"
  let s := if ← isDefEq s q(true) then true else false
  return (#[], ← addFloat (Float.ofScientific m s e))

/-- Compile Bool constants -/
partial def compileConstant (e : Expr) : 
    GraphCompileM (Array PortBundle × PortBundle) := do
  if (← isDefEq e q(true)) then
    return (#[], ← addBool true)
  if (← isDefEq e q(false)) then
    return (#[], ← addBool false)
  throwErrorWithStack m!"Cannot compile constant: {e}"

/-- Compile match expressions -/
partial def compileMatchExpr (e : Expr) (info : MatcherInfo) : 
    GraphCompileM (Array PortBundle × PortBundle) := do
  let body := e.appArg!
  let x := e.appFn!.appArg!
  
  trace[HouLean.Apex.compiler] "Compiling match on {x}"
  
  if x.isFVar then
    trace[HouLean.Apex.compiler] "  → fvar case, reducing match"
    compile (← whnf e)
  else
    let matchFn := e.getAppFn.constName!
    trace[HouLean.Apex.compiler] "  → nontrivial match"
    if info.numParams = 0 ∧ info.numDiscrs = 1 ∧ info.altNumParams.size = 1 then
      let fn := e.appFn!.appFn!
      compile (Expr.letE `r (← inferType x) x ((fn.app (.bvar 0)).app body) false)
    else
      let e' ← whnf (← unfold e matchFn).expr
      withUnfolded matchFn do
        unless e != e' do
          throwErrorWithStack m!"can't compile {e}"
        trace[HouLean.Apex.compiler] "reduced to: {e'}"
        compile e'

/-- Try to compile using implemented_by override -/
partial def tryCompileImplementedBy (fn : Expr) (args : Array Expr) : 
    GraphCompileM (Option (Array PortBundle × PortBundle)) := do

  let s := compilerExt.getState (← getEnv)

  -- Direct expr level overrides
  if let some fn' := s.implementedByExpr[fn]? then
    trace[HouLean.Apex.compiler] "Using implemented_by override for {fn} -> {fn'}"
    return some (← compile (fn'.beta args))
  
  -- Name based overrides
  if let (.const fn _) := fn then
    if let some (fn', argMap) := s.implementedByName.find? fn then
      trace[HouLean.Apex.compiler] "Using implemented_by override for {fn} -> {fn'}"
      let args' := argMap.map (fun i? => i?.map (fun i => args[i]!))
      let e' ← do
        try 
          let e' ← mkAppOptM fn' args'
          pure e'
        catch err =>
          throwErrorWithStack m!"Failed to apply implemented_by override {fn} -> {fn'}\n{err.toMessageData}"
      return ← compile e'


  return none

/-- Try to compile variadic argument -/
partial def tryCompileVariadic (e : Expr) : 
    GraphCompileM (Option (Array PortBundle × PortBundle)) := do
  if let some xs ← isVariadicArg e then
    let xs ← xs.mapM (fun x => do pure (← compile x).2)
    return some (#[], arrayToBundle xs)
  return none

/-- Compile constructor application -/
partial def compileConstructor (e : Expr) (info : ConstructorVal) (args : Array Expr) : 
    GraphCompileM (Array PortBundle × PortBundle) := do
  -- unless ← isStructureType (← inferType e) do
  --   throwErrorWithStack m!"Constructor must produce a structure type: {e}"
  
  let args := args[info.numParams:].toArray
  let args ← args.mapM (fun x => do pure (← compile x).2)
  return (#[], .node args)

/-- Compile projection function -/
partial def compileProjectionFn (fname : Name) (args : Array Expr) : 
    GraphCompileM (Array PortBundle × PortBundle) := do
  let some info ← getProjectionFnInfo? fname
    | throwErrorWithStack m!"Expected projection function: {fname}"
  
  trace[HouLean.Apex.compiler] "Compiling projection function {fname}"
  let x := args[args.size - 1]!
  let (_, x) ← compile x
  let some si := x.child? info.i
    | throwErrorWithStack m!"Invalid projection {info.i} of {args[args.size - 1]!}: {x.toString}"
  return (#[], si)

/-- Compile for-loop -/
partial def compileForLoop (e : Expr) (args : Array Expr) : 
    GraphCompileM (Array PortBundle × PortBundle) := do
  trace[HouLean.Apex.compiler] "Compiling for-loop"
  
  let monad := args[0]!
  let range ← whnf args[3]!

  unless ← isDefEq monad q(@Id : Type → Type) do
    throwErrorWithStack m!"Unsupported monad {monad} in for loop"

  unless range.isAppOfArity' ``Std.Range.mk 4 do
    throwErrorWithStack m!"Unsupported range {range} in for loop"

  let b ← whnf (range.getArg! 0)
  let s ← whnf (range.getArg! 2)
  unless ← isDefEq b q(0 : Nat) do
    throwErrorWithStack m!"Invalid range start: {b}"
  unless ← isDefEq s q(1 : Nat) do
    throwErrorWithStack m!"Invalid range step: {s}"

  let (_, iterations) ← compile (range.getArg! 1)
  let (loopIns, loopOut) ← compile args[4]!
  let (_, input) ← compile args[5]!

  let loopIndexIn := loopIns[0]!
  let loopIn := loopIns[2]!

  let stateShape ← input.mapIdxM (fun _ p => do 
    let p ← readGraph (fun g => pure g.ports[p]!)
    match p.type with
    | .builtin typeName => return (p.name, typeName)
    | _ => throwErrorWithStack m!"Invalid state {args[5]!} : {← inferType args[5]!} in for loop")
  
  let bp ← addForBegin stateShape
  let ep ← addForEnd stateShape

  let _ ← makeConnection input bp.stateIn
  let _ ← makeConnection iterations bp.iterations
  let _ ← makeConnection bp.scope ep.scope
  let _ ← makeConnection bp.index loopIndexIn
  let _ ← makeConnection bp.stateOut loopIn
  let _ ← makeConnection loopOut ep.stateIn

  return (#[], ep.stateOut)

/-- Compile normal function application -/
partial def compileNormalApp (fn : Expr) (args : Array Expr) (e : Expr) 
    (userName? : Option String) : 
    GraphCompileM (Array PortBundle × PortBundle) := do
  let .const fname _ := fn 
    | throwErrorWithStack m!"Expected constant function: {fn}"

  let args ← getExplicitArgs fn args
  let some nodeType ← getNodeType (.const fname []) 
    | let r ← unfold e fname
      if r.expr == e then
        throwErrorWithStack m!"No APEX node for {fname}"
      trace[HouLean.Apex.compiler] "Unfolding {fname}"
      withUnfolded fname do
        return ← compile r.expr

  let argPorts ← args.mapM (fun x => do pure (← compile x).2)
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
  return (#[], outputBundle)

/-- Compile function application -/
partial def compileApplication (e : Expr) (userName? : Option String) : 
    GraphCompileM (Array PortBundle × PortBundle) := do
  let (fn, args) := e.withApp fun fn args => (fn, args)

  -- Try implemented_by override
  if let some result ← tryCompileImplementedBy fn args then
    return result

  -- Handle match statements
  if let some info := isMatcherAppCore? (← getEnv) e then
    return ← compileMatchExpr e info

  let .const fname _ := fn 
    | throwErrorWithStack m!"Unexpected function application: {e}"

  -- Check if function should be unfolded
  if (compilerExt.getState (← getEnv)).toUnfold.contains fname then
    let r ← Meta.unfold e fname
    trace[HouLean.Apex.compiler] "Unfolding {fname}"
    return ← compile r.expr

  -- Try variadic argument
  if let some result ← tryCompileVariadic e then
    return result

  -- Handle constructor
  if let some (info, args) ← constructorApp? e then
    return ← compileConstructor e info args

  -- Handle projection function
  if let some _ ← getProjectionFnInfo? fname then
    return ← compileProjectionFn fname args

  -- Handle for-loop
  if fname = ``Std.Range.forIn'.loop then
    return ← compileForLoop e args

  -- Normal application
  compileNormalApp fn args e userName?

/-- Compile let expression -/
partial def compileLet (n : Name) (t : Expr) (v : Expr) (b : Expr) : 
    GraphCompileM (Array PortBundle × PortBundle) := do
  if t.isForall then
    return ← compile (b.instantiate1 v)
  
  withLocalDeclD n t fun x => do
    let (_, letOutput) ← compile v
    modify (fun s => {s with fvarToPort := s.fvarToPort.insert x.fvarId! letOutput})
    let b := b.instantiate1 x
    compile b

/-- Compile projection expression -/
partial def compileProjection (s : Name) (i : Nat) (x : Expr) : 
    GraphCompileM (Array PortBundle × PortBundle) := do
  trace[HouLean.Apex.compiler] "Compiling projection {s}.{i}"
  
  let t ← whnf (← inferType x)
  if ← isStructureType t then
    let x' := x
    let (_, x) ← compile x
    let some si := x.child? i
      | throwErrorWithStack m!"Invalid projection {i} of {x'}: {x.toString}"
    return (#[], si)
  else
    let info := getStructureInfo (← getEnv) s
    let some fn := info.getProjFn? i
      | throwErrorWithStack m!"Invalid projection {i} for {t}"
    let fn := (← mkAppM fn #[x]).appFn!
    
    -- Try implemented_by override
    if let some result ← tryCompileImplementedBy fn #[x] then
      return result
    else
      throwErrorWithStack m!"Don't know how to compile projection {fn}"

/-- Compile lambda expression -/
partial def compileLambda (e : Expr) : 
    GraphCompileM (Array PortBundle × PortBundle) := do
  forallTelescope (← inferType e) fun xs _ => do
    if xs.size = 0 then
      throwErrorWithStack m!"Function expected: {e}"

    let b ← whnfC (e.beta xs)

    let mut inputs : Array PortBundle := #[]
    for x in xs do
      let (input, _) ← addFVar x.fvarId!
      inputs := inputs.push input
    let (_,output) ← compile b

    return (inputs, output)

/-- Compile Lean expression to APEX graph -/
partial def toApexGraph (e : Expr) (userName? : Option String := none) : 
    GraphCompileM (Array PortBundle × PortBundle) := do
  Meta.withIncRecDepth do
  
  withTraceNode `HouLean.Apex.compiler 
      (fun r => return m!"[{ExceptToEmoji.toEmoji r}] {e}") do
    
    let e' ← withConfig (fun cfg => {cfg with iota := false, zeta := false}) <| whnfC e
    if e != e' then trace[HouLean.Apex.compiler] m!"reduced to: {e'}"
    let e := e'

    match e with
    | .bvar _ => 
      throwErrorWithStack m!"Cannot compile bound variable: {e}"
    | .fvar id => 
      compileVariable id e
    | .lit lit =>
      compileLiteral lit
    | mkApp3 (.const ``Float.ofScientific []) m s e => 
      compileFloat m s e
    | .const _ _ =>
      compileConstant e
    | .sort _ => 
      throwErrorWithStack m!"Cannot compile sort: {e}"
    | .mvar _ => 
      let e ← instantiateMVars e
      if e.isMVar then 
        throwErrorWithStack m!"Cannot compile metavariable: {e}"
      compile (← instantiateMVars e)
    | .app .. =>
      compileApplication e userName?
    | .letE n t v b _ => 
      compileLet n t v b
    | .proj s i x => 
      compileProjection s i x
    | .lam .. =>
      compileLambda e
    | _ => 
      throwErrorWithStack m!"Cannot compile expression: {e}. Constructor {e.ctorName}"

end

def programToApexGraph (e : Expr) : MetaM ApexGraph := do
  GraphCompileM.run' do
    let (inputs, output) ← compile e

    let inputs := inputs.map (·.flatten) |>.flatten
    let output := output.flatten

    modifyGraph (fun g => pure ({g with 
      inputPorts := inputs
      outputPorts := output},()))


open Elab Term Command in
/-- Print APEX graph for given Lean expression. -/
elab t:"#apex_graph" x:term : command => do
  liftTermElabM do
  let x ← elabTermAndSynthesize x none
  let g ← programToApexGraph x
  logInfoAt t s!"{g}"


end HouLean.Apex.Compiler
