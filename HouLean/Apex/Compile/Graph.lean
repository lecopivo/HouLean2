import HouLean.Apex.Compile.NodeType
import HouLean.Apex.Generated.Nodes

open Lean Meta Std

namespace HouLean.Apex.Compiler

inductive LiteralVal
  | int (val : Int)
  | float (val : Float)
  | str (str : String)
  | bool (str : Bool)
deriving Inhabited, Repr

def LiteralVal.toString : LiteralVal → String
  | .int val => s!"{val}"
  | .float val => s!"{val}"
  | .str val => val.quote
  | .bool val => s!"{val}".capitalize -- designed to be consumable by Python

inductive OutputConnection
  | port  (globalId : Nat)
  | input (id : Nat)
  | literal (val : LiteralVal)

structure ApexGraph where
  nodes : Array Node
  /-- ports[i] = (node id, local port id) -/
  ports : Array Port
  /-- Wire connecting output port and input port -/
  wires : Array (PortId × PortId)
  /-- Stores information saying which ports must be set to which value. -/
  literals : Array (LiteralVal × PortId)
  /-- Each input port corresponds to multiple ports. 

  When connection to an input of a graph one should connect to all the input ports. 

  There are more like temporary connections that are still missing the source port. -/
  inputs : Array (LocalPort × Array PortId)
  /-- output is either a port or directly fething an input. 

  There are more like temporary connections that are still missing target port. -/
  outputs : Array (LocalPort × OutputConnection)
deriving Inhabited


def ApexGraph.printPort (g : ApexGraph) (p : PortId) : String := 
  s!"/{g.nodes[g.ports[p]!.nodeId]!.name}/{g.ports[p]!.name}[{if g.ports[p]!.dir == .input then "in" else "out"}]"

instance : ToString ApexGraph := ⟨fun g => 
  Id.run do
    let mut s := "Nodes:\n"
    for n in g.nodes, i in [0:g.nodes.size] do
      s := s ++ s!"  {i}: {n.name} : {n.type.name}\n"

    s := s ++ "\nPorts:\n"
    for i in [0:g.ports.size] do
      s := s ++ s!"  {i}: {g.printPort i}\n"

    if g.inputs.size != 0 then
      s := s ++ "\nInputs:\n"
      for (t,is) in g.inputs do
        s := s ++ s!"  {t.name}[in] -> {is.map (g.printPort ·)}\n"

    if g.outputs.size != 0 then
      s := s ++ "\nOutputs:\n"
      for (t,i) in g.outputs do
        match i with
        | .port portId =>
          s := s ++ s!"  {g.printPort portId} -> {t.name}[out]\n"
        | .input inputId =>
          let t' := g.inputs[inputId]!.1
          s := s ++ s!"  {t'.name}[in] -> {t.name}[out]\n"
        | .literal val =>
          s := s ++ s!"  lit[{val.toString}] -> {t.name}[out]\n"

    s := s ++ "\nWires:\n"
    for (src, trg) in g.wires, i in Array.range g.wires.size do
      s := s ++ s!"  {i}: {g.printPort src} -> {g.printPort trg}\n"

    s := s ++ "\nLiterals:\n"
    for (val, nodeId) in g.literals, i in Array.range g.literals.size do
      match val with
      | .int val => 
        s := s ++ s!"  {i}: int {val} -> {nodeId} \n"
      | .float val => 
        s := s ++ s!"  {i}: float {val} -> {nodeId} \n"
      | .str str =>
        s := s ++ s!"  {i}: str \"{str}\" -> {nodeId} \n"
      | .bool b =>
        s := s ++ s!"  {i}: bool \"{b}\" -> {nodeId} \n"
    return s⟩

-- def ApexGraph.addInt (g : ApexGraph) (val : Int) (port : Port) : ApexGraph :=
--   {g with literals := g.literals.push (.int val, port.globalId)}

-- def ApexGraph.addFloat (g : ApexGraph) (val : Float) (port : Port) : ApexGraph :=
--   {g with literals := g.literals.push (.float val, port.globalId)}

-- def ApexGraph.addBool (g : ApexGraph) (val : Bool) (port : Port) : ApexGraph :=
--   {g with literals := g.literals.push (.bool val, port.globalId)}

-- def ApexGraph.addString (g : ApexGraph) (val : String) (port : Port) : ApexGraph :=
--   {g with literals := g.literals.push (.str val, port.globalId)}


/-- Port pointer, this could point to:

  - an existing port on a node
  - virtual input/output ports
  - hold a literal value

Connecting to:
  - existing port will make a connection
  - connecting input port to a literal value will set that port to that value
    connecting output port to a literal value will produce an error
  - connecting input/output will just add the port to inputs/outputs
-/
inductive PortPtr where
  /-- Connecting an input port to literal value will set that port to the literal value -/
  | literal (val : LiteralVal)
  /-- Global id of a port -/
  | port (id : Nat)
  /-- Global id of a port -/
  | subport (id : Nat) (localId : Nat)
  /-- Input port -/
  | input (id : Nat)
  /-- Output port -/
  | output (name : Name)
deriving Inhabited, Repr

protected def PortPtr.toString : PortPtr → String
  | literal (.int val)
  | literal (.float val)
  | literal (.str val)
  | literal (.bool val) => s!"lit[{val}]"
  | port id => toString id
  | subport id localId => s!"{id}[{localId}]"
  | input id => s!"in[{id}]"
  | output id => s!"out[{id}]"

instance : ToString PortPtr := ⟨PortPtr.toString⟩


def ApexGraph.addInput (g : ApexGraph) (name : Name) (type : PortType) : (ApexGraph×Nat) :=
  let id := g.inputs.size
  let port : LocalPort := {
    localId := g.inputs.size
    name := name
    type := type
    dir := .output
  }
  ({g with inputs := g.inputs.push (port,#[])}, id)
  

abbrev PortBundle := ArrayTree PortPtr

partial def arrayToProdBundle (ps : Array PortBundle) : PortBundle :=
  if ps.size = 0 then
    .node #[]
  else if ps.size = 1 then
    ps[0]!
  else
    .node #[ps[0]!, arrayToProdBundle ps[1:].toArray]

/-- Add a node to the graph with optional subports -/
def ApexGraph.addNode (g : ApexGraph) (nodeType : NodeType) (nodeName : String) : 
    struct { graph : ApexGraph, nodeId : Nat, inputs : Array PortBundle, output : PortBundle} := Id.run do
  let nodeOff := g.nodes.size
  let mut portOff := g.ports.size

  let inputs : Array (ArrayTree Port) := 
    nodeType.inputs.map (fun input =>
    input.map (fun localPort => 
      {localPort with 
        globalId := localPort.localId + portOff
        nodeId := nodeOff}))

  let output : (ArrayTree Port) := 
    nodeType.output.map (fun localPort => 
      {localPort with 
        globalId := localPort.localId + portOff
        nodeId := nodeOff})
  
  let mut ports : Array Port := #[]
  if nodeType.hasRunData then
    ports := ports.push { 
      localId := 0, globalId := portOff, 
      name := .anonymous, type := .rundata, 
      dir := .output, nodeId := nodeOff}
  ports := ports ++ (inputs.map (·.flatten)).flatten ++ output.flatten

  let node : Node := {
    name := nodeName
    type := nodeType
    globalId := nodeOff
    inputs := inputs.map (fun input => input.map (fun p => p.globalId))
    output := output.map (fun p => p.globalId)
  }

  let g := { g with
    nodes := g.nodes.push node
    ports := g.ports ++ ports
  }

  return {
    graph := g
    nodeId := nodeOff
    inputs := inputs.map (fun input => input.map (fun p => .port p.globalId))
    output := output.map (fun p => .port p.globalId)
  }

def ApexGraph.addValueNode (g : ApexGraph) (t : ApexTypeTag) : ApexGraph × PortId × PortId :=
  let inputPort : LocalPort := {
    localId := 0
    name := `parm
    type := .builtin t
    dir := .input
  }
  let outputPort : LocalPort := {
    localId := 1
    name := `value
    type := .builtin t
    dir := .output
  }
  let type : NodeType := {
    name := s!"Value<{t.toString}>"
    leanDecl := Name.appendAfter `HouLean.Apex.Generated.Value t.toString
    hasRunData := false
    inputs := #[.leaf inputPort]
    output := .leaf outputPort
  }
  if let ⟨g,_,#[.leaf (.port inputId)], .leaf (.port outputId)⟩ := g.addNode type "value" then
    (g, inputId, outputId)
  else
    panic! s!"APEX compiler bug in {decl_name%}, something went wrong adding value node!"


def LiteralVal.typeTag (val : LiteralVal) : ApexTypeTag := 
  match val with
  | .float .. => .float
  | .int .. => .int
  | .bool .. => .bool
  | .str .. => .string

partial def ApexGraph.addConnection (g : ApexGraph) (src trg : PortPtr) : Except String ApexGraph := do
  match src, trg with
  | .port src, .port trg =>
    return { g with wires := g.wires.push (src, trg) }

  | .literal val, .port trg =>
    unless g.ports[trg]!.dir == .input do
      throw s!"Assigning literal value to output port!\n{repr src} -> {repr trg}"

    let trgPort := g.ports[trg]!
    if ¬trgPort.isVariadic then
      return { g with literals := g.literals.push (val, trg) }
    else
      let (g,valueIn, valueOut) := g.addValueNode val.typeTag
      let g ← g.addConnection (.literal val) (.port valueIn)
      let g ← g.addConnection (.port valueOut) (.port trg)
      return g

  | .port src, .output name =>
    unless g.ports[src]!.dir == .output do
      throw s!"Graph output can be connected only to output port!\n{repr src} -> {repr trg}"

    let port := { g.ports[src]!.toLocalPort with 
      dir := .input
      localId := g.outputs.size
      name := name
    }     
    return {g with outputs := g.outputs.push (port, .port src)}

  | .literal val, .output name =>
    let port : LocalPort := default
    let port := {port with
      localId := g.outputs.size
      name := name
      type := .builtin (match val with 
        | .float .. => .float 
        | .int .. => .int
        | .bool .. => .bool
        | .str .. => .string)
      dir := .input
    }
    return {g with outputs := g.outputs.push ({port with name := name}, .literal val)}

  | .input id, trg =>    
    unless id < g.inputs.size do
      throw s!"Connecting to non-existent input port!\n{repr src} -> {repr trg}"
    match trg with
    | .port trg => 
      unless g.ports[trg]!.dir == .input do
        throw s!"Graph input can be connected only to input port!\n{repr src} -> {repr trg}"

      return {g with inputs := g.inputs.modify id (fun (t,ports) => (t, ports.push trg))}
    | .output name =>
      let port := { g.inputs[id]!.1 with 
        dir := .input
        localId := g.outputs.size
        name := name
      }     
      return {g with outputs := g.outputs.push (port, .input id)}
    | _ => 
      throw s!"Trying to make an invalid connection {repr src} -> {repr trg}" 
  | _, _ =>
    throw s!"Trying to make an invalid connection {repr src} -> {repr trg}"

-- def ApexGraph.addConnections (g : ApexGraph) (src trg : ArrayTree PortId) : ApexGraph :=
--   let wires := src.flatten.zip trg.flatten
--   { g with wires := g.wires ++ wires }


-- structure ForBeginPorts where
--   iterations : PortBundle
--   scope : PortBundle
--   index : PortBundle
--   stateIn  : PortBundle
--   stateOut : PortBundle
-- deriving Inhabited

-- def ApexGraph.addForBegin (g : ApexGraph) (stateType : ApexStaticType) : ApexGraph × ForBeginPorts :=
--   Id.run do

--   let localPorts : Array LocalPort := #[
--     { localId := 0, name :=    "rundata", type :=       .rundata, dir := .output},
--     { localId := 1, name := "iterations", type := .builtin "Int", dir := .input},
--     { localId := 2, name :=  "__spare__", type :=     .undefined, dir := .input},
--     { localId := 3, name :=      "scope", type :=     .undefined, dir := .output},
--     { localId := 4, name :=      "index", type := .builtin "Int", dir := .output},
--     { localId := 5, name :=  "__spare__", type :=     .undefined, dir := .output}
--   ]

--   let nodeType : NodeType := {
--     name := "ForBegin"
--     ports := localPorts
--   }

--   let nodeOff := g.nodes.size
--   let portOff := g.ports.size

--   let ports := localPorts.map (fun p => 
--     {p with globalId := portOff + p.localId, nodeId := nodeOff : Port})

--   let stateIn := stateType.mapIdx fun i (name,typeName) => 
--     { localId  := 2*i + localPorts.size, 
--       globalId := 2*i + localPorts.size + portOff
--       nodeId := nodeOff
--       name := name
--       type := .builtin typeName
--       dir := .input : Port}

--   let stateOut := stateType.mapIdx fun i (name,typeName) => 
--     { localId  := 2*i + 1 + localPorts.size, 
--       globalId := 2*i + 1 + localPorts.size + portOff
--       nodeId := nodeOff
--       name := name
--       type := .builtin typeName
--       dir := .output : Port}

--   let statePorts := stateIn.flatten.zip stateOut.flatten |>.map (fun (x,y) => #[x,y]) |>.flatten
--   let ports := ports ++ statePorts
  
--   let node : Node := {
--     type := nodeType
--     globalId := nodeOff
--     name := "forbegin"
--     ports := ports.map (fun p => p.globalId)
--     subPorts := #[{
--       inputPortId  := some (2 + portOff)
--       outputPortId := some (5 + portOff)
--       subports := stateIn.flatten.map (fun p => (p.name, p.type.builtin!))
--       }]
--   }

--   let g := {g with
--     nodes := g.nodes.push node
--     ports := g.ports ++ ports
--   }

--   let r := {
--     iterations := .leaf ports[1]!.globalId
--     scope := .leaf ports[3]!.globalId
--     index := .leaf ports[4]!.globalId
--     stateIn := stateIn.mapIdx (fun _ p => p.globalId)
--     stateOut := stateOut.mapIdx (fun _ p => p.globalId)
--   }
  
--   return (g, r)

-- structure ForEndPorts where
--   scope : PortBundle
--   stateIn  : PortBundle
--   stateOut : PortBundle
-- deriving Inhabited


-- def ApexGraph.addForEnd (g : ApexGraph) (stateType : ApexStaticType) : 
--     ApexGraph × ForEndPorts :=
--   Id.run do

--   let localPorts : Array LocalPort := #[
--     { localId := 0, name :=   "rundata", type :=       .rundata, dir := .output},
--     { localId := 1, name :=     "scope", type := .builtin "Int", dir := .input},
--     { localId := 2, name := "__spare__", type :=     .undefined, dir := .input},
--     { localId := 3, name := "__spare__", type :=     .undefined, dir := .output}
--   ]

--   let nodeType : NodeType := {
--     name := "ForEnd"
--     ports := localPorts
--   }

--   let nodeOff := g.nodes.size
--   let portOff := g.ports.size

--   let ports := localPorts.map (fun p => 
--     {p with globalId := portOff + p.localId, nodeId := nodeOff : Port})

--   let stateIn := stateType.mapIdx fun i (name,typeName) => 
--     { localId  := 2*i + localPorts.size, 
--       globalId := 2*i + localPorts.size + portOff
--       nodeId := nodeOff
--       name := name
--       type := .builtin typeName
--       dir := .input : Port}

--   let stateOut := stateType.mapIdx fun i (name,typeName) => 
--     { localId  := 2*i + 1 + localPorts.size, 
--       globalId := 2*i + 1 + localPorts.size + portOff
--       nodeId := nodeOff
--       name := name
--       type := .builtin typeName
--       dir := .output : Port}

--   let statePorts := stateIn.flatten.zip stateOut.flatten |>.map (fun (x,y) => #[x,y]) |>.flatten
--   let ports := ports ++ statePorts
  
--   let node : Node := {
--     type := nodeType
--     globalId := nodeOff
--     name := "forend"
--     ports := ports.map (fun p => p.globalId)
--     subPorts := #[{
--       inputPortId  := some (2 + portOff)
--       outputPortId := some (3 + portOff)
--       subports := stateIn.flatten.map (fun p => (p.name,p.type.builtin!))
--       }]
--   }

--   let g := {g with
--     nodes := g.nodes.push node
--     ports := g.ports ++ ports
--   }

--   let r := {
--     scope := .leaf ports[1]!.globalId
--     stateIn := stateIn.mapIdx (fun _ p => p.globalId)
--     stateOut := stateOut.mapIdx (fun _ p => p.globalId)
--   }
  
--   return (g, r)

-- todo: implement this!
-- should we have port index or node index. Can I have two subports with the same name
-- but of different ports on a single node? 

def formatStrName (name : String) : String := name.replace "." "_"
def formatName (name : Name) : String := formatStrName (toString name.eraseMacroScopes)

def ensureUniqueName (_nodeId : Nat) (_dir : PortDir) (name : Name) : String := formatName name
  
def ApexGraph.pythonBuildScript (g : ApexGraph) : String := 
Id.run do
  let mut s := ""

  s := s ++ s!"import apex" ++ "\n"
  s := s ++ s!"geo = hou.pwd().geometry()" ++ "\n"
  s := s ++ s!"graph = apex.Graph()" ++ "\n"

  s := s ++ "\n# Adding Nodes" ++ "\n"
  for n in g.nodes, i in [0:g.nodes.size] do
    s := s ++ s!"n{i} = graph.addNode(\"{formatStrName n.name}\", \"{n.type.name}\")" ++ "\n"

  s := s ++ "\n# Add Wires" ++ "\n"
  for (src, trg) in g.wires, wireId in [0:g.wires.size] do
    let srcPort := g.ports[src]!
    let trgPort := g.ports[trg]!
    let srcNode := g.nodes[srcPort.nodeId]!
    let trgNode := g.nodes[trgPort.nodeId]!
    match srcPort.isVariadic, trgPort.isVariadic with
    | false, false =>
      s := s ++ s!"graph.addWire({src}, {trg}) # {srcNode.name}[{srcPort.name}] -> {trgNode.name}[{trgPort.name}]" ++ "\n"
    | false, true =>
      let trgPortName := ensureUniqueName (g.nodes.size) .input (srcPort.name.appendAfter (toString wireId))
      s := s ++ s!"trgId = graph.addSubPort({trg}, \"{trgPortName}\")" ++ "\n" 
      s := s ++ s!"graph.addWire({src}, trgId)" ++ "\n" 
    | true, false =>
      let srcPortName := ensureUniqueName (g.nodes.size) .output (trgPort.name.appendAfter (toString wireId))
      s := s ++ s!"srcId = graph.addSubPort({src}, \"{srcPortName}\")" ++ "\n" 
      s := s ++ s!"graph.addWire(srcId, {trg})" ++ "\n" 
    | _, _ => 
      pure ()

  s := s ++ "\n# Add Inputs and Outputs" ++ "\n"
  if g.inputs.size != 0 then
    s := s ++ s!"inputNodeId = graph.addNode(\"input\", \"__parms__\")" ++ "\n"
    s := s ++ s!"inputs = []" ++ "\n"
    for (port, inputs) in g.inputs do
      let inputPortName := ensureUniqueName (g.nodes.size) .output port.name
      s := s ++ s!"inputPortId = graph.addGraphInput(inputNodeId, \"{inputPortName}\")" ++ "\n"
      s := s ++ s!"inputs.append(inputPortId)" ++ "\n"
      for inputId in inputs do
        let inputPort := g.ports[inputId]!
        if ¬inputPort.isVariadic then
          s := s ++ s!"graph.addWire(inputPortId, {inputId})" ++ "\n"    
        else
          let subPortName := ensureUniqueName (inputPort.nodeId) .input (Name.mkSimple (inputPortName ++ toString inputId))
          s := s ++ s!"id = graph.addSubPort({inputId}, \"{subPortName}\")" ++ "\n" 
          s := s ++ s!"graph.addWire(inputPortId, id)" ++ "\n" 

  s := s ++ "\n"    

  if g.outputs.size != 0 then
    s := s ++ s!"outputNodeId = graph.addNode(\"output\", \"__output__\")" ++ "\n"
    for (port,output) in g.outputs do
      let name := ensureUniqueName (g.nodes.size) .input port.name
      s := s ++ s!"outputPortId = graph.addGraphOutput(outputNodeId, \"{name}\")" ++ "\n"
      match output with
      | .port portId =>
        s := s ++ s!"graph.addWire({portId}, outputPortId)" ++ "\n"    
      | .input inputId =>
        s := s ++ s!"graph.addWire(inputs[{inputId}], outputPortId)" ++ "\n"    
      | .literal _val =>
        pure () -- maybe drop down a value node?

  s := s ++ "\n# Set Literal Values" ++ "\n"
  for (val, portId) in g.literals do
    let p := g.ports[portId]!
    -- nodes are index from 1-in python API
    s := s ++ s!"graph.setNodeParm({p.nodeId+1}, \"{p.name}\", {val.toString})" ++ "\n"

  s := s ++ "\n# Layout and Save Graph" ++ "\n"
  s := s ++ "graph.layout()" ++ "\n"
  s := s ++ "graph.saveToGeometry(geo)" ++ "\n"
  return s

