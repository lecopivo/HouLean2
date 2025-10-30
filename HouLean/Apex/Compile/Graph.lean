import HouLean.Apex.Compile.NodeType

open Lean Meta Std

namespace HouLean.Apex.Compiler

inductive LiteralVal
  | int (val : Int)
  | float (val : Float)
  | str (str : String)
  | bool (str : Bool)
deriving Inhabited

def LiteralVal.toString : LiteralVal → String
  | .int val => s!"{val}"
  | .float val => s!"{val}"
  | .str val => val.quote
  | .bool val => s!"{val}"


structure ApexGraph where
  nodes : Array Node
  /-- ports[i] = (node id, local port id) -/
  ports : Array Port
  /-- Wire connecting output port and input port -/
  wires : Array (PortId × PortId)
  /-- Literals values which point to `Value<_>` node of appropriate type -/
  literals : Array (LiteralVal × PortId)
  inputPorts : Array PortId
  outputPorts : Array PortId
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

    if g.inputPorts.size != 0 then
      s := s ++ "\nInput Ports:\n"
      for i in g.inputPorts do
        s := s ++ s!"  {i}: {g.printPort i}\n"

    if g.outputPorts.size != 0 then
      s := s ++ "\nOutput Ports:\n"
      for i in g.outputPorts do
        s := s ++ s!"  {i}: {g.printPort i}\n"

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

def ApexGraph.addInt (g : ApexGraph) (val : Int) (port : Port) : ApexGraph :=
  {g with literals := g.literals.push (.int val, port.globalId)}

def ApexGraph.addFloat (g : ApexGraph) (val : Float) (port : Port) : ApexGraph :=
  {g with literals := g.literals.push (.float val, port.globalId)}

def ApexGraph.addBool (g : ApexGraph) (val : Bool) (port : Port) : ApexGraph :=
  {g with literals := g.literals.push (.bool val, port.globalId)}

def ApexGraph.addString (g : ApexGraph) (val : String) (port : Port) : ApexGraph :=
  {g with literals := g.literals.push (.str val, port.globalId)}

abbrev PortBundle := ArrayTree PortId

structure ForBeginPorts where
  iterations : PortBundle
  scope : PortBundle
  index : PortBundle
  stateIn  : PortBundle
  stateOut : PortBundle
deriving Inhabited

def ApexGraph.addForBegin (g : ApexGraph) (stateType : ApexStaticType) : ApexGraph × ForBeginPorts :=
  Id.run do

  let localPorts : Array LocalPort := #[
    { localId := 0, name :=    "rundata", type :=       .rundata, dir := .output},
    { localId := 1, name := "iterations", type := .builtin "Int", dir := .input},
    { localId := 2, name :=  "__spare__", type :=     .undefined, dir := .input},
    { localId := 3, name :=      "scope", type :=     .undefined, dir := .output},
    { localId := 4, name :=      "index", type := .builtin "Int", dir := .output},
    { localId := 5, name :=  "__spare__", type :=     .undefined, dir := .output}
  ]

  let nodeType : NodeType := {
    name := "ForBegin"
    ports := localPorts
  }

  let nodeOff := g.nodes.size
  let portOff := g.ports.size

  let ports := localPorts.map (fun p => 
    {p with globalId := portOff + p.localId, nodeId := nodeOff : Port})

  let stateIn := stateType.mapIdx fun i (name,typeName) => 
    { localId  := 2*i + localPorts.size, 
      globalId := 2*i + localPorts.size + portOff
      nodeId := nodeOff
      name := name
      type := .builtin typeName
      dir := .input : Port}

  let stateOut := stateType.mapIdx fun i (name,typeName) => 
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
    name := "forbegin"
    ports := ports.map (fun p => p.globalId)
    subPorts := #[{
      inputPortId  := some (2 + portOff)
      outputPortId := some (5 + portOff)
      subports := stateIn.flatten.map (fun p => (p.name, p.type.builtin!))
      }]
  }

  let g := {g with
    nodes := g.nodes.push node
    ports := g.ports ++ ports
  }

  let r := {
    iterations := .leaf ports[1]!.globalId
    scope := .leaf ports[3]!.globalId
    index := .leaf ports[4]!.globalId
    stateIn := stateIn.mapIdx (fun _ p => p.globalId)
    stateOut := stateOut.mapIdx (fun _ p => p.globalId)
  }
  
  return (g, r)

structure ForEndPorts where
  scope : PortBundle
  stateIn  : PortBundle
  stateOut : PortBundle
deriving Inhabited


def ApexGraph.addForEnd (g : ApexGraph) (stateType : ApexStaticType) : 
    ApexGraph × ForEndPorts :=
  Id.run do

  let localPorts : Array LocalPort := #[
    { localId := 0, name :=   "rundata", type :=       .rundata, dir := .output},
    { localId := 1, name :=     "scope", type := .builtin "Int", dir := .input},
    { localId := 2, name := "__spare__", type :=     .undefined, dir := .input},
    { localId := 3, name := "__spare__", type :=     .undefined, dir := .output}
  ]

  let nodeType : NodeType := {
    name := "ForEnd"
    ports := localPorts
  }

  let nodeOff := g.nodes.size
  let portOff := g.ports.size

  let ports := localPorts.map (fun p => 
    {p with globalId := portOff + p.localId, nodeId := nodeOff : Port})

  let stateIn := stateType.mapIdx fun i (name,typeName) => 
    { localId  := 2*i + localPorts.size, 
      globalId := 2*i + localPorts.size + portOff
      nodeId := nodeOff
      name := name
      type := .builtin typeName
      dir := .input : Port}

  let stateOut := stateType.mapIdx fun i (name,typeName) => 
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
    name := "forend"
    ports := ports.map (fun p => p.globalId)
    subPorts := #[{
      inputPortId  := some (2 + portOff)
      outputPortId := some (3 + portOff)
      subports := stateIn.flatten.map (fun p => (p.name,p.type.builtin!))
      }]
  }

  let g := {g with
    nodes := g.nodes.push node
    ports := g.ports ++ ports
  }

  let r := {
    scope := .leaf ports[1]!.globalId
    stateIn := stateIn.mapIdx (fun _ p => p.globalId)
    stateOut := stateOut.mapIdx (fun _ p => p.globalId)
  }
  
  return (g, r)
  

def ApexGraph.addConnection (g : ApexGraph) (src trg : PortId) : ApexGraph :=
  { g with wires := g.wires.push (src, trg) }

def ApexGraph.addConnections (g : ApexGraph) (src trg : ArrayTree PortId) : ApexGraph :=
  let wires := src.flatten.zip trg.flatten
  { g with wires := g.wires ++ wires }

def ApexGraph.pythonBuildScript (g : ApexGraph) (debug := false): String := 
Id.run do
  let mut s := ""

  s := s ++ s!"import apex" ++ "\n"
  s := s ++ s!"geo = hou.pwd().geometry()" ++ "\n"
  s := s ++ s!"graph = apex.Graph()" ++ "\n"

  s := s ++ "\n# Adding Nodes" ++ "\n"
  let mut portCount : Nat := 0
  for n in g.nodes, i in [0:g.nodes.size] do
    s := s ++ s!"n{i} = graph.addNode(\"{n.name}\", \"{n.type.name}\")" ++ "\n"
    for ⟨p?, q?, names⟩ in n.subPorts do
      let i? := p? <|> q?
      if let some i := i? then
        for name in names do
          s := s ++ s!"graph.addSubPort({i}, \"{name}\")" ++ "\n"

    portCount := portCount + n.ports.size

    if debug then
      s := s ++ s!"if sum([1 for p in graph.allPorts()]) != {portCount}:\
                 \n    raise TypeError(\"number of ports do not match after creating node {i} {n.type.name}\")" ++ "\n"

  s := s ++ "\n# Add Wires" ++ "\n"
  for (src, trg) in g.wires do
    let srcPort := g.ports[src]!
    let trgPort := g.ports[trg]!
    let srcNode := g.nodes[srcPort.nodeId]!
    let trgNode := g.nodes[trgPort.nodeId]!
    s := s ++ s!"graph.addWire({src}, {trg}) # {srcNode.name}[{srcPort.name}] -> {trgNode.name}[{trgPort.name}]" ++ "\n"

  s := s ++ "\n# Add Inputs and Outputs" ++ "\n"
  if g.inputPorts.size != 0 then
    s := s ++ s!"graph.addNode(\"input\", \"__parms__\")" ++ "\n"
    for inputPortId in g.inputPorts, i in [0:g.inputPorts.size] do
      let j := g.ports.size + i + 1
      let p := g.ports[inputPortId]!
      s := s ++ s!"graph.addGraphInput({g.nodes.size+1}, \"{p.name}\")" ++ "\n"
      s := s ++ s!"graph.addWire({j}, {inputPortId})" ++ "\n"

  if g.outputPorts.size != 0 then
    s := s ++ s!"graph.addNode(\"output\", \"__output__\")" ++ "\n"
    for outputPortId in g.outputPorts, i in [0:g.outputPorts.size] do
      let j := g.ports.size + i + 2 + g.inputPorts.size
      let p := g.ports[outputPortId]!
      s := s ++ s!"graph.addGraphOutput({g.nodes.size+2}, \"{p.name}{i}\")" ++ "\n"
      s := s ++ s!"graph.addWire({outputPortId}, {j})" ++ "\n"

  for (val, portId) in g.literals do
    let p := g.ports[portId]!
    -- nodes are index from 1-in python
    s := s ++ s!"graph.setNodeParm({p.nodeId+1}, \"{p.name}\", {val.toString})" ++ "\n"

  s := s ++ "\n# Layout and Save Graph" ++ "\n"
  s := s ++ "graph.layout()" ++ "\n"
  s := s ++ "graph.saveToGeometry(geo)" ++ "\n"
  return s

