import HouLean.Apex.Compile.NodeType

open Lean Meta Std

namespace HouLean.Apex.Compiler

inductive LiteralVal
  | int (val : Int)
  | float (val : Float)
  | str (str : String)
  | bool (str : Bool)
deriving Inhabited

structure ApexGraph where
  nodes : Array Node
  /-- ports[i] = (node id, local port id) -/
  ports : Array Port
  /-- Wire connecting output port and input port -/
  wires : Array (PortId × PortId)
  /-- Literals values which point to `Value<_>` node of appropriate type -/
  literals : Array (LiteralVal × PortId)
deriving Inhabited

-- instance : ToString ApexGraph := ⟨fun g => 
--   Id.run do
--     let mut s := "Nodes:\n"
--     for n in g.nodes, i in Array.range g.nodes.size do
--       s := s ++ s!"  {i}: {n.apexName}\n"

--     s := s ++ "\nPorts:\n"
--     for (nodeId, localId) in g.ports, i in Array.range g.ports.size do
--       s := s ++ s!"  {i}: {nodeId} {localId}\n"

--     s := s ++ "\nWires:\n"
--     for (src, trg) in g.wires, i in Array.range g.wires.size do
--       s := s ++ s!"  {i}: {src} -> {trg}\n"

--     s := s ++ "\nLiterals:\n"
--     for (val, nodeId) in g.literals, i in Array.range g.literals.size do
--       match val with
--       | .int val => 
--         s := s ++ s!"  {i}: int {val} -> {nodeId} \n"
--       | .float val => 
--         s := s ++ s!"  {i}: float {val} -> {nodeId} \n"
--       | .str str =>
--         s := s ++ s!"  {i}: str \"{str}\" -> {nodeId} \n"
--     return s⟩

-- /-- Merge two disjoint graphs -/
-- def ApexGraph.merge (g h : ApexGraph) : ApexGraph :=

--   let nodes := g.nodes ++ h.nodes
--   let ports := g.ports ++ h.ports.map (fun (node,portId) => (node+g.nodes.size, portId)) 
--   let wires := g.wires ++ h.wires.map (fun (i,j) => (i+g.ports.size, j+g.ports.size))
--   let literals := g.literals ++ h.literals.map (fun (val, nodeId) => (val, nodeId + g.nodes.size))
--   {
--     nodes := nodes
--     ports := ports
--     wires := wires
--     literals := literals
--   }

/-- Add `node` to the graph and return new input and output port id's

It is assumed that `ApexNode` has local port indices stored in `inputs` and `output`
-/
def ApexGraph.addNode (g : ApexGraph) (node : NodeType) : ApexGraph × Nat × Array (ArrayTree Port) × (ArrayTree Port) := sorry
-- Id.run do
--   let mut ⟨nodes, ports, wires, literals⟩ := g
--   let nodeId := nodes.size

--   let off := ports.size

--   -- make all ports
--   for i in [0:node.] do
--     ports := ports.push (nodeId, i)

--   -- offset ports from local to global port indices
--   let inputs := node.inputs.map (fun input => input.map (fun _ (id,name) => (id + off, name)))
--   let output := node.output.map (fun _ (id,name) => (id + off, name))

--   nodes := nodes.push { node with 
--     inputs := inputs
--     output := output
--     subPorts := node.subPorts.map (fun (p, count) => (p + off, count)), 
--   }

--   -- drop names
--   let inputs := inputs.map (fun input => input.map (fun _ (id,_) => id))
--   let output := output.map (fun _ (id,_) => id)

--   return (⟨nodes, ports, wires, literals⟩, nodeId, inputs, output)

def ApexGraph.addInt (g : ApexGraph) (val : Int) (port : Port) : ApexGraph :=
  {g with literals := g.literals.push (.int val, port.globalId)}

def ApexGraph.addFloat (g : ApexGraph) (val : Float) (port : Port) : ApexGraph :=
  {g with literals := g.literals.push (.float val, port.globalId)}

def ApexGraph.addBool (g : ApexGraph) (val : Bool) (port : Port) : ApexGraph :=
  {g with literals := g.literals.push (.bool val, port.globalId)}

def ApexGraph.addString (g : ApexGraph) (val : String) (port : Port) : ApexGraph :=
  {g with literals := g.literals.push (.str val, port.globalId)}

structure ForBeginPorts where
  iterations : Port
  scope : Port
  index : Port
  stateIn  : ArrayTree Port
  stateOut : ArrayTree Port
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

  let stateIn := stateType.mapIdx fun i typeName => 
    { localId  := 2*i + localPorts.size, 
      globalId := 2*i + localPorts.size + portOff
      nodeId := nodeOff
      name := s!"x{i}"
      type := .builtin typeName
      dir := .input : Port}

  let stateOut := stateType.mapIdx fun i typeName => 
    { localId  := 2*i + 1 + localPorts.size, 
      globalId := 2*i + 1 + localPorts.size + portOff
      nodeId := nodeOff
      name := s!"x{i}"
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
      variadicPortLocalId := 2
      outputPortId := some 5
      subports := stateIn.flatten.map (fun p => p.name)
      }]
  }

  let g := {g with
    nodes := g.nodes.push node
    ports := g.ports ++ ports
  }

  let r := {
    iterations := ports[1]!
    scope := ports[3]!
    index := ports[4]!
    stateIn := stateIn
    stateOut := stateOut
  }
  
  return (g, r)

structure ForEndPorts where
  iterations : Port
  scope : Port
  index : Port
  stateIn  : ArrayTree Port
  stateOut : ArrayTree Port
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

  let stateIn := stateType.mapIdx fun i typeName => 
    { localId  := 2*i + localPorts.size, 
      globalId := 2*i + localPorts.size + portOff
      nodeId := nodeOff
      name := s!"x{i}"
      type := .builtin typeName
      dir := .input : Port}

  let stateOut := stateType.mapIdx fun i typeName => 
    { localId  := 2*i + 1 + localPorts.size, 
      globalId := 2*i + 1 + localPorts.size + portOff
      nodeId := nodeOff
      name := s!"x{i}"
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
      variadicPortLocalId := 2
      outputPortId := some 3
      subports := stateIn.flatten.map (fun p => p.name)
      }]
  }

  let g := {g with
    nodes := g.nodes.push node
    ports := g.ports ++ ports
  }

  let r := {
    iterations := ports[1]!
    scope := ports[3]!
    index := ports[4]!
    stateIn := stateIn
    stateOut := stateOut
  }
  
  return (g, r)
  

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
    for ⟨p, _, names⟩ in n.subPorts do
      for name in names do
        s := s ++ s!"graph.addSubPort({p}, {name})" ++ "\n"

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

  s := s ++ "\n# Layout and Save Graph" ++ "\n"
  s := s ++ "graph.layout()" ++ "\n"
  s := s ++ "graph.saveToGeometry(geo)" ++ "\n"
  return s

#exit

open Qq

run_meta
  let c : GraphCompileM Unit := do
    let e := q(fun x : Float => let y := x; y)
    let _ ← functionToApexGraph e
  let (_,⟨g,_⟩) ← c default
  IO.println g
  IO.println g.pythonBuildScript


attribute [apex_node "Add<Float>"] Float.add
attribute [apex_node "Multiply<Float>"] Float.mul

run_meta
  let c : GraphCompileM Unit := do
    let e := q(fun x y : Float => let z := x * y; let w := z * x; w * z + x + z)
    let _ ← functionToApexGraph e
  let (_,⟨g,_⟩) ← c default
  IO.println g
  IO.println g.pythonBuildScript

structure Float2 where
  x : Float
  y : Float

instance : Add Float2 := ⟨fun x y => ⟨x.1+y.1, x.2+y.2⟩⟩

run_meta
  let c : GraphCompileM Unit := do
    let e := q(fun x y : Float2 => x + y)
    let _ ← functionToApexGraph e
  let (_,⟨g,_⟩) ← c default
  IO.println g

abbrev add2 (x y : Float × Float) : Float × Float := (x.1+y.1, x.2+y.2)
abbrev add3 (x y : Float × Float × Float) : Float × Float × Float := (x.1+y.1, add2 x.2 y.2)
abbrev dot2 (x y : Float × Float) : Float := (x.1*y.1 + x.2*y.2)
abbrev dot3 (x y : Float × Float × Float) : Float := (x.1*y.1 + dot2 x.2 y.2)

instance : Add Vector2 := ⟨fun x y => Apex.AddVector2 x [y]⟩  
instance : Add Vector3 := ⟨fun x y => Apex.AddVector3 x [y]⟩

abbrev Vector2.x (v : Vector2) : Float := Apex.GetComponentVector2 v 0
abbrev Vector2.y (v : Vector2) : Float := Apex.GetComponentVector2 v 1
abbrev Vector2.mk (x y : Float) : Vector2 := Apex.FloatToVector2 x y

abbrev Vector3.x (v : Vector3) : Float := Apex.GetComponentVector3 v 0
abbrev Vector3.y (v : Vector3) : Float := Apex.GetComponentVector3 v 1
abbrev Vector3.z (v : Vector3) : Float := Apex.GetComponentVector3 v 2
abbrev Vector3.mk (x y z : Float) : Vector3 := Apex.FloatToVector3 x y z

abbrev Vector4.x (v : Vector4) : Float := Apex.GetComponentVector4 v 0
abbrev Vector4.y (v : Vector4) : Float := Apex.GetComponentVector4 v 1
abbrev Vector4.z (v : Vector4) : Float := Apex.GetComponentVector4 v 2
abbrev Vector4.w (v : Vector4) : Float := Apex.GetComponentVector4 v 3
abbrev Vector4.mk (x y z w : Float) : Vector4 := Apex.FloatToVector4 x y z w

instance : Add Vector4 := ⟨fun u v => .mk (u.x+v.x) (u.y+v.y) (u.z+v.z) (u.w+v.w)⟩  

run_meta
  let c : GraphCompileM Unit := do
    let e := q(fun x y : Vector4 => x + y)
    let _ ← functionToApexGraph e
  let (_,⟨g,_⟩) ← c default
  IO.println g
  IO.println g.pythonBuildScript

run_meta
  let c : GraphCompileM Unit := do
    let e := q(fun x : Vector3 => Apex.AddVector3 x [x,x,x,x])
    let _ ← functionToApexGraph e
  let (_,⟨g,_⟩) ← c default
  IO.println g
  IO.println g.pythonBuildScript

run_meta
  let c : GraphCompileM Unit := do
    let e := q(fun x : Float => let y := (x,x); dot2 y y)
    let _ ← functionToApexGraph e
  let (_,⟨g,_⟩) ← c default
  IO.println g
  IO.println g.pythonBuildScript


abbrev foo := fun x : Float => Id.run do
    let mut x : Float := x
    for _ in [0:10] do
      x := x + x
    x

abbrev bar := fun x : Float => Id.run do
    let mut x : Float := x
    let mut y : Float := x
    for _ in [0:10] do
      let tmp := y
      y := x + y
      x := tmp
    x

abbrev foobar := fun x' : Float => Id.run do
    let mut x : Vector3 := Vector3.mk 0 0 0
    let mut y : Vector3 := Vector3.mk 1 10 100
    let mut n : Nat := 0
    for i in [0:10] do
      let tmp := y
      y := x.add y
      x := tmp
      n := n + i*i
    x

attribute [apex_type "Int"] Nat
attribute [apex_node "Add<Int>"] Nat.add
attribute [apex_node "Add<Nat>"] Nat.mul

run_meta
  let c : GraphCompileM Unit := do
    let e := q(foo)
    let _ ← functionToApexGraph e
  let (_,⟨g,_⟩) ← c default
  IO.println g
  IO.println (g.pythonBuildScript true)

run_meta
  let c : GraphCompileM Unit := do
    let e := q(bar)
    let _ ← functionToApexGraph e
  let (_,⟨g,_⟩) ← c default
  IO.println g
  IO.println (g.pythonBuildScript true)

set_option pp.all true in
run_meta
  let c : GraphCompileM Unit := do
    let e := q(foobar)
    let _ ← functionToApexGraph e
  let (_,⟨g,_⟩) ← c default
  IO.println g
  IO.println g.pythonBuildScript
