import HouLean.LeanGraph.Traverse

/-! In this file we analyze scopes of the Lean Graph.

Each output node defines a new scope. Each scope/output node has input nodes attached to it. 

Which input corresponds to which output is done by computing immediate post-dominators.

Resulting scopes have tree-like structure. The outermost scope at the top corresponds to the unconnected 
output node. (There is also the ground scope i.e. no scope attached and that corresponds to constant terms 
that do not depend on any inputs)

The scope of a node is the deepest (maximum) of all the scopes of input nodes. The maximum might not exist 
as not all scopes are comparable. In that case the network is considered to be ill-formed.

-/

open Lean Meta Elab Term Std

namespace HouLean
namespace LeanGraph.Traverse

/-- Scope hierarchy for nodes in the graph -/
inductive Scope where
  /-- Scope for constant terms not living in any scope and not depending on any input variables -/
  | ground 
  /-- Scope of an output node `nodeName` -/
  | node (nodeName : String)
  deriving BEq, Hashable, Repr

instance : ToString Scope where
  toString := fun
    | .ground => "ground"
    | .node name => s!"scope({name})"

/-- Transitive closure of the scope hierarchy for efficient comparison -/
structure ScopeHierarchy where
  /-- Maps each scope to all scopes it contains (transitive) -/
  containsMap : HashMap Scope (HashSet Scope)
  /-- Store paret of each scope -/
  parents : HashMap Scope Scope
  /-- Scope inputs, for each node store a set of input nodes. -/
  inputs : HashMap Scope (HashSet String)
  /-- Root scope (typically the unconnected output) -/
  root : Scope

instance : Inhabited ScopeHierarchy where
  default := { containsMap := {}, parents := {}, inputs := {}, root := .ground }

instance : ToString ScopeHierarchy where
  toString h := Id.run do
    let mut s := "Scope Hierachy:\n"
    ++
    s!"  root: {h.root}\n\n"
    for (scope,inners) in h.containsMap do
      s := s ++ s!"  {scope}{(h.inputs[scope]?.getD {}).toList}  ⊆  {h.parents[scope]?}\n     {inners.toList}\n"
    s

namespace ScopeHierarchy

/-- Check if scope1 contains scope2 (scope1 is outer, scope2 is inner) -/
def scopeContains (h : ScopeHierarchy) (outer inner : Scope) : Bool :=
  if outer == inner then true
  else
    match h.containsMap.get? outer with
    | none => false
    | some contained => contained.contains inner

/-- Compare two scopes and return the maximum (innermost) if comparable -/
def max (h : ScopeHierarchy) (s1 s2 : Scope) : Option Scope :=
  if s1 == s2 then
    some s1
  else if h.scopeContains s1 s2 then
    some s2  -- s2 is inner (deeper)
  else if h.scopeContains s2 s1 then
    some s1  -- s1 is inner (deeper)
  else
    none  -- not comparable

/-- Get the least common ancestor of two scopes -/
def lca (h : ScopeHierarchy) (s1 s2 : Scope) : Scope :=
  if s1 == s2 then s1
  else if h.scopeContains s1 s2 then s1
  else if h.scopeContains s2 s1 then s2
  else h.root

end ScopeHierarchy

/-- Build the scope hierarchy from immediate post-dominators -/
def buildScopeHierarchy (graph : LeanGraph) (idom : HashMap String String) 
    (ctx : Context) : TermElabM ScopeHierarchy := do
  
  withTraceNode `HouLean.LeanGraph.typecheck
    (fun _ => return "Building scope hierarchy") do

  -- Build the parent relationship from idom
  let mut parents : HashMap Scope Scope := {}
  let mut inputs : HashMap Scope (HashSet String) := {}
  for (inputName, outputName) in idom.toList do
    let childScope := Scope.node inputName
    let parentScope := Scope.node outputName
    let some childNode := ctx.nodeMap[inputName]? | throwError "Invalid node name {inputName}"
    if childNode.type.leanConstant == ``HouLean.input then
      inputs := inputs.alter parentScope (fun ins? => (ins?.getD {}).insert inputName)
    else
      parents := parents.insert childScope parentScope
      trace[HouLean.LeanGraph.typecheck] "Scope parent: {childScope} -> {parentScope}"

  
  -- Find the root scope (unconnected output node)
  let mut rootScope : Scope := .ground
  for node in graph.nodes do
    if node.type.leanConstant == ``HouLean.output then
      if ((ctx.outputConnections.get? node.name).map (·.size) |>.getD 0) == 0 then
        rootScope := .node node.name
        parents := parents.insert rootScope .ground
        trace[HouLean.LeanGraph.typecheck] "Root scope: {node.name}"
        break
  
  -- Build transitive closure: for each scope, compute all scopes it contains
  let mut containsMap : HashMap Scope (HashSet Scope) := {}
  
  -- Initialize: each scope contains itself
  containsMap := containsMap.insert .ground {.ground}
  for node in graph.nodes do
    if node.type.leanConstant == ``HouLean.output then
      let scope := Scope.node node.name
      containsMap := containsMap.insert scope {scope}
    
  -- Compute transitive closure using fixed-point iteration
  let mut changed := true
  let mut iterations := 0
  while changed do
    changed := false
    iterations := iterations + 1
    
    for (child, parent) in parents.toList do
      let childContained := containsMap.getD child {child}
      let parentContained := containsMap.getD parent {parent}
      
      -- Parent should contain everything child contains
      let newParentContained := parentContained.union childContained
      
      if newParentContained.toList.length != parentContained.toList.length then
        containsMap := containsMap.insert parent newParentContained
        changed := true
  
  trace[HouLean.LeanGraph.typecheck] "Scope hierarchy built in {iterations} iterations"
  for (scope, contained) in containsMap.toList do
    trace[HouLean.LeanGraph.typecheck] "{scope} contains: {contained.toList}"
  
  return { containsMap, parents, inputs, root := rootScope }



/-- Assign scopes to all nodes in the graph -/
partial def assignNodeScopes (graph : LeanGraph) (ctx : Context) (hierarchy : ScopeHierarchy) 
    : TermElabM (HashMap String Scope) := do
  
  withTraceNode `HouLean.LeanGraph.typecheck
    (fun _ => return "Assigning scopes to nodes") do

  let mut scopes : HashMap String Scope := {}

  for (scope, inputs) in hierarchy.inputs do
    for input in inputs do
      scopes := scopes.insert input scope

  for (scope, parentScope) in hierarchy.parents do
    if let .node nodeName := scope then
      scopes := scopes.insert nodeName parentScope
  
  -- Recursive function to compute scope for a node using StateT
  let rec getNodeScope (nodeName : String)
      : StateT ((HashMap String Scope) × HashSet String) CoreM Scope := do

    -- Return cached result if available
    let nodeScopes := (← get).1
    if let some scope := nodeScopes.get? nodeName then
      return scope
    
    -- Check for cycles
    if (← get).2.contains nodeName then
      throwError m!"Cycle detected while computing scope for node {nodeName}"
    
    -- mark current node as visited
    modify (fun (cache, vis) => (cache, vis.insert nodeName))
    
    -- Regular nodes: scope is the maximum of all input dependencies
    let inputConnections := ctx.inputConnections.getD nodeName #[]
    let mut scope := Scope.ground
    
    for conn in inputConnections do
      let depScope ← getNodeScope conn.outputNodeName
      
      match hierarchy.max scope depScope with
      | some maxScope => scope := maxScope
      | none => 
        throwError m!"Node {nodeName} has incomparable scopes: {scope} and {depScope}. \
                      The graph contains conflicting control flow that cannot be resolved."
    
    modify (fun (cache, vis) => (cache.insert nodeName scope, vis))
    trace[HouLean.LeanGraph.typecheck] "Node {nodeName} -> {scope}"
    return scope
  
  -- Process all nodes
  let (_, (nodeScopes,_)) ← (graph.nodes.foldlM (init := ()) fun _ node => do
    let name := Node.name node
    let _ ← getNodeScope name
    pure ()
  ).run (scopes,{})
  
  return nodeScopes

/-- Complete scope analysis for a graph -/
def analyzeScopesComplete (graph : LeanGraph) (ctx : Context) 
    : TermElabM (HashMap String Scope × ScopeHierarchy) := do
  
  withTraceNode `HouLean.LeanGraph.typecheck
    (fun _ => return "Complete scope analysis") do
  
  -- Extract input-output subgraph
  let subgraph ← extractInputOutputSubgraph graph ctx
  
  -- Compute immediate post-dominators
  let idom := subgraph.computePostDominators
  
  trace[HouLean.LeanGraph.typecheck] "Immediate post-dominators:"
  for (node, dom) in idom.toList do
    trace[HouLean.LeanGraph.typecheck] "  {node} -> {dom}"
  
  -- Build scope hierarchy
  let hierarchy ← buildScopeHierarchy graph idom ctx
  
  -- Assign scopes to all nodes
  let nodeScopes ← assignNodeScopes graph ctx hierarchy
  
  return (nodeScopes, hierarchy)

/-- Validate that a graph has well-formed scopes -/
def validateScopes (graph : LeanGraph) : TermElabM Bool := do
  let ctx ← buildContext graph
  let ctx ← analyzeInputOutputFlow graph ctx
  
  try
    let _ ← analyzeScopesComplete graph ctx
    return true
  catch e =>
    trace[HouLean.LeanGraph.typecheck] "Scope validation failed: {e.toMessageData}"
    return false

end LeanGraph.Traverse
end HouLean
