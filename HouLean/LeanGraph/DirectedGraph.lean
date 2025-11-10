import Std.Data.HashMap
import Std.Data.HashSet

namespace HouLean.DAG

/-- A directed graph represented as adjacency lists -/
structure DiGraph (α : Type) [BEq α] [Hashable α] where
  /-- Adjacency list: maps each node to its direct successors -/
  successors : Std.HashMap α (Std.HashSet α)
  /-- All nodes in the graph -/
  nodes : Std.HashSet α
  deriving Inhabited

namespace DiGraph

variable {α : Type} [BEq α] [Hashable α]

/-- Create an empty graph -/
def empty : DiGraph α :=
  { successors := {}, nodes := {} }

/-- Add an edge from source to target -/
def addEdge (g : DiGraph α) (source target : α) : DiGraph α :=
  let succs := g.successors.alter source fun s? =>
    some (s?.getD {} |>.insert target)
  { successors := succs
    nodes := g.nodes.insert source |>.insert target }

/-- Get direct successors of a node -/
def getSuccessors (g : DiGraph α) (node : α) : Std.HashSet α :=
  g.successors.getD node {}

/--
Compute immediate post-dominators for all nodes efficiently.
A node D post-dominates N if all paths from N to any exit pass through D.
The immediate post-dominator (idom) is the closest such node.

Returns a map from each node to its immediate post-dominator.
Uses iterative dataflow algorithm - O(V·E) complexity.
-/
def computePostDominators (g : DiGraph α) : Std.HashMap α α := Id.run do
  -- Find all nodes with no successors (exits/sinks)
  let mut exits : Std.HashSet α := {}
  for node in g.nodes.toList do
    if (g.getSuccessors node).isEmpty then
      exits := exits.insert node
  
  -- If no exits, every node can reach itself indefinitely - no post-dominators
  if exits.isEmpty then
    return {}
  
  -- Initialize: each node is post-dominated by all nodes (except exits)
  let mut postDom : Std.HashMap α (Std.HashSet α) := {}
  for node in g.nodes.toList do
    if exits.contains node then
      postDom := postDom.insert node {node}
    else
      postDom := postDom.insert node g.nodes
  
  -- Iterative dataflow: post-dom(n) = {n} ∪ (∩ post-dom(s) for s in successors(n))
  let mut changed := true
  let mut iterations := 0
  while changed && iterations < 100 do
    changed := false
    iterations := iterations + 1
    
    for node in g.nodes.toList do
      if exits.contains node then
        continue
      
      let succs := g.getSuccessors node
      if succs.isEmpty then
        continue
      
      -- Intersect post-dominators of all successors
      let mut newPostDom : Option (Std.HashSet α) := none
      for succ in succs.toList do
        let succPostDom := postDom.getD succ {}
        match newPostDom with
        | none => newPostDom := some succPostDom
        | some current =>
          let mut intersection : Std.HashSet α := {}
          for n in current.toList do
            if succPostDom.contains n then
              intersection := intersection.insert n
          newPostDom := some intersection
      
      -- Add node itself
      match newPostDom with
      | some pdom =>
        let finalPostDom := pdom.insert node
        let oldPostDom := postDom.getD node {}
        if finalPostDom.toList.length != oldPostDom.toList.length then
          postDom := postDom.insert node finalPostDom
          changed := true
      | none => pure ()
  
  -- Extract immediate post-dominators (closest post-dominator)
  let mut idom : Std.HashMap α α := {}
  for node in g.nodes.toList do
    let pdoms := (postDom.getD node {}).toList.filter (· != node)
    
    if pdoms.isEmpty then
      continue
    
    -- Find the closest post-dominator (one not post-dominated by others)
    for candidate in pdoms do
      let mut isImmediate := true
      for other in pdoms do
        if other != candidate then
          let otherPdoms := postDom.getD other {}
          if otherPdoms.contains candidate then
            isImmediate := false
            break
      
      if isImmediate then
        idom := idom.insert node candidate
        break
  
  return idom

/--
Find the nearest reconvergence point (immediate post-dominator) of a node.
This is the nearest node R such that all paths from the given node pass through R.

Uses precomputed post-dominator tree - O(V·E) preprocessing, O(1) query.
-/
def nearestReconvergencePoint (g : DiGraph α) (node : α) : Option α := 
  let idom := g.computePostDominators
  idom.get? node

end DiGraph

end HouLean.DAG

-- Test cases
open HouLean.DAG

/-- Basic example with branching and reconvergence -/
def exampleGraph2 : DiGraph String :=
  DiGraph.empty
    |>.addEdge "A" "C"
    |>.addEdge "A" "E"
    |>.addEdge "B" "D"
    |>.addEdge "B" "C"
    |>.addEdge "C" "D"
    |>.addEdge "D" "E"
    |>.addEdge "E" "F"

/-- info: some "E" -/
#guard_msgs in
#eval exampleGraph2.nearestReconvergencePoint "A"

/-- info: some "D" -/
#guard_msgs in
#eval exampleGraph2.nearestReconvergencePoint "B"

/-- info: some "D" -/
#guard_msgs in
#eval exampleGraph2.nearestReconvergencePoint "C"

/-- info: some "E" -/
#guard_msgs in
#eval exampleGraph2.nearestReconvergencePoint "D"

/-- info: some "F" -/
#guard_msgs in
#eval exampleGraph2.nearestReconvergencePoint "E"

/-- Diamond pattern - classic reconvergence -/
def diamondGraph : DiGraph String :=
  DiGraph.empty
    |>.addEdge "A" "B"
    |>.addEdge "A" "C"
    |>.addEdge "B" "D"
    |>.addEdge "C" "D"
    |>.addEdge "D" "E"

/-- info: some "D" -/
#guard_msgs in
#eval diamondGraph.nearestReconvergencePoint "A"

/-- info: some "D" -/
#guard_msgs in
#eval diamondGraph.nearestReconvergencePoint "B"

/-- info: some "D" -/
#guard_msgs in
#eval diamondGraph.nearestReconvergencePoint "C"

/-- info: some "E" -/
#guard_msgs in
#eval diamondGraph.nearestReconvergencePoint "D"

/-- Nested diamonds - multiple reconvergence points -/
def nestedDiamonds : DiGraph String :=
  DiGraph.empty
    |>.addEdge "A" "B"
    |>.addEdge "A" "C"
    |>.addEdge "B" "D"
    |>.addEdge "C" "D"
    |>.addEdge "D" "E"
    |>.addEdge "D" "F"
    |>.addEdge "E" "G"
    |>.addEdge "F" "G"
    |>.addEdge "G" "H"

/-- info: some "D" -/
#guard_msgs in
#eval nestedDiamonds.nearestReconvergencePoint "A"

/-- info: some "D" -/
#guard_msgs in
#eval nestedDiamonds.nearestReconvergencePoint "B"

/-- info: some "G" -/
#guard_msgs in
#eval nestedDiamonds.nearestReconvergencePoint "D"

/-- info: some "G" -/
#guard_msgs in
#eval nestedDiamonds.nearestReconvergencePoint "E"

/-- info: some "H" -/
#guard_msgs in
#eval nestedDiamonds.nearestReconvergencePoint "G"

/-- Multiple exits - divergent paths -/
def multipleExits : DiGraph String :=
  DiGraph.empty
    |>.addEdge "A" "B"
    |>.addEdge "A" "C"
    |>.addEdge "B" "D"
    |>.addEdge "C" "E"

/-- info: none -/
#guard_msgs in
#eval multipleExits.nearestReconvergencePoint "A"

/-- info: some "D" -/
#guard_msgs in
#eval multipleExits.nearestReconvergencePoint "B"

/-- info: some "E" -/
#guard_msgs in
#eval multipleExits.nearestReconvergencePoint "C"

/-- Linear chain - trivial case -/
def linearChain : DiGraph String :=
  DiGraph.empty
    |>.addEdge "A" "B"
    |>.addEdge "B" "C"
    |>.addEdge "C" "D"

/-- info: some "B" -/
#guard_msgs in
#eval linearChain.nearestReconvergencePoint "A"

/-- info: some "C" -/
#guard_msgs in
#eval linearChain.nearestReconvergencePoint "B"

/-- info: some "D" -/
#guard_msgs in
#eval linearChain.nearestReconvergencePoint "C"

/-- info: none -/
#guard_msgs in
#eval linearChain.nearestReconvergencePoint "D"

/-- Single node with self loop -/
def singleNodeLoop : DiGraph String :=
  DiGraph.empty.addEdge "A" "A"

/-- info: none -/
#guard_msgs in
#eval singleNodeLoop.nearestReconvergencePoint "A"

/-- Isolated node - no edges, degenerate case -/
def isolatedNode : DiGraph String :=
  { successors := {}, nodes := {"A"} }

/-- info: none -/
#guard_msgs in
#eval isolatedNode.nearestReconvergencePoint "A"

/-- Complex convergence - triple diamond -/
def tripleDiamond : DiGraph String :=
  DiGraph.empty
    |>.addEdge "A" "B"
    |>.addEdge "A" "C"
    |>.addEdge "A" "D"
    |>.addEdge "B" "E"
    |>.addEdge "C" "E"
    |>.addEdge "D" "E"
    |>.addEdge "E" "F"

/-- info: some "E" -/
#guard_msgs in
#eval tripleDiamond.nearestReconvergencePoint "A"

/-- info: some "E" -/
#guard_msgs in
#eval tripleDiamond.nearestReconvergencePoint "B"

/-- info: some "E" -/
#guard_msgs in
#eval tripleDiamond.nearestReconvergencePoint "C"

/-- info: some "E" -/
#guard_msgs in
#eval tripleDiamond.nearestReconvergencePoint "D"

/-- info: some "F" -/
#guard_msgs in
#eval tripleDiamond.nearestReconvergencePoint "E"

/-- Asymmetric reconvergence - one path is longer -/
def asymmetricPaths : DiGraph String :=
  DiGraph.empty
    |>.addEdge "A" "B"
    |>.addEdge "A" "C"
    |>.addEdge "B" "D"
    |>.addEdge "B" "E"
    |>.addEdge "C" "F"
    |>.addEdge "D" "G"
    |>.addEdge "E" "G"
    |>.addEdge "F" "G"
    |>.addEdge "G" "H"

/-- info: some "G" -/
#guard_msgs in
#eval asymmetricPaths.nearestReconvergencePoint "A"

/-- info: some "G" -/
#guard_msgs in
#eval asymmetricPaths.nearestReconvergencePoint "B"

/-- info: some "F" -/
#guard_msgs in
#eval asymmetricPaths.nearestReconvergencePoint "C"
  
/-- info: some "H" -/
#guard_msgs in
#eval asymmetricPaths.nearestReconvergencePoint "G"

/-- Partial convergence - some paths converge, others don't -/
def partialConvergence : DiGraph String :=
  DiGraph.empty
    |>.addEdge "A" "B"
    |>.addEdge "A" "C"
    |>.addEdge "A" "D"
    |>.addEdge "B" "E"
    |>.addEdge "C" "E"
    |>.addEdge "D" "F"
    |>.addEdge "E" "G"

/-- info: none -/
#guard_msgs in
#eval partialConvergence.nearestReconvergencePoint "A"

/-- info: some "E" -/
#guard_msgs in
#eval partialConvergence.nearestReconvergencePoint "B"

/-- info: some "E" -/
#guard_msgs in
#eval partialConvergence.nearestReconvergencePoint "C"

/-- info: some "F" -/
#guard_msgs in
#eval partialConvergence.nearestReconvergencePoint "D"

/-- Late convergence - paths converge far downstream -/
def lateConvergence : DiGraph String :=
  DiGraph.empty
    |>.addEdge "A" "B"
    |>.addEdge "A" "C"
    |>.addEdge "B" "D"
    |>.addEdge "C" "E"
    |>.addEdge "D" "F"
    |>.addEdge "E" "F"
    |>.addEdge "F" "G"
    |>.addEdge "F" "H"
    |>.addEdge "G" "I"
    |>.addEdge "H" "I"

/-- info: some "F" -/
#guard_msgs in
#eval lateConvergence.nearestReconvergencePoint "A"

/-- info: some "I" -/
#guard_msgs in
#eval lateConvergence.nearestReconvergencePoint "F"

/-- info: some "I" -/
#guard_msgs in
#eval lateConvergence.nearestReconvergencePoint "G"

/-- Web pattern - highly interconnected -/
def webPattern : DiGraph String :=
  DiGraph.empty
    |>.addEdge "A" "B"
    |>.addEdge "A" "C"
    |>.addEdge "A" "D"
    |>.addEdge "B" "E"
    |>.addEdge "C" "E"
    |>.addEdge "D" "F"
    |>.addEdge "E" "F"
    |>.addEdge "E" "G"
    |>.addEdge "F" "G"
    |>.addEdge "G" "H"

/-- info: some "G" -/
#guard_msgs in
#eval webPattern.nearestReconvergencePoint "A"

/-- info: some "E" -/
#guard_msgs in
#eval webPattern.nearestReconvergencePoint "B"

/-- info: some "G" -/
#guard_msgs in
#eval webPattern.nearestReconvergencePoint "E"

/-- Empty graph - edge case -/
def emptyGraph : DiGraph String :=
  DiGraph.empty

/-- info: none -/
#guard_msgs in
#eval emptyGraph.nearestReconvergencePoint "NonExistent"

/-- Two component graph - disconnected -/
def twoComponents : DiGraph String :=
  DiGraph.empty
    |>.addEdge "A" "B"
    |>.addEdge "B" "C"
    |>.addEdge "X" "Y"
    |>.addEdge "Y" "Z"

/-- info: some "B" -/
#guard_msgs in
#eval twoComponents.nearestReconvergencePoint "A"

/-- info: some "Y" -/
#guard_msgs in
#eval twoComponents.nearestReconvergencePoint "X"

/-- Star pattern - many paths from one node -/
def starPattern : DiGraph String :=
  DiGraph.empty
    |>.addEdge "Center" "A"
    |>.addEdge "Center" "B"
    |>.addEdge "Center" "C"
    |>.addEdge "Center" "D"
    |>.addEdge "A" "End"
    |>.addEdge "B" "End"
    |>.addEdge "C" "End"
    |>.addEdge "D" "End"

/-- info: some "End" -/
#guard_msgs in
#eval starPattern.nearestReconvergencePoint "Center"
