import Std.Data.HashMap
import Std.Data.HashSet

open Std

/-- A directed graph represented as an adjacency list -/
structure DiGraph (α : Type) [BEq α] [Hashable α] where
  /-- Map from vertex to its outgoing neighbors -/
  adj : Std.HashMap α (Std.HashSet α)
  deriving Inhabited

namespace DiGraph

variable {α : Type} [BEq α] [Hashable α]

/-- Calculate in-degrees for all vertices -/
def inDegrees (g : DiGraph α) : Std.HashMap α Nat :=
  let vertices := g.adj.fold (fun acc v _ => acc.insert v 0) {}
  g.adj.fold (fun acc _ neighbors =>
    neighbors.fold (fun acc' v =>
      acc'.insert v ((acc'.get? v).getD 0 + 1)
    ) acc
  ) vertices

/-- Kahn's algorithm for topological sorting.
    Returns `some sorted` if the graph is a DAG, `none` if there's a cycle. -/
partial def kahnSort (g : DiGraph α) : Option (List α) :=
  let inDeg := g.inDegrees
  
  -- Initialize S with all nodes that have in-degree 0
  let initial_s := inDeg.fold (fun acc v deg =>
    if deg == 0 then v :: acc else acc
  ) []
  
  -- Helper function for the main loop
  let rec loop (s : List α) (l : List α) (inDeg : Std.HashMap α Nat) (edgeCount : Nat) : Option (List α) :=
    match s with
    | [] => 
        -- If there are remaining edges, we have a cycle
        if edgeCount > 0 then none else some l.reverse
    | n :: rest =>
        -- Get neighbors of n
        let neighbors := (g.adj.get? n).getD {}
        
        -- Process each neighbor
        let (new_s, new_inDeg, new_edgeCount) := neighbors.fold
          (fun (s', inDeg', count) m =>
            let new_deg := (inDeg'.get? m).getD 0 - 1
            let inDeg'' := inDeg'.insert m new_deg
            let s'' := if new_deg == 0 then m :: s' else s'
            (s'', inDeg'', count - 1)
          )
          (rest, inDeg, edgeCount)
        
        loop new_s (n :: l) new_inDeg new_edgeCount
  
  -- Count total edges
  let totalEdges := g.adj.fold (fun acc _ neighbors => acc + neighbors.size) 0
  
  loop initial_s [] inDeg totalEdges

end DiGraph



/-- Example usage -/
def exampleGraph : DiGraph Nat := {
  adj := Std.HashMap.ofList [
    (5, Std.HashSet.ofList [2, 0]),
    (4, Std.HashSet.ofList [0, 1]),
    (2, Std.HashSet.ofList [3]),
    (3, Std.HashSet.ofList [1]),
    (0, {}),
    (1, {})
  ]
}

#eval exampleGraph.kahnSort
-- Output: some [4, 5, 2, 0, 3, 1] (or another valid topological ordering)

/-- Example with a cycle -/
def cyclicGraph : DiGraph Nat := {
  adj := Std.HashMap.ofList [
    (0, Std.HashSet.ofList [1]),
    (1, Std.HashSet.ofList [2]),
    (2, Std.HashSet.ofList [0])
  ]
}

#eval cyclicGraph.kahnSort
-- Output: none
