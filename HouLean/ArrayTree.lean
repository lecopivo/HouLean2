
inductive ArrayTree (α : Type u) where
  | leaf (val : α)
  | node (children : Array (ArrayTree α))
deriving Inhabited, BEq

def ArrayTree.val! {α} [Inhabited α] (p : ArrayTree α) : α := 
  match p with
  | .leaf val => val
  | .node _ => panic! "ArrayTree.val! getting a value from a node!"

def ArrayTree.children! {α} [Inhabited α] (p : ArrayTree α) : Array (ArrayTree α) := 
  match p with
  | .leaf _ => panic! "ArrayTree.children! panic: getting children from a leaf"
  | .node children => children

def ArrayTree.val? {α} [Inhabited α] (p : ArrayTree α) : Option α := 
  match p with
  | .leaf val => some val
  | .node _ => none

def ArrayTree.children? {α} [Inhabited α] (p : ArrayTree α) : Option (Array (ArrayTree α)) := 
  match p with
  | .leaf _ => none
  | .node children => some children

def ArrayTree.child? {α} [Inhabited α] (p : ArrayTree α) (i : Nat) : Option (ArrayTree α) := 
  match p with
  | .leaf _ => none
  | .node children => children[i]?

def ArrayTree.isLeaf {α} (p : ArrayTree α) : Bool := 
  match p with
  | .leaf _ => true
  | .node _ => false

def ArrayTree.isNode {α} (p : ArrayTree α) : Bool := 
  match p with
  | .leaf _ => false
  | .node _ => true

def ArrayTree.leafNum {α} (p : ArrayTree α) : Nat :=
  match p with
  | .leaf _ => 1
  | .node cs => cs.foldl (init:=0) (fun s c => s + c.leafNum)

def ArrayTree.flatten {α} (t : ArrayTree α) : Array α :=
  match t with
  | .leaf v => #[v]
  | .node cs => cs.foldl (init:=#[]) (fun arr c => arr ++ c.flatten)

protected def ArrayTree.toString {α} [ToString α] (t : ArrayTree α) : String :=
  match t with
  | .leaf v => toString v
  | .node cs => toString (cs.map ArrayTree.toString)

def ArrayTree.mapIdxM {m} [Monad m] {α β} (p : ArrayTree α) (f : Nat → α → m β) : m (ArrayTree β) := do
  return (← go p 0).1
where
  go (p : ArrayTree α) (off : Nat) : m (ArrayTree β × Nat) := do
    match p with
    | .leaf i => return (.leaf (← f off i), off + 1)
    | .node ps => do
      let mut off := off
      let mut r : Array (ArrayTree β) := #[]
      for p in ps do
        let (p', off') ← go p off
        r := r.push p'
        off := off'
      return (.node r, off)

def ArrayTree.mapIdx {α β} (p : ArrayTree α) (f : Nat → α → β) : ArrayTree β := 
  p.mapIdxM (m:=Id) f
