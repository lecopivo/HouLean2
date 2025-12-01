import HouLean.Math
import HouLean.Data.Defs
import HouLean.Data.Vector

open HouLean Math

namespace HouLean

instance {α} {m n : Nat} : GetElem? (Matrix α m n) (Nat×Nat) α (fun _ ij => ij.1 < m ∧ ij.2 < n) where
  getElem A ij h := A.data[ij.1][ij.2]
  getElem? A ij := A.data[ij.1]? |>.map (fun row => row[ij.2]?) |>.join
  getElem! A ij := A.data[ij.1]![ij.2]!

instance {α} {m n : Nat} : SetElem? (Matrix α m n) (Nat×Nat) α (fun _ ij => ij.1 < m ∧ ij.2 < n) where
  setElem A ij x h :=
    let (i,j) := ij
    ⟨A.data.set i (A.data[i].set j x) (by grind)⟩
  setElem? A ij x :=
    let (i,j) := ij
    if h : i < m ∧ j < n then
      some ⟨A.data.set i (A.data[i].set j x) (by get_elem_tactic)⟩
    else
      none
  setElem! A ij x :=
    let (i,j) := ij
    ⟨A.data.set! i (A.data[i]!.set! j x)⟩

namespace Matrix

-- Row and column extraction
def row (a : Matrix α m n) (i : Nat) (h : i < m := by get_elem_tactic) :
    Vector α n := a.data[i]

def col (a : Matrix α m n) (j : Nat) (h : j < n := by get_elem_tactic) :
    Vector α m := Vector.ofFn fun i => a[(i.1,j)]

-- row projections
def row0 (a : Matrix α m n) (h : 0 < m := by get_elem_tactic) := a.row 0
def row1 (a : Matrix α m n) (h : 1 < m := by get_elem_tactic) := a.row 1
def row2 (a : Matrix α m n) (h : 2 < m := by get_elem_tactic) := a.row 2
def row3 (a : Matrix α m n) (h : 3 < m := by get_elem_tactic) := a.row 3
def row4 (a : Matrix α m n) (h : 4 < m := by get_elem_tactic) := a.row 4
def row5 (a : Matrix α m n) (h : 5 < m := by get_elem_tactic) := a.row 5
def row6 (a : Matrix α m n) (h : 6 < m := by get_elem_tactic) := a.row 6
def row7 (a : Matrix α m n) (h : 7 < m := by get_elem_tactic) := a.row 7

def xx (a : Matrix α m n) (h : m > 0 ∧ n > 0 := by get_elem_tactic) : α := a.data[0][0]
def xy (a : Matrix α m n) (h : m > 0 ∧ n > 1 := by get_elem_tactic) : α := a.data[0][1]
def xz (a : Matrix α m n) (h : m > 0 ∧ n > 2 := by get_elem_tactic) : α := a.data[0][2]
def xw (a : Matrix α m n) (h : m > 0 ∧ n > 3 := by get_elem_tactic) : α := a.data[0][3]

def yx (a : Matrix α m n) (h : m > 1 ∧ n > 0 := by get_elem_tactic) : α := a.data[1][0]
def yy (a : Matrix α m n) (h : m > 1 ∧ n > 1 := by get_elem_tactic) : α := a.data[1][1]
def yz (a : Matrix α m n) (h : m > 1 ∧ n > 2 := by get_elem_tactic) : α := a.data[1][2]
def yw (a : Matrix α m n) (h : m > 1 ∧ n > 3 := by get_elem_tactic) : α := a.data[1][3]

def zx (a : Matrix α m n) (h : m > 2 ∧ n > 0 := by get_elem_tactic) : α := a.data[2][0]
def zy (a : Matrix α m n) (h : m > 2 ∧ n > 1 := by get_elem_tactic) : α := a.data[2][1]
def zz (a : Matrix α m n) (h : m > 2 ∧ n > 2 := by get_elem_tactic) : α := a.data[2][2]
def zw (a : Matrix α m n) (h : m > 2 ∧ n > 3 := by get_elem_tactic) : α := a.data[2][3]

def wx (a : Matrix α m n) (h : m > 3 ∧ n > 0 := by get_elem_tactic) : α := a.data[3][0]
def wy (a : Matrix α m n) (h : m > 3 ∧ n > 1 := by get_elem_tactic) : α := a.data[3][1]
def wz (a : Matrix α m n) (h : m > 3 ∧ n > 2 := by get_elem_tactic) : α := a.data[3][2]
def ww (a : Matrix α m n) (h : m > 3 ∧ n > 3 := by get_elem_tactic) : α := a.data[3][3]


-- todo: add support for `#m[xx,xy; yx,yy]`
macro "#m[" rows:term,* "]" : term => `(⟨#v[ $rows,* ]⟩)

-- Matrix operations
def ofFn (f : (i j : Nat) → (h : i < m ∧ j < n) → α) : Matrix α m n :=
  ⟨.ofFn (fun i => .ofFn fun j => f i j (by grind))⟩

def map (f : α → β) (a : Matrix α m n) : Matrix β m n :=
  { data := a.data.map (·.map f) }

def mapRows (f : Vector α n → Vector β n) (a : Matrix α m n) : Matrix β m n :=
  { data := a.data.map f }

def mapRows₂ (f : Vector α n → Vector β n → Vector γ n)
    (a : Matrix α m n) (b : Matrix β m n) : Matrix γ m n :=
  { data := (a.data.zip b.data).map f.uncurry }

def transpose (a : Matrix α m n) : Matrix α n m :=
  ofFn (fun j i _ => a[(i,j)])

-- Identity matrix
def identity [Zero α] [One α] (n : Nat) : Matrix α n n :=
  { data := Vector.ofFn fun i =>
      Vector.ofFn fun j =>
        if i.val = j.val then 1 else 0 }

-- Zero matrix
def zero [Zero α] (m n : Nat) : Matrix α m n :=
  { data := Vector.ofFn fun _ => Vector.ofFn fun _ => 0 }


def add [Add α] (a b : Matrix α m n) : Matrix α m n :=
  mapRows₂ (· + ·) a b

def sub [Sub α] (a b : Matrix α m n) : Matrix α m n :=
  mapRows₂ (· - ·) a b

-- Instances
instance [Add α] : Add (Matrix α m n) where
  add := add

instance [Sub α] : Sub (Matrix α m n) where
  sub := sub

def matMul [Add α] [Mul α] [Zero α] (a : Matrix α m k) (b : Matrix α k n) : Matrix α m n :=
  ofFn (fun i j _ => (a.row i).dot (b.col j))

-- instance [Zero α] [Add α] [Mul α] : HMul (Matrix α m k) (Matrix α k n) (Matrix α m n) where
--   hMul := mul

-- instance [Mul α] : HMul α (Matrix α m n) (Matrix α m n) where
--   hMul := scalar_mul

instance [Zero α] : Zero (Matrix α m n) where
  zero := zero m n

instance [Zero α] [One α] : One (Matrix α n n) where
  one := identity n

-- ToString instance for debugging
instance [ToString α] : ToString (Matrix α m n) where
  toString a :=
    "[" ++ String.intercalate ",\n " (a.data.toList.map fun row =>
      "[" ++ String.intercalate ", " (row.toList.map toString) ++ "]") ++ "]"


-- syntax "#m[" sepBy(term,* , ";") "]" : term

-- macro_rules
-- | `(#m[ $x:term,* ; $rest ]) => sorry
