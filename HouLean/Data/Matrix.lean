import HouLean.Math
import HouLean.Data.Defs

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

def Matrix.xx (a : Matrix α m n) (h : m > 0 ∧ n > 0 := by get_elem_tactic) : α := a.data[0][0]
def Matrix.xy (a : Matrix α m n) (h : m > 0 ∧ n > 1 := by get_elem_tactic) : α := a.data[0][1]
def Matrix.xz (a : Matrix α m n) (h : m > 0 ∧ n > 2 := by get_elem_tactic) : α := a.data[0][2]
def Matrix.xw (a : Matrix α m n) (h : m > 0 ∧ n > 3 := by get_elem_tactic) : α := a.data[0][3]

def Matrix.yx (a : Matrix α m n) (h : m > 1 ∧ n > 0 := by get_elem_tactic) : α := a.data[1][0]
def Matrix.yy (a : Matrix α m n) (h : m > 1 ∧ n > 1 := by get_elem_tactic) : α := a.data[1][1]
def Matrix.yz (a : Matrix α m n) (h : m > 1 ∧ n > 2 := by get_elem_tactic) : α := a.data[1][2]
def Matrix.yw (a : Matrix α m n) (h : m > 1 ∧ n > 3 := by get_elem_tactic) : α := a.data[1][3]

def Matrix.zx (a : Matrix α m n) (h : m > 2 ∧ n > 0 := by get_elem_tactic) : α := a.data[2][0]
def Matrix.zy (a : Matrix α m n) (h : m > 2 ∧ n > 1 := by get_elem_tactic) : α := a.data[2][1]
def Matrix.zz (a : Matrix α m n) (h : m > 2 ∧ n > 2 := by get_elem_tactic) : α := a.data[2][2]
def Matrix.zw (a : Matrix α m n) (h : m > 2 ∧ n > 3 := by get_elem_tactic) : α := a.data[2][3]

def Matrix.wx (a : Matrix α m n) (h : m > 3 ∧ n > 0 := by get_elem_tactic) : α := a.data[3][0]
def Matrix.wy (a : Matrix α m n) (h : m > 3 ∧ n > 1 := by get_elem_tactic) : α := a.data[3][1]
def Matrix.wz (a : Matrix α m n) (h : m > 3 ∧ n > 2 := by get_elem_tactic) : α := a.data[3][2]
def Matrix.ww (a : Matrix α m n) (h : m > 3 ∧ n > 3 := by get_elem_tactic) : α := a.data[3][3]

namespace Matrix

-- Matrix operations
def transpose (a : Matrix α m n) : Matrix α n m :=
  { data := Vector.ofFn fun j => Vector.ofFn fun i => a.data[i][j] }

def map (f : α → β) (a : Matrix α m n) : Matrix β m n :=
  { data := a.data.map (·.map f) }

def zipWith (f : α → β → γ) (a : Matrix α m n) (b : Matrix β m n) : Matrix γ m n :=
  { data := a.data.zipWith (·.zipWith f ·) b.data }

def add [Add α] (a b : Matrix α m n) : Matrix α m n :=
  zipWith (· + ·) a b

def sub [Sub α] (a b : Matrix α m n) : Matrix α m n :=
  zipWith (· - ·) a b

-- Identity matrix
def identity [Zero α] [One α] (n : Nat) : Matrix α n n :=
  { data := Vector.ofFn fun i =>
      Vector.ofFn fun j =>
        if i.val = j.val then 1 else 0 }

def ofFn (f : Fin m → Fin n → α) : Matrix α m n :=
  .mk (Vector.ofFn (fun i => Vector.ofFn fun j => f i j))

-- Zero matrix
def zero [Zero α] (m n : Nat) : Matrix α m n :=
  { data := Vector.ofFn fun _ => Vector.ofFn fun _ => 0 }

-- Row and column extraction
def row (a : Matrix α m n) (i : Fin m) : Vector α n := a.data[i]

def col (a : Matrix α m n) (j : Fin n) : Vector α m :=
  Vector.ofFn fun i => a.data[i][j]

-- Instances
instance [Add α] : Add (Matrix α m n) where
  add := add

instance [Sub α] : Sub (Matrix α m n) where
  sub := sub

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
