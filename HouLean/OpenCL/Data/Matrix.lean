import HouLean.OpenCL.Data.Vector
import HouLean.OpenCL.Data.ArrayRef

namespace HouLean.OpenCL

open Compiler Qq HouLean Math

structure Matrix (α : Type) (m n : Nat) where
  data : Vector (Vector α n) m

def Matrix.xx (a : Matrix α m n) (h : m > 0 ∧ n > 0 := by grind) : α := a.data[0][0]
def Matrix.xy (a : Matrix α m n) (h : m > 0 ∧ n > 1 := by grind) : α := a.data[0][1]
def Matrix.xz (a : Matrix α m n) (h : m > 0 ∧ n > 2 := by grind) : α := a.data[0][2]
def Matrix.xw (a : Matrix α m n) (h : m > 0 ∧ n > 3 := by grind) : α := a.data[0][3]

def Matrix.yx (a : Matrix α m n) (h : m > 1 ∧ n > 0 := by grind) : α := a.data[1][0]
def Matrix.yy (a : Matrix α m n) (h : m > 1 ∧ n > 1 := by grind) : α := a.data[1][1]
def Matrix.yz (a : Matrix α m n) (h : m > 1 ∧ n > 2 := by grind) : α := a.data[1][2]
def Matrix.yw (a : Matrix α m n) (h : m > 1 ∧ n > 3 := by grind) : α := a.data[1][3]

def Matrix.zx (a : Matrix α m n) (h : m > 2 ∧ n > 0 := by grind) : α := a.data[2][0]
def Matrix.zy (a : Matrix α m n) (h : m > 2 ∧ n > 1 := by grind) : α := a.data[2][1]
def Matrix.zz (a : Matrix α m n) (h : m > 2 ∧ n > 2 := by grind) : α := a.data[2][2]
def Matrix.zw (a : Matrix α m n) (h : m > 2 ∧ n > 3 := by grind) : α := a.data[2][3]

def Matrix.wx (a : Matrix α m n) (h : m > 3 ∧ n > 0 := by grind) : α := a.data[3][0]
def Matrix.wy (a : Matrix α m n) (h : m > 3 ∧ n > 1 := by grind) : α := a.data[3][1]
def Matrix.wz (a : Matrix α m n) (h : m > 3 ∧ n > 2 := by grind) : α := a.data[3][2]
def Matrix.ww (a : Matrix α m n) (h : m > 3 ∧ n > 3 := by grind) : α := a.data[3][3]

/-- `Matrix T m n` on OpenCL level is modeled as structure with `m` row vectors.  -/
instance [t : AtomicOpenCLType α] [AllowedVectorSize n] : OCLType (Matrix α m n) where
  oclType := .struct {
    name :=
      if m = n then
        s!"matrix{m}{t.shortName}"
      else
        s!"matrix{m}{n}{t.shortName}"
    shortName := s!"{t.shortName}{m}{n}"
    fields :=
      let rowType := OCLType.oclType (α:=Vector α n)
      Array.range m |>.map (fun i => {
        type := rowType
        name := s!"row{i}"
        })
  }


-- General element access
def get (a : Matrix α m n) (i : Fin m) (j : Fin n) : α := a.data[i][j]

def set (a : Matrix α m n) (i : Fin m) (j : Fin n) (val : α) : Matrix α m n :=
  { data := a.data.set i (a.data[i].set j val) }

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

instance [Zero α] [Add α] [Mul α] : HMul (Matrix α m k) (Matrix α k n) (Matrix α m n) where
  hMul := mul

instance [Mul α] : HMul α (Matrix α m n) (Matrix α m n) where
  hMul := scalar_mul

instance [Zero α] : Zero (Matrix α m n) where
  zero := zero m n

instance [Zero α] [One α] : One (Matrix α n n) where
  one := identity n

-- ToString instance for debugging
instance [ToString α] : ToString (Matrix α m n) where
  toString a :=
    "[" ++ String.intercalate ",\n " (a.data.toList.map fun row =>
      "[" ++ String.intercalate ", " (row.toList.map toString) ++ "]") ++ "]"





-- #eval OCLType.oclType (Matrix Int32 3 3)
