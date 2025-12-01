import HouLean.OpenCL.Data.Vector
import HouLean.Data.Matrix

namespace HouLean.OpenCL

open Compiler Qq HouLean Math

namespace Matrix

/-- `Matrix T m n` on OpenCL level is modeled as structure with `m` row vectors.  -/
instance [t : AtomicOpenCLType α] [AllowedVectorSize n] : OpenCLType (Matrix α m n) :=
  let name :=
    --if m = n then
    --  s!"matrix{m}{t.shortName}"
    --else
      s!"matrix{m}{n}{t.shortName}"
  {
    name := name
    shortName := s!"{t.shortName}{m}{n}"
    definition? :=
      let r : OpenCLType (Vector α n) := by infer_instance
      ((Array.range m).foldl (init:=s!"typedef struct\{")
        (fun s i => s ++ s!"\n  {r.name} row{i}")) ++ s!"\n}  {name};"
  }

-- constructor
def matrixMk2 (row0 row1 : Vector α n) : Matrix α 2 n := ⟨#v[row0, row1]⟩
def matrixMk3 (row0 row1 row2 : Vector α n) : Matrix α 3 n := ⟨#v[row0, row1,row2]⟩
def matrixMk4 (row0 row1 row2 row3 : Vector α n) : Matrix α 4 n := ⟨#v[row0, row1,row2,row3]⟩
def matrixMk8 (row0 row1 row2 row3 row4 row5 row6 row7 : Vector α n) : Matrix α 8 n :=
  ⟨#v[row0,row1,row2,row3,row4,row5,row6,row7]⟩
def matrixMk16 (row0 row1 row2 row3 row4 row5 row6 row7
                row8 row9 row10 row11 row12 row13 row14 row15 : Vector α n) : Matrix α 16 n :=
  ⟨#v[row0,row1,row2,row3,row4,row5,row6,row7,
      row8,row9,row10,row11,row12,row13,row14,row15]⟩

section
variable (row0 row1 row2 row3 : Vector α n)
implemented_by : #m[row0,row1] = matrixMk2 row0 row1 := by rfl
implemented_by : #m[row0,row1,row2] = matrixMk3 row0 row1 row2 := by rfl
implemented_by : Matrix.mk (vectorMk3 row0 row1 row2) = matrixMk3 row0 row1 row2 := by rfl
implemented_by : #m[row0,row1,row2,row3] = matrixMk4 row0 row1 row2 row3 := by rfl
end

section
variable [AtomicOpenCLType α] [AllowedVectorSize n]

instance : OpenCLFunction (matrixMk2 (α:=α) (n:=n)) where
  name :=
    let t : OpenCLType (Matrix α 2 n) := by infer_instance
    s!"({t.name})"
  kind := .constructor

instance : OpenCLFunction (matrixMk3 (α:=α) (n:=n)) where
  name :=
    let t : OpenCLType (Matrix α 3 n) := by infer_instance
    s!"({t.name})"
  kind := .constructor

instance : OpenCLFunction (matrixMk4 (α:=α) (n:=n)) where
  name :=
    let t : OpenCLType (Matrix α 4 n) := by infer_instance
    s!"({t.name})"
  kind := .constructor

instance : OpenCLFunction (matrixMk8 (α:=α) (n:=n)) where
  name :=
    let t : OpenCLType (Matrix α 8 n) := by infer_instance
    s!"({t.name})"
  kind := .constructor

instance : OpenCLFunction (matrixMk16 (α:=α) (n:=n)) where
  name :=
    let t : OpenCLType (Matrix α 16 n) := by infer_instance
    s!"({t.name})"
  kind := .constructor


-- Row projection
instance : OpenCLFunction (@Matrix.row0 (α:=α) (m:=m) (n:=n)) where
  name := ".row0"
  kind := .postfix

instance : OpenCLFunction (@Matrix.row1 (α:=α) (m:=m) (n:=n)) where
  name := ".row1"
  kind := .postfix

instance : OpenCLFunction (@Matrix.row2 (α:=α) (m:=m) (n:=n)) where
  name := ".row2"
  kind := .postfix

instance : OpenCLFunction (@Matrix.row3 (α:=α) (m:=m) (n:=n)) where
  name := ".row3"
  kind := .postfix

instance : OpenCLFunction (@Matrix.row4 (α:=α) (m:=m) (n:=n)) where
  name := ".row4"
  kind := .postfix

instance : OpenCLFunction (@Matrix.row5 (α:=α) (m:=m) (n:=n)) where
  name := ".row5"
  kind := .postfix

instance : OpenCLFunction (@Matrix.row6 (α:=α) (m:=m) (n:=n)) where
  name := ".row6"
  kind := .postfix

instance : OpenCLFunction (@Matrix.row7 (α:=α) (m:=m) (n:=n)) where
  name := ".row7"
  kind := .postfix
end

implemented_by (a : Matrix α m n) (h : 0 < m) : a.row 0 = a.row0 := by rfl
implemented_by (a : Matrix α m n) (h : 1 < m) : a.row 1 = a.row1 := by rfl
implemented_by (a : Matrix α m n) (h : 2 < m) : a.row 2 = a.row2 := by rfl
implemented_by (a : Matrix α m n) (h : 3 < m) : a.row 3 = a.row3 := by rfl

implemented_by (a : Matrix α m n) (h : 0 < m) : a.row (⟨0,h⟩ : Fin m) = a.row0 := by rfl
implemented_by (a : Matrix α m n) (h : 1 < m) : a.row (⟨1,h⟩ : Fin m) = a.row1 := by rfl
implemented_by (a : Matrix α m n) (h : 2 < m) : a.row (⟨2,h⟩ : Fin m) = a.row2 := by rfl
implemented_by (a : Matrix α m n) (h : 3 < m) : a.row (⟨3,h⟩ : Fin m) = a.row3 := by rfl

implemented_by (a : Matrix α m n) [OfNat (Fin m) 0] : a.row (0 : Fin m) = a.row0 sorry_proof
implemented_by (a : Matrix α m n) [OfNat (Fin m) 1] : a.row (1 : Fin m) = a.row1 sorry_proof
implemented_by (a : Matrix α m n) [OfNat (Fin m) 2] : a.row (2 : Fin m) = a.row2 sorry_proof
implemented_by (a : Matrix α m n) [OfNat (Fin m) 3] : a.row (3 : Fin m) = a.row3 sorry_proof

implemented_by (a : Matrix α m n) (h : 0 < n) : a.col 0 = a.col0 := by rfl
implemented_by (a : Matrix α m n) (h : 1 < n) : a.col 1 = a.col1 := by rfl
implemented_by (a : Matrix α m n) (h : 2 < n) : a.col 2 = a.col2 := by rfl
implemented_by (a : Matrix α m n) (h : 3 < n) : a.col 3 = a.col3 := by rfl

implemented_by (a : Matrix α m n) (h : 0 < n) : a.col (⟨0,h⟩ : Fin n) = a.col0 := by rfl
implemented_by (a : Matrix α m n) (h : 1 < n) : a.col (⟨1,h⟩ : Fin n) = a.col1 := by rfl
implemented_by (a : Matrix α m n) (h : 2 < n) : a.col (⟨2,h⟩ : Fin n) = a.col2 := by rfl
implemented_by (a : Matrix α m n) (h : 3 < n) : a.col (⟨3,h⟩ : Fin n) = a.col3 := by rfl

implemented_by (a : Matrix α m n) [OfNat (Fin n) 0] : a.col (0 : Fin n) = a.col0 sorry_proof
implemented_by (a : Matrix α m n) [OfNat (Fin n) 1] : a.col (1 : Fin n) = a.col1 sorry_proof
implemented_by (a : Matrix α m n) [OfNat (Fin n) 2] : a.col (2 : Fin n) = a.col2 sorry_proof
implemented_by (a : Matrix α m n) [OfNat (Fin n) 3] : a.col (3 : Fin n) = a.col3 sorry_proof

implemented_by (a : Matrix α m n) (ij : Nat×Nat) (h) : a[ij]'h = (a.row ij.1)[ij.2] := by rfl
implemented_by (a : Matrix α m n) (i : Fin m) (j : Nat) (h) : a[(i.1,j)]'h = (a.row i)[j] := by rfl
implemented_by (a : Matrix α m n) (i : Nat) (j : Fin n) (h) : a[(i,j.1)]'h = (a.row i)[j] := by rfl
implemented_by (a : Matrix α m n) (i : Fin m) (j : Fin n) : a[(i.1,j.1)] = (a.row i)[j] := by rfl


-- ofFn
implemented_by (f : (i j : Nat) → (h : i < 2 ∧ j < n) → α) :
  Matrix.ofFn f = #m[.ofFn (fun j => f 0 j (by grind)),
                     .ofFn (fun j => f 1 j (by grind))]
implemented_by (f : (i j : Nat) → (h : i < 3 ∧ j < n) → α) :
  Matrix.ofFn f = #m[.ofFn (fun j => f 0 j (by grind)),
                     .ofFn (fun j => f 1 j (by grind)),
                     .ofFn (fun j => f 2 j (by grind))]
implemented_by (f : (i j : Nat) → (h : i < 4 ∧ j < n) → α) :
  Matrix.ofFn f = #m[.ofFn (fun j => f 0 j (by grind)),
                     .ofFn (fun j => f 1 j (by grind)),
                     .ofFn (fun j => f 2 j (by grind)),
                     .ofFn (fun j => f 3 j (by grind))]

-- map
implemented_by (a : Matrix α 2 n) (f : α → β) :
    a.map f = #m[a.row0.map f, a.row1.map f]
implemented_by (a : Matrix α 3 n) (f : α → β) :
    a.map f = #m[a.row0.map f, a.row1.map f, a.row2.map f]
implemented_by (a : Matrix α 4 n) (f : α → β) :
    a.map f = #m[a.row0.map f, a.row1.map f, a.row2.map f, a.row3.map f]

-- mapRows
implemented_by (a : Matrix α 2 n) (f : Vector α n → Vector β n) :
    a.mapRows f = #m[f a.row0, f a.row1]
implemented_by (a : Matrix α 3 n) (f : Vector α n → Vector β n) :
    a.mapRows f = #m[f a.row0, f a.row1, f a.row2]
implemented_by (a : Matrix α 4 n) (f : Vector α n → Vector β n) :
    a.mapRows f = #m[f a.row0, f a.row1, f a.row2, f a.row3]

-- mapRows₂
implemented_by (a : Matrix α 2 n) (b : Matrix β 2 n) (f : Vector α n → Vector β n → Vector γ n) :
    a.mapRows₂ f b = #m[f a.row0 b.row0, f a.row1 b.row1]
implemented_by (a : Matrix α 3 n) (b : Matrix β 3 n) (f : Vector α n → Vector β n → Vector γ n) :
    a.mapRows₂ f b = #m[f a.row0 b.row0, f a.row1 b.row1, f a.row2 b.row2]
implemented_by (a : Matrix α 4 n) (b : Matrix β 4 n) (f : Vector α n → Vector β n → Vector γ n) :
    a.mapRows₂ f b = #m[f a.row0 b.row0, f a.row1 b.row1, f a.row2 b.row2, f a.row3 b.row3]
