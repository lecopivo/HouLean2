import HouLean.OpenCL.Data.Vector
import HouLean.OpenCL.Data.Fin
import HouLean.OpenCL.Data.Int
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

-- why do we have to unfold this?
@[opencl_csimp]
def vload [Inhabited α] [Zero α] [AtomicOpenCLType α] [AllowedVectorSize n]
    (ptr : Pointer α) (off : UInt64) : OpenCLM (Matrix α m n) := do
  return Matrix.mk (← Vector.ofFnM fun i =>
    ArrayType.get (α:=Vector α n) ptr ((off.toUInt32 * m.toUInt32 + i.1.toUInt32).toUInt64))

def vstore [Inhabited α] [Zero α] [AtomicOpenCLType α] [AllowedVectorSize n]
    (ptr : Pointer α) (off : UInt64) (value : Matrix α m n) : OpenCLM Unit := do
  Fin.foldlM m (init:=()) (fun _ i =>
    ArrayType.set ptr ((off.toUInt32 * m.toUInt32 + i.1.toUInt32).toUInt64) (value.row i))

-- why do we have to unfold this?
@[reducible]
instance [Inhabited α] [Zero α] [AtomicOpenCLType α] [AllowedVectorSize n] : ArrayType (Matrix α m n) α where
  get := vload
  set := vstore

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
implemented_by : #m[row0,row1,row2,row3] = matrixMk4 row0 row1 row2 row3 := by rfl

implemented_by : Matrix.mk (vectorMk2 row0 row1) = matrixMk2 row0 row1 := by rfl
implemented_by : Matrix.mk (vectorMk3 row0 row1 row2) = matrixMk3 row0 row1 row2 := by rfl
implemented_by : Matrix.mk (vectorMk4 row0 row1 row2 row3) = matrixMk4 row0 row1 row2 row3 := by rfl

implemented_by (f : Fin 2 → Vector α n) : Matrix.mk (.ofFn f) = matrixMk2 (f 0) (f 1) := by rfl
implemented_by (f : Fin 3 → Vector α n) : Matrix.mk (.ofFn f) = matrixMk3 (f 0) (f 1) (f 2) := by rfl
implemented_by (f : Fin 4 → Vector α n) : Matrix.mk (.ofFn f) = matrixMk4 (f 0) (f 1) (f 2) (f 3) := by rfl
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

end


/-- Same as `Matrix.col` but `i` is implicit argument thus forced to be compile time evaluated.

todo: maybe have a separate mechanism for compile time arguemnts ... -/
def _root_.HouLean.Matrix.colI {j : Nat} (a : Matrix α m n) (h : j < n := by get_elem_tactic) : Vector α m :=
  .ofFn fun i => a[i.1,j]

@[opencl_csimp]
theorem opencl_rewrite_matrix_row [Inhabited α] (a : Matrix α m n) (i : Nat) (h) :
    a.row i h
    =
    (a |> oclFunction (type := Matrix α m n → Vector α n) s!".row{i}" (kind := .postfix)) := sorry_proof

@[opencl_csimp]
theorem opencl_rewrite_matrix_col (a : Matrix α m n) (j : Nat) (h) :
    a.col j h
    =
    a.colI (j:=j) h := by rfl

@[opencl_csimp]
theorem opencl_rewrite_matrix_getElem (a : Matrix α m n) (ij : Nat×Nat) (h) :
    a[ij]'h
    =
    (a.row ij.1)[ij.2] := by rfl


attribute [opencl_csimp] Matrix.ofFn

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
implemented_by (a : Matrix α m n) (f : α → β) :
    a.map f = a.mapRows (fun row => row.map f)

-- mapRows
implemented_by (a : Matrix α 2 n) (f : Vector α n → Vector β n) :
    a.mapRows f = #m[f (a.row 0), f (a.row 1)]
implemented_by (a : Matrix α 3 n) (f : Vector α n → Vector β n) :
    a.mapRows f = #m[f (a.row 0), f (a.row 1), f (a.row 2)]
implemented_by (a : Matrix α 4 n) (f : Vector α n → Vector β n) :
    a.mapRows f = #m[f (a.row 0), f (a.row 1), f (a.row 2), f (a.row 3)]

-- mapRows₂
implemented_by (a : Matrix α 2 n) (b : Matrix β 2 n) (f : Vector α n → Vector β n → Vector γ n) :
    a.mapRows₂ f b = #m[f (a.row 0) (b.row 0), f (a.row 1) (b.row 1)]
implemented_by (a : Matrix α 3 n) (b : Matrix β 3 n) (f : Vector α n → Vector β n → Vector γ n) :
    a.mapRows₂ f b = #m[f (a.row 0) (b.row 0), f (a.row 1) (b.row 1), f (a.row 2) (b.row 2)]
implemented_by (a : Matrix α 4 n) (b : Matrix β 4 n) (f : Vector α n → Vector β n → Vector γ n) :
    a.mapRows₂ f b = #m[f (a.row 0) (b.row 0), f (a.row 1) (b.row 1), f (a.row 2) (b.row 2), f (a.row 3) (b.row 3)]


-- identity
def _root_.HouLean.Matrix.identityN {α} [Zero α] [One α] {n : Nat} : Matrix α n n :=
  .ofFn (fun i j _ => if i = j then 1 else 0)

implemented_by {α} [Zero α] [One α] : Matrix.identity α n = Matrix.identityN (n:=n) := by rfl
implemented_by {α} [Zero α] [One α] : (1 : Matrix α n n) = Matrix.identityN (n:=n) := by rfl

-- zero
def _root_.HouLean.Matrix.zeroN {α} [Zero α] {m n : Nat} : Matrix α m n :=
  .ofFn (fun _ _ _ => 0)

implemented_by {α} [Zero α] : Matrix.zero α m n = Matrix.zeroN (α := α) (m:=m) (n:=n) := by rfl
implemented_by {α} [Zero α] : (0 : Matrix α m n) = Matrix.zeroN (α := α) (m:=m) (n:=n) := by rfl

-- add works
-- sub works
-- smul works
-- sdiv works
-- matMul works

-- vecMul
/-- Likely a better implementation of `vecMul` for OpenCL -/
def _root_.HouLean.Matrix.vecMul_sum_rows [Add α] [Zero α] [Mul α] (v : Vector α m) (a : Matrix α m n) : Vector α n :=
  sum fun j : Fin m => v[j] * a.row j
implemented_by [Add α] [Zero α] [Mul α] (v : Vector α m) (a : Matrix α m n) : a.vecMul v = a.vecMul_sum_rows v
