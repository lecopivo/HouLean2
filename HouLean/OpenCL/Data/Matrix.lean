import HouLean.OpenCL.Data.Vector
import HouLean.OpenCL.Data.Fin
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
implemented_by [t : AtomicOpenCLType α] [Inhabited α] (n) (xs : List (Vector α n)) (h) :
  Matrix.mk (m:=m) (n:=n) (Vector.mk xs.toArray h)
  =
  (oclFunction (ArgList (Vector α n) → Matrix α m n) s!"(matrix{m}{n}{t.shortName})" .constructor)
  (ArgList.ofList xs)

-- ofFn
attribute [opencl_csimp] Matrix.ofFn
implemented_by [t : AtomicOpenCLType α] [Inhabited α] (f : Fin m → Vector α n) :
  Matrix.mk (m:=m) (n:=n) (.ofFn f)
  =
  (oclFunction (ArgList (Vector α n) → Matrix α m n) s!"(matrix{m}{n}{t.shortName})" .constructor)
  (ArgList.ofFn f)

/-- Same as `Matrix.col` but `i` is implicit argument thus forced to be compile time evaluated.

todo: maybe have a separate mechanism for compile time arguemnts ... -/
def _root_.HouLean.Matrix.colI {j : Nat} (a : Matrix α m n) (h : j < n := by get_elem_tactic) : Vector α m :=
  .ofFn fun i => a[i.1,j]

@[opencl_csimp]
theorem opencl_rewrite_matrix_row [Inhabited α] (a : Matrix α m n) (i : Nat) (h) :
    a.row i h
    =
    (oclFunction (type := Matrix α m n → Vector α n) s!".row{i}" (kind := .postfix))
    a := sorry_proof

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

-- map
implemented_by (a : Matrix α m n) (f : (i : Nat) → Vector α n → (i < m)→ Vector β n) :
    a.mapRowsFinIdx f = let a := a; .mk (.ofFn fun i => f i (a.row i) (by grind))

attribute [opencl_csimp]
  Matrix.mapRowsIdx Matrix.mapRows Matrix.mapRows₂
  Matrix.mapFinIdx Matrix.mapIdx Matrix.map

-- identity
def _root_.HouLean.Matrix.identityN {α} [Zero α] [One α] {n : Nat} : Matrix α n n :=
  .ofFn (fun i j _ => if i = j then 1 else 0)

implemented_by {α} [Zero α] [One α] : Matrix.identity α n = Matrix.identityN (n:=n) := by rfl
implemented_by {α} [Zero α] [One α] : (1 : Matrix α n n) = Matrix.identityN (n:=n) := by rfl

-- zero
def _root_.HouLean.Matrix.zeroN {α} [Zero α] {m n : Nat} : Matrix α m n :=
  .mk (.ofFn fun _ => 0)

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
