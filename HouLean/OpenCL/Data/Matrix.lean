import HouLean.OpenCL.Data.Vector
import HouLean.OpenCL.Data.Fin
-- import HouLean.OpenCL.Data.ArrayType
import HouLean.OpenCL.Reference
import HouLean.Data.Matrix

namespace HouLean.OpenCL

open Compiler Qq HouLean Math

namespace Matrix

open Lean  in
def matrixTypeIdent (elem : String) (m n : Nat) : Ident :=
  mkIdent <| .mkSimple <| s!"matrix{m}{n}{elem}"

open Lean Meta Compiler3 in
/- `Matrix T m n` is compiled to `Tn` e.g. `Vector Float32 3 ==> float3` -/
impl_by {m n : Nat} {T} [AtomicOpenCLType T] [AllowedVectorSize n] : Matrix T m n ==> do

  let some m ← runInterpreter? Nat m
    | throwError m!"Number of rows {m} of a matrix has to be known at compile time!"
  let some n ← runInterpreter? Nat n
    | throwError m!"Number of columns {n} of a matrix has to be known at compile time!"
  let type ← compileType T

  let id := matrixTypeIdent s!"{type}" m n

  return ← `(oclExpr| $id:ident)


/- Matrix contructor rule  e.g. #m[row0,row1,row2] ==> (matrix33){row0,row1,row2}  -/
open Meta Compiler3 in
impl_by {m n : Nat} {α : Type} [AtomicOpenCLType α] (l : List (Vector α n)) (h) :
    Matrix.mk (m:=m) (Vector.mk l.toArray h) ==> do

  let xs ← splitList l
  let xs ← xs.mapM compileExpr
  let t ← compileType α

  let some m ← runInterpreter? Nat m
    | throwError m!"Number of rows {m} of a matrix has to be known at compile time!"
  let some n ← runInterpreter? Nat n
    | throwError m!"Number of columns {n} of a matrix has to be known at compile time!"
  let matrixId := matrixTypeIdent s!"{t}" m n

  return ← `(oclExpr| ($matrixId){$xs:oclExpr,*} )

/- Matrix projection rule  e.g. a.row 2 ==> a.row2  -/
open Lean Meta Compiler3 in
impl_by {m n : Nat} {T} (a : Matrix T m n) (i) (h) : a.row i h  ==> do

  let a ← compileExpr a
  let some i ← runInterpreter? Nat i
    | throwError m!"Number of rows {m} of a matrix has to be known at compile time!"
  let proj := mkIdent (Name.appendAfter `row (toString i))

  return ← `(oclExpr| $a:oclExpr.$proj:ident)


@[opencl_csimp]
theorem matrix_getElem_nat_nat (a : Matrix T m n) (ij : Nat×Nat) (h) : a[ij]'h = (a.row ij.1)[ij.2] := by rfl
@[opencl_csimp]
theorem matrix_getElem_fin_nat (a : Matrix T m n) (ij : Fin m'×Nat) (h) : a[ij]'h = (a.row ij.1)[ij.2] := by rfl
@[opencl_csimp]
theorem matrix_getElem_nat_fin (a : Matrix T m n) (ij : Nat×Fin n') (h) : a[ij]'h = (a.row ij.1)[ij.2] := by rfl
@[opencl_csimp]
theorem matrix_getElem_fin_fin (a : Matrix T m n) (ij : Fin m'×Fin n') (h) : a[ij]'h = (a.row ij.1)[ij.2] := by rfl

@[opencl_csimp]
theorem matrix_mapRowsFinIdx (f : (i : Nat) →  Vector α n → (h : i < m) → Vector β n') (a : Matrix α m n) :
  a.mapRowsFinIdx f = Matrix.fromRows fun i h => f i (a.row i) h := by sorry_proof



#exit
-- why do we have to unfold this?
@[opencl_csimp]
def vload [Inhabited α] [Zero α] [AtomicOpenCLType α] [AllowedVectorSize n]
    (ptr : Pointer α) (off : UInt64) : OpenCLM (Matrix α m n) := do
  return Matrix.mk (← Vector.ofFnM fun i =>
    ArrayType.get (Elem:=Vector α n) ptr ((off.toUInt32 * m.toUInt32 + i.1.toUInt32).toUInt64))

def vstore [Inhabited α] [Zero α] [AtomicOpenCLType α] [AllowedVectorSize n]
    (ptr : Pointer α) (off : UInt64) (value : Matrix α m n) : OpenCLM Unit := do
  Fin.foldlM m (init:=()) (fun _ i =>
    ArrayType.set ptr ((off.toUInt32 * m.toUInt32 + i.1.toUInt32).toUInt64) (value.row i))

-- why do we have to unfold this?
@[reducible]
instance [Inhabited α] [Zero α] [AtomicOpenCLType α] [AllowedVectorSize n] :
    ArrayType (Matrix α m n) (Pointer α) where
  get := vload
  set := vstore

-- constructor
implemented_by [t : AtomicOpenCLType α] [Inhabited α] (n) (xs : List (Vector α n)) (h) :
  Matrix.mk (m:=m) (n:=n) (Vector.mk xs.toArray h)
  =
  (oclFunction (List (Vector α n) → Matrix α m n) s!"(matrix{m}{n}{t.shortName})" .constructor)
  (argList xs)

-- ofFn
attribute [opencl_csimp] Matrix.ofFn
implemented_by [t : AtomicOpenCLType α] [Inhabited α] (f : Fin m → Vector α n) :
  Matrix.mk (m:=m) (n:=n) (.ofFn f)
  =
  (oclFunction (List (Vector α n) → Matrix α m n) s!"(matrix{m}{n}{t.shortName})" .constructor)
  (argList (List.ofFn f))


open Lean Elab Term Meta in
elab c:"#ocl_compile_expr" f:term : command => do
  Command.runTermElabM fun _ => do
    let f ← elabTermAndSynthesize f none
    lambdaTelescope f fun _ b => do
      let stx ← Compiler3.compileExpr b
      logInfoAt c stx



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

-- It is very important to have `↓` here to prevent the simplifier to turn the
-- index product `id : Nat×Nat` into OpenCL implementation.
@[opencl_csimp ↓]
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
