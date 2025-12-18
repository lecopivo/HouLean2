import HouLean.Data.Matrix
import HouLean.OpenCL.Compiler
import HouLean.OpenCL.Data.ArrayType
import HouLean.OpenCL.Data.Fin
import HouLean.OpenCL.Data.Vector
import HouLean.OpenCL.Reference

import HouLean.OpenCL.Data.Pointer
import HouLean.OpenCL.Compiler

namespace HouLean.OpenCL

open Lean Meta Qq Math Compiler

namespace Matrix

open Lean  in
def matrixTypeName (elem : Name) (m n : Nat) : Name :=
  let elem := elem.toString
  .mkSimple <| s!"matrix{m}{n}{elem}"

/- `Matrix T m n` is compiled to `Tn` e.g. `Vector Float32 3 ==> float3` -/
impl_by {m n : Nat} {T} [AtomicOpenCLType T] [AllowedVectorSize n] : Matrix T m n ==> do

  let some m ← runInterpreter? Nat m
    | throwError m!"Number of rows {m} of a matrix has to be known at compile time!"
  let some n ← runInterpreter? Nat n
    | throwError m!"Number of columns {n} of a matrix has to be known at compile time!"
  let type ← compileType T
  let name := matrixTypeName type.name m n

  return { name }


/- Matrix contructor rule  e.g. #m[row0,row1,row2] ==> (matrix33){row0,row1,row2}  -/
impl_by {m n : Nat} {α : Type} [AtomicOpenCLType α] (l : List (Vector α n)) (h) :
    Matrix.mk (m:=m) (Vector.mk l.toArray h) ==> do

  let xs ← splitList l
  let xs ← xs.mapM compileExpr
  let t ← compileType α

  let some m ← runInterpreter? Nat m
    | throwError m!"Number of rows {m} of a matrix has to be known at compile time!"
  let some n ← runInterpreter? Nat n
    | throwError m!"Number of columns {n} of a matrix has to be known at compile time!"
  let matrixId := mkIdent (matrixTypeName t.name m n)

  return ← `(clExpr| ($matrixId:ident){$[$xs:clExpr],*} )

/- Matrix projection rule  e.g. a.row 2 ==> a.row2  -/
impl_by {m n : Nat} {T} (a : Matrix T m n) (i) (h) : a.row i h  ==> do

  let a ← compileExpr a
  let some i ← runInterpreter? Nat i
    | throwError m!"Number of rows {m} of a matrix has to be known at compile time!"
  let proj := mkIdent (Name.appendAfter `row (toString i))

  return ← `(clExpr| $a:clExpr.$proj:ident)


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




-- why do we have to unfold this?
def vload [Inhabited α] [Zero α] [AtomicOpenCLType α] [AllowedVectorSize n]
    (ptr : Pointer α) (off : UInt64) : OpenCLM (Matrix α m n) := do
  return Matrix.mk (← Vector.ofFnM fun i =>
    ArrayType.get (Elem:=Vector α n) ptr ((off.toUInt32 * m.toUInt32 + i.1.toUInt32).toUInt64))

def vstore [Inhabited α] [Zero α] [AtomicOpenCLType α] [AllowedVectorSize n]
    (ptr : Pointer α) (off : UInt64) (value : Matrix α m n) : OpenCLM Unit := do
  Fin.foldlM m (init:=()) (fun _ i =>
    ArrayType.set ptr ((off.toUInt32 * m.toUInt32 + i.1.toUInt32).toUInt64) (value.row i))

instance [Inhabited α] [Zero α] [AtomicOpenCLType α] [AllowedVectorSize n] :
    ArrayType (Matrix α m n) (Pointer α) where
  get := vload
  set := vstore


def vload' [Inhabited α] [Zero α] [AtomicOpenCLType α] [AllowedVectorSize n] {addr const restrict}
    (ptr : DPointer α addr const restrict) (off : USize) : OpenCLM (Matrix α m n) := do
  return Matrix.mk (← Vector.ofFnM fun i =>
    ptr.vload n ((off + i.1.toUSize) * n.toUSize))

def vstore' [Inhabited α] [Zero α] [AtomicOpenCLType α] [AllowedVectorSize n] {addr restrict}
    (ptr : DPointer α addr (const:=false) restrict) (off : USize) (value : Matrix α m n) : OpenCLM Unit := do
  Fin.foldlM m (init:=()) fun _ i =>
  -- unroll for h : i in [0:m] do
    ptr.vstore ((off + i.1.toUSize) * n.toUSize) (value.row i)

-- set_option pp.funBinderTypes true in
-- set_option trace.HouLean.OpenCL.compiler true in
-- #opencl_compile vstore' (α:=Float32) (m:=3) (n:=3) (addr:=.global) (restrict:=true)
-- #opencl_compile vload' (α:=Float32) (m:=4) (n:=3) (addr:=.global) (restrict:=true) (const:=true)


-- -- vecMul
-- /-- Likely a better implementation of `vecMul` for OpenCL -/
-- def _root_.HouLean.Matrix.vecMul_sum_rows [Add α] [Zero α] [Mul α] (v : Vector α m) (a : Matrix α m n) : Vector α n :=
--   sum fun j : Fin m => v[j] * a.row j
-- implemented_by [Add α] [Zero α] [Mul α] (v : Vector α m) (a : Matrix α m n) : a.vecMul v = a.vecMul_sum_rows v
