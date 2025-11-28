import HouLean.OpenCL.Compiler.Main
import HouLean.OpenCL.Basic
import HouLean.OpenCL.Data.Int
import HouLean.OpenCL.Data.Vector3
import HouLean.OpenCL.Data.Vector

namespace HouLean.OpenCL

open Compiler Qq

abbrev Float64 := Float

class OCLType (α : Type) where
  oclType : OpenCLType

/-- Atomic OpenCL types are char, uchar, short, ushort, int, uint, long, ulong, float, double

These are the types we can have pointers to and can have short vector version of, like `float3` -/
class AtomicOpenCLType (T : Type) where
  name : String
  shortName : String

instance : AtomicOpenCLType Float64 where
  name := "double"
  shortName := "d"

instance : AtomicOpenCLType Float32 where
  name := "float"
  shortName := "f"

instance : AtomicOpenCLType Int32 where
  name := "int"
  shortName := "i"

instance : AtomicOpenCLType Int64 where
  name := "long"
  shortName := "l"

instance : AtomicOpenCLType UInt32 where
  name := "uint"
  shortName := "ui"

instance : AtomicOpenCLType UInt64 where
  name := "ulong"
  shortName := "ul"

/-- OpenCL short vector types are allowed of only a specific length. This class allows to enforce
this restriction on the size `n`. -/
class AllowedVectorSize (n : Nat) where
  valid : n = 2 || n = 3 || n = 4 || n = 8 || n = 16

instance : AllowedVectorSize 2 := ⟨by simp⟩
instance : AllowedVectorSize 3 := ⟨by simp⟩
instance : AllowedVectorSize 4 := ⟨by simp⟩
instance : AllowedVectorSize 8 := ⟨by simp⟩
instance : AllowedVectorSize 16 := ⟨by simp⟩

instance [t : AtomicOpenCLType T] [AllowedVectorSize n] : OCLType (Vector T n) where
  oclType := .atom s!"{t.name}{n}" s!"{t.shortName}{n}"

class OCLFunction {F : Type} (f : F) where
  oclFunction : OpenCLFunction


----------------------------------------------------------------------------------------------------
-- array types -------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

instance [AtomicOpenCLType T] : ArrayType T where
  get := sorry -- a[i]
  set := sorry -- (s[i] := 1)

instance [AtomicOpenCLType T] [AllowedVectorSize n] : ArrayType (Vector T n) where
  get := sorry -- vloadn
  set := sorry -- vstoren

instance [AtomicOpenCLType T] : OCLFunction (ArrayType.get (α:=T)) where
  oclFunction := sorry

instance [AtomicOpenCLType T] : OCLFunction (ArrayType.set (α:=T)) where
  oclFunction := sorry

instance [AtomicOpenCLType T] [AllowedVectorSize n] : OCLFunction (ArrayType.get (α:=Vector T n)) where
  oclFunction := sorry

instance [AtomicOpenCLType T] [AllowedVectorSize n] : OCLFunction (ArrayType.set (α:=Vector T n)) where
  oclFunction := sorry


-- instance {n : Nat}  [OCLType T] [ArrayType (Vector T n)] : OCLFunction (ArrayType.get (α:=Vector T n)) where
--   oclFunction :=

-- -- todo: remove these, they should be automatic
-- run_meta compileFunction q(fun (arr : ArrayRef Vector3) idx => ArrayType.get arr idx)
-- run_meta compileFunction q(fun (arr : ArrayRef Vector3) idx val => ArrayType.set arr idx val)
