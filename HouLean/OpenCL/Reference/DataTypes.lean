import HouLean.OpenCL.Compiler
import HouLean.OpenCL.Basic

namespace HouLean.OpenCL

open Compiler Meta Lean

variable {α : Type} [AtomicOpenCLType α] {n : Nat}


----------------------------------------------------------------------------------------------------
-- Built-in Scalar Data Types ----------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

impl_type_by Bool ==> bool
--impl_type_by Char ==> char
impl_type_by Int16 ==> short
impl_type_by UInt16 ==> ushort
impl_type_by Int32 ==> int
impl_type_by UInt32 ==> uint
impl_type_by Int ==> int
impl_type_by Nat ==> uint
impl_type_by Int64 ==> long
impl_type_by UInt64 ==> uong
impl_type_by Float32 ==> float
impl_type_by Float64 ==> double
-- impl_type_by Float16 ==> half
impl_type_by USize ==> size_t
impl_type_by Unit ==> void



----------------------------------------------------------------------------------------------------
-- Built-in Vector Data Types ----------------------------------------------------------------------
----------------------------------------------------------------------------------------------------


/- `Vector T n` is compiled to `Tn` e.g. `Vector Float32 3 ==> float3` -/
impl_by {n : Nat} {T} [AtomicOpenCLType T] [AllowedVectorSize n] : Vector T n ==> do do

  let some n ← runInterpreter? Nat n
    | throwError m!"Size {n} of vector has to be known at compile time!"
  let type ← compileType T

  let name := type.name.appendAfter (toString n)

  return {
    quals := #[]
    name := name
    pointer := false
  }


/- Vector contructor rule  e.g. #v[x,y,z] ==> (float3){x,y,z}

The notation `#v[x,y,z]` stands for `Vector.mk [x,y,z].toArray h` and this rule matches on this
irrespective of the length of the list.
 -/
set_option pp.all true in
impl_by {n : Nat} {α : Type} [AtomicOpenCLType α] [AllowedVectorSize n] (l : List α) (h) :
    Vector.mk (n:=n) l.toArray h ==> do

  let xs ← splitList l
  let xs ← xs.mapM compileExpr
  let type ← compileType α

  let some n ← runInterpreter? Nat n
    | throwError m!"Size {n} of vector has to be known at compile time!"
  let name := mkIdent <| type.name.appendAfter (toString n)

  return ← `(clExpr| ($name){$[$xs:clExpr],*} )
