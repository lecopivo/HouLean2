import HouLean.OpenCL.Compiler.RewriteRules

open Lean HouLean Meta OpenCL Compiler Compiler3 Math

namespace HouLean.OpenCL

variable {α : Type} [AtomicOpenCLType α] {n : Nat}



----------------------------------------------------------------------------------------------------
-- Built-in Scalar Data Types ----------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

impl_by : Bool ==> bool
--impl_by : Char ==> char
impl_by : Int16 ==> short
impl_by : UInt16 ==> ushort
impl_by : Int32 ==> int
impl_by : UInt32 ==> uint
impl_by : Int ==> int
impl_by : Nat ==> uint
impl_by : Int64 ==> long
impl_by : UInt64 ==> uong
impl_by : Float32 ==> float
impl_by : Float64 ==> double
-- impl_by : Float16 ==> half
impl_by : USize ==> size_t
impl_by : Unit ==> void



----------------------------------------------------------------------------------------------------
-- Built-in Vector Data Types ----------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

/- `Vector T n` is compiled to `Tn` e.g. `Vector Float32 3 ==> float3` -/
impl_by {n : Nat} {T} [AtomicOpenCLType T] [AllowedVectorSize n] : Vector T n ==> do

  let some n ← runInterpreter? Nat n
    | throwError m!"Size {n} of vector has to be known at compile time!"
  let type ← compileType T

  let name := mkIdent <| type.getId.appendAfter (toString n)

  return ← `(oclExpr| $name:ident)


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
  let name := mkIdent <| type.getId.appendAfter (toString n)

  return ← `(oclExpr| ($name){$xs:oclExpr,*} )
