import HouLean.OpenCL.Compiler
import HouLean.OpenCL.Compiler.Compile
import HouLean.OpenCL.Reference


namespace HouLean.OpenCL



inductive PointerAddressSpace where
  | global | loc | priv | default
deriving Lean.ToExpr

opaque DPointer.nonemptyType (T : Type) (addr : PointerAddressSpace) (const : Bool) (restrict : Bool) : NonemptyType.{0}
set_option linter.unusedVariables false in
/-- Decorated pointer -/
def DPointer (T : Type) (addr : PointerAddressSpace := .default) (const : Bool := false) (restrict : Bool := false) : Type :=
  (DPointer.nonemptyType T addr const restrict).type


namespace DPointer

variable {T : Type} {addr : PointerAddressSpace} {const : Bool} {restrict : Bool}

instance : Nonempty (DPointer T addr const restrict) :=
  by exact (DPointer.nonemptyType T addr const restrict).property

opaque get [Inhabited T] (a : DPointer T addr const restrict) (offset : USize) : OpenCLM T
opaque set (a : DPointer T addr (const := false) restrict) (offset : USize) (val : T) : OpenCLM Unit

opaque vload [Inhabited T] (a : DPointer T addr const restrict) (n : Nat) (offset : USize) : OpenCLM (Vector T n)
opaque vstore {n} (a : DPointer T addr (const := false) restrict) (offset : USize) (val : Vector T n) : OpenCLM Unit


impl_by : OpenCLM T ==> T

open Compiler Lean Qq Meta
impl_by : DPointer T addr const restrict ==> do
  do

  let t ← compileType T

  let some addr ← Meta.runInterpreter? PointerAddressSpace addr
    | throwError m!"Pointer address space, {addr}, needs to be know at compile time!"
  let gl ← `(clTypeQ| global)
  let lo ← `(clTypeQ| local)
  let pr ← `(clTypeQ| private)
  let a? : Option (TSyntax `clTypeQ)  :=
    match addr with
    | PointerAddressSpace.global => some gl
    | PointerAddressSpace.loc => some lo
    | PointerAddressSpace.priv => some pr
    | PointerAddressSpace.default => none

  let some const ← Meta.runInterpreter? Bool const
    | throwError m!"it needs to be known at compile time if a pointer is constant or not!"
  let c ← `(clTypeQ| const)
  let c? := if const then some c else none


  let some restrict ← Meta.runInterpreter? Bool restrict
    | throwError m!"it needs to be known at compile time if a pointer is restrict or not!"
  let r ← `(clTypeQ| restrict)
  let r? := (if restrict then some r else none)


  -- Build specs array
  let mut specs : Array (TSyntax `clSpec) := #[]
  if let some a := a? then specs := specs.push ⟨a.raw⟩
  if let some c := c? then specs := specs.push ⟨c.raw⟩
  -- Assuming t is a clTypeSpec, coerce it to clSpec:
  let t ← `(clSpec| $t:ident)
  specs := specs.push t

  -- Build pointer qualifiers
  let ptrQuals : Array (TSyntax `clTypeQ) :=
    if let some r := r? then #[r] else #[]

  -- Build the type with explicit structure
  let ty ← `(clType| $specs:clSpec* * $ptrQuals:clTypeQ*)

  let ptrId := mkIdent (.mkSimple (s!"{ty.raw.prettyPrint}"))

  -- todo: add option to return type not only expression
  return ← `(clExpr| $ptrId:ident)



variable [Inhabited T]

impl_by (ptr : DPointer T addr const restrict) (off : USize) :
    ptr.get off ==> ptr[off]

impl_by (ptr : DPointer T addr (const:=false) restrict) (off : USize) (val : T) :
    ptr.set off val ==> ptr[off] = val


impl_by (ptr : DPointer T addr const restrict) (off : USize) (n : Nat) :
    ptr.vload n off ==> do

  let some n ← Meta.runInterpreter? Nat n
    | throwError "Function `vload` needs to know the vector size, {n}, at compile time!"

  let vloadId := mkIdent (.mkSimple s!"vload{n}")
  let ptr ← compileExpr ptr
  let off ← compileExpr off

  return ← `(clExpr| $vloadId:ident($off, $ptr))

impl_by {n} (ptr : DPointer T addr (const:=false) restrict) (off : USize) (val : Vector T n) :
    ptr.vstore off val ==> do

  let some n ← Meta.runInterpreter? Nat n
    | throwError "Function `vload` needs to know the vector size, {n}, at compile time!"

  let vstoreId := mkIdent (.mkSimple s!"vstore{n}")
  let ptr ← compileExpr ptr
  let off ← compileExpr off
  let val ← compileExpr val

  return ← `(clExpr| $vstoreId:ident($val, $off, $ptr))




end DPointer


/--
info: « const  float  * » main(« const  float  * » ptr){
      return ptr;
}
-/
#guard_msgs in
#opencl_compile (fun ptr : DPointer Float32 (const := true) => ptr)


/--
info: float main(« float  * » ptr){
      return ptr[0];
}
-/
#guard_msgs in
#opencl_compile (fun ptr : DPointer Float32 => ptr.get 0)


/--
info: void main(« global  float  *  restrict » ptr, float x){
      const float x0 = ptr[0];
      const float x1 = ptr[1];
      ptr[2] = x0 + x1 + x;
      return ptr[3] = x0 * x1 * x;
}
-/
#guard_msgs in
#opencl_compile ((fun (ptr : DPointer Float32 (addr:=.global) (restrict:=true)) (x : Float32) => do
  let x0 ← ptr.get 0
  let x1 ← ptr.get 1
  ptr.set 2 (x0 + x1 + x)
  ptr.set 3 (x0 * x1 * x)))
