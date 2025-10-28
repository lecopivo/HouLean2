import Lean
-- import HouLean.ArrayTree
-- import HouLean.Data

open Lean

namespace HouLean.Apex

/-- List of element of type `α` whose length `n` has to be known at the APEX compile time. -/
def VariadicArg (α : Type u) (n : Nat) := Vector α n

instance {n} {α} [Inhabited α] : Inhabited (VariadicArg α n) := by 
  unfold VariadicArg; infer_instance

instance : CoeDep (List α) [] (VariadicArg α 0) where
  coe := #v[]
instance (x0 : α) : CoeDep (List α) [x0] (VariadicArg α 1) where
  coe := #v[x0]
instance (x0 x1 : α) : CoeDep (List α) [x0,x1] (VariadicArg α 2) where
  coe := #v[x0,x1]
instance (x0 x1 x2 : α) : CoeDep (List α) [x0,x1,x2] (VariadicArg α 3) where
  coe := #v[x0,x1,x2]
-- ...

namespace Compiler

/-- Name of builtin APEX type. -/
abbrev TypeName := String

inductive PortType where
  | builtin  (typeName : TypeName)
  | variadic (elemTypeName : TypeName)
  | rundata
  | undefined
deriving Inhabited, BEq

inductive PortDir where
  | input | output
deriving Inhabited, BEq

structure LocalPort where
  /-- local id of a port within node -/
  localId : Nat
  name : String
  type : PortType
  dir : PortDir
deriving Inhabited

structure Port extends LocalPort where
  /-- Global id of a port, can be invalid if port is only part of a node specification. -/
  globalId : Nat
  /-- Global node id of a port, can be invalid if port is only part of a node specification. -/
  nodeId : Nat
deriving Inhabited

/-- APEX type that corresponds to a Lean type. Exact size of variadic types might not be known. -/
inductive ApexType where
  | builtin (typeName : TypeName)
  /-- Variadic type with size expressed as `Expr`. 

  During compilation to APEX graph this `n` has to be a literal value. -/
  | variadic (elemTypeName : TypeName) (n : Expr)
  -- stores field name and name of the builtin type
  | struct (bundle : ArrayTree (String×TypeName))
deriving Inhabited, BEq

/-- APEX type that corresponds to a Lean type. Size of variadic types is known.

default name of variable of this type and its builtin type name
 -/
abbrev ApexStaticType := ArrayTree (String × TypeName)

structure ApexFunType where
  inputs : Array ApexType
  output : ApexType
deriving Inhabited, BEq

structure NodeType where
  name : String
  ports : Array LocalPort
deriving Inhabited

structure AddSubPortSpec where
  variadicPortLocalId : Nat
  /-- Some variadic ports are inputs and create the same amount of output subports 
  For example `ForBagin.__spare__` port 2 is input and port 5 is corresponding output -/ 
  outputPortId : Option Nat 
  /-- Names of all subports to be creates. -/
  subports : Array String
deriving Inhabited

abbrev PortId := Nat

/-- Concrete node that lives in some APEX graph. -/
structure Node where
  name : String
  type : NodeType
  globalId : Nat
  ports : Array PortId
  subPorts : Array AddSubPortSpec
deriving Inhabited

-- /-- Type of Lean function in terms of APEX types. -/
-- deriving Inhabited
