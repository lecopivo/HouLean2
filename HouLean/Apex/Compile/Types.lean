import Lean
import HouLean.ArrayTree
import HouLean.Data
import HouLean.Apex.Generated.Defs

open Lean

namespace HouLean

/-- This class maps Lean type `α` to Apex compatible type `β`. 

This extensible type level function mapping α to β is used to 
transform Lean code to a smaller subset of Lean which is supported
by Apex compiler. Therefore many APEX compiler extensions can be done
through providing instances like `ApexType` and one does not have to
touch the compiler!.
-/
class ApexType (α : Type u) (β : outParam (Type v)) where
  toApex : α → β
  fromApex : β → α

export ApexType (toApex fromApex)


namespace Apex

/-- List of element of type `α` whose length `n` has to be known at the APEX compile time. -/
inductive VariadicArg (α : Type u) : Nat → Type u where
  | nil : VariadicArg α 0
  | cons {n} (head : α) (tail : VariadicArg α n) : VariadicArg α (n+1)
-- def VariadicArg (α : Type u) (n : Nat) := Vector α n

protected def VariadicArg.default [Inhabited α] : (n : Nat) → VariadicArg α n
  | 0 => .nil
  | n+1 => .cons default (.default n)

instance {α} [Inhabited α] : Inhabited (VariadicArg α n) := ⟨VariadicArg.default n⟩

syntax "#a[" term,* "]" : term
macro_rules 
| `(#a[]) => `(VariadicArg.nil)
| `(#a[$x:term]) => `(VariadicArg.cons $x .nil)
| `(#a[$x:term,$xs:term,*]) => `(VariadicArg.cons $x #a[$xs:term,*])

inductive VariadicArg' : List ApexTypeTag  → Type u where
  | nil : VariadicArg' []
  | cons {t : ApexTypeTag} {ts : List ApexTypeTag} (head : t.toType) (tail : VariadicArg' ts) : VariadicArg' (t::ts)

class ApexTypeFlatten (α : Type u) (ts : outParam (List ApexTypeTag)) : Type u where
  apexFlatten : α → VariadicArg' ts
  apexUnflatten : VariadicArg' ts → α

export ApexTypeFlatten (apexFlatten apexUnflatten)

def _root_.HouLean.Apex.Untyped.getFloat! (x : Untyped) : Float := 
  match x with
  | .float x => x
  | _ => panic! s!"Getting float from untyped which has different type!"

def _root_.HouLean.Apex.Untyped.getInt! (x : Untyped) : Int := 
  match x with
  | .int x => x
  | _ => panic! s!"Getting int from untyped which has different type!"

instance : ApexTypeFlatten Float [.float] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten Int [.int] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten Geometry [.geometry] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten String [.string] where
  apexFlatten x := .cons x .nil
  apexUnflatten := fun (.cons x .nil) => x

instance : ApexTypeFlatten Unit [.int] where
  apexFlatten _ := .cons (0:Int) .nil
  apexUnflatten := fun (.cons _ .nil) => ()


set_option warn.sorry false in
unsafe instance [ApexTypeFlatten α ts] [ApexTypeFlatten β ss] : ApexTypeFlatten (α×β) (ts ++ ss) where
  apexFlatten x := sorry
  apexUnflatten x := sorry

set_option warn.sorry false in
unsafe instance [ApexTypeFlatten α ts] [ApexTypeFlatten β ss] : ApexTypeFlatten (MProd α β) (ts ++ ss) where
  apexFlatten x := sorry
  apexUnflatten x := sorry


-- ...

namespace Compiler

initialize registerTraceClass `HouLean.Apex.compiler

/-- Name of builtin APEX type. -/
abbrev TypeName := String

deriving instance Repr for ApexTypeTag
instance : ToString ApexTypeTag := ⟨fun t => t.toString⟩


inductive PortType where
  | builtin  (typeTag : ApexTypeTag)
  /-- Option beacause we can have `VariadicArg<void>`
  `size` is currently known number of subports -/
  | variadic (elemTypeTag : Option ApexTypeTag)
  | rundata
  | undefined
deriving Inhabited, BEq, Repr

def PortType.builtin! (t : PortType) : ApexTypeTag :=
  match t with
  | .builtin n => n
  | _ => panic! "invalid use of PortType.buildin!"

def PortType.isVariadic (t : PortType) : Bool :=
  match t with
  | .variadic .. => true
  | _ => false

inductive PortDir where
  | input | output
deriving Inhabited, BEq, Repr

structure LocalPort where
  /-- local id of a port within node -/
  localId : Nat
  name : Name
  type : PortType
  dir : PortDir
deriving Inhabited, Repr

structure Port extends LocalPort where
  /-- Global id of a port, can be invalid if port is only part of a node specification. -/
  globalId : Nat
  /-- Global node id of a port, can be invalid if port is only part of a node specification. -/
  nodeId : Nat
deriving Inhabited

def Port.isVariadic (p : Port) : Bool := 
  match p.type with
  | .variadic .. => true
  | _ => false

/-- APEX type that corresponds to a Lean type. Exact size of variadic types might not be known. -/
abbrev ApexStruct := ArrayTree (Name × ApexTypeTag)
instance : ToString ApexStruct := ⟨fun t => t.toString⟩

-- inductive ApexType where
--   | builtin (typeTag : ApexTypeTag)
--   /-- Variadic type with size expressed as `Expr`. 

--   During compilation to APEX graph this `n` has to be a literal value. -/
--   | variadic (elemTypeName : ApexTypeTag) (n : Expr)
--   -- stores field name and name of the builtin type
--   | struct (bundle : ArrayTree (String×ApexTypeTag))
-- deriving Inhabited, BEq

-- def ApexType.toString (t : ApexType) : String :=
--   match t with
--   | .builtin t => t
--   | .variadic t _ => s!"({t},...)"
--   | .struct t => (t.mapIdx (fun _ n => n.2)).toString

-- instance : ToString ApexType := ⟨fun t => t.toString⟩

/-- APEX type that corresponds to a Lean type. Size of variadic types is known.

default name of variable of this type and its builtin type name
 -/
abbrev ApexStaticType := ArrayTree (String × TypeName)

structure ApexFunType where
  inputs : Array ApexStruct
  output : ApexStruct
deriving Inhabited, BEq

structure NodeType where
  name : String
  leanDecl : Name
  /-- This indicates if a node has additional output port with run data -/
  hasRunData : Bool
  /-- Shapes of the inputs, numbers are local indices of the ports -/
  inputs : Array (ArrayTree LocalPort)
  /-- Shapes of the output, numbers are local indices of the ports -/
  output : ArrayTree LocalPort
  variadicPortGroups : Array (Array Nat)
deriving Inhabited, Repr

/-- Specify for which port to create subports given their names and types in `subports`

Variadic ports usually come in pairs as input and output, or 
 -/
structure AddSubPortSpec where
  inputPortId : Option Nat
  /-- Some variadic ports are inputs and create the same amount of output subports 
  For example `ForBagin.__spare__` port 2 is input and port 5 is corresponding output -/ 
  outputPortId : Option Nat 
  /-- Names of all subports to be creates. -/
  subports : Array (String×TypeName)
deriving Inhabited

inductive PortId where
  | port (id : Nat)
  | subport (id : Nat) (localId : Nat)
deriving Inhabited, BEq, Repr

/-- Concrete node that lives in some APEX graph. -/
structure Node where
  name : String
  type : NodeType
  globalId : Nat
  /-- Shapes of the inputs, numbers are local indices of the ports -/
  inputs : Array (ArrayTree Nat)
  /-- Shapes of the output, numbers are local indices of the ports -/
  output : ArrayTree Nat
  /-- Number of subports for of nodes ports. Ports are grouped into groups are they might share the same size
  and `apex.Graph.addSubPort` should be called only on one of them. -/
  subportSizes : Array (Nat × Array Nat)
deriving Inhabited


def Node.ensureSubportSize (node : Node) (globalId : Nat) (newSize : Nat) : Node := 
  {node with
    subportSizes := node.subportSizes.map (fun (size,ports) => 
      if ports.contains globalId then
        (max size newSize, ports)
      else
        (size, ports))}
      


-- /-- Type of Lean function in terms of APEX types. -/
-- deriving Inhabited
