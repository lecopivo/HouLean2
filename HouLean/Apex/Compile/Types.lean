import Lean
import HouLean.ArrayTree
import HouLean.Data
import HouLean.Apex.Generated.Defs
import HouLean.Apex.Compile.Init


open Lean

namespace HouLean.Apex

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

def PortType.isVariadic' (t : PortType) : Bool :=
  match t with
  | .variadic none => true
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

def Port.isVariadic (p : Port) : Bool := p.type.isVariadic
def Port.isVariadic' (p : Port) : Bool := p.type.isVariadic'

/-- APEX type that corresponds to a Lean type. Exact size of variadic types might not be known. -/
abbrev ApexStruct := ArrayTree (Name × ApexTypeTag)
instance : ToString ApexStruct := ⟨fun t => t.toString⟩

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
      
