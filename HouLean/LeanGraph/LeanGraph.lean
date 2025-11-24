import Lean.Data.Json
import HouLean.Data
import HouLean.Init

open Lean

namespace HouLean.LeanGraph

initialize registerTraceClass `HouLean.lean_graph

/-- Trace class for LeanGraph elaboration -/
register_option trace.HouLean.LeanGraph.typecheck : Bool := {
  defValue := false
  descr := "trace elaboration of LeanGraph nodes to Lean expressions"
}


inductive PortType where
  | builtin (name type : String)
  | struct  (name type : String) (subports : Array PortType)
deriving ToJson, FromJson, Inhabited, Repr

def PortType.typeName (t : PortType) : String :=
  match t with
  | .builtin _ tn => tn
  | .struct  _ tn _ => tn

def PortType.name (t : PortType) : String :=
  match t with
  | .builtin n _ => n
  | .struct  n _ _ => n

partial def PortType.toString (t : PortType) (indent : String := "")  : String :=
  match t with
  | .builtin n tn => s!"{indent}{n} : {tn}"
  | .struct  n tn xs =>
    let xs := xs.map (fun x => x.toString (indent ++ "  ")) |>.joinl (map:=id) (·++"\n"++·)
    s!"{indent}{n} : \{\n{xs}\n{indent} : {tn}}"

instance : ToString PortType := ⟨PortType.toString⟩

structure NodeType where
  name : String
  leanConstant : Name
  inputs : Array PortType
  outputs : Array PortType
deriving ToJson, FromJson, Inhabited, Repr

def NodeType.toString (t : NodeType) : String :=
  let inputs := t.inputs.joinl (map:=PortType.toString) (·++"\n"++·)
  let outputs := t.outputs.joinl (map:=PortType.toString) (·++"\n"++·)
  s!"{t.name} {t.leanConstant}\ninputs:\n{inputs}\noutputs:\n{outputs}"

instance : ToString NodeType := ⟨NodeType.toString⟩

structure Node where
  name : String
  /-- Lean constant this node corresponds to. -/
  type : NodeType
  /-- Expression values for top level input ports -/
  portValues : Array (Option String)
  /-- Scope of this node. It is the name of the corresponding output node. -/
  scope : Option String
  /-- The number of attached visualizers to this node with the name of this node. -/
  visualizers : Option Nat
  x : Float
  y : Float
deriving ToJson, FromJson, Inhabited, Repr

structure Connection where
  /-- Source node of the connection -/
  outputNodeName : String
  outputIndex : List Nat
  /-- Target node of the connection -/
  inputNodeName : String
  inputIndex : List Nat
  outputRelPos : Float
  inputRelPos : Float
  /-- Implicit connections are not actual connections
  but indicate that the target/input port depends on the
  source/output port.

  The main difference from normal connactions is that
  there can be multiple implicit connections going into
  the same input port. -/
  isImplicit := false
deriving ToJson, FromJson, Inhabited, Repr

end LeanGraph

open LeanGraph in
structure LeanGraph where
  portTypes : Array PortType
  nodeTypes : Array NodeType
  nodes : Array Node
  connections : Array Connection
deriving ToJson, FromJson, Inhabited, Repr
