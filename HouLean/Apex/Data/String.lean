import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Apex.Lean.Decidable

open HouLean.Apex Generated

@[apex_implements String.append]
def String.append.apex_impl (x y : String) : String := AddString x #v[y]

@[apex_implements String.decEq]
def String.decEq.apex_impl (x y : String) : Bool := EqualsString x y 




class ApexToString (α : Type u) where
  toString : α → String

instance : ApexToString Int where
  toString := string_FromInteger
    
def _root_.ToString.toString.apex_impl {α} [ApexToString α] (a : α) : String := 
  ApexToString.toString a

open Compiler in
run_meta compilerExt.add (.implementedByName ``toString ``ToString.toString.apex_impl
  #[some 0, none, some 2]) default


