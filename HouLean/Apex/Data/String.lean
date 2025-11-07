import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Apex.Lean.Decidable

open HouLean.Apex Generated

@[apex_implements String.append]
def String.append.apex_impl (x y : String) : String := AddString x #a[y]

@[apex_implements String.decEq]
def String.decEq.apex_impl (x y : String) : Bool := EqualsString x y 

