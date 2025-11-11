import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Apex.Lean.Decidable
import HouLean.Apex.Lean.TwoWaySwitch


namespace HouLean.Apex.Compiler

open HouLean 

namespace Generated

@[apex_node "IfBegin" has_rundata]
opaque IfBegin (condition : Bool) {ts} (spare : VariadicArg' ts) : 
  Int × VariadicArg' ts := cast sorry_proof ()

@[apex_node "IfEnd" has_rundata]
opaque IfEnd (scope : Int) {ts} (spare : VariadicArg' ts) : 
  VariadicArg' ts := cast sorry_proof ()

end Generated


open Generated TwoWaySwitch

noncomputable -- this is confusing the compiler :) so we have to mark it with concomputable
def ite.apex_impl_with_pass_through {α : Type} (condition : Bool)
  {Context : Type} {Ctx} [ApexTypeFlatten Context Ctx]
  {A} [ApexTypeFlatten α A] [TwoWaySwitch α]
  (t e : Context → α) (ctx : Context)  :=

  let condition := condition
  
  -- true branch
  let ctx := apexFlatten ctx
  let t1 := IfBegin condition ctx
  let t2 := t (apexUnflatten t1.2)
  let t3 := IfEnd t1.1 (apexFlatten t2)
  let t4 : α := (apexUnflatten t3)

  -- false branch
  let ectx := apexFlatten ectx
  let e1 := IfBegin (!condition) ectx
  let e2 := e (apexUnflatten e1.2)
  let e3 := IfEnd e1.1 (apexFlatten e2)
  let e4 : α := (apexUnflatten e3)

  twoWaySwitch e4 t4 condition

