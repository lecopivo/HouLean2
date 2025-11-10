import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Apex.Lean.Decidable
import HouLean.Apex.Lean.TwoWaySwitch


namespace HouLean.Apex.Compiler

open HouLean 

namespace Generated

@[apex_node "IfBegin" has_rundata]
opaque IfBegin (condition : Bool) {ts} (spare : VariadicArg' ts) : 
  struct {scope : Int, spare : VariadicArg' ts} := cast sorry_proof ()

@[apex_node "IfEnd" has_rundata]
opaque IfEnd (scope : Int) {ts} (spare : VariadicArg' ts) : 
  VariadicArg' ts := cast sorry_proof ()

end Generated


open Generated TwoWaySwitch

noncomputable -- this is confusing the compiler :) so we have to mark it with concomputable
def ite.apex_impl_with_pass_through {α : Type} (condition : Bool)
  {TContext : Type} {TCtx} [ApexTypeFlatten TContext TCtx] 
  {EContext : Type} {ECtx} [ApexTypeFlatten EContext ECtx] 
  {A} [ApexTypeFlatten α A] [TwoWaySwitch α]
  (t : TContext → α) (e : EContext → α) (tctx : TContext) (ectx : EContext) := 

  let condition := condition
  
  -- true branch
  let tctx := apexFlatten tctx
  let t1 := IfBegin condition tctx
  let t2 := t (apexUnflatten t1.spare)
  let t3 := IfEnd t1.scope (apexFlatten t2)
  let t4 : α := (apexUnflatten t3)

  -- false branch
  let ectx := apexFlatten ectx
  let e1 := IfBegin (!condition) ectx
  let e2 := e (apexUnflatten e1.spare)
  let e3 := IfEnd e1.scope (apexFlatten e2)
  let e4 : α := (apexUnflatten e3)

  twoWaySwitch e4 t4 condition

