import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Apex.Lean.ForInStep

namespace HouLean.Apex.Compiler

open HouLean 

namespace Generated

@[apex_node "ForBegin" has_rundata]
opaque ForBegin (iterations : Int) {ts} (spare : VariadicArg' ts) : 
  struct {scope : Int, index : Int,  spare : VariadicArg' ts} := cast sorry_proof ()

@[apex_node "ForEnd" has_rundata]
opaque ForEnd (scope : Int) {ts} (spare : VariadicArg' ts) : 
  VariadicArg' ts := cast sorry_proof ()

end Generated

--------------------------------------------------------------------------------------------------------

class ForLoop (m : Type → Type) (State : Type) (Range : Type) (Index : outParam (Type)) where
  forBegin : Range → State → m (Int × Index × State)
  forEnd : Int → State → m State

open ForLoop Generated

noncomputable
instance [ApexTypeFlatten α ts] : ForLoop Id α Std.Range Nat where 
  forBegin range x := 
    let n := range.size
    let ⟨scope, index, x⟩ := ForBegin (Int.ofNat n) (apexFlatten x)
    let index' := (range.start + index * range.step).toNat
    return ⟨scope, index', apexUnflatten x⟩
  forEnd scope state := 
    let state := ForEnd scope (apexFlatten state)
    (apexUnflatten state : α)

-- StateT monad instance
instance [Monad m] (State' : Type) 
    [ForLoop m (State×State') Range Index] : 
    ForLoop (StateT State' m) State Range Index where 
  forBegin range state state' := do
    let (scope, index, (state,state')) ← forBegin range (state,state')
    return ((scope, index, state), state')
  forEnd scope state state' := forEnd Range scope (state,state')


unsafe def ForIn.forIn.apex_impl {m : Type → Type} {Range : Type} {Index : Type} 
    [ForIn m Range Index] {State : Type} [Monad m] [ForLoop m State Range Index]
    (range : Range) (init : State) (f : Index → State → m (ForInStep State)) : m State := do
  let (scope, index, state) ← forBegin range init
  let state ← f index state
  forEnd Range scope state.value
  
run_meta compilerExt.add (.implementedByName ``ForIn.forIn ``ForIn.forIn.apex_impl 
  #[some 0, some 1, some 2, some 3, some 4, some 5, none, some 6, some 7, some 8])


  
-- todo: move this
instance [ApexType α A] : ApexType (Id α) A where
  toApex x := toApex (α:=α) x
  fromApex x := fromApex (α:=α) x
