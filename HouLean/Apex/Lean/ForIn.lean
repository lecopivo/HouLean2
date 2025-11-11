import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Apex.Lean.ForInStep

namespace HouLean.Apex.Compiler

open HouLean 

namespace Generated

@[apex_node "ForBegin" has_rundata]
opaque ForBegin (iterations : Int) {ts} (spare : VariadicArg' ts) : 
  Int × Int × VariadicArg' ts := cast sorry_proof ()

@[apex_node "ForEnd" has_rundata]
opaque ForEnd (scope : Int) {ts} (spare : VariadicArg' ts) : 
  VariadicArg' ts := cast sorry_proof ()

end Generated

--------------------------------------------------------------------------------------------------------

noncomputable
unsafe def _root_.HouLean.forLoop.apex_impl {State Context : Type} {ts} [ApexTypeFlatten (State×Context) ts]
    (iterations : Int) (init : State) (ctx : Context) (loop : Context → Int → State → State) : State := 
  let r := Generated.ForBegin iterations (apexFlatten (init, ctx))
  let scope := r.1
  let index := r.2.1
  let stateAndContext : State×Context := apexUnflatten r.2.2
  let state := stateAndContext.1
  let ctx := stateAndContext.2
  let state := loop ctx index state
  let stateAndContext := Generated.ForEnd scope (apexFlatten (state,ctx))
  let state := (apexUnflatten (α:=State×Context) stateAndContext).1
  state


/-- Basic for loop function that runs `loop` on `init` for given number of `iterations`. 

This function has internal support from the Lean -> APEX compiler and gets translated to `forLoopWithContext`. -/
opaque _root_.HouLean.forLoop {State : Type} (interations : Int) (init : State) (loop : Int → State → State) : State := init


/-- Class used to provide APEX implementation for for loops. 

This is mainly used to support monad transformers. -/
class ApexForIn (m : Type → Type) (State : Type) (Range : Type) (Index : outParam (Type)) where
  forIn (range : Range) (init : State) (loop : Index → State → m (ForInStep State)) : m State


open ApexForIn Generated

noncomputable
instance [ApexTypeFlatten α ts] : ApexForIn Id α Std.Range Nat where 
  forIn range init loop :=
    let iterations := range.size
    forLoop iterations init (fun index state =>
      let i := (Int.ofNat range.start + index * Int.ofNat range.step).toNat
      let result := (loop i state).value
      result)

-- StateT monad instance
instance [Monad m] (State' : Type)
    [ApexForIn m (State×State') Range Index] : 
    ApexForIn (StateT State' m) State Range Index where 
  forIn range init loop init' := do
    ApexForIn.forIn (State:=State×State') range (init, init')
      fun index s => do
        let state  := s.1
        let state' := s.2
        let r ← loop index state state'
        let state := r.1.value -- todo: this needs to change once we support breaking out of loops
        let state' := r.2
        return .yield (state, state')

-- ReaderT monad instance
instance [Monad m] (State' : Type)
    [ApexForIn m (State×State') Range Index] :
    ApexForIn (ReaderT State' m) State Range Index where
  forIn range init loop init' := do
    let r ← ApexForIn.forIn (State:=State×State') range (init, init')
      fun index s => do
        let state  := s.1
        let state' := s.2
        let r ← loop index state state'
        let state := r.value -- todo: this needs to change once we support breaking out of loops
        return .yield (state, state')
    return r.1


-- This version of for loop is nice to use in Lean Graph
/-- Basic for loop function that executes code in some context, like `VisualizeM` context. -/
def _root_.HouLean.forLoopM {State : Type} {m : Type → Type} 
    [ApexTypeFlatten State ts] [Monad m] [ApexForIn m State Std.Range Nat] 
    (iterations : Int) (init : State) (loop : Int → State → m State) : m State := 
  ApexForIn.forIn [0:iterations.toNat] init (fun index state => do
    let r ← loop (Int.ofNat index) state
    return .yield r)

unsafe def ForIn.forIn.apex_impl {m : Type → Type} {Range : Type} {Index : Type}
    [ForIn m Range Index] {State : Type} [Monad m] [ApexForIn m State Range Index]
    (range : Range) (init : State) (f : Index → State → m (ForInStep State)) : m State := do
  ApexForIn.forIn range init f
 
run_meta compilerExt.add (.implementedByName ``ForIn.forIn ``ForIn.forIn.apex_impl 
  #[some 0, some 1, some 2, some 3, some 4, some 5, none, some 6, some 7, some 8])

-- todo: move this
instance [ApexType α A] : ApexType (Id α) A where
  toApex x := toApex (α:=α) x
  fromApex x := fromApex (α:=α) x
