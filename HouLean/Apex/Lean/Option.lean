import HouLean.Init
import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy

open HouLean Apex Compiler

namespace HouLean

-- @[apex_implements Option] -- this should work too as `argMap` is trivial here
structure Maybe (α : Type u) where
  val : α
  valid : Bool

-- @[apex_implements Option.some] -- this should work as `argMap` is trivial here
def Maybe.some {α} (val : α) : Maybe α := ⟨val, true⟩

-- @[apex_implements Option.none] -- what syntax should be here/should I figure out `argMap` automatically?
def Maybe.none {α} [Inhabited α] : Maybe α := ⟨default, false⟩

-- ideally this should just be
-- attribute [apex] Option -- and it will generate all this stuff automatically 

def _root_.Option.toMaybe {α} [Inhabited α] (val? : Option α) : Maybe α := 
  match val? with
  | .some val => ⟨val, true⟩
  | .none => ⟨default, false⟩

unsafe def Maybe.optionRec {α : Type u} [Inhabited α] 
  {motive : Option α → Sort v} (none : motive .none)
  (some : (val : α) → motive (.some val)) (t : Option α) : motive t :=
  let t' := t.toMaybe
  if t'.2 then
    unsafeCast (some t'.1)
  else
    unsafeCast none

end HouLean

run_meta compilerExt.add (.implementedByName ``Option ``Maybe #[some 0]) default
run_meta compilerExt.add (.implementedByName ``Option.some ``Maybe.some #[some 0, some 1]) default
run_meta compilerExt.add (.implementedByName ``Option.none ``Maybe.none #[some 0, none]) default
run_meta compilerExt.add (.implementedByName ``Option.toMaybe ``id' #[none, some 2]) default
run_meta compilerExt.add (.implementedByName ``Option.rec ``Maybe.optionRec #[some 0, none, some 1, some 2, some 3, some 4]) default

