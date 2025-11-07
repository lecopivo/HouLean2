import HouLean.Init
import HouLean.Apex.ApexType
import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy

open HouLean Apex Compiler

namespace HouLean.Apex.Compiler

-- @[apex_implements Option] -- this should work too as `argMap` is trivial here
abbrev Maybe (α : Type u) := α × Bool

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

def Maybe.toOption {α} (val? : Maybe α) : Option α := 
  match val?.2 with
  | true => .some val?.1
  | false => .none

def Maybe.map {α β} (f : α → β) (x : Maybe α) : Maybe β :=
  ⟨f x.1, x.2⟩

unsafe def Maybe.optionRec {α : Type u} [Inhabited α] 
  {motive : Option α → Sort v} (n : motive .none)
  (s : (val : α) → motive (.some val)) (t : Option α) : motive t :=
  let t' := t.toMaybe
  if t'.2 then
    unsafeCast (s t'.1)
  else
    unsafeCast n

end HouLean.Apex.Compiler

instance {α A} [Inhabited α] [ApexType α A] : ApexType (Option α) (Maybe A) where
  toApex x := x.toMaybe.map toApex
  fromApex x := x.toOption.map fromApex

-- constructors and recursor
run_meta compilerExt.add (.implementedByName ``Option.some ``Maybe.some #[some 0, some 1]) default
run_meta compilerExt.add (.implementedByName ``Option.none ``Maybe.none #[some 0, none]) default
run_meta compilerExt.add (.implementedByName ``Option.rec ``Maybe.optionRec #[some 0, none, some 1, some 2, some 3, some 4]) default

-- compilation morphism
run_meta compilerExt.add (.implementedByName ``Option.toMaybe ``id' #[none, some 2]) default
run_meta compilerExt.add (.implementedByName ``Maybe.toOption ``id' #[none, some 1]) default

