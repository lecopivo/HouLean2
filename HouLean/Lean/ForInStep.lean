import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy

open HouLean Apex

#check ForInStep
#check Id

open HouLean.Apex.Compiler

abbrev Id' (α : Sort u) := α


/-- Pair type with `Bool` -/
abbrev WithBool (α : Type u) := α × Bool

set_option pp.all true 
run_meta addImplementedByName ``ForInStep ``Id' default
run_meta addImplementedByName ``Option ``WithBool default
run_meta addImplementedByName ``ForInStep.yield ``id default

run_meta addImplementedByName ``Nat ``Int default

open Lean Meta Qq
run_meta
  let some t ← getApexType? q(ForInStep Float) | throwError "buuuu"
  logInfo t.toString

  let some t ← getApexType? q(Option Float) | throwError "buuuu"
  logInfo t.toString

  let some t ← getApexType? q(Option Nat × Option (Option Float)) | throwError "buuuu"
  logInfo t.toString

#exit

-- Id type constructor implements ForInStep type constructor
attribute [apex_implements ForInStep] Id

@[apex_implements ForInStep.yield]
def ForInStep.yield.apex_impl {α} (x : α) := x



inductive Foo
  | foo | bar | foobar

-- todo: have an attribute/deriving that generates these automatically

-- Foo is implemented by Int
attribute [apex_implements Foo] Int

@[apex_implements Foo.foo]
def Foo.foo.apex_impl : Int := 0

@[apex_implements Foo.bar]
def Foo.bar.apex_impl : Int := 1

@[apex_implements Foo.foobar]
def Foo.foobar.apex_impl : Int := 2
