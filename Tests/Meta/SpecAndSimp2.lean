import HouLean.Meta.SpecializeAndSimp2.Main

namespace Test.Meta.Sas2

open Lean Elab Command Term

open HouLean.Meta.Sas


elab "#sas " e:term : command =>
  runTermElabM fun _ctx => do
    let e ← elabTermAndSynthesize e none
    let e ← sas e #[`simp]
    logInfo e
    pure ()

set_option trace.HouLean.sas true
set_option trace.HouLean.sas.simp true

simproc ↓ binder_backstop (_) := fun e => do
  if e.isLambda ||
     e.isLet ||
     (← Meta.isMatcherApp e) then
    return .done { expr := e }
  return .continue

#sas fun x y : Nat =>
  let a :=
    let a1 := x + 0 + y
    let a2 := 1 * a1 * 1 * x * 1
    a1 + a2 + x
  let b := x * y
  a + b + y


#sas fun x : Nat =>
  let a := x*x + 0
  a + x

#sas fun x : Nat =>
  let a := some x
  let a := a.map (·+1)
  a.getD 0 + x



def addVectors {α n} [Add α] (x y : Vector α n) (bias? : Option α) :=
  if let some bias := bias? then
    (x + y).map (·+bias)
  else
    (x + y)


def integrateParticle {α n} [One α] [Sub α] [Add α] [Mul α]
    (x v : Vector α n) (dt : α)
    (force? : Option (Vector α n)) (drag? : Option α) :
    Vector α n × Vector α n := Id.run do
  let mut x := x
  let mut v := v
  if let some f := force? then
    v := v + dt * f
  if let some drag := drag? then
    v := (1 - dt * drag) * v
  x := x + dt * v
  return (x,v)


def foo {α n} (x : Vector α n) : Option (Vector α n) := some x

#sas fun (x y : Vector Float 3) (a : Float) =>
  addVectors x y (some a)


#sas fun (x v f : Vector Float 3) =>
  integrateParticle x (addVectors v v (some 2)) 0.1 (foo f) none

#check foo.spec_Float_3

#check integrateParticle.spec_Float_3_0_1_default_false
