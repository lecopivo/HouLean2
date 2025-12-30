import HouLean.Meta.SpecializeAndSimp2.Main
import HouLean.Meta.SpecializeAndSimp2.Command
import HouLean.Meta.FloatPolynomial
import HouLean.Data.Vector
import HouLean.Meta.RewriteBy

namespace Test.Meta.Sas2

open Lean Elab Command Term

open HouLean.Meta.Sas


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

#sas fun x : Nat => x + 0

@[simp]
theorem add_self (x : Nat) : x + x = 2 * x := by grind

#sas fun x : Nat =>
  let a := x*x + 0
  a + x

def id' {α} (a : α) := a

#sas fun x : Nat =>
  let a := some x
  let a := a.map (·+1)
  a.getD 0 + x

def f1 {α} [Inhabited α] (x : Option α) := x.getD default

#sas fun x : Option Float => f1 x

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

def bar {α} (x : α) : Option α := some x

-- set_option pp.funBinderTypes true

#check Vector.zipWith
#check VectorFloat3.fromVector

set_option trace.HouLean.sas true

#sas fun (x y : VectorFloat3) => -x.toVector

#sas fun (x y : Vector Float 3) (a : Float) =>
  addVectors x y (some a)

#sas fun (x v f : Vector Float 3) (d : Float) =>
  integrateParticle x (addVectors v v (some 2)) 0.1 (foo f) (bar d)

#sas fun (x : Vector Float 3) =>
  if x[0] + x[1] > 0 then
    some (x[0] + x[1] + x[2])
  else
    none

#check @ite

set_option trace.HouLean.sas true in
set_option trace.HouLean.sas.simp true in
#sas fun (x : Vector Float 3) => x[0] + x[1]

#check foo.spec_Float_3
#check integrateParticle.spec_Float_3_0_1_default_false



structure Particle (X : Type) where
  position : X
  velocity : X

def system [Add X] [HMul R X X] [Zero X]
    (force? : Option (X → X)) (drag? : Option R) (x v : X) : X × X := Id.run do
  let mut force : X := 0
  if let some f := force? then
    force := force + f x
  if let some drag := drag? then
    force := force + drag * v
  return (v, force)



#print TypeEncoding


def bar' (x : Float) (a? b? : Option Float) := Id.run do
  let mut x := x
  if let some a := a? then
    x := x + a
  if let some b := b? then
    x := x + b
  return x


variable (x a : Float)

#sas (bar' x (some a) none)
  rewrite_by
    unfold bar'


#sas (fun x : Float => x + 0)
