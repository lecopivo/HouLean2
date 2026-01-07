import HouLean.Meta.SpecializeAndSimp2.Main
import HouLean.Meta.SpecializeAndSimp2.Command
import HouLean.Meta.FloatPolynomial
import HouLean.Data.Vector
import HouLean.Meta.RewriteBy

namespace Test.Meta.Sas2

open Lean Elab Command Term

open HouLean.Meta.Sas


-- set_option trace.HouLean.sas true
-- set_option trace.HouLean.sas.simp true

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

#sas fun x : Float => x + 0

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

set_option pp.funBinderTypes true

/-- info: fun (x : VectorFloat3) => x -/
#guard_msgs in
#sas fun x : Vector Float 3 => x

/-- info: fun (x : VectorFloat3) => x.x0 -/
#guard_msgs in
#sas fun x : Vector Float 3 => x[0]

/-- info: fun (x : VectorFloat3) => x.x1 -/
#guard_msgs in
#sas fun x : Vector Float 3 => x[1]

/-- info: fun (x : VectorFloat3) => x.x2 -/
#guard_msgs in
#sas fun x : Vector Float 3 => x[2]

def f2 {α : Type} (x : Vector α 3) := x

#sas fun x y z : Float => #v[x,y,z]

/--
info: fun (x : VectorFloat3) =>
  let tmp := Neg.neg.Vector_Float_3 x;
  tmp
-/
#guard_msgs in
#sas fun x : Vector Float 3 => - x

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

#print integrateParticle

def foo {α n} (x : Vector α n) : Option (Vector α n) := some x

def bar {α} (x : α) : Option α := some x

-- set_option pp.funBinderTypes true

#check Vector.zipWith
#check VectorFloat3.fromVector

set_option trace.HouLean.sas true

#sas fun (x y : VectorFloat3) => -x.toVector

-- #sas fun (x y : Vector Float 3) (a : Float) =>
--   addVectors x y (some a)

-- #sas fun (x v f : Vector Float 3) (d : Float) =>
--   integrateParticle x (addVectors v v (some 2)) 0.1 (foo f) (bar d)

#sas fun (x : Vector Float 3) =>
  if x[0] + x[1] > 0 then
    some (x[0] + x[1] + x[2])
  else
    none

#check @ite

set_option trace.HouLean.sas true in
set_option trace.HouLean.sas.simp true in
#sas fun (x : Vector Float 3) => x[0] + x[1]


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

def bar' (x : Float) (a? b? : Option Float) := Id.run do
  let mut x := x
  if let some a := a? then
    x := x + a
  if let some b := b? then
    x := x + b
  return x

#print bar'

variable (x a : Float) (b? : Option Float)

@[simp]
theorem bind_id (x : Id α) (f : α → Id β) : bind x f = let y := x.run; f x := by rfl

@[simp]
theorem pure_id (x : α) : pure (f:=Id) x = x := by rfl

attribute [simp] Id.run
set_option trace.HouLean.sas.simp false
#sas (fun (x : Float) (b? : Option Float) => (bar' x (some a) b?))

#sas (fun x : Float => x + 0)

#sas (fun x y : Nat => x + y)

#check (fun x y : Nat => Add.add x y) rewrite_by simp


def f2 (x y : Vector Float 3) := x[0] + y[0]

set_option pp.funBinderTypes true in
set_option trace.HouLean.sas true in
#sas fun (x y : Vector Float 3) => f2 x y


#sas fun (x : Option Nat) => x.getD 0
