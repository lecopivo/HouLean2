import HouLean.Meta.AnonymousStruct

import Qq 
-- Tests
section Tests


/-- info: struct {x : Float, y : Float} : Type -/
#guard_msgs in
#check struct { x : Float, y : Float }

/-- info: struct {x : Float, y : Float, z : Float} : Type -/
#guard_msgs in
#check struct { x : Float, y : Float, z : Float }

variable (v : struct { x : Float, y : Float })
variable (u : struct { x : Float, y : Float, z : Float })

/-- info: { u := 1, v := 2 } : struct {u : Float, v : Float} -/
#guard_msgs in
#check {u := 1, v := 2 : struct { u : Float, v : Float}}

/-- info: v.x : Float -/
#guard_msgs in
#check v.x  -- Dot notation works!

/-- info: v.y : Float -/
#guard_msgs in
#check v.y

/-- info: u.z : Float -/
#guard_msgs in
#check u.z

/-- info: u : struct {x : Float, y : Float, z : Float} -/
#guard_msgs in
#check u

-- set_option pp.notation false
-- Nested structs
abbrev Particle := struct { pos : struct { x : Float, y : Float }, vel : struct { x : Float, y : Float } }

variable (s : Particle)

/-- info: s.vel : struct {x : Float, y : Float} -/
#guard_msgs in
#check s.vel

/-- info: s.pos.x : Float -/
#guard_msgs in
#check s.pos.x

-- Constructor syntax should work
def myVec : struct { x : Float, y : Float } := { x := 1.0, y := 2.0 }

-- deriving instance Inhabited for AnonStruct

/-- info: instInhabitedAnonStruct -/
#guard_msgs in
#synth Inhabited struct {x : Float, y : Float}


/-- info: myVec : struct {x : Float, y : Float} -/
#guard_msgs in
#check myVec

/-- info: myVec.x : Float -/
#guard_msgs in
#check myVec.x

def foo (a : Float) : struct {x : Float, y : Float} := 
  { x := a.cos, y := a.sin }

instance : Add (struct { x : Float, y : Float }) := ⟨fun u v => ⟨u.x+v.x, u.y+v.y⟩⟩

/-- info: { x := 1.916065, y := 0.489252 } -/
#guard_msgs in
#eval 
  let a := foo 0.1
  let b := foo 0.4
  a + b


variable (A B : Type) (n m : Nat)

variable (s : struct {x : Float, a : A, b : B, arr : Array (A×B)})

/-- info: s.a : A -/
#guard_msgs in
#check s.a

/-- info: s.b : B -/
#guard_msgs in
#check s.b

/-- info: s.x : Float -/
#guard_msgs in
#check s.x

/-- info: s.arr : Array (A × B) -/
#guard_msgs in
#check s.arr

/-- error: Pushing new elements to parametrized structures is not yet supported! -/
#guard_msgs in
#check struct s push% val := 0.1

/-- error: Pop elements from parametrized structures is not yet supported! -/
#guard_msgs in
#check struct s pop% val

/-- info: { x := 0.995004, y := 0.099833, z := 0.300000 } -/
#guard_msgs in
#eval struct (foo 0.1) push% z := 0.3 

/-- info: { y := 0.099833 } -/
#guard_msgs in
#eval struct (foo 0.1) pop% x

variable (a : A) (val : Float)


/--
info: let __src := foo 0.1;
{ x := __src.x, y := __src.y, val := val } : AnonStruct_6
-/
#guard_msgs in
#check struct (foo 0.1) push% val := val

/-- error: New field's type can't depend on a parameter yet! -/
#guard_msgs in
#check struct (foo 0.1) push% a := a

/-- info: { x := 0.100000 } -/
#guard_msgs in
#eval struct (default : struct {}) push% x := 0.1


-- buildStruct% x 0.1 y 0.2 z 0.3

end Tests
