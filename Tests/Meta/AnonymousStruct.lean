import HouLean.Meta.AnonymouStruct

import Qq 
-- Tests
section Tests

open AnonymousStruct

/-- info: struct {x✝:Float✝, y✝:Float✝} : Type -/
#guard_msgs in
#check struct { x : Float, y : Float }

/-- info: struct {x✝:Float✝, y✝:Float✝, z✝:Float✝} : Type -/
#guard_msgs in
#check struct { x : Float, y : Float, z : Float }

variable (v : struct { x : Float, y : Float })
variable (u : struct { x : Float, y : Float, z : Float })


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

/-- info: u : struct {x✝:Float✝, y✝:Float✝, z✝:Float✝} -/
#guard_msgs in
#check u

-- set_option pp.notation false
-- Nested structs
abbrev Particle := struct { pos : struct { x : Float, y : Float }, vel : struct { x : Float, y : Float } }

variable (s : Particle)

/-- info: s.vel : struct {x✝:Float✝, y✝:Float✝} -/
#guard_msgs in
#check s.vel

/-- info: s.pos.x : Float -/
#guard_msgs in
#check s.pos.x

-- Constructor syntax should work
def myVec : struct { x : Float, y : Float } := { x := 1.0, y := 2.0 }

/-- info: myVec : struct {x✝:Float✝, y✝:Float✝} -/
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

end Tests
