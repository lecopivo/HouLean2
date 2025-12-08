import HouLean
import HouLean.LeanGraph.Extension
import HouLean.LeanGraph.Init

open HouLean

set_option trace.HouLean.lean_graph true

namespace Tests.PortType
/--
trace: [HouLean.lean_graph] New port type registered
    vec3 : {
      x : ?_
      y : ?_
      z : ?_
     : Tests.PortType.Vec3 ?_}
-/
#guard_msgs in
@[lean_graph_type]
structure Vec3 (α : Type) where
  x : α
  y : α
  z : α

structure Par3 (α : Type) where
  pos : Vec3 α
  vel : Vec3 α


/--
trace: [HouLean.lean_graph] New node type registered
    Tests_PortType_Vec3_add Tests.PortType.Vec3.add
    inputs:
    u : Tests.PortType.Vec3 ?_
    v : Tests.PortType.Vec3 ?_
    outputs:
    vec3 : Tests.PortType.Vec3 ?_
-/
#guard_msgs in
@[lean_graph_node]
def Vec3.add {α} [Add α] (u v : Vec3 α) : Vec3 α :=
  ⟨u.x + v.x, u.y + v.y, u.z + v.z⟩


/--
trace: [HouLean.lean_graph] New node type registered
    Tests_PortType_Par3_add Tests.PortType.Par3.add
    inputs:
    p : {
      pos : Tests.PortType.Vec3 ?_
      vel : Tests.PortType.Vec3 ?_
     : Tests.PortType.Par3 ?_}
    q : {
      pos : Tests.PortType.Vec3 ?_
      vel : Tests.PortType.Vec3 ?_
     : Tests.PortType.Par3 ?_}
    outputs:
    par3 : {
      pos : Tests.PortType.Vec3 ?_
      vel : Tests.PortType.Vec3 ?_
     : Tests.PortType.Par3 ?_}
-/
#guard_msgs in
@[lean_graph_node]
def Par3.add {α} [Add α] (p q : Par3 α) : Par3 α :=
  ⟨p.pos.add q.pos, p.vel.add q.vel⟩



/--
info: prod : {
  fst : ?_
  snd : ?_
 : Prod ?_ ?_}
-/
#guard_msgs in
#port_type ?_ × ?_

/--
info: prod : {
  fst : Float
  snd : {
    fst : Float
    snd : Float
   : Prod Float Float}
 : Prod Float (Prod Float Float)}
-/
#guard_msgs in
#port_type Float × Float × Float

/-- info: array : Array ?_ -/
#guard_msgs in
#port_type Array ?_

/-- info: array : Array Float -/
#guard_msgs in
#port_type Array Float


/--
info: prod : {
  fst : Array ?_
  snd : {
    fst : ?_
    snd : {
      fst : Int
      snd : ?_
     : Prod Int ?_}
   : Prod ?_ (Prod Int ?_)}
 : Prod (Array ?_) (Prod ?_ (Prod Int ?_))}
-/
#guard_msgs in
#port_type Array ?_ × (?_ × (Int × ?_))

/-- info: vector3 : HouLean.Vector3 -/
#guard_msgs in
#port_type Vector3

/-- info: float : Float -/
#guard_msgs in
#port_type Float

/-- info: vec3 : Tests.PortType.Vec3 (Array ?_) -/
#guard_msgs in
#port_type Vec3 (Array ?_)

/--
info: par3 : {
  pos : Tests.PortType.Vec3 Float
  vel : Tests.PortType.Vec3 Float
 : Tests.PortType.Par3 Float}
-/
#guard_msgs in
#port_type Par3 Float

/--
info: par3 : {
  pos : Tests.PortType.Vec3 (Tests.PortType.Vec3 ?_)
  vel : Tests.PortType.Vec3 (Tests.PortType.Vec3 ?_)
 : Tests.PortType.Par3 (Tests.PortType.Vec3 ?_)}
-/
#guard_msgs in
#port_type Par3 (Vec3 ?_)
