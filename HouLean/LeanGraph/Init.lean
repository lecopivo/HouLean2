import HouLean.LeanGraph.Extension
import HouLean.Math
import HouLean.Meta.AnonymousStruct

-- todo: once lean graph is more mature this should be redistributed to other files

namespace HouLean

attribute [lean_graph_type_builtin] 
  Float Int Nat String Array

attribute [lean_graph_type] 
  Vector3 Matrix3

attribute [lean_graph_node] id
attribute [lean_graph_node] Prod.mk

attribute [lean_graph_node] Float.add
attribute [lean_graph_node] Float.sub
attribute [lean_graph_node] Float.mul
attribute [lean_graph_node] Float.div
attribute [lean_graph_node] Float.neg
attribute [lean_graph_node] Float.sin
attribute [lean_graph_node] Float.cos
attribute [lean_graph_node] Float.exp
attribute [lean_graph_node] Float.tan
attribute [lean_graph_node] Float.atan2

attribute [lean_graph_node] Vector3.add
attribute [lean_graph_node] Vector3.sub
attribute [lean_graph_node] Vector3.smul
attribute [lean_graph_node] Vector3.neg
attribute [lean_graph_node] Vector3.toGeodetic
attribute [lean_graph_node] Vector3.fromGeodetic
attribute [lean_graph_node] Vector3.toSpherical
attribute [lean_graph_node] Vector3.fromSpherical


-- attribute [lean_graph_node] Prod.map

attribute [lean_graph_node "Arith_add"] HAdd.hAdd
attribute [lean_graph_node "Arith_sub"] HSub.hSub
attribute [lean_graph_node "Arith_mul"] HMul.hMul
attribute [lean_graph_node "Arith_div"] HDiv.hDiv
attribute [lean_graph_node "Arith_neg"] Neg.neg
attribute [lean_graph_node "Arith_inv"] Inv.inv
attribute [lean_graph_node "Arith_one"] One.one
attribute [lean_graph_node "Arith_zero"] Zero.zero

open Math
attribute [lean_graph_node "Math_sin"] sin
attribute [lean_graph_node "Math_cos"] cos
attribute [lean_graph_node "Math_tan"] tan
attribute [lean_graph_node "Math_sqrt"] sqrt
attribute [lean_graph_node "Math_exp"] exp
attribute [lean_graph_node "Math_log"] log
attribute [lean_graph_node "Math_length"] length
attribute [lean_graph_node "Math_dot"] dot
attribute [lean_graph_node "Math_cross"] cross
attribute [lean_graph_node "Math_normalize"] normalize
attribute [lean_graph_node "Math_normalized"] normalized
attribute [lean_graph_node "Math_lerp"] lerp
attribute [lean_graph_node "Math_smoothstep"] smoothstep

-- attribute [lean_graph_node] GetElem.getElem
attribute [lean_graph_node] GetElem?.getElem?
attribute [lean_graph_node] GetElem?.getElem!

attribute [lean_graph_node] Array.get
attribute [lean_graph_node] Array.set!
attribute [lean_graph_node] Array.get?
attribute [lean_graph_node] Array.get!
attribute [lean_graph_node] Array.push
attribute [lean_graph_node] Array.append
attribute [lean_graph_node] Array.size
attribute [lean_graph_node] Array.size

set_option trace.HouLean.lean_graph true
@[lean_graph_node]
def foo (x y z : Float) : struct {r : Float, g : Float, b : Float} := ⟨x,y,z⟩

@[lean_graph_node]
def bar (x y : Float) : struct {r : Float, g : Float} := ⟨x,y⟩

open Lean
run_meta
  let types ← LeanGraph.getGraphTypes
  IO.println (toJson types)

end HouLean
