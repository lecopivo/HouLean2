import HouLean
import HouLean.LeanGraph.GraphToCode

open Lean HouLean Apex LeanGraph Meta Std Elab



variable (input : HouLean.Apex.Geometry)


def typeCheckGraph (filePath : String) : TermElabM Unit := do
  let s ← IO.FS.withFile filePath .read fun file => do
    file.readToEnd

  let .ok json := Json.parse s
    | throwError "failed parsing json!"
  let .ok graph := fromJson? (α:=LeanGraph) json
    | throwError "failed loading graph!"

  let r ← typeCheck graph

  let types := r.graph.nodes.map (·.type)
  let t := types.map (fun t => t.toString)

  let mvars := (r.mainProgram.collectMVars {}).result

  logInfo r.code
  logInfo m!"{t}"
  if mvars.size ≠ 0 then
    logInfo m!"mvar types: {← mvars.mapM (fun m => do pure (Expr.mvar m, ← m.getType))}"


/--
info: import HouLean

def run :=
  let output := fun (input : Float) => input;
  output
---
info: [output HouLean.output
 inputs:
 output : Float
 outputs:
 function : Float → Float,
 input HouLean.input
 inputs:
 type : Type
 outputs:
 input : Float]
-/
#guard_msgs in
run_elab typeCheckGraph "Tests/LeanGraph/graph_to_code_unit_test_1.json"


/--
info: import HouLean

def run :=
  let output := fun (input : ?m.1) => input;
  output
---
info: [output HouLean.output
 inputs:
 output : ?_
 outputs:
 function : ?m.1 → ?m.1,
 input HouLean.input
 inputs:
 type : Type
 outputs:
 input : ?_]
---
info: mvar types: [(?m.1, Type)]
-/
#guard_msgs in
run_elab typeCheckGraph "Tests/LeanGraph/graph_to_code_unit_test_2.json"


/--
info: import HouLean

def run :=
  let output := fun (input : Float) => input;
  output
---
info: [output HouLean.output
 inputs:
 output : Float
 outputs:
 function : Float → Float,
 input HouLean.input
 inputs:
 type : Type
 outputs:
 input : Float,
 Float_add Float.add
 inputs:
 a : Float
 a : Float
 outputs:
 output : Float]
-/
#guard_msgs in
run_elab typeCheckGraph "Tests/LeanGraph/graph_to_code_unit_test_3.json"

/--
info: import HouLean

def run :=
  withVisualizer
    (let output_1 := ?m.11;
    output_1)
---
info: [output HouLean.output
 inputs:
 output : Float
 outputs:
 function : Float → Float,
 input HouLean.input
 inputs:
 type : Type
 outputs:
 input : Float,
 output HouLean.output
 inputs:
 output : ?_
 outputs:
 function : ?_]
---
info: mvar types: [(?m.11, VisualizeM Geometry)]
-/
#guard_msgs in
run_elab typeCheckGraph "Tests/LeanGraph/graph_to_code_unit_test_4.json"


/--
info: import HouLean

def run :=
  let output := fun (input : Float) => input;
  fun (input_1 : ?m.7) => ?m.18 input_1
---
info: [output HouLean.output
 inputs:
 output : Float
 outputs:
 function : Float → Float,
 input HouLean.input
 inputs:
 type : Type
 outputs:
 input : Float,
 input HouLean.input
 inputs:
 type : Type
 outputs:
 input_1 : ?_]
---
info: mvar types: [(?m.14, (Float → Float) → Type),
 (?m.18,
  let output := fun input => input;
  (input_1 : ?m.7) → ?m.17 input_1)]
-/
#guard_msgs in
run_elab typeCheckGraph "Tests/LeanGraph/graph_to_code_unit_test_5.json"

/--
info: import HouLean

def run :=
  let output := fun (input : Float) => input;
  output
---
info: [output HouLean.output
 inputs:
 output : Float
 outputs:
 function : Float → Float,
 input HouLean.input
 inputs:
 type : Type
 outputs:
 input : Float,
 Arith_add HAdd.hAdd
 inputs:
 a : ?_
 a : ?_
 outputs:
 output : ?_]
-/
#guard_msgs in
run_elab typeCheckGraph "Tests/LeanGraph/graph_to_code_unit_test_6.json"


-- todo: node not connected to an output does not get updated!
/--
info: import HouLean

def run :=
  let output := fun (input : Float) => input;
  output
---
info: [output HouLean.output
 inputs:
 output : Float
 outputs:
 function : Float → Float,
 input HouLean.input
 inputs:
 type : Type
 outputs:
 input : Float,
 Arith_add HAdd.hAdd
 inputs:
 a : Float
 a : Float
 outputs:
 output : Float]
-/
#guard_msgs in
run_elab typeCheckGraph "Tests/LeanGraph/graph_to_code_unit_test_7.json"



/--
info: import HouLean

def run :=
  let output := fun (input : Float) =>
    let Arith_add := input + input;
    let Prod_fst := (input, Arith_add).fst;
    Prod_fst;
  output
---
info: [output HouLean.output
 inputs:
 output : Float
 outputs:
 function : Float → Float,
 input HouLean.input
 inputs:
 type : Type
 outputs:
 input : Float,
 Arith_add HAdd.hAdd
 inputs:
 a : Float
 a : Float
 outputs:
 output : Float,
 Prod_fst Prod.fst
 inputs:
 self : {
   fst : Float
   snd : Float
  : Prod Float Float}
 outputs:
 output : Float]
-/
#guard_msgs in
run_elab typeCheckGraph "Tests/LeanGraph/graph_to_code_unit_test_8.json"

/--
info: import HouLean

def run :=
  fun (input : Float) =>
    let Math_cos := Math.cos input;
    Math_cos
---
info: [input HouLean.input
 inputs:
 type : Type
 outputs:
 input : Float,
 output HouLean.output
 inputs:
 output : ?_
 outputs:
 function : ?_,
 Math_cos HouLean.Math.Cos.cos
 inputs:
 x : Float
 outputs:
 output : Float]
-/
#guard_msgs in
run_elab typeCheckGraph "Tests/LeanGraph/graph_to_code_unit_test_9.json"


/--
info: import HouLean

def run :=
  let Vector3_add := default.add default;
  let Arith_add := Vector3_add + { z := Vector3_add.y : Vector3 };
  Arith_add
---
info: [input HouLean.input
 inputs:
 type : Type
 outputs:
 input : Float,
 Vector3_add HouLean.Vector3.add
 inputs:
 a : HouLean.Vector3
 b : HouLean.Vector3
 outputs:
 output : HouLean.Vector3,
 Arith_add HAdd.hAdd
 inputs:
 a : HouLean.Vector3
 a : HouLean.Vector3
 outputs:
 output : HouLean.Vector3,
 output HouLean.output
 inputs:
 output : Float
 outputs:
 function : Float → Float]
-/
#guard_msgs in
run_elab typeCheckGraph "Tests/LeanGraph/graph_to_code_unit_test_10.json"


/--
info: import HouLean

def run :=
  let output := fun (input : Float) => input;
  output
---
info: [output HouLean.output
 inputs:
 output : Float
 outputs:
 function : Float → Float,
 input HouLean.input
 inputs:
 type : Type
 outputs:
 input : Float,
 Arith_add HAdd.hAdd
 inputs:
 a : Float
 a : Float
 outputs:
 output : Float,
 Arith_add HAdd.hAdd
 inputs:
 a : Float
 a : Float
 outputs:
 output : Float]
-/
#guard_msgs in
run_elab typeCheckGraph "Tests/LeanGraph/graph_to_code_unit_test_11.json"

/--
info: import HouLean

def run :=
  withVisualizer
    (let output := ?m.18;
    output)
---
info: [input HouLean.input
 inputs:
 type : Type
 outputs:
 input : ?_,
 eval HouLean.eval
 inputs:
 function : ?m.1 → ?m.1
 input : ?_
 outputs:
 output : ?_,
 output HouLean.output
 inputs:
 output : ?_
 outputs:
 function : ?_,
 output HouLean.output
 inputs:
 output : ?_
 outputs:
 function : ?m.1 → ?m.1]
---
info: mvar types: [(?m.18, VisualizeM Geometry)]
-/
#guard_msgs in
run_elab typeCheckGraph "Tests/LeanGraph/graph_to_code_unit_test_12.json"


/--
info: import HouLean

def run :=
  fun (input : Vector3) =>
    let Vector3_abs := input.abs;
    fun (input_1 : Vector3) =>
    let Vector3_add := input_1.add Vector3_abs;
    Vector3_add
---
info: [input HouLean.input
 inputs:
 type : Type
 outputs:
 input : HouLean.Vector3,
 Vector3_abs HouLean.Vector3.abs
 inputs:
 v : HouLean.Vector3
 outputs:
 output : HouLean.Vector3,
 Vector3_add HouLean.Vector3.add
 inputs:
 a : HouLean.Vector3
 b : HouLean.Vector3
 outputs:
 output : HouLean.Vector3,
 input HouLean.input
 inputs:
 type : Type
 outputs:
 input_1 : HouLean.Vector3]
-/
#guard_msgs in
run_elab typeCheckGraph "Tests/LeanGraph/graph_to_code_unit_test_13.json"

/--
info: import HouLean

def run :=
  let output_1 := fun (input : ?m.1) => input;
  let eval := eval output_1 default;
  eval
---
info: [input HouLean.input
 inputs:
 type : Type
 outputs:
 input : ?_,
 eval HouLean.eval
 inputs:
 function : ?m.1 → ?m.1
 input : ?_
 outputs:
 output : ?_,
 output HouLean.output
 inputs:
 output : ?_
 outputs:
 function : ?m.1 → ?m.1]
---
info: mvar types: [(?m.1, Type), (?m.14, Inhabited ?m.1)]
-/
#guard_msgs in
run_elab typeCheckGraph "Tests/LeanGraph/graph_to_code_unit_test_14.json"


/--
info: import HouLean

def run :=
  fun (input : Geometry) =>
    withVisualizer
      (let output := fun (input : Geometry) => do
        let visualize ← visualize "visualize" input default
        pure visualize;
      output input)
---
info: [input HouLean.input
 inputs:
 type : Type
 outputs:
 input : HouLean.Apex.Geometry,
 output HouLean.output
 inputs:
 output : HouLean.Apex.VisualizeM HouLean.Apex.Geometry
 outputs:
 function : Geometry → VisualizeM Geometry,
 visualize HouLean.Apex.visualize
 inputs:
 nodeName : String
 x : HouLean.Apex.Geometry
 opts : Unit
 outputs:
 output : HouLean.Apex.VisualizeM HouLean.Apex.Geometry]
-/
#guard_msgs in
run_elab typeCheckGraph "Tests/LeanGraph/graph_to_code_unit_test_19.json"
