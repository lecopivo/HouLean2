import HouLean.Python.Translation

open HouLean Python Float

macro "python_expr%" e:pyExpr : term => do
  let (t,_) ← translatePyExpr e {} {}
  `(do $t:term)

open Lean Elab Command in
elab "#eval_python_expr" e:pyExpr : command => do
  elabCommand (← `(#eval show Id _ from python_expr% $e))

def x : Float := 0.1
def y : Float := 1.0
def z : Float := 10.0

def a : Int := -3
def b : Int := 7
def c : Int := 42

/-- info: 0.099833 -/
#guard_msgs in
#eval_python_expr (sin(x))

/-- info: (0.099833, 168) -/
#guard_msgs in
#eval_python_expr (sin(x), (a + b)*c)

/-- info: 2.200000 -/
#guard_msgs in
#eval_python_expr (lambda x: x + x)(x+y)

/-- info: 5.636230 -/
#guard_msgs in
#eval_python_expr (lambda x: x + x)(sin(x)+exp(y))

/-- info: 8 -/
#guard_msgs in
#eval_python_expr (lambda x: x + x)(a+b)

/-- info: 4 -/
#guard_msgs in
#eval_python_expr [1,2,3,4][3]

/-- info: [10, 20, 30, 40] -/
#guard_msgs in
#eval_python_expr [10*x for x in [1,2,3,4]]

/-- info: [20, 40] -/
#guard_msgs in
#eval_python_expr [10*x for x in [1,2,3,4] if x % 2 == 0]

/-- info: true -/
#guard_msgs in
#eval_python_expr 2 in [1,2,3,4]

/-- info: false -/
#guard_msgs in
#eval_python_expr 10 in [1,2,3,4]
