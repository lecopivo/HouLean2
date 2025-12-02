import HouLean.Python.Translation

open HouLean Python IO

open Lean Elab Command in
elab "#eval_python " e:pyBlock : command => do
  elabCommand (← `(#eval show IO _ from python% $e))


/-- info: Hello python -/
#guard_msgs in
#eval_python
  println("Hello python")


/-- info: 1042 -/
#guard_msgs in
#eval_python
  a = 42
  a += 1000
  println(a)


/-- info: [42, 7, 1, 10, 100] -/
#guard_msgs in
#eval_python
  a : List Nat = [42,7]
  a = a.append([1,10,100])
  println(a)

/--
info: 10
hello
true
-/
#guard_msgs in
#eval_python
  a = (10,"hello", True)
  number, string, bool = a
  println(number)
  println(string)
  println(bool)


-- #eval_python
--   a = (10,"hello", True)
--   {number, stringbool} = a
--   println(number)
--   println(stringbool)


/-- info: yeah I thought so -/
#guard_msgs in
#eval_python
  if True == False:
    println("this can't be true!")
  else:
    println("yeah I thought so")


/-- info: 1100 -/
#guard_msgs in
#eval_python
  a = 10

  if a % 2 == 0:
    a += 1 -- this should execute
  else:
    a *= 10

  if a % 2 == 0:
    a += 3
  else:
    a *= 100 -- this should execute

  println(a)


/--
info: 0
1
2
3
4
-/
#guard_msgs in
#eval_python
  n = 5
  i = 0
  while i < n:
    println(i)
    i += 1


/--
info: 0
1
2
3
4
5
-/
#guard_msgs in
#eval_python
  for i in [0,1,2,3,4,5]:
    println(i)


/--
info: 0
1
3
-/
#guard_msgs in
#eval_python
  for i in [0,1,2,3,4,5]:
    if i == 2:
      continue
    if i == 4:
      break
    println(i)


/--
info: 0
incrementing
1
incrementing
2
incrementing
3
incrementing
-/
#guard_msgs in
#eval_python
  def increment(i : Nat):
    println("incrementing")
    return i + 1

  i = 0
  while i < 4:
    println(i)
    i = increment(i)
