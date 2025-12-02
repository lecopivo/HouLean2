import HouLean.Python.Translation

open HouLean Python IO

-- Test True/False/None
def testBooleans : IO Unit := python%
  flag = True
  if flag:
    println("Flag is true")

  done = False
  while not done:
    println("Looping once")
    done = True

/--
info: Flag is true
Looping once
-/
#guard_msgs in
#eval testBooleans

-- Test break and continue
def testLoopControl : IO Unit := python%
  for i in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]:
    if i == 3:
      continue
    if i == 7:
      break
    println(i)

/--
info: 1
2
4
5
6
-/
#guard_msgs in
#eval testLoopControl

#exit
#check show IO _ from do python%
  def double(x):
    return 2*x
  return double(5)

#python_translation
  def double(x):
    return 2*x
  return double(5)

open Lean Meta Elab Term in
elab "maybe_pure%' " t:term : term => do
  let type ← mkFreshExprMVar (some (.sort 2))
  let e ← elabTerm t type -- expectedType?
  let type ← inferType e
  if ← isMonadApp type then
    return e
  else
    elabTerm (← `(pure $t)) none


#check show IO _ from do
  let double := fun x => do
    return 2 * x
  return (double 5)

#check python%
  double = lambda x: 2 * x
  return double(2)

#exit
-- Test lambda expressions
def testLambda : Nat := Id.run <| python%
  double = lambda x: x * 2
  add = lambda x, y: x + y

  result = double(5)
  result = add(result, 3)
  return result


-- Test lambda expressions
def testFunctions : Nat := Id.run <| python%
  def double(x):
    return x * 2

  def add(x, y):
    return x + y

  result = double(5)
  result = add(result, 3)
  return result

/-- info: 13 -/
#guard_msgs in
#eval testLambda  -- Should be 13

-- Test function with parameter reassignment
def testParamReassign : Nat := Id.run <| python%
  def increment(x: Nat):
    x = x + 1
    return x

  return increment(41)

/--
error: aborting evaluation since the expression depends on the 'sorry' axiom, which can lead to runtime instability and crashes.

To attempt to evaluate anyway despite the risks, use the '#eval!' command.
-/
#guard_msgs in
#eval testParamReassign  -- Should be 42

-- Test mixed typed/untyped parameters
def testMixedParams : IO Unit := python
  def scale(x: Nat, factor):
    return x * factor

  result = scale(10, 3)
  print(result)  -- 30

#eval testMixedParams

-- Test typed variable assignment
def testTypedAssign : IO Unit := python
  x: Nat = 5
  y: Float = 3.14
  items: List Nat = [1, 2, 3]

  print(x)
  print(y)
  print(items)

  empty: List String = []
  print(empty)

#eval testTypedAssign

-- Combined example
def combinedTest : IO Unit := python
  numbers = [1, 2, 3, 4, 5]
  mapper = lambda x: x * x

  for n in numbers:
    if n == 4:
      continue
    squared = mapper(n)
    print(squared)

  found = False
  i = 0
  while True:
    if i >= 10:
      break
    if i == 5:
      found = True
      break
    i += 1

  if found:
    print("Found 5!")

#eval combinedTest

-- Test tuple unpacking
def testTupleUnpack : IO Unit := python
  a, b = (1, 2)
  print(a)
  print(b)

  x, y, z = (10, 20, 30)
  print(x + y + z)

  a, b = (b, a)
  print(a)  -- should be 2
  print(b)  -- should be 1

#eval testTupleUnpack

-- Test multiple return values
def divmod (n : Nat) (d : Nat) : Nat × Nat := Id.run <| python
  q = n / d
  r = n % d
  return q, r

#eval divmod 17 5  -- (3, 2)

-- Using multiple return with unpacking
def testMultiReturn : IO Unit := python
  def minmax(a: Nat, b: Nat):
    if a < b:
      return a, b
    else:
      return b, a

  lo, hi = minmax(10, 3)
  print(lo)  -- 3
  print(hi)  -- 10

#eval testMultiReturn

-- Mixed: some vars new, some existing
def testMixedUnpack : IO Unit := python
  x = 100
  x, y = (42, 99)
  print(x)  -- 42
  print(y)  -- 99

#eval testMixedUnpack

-- Test struct unpacking with numeric projection
structure Vec3 where
  x : Float
  y : Float
  z : Float

def testStructUnpack : IO Unit := python
  v = Vec3.mk 1.0 2.0 3.0

  {a, b, c} = v
  print(a)  -- 1.0
  print(b)  -- 2.0
  print(c)  -- 3.0

  p = (10, 20)
  {x, y} = p
  print(x)  -- 10
  print(y)  -- 20

#eval testStructUnpack

-- Comparison: tuple vs struct unpacking
def unpackComparison : IO Unit := python
  triple = (1, 2, 3)

  -- Tuple unpacking: nested Prod projection .1, .2.1, .2.2
  a, b, c = triple

  print(a)
  print(b)
  print(c)

#eval unpackComparison

-- Test elif chains
def testElif : IO Unit := python
  x = 75

  if x >= 90:
    print("A")
  elif x >= 80:
    print("B")
  elif x >= 70:
    print("C")
  elif x >= 60:
    print("D")
  else:
    print("F")

#eval testElif  -- Should print "C"

-- Test elif without final else
def testElifNoElse : IO Unit := python
  x = 50

  if x > 100:
    print("big")
  elif x > 75:
    print("medium")
  elif x > 25:
    print("small")

#eval testElifNoElse  -- Should print "small"

-- Test new augmented assignments
def testAugAssign : IO Unit := python
  x = 10
  x *= 3
  print(x)  -- 30

  x /= 2
  print(x)  -- 15

  x %= 4
  print(x)  -- 3

#eval testAugAssign

-- Test power operator
def testPower : Nat := Id.run <| python
  x = 2 ** 10
  return x

#eval testPower  -- 1024

-- Test unary minus
def testUnaryMinus : Int := Id.run <| python
  x = 5
  y = -x
  z = -(-y)
  return y + z

#eval testUnaryMinus  -- 0

-- Test ternary expression
def testTernary : IO Unit := python
  x = 10
  result = "big" if x > 5 else "small"
  print(result)  -- "big"

  grade = "A" if x >= 90 else "B" if x >= 80 else "C"
  print(grade)  -- "C"

#eval testTernary

-- Test `in` operator
def testIn : IO Unit := python
  items = [1, 2, 3, 4, 5]

  if 3 in items:
    print("found 3")

  if 10 in items:
    print("found 10")
  else:
    print("10 not found")

  msg = "yes" if 2 in items else "no"
  print(msg)

#eval testIn

-- Test list comprehensions
def testListComp : IO Unit := python
  numbers = [1, 2, 3, 4, 5]

  doubled = [x * 2 for x in numbers]
  print(doubled)  -- [2, 4, 6, 8, 10]

  evens = [x for x in numbers if x % 2 == 0]
  print(evens)  -- [2, 4]

  doubledEvens = [x * 2 for x in numbers if x % 2 == 0]
  print(doubledEvens)  -- [4, 8]

  squares = [x ** 2 for x in numbers]
  print(squares)  -- [1, 4, 9, 16, 25]

#eval testListComp

-- List comp with tuples
def testListCompTuple : IO Unit := python
  items = [1, 2, 3]
  pairs = [(x, x * x) for x in items]
  print(pairs)

#eval testListCompTuple

-- Test default parameters
def testDefaultParams : IO Unit := python
  def greet(name, greeting="Hello"):
    print(greeting)
    print(name)

  greet("Alice")
  greet("Bob", "Hi")

#eval testDefaultParams

-- Default params with types
def testDefaultParamsTyped : IO Unit := python
  def power(base: Nat, exp: Nat = 2):
    return base ** exp

  print(power(3))      -- 9 (3^2)
  print(power(2, 10))  -- 1024 (2^10)

#eval testDefaultParamsTyped

-- Multiple defaults
def testMultipleDefaults : IO Unit := python
  def configure(host: String = "localhost", port: Nat = 8080, debug: Bool = False):
    print(host)
    print(port)
    print(debug)

  configure()
  configure("example.com")
  configure("example.com", 443)
  configure("example.com", 443, True)

#eval testMultipleDefaults

-- Test keyword arguments in function calls
def testKeywordArgs : IO Unit := python
  def greet(name: String, greeting: String = "Hello", punctuation: String = "!"):
    print(greeting)
    print(name)
    print(punctuation)

  -- All positional
  greet("Alice", "Hi", "?")

  -- Using defaults
  greet("Bob")

  -- Named argument
  greet("Charlie", punctuation="...")

  -- Out of order with names
  greet(punctuation="!!", name="Dave", greeting="Hey")

#eval testKeywordArgs
