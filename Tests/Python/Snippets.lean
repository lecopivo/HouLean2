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


-- Test lambda expressions
def testLambda : Nat := Id.run <| python%
  double = lambda x: x * 2
  add = lambda x, y: x + y

  result = double(5)
  result = add(result, 3)
  return result

/-- info: 13 -/
#guard_msgs in
#eval testLambda


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
#eval testFunctions

-- -- Test function expressions
-- def testFunctions2 : Float := Id.run <| python%
--   def foo(x : Float):
--     x = sin(x)
--     x = cos(x)
--     return x

--   def add(x, y):
--     return x + y

--   a = foo(5)
--   b = foo(10)
--   result = add(a, b)
--   return result

-- /-- info: 13 -/
-- #guard_msgs in
-- #eval testFunctions2

-- Test function with parameter reassignment
def testParamReassign : Nat := Id.run <| python%
  def increment(x: Nat):
    x = x + 1
    return x

  return increment(41)

/-- info: 42 -/
#guard_msgs in
#eval testParamReassign

-- -- Test mixed typed/untyped parameters
-- def testMixedParams : IO Unit := python%
--   def scale(x: Nat, factor):
--     return x * factor

--   result = scale(10, 3)
--   print(result)  -- 30

-- #eval testMixedParams

-- -- Test typed variable assignment
-- def testTypedAssign : IO Unit := python%
--   x: Nat = 5
--   y: Float = 3.14
--   items: List Nat = [1, 2, 3]

--   print(x)
--   print(y)
--   print(items)

--   empty: List String = []
--   print(empty)

-- #eval testTypedAssign

-- Combined example
def combinedTest : IO Unit := python%
  numbers = [1, 2, 3, 4, 5]
  mapper = lambda x: x * x

  for n in numbers:
    if n == 4:
      continue
    squared = mapper(n)
    IO.println(squared)

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
    IO.println("Found 5!")

/--
info: 1
4
9
25
Found 5!
-/
#guard_msgs in
#eval combinedTest

-- Test tuple unpacking
def testTupleUnpack : IO Unit := python%
  a, b = (1, 2)
  IO.println(a)
  IO.println(b)

  x, y, z = (10, 20, 30)
  IO.println(x + y + z)

  a, b = (b, a)
  IO.println(a)  -- should be 2
  IO.println(b)  -- should be 1


/--
info: 1
2
60
2
1
-/
#guard_msgs in
#eval testTupleUnpack

-- Test multiple return values
def divmod (n : Nat) (d : Nat) : Nat × Nat := Id.run <| python%
  q = n / d
  r = n % d
  return q, r

/-- info: (3, 2) -/
#guard_msgs in
#eval divmod 17 5  -- (3, 2)

-- Using multiple return with unpacking
def testMultiReturn : IO Unit := python%
  def minmax(a: Nat, b: Nat):
    if a < b:
      return a, b
    else:
      return b, a

  lo, hi = minmax(10, 3)
  IO.println(lo)  -- 3
  IO.println(hi)  -- 10

/--
info: 3
10
-/
#guard_msgs in
#eval testMultiReturn

-- Mixed: some vars new, some existing
def testMixedUnpack : IO Unit := python%
  x = 100
  x, y = (42, 99)
  IO.println(x)  -- 42
  IO.println(y)  -- 99


/--
info: 42
99
-/
#guard_msgs in
#eval testMixedUnpack

-- -- Test struct unpacking with numeric projection
-- structure Vec3 where
--   x : Float
--   y : Float
--   z : Float

-- def testStructUnpack : IO Unit := python%
--   v = Vec3.mk 1 2 3

--   {a, b, c} = v
--   IO.println(a)  -- 1.0
--   IO.println(b)  -- 2.0
--   IO.println(c)  -- 3.0

--   p = (10, 20)
--   {x, y} = p
--   IO.println(x)  -- 10
--   IO.println(y)  -- 20

-- #guard_msgs in
-- #eval testStructUnpack

-- Comparison: tuple vs struct unpacking
def unpackComparison : IO Unit := python%
  triple = (1, 2, 3)

  -- Tuple unpacking: nested Prod projection .1, .2.1, .2.2
  a, b, c = triple

  IO.println(a)
  IO.println(b)
  IO.println(c)

/--
info: 1
2
3
-/
#guard_msgs in
#eval unpackComparison

-- -- Test elif chains
-- def testElif : IO Unit := python%
--   x = 75

--   if x >= 90:
--     IO.println("A")
--   elif x >= 80:
--     IO.println("B")
--   elif x >= 70:
--     IO.println("C")
--   elif x >= 60:
--     IO.println("D")
--   else:
--     IO.println("F")

-- #eval testElif  -- Should IO.println "C"

-- -- Test elif without final else
-- def testElifNoElse : IO Unit := python%
--   x = 50

--   if x > 100:
--     IO.println("big")
--   elif x > 75:
--     IO.println("medium")
--   elif x > 25:
--     IO.println("small")

-- #eval testElifNoElse  -- Should IO.println "small"

-- Test new augmented assignments
def testAugAssign : IO Unit := python%
  x = 10
  x *= 3
  IO.println(x)  -- 30

  x /= 2
  IO.println(x)  -- 15

  x %= 4
  IO.println(x)  -- 3

/--
info: 30
15
3
-/
#guard_msgs in
#eval testAugAssign

-- Test power operator
def testPower : Nat := Id.run <| python%
  x = 2 ** 10
  return x

/-- info: 1024 -/
#guard_msgs in
#eval testPower  -- 1024

-- Test unary minus
def testUnaryMinus : Int := Id.run <| python%
  x = 5
  y = -x
  z = -(-y)
  return y + z

/-- info: -10 -/
#guard_msgs in
#eval testUnaryMinus  -- 0

-- -- Test ternary expression
-- def testTernary : IO Unit := python%
--   x = 10
--   result = "big" if x > 5 else "small"
--   IO.println(result)  -- "big"

--   grade = "A" if x >= 90 else "B" if x >= 80 else "C"
--   IO.println(grade)  -- "C"

-- #guard_msgs in
-- #eval testTernary

-- -- Test `in` operator
-- def testIn : IO Unit := python%
--   items = [1, 2, 3, 4, 5]

--   if 3 in items:
--     IO.println("found 3")

--   if 10 in items:
--     IO.println("found 10")
--   else:
--     IO.println("10 not found")

--   msg = "yes" if 2 in items else "no"
--   IO.println(msg)

-- #guard_msgs in
-- #eval testIn

-- Test list comprehensions
def testListComp : IO Unit := python%
  numbers = [1, 2, 3, 4, 5]

  doubled = [x * 2 for x in numbers]
  IO.println(doubled)  -- [2, 4, 6, 8, 10]

  evens = [x for x in numbers if x % 2 == 0]
  IO.println(evens)  -- [2, 4]

  doubledEvens = [x * 2 for x in numbers if x % 2 == 0]
  IO.println(doubledEvens)  -- [4, 8]

  squares = [x ** 2 for x in numbers]
  IO.println(squares)  -- [1, 4, 9, 16, 25]

/--
info: [2, 4, 6, 8, 10]
[2, 4]
[4, 8]
[1, 4, 9, 16, 25]
-/
#guard_msgs in
#eval testListComp

-- List comp with tuples
def testListCompTuple : IO Unit := python%
  items = [1, 2, 3]
  pairs = [(x, x * x) for x in items]
  IO.println(pairs)

/-- info: [(1, 1), (2, 4), (3, 9)] -/
#guard_msgs in
#eval testListCompTuple

-- -- Test default parameters
-- def testDefaultParams : IO Unit := python%
--   def greet(name, greeting="Hello"):
--     IO.println(greeting)
--     IO.println(name)

--   greet("Alice")
--   greet("Bob", "Hi")

-- #guard_msgs in
-- #eval testDefaultParams

-- -- Default params with types
-- def testDefaultParamsTyped : IO Unit := python%
--   def power(base: Nat, exp: Nat = 2):
--     return base ** exp

--   IO.println(power(3))      -- 9 (3^2)
--   IO.println(power(2, 10))  -- 1024 (2^10)

-- #guard_msgs in
-- #eval testDefaultParamsTyped

-- -- Multiple defaults
-- def testMultipleDefaults : IO Unit := python%
--   def configure(host: String = "localhost", port: Nat = 8080, debug: Bool = False):
--     IO.println(host)
--     IO.println(port)
--     IO.println(debug)

--   configure()
--   configure("example.com")
--   configure("example.com", 443)
--   configure("example.com", 443, True)

-- #guard_msgs in
-- #eval testMultipleDefaults

-- -- Test keyword arguments in function calls
-- def testKeywordArgs : IO Unit := python%
--   def greet(name: String, greeting: String = "Hello", punctuation: String = "!"):
--     IO.println(greeting)
--     IO.println(name)
--     IO.println(punctuation)

--   -- All positional
--   greet("Alice", "Hi", "?")

--   -- Using defaults
--   greet("Bob")

--   -- Named argument
--   greet("Charlie", punctuation="...")

--   -- Out of order with names
--   greet(punctuation="!!", name="Dave", greeting="Hey")

-- #guard_msgs in
-- #eval testKeywordArgs
