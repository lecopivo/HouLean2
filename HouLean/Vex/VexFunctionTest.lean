import HouLean.Vex.VexFunction
-- Example VEX functions (these should be in a separate file in practice)
-- Uncomment when testing in a separate module

open VEX

-- Test 1: Simple function
vexfunction
float square(float x) {
  return x * x;
}

/-- info: 9.000000 -/
#guard_msgs in
#eval square(3.0)

-- Test 2: Vector operations
vexfunction
vector addVectors(vector a; vector b) {
  return a + b;
}

-- Test 3: Control flow
vexfunction
float clamp(float val; float min; float max) {
  if (val < min)
    return min;
  else if (val > max)
    return max;
  return val;
}

/-- info: 5.000000 -/
#guard_msgs in
#eval clamp(3.0, 5.0, 10.0)

/-- info: 7.000000 -/
#guard_msgs in
#eval clamp(7.0, 5.0, 10.0)

/-- info: 10.000000 -/
#guard_msgs in
#eval clamp(15.0, 5.0, 10.0)

-- Test 4: Multiple arguments
vexfunction
float lerp(float a; float b; float t) {
  return a + (b - a) * t;
}

/-- info: 5.000000 -/
#guard_msgs in
#eval lerp(0.0, 10.0, 0.5)

-- Test 5: Displacement
vexfunction
vector displace(vector P; float amount) {
  vector N = normalize(P);
  return P + N * amount;
}

-- Test 6: Integer operations
vexfunction
int factorial(int n) {
  int result = 1;
  for (int i = 2; i <= n; i++) {
    result = result * i;
  }
  return result;
}

/-- info: 120 -/
#guard_msgs in
#eval factorial(5)

-- Test 7: Conditional logic
vexfunction
int sign(float x) {
  if (x > 0.0)
    return 1;
  else if (x < 0.0)
    return -1;
  return 0;
}

/-- info: 1 -/
#guard_msgs in
#eval sign(5.0)

/-- info: -1 -/
#guard_msgs in
#eval sign(-3.0)

/-- info: 0 -/
#guard_msgs in
#eval sign(0.0)

-- Test 8: Overloaded functions with different arities
vexfunction
float add(float x; float y) {
  return x + y;
}

vexfunction
float add(float x; float y; float z) {
  return x + y + z;
}

vexfunction
float add(float x; float y; float z; float w) {
  return x + y + z + w;
}

/-- info: 3.000000 -/
#guard_msgs in
#eval add(1.0, 2.0)

/-- info: 6.000000 -/
#guard_msgs in
#eval add(1.0, 2.0, 3.0)

/-- info: 10.000000 -/
#guard_msgs in
#eval add(1.0, 2.0, 3.0, 4.0)

-- Test 9: Mixed type overloading
vexfunction
int add(int x; int y) {
  return x + y;
}

/-- info: 3 -/
#guard_msgs in
#eval add(1, 2)

-- Test 10: Multiplication overloading
vexfunction
float mul(float x; float y) {
  return x * y;
}

vexfunction
float mul(float x; float y; float z; float w) {
  return x * y * z * w;
}

/-- info: 200.000000 -/
#guard_msgs in
#eval mul(10.0, 20.0)

/-- info: 24.000000 -/
#guard_msgs in
#eval mul(1.0, 2.0, 3.0, 4.0)

-- Test 11: Loop with accumulator
vexfunction
int sumTo(int n) {
  int sum = 0;
  for (int i = 1; i <= n; i++) {
    sum = sum + i;
  }
  return sum;
}

/-- info: 55 -/
#guard_msgs in
#eval sumTo(10)

-- Test 12: Nested conditionals
vexfunction
float categorize(float x) {
  if (x < 0.0) {
    if (x < -10.0)
      return -2.0;
    return -1.0;
  }
  if (x > 10.0)
    return 2.0;
  return 1.0;
}

/-- info: -2.000000 -/
#guard_msgs in
#eval categorize(-15.0)

/-- info: -1.000000 -/
#guard_msgs in
#eval categorize(-5.0)

/-- info: 1.000000 -/
#guard_msgs in
#eval categorize(5.0)

/-- info: 2.000000 -/
#guard_msgs in
#eval categorize(15.0)

-- Test 13: Float power (simple iterative version)
vexfunction
float pow(float base; int exp) {
  float result = 1.0;
  for (int i = 0; i < exp; i++) {
    result = result * base;
  }
  return result;
}

/-- info: 8.000000 -/
#guard_msgs in
#eval pow(2.0, 3)

/-- info: 81.000000 -/
#guard_msgs in
#eval pow(3.0, 4)

-- Test 14: Distance calculation
vexfunction
float distance2d(float x1; float y1; float x2; float y2) {
  float dx = x2 - x1;
  float dy = y2 - y1;
  return sqrt(dx * dx + dy * dy);
}

-- Test 15: Error cases - type mismatch
/--
error: no matching overload for add

Provided arguments: String × String

Available overloads:
  • add : Float × Float → Float
  • add : Float × Float × Float → Float
  • add : Float × Float × Float × Float → Float
  • add : Int × Int → Int
-/
#guard_msgs in
#eval add("a", "b")

-- Test 16: Error cases - wrong arity
/--
error: no matching overload for add

Provided arguments: ?m.12042

Available overloads:
  • add : Float × Float → Float
  • add : Float × Float × Float → Float
  • add : Float × Float × Float × Float → Float
  • add : Int × Int → Int
-/
#guard_msgs in
#eval add(1.0)

-- Test 17: Max function
vexfunction
float max(float a; float b) {
  if (a > b)
    return a;
  return b;
}

vexfunction
int max(int a; int b) {
  if (a > b)
    return a;
  return b;
}

/-- info: 10.000000 -/
#guard_msgs in
#eval max(10.0, 5.0)

/-- info: 10 -/
#guard_msgs in
#eval max(10, 5)

-- Test 18: Min function
vexfunction
float min(float a; float b) {
  if (a < b)
    return a;
  return b;
}

/-- info: 3.000000 -/
#guard_msgs in
#eval min(3.0, 7.0)

-- Test 19: Absolute value
vexfunction
float abs(float x) {
  if (x < 0.0)
    return -x;
  return x;
}

vexfunction
int abs(int x) {
  if (x < 0)
    return -x;
  return x;
}

/-- info: 5.000000 -/
#guard_msgs in
#eval abs(-5.0)

/-- info: 5.000000 -/
#guard_msgs in
#eval abs(5.0)

/-- info: 5 -/
#guard_msgs in
#eval abs(-5)

-- Test 20: Fibonacci
vexfunction
int fib(int n) {
  if (n <= 1)
    return n;
  int a = 0;
  int b = 1;
  for (int i = 2; i <= n; i++) {
    int temp = a + b;
    a = b;
    b = temp;
  }
  return b;
}

/-- info: 34 -/
#guard_msgs in
#eval fib(9)

/-- info: 55 -/
#guard_msgs in
#eval fib(10)

/-- info: 89 -/
#guard_msgs in
#eval fib(11)
