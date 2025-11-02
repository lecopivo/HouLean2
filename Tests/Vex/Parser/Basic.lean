import HouLean.Vex.Compiler.Grammar
-- import HouLean.Vex.VexToTerm
-- import Lean


/-
VEX Grammar Tests
-/


open Lean VEX
open Lean Elab Term

-- Test 1: Simple function

/--
info: float square(float x) {
    return x * x;
  }
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  float square(float x) {
    return x * x;
  }  


/--
info: vector addVectors(vector a; vector b) {
    return a + b;
  }
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  vector addVectors(vector a; vector b) {
    return a + b;
  }


-- #guard_msgs in 
-- #eval IO.println <| vexsnippet%
--   struct Ray {
--     vector origin;
--     vector direction;
--     float distance;
--   };

-- Test 4: Control flow
/--
info: float clamp(float val; float min; float max) {
    if (val < min)
      return min;
    else if (val > max)
      return max;
    return val;
  }
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  float clamp(float val; float min; float max) {
    if (val < min)
      return min;
    else if (val > max)
      return max;
    return val;
  }

-- Test 5: Foreach loop


/--
info: float sum_array(float arr[]) {
    float total = 0;
    foreach (float val; arr ) {
      total += val;
    }
    return total;
  }
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  float sum_array(float arr[]) {
    float total = 0;
    foreach (float val; arr ) {
      total += val;
    }
    return total;
  }

-- Test 6: Export parameters
/--
info: void computeColor(vector pos; export vector Cd) {
    Cd = abs(pos);
  }
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  void computeColor(vector pos; export vector Cd) {
    Cd = abs(pos);
  }

-- Test 7: Matrix literal
/--
info: matrix3 rotateX(float angle) {
    float c = cos(angle);
    float s = sin(angle);
    matrix3 m = {1, 0, 0, 0, c, -s, 0, s, c};
    return m;
  }
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  matrix3 rotateX(float angle) {
    float c = cos(angle);
    float s = sin(angle);
    matrix3 m = {1, 0, 0, 0, c, -s, 0, s, c};
    return m;
  }

-- Test 8: Simple attribute access

/--
info: vector displace(vector P; float amount) {
    vector N = normalize(P);
    return P + N * amount;
  }
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  vector displace(vector P; float amount) {
    vector N = normalize(P);
    return P + N * amount;
  }

-- Test 9: Channel attributes
/--
info: void setAttributes() {
    @Cd = {1, 0, 0};
    v@N = normalize(@P);
    f@pscale = 0.5;
    i@id = @ptnum;
    s@name = "particle";
  }
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  void setAttributes() {
    @Cd = {1, 0, 0};
    v@N = normalize(@P);
    f@pscale = 0.5;
    i@id = @ptnum;
    s@name = "particle";
  }

-- Test 10: Conditional attribute
/--
info: void processPoint() {
    vector pos = v@P;
    float scale = f@pscale;
    if (scale < 0.1) {
      f@pscale = 0.1;
    }
    v@Cd = normalize(pos);
  }
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  void processPoint() {
    vector pos = v@P;
    float scale = f@pscale;
    if (scale < 0.1) {
      f@pscale = 0.1;
    }
    v@Cd = normalize(pos);
  }

-- Test 11: Dictionary attributes
/--
info: void useDictAttrib() {
    float density = d@density;
    d@temperature = 300.0;
  }
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  void useDictAttrib() {
    float density = d@density;
    d@temperature = 300.0;
  }

-- Test 12: Method calls
/--
info: void useVectorMethods() {
    vector v = {1, 2, 3};
    float len = v->length();
    vector normalized = v->normalize();
  }
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  void useVectorMethods() {
    vector v = {1, 2, 3};
    float len = v->length();
    vector normalized = v->normalize();
  }

-- Test 13: String methods
/--
info: void stringMethods() {
    string s = "hello world";
    int len = s->length();
    string upper = s->upper();
    string sub = s->substr(0, 5);
  }
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  void stringMethods() {
    string s = "hello world";
    int len = s->length();
    string upper = s->upper();
    string sub = s->substr(0, 5);
  }

-- Test 14: Array methods

/--
info: void arrayMethods() {
    int arr[] = {1, 2, 3, 4, 5};
    int len = arr->length();
    arr->push(6);
    int last = arr->pop();
  }
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  void arrayMethods() {
    int arr[] = {1, 2, 3, 4, 5};
    int len = arr->length();
    arr->push(6);
    int last = arr->pop();
  }
  

-- VEX Snippet tests (for wrangle nodes)

-- Snippet Test 1: Function definition with usage
/--
info: float foo(int a; int b) {
    return a + b;
  }

  int c = foo(10, 20);
  v@P = {(float)c, 2.0, 12.0};
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  float foo(int a; int b) {
    return a + b;
  }

  int c = foo(10, 20);
  v@P = {(float)c, 2.0, 12.0};

-- Snippet Test 2: Color from Bounding Box
/-- info: @Cd = relbbox(0, @P); -/
#guard_msgs in
#eval IO.println <| vexsnippet%
  @Cd = relbbox(0, @P);

-- Snippet Test 3: Random Point Color
/--
info: float seed = 0.12345;
  @Cd = rand(seed + @ptnum);
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  float seed = 0.12345;
  @Cd = rand(seed + @ptnum);

-- Snippet Test 4: Color Based on Threshold
/--
info: int condition = (@P.x > 0) ? 1 : 0;
  @Cd = set(condition, 0, 0);
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  int condition = (@P.x > 0) ? 1 : 0;
  @Cd = set(condition, 0, 0);

-- Snippet Test 5: Point Group on Threshold
/--
info: string group = "mygroup";
  int condition = (@P.x > 0) ? 1 : 0;
  setpointgroup(geoself(), group, @ptnum, condition);
  @Cd = set(condition, 0, 0);
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  string group = "mygroup";
  int condition = (@P.x > 0) ? 1 : 0;
  setpointgroup(geoself(), group, @ptnum, condition);
  @Cd = set(condition, 0, 0);

-- Snippet Test 6: Fetch Second Input Cd Attribute
/-- info: @Cd = point(1, "Cd", @ptnum); -/
#guard_msgs in
#eval IO.println <| vexsnippet%
  @Cd = point(1, "Cd", @ptnum);

-- Snippet Test 7: Fetch Second Input Attribute by id/name
/--
info: int match_pt = findattribval(1, "point", "id", @id);
  @P = point(1, "P", match_pt);
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  int match_pt = findattribval(1, "point", "id", @id);
  @P = point(1, "P", match_pt);

-- Snippet Test 8: Nearest Point Distance
/--
info: int closept = nearpoint(1, @P);
  vector value = point(1, "P", closept);
  @dist = length(@P - value);
  @Cd = set(@dist, 0, 0);
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  int closept = nearpoint(1, @P);
  vector value = point(1, "P", closept);
  @dist = length(@P - value);
  @Cd = set(@dist, 0, 0);

-- Snippet Test 9: Grow Hairs
/--
info: vector dir = {0, 1, 0};
  float len = 1.0;
  int steps = 10;
  float jitter = 0.1;
  float seed = 0.12345;

  vector pos = @P;
  int pr = addprim(geoself(), "polyline");

  addvertex(geoself(), pr, @ptnum);
  for (int i = 0; i < steps; i++) {
    pos += dir * len / steps;
    pos += (vector(rand(@ptnum + seed)) - 0.5) * jitter;

    int pt = addpoint(geoself(), @ptnum);
    setpointattrib(geoself(), "P", pt, pos);

    addvertex(geoself(), pr, pt);
    seed += $$PI;
  }
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  vector dir = {0, 1, 0};
  float len = 1.0;
  int steps = 10;
  float jitter = 0.1;
  float seed = 0.12345;

  vector pos = @P;
  int pr = addprim(geoself(), "polyline");

  addvertex(geoself(), pr, @ptnum);
  for (int i = 0; i < steps; i++) {
    pos += dir * len / steps;
    pos += (vector(rand(@ptnum + seed)) - 0.5) * jitter;

    int pt = addpoint(geoself(), @ptnum);
    setpointattrib(geoself(), "P", pt, pos);

    addvertex(geoself(), pr, pt);
    seed += $$PI;
  }

-- Snippet Test 10: Get Neighbouring Points into Attribute
/-- info: i[]@neighbours = neighbours(0, @ptnum); -/
#guard_msgs in
#eval IO.println <| vexsnippet%
  i[]@neighbours = neighbours(0, @ptnum);

-- Snippet Test 11: Average Neighbouring Points
/--
info: int n[] = neighbours(0, @ptnum);
  vector avgP = @P;

  foreach (int pt; n) {
    avgP += point(0, "P", pt);
  }

  avgP /= len(n) + 1;
  @P = avgP;
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  int n[] = neighbours(0, @ptnum);
  vector avgP = @P;

  foreach (int pt; n) {
    avgP += point(0, "P", pt);
  }

  avgP /= len(n) + 1;
  @P = avgP;

-- Snippet Test 12: Creates a new line between points
/--
info: int createLine(int pt_a; int pt_b) {
    int prim = addprim(0, "polyline");
    addvertex(0, prim, pt_a);
    addvertex(0, prim, pt_b);
    return prim;
  }

  createLine(0, 1);
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  int createLine(int pt_a; int pt_b) {
    int prim = addprim(0, "polyline");
    addvertex(0, prim, pt_a);
    addvertex(0, prim, pt_b);
    return prim;
  }

  createLine(0, 1);

-- Snippet Test 13: Removes the last point on each incoming line
/--
info: int vtx_count = primvertexcount(0, @primnum) - 1;
  int ptnum = primvertex(0, @primnum, vtx_count);
  removepoint(0, ptnum);
-/
#guard_msgs in
#eval IO.println <| vexsnippet%
  int vtx_count = primvertexcount(0, @primnum) - 1;
  int ptnum = primvertex(0, @primnum, vtx_count);
  removepoint(0, ptnum);

