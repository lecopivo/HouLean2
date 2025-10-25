import HouLean.Vex.Parser
import HouLean.Vex.VexToTerm
import Lean

/-
VEX Grammar Tests
-/

namespace VEX.Test

open Lean VEX
open Lean Elab Term

-- Test 1: Simple function

/--
info:  def  square  (  x  :  Float  )  :  Float  :=  do  return  x  *  x ⏎
---
info:  float  square  (  float  x  )  {  return  x  *  x  ;  }
-/
#guard_msgs in
run_elab
  let s ← `(vexFuncDecl|
    float square(float x) {
      return x * x;
    })

  let ctx := VEX.defaultPointContext (mkIdent `ptnum) (mkIdent `primnum) (mkIdent `vtxnum)
  let x ← liftMacroM (VEX.vexFuncDeclToCommand s ctx)
  IO.println (s.raw.prettyPrint)
  logInfo m!"{x.raw.prettyPrint}"

-- Test 2: Vector operations
/--
info:  vector  addVectors  (  vector  a  ;  vector  b  )  {  return  a  +  b  ;  }
  def  addVectors  (  a  :  Vector3  )  (  b  :  Vector3  )  :  Vector3  :=  do  return  a  +  b
-/
#guard_msgs in
run_elab
  let s ← `(vexFuncDecl|
    vector addVectors(vector a; vector b) {
      return a + b;
    })
  
  let ctx := VEX.defaultPointContext (mkIdent `ptnum) (mkIdent `primnum) (mkIdent `vtxnum)
  let x ← liftMacroM (VEX.vexFuncDeclToCommand s ctx)
  IO.println (s.raw.prettyPrint)
  IO.println (x.raw.prettyPrint)

-- Test 3: Struct definition
/--
info:  struct  Ray  {  vector  origin  ;  vector  direction  ;  float  distance  ;  }  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexProgram|
    struct Ray {
      vector origin;
      vector direction;
      float distance;
    };
  )
  IO.println (s.raw.prettyPrint)

-- Test 4: Control flow
/--
info:  float  clamp  (  float  val  ;  float  min  ;  float  max  )  {  if  (  val  <  min  )  return  min  ;  else  if  (  val  >  max  )  return  max  ;  return  val  ;  }
  def  clamp  (  val  :  Float  )  (  min  :  Float  )  (  max  :  Float  )  :  Float  :=  do  if  val  <  min  then  do  return  min  else  do  if  val  >  max  then  do  return  max  return  val
-/
#guard_msgs in
run_elab
  let s ← `(vexFuncDecl|
    float clamp(float val; float min; float max) {
      if (val < min)
        return min;
      else if (val > max)
        return max;
      return val;
    })
  
  let ctx := VEX.defaultPointContext (mkIdent `ptnum) (mkIdent `primnum) (mkIdent `vtxnum)
  let x ← liftMacroM (VEX.vexFuncDeclToCommand s ctx)
  IO.println (s.raw.prettyPrint)
  IO.println (x.raw.prettyPrint)

-- Test 5: Foreach loop
/--
info:  float  sum_array  (  float  arr  [  ]  )  {  float  total  =  0  ;  foreach  (  float  val  ;  arr  )  {  total  +=  val  ;  }  return  total  ;  }
-/
#guard_msgs in
run_meta
  let s ← `(vexProgram|
    float sum_array(float arr[]) {
      float total = 0;
      foreach (float val; arr) {
        total += val;
      }
      return total;
    })
  IO.println (s.raw.prettyPrint)

-- Test 6: Export parameters
/--
info:  void  computeColor  (  vector  pos  ;  export  vector  Cd  )  {  Cd  =  abs  (  pos  )  ;  }
  def  computeColor  (  pos  :  Vector3  )  (  Cd  :  Vector3  )  :  Unit  :=  do  Cd  :=  abs  pos
-/
#guard_msgs in
run_elab
  let s ← `(vexFuncDecl|
    void computeColor(vector pos; export vector Cd) {
      Cd = abs(pos);
    })
  
  let ctx := VEX.defaultPointContext (mkIdent `ptnum) (mkIdent `primnum) (mkIdent `vtxnum)
  let x ← liftMacroM (VEX.vexFuncDeclToCommand s ctx)
  IO.println (s.raw.prettyPrint)
  IO.println (x.raw.prettyPrint)

-- Test 7: Matrix literal
/--
info:  matrix3  rotateX  (  float  angle  )  {  float  c  =  cos  (  angle  )  ;  float  s  =  sin  (  angle  )  ;  matrix3  m  =  {  1  ,  0  ,  0  ,  0  ,  c  ,  -  s  ,  0  ,  s  ,  c  }  ;  return  m  ;  } ⏎
 def  rotateX  (  angle  :  Float  )  :  Matrix3  :=  do  let  mut  c  :  Float  :=  cos  angle  let  mut  s  :  Float  :=  sin  angle  let  mut  m  :  Matrix3  :=  #vex[  c  ]  return  m
-/
#guard_msgs in
run_elab
  let s ← `(vexFuncDecl|
    matrix3 rotateX(float angle) {
      float c = cos(angle);
      float s = sin(angle);
      matrix3 m = {1, 0, 0, 0, c, -s, 0, s, c};
      return m;
    })
  
  let ctx := VEX.defaultPointContext (mkIdent `ptnum) (mkIdent `primnum) (mkIdent `vtxnum)
  let x ← liftMacroM (VEX.vexFuncDeclToCommand s ctx)
  IO.println (s.raw.prettyPrint)
  IO.println (x.raw.prettyPrint)

-- Test 8: Simple attribute access
/--
info:  vector  displace  (  vector  P  ;  float  amount  )  {  vector  N  =  normalize  (  P  )  ;  return  P  +  N  *  amount  ;  }
  def  displace  (  P  :  Vector3  )  (  amount  :  Float  )  :  Vector3  :=  do  let  mut  N  :  Vector3  :=  normalize  P  return  P  +  N  *  amount
-/
#guard_msgs in
run_elab
  let s ← `(vexFuncDecl|
    vector displace(vector P; float amount) {
      vector N = normalize(P);
      return P + N * amount;
    })
  
  let ctx := VEX.defaultPointContext (mkIdent `ptnum) (mkIdent `primnum) (mkIdent `vtxnum)
  let x ← liftMacroM (VEX.vexFuncDeclToCommand s ctx)
  IO.println (s.raw.prettyPrint)
  IO.println (x.raw.prettyPrint)

-- Test 9: Channel attributes
/--
info:  void  setAttributes  (  )  {  @  Cd  =  {  1  ,  0  ,  0  }  ;  v@  N  =  normalize  (  @  P  )  ;  f@  pscale  =  0.5  ;  i@  id  =  @  ptnum  ;  s@  name  =  "particle"  ;  } ⏎
 def  setAttributes  :  Unit  :=  do  setPointAttrib  "Cd"  #vex[  0  ]  setPointVectorAttrib  "N"  normalize  getPointAttrib  "P"  setPointFloatAttrib  "pscale"  0.5  setPointIntAttrib  "id"  ptnum  setPointStringAttrib  "name"  "particle"
-/
#guard_msgs in
run_elab
  let s ← `(vexFuncDecl|
    void setAttributes() {
      @Cd = {1, 0, 0};
      v@N = normalize(@P);
      f@pscale = 0.5;
      i@id = @ptnum;
      s@name = "particle";
    })
  
  let ctx := VEX.defaultPointContext (mkIdent `ptnum) (mkIdent `primnum) (mkIdent `vtxnum)
  let x ← liftMacroM (VEX.vexFuncDeclToCommand s ctx)
  IO.println (s.raw.prettyPrint)
  IO.println (x.raw.prettyPrint)

-- Test 10: Conditional attribute
/--
info:  void  processPoint  (  )  {  vector  pos  =  v@  P  ;  float  scale  =  f@  pscale  ;  if  (  scale  <  0.1  )  {  f@  pscale  =  0.1  ;  }  v@  Cd  =  normalize  (  pos  )  ;  } ⏎
 def  processPoint  :  Unit  :=  do  let  mut  pos  :  Vector3  :=  getPointVectorAttrib  "P"  let  mut  scale  :  Float  :=  getPointFloatAttrib  "pscale"  if  scale  <  0.1  then  do  setPointFloatAttrib  "pscale"  0.1  setPointVectorAttrib  "Cd"  normalize  pos
-/
#guard_msgs in
run_elab
  let s ← `(vexFuncDecl|
    void processPoint() {
      vector pos = v@P;
      float scale = f@pscale;
      if (scale < 0.1) {
        f@pscale = 0.1;
      }
      v@Cd = normalize(pos);
    })
  
  let ctx := VEX.defaultPointContext (mkIdent `ptnum) (mkIdent `primnum) (mkIdent `vtxnum)
  let x ← liftMacroM (VEX.vexFuncDeclToCommand s ctx)
  IO.println (s.raw.prettyPrint)
  IO.println (x.raw.prettyPrint)

-- Test 11: Dictionary attributes
/--
info:  void  useDictAttrib  (  )  {  float  density  =  d@  density  ;  d@  temperature  =  300.0  ;  }
  def  useDictAttrib  :  Unit  :=  do  let  mut  density  :  Float  :=  getPointDictAttrib  "density"  setPointDictAttrib  "temperature"  300.0
-/
#guard_msgs in
run_elab
  let s ← `(vexFuncDecl|
    void useDictAttrib() {
      float density = d@density;
      d@temperature = 300.0;
    })
  
  let ctx := VEX.defaultPointContext (mkIdent `ptnum) (mkIdent `primnum) (mkIdent `vtxnum)
  let x ← liftMacroM (VEX.vexFuncDeclToCommand s ctx)
  IO.println (s.raw.prettyPrint)
  IO.println (x.raw.prettyPrint)

-- Test 12: Method calls
/--
info:  void  useVectorMethods  (  )  {  vector  v  =  {  1  ,  2  ,  3  }  ;  float  len  =  v  ->  length  (  )  ;  vector  normalized  =  v  ->  normalize  (  )  ;  } ⏎
 def  useVectorMethods  :  Unit  :=  do  let  mut  v  :  Vector3  :=  #vex[  3  ]  let  mut  len  :  Float  :=  v  .  length  let  mut  normalized  :  Vector3  :=  v  .  normalize
-/
#guard_msgs in
run_elab
  let s ← `(vexFuncDecl|
    void useVectorMethods() {
      vector v = {1, 2, 3};
      float len = v->length();
      vector normalized = v->normalize();
    })
  
  let ctx := VEX.defaultPointContext (mkIdent `ptnum) (mkIdent `primnum) (mkIdent `vtxnum)
  let x ← liftMacroM (VEX.vexFuncDeclToCommand s ctx)
  IO.println (s.raw.prettyPrint)
  IO.println (x.raw.prettyPrint)

-- Test 13: String methods
/--
info:  void  stringMethods  (  )  {  string  s  =  "hello world"  ;  int  len  =  s  ->  length  (  )  ;  string  upper  =  s  ->  upper  (  )  ;  string  sub  =  s  ->  substr  (  0  ,  5  )  ;  } ⏎
 def  stringMethods  :  Unit  :=  do  let  mut  s  :  String  :=  "hello world"  let  mut  len  :  Int  :=  s  .  length  let  mut  upper  :  String  :=  s  .  upper  let  mut  sub  :  String  :=  s  .  substr  5
-/
#guard_msgs in
run_elab
  let s ← `(vexFuncDecl|
    void stringMethods() {
      string s = "hello world";
      int len = s->length();
      string upper = s->upper();
      string sub = s->substr(0, 5);
    })
  
  let ctx := VEX.defaultPointContext (mkIdent `ptnum) (mkIdent `primnum) (mkIdent `vtxnum)
  let x ← liftMacroM (VEX.vexFuncDeclToCommand s ctx)
  IO.println (s.raw.prettyPrint)
  IO.println (x.raw.prettyPrint)

-- Test 14: Array methods
-- /--
-- info:  void  arrayMethods  (  )  {  int  arr  [  ]  =  {  1  ,  2  ,  3  ,  4  ,  5  }  ;  int  len  =  arr  ->  length  (  )  ;  arr  ->  push  (  6  )  ;  int  last  =  arr  ->  pop  (  )  ;  }
--   def  arrayMethods  :  Unit  :=  do  let  mut  arr  :  Array  Int  :=  #vex[1,  2,  3,  4,  5]  let  mut  len  :  Int  :=  arr.length  let  _  :=  arr.push  6  let  mut  last  :  Int  :=  arr.pop
-- -/
-- #guard_msgs in
-- run_elab
--   let s ← `(vexFuncDecl|
--     void arrayMethods() {
--       int arr[] = {1, 2, 3, 4, 5};
--       int len = arr->length();
--       arr->push(6);
--       int last = arr->pop();
--     })
  
--   let ctx := VEX.defaultPointContext (mkIdent `ptnum) (mkIdent `primnum) (mkIdent `vtxnum)
--   let x ← liftMacroM (VEX.vexFuncDeclToCommand s ctx)
--   IO.println (s.raw.prettyPrint)
--   IO.println (x.raw.prettyPrint)

-- VEX Snippet tests (for wrangle nodes)

-- Snippet Test 1: Function definition with usage
/--
info:  float  foo  (  int  a  ;  int  b  )  {  return  a  +  b  ;  }  int  c  =  foo  (  10  ,  20  )  ;  v@  P  =  {  (  float  )  c  ,  2.0  ,  12.0  }  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    float foo(int a; int b) {
      return a + b;
    }
    
    int c = foo(10, 20);
    v@P = {(float)c, 2.0, 12.0};
  )
  IO.println (s.raw.prettyPrint)

-- Snippet Test 2: Color from Bounding Box
/--
info:  @  Cd  =  relbbox  (  0  ,  @  P  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    @Cd = relbbox(0, @P);
  )
  IO.println (s.raw.prettyPrint)

-- Snippet Test 3: Random Point Color
/--
info:  float  seed  =  0.12345  ;  @  Cd  =  rand  (  seed  +  @  ptnum  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    float seed = 0.12345;
    @Cd = rand(seed + @ptnum);
  )
  IO.println (s.raw.prettyPrint)

-- Snippet Test 4: Color Based on Threshold
/--
info:  int  condition  =  (  @  P.x  >  0  )  ?  1  :  0  ;  @  Cd  =  set  (  condition  ,  0  ,  0  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int condition = (@P.x > 0) ? 1 : 0;
    @Cd = set(condition, 0, 0);
  )
  IO.println (s.raw.prettyPrint)

-- Snippet Test 5: Point Group on Threshold
/--
info:  string  group  =  "mygroup"  ;  int  condition  =  (  @  P.x  >  0  )  ?  1  :  0  ;  setpointgroup  (  geoself  (  )  ,  group  ,  @  ptnum  ,  condition  )  ;  @  Cd  =  set  (  condition  ,  0  ,  0  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    string group = "mygroup";
    int condition = (@P.x > 0) ? 1 : 0;
    setpointgroup(geoself(), group, @ptnum, condition);
    @Cd = set(condition, 0, 0);
  )
  IO.println (s.raw.prettyPrint)

-- Snippet Test 6: Fetch Second Input Cd Attribute
/--
info:  @  Cd  =  point  (  1  ,  "Cd"  ,  @  ptnum  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    @Cd = point(1, "Cd", @ptnum);
  )
  IO.println (s.raw.prettyPrint)

-- Snippet Test 7: Fetch Second Input Attribute by id/name
/--
info:  int  match_pt  =  findattribval  (  1  ,  "point"  ,  "id"  ,  @  id  )  ;  @  P  =  point  (  1  ,  "P"  ,  match_pt  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int match_pt = findattribval(1, "point", "id", @id);
    @P = point(1, "P", match_pt);
  )
  IO.println (s.raw.prettyPrint)

-- Snippet Test 8: Nearest Point Distance
/--
info:  int  closept  =  nearpoint  (  1  ,  @  P  )  ;  vector  value  =  point  (  1  ,  "P"  ,  closept  )  ;  @  dist  =  length  (  @  P  -  value  )  ;  @  Cd  =  set  (  @  dist  ,  0  ,  0  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int closept = nearpoint(1, @P);
    vector value = point(1, "P", closept);
    @dist = length(@P - value);
    @Cd = set(@dist, 0, 0);
  )
  IO.println (s.raw.prettyPrint)

-- Snippet Test 9: Grow Hairs
/--
info:  vector  dir  =  {  0  ,  1  ,  0  }  ;  float  len  =  1.0  ;  int  steps  =  10  ;  float  jitter  =  0.1  ;  float  seed  =  0.12345  ;  vector  pos  =  @  P  ;  int  pr  =  addprim  (  geoself  (  )  ,  "polyline"  )  ;  addvertex  (  geoself  (  )  ,  pr  ,  @  ptnum  )  ;  for  (  int  i  =  0  ;  i  <  steps  ;  i  ++  )  {  pos  +=  dir  *  len  /  steps  ;  pos  +=  (  vector  (  rand  (  @  ptnum  +  seed  )  )  -  0.5  )  *  jitter  ;  int  pt  =  addpoint  (  geoself  (  )  ,  @  ptnum  )  ;  setpointattrib  (  geoself  (  )  ,  "P"  ,  pt  ,  pos  )  ;  addvertex  (  geoself  (  )  ,  pr  ,  pt  )  ;  seed  +=  $$  PI  ;  }
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
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
  )
  IO.println (s.raw.prettyPrint)

-- Snippet Test 10: Get Neighbouring Points into Attribute
/--
info:  i[]@  neighbours  =  neighbours  (  0  ,  @  ptnum  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    i[]@neighbours = neighbours(0, @ptnum);
  )
  IO.println (s.raw.prettyPrint)

-- Snippet Test 11: Average Neighbouring Points
/--
info:  int  n  [  ]  =  neighbours  (  0  ,  @  ptnum  )  ;  vector  avgP  =  @  P  ;  foreach  (  int  pt  ;  n  )  {  avgP  +=  point  (  0  ,  "P"  ,  pt  )  ;  }  avgP  /=  len  (  n  )  +  1  ;  @  P  =  avgP  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int n[] = neighbours(0, @ptnum);
    vector avgP = @P;
    
    foreach (int pt; n) {
      avgP += point(0, "P", pt);
    }
    
    avgP /= len(n) + 1;
    @P = avgP;
  )
  IO.println (s.raw.prettyPrint)

-- Snippet Test 12: Creates a new line between points
/--
info:  int  createLine  (  int  pt_a  ;  int  pt_b  )  {  int  prim  =  addprim  (  0  ,  "polyline"  )  ;  addvertex  (  0  ,  prim  ,  pt_a  )  ;  addvertex  (  0  ,  prim  ,  pt_b  )  ;  return  prim  ;  }  createLine  (  0  ,  1  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int createLine(int pt_a; int pt_b) {
      int prim = addprim(0, "polyline");
      addvertex(0, prim, pt_a);
      addvertex(0, prim, pt_b);
      return prim;
    }
    
    createLine(0, 1);
  )
  IO.println (s.raw.prettyPrint)

-- Snippet Test 13: Removes the last point on each incoming line
/--
info:  int  vtx_count  =  primvertexcount  (  0  ,  @  primnum  )  -  1  ;  int  ptnum  =  primvertex  (  0  ,  @  primnum  ,  vtx_count  )  ;  removepoint  (  0  ,  ptnum  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int vtx_count = primvertexcount(0, @primnum) - 1;
    int ptnum = primvertex(0, @primnum, vtx_count);
    removepoint(0, ptnum);
  )
  IO.println (s.raw.prettyPrint)

end VEX.Test
