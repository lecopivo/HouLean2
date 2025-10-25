import HouLean.Vex.Parser
import Lean

/-
VEX Snippets Tests

Test cases derived from VEX snippets collection by Kiryha:
https://github.com/kiryha/Houdini/wiki/vex-snippets

These snippets demonstrate common VEX patterns used in Houdini for
geometry manipulation, attribute processing, and procedural effects.

Copyright notice: These code examples are from the kiryha/Houdini wiki
and are used here for testing the VEX parser implementation.
-/

namespace VEX.SnippetTests

open Lean VEX

-- ============================================================================
-- DATATYPES
-- ============================================================================

-- Integer declarations
/-- info:  int  myInteger  =  1  ;  i@  myInteger  =  1  ; -/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int myInteger = 1;
    i@myInteger = 1;
  )
  IO.println (s.raw.prettyPrint)

-- Float declarations
/-- info:  float  myFloat  =  4.14  ;  f@  myFloat  =  3.14  ; -/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    float myFloat = 4.14;
    f@myFloat = 3.14;
  )
  IO.println (s.raw.prettyPrint)

-- String declarations
/--
info:  string  myStiring  =  "C:/cache/animation.abc"  ;  s@  myString  =  "C:/cache/animation.abc"  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    string myStiring = "C:/cache/animation.abc";
    s@myString = "C:/cache/animation.abc";
  )
  IO.println (s.raw.prettyPrint)

-- Array declarations
/--
info:  string  variations  [  ]  =  {  "A"  ,  "B"  ,  "C"  }  ;  string  variables  [  ]  =  array  (  variable_A  ,  variable_B  ,  variable_C  )  ;  s[]@  variations  =  {  "A"  ,  "B"  ,  "C"  }  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    string variations[] = {"A", "B", "C"};
    string variables[] = array(variable_A, variable_B, variable_C);
    s[]@variations = {"A", "B", "C"};
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- DATA TYPE CONVERSION
-- ============================================================================

-- Integer to string
/-- info:  int  number  =  123  ;  string  text  =  itoa  (  number  )  ; -/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int number = 123;
    string text = itoa(number);
  )
  IO.println (s.raw.prettyPrint)

-- String to integer
run_meta
  let s ← `(vexSnippet|
    string text = "123";
    int number = atoi(text);
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- STRINGS
-- ============================================================================

-- Print strings
/-- info:  printf  (  "Hello, World!"  )  ; -/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    printf("Hello, World!");
  )
  IO.println (s.raw.prettyPrint)

-- Print string variable
/-- info:  string  text  =  "Hello, World!"  ;  printf  (  "The text is: %s \n"  ,  text  )  ; -/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    string text = "Hello, World!";
    printf("The text is: %s \n", text);
  )
  IO.println (s.raw.prettyPrint)

-- Concatenate strings
/--
info:  string  node  =  "SOP"  ;  string  value  =  "256"  ;  string  output  =  sprintf  (  "%s%s%s"  ,  node  ,  " = "  ,  value  )  ;  printf  (  "%s"  ,  output  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    string node = "SOP";
    string value = "256";
    string output = sprintf("%s%s%s", node, " = ", value);
    printf("%s", output);
  )
  IO.println (s.raw.prettyPrint)

-- Reverse string
/-- info:  string  text  =  "ABCD"  ;  reverse  (  text  )  ; -/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    string text = "ABCD";
    reverse(text);
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- ARRAYS
-- ============================================================================

-- Array operations
/--
info:  int  numbers  [  ]  =  {  5  ,  4  ,  3  ,  2  ,  1  }  ;  printf  (  " %d\n"  ,  numbers  [  0  ]  )  ;  printf  (  " %d\n"  ,  numbers  [  -  1  ]  )  ;  printf  (  " %d\n"  ,  sort  (  numbers  )  )  ;  printf  (  " %d\n"  ,  reverse  (  sort  (  numbers  )  )  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int numbers[] = {5, 4, 3, 2, 1};
    printf(" %d\n", numbers[0]);
    printf(" %d\n", numbers[-1]);
    printf(" %d\n", sort(numbers));
    printf(" %d\n", reverse(sort(numbers)));
  )
  IO.println (s.raw.prettyPrint)

-- Add element to array
/--
info:  int  numbers  [  ]  =  {  1  ,  2  ,  3  ,  4  ,  5  ,  6  }  ;  numbers  [  1  ]  =  7  ;  numbers  [  6  ]  =  7  ;  append  (  numbers  ,  7  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int numbers[] = {1, 2, 3, 4, 5, 6};
    numbers[1] = 7;
    numbers[6] = 7;
    append(numbers, 7);
  )
  IO.println (s.raw.prettyPrint)

-- Find element in array
/--
info:  int  numbers  [  ]  =  {  5  ,  4  ,  3  ,  2  ,  1  }  ;  int  index_of_4  =  find  (  numbers  ,  4  )  ;  printf  (  "%d"  ,  index_of_4  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int numbers[] = {5, 4, 3, 2, 1};
    int index_of_4 = find(numbers, 4);
    printf("%d", index_of_4);
  )
  IO.println (s.raw.prettyPrint)

-- Split string to array
/--
info:  string  numbres  =  "1 2 3 4 5 6"  ;  string  array  [  ]  =  split  (  numbres  ,  " "  )  ;  printf  (  "%s"  ,  array  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    string numbres = "1 2 3 4 5 6";
    string array[] = split(numbres, " ");
    printf("%s", array);
  )
  IO.println (s.raw.prettyPrint)

-- Split integer into array
/--
info:  int  int_number  =  312654  ;  string  string_number  =  itoa  (  int_number  )  ;  int  int_numbers  [  ]  ;  for  (  int  n  =  0  ;  n  <  len  (  string_number  )  ;  n  ++  )  {  int_numbers  [  n  ]  =  atoi  (  string_number  [  n  ]  )  ;  }  printf  (  "%s"  ,  int_numbers  )  ;  printf  (  "%s"  ,  sort  (  int_numbers  )  )  ;  printf  (  "%s"  ,  reverse  (  sort  (  int_numbers  )  )  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int int_number = 312654;
    string string_number = itoa(int_number);
    int int_numbers[];
    
    for (int n = 0; n < len(string_number); n++) {
      int_numbers[n] = atoi(string_number[n]);
    }
    
    printf("%s", int_numbers);
    printf("%s", sort(int_numbers));
    printf("%s", reverse(sort(int_numbers)));
  )
  IO.println (s.raw.prettyPrint)

-- Initialize array with variables
/-- info:  int  num  =  2  ;  int  nums  [  ]  =  array  (  0  ,  1  ,  num  )  ; -/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int num = 2;
    int nums[] = array(0, 1, num);
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- GET AND SET ATTRIBUTE VALUES
-- ============================================================================

-- Get attribute from input
/--
info:  vector  point_pos  =  v@  opinput0_P  ;  vector  point_pos2  =  point  (  0  ,  "P"  ,  @  ptnum  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    vector point_pos = v@opinput0_P;
    vector point_pos2 = point(0, "P", @ptnum);
  )
  IO.println (s.raw.prettyPrint)

-- Get primitive attribute in point mode
/-- info:  primattrib  (  0  ,  "attribute_name"  ,  @  ptnum  ,  0  )  ; -/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    primattrib(0, "attribute_name", @ptnum, 0);
  )
  IO.println (s.raw.prettyPrint)

-- Create and set attributes
/--
info:  addpointattrib  (  0  ,  "Cd"  ,  {  1  ,  0  ,  0  }  )  ;  setpointattrib  (  0  ,  "myattr"  ,  0  ,  1.0  ,  "set"  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    addpointattrib(0, "Cd", {1, 0, 0});
    setpointattrib(0, "myattr", 0, 1.0, "set");
  )
  IO.println (s.raw.prettyPrint)

-- Set attribute values
/--
info:  f@  pi  =  3.1415  ;  v@  vector_a  =  {  1  ,  2  ,  3  }  ;  v@  vector_b  =  set  (  1  ,  2  ,  @  P.z  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    f@pi = 3.1415;
    v@vector_a = {1, 2, 3};
    v@vector_b = set(1, 2, @P.z);
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- MODIFY INPUT VALUES
-- ============================================================================

-- Modify with fit and ramp
/--
info:  float  input  ;  input  =  fit  (  input  ,  0  ,  1  ,  0  ,  10  )  ;  input  =  chramp  (  "Modify_Value"  ,  input  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    float input;
    input = fit(input, 0, 1, 0, 10);
    input = chramp("Modify_Value", input);
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- GET POINTS AND PRIMITIVES
-- ============================================================================

-- Expand groups
/--
info:  int  points  [  ]  =  expandpointgroup  (  0  ,  "!*"  )  ;  int  primitives  [  ]  =  expandprimgroup  (  0  ,  "!*"  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int points[] = expandpointgroup(0, "!*");
    int primitives[] = expandprimgroup(0, "!*");
  )
  IO.println (s.raw.prettyPrint)

-- Get primitive points
/-- info:  int  points  [  ]  =  primpoints  (  0  ,  @  primnum  )  ; -/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int points[] = primpoints(0, @primnum);
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- CREATE GROUPS
-- ============================================================================

-- Create point group
/--
info:  if  (  @  P.x  >  1  )  {  setpointgroup  (  0  ,  "high"  ,  @  ptnum  ,  1  ,  "set"  )  ;  }
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    if (@P.x > 1) {
      setpointgroup(0, "high", @ptnum, 1, "set");
    }
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- LOOPS
-- ============================================================================

-- Create open shape with for loop
/--
info:  int  primitive  =  addprim  (  0  ,  "polyline"  )  ;  int  numberOfPoints  =  @  numpt  ;  for  (  int  n  =  0  ;  n  <  numberOfPoints  ;  n  ++  )  {  addvertex  (  0  ,  primitive  ,  n  )  ;  }
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int primitive = addprim(0, "polyline");
    int numberOfPoints = @numpt;
    
    for (int n = 0; n < numberOfPoints; n++) {
      addvertex(0, primitive, n);
    }
  )
  IO.println (s.raw.prettyPrint)

-- Create closed shape with foreach
/--
info:  int  primitive  =  addprim  (  0  ,  "poly"  )  ;  int  allPoints  [  ]  =  expandpointgroup  (  0  ,  "!*"  )  ;  foreach  (  int  currentPoint  ;  allPoints  )  {  addvertex  (  0  ,  primitive  ,  currentPoint  )  ;  }
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int primitive = addprim(0, "poly");
    int allPoints[] = expandpointgroup(0, "!*");
    
    foreach (int currentPoint; allPoints) {
      addvertex(0, primitive, currentPoint);
    }
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- CONDITIONS
-- ============================================================================

-- Scale first and last points
/--
info:  if  (  (  @  ptnum  ==  0  )  ||  (  @  ptnum  ==  (  @  numpt  -  1  )  )  )  f@  pscale  =  10  ;  else  f@  pscale  =  1  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    if ((@ptnum == 0) || (@ptnum == (@numpt - 1))) 
      f@pscale = 10;
    else 
      f@pscale = 1;
  )
  IO.println (s.raw.prettyPrint)

-- Ternary operator
/--
info:  f@  pscale  =  (  @  ptnum  ==  0  )  ||  @  ptnum  ==  (  @  numpt  -  1  )  ?  10  :  1  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    f@pscale = (@ptnum == 0) || @ptnum == (@numpt - 1) ? 10 : 1;
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- VECTORS
-- ============================================================================

-- Vector between points
/--
info:  vector  vector_A  =  normalize  (  point  (  0  ,  "P"  ,  1  )  )  ;  vector  vector_B  =  normalize  (  point  (  0  ,  "P"  ,  0  )  )  ;  vector  vector_AB  =  vector_A  -  vector_B  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    vector vector_A = normalize(point(0, "P", 1));
    vector vector_B = normalize(point(0, "P", 0));
    vector vector_AB = vector_A - vector_B;
  )
  IO.println (s.raw.prettyPrint)

-- Build tangent normals
/--
info:  vector  vector_A  =  normalize  (  point  (  0  ,  "P"  ,  @  ptnum  )  )  ;  vector  vector_B  =  normalize  (  point  (  0  ,  "P"  ,  @  ptnum  +  1  )  )  ;  @  N  =  vector_A  -  vector_B  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    vector vector_A = normalize(point(0, "P", @ptnum));
    vector vector_B = normalize(point(0, "P", @ptnum + 1));
    @N = vector_A - vector_B;
  )
  IO.println (s.raw.prettyPrint)

-- Angle between vectors
/--
info:  float  angle  =  acos  (  dot  (  normalize  (  vector_A  )  ,  normalize  (  vector_B  )  )  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    float angle = acos(dot(normalize(vector_A), normalize(vector_B)));
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- TRANSFORMATION MATRIX
-- ============================================================================

-- Get and apply matrix
/-- info:  matrix  matrx  =  optransform  (  "obj/geometry_01"  )  ;  @  P  *=  matrx  ; -/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    matrix matrx = optransform("obj/geometry_01");
    @P *= matrx;
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- VEX FUNCTIONS
-- ============================================================================

-- Custom function
/--
info:  vector  [  ]  get_point_positions  (  )  {  vector  points  [  ]  ;  for  (  int  i  =  0  ;  i  <  npoints  (  0  )  ;  i  ++  )  {  vector  point_position  =  point  (  0  ,  "P"  ,  i  )  ;  append  (  points  ,  point_position  )  ;  }  return  points  ;  }  printf  (  "Array = %s\n"  ,  get_point_positions  (  )  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    vector[] get_point_positions() {
      vector points[];
      for (int i = 0; i < npoints(0); i++) {
        vector point_position = point(0, "P", i);
        append(points, point_position);
      }
      return points;
    }
    
    printf("Array = %s\n", get_point_positions());
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- TOOLS - HANGING WIRE
-- ============================================================================

-- Hanging wire with ramp
/--
info:  int  number_of_points  =  chi  (  "number_of_points"  )  ;  vector  anchor_a  =  point  (  0  ,  "P"  ,  0  )  ;  vector  anchor_b  =  point  (  0  ,  "P"  ,  1  )  ;  for  (  int  i  =  1  ;  i  <  number_of_points  +  1  ;  i  ++  )  {  vector  segment_len  =  (  anchor_b  -  anchor_a  )  /  (  number_of_points  +  1  )  ;  vector  position  =  anchor_a  +  i  *  segment_len  ;  float  range  =  fit  (  i  ,  1  ,  number_of_points  ,  0  ,  1  )  ;  position.y  -=  chramp  (  "Shape"  ,  range  )  ;  vector  point_position  =  set  (  position.x  ,  position.y  ,  position.z  )  ;  int  point  =  addpoint  (  0  ,  point_position  )  ;  if  (  i  ==  1  )  addprim  (  0  ,  "polyline"  ,  0  ,  2  )  ;  if  (  i  !=  0  &&  i  !=  number_of_points  )  addprim  (  0  ,  "polyline"  ,  i  +  1  ,  i  +  2  )  ;  if  (  i  ==  number_of_points  )  addprim  (  0  ,  "polyline"  ,  number_of_points  +  1  ,  1  )  ;  }
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int number_of_points = chi("number_of_points");
    vector anchor_a = point(0, "P", 0);
    vector anchor_b = point(0, "P", 1);
    
    for (int i = 1; i < number_of_points + 1; i++) {
      vector segment_len = (anchor_b - anchor_a) / (number_of_points + 1);
      vector position = anchor_a + i * segment_len;
      
      float range = fit(i, 1, number_of_points, 0, 1);
      position.y -= chramp("Shape", range);
      vector point_position = set(position.x, position.y, position.z);
      
      int point = addpoint(0, point_position);
      
      if (i == 1) 
        addprim(0, "polyline", 0, 2);
      if (i != 0 && i != number_of_points) 
        addprim(0, "polyline", i + 1, i + 2);
      if (i == number_of_points) 
        addprim(0, "polyline", number_of_points + 1, 1);
    }
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- FLATTEN MESH BY UVS
-- ============================================================================

/--
info:  v@  rest  =  @  P  ;  @  P  =  vertex  (  0  ,  "uv"  ,  pointvertex  (  0  ,  @  ptnum  )  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    v@rest = @P;
    @P = vertex(0, "uv", pointvertex(0, @ptnum));
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- REMAP RANDOM
-- ============================================================================

/-- info:  float  random  =  rand  (  @  ptnum  )  *  2  -  1  ; -/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    float random = rand(@ptnum) * 2 - 1;
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- BEND CURVES
-- ============================================================================

/--
info:  int  points  [  ]  =  primpoints  (  0  ,  @  primnum  )  ;  matrix3  matrx  =  ident  (  )  ;  float  angle  =  radians  (  chf  (  "angle"  )  )  ;  vector  axis  =  {  1  ,  0  ,  0  }  ;  vector  init_pos  =  point  (  0  ,  "P"  ,  points  [  0  ]  )  ;  vector  prev_pos  =  init_pos  ;  for  (  int  n  =  0  ;  n  <  len  (  points  )  ;  n  ++  )  {  vector  curr_pos  =  point  (  0  ,  "P"  ,  points  [  n  ]  )  ;  rotate  (  matrx  ,  angle  ,  axis  )  ;  vector  new_pos  =  (  curr_pos  -  init_pos  )  *  matrx  +  prev_pos  ;  init_pos  =  curr_pos  ;  prev_pos  =  new_pos  ;  setpointattrib  (  0  ,  "P"  ,  points  [  n  ]  ,  new_pos  )  ;  }
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int points[] = primpoints(0, @primnum);
    
    matrix3 matrx = ident();
    float angle = radians(chf("angle"));
    vector axis = {1, 0, 0};
    
    vector init_pos = point(0, "P", points[0]);
    vector prev_pos = init_pos;
    
    for (int n = 0; n < len(points); n++) {
      vector curr_pos = point(0, "P", points[n]);
      rotate(matrx, angle, axis);
      
      vector new_pos = (curr_pos - init_pos) * matrx + prev_pos;
      init_pos = curr_pos;
      prev_pos = new_pos;
      
      setpointattrib(0, "P", points[n], new_pos);
    }
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- CREATE UVS ON CURVES
-- ============================================================================

/--
info:  f@  uv  =  float  (  vertexprimindex  (  0  ,  @  ptnum  )  )  /  (  @  numvtx  -  1  )  ;  @  Cd  =  chramp  (  "Value"  ,  @  uv  )  ;  if  (  rand  (  @  primnum  )  >  0.9  )  {  @  Cd  =  {  1  ,  0  ,  0  }  ;  }
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    f@uv = float(vertexprimindex(0, @ptnum)) / (@numvtx - 1);
    @Cd = chramp("Value", @uv);
    
    if (rand(@primnum) > 0.9) {
      @Cd = {1, 0, 0};
    }
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- STICK POINTS TO ANIMATED GEOMETRY
-- ============================================================================

/--
info:  int  prim  ;  vector  uv  ;  xyzdist  (  1  ,  @  P  ,  prim  ,  uv  )  ;  @  P  =  primuv  (  2  ,  "P"  ,  prim  ,  uv  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int prim;
    vector uv;
    
    xyzdist(1, @P, prim, uv);
    @P = primuv(2, "P", prim, uv);
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- MOVE TO ORIGIN AND BACK
-- ============================================================================

-- Move to origin
/--
info:  vector  min  =  {  0  ,  0  ,  0  }  ;  vector  max  =  {  0  ,  0  ,  0  }  ;  getpointbbox  (  0  ,  min  ,  max  )  ;  vector  centroid  =  (  max  +  min  )  /  2.0  ;  vector  translate  =  centroid  ;  vector  rotate  =  {  0  ,  0  ,  0  }  ;  vector  scale  =  {  1  ,  1  ,  1  }  ;  matrix  xform  =  invert  (  maketransform  (  0  ,  0  ,  translate  ,  rotate  ,  scale  )  )  ;  @  P  *=  xform  ;  m4@  xform_matrix  =  xform  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    vector min = {0, 0, 0};
    vector max = {0, 0, 0};
    getpointbbox(0, min, max);
    vector centroid = (max + min) / 2.0;
    
    vector translate = centroid;
    vector rotate = {0, 0, 0};
    vector scale = {1, 1, 1};
    matrix xform = invert(maketransform(0, 0, translate, rotate, scale));
    @P *= xform;
    
    m4@xform_matrix = xform;
  )
  IO.println (s.raw.prettyPrint)

-- Return from origin
/-- info:  @  P  *=  invert  (  m4@  xform_matrix  )  ; -/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    @P *= invert(m4@xform_matrix);
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- NOISE
-- ============================================================================

/--
info:  @  Cd  =  {  1  ,  1  ,  1  }  ;  float  noseValues  =  noise  (  @  P  *  (  1  /  chf  (  "Noise_Size"  )  )  +  chf  (  "Noise_Offset"  )  )  ;  if  (  noseValues  >  chf  (  "Noise_Threshold"  )  )  {  @  Cd  =  0  ;  if  (  rand  (  @  ptnum  )  <  ch  (  "delete_black"  )  )  {  if  (  chi  (  "del"  )  ==  0  )  {  @  Cd  =  {  1  ,  0  ,  0  }  ;  }  else  {  removepoint  (  0  ,  @  ptnum  )  ;  }  }  }  if  (  noseValues  <  chf  (  "Noise_Threshold"  )  )  {  if  (  rand  (  @  ptnum  )  <  ch  (  "delete_white"  )  )  {  if  (  chi  (  "del"  )  ==  0  )  {  @  Cd  =  {  1  ,  0  ,  0  }  ;  }  else  {  removepoint  (  0  ,  @  ptnum  )  ;  }  }  }
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    @Cd = {1, 1, 1};
    
    float noseValues = noise(@P * (1 / chf("Noise_Size")) + chf("Noise_Offset"));
    
    if (noseValues > chf("Noise_Threshold")) {
      @Cd = 0;
      if (rand(@ptnum) < ch("delete_black")) {
        if (chi("del") == 0) {
          @Cd = {1, 0, 0};
        } else {
          removepoint(0, @ptnum);
        }
      }
    }
    
    if (noseValues < chf("Noise_Threshold")) {
      if (rand(@ptnum) < ch("delete_white")) {
        if (chi("del") == 0) {
          @Cd = {1, 0, 0};
        } else {
          removepoint(0, @ptnum);
        }
      }
    }
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- FLATTEN SURFACE BOTTOM
-- ============================================================================

/--
info:  float  min  =  ch  (  "flatten_disrtance"  )  +  getbbox_min  (  0  )  .  y  ;  float  max  =  getbbox_max  (  0  )  .  y  ;  float  Y  =  clamp  (  @  P.y  ,  min  ,  max  )  ;  @  P  =  set  (  @  P.x  ,  Y  ,  @  P.z  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    float min = ch("flatten_disrtance") + getbbox_min(0).y;
    float max = getbbox_max(0).y;
    float Y = clamp(@P.y, min, max);
    
    @P = set(@P.x, Y, @P.z);
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- MULTIPLY DISTRIBUTION
-- ============================================================================

/-- info:  value  =  pow  (  value  ,  8.0  )  ; -/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    value = pow(value, 8.0);
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- NOISE THE POINTS
-- ============================================================================

/--
info:  float  noise  =  chf  (  "Noise_Power"  )  ;  float  freq  =  chf  (  "Noise_Frequency"  )  ;  vector  noiseXYZ  =  noise  (  @  P  *  freq  )  ;  v@  ns  =  fit  (  noiseXYZ  ,  0  ,  1  ,  -  1  ,  1  )  *  noise  ;  @  P.x  +=  @  ns.x  ;  @  P.z  +=  @  ns.z  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    float noise = chf("Noise_Power");
    float freq = chf("Noise_Frequency");
    vector noiseXYZ = noise(@P * freq);
    v@ns = fit(noiseXYZ, 0, 1, -1, 1) * noise;
    @P.x += @ns.x;
    @P.z += @ns.z;
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- SELECT MESH BORDER POINTS
-- ============================================================================

/--
info:  int  nbPts  =  neighbourcount  (  0  ,  @  ptnum  )  ;  i@  group_border  =  nbPts  ==  3  |  nbPts  ==  2  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int nbPts = neighbourcount(0, @ptnum);
    i@group_border = nbPts == 3 | nbPts == 2;
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- SHAPE POLYWIRE WITH RAMP
-- ============================================================================

-- run_meta
--   let s ← `(vexSnippet|
--     i[]@primPts = primpoints(0, @primnum);
    
--     foreach (int i; int currentPoint; @primPts) {
--       float ramp_index = fit(i, 0, len(@primPts) - 1, 0, 1);
--       f@widthPrim = chramp("shape", ramp_index) / 20;
--       setpointattrib(0, "width", currentPoint, @widthPrim, "set");
--     }
--   )
--   IO.println (s.raw.prettyPrint)

-- ============================================================================
-- VEX STRINGS
-- ============================================================================

/--
info:  int  version  =  1  ;  string  fileName  =  sprintf  (  "fileName_%02d.abc"  ,  version  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int version = 1;
    string fileName = sprintf("fileName_%02d.abc", version);
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- FIND CLOSEST POINTS
-- ============================================================================

/--
info:  float  maxdist  =  0.8  ;  int  maxpoints  =  10  ;  int  closept  [  ]  =  pcfind  (  0  ,  "P"  ,  @  P  ,  maxdist  ,  maxpoints  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    float maxdist = 0.8;
    int maxpoints = 10;
    
    int closept[] = pcfind(0, "P", @P, maxdist, maxpoints);
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- RANDOMIZE FILE NAME
-- ============================================================================

/--
info:  string  variations  [  ]  =  {  "A"  ,  "B"  ,  "C"  }  ;  int  variationIndex  =  rint  (  fit  (  rand  (  @  ptnum  )  ,  0  ,  1  ,  0  ,  2  )  )  ;  string  path  =  sprintf  (  "D:/PROJECTS/VEX/geo/sim_%s_01.abc"  ,  variations  [  variationIndex  ]  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    string variations[] = {"A", "B", "C"};
    int variationIndex = rint(fit(rand(@ptnum), 0, 1, 0, 2));
    string path = sprintf("D:/PROJECTS/VEX/geo/sim_%s_01.abc", variations[variationIndex]);
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- FADE GRID Y DEFORMATION
-- ============================================================================

/--
info:  float  objectSize  =  (  getbbox_max  (  0  )  .  x  +  getbbox_max  (  0  )  .  z  )  /  2  ;  float  dist  =  distance  (  0  ,  @  P  )  ;  float  offset  =  chf  (  "offset"  )  ;  float  fade  =  chramp  (  "fade"  ,  fit  (  dist  ,  0  ,  objectSize  +  offset  ,  0  ,  1  )  )  ;  @  P.y  *=  fade  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    float objectSize = (getbbox_max(0).x + getbbox_max(0).z) / 2;
    float dist = distance(0, @P);
    float offset = chf("offset");
    float fade = chramp("fade", fit(dist, 0, objectSize + offset, 0, 1));
    @P.y *= fade;
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- FADE NOISE ON CURVES WITH RAMP
-- ============================================================================

/--
info:  float  remap_uv  =  chramp  (  "remap_uv"  ,  @  uv.x  )  ;  float  power  =  chf  (  "Noise_Power"  )  ;  float  freq  =  chf  (  "Noise_Frequency"  )  ;  vector  noiseXYZ  =  noise  (  @  P  *  freq  )  ;  vector  displace  =  fit  (  noiseXYZ  ,  0  ,  1  ,  -  1  ,  1  )  *  power  *  remap_uv  ;  @  P  +=  displace  ;  @  Cd  =  remap_uv  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    float remap_uv = chramp("remap_uv", @uv.x);
    float power = chf("Noise_Power");
    float freq = chf("Noise_Frequency");
    
    vector noiseXYZ = noise(@P * freq);
    vector displace = fit(noiseXYZ, 0, 1, -1, 1) * power * remap_uv;
    @P += displace;
    @Cd = remap_uv;
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- ROTATE GEO WITH MATRIX
-- ============================================================================

/--
info:  matrix3  matrx  =  ident  (  )  ;  float  angle  =  radians  (  chf  (  "angle"  )  )  ;  vector  axis  =  {  0  ,  1  ,  0  }  ;  rotate  (  matrx  ,  angle  ,  axis  )  ;  @  P  *=  matrx  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    matrix3 matrx = ident();
    float angle = radians(chf("angle"));
    vector axis = {0, 1, 0};
    
    rotate(matrx, angle, axis);
    @P *= matrx;
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- ADJUST PIVOT POINT
-- ============================================================================

/--
info:  matrix3  matrx  =  ident  (  )  ;  float  angle  =  radians  (  36  )  ;  vector  axis  =  {  1  ,  0  ,  0  }  ;  vector  pivot  =  {  0  ,  2.56  ,  0  }  ;  rotate  (  matrx  ,  angle  ,  axis  )  ;  @  P  =  (  @  P  -  pivot  )  *  matrx  +  pivot  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    matrix3 matrx = ident();
    float angle = radians(36);
    vector axis = {1, 0, 0};
    vector pivot = {0, 2.56, 0};
    
    rotate(matrx, angle, axis);
    @P = (@P - pivot) * matrx + pivot;
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- ROTATE WITH QUATERNION
-- ============================================================================

/--
info:  float  angle_X  =  radians  (  chf  (  "angle_X"  )  )  ;  float  angle_Y  =  radians  (  chf  (  "angle_Y"  )  )  ;  float  angle_Z  =  radians  (  chf  (  "angle_Z"  )  )  ;  vector  rotations  =  set  (  angle_X  ,  angle_Y  ,  angle_Z  )  ;  @  P  =  qrotate  (  quaternion  (  rotations  )  ,  @  P  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    float angle_X = radians(chf("angle_X"));
    float angle_Y = radians(chf("angle_Y"));
    float angle_Z = radians(chf("angle_Z"));
    
    vector rotations = set(angle_X, angle_Y, angle_Z);
    @P = qrotate(quaternion(rotations), @P);
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- ROTATE Y COPIES WITH QUATERNION MULTIPLY
-- ============================================================================

/--
info:  @  N  ;  @  up  =  {  0  ,  1  ,  0  }  ;  @  orient  =  quaternion  (  maketransform  (  @  N  ,  @  up  )  )  ;  vector4  rotate_Y  =  quaternion  (  radians  (  ch  (  "Rotate_Y"  )  )  ,  {  0  ,  1  ,  0  }  )  ;  @  orient  =  qmultiply  (  @  orient  ,  rotate_Y  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    @N;
    @up = {0, 1, 0};
    
    @orient = quaternion(maketransform(@N, @up));
    vector4 rotate_Y = quaternion(radians(ch("Rotate_Y")), {0, 1, 0});
    @orient = qmultiply(@orient, rotate_Y);
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- RANDOMIZE COPIES
-- ============================================================================

/--
info:  @  N  ;  @  up  =  {  0  ,  1  ,  0  }  ;  float  randPos_X  =  fit01  (  rand  (  @  ptnum  )  ,  -  ch  (  "Translate_X"  )  ,  ch  (  "Translate_X"  )  )  ;  float  randPos_Y  =  fit01  (  rand  (  @  ptnum  )  ,  -  ch  (  "Translate_Y"  )  ,  ch  (  "Translate_Y"  )  )  ;  float  randPos_Z  =  fit01  (  rand  (  @  ptnum  )  ,  -  ch  (  "Translate_Z"  )  ,  ch  (  "Translate_Z"  )  )  ;  vector  randPos  =  set  (  randPos_X  ,  randPos_Y  ,  randPos_Z  )  ;  float  randRot_X  =  fit01  (  rand  (  @  ptnum  )  ,  -  ch  (  "Rotate_X"  )  ,  ch  (  "Rotate_X"  )  )  ;  float  randRot_Y  =  fit01  (  rand  (  @  ptnum  )  ,  -  ch  (  "Rotate_Y"  )  ,  ch  (  "Rotate_Y"  )  )  ;  float  randRot_Z  =  fit01  (  rand  (  @  ptnum  )  ,  -  ch  (  "Rotate_Z"  )  ,  ch  (  "Rotate_Z"  )  )  ;  @  P  +=  randPos  ;  @  orient  =  quaternion  (  maketransform  (  @  N  ,  @  up  )  )  ;  vector4  rotate_X  =  quaternion  (  radians  (  randRot_X  )  ,  {  1  ,  0  ,  0  }  )  ;  vector4  rotate_Y  =  quaternion  (  radians  (  randRot_Y  )  ,  {  0  ,  1  ,  0  }  )  ;  vector4  rotate_Z  =  quaternion  (  radians  (  randRot_Z  )  ,  {  0  ,  0  ,  1  }  )  ;  @  orient  =  qmultiply  (  @  orient  ,  rotate_X  )  ;  @  orient  =  qmultiply  (  @  orient  ,  rotate_Y  )  ;  @  orient  =  qmultiply  (  @  orient  ,  rotate_Z  )  ;  @  scale  =  fit01  (  rand  (  @  ptnum  )  ,  chf  (  "Scale_MIN"  )  ,  chf  (  "Scale_MAX"  )  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    @N;
    @up = {0, 1, 0};
    
    float randPos_X = fit01(rand(@ptnum), -ch("Translate_X"), ch("Translate_X"));
    float randPos_Y = fit01(rand(@ptnum), -ch("Translate_Y"), ch("Translate_Y"));
    float randPos_Z = fit01(rand(@ptnum), -ch("Translate_Z"), ch("Translate_Z"));
    vector randPos = set(randPos_X, randPos_Y, randPos_Z);
    
    float randRot_X = fit01(rand(@ptnum), -ch("Rotate_X"), ch("Rotate_X"));
    float randRot_Y = fit01(rand(@ptnum), -ch("Rotate_Y"), ch("Rotate_Y"));
    float randRot_Z = fit01(rand(@ptnum), -ch("Rotate_Z"), ch("Rotate_Z"));
    
    @P += randPos;
    
    @orient = quaternion(maketransform(@N, @up));
    vector4 rotate_X = quaternion(radians(randRot_X), {1, 0, 0});
    vector4 rotate_Y = quaternion(radians(randRot_Y), {0, 1, 0});
    vector4 rotate_Z = quaternion(radians(randRot_Z), {0, 0, 1});
    @orient = qmultiply(@orient, rotate_X);
    @orient = qmultiply(@orient, rotate_Y);
    @orient = qmultiply(@orient, rotate_Z);
    
    @scale = fit01(rand(@ptnum), chf("Scale_MIN"), chf("Scale_MAX"));
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- SPIRAL
-- ============================================================================

/--
info:  float  angle  ;  vector  pos  =  {  0  ,  0  ,  0  }  ;  int  npoints  =  chi  (  "number_of_points"  )  ;  float  step  =  radians  (  ch  (  "sweep"  )  )  /  npoints  ;  for  (  int  n  =  0  ;  n  <  npoints  ;  n  ++  )  {  angle  =  step  *  n  ;  pos.x  =  cos  (  angle  )  ;  pos.y  =  angle  /  10  ;  pos.z  =  sin  (  angle  )  ;  addpoint  (  0  ,  pos  )  ;  }
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    float angle;
    vector pos = {0, 0, 0};
    int npoints = chi("number_of_points");
    float step = radians(ch("sweep")) / npoints;
    
    for (int n = 0; n < npoints; n++) {
      angle = step * n;
      
      pos.x = cos(angle);
      pos.y = angle / 10;
      pos.z = sin(angle);
      
      addpoint(0, pos);
    }
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- SPIRAL GROW
-- ============================================================================

/--
info:  float  angle  ;  vector  pos  =  {  0  ,  0  ,  0  }  ;  int  npoints  =  chi  (  "number_of_points"  )  ;  float  step  =  radians  (  ch  (  "sweep"  )  )  /  npoints  ;  for  (  int  n  =  0  ;  n  <  npoints  ;  n  ++  )  {  angle  =  step  *  n  ;  pos.x  =  sin  (  angle  )  *  angle  ;  pos.y  =  angle  ;  pos.z  =  cos  (  angle  )  *  angle  ;  addpoint  (  0  ,  pos  )  ;  }
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    float angle;
    vector pos = {0, 0, 0};
    int npoints = chi("number_of_points");
    float step = radians(ch("sweep")) / npoints;
    
    for (int n = 0; n < npoints; n++) {
      angle = step * n;
      
      pos.x = sin(angle) * angle;
      pos.y = angle;
      pos.z = cos(angle) * angle;
      
      addpoint(0, pos);
    }
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- PHYLOTAXIS
-- ============================================================================

/--
info:  int  count  =  400  ;  float  bound  =  10.0  ;  float  tau  =  6.28318530  ;  float  phi  =  (  1  +  sqrt  (  5  )  )  /  2  ;  float  golden_angle  =  (  2  -  phi  )  *  tau  ;  vector  pos  =  {  0  ,  0  ,  0  }  ;  float  radius  =  1.0  ;  float  theta  =  0  ;  int  pt  ;  vector  polar_to_cartesian  (  float  theta  ;  float  radius  )  {  return  set  (  cos  (  theta  )  *  radius  ,  0  ,  sin  (  theta  )  *  radius  )  ;  }  for  (  int  n  =  0  ;  n  <  count  ;  n  ++  )  {  radius  =  bound  *  pow  (  float  (  n  )  /  float  (  count  )  ,  ch  (  "power"  )  )  ;  theta  +=  golden_angle  ;  pos  =  polar_to_cartesian  (  theta  ,  radius  )  ;  pt  =  addpoint  (  0  ,  pos  )  ;  setpointattrib  (  0  ,  "pscale"  ,  pt  ,  pow  (  radius  ,  0.5  )  )  ;  setpointattrib  (  0  ,  "N"  ,  pt  ,  normalize  (  -  pos  )  )  ;  setpointattrib  (  0  ,  "up"  ,  pt  ,  set  (  0  ,  1  ,  0  )  )  ;  }
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int count = 400;
    float bound = 10.0;
    float tau = 6.28318530;
    float phi = (1 + sqrt(5)) / 2;
    float golden_angle = (2 - phi) * tau;
    vector pos = {0, 0, 0};
    float radius = 1.0;
    float theta = 0;
    int pt;
    
    vector polar_to_cartesian(float theta; float radius) {
      return set(cos(theta) * radius, 0, sin(theta) * radius);
    }
    
    for (int n = 0; n < count; n++) {
      radius = bound * pow(float(n) / float(count), ch("power"));
      theta += golden_angle;
      
      pos = polar_to_cartesian(theta, radius);
      
      pt = addpoint(0, pos);
      setpointattrib(0, "pscale", pt, pow(radius, 0.5));
      setpointattrib(0, "N", pt, normalize(-pos));
      setpointattrib(0, "up", pt, set(0, 1, 0));
    }
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- CREATE GEOMETRY FROM POINTS ARRAY
-- ============================================================================

/--
info:  float  searchRadius  =  ch  (  "searchRadius"  )  ;  int  nearpnts  [  ]  =  nearpoints  (  0  ,  @  P  ,  searchRadius  )  ;  foreach  (  int  pnt  ;  nearpnts  )  {  if  (  pnt  !=  @  ptnum  )  {  int  line  =  addprim  (  0  ,  "polyline"  )  ;  addvertex  (  0  ,  line  ,  @  ptnum  )  ;  addvertex  (  0  ,  line  ,  pnt  )  ;  }  }
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    float searchRadius = ch("searchRadius");
    int nearpnts[] = nearpoints(0, @P, searchRadius);
    foreach (int pnt; nearpnts) {
      if (pnt != @ptnum) {
        int line = addprim(0, "polyline");
        addvertex(0, line, @ptnum);
        addvertex(0, line, pnt);
      }
    }
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- ALGORITHMS - SWAP VARIABLES
-- ============================================================================

/--
info:  int  varable_a  =  256  ;  int  varable_b  =  512  ;  int  swap  =  variable_a  ;  variable_a  =  variable_b  ;  variable_b  =  swap  ;  printf  (  "variable_a = %s, variable_b = %s"  ,  variable_a  ,  variable_b  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int varable_a = 256;
    int varable_b = 512;
    
    int swap = variable_a;
    variable_a = variable_b;
    variable_b = swap;
    
    printf("variable_a = %s, variable_b = %s", variable_a, variable_b);
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- REVERSE ARRAY
-- ============================================================================

/--
info:  int  int_numbers  [  ]  =  {  1  ,  2  ,  3  ,  4  ,  5  ,  6  }  ;  int  rversed  [  ]  ;  for  (  int  i  =  0  ;  i  <  len  (  int_numbers  )  /  2  ;  i  ++  )  {  int  number_from_start  =  int_numbers  [  i  ]  ;  int  index_from_end  =  len  (  int_numbers  )  -  i  -  1  ;  rversed  [  i  ]  =  int_numbers  [  index_from_end  ]  ;  rversed  [  index_from_end  ]  =  number_from_start  ;  }  printf  (  "%s"  ,  rversed  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int int_numbers[] = {1, 2, 3, 4, 5, 6};
    int rversed[];
    
    for (int i = 0; i < len(int_numbers) / 2; i++) {
      int number_from_start = int_numbers[i];
      int index_from_end = len(int_numbers) - i - 1;
      
      rversed[i] = int_numbers[index_from_end];
      rversed[index_from_end] = number_from_start;
    }
    
    printf("%s", rversed);
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- CHOICE SORT
-- ============================================================================

/--
info:  int  numbers  [  ]  =  array  (  0  ,  4  ,  3  ,  2  ,  1  )  ;  for  (  int  i  =  0  ;  i  <  len  (  numbers  )  -  1  ;  i  ++  )  {  for  (  int  n  =  i  +  1  ;  n  <  len  (  numbers  )  ;  n  ++  )  {  if  (  numbers  [  i  ]  >  numbers  [  n  ]  )  {  int  swap  =  numbers  [  i  ]  ;  numbers  [  i  ]  =  numbers  [  n  ]  ;  numbers  [  n  ]  =  swap  ;  }  }  }  printf  (  "Array = %s\n"  ,  numbers  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int numbers[] = array(0, 4, 3, 2, 1);
    
    for (int i = 0; i < len(numbers) - 1; i++) {
      for (int n = i + 1; n < len(numbers); n++) {
        if (numbers[i] > numbers[n]) {
          int swap = numbers[i];
          numbers[i] = numbers[n];
          numbers[n] = swap;
        }
      }
    }
    
    printf("Array = %s\n", numbers);
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- BUBBLE SORT
-- ============================================================================

/--
info:  int  numbers  [  ]  =  array  (  0  ,  4  ,  3  ,  2  ,  1  )  ;  for  (  int  i  =  1  ;  i  <  len  (  numbers  )  ;  i  ++  )  {  for  (  int  n  =  0  ;  n  <  len  (  numbers  )  -  i  ;  n  ++  )  {  if  (  numbers  [  n  ]  >  numbers  [  n  +  1  ]  )  {  int  swap  =  numbers  [  n  ]  ;  numbers  [  n  ]  =  numbers  [  n  +  1  ]  ;  numbers  [  n  +  1  ]  =  swap  ;  }  }  }  printf  (  "Array = %s\n"  ,  numbers  )  ;
-/
#guard_msgs in
run_meta
  let s ← `(vexSnippet|
    int numbers[] = array(0, 4, 3, 2, 1);
    
    for (int i = 1; i < len(numbers); i++) {
      for (int n = 0; n < len(numbers) - i; n++) {
        if (numbers[n] > numbers[n + 1]) {
          int swap = numbers[n];
          numbers[n] = numbers[n + 1];
          numbers[n + 1] = swap;
        }
      }
    }
    
    printf("Array = %s\n", numbers);
  )
  IO.println (s.raw.prettyPrint)

-- ============================================================================
-- LONGEST COMMON PREFIX
-- ============================================================================

-- run_meta
--   let s ← `(vexSnippet|
--     string names[] = array("floor", "flower", "flight");
--     string prefix;
    
--     for (int i = 1; i < len(names); i++) {
--       for (int n = 0; n < len(names[i]); n++) {
--         if (names[i][n] != names[0][n]) {
--           break;
--         }
        
--         prefix = names[i][0:n + 1];
--       }
--     }
    
--     printf("The common prefix is: %s\n", prefix);
--   )
--   IO.println (s.raw.prettyPrint)

end VEX.SnippetTests
