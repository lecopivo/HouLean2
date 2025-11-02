import HouLean
import HouLean.Vex.VexFunction
-- import HouLean.Vex.NoiseAndRandomness

open HouLean Apex


-- Color From Bounding Box

/-- info: "@Cd = realbox(0, @P);" : String -/
#guard_msgs in
#check vexsnippet%
  @Cd = realbox(0, @P);

@[apex]
def ColorFromBoundingBox (geo : Geometry) : Geometry := Id.run do
  let mut geo := geo  
  let (_,size,min,_,_) := geo.boundingBox
  for i in [0:geo.numPoints.toNat] do
    let P : Vector3 := geo.pointAttrib (Int.ofNat i) "P"
    let relP := (P - min).compDiv size
    geo := geo.setPointAttrib (Int.ofNat i) "Cd" relP
  return geo


-- Random Point Color

/-- info: "float seed = 0.12345;\n  @Cd = rand(seed + @ptnum);" : String -/
#guard_msgs in
#check vexsnippet%
  float seed = 0.12345;
  @Cd = rand(seed + @ptnum);

-- todo: move this
instance : Zero Vector3 := ⟨⟨0,0,0⟩⟩

-- todo: implement this! / inplement NoiseAndRandomness
vexfunction vector rand(float seed) { return 0; }

@[apex]
def RandomPointColor (geo: Geometry) : Geometry := Id.run do
  let mut geo := geo
  for ptnum in [0:geo.numPoints.toNat] do
    let seed := 0.12345
    let color : Vector3 := rand(seed + ptnum.toFloat)
    geo := geo.setPointAttrib (Int.ofNat ptnum) "Cd" color
  return geo
  

-- Point Group Based on Threashold

/--
info: "string group = \"mygroup\";        \n  int condition = (@P.x > 0) ? 1: 0; \n  setpointgroup(geoself(), group, @ptnum, condition);\n  @Cd = set( condition, 0, 0);" : String
-/
#guard_msgs in
#check vexsnippet%
  string group = "mygroup";        
  int condition = (@P.x > 0) ? 1: 0; 
  setpointgroup(geoself(), group, @ptnum, condition);
  @Cd = set( condition, 0, 0);      

@[apex]
def PointGroupBasedOnThreashold (geo : Geometry) : Geometry := Id.run do
  let mut geo := geo
  let group := "mygroup"
  for ptnum in [0:geo.numPoints.toNat] do
    let P : Vector3 := geo.pointAttrib (Int.ofNat ptnum) "P"
    let condition := if P.x > 0 then (1:Int) else (0:Int)
    geo := geo.setPointAttrib (Int.ofNat ptnum) group condition
  return geo
      


-- Fetch Second Input Cd Attrib

/-- info: "@Cd = point(1, \"Cd\", @ptnum);" : String -/
#guard_msgs in
#check vexsnippet%
  @Cd = point(1, "Cd", @ptnum);

@[apex]
def FetchSecondInputCdAttrib (geo geo1 : Geometry) : Geometry := Id.run do
  let mut geo := geo
  for ptnum in [0:geo.numPoints.toNat] do
    let Cd : Vector3 := geo1.pointAttrib (Int.ofNat ptnum) "Cd"
    geo := geo.setPointAttrib (Int.ofNat ptnum) "Cd" Cd
  return geo



-- Fetch Second Input Attrib by Id

/--
info: "int match_pt = findattribval(1, \"point\", \"id\", @id);\n  @match_pt = match_pt;\n\n  @Cd = point(1, \"Cd\", match_pt);" : String
-/
#guard_msgs in
#check vexsnippet%
  int match_pt = findattribval(1, "point", "id", @id);
  @match_pt = match_pt;

  @Cd = point(1, "Cd", match_pt);


@[apex]
def FetchSecondInputAttribById (geo geo1 : Geometry) : Geometry := Id.run do
  let mut geo := geo

  for ptnum in [0:geo.numPoints.toNat] do
    let ptnum : Int := Int.ofNat ptnum
    let id : Int := geo.pointAttrib ptnum "id"
    let match_pts : Array Int := fromApex (geo1.findPointAttrib "id" id)
    let match_pt := match_pts[0]!

    geo := geo.setPointAttrib ptnum "match_pt" match_pt
    let Cd : Vector3 := geo1.pointAttrib ptnum "Cd"
    geo := geo.setPointAttrib ptnum "Cd" Cd
    pure ()

  return geo


-- Nearest Point Distance

/--
info: "int closept = nearpoint(1, @P);\n  vector value = point(1, \"P\", closept);\n  @dist = length(@P - value);\n  @Cd = set(@dist, 0, 0);" : String
-/
#guard_msgs in
#check vexsnippet%
  int closept = nearpoint(1, @P);
  vector value = point(1, "P", closept);
  @dist = length(@P - value);
  @Cd = set(@dist, 0, 0);


@[apex]
def NearestPointDistance (geo geo1 : Geometry) : Geometry := Id.run do
  
  let mut geo := geo
  for ptnum in [0:geo.numPoints.toNat] do
    let ptnum : Int := Int.ofNat ptnum
    
    let P : Vector3 := geo.pointAttrib ptnum "P"
    let closept : Int := geo.pointAttrib ptnum "closept" -- missing nearpoint thus needs to be precomputed
    let value : Vector3 := geo1.pointAttrib closept "P"
  
    let dist := (P - value).length
    geo := geo.setPointAttrib ptnum "dist" dist
    geo := geo.setPointAttrib ptnum "Cd" (⟨dist,0,0⟩ : Vector3)

  return geo



-- Grow Hair

/--
info: "vector dir = { 0, 1, 0 };\n  float len = 1.0;\n  int   steps = 10;\n  float jitter = 0.1;\n  float seed = 0.12345;\n\n  vector  pos = @P;\n  int     pr = addprim(geoself(), \"polyline\");\n\n  addvertex(geoself(), pr, @ptnum);\n  for (int i = 0; i < steps; i++)\n  {\n    pos += dir * len / steps;\n    pos += (vector(rand( @ptnum + seed )) - 0.5) * jitter;\n\n    int pt = addpoint(geoself(), @ptnum);\n    setpointattrib(geoself(), \"P\", pt, pos);\n\n    addvertex(geoself(), pr, pt);\n    seed += $PI;\n  }" : String
-/
#guard_msgs in
#check vexsnippet%
  vector dir = { 0, 1, 0 };
  float len = 1.0;
  int   steps = 10;
  float jitter = 0.1;
  float seed = 0.12345;

  vector  pos = @P;
  int     pr = addprim(geoself(), "polyline");

  addvertex(geoself(), pr, @ptnum);
  for (int i = 0; i < steps; i++)
  {
    pos += dir * len / steps;
    pos += (vector(rand( @ptnum + seed )) - 0.5) * jitter;

    int pt = addpoint(geoself(), @ptnum);
    setpointattrib(geoself(), "P", pt, pos);

    addvertex(geoself(), pr, pt);
    seed += $PI;
  }


def GrowHair (geo : Geometry) : Geometry := Id.run do

  let dir : Vector3 := ⟨0,1,0⟩
  let len : Float := 1.0
  let steps : Int := 10
  let jitter := 0.1
  let mut seed := 0.12345
  let pi := 3.14159

  let mut geo := geo
  
  for ptnum in [0:geo.numPoints.toNat] do
    -- VEX buffers for new points and primitives
    let mut newprimtypes : Array String := #[] -- primitive types
    let mut newpoints : Array Int := #[] -- ptnum of an existing point to copy data from
    let mut newpointsP : Array Vector3 := #[] -- ptnum of an existing point to copy data from
    let mut newvertices : Array (Int × Int) := #[] -- list of (prim, point)
    let mut newprimid : Int := geo.numPrims
    let mut newpointid : Int := geo.numPoints
    let mut newvertexid : Int := 0 -- no @vtxnum ...
    
    let ptnum : Int := Int.ofNat ptnum
    
    let mut pos : Vector3 := geo.pointAttrib ptnum "P"

    -- addprim(g@geo, "polyline")
    let pr : Int := newprimid
    newprimid := newprimid + 1
    newprimtypes := newprimtypes.push "polyline"
    
    newvertices := newvertices.push (pr, ptnum)
    newvertexid := newvertexid + 1
    for j in [0:steps.toNat] do
      pos := pos + (len / steps.toInt64.toFloat) * dir
      pos := pos + jitter * ((rand(ptnum.toInt64.toFloat + seed) : Vector3) - ⟨0.5,0.5,0.5⟩)

      -- int pt = addpoint(geoself(), @ptnum);
      let pt : Int := newpointid
      newpointid := newpointid + 1
      newpoints := newpoints.push ptnum
      -- setpointattrib(geoself(), "P", pt, pos);
      newpointsP := newpointsP.push pos
      
      -- addvertex(geoself(), pr, pt);
      newvertices := newvertices.push (pr, pt)
      newvertexid := newvertexid + 1

      seed := seed + pi

    geo := geo.setPointAttrib ptnum "__apex_new_prims" (toApex newprimtypes)
    geo := geo.setPointAttrib ptnum "__apex_new_points" (toApex newpoints)
    -- geo := geo.setPointAttrib ptnum "__apex_new_points_P" (toApex newpointsP)
    -- geo := geo.setPointAttrib ptnum "__apex_new_vertices" (toApex newvertices)

  -- geo := geo.flushNewGeometry

  return geo


--  Get Neighbouring Points into Attribute

/-- info: "i[]@neighbours = neighbours(0, @ptnum);" : String -/
#guard_msgs in
#check vexsnippet%
  i[]@neighbours = neighbours(0, @ptnum);

-- todo: probably add new APEX node or something
opaque GetNeighbouringPointsIntoAttribute (geo : Geometry) : Geometry



--  Average Neighbouring Points

/--
info: "int n[] = neighbours(0, @ptnum);\n  vector avgP = @P;\n\n  foreach (int pt; n)\n  {\n      avgP += point(0, \"P\", pt);\n  }\n\n  avgP /= len(n)+1;\n  @P = avgP;" : String
-/
#guard_msgs in
#check vexsnippet%
  int n[] = neighbours(0, @ptnum);
  vector avgP = @P;

  foreach (int pt; n)
  {
      avgP += point(0, "P", pt);
  }

  avgP /= len(n)+1;
  @P = avgP;


-- this has the issue that we are modifying point position as we are modifying them
def AverageNeighbouringPoints (geo : Geometry) : Geometry := Id.run do
  let oldGeo := geo
  let mut geo := geo
  
  for ptnum in [0:geo.numPoints.toNat] do
    let ptnum : Int := Int.ofNat ptnum
    let P : Vector3 := oldGeo.pointAttrib ptnum "P"
    let n : Array Int := fromApex (oldGeo.pointAttrib ptnum "neighbours")
    let mut avgP := P

    for pt in n do
      let Q : Vector3 := oldGeo.pointAttrib pt "P"
      avgP := avgP + Q
    
    avgP := avgP / (n.size + 1).toFloat
    geo := geo.setPointAttrib ptnum "P" avgP

  return geo


-- Create Line Between Points

/--
info: "int createLine(int pt_a; int pt_b)\n  {\n      int prim = addprim(0, \"polyline\");\n      addvertex(0, prim, pt_a);\n      addvertex(0, prim, pt_b);\n      return prim;\n  }\n\n  createLine(0, @ptnum);" : String
-/
#guard_msgs in
#check vexsnippet%
  int createLine(int pt_a; int pt_b)
  {
      int prim = addprim(0, "polyline");
      addvertex(0, prim, pt_a);
      addvertex(0, prim, pt_b);
      return prim;
  }

  createLine(0, @ptnum);



-- Remove First Last Point On Line

/--
info: "int vtx_count = primvertexcount(0, @primnum) - 1;\n\n  int ptnum = primvertex(0, @primnum, vtx_count);\n  removepoint(0, ptnum);" : String
-/
#guard_msgs in
#check vexsnippet%
  int vtx_count = primvertexcount(0, @primnum) - 1;

  int ptnum = primvertex(0, @primnum, vtx_count);
  removepoint(0, ptnum);

