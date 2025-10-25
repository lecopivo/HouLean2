import HouLean.Apex.Basic
import HouLean.Apex.Array
import HouLean.Apex.Dict

namespace HouLean.Apex.SOP

open Generated

-- ============================================================================
-- Common Parameter Structures
-- ============================================================================

/-- Transform parameters for geometric operations -/
structure Transform where
  /-- Translation vector -/
  translate : Vector3 := ⟨0, 0, 0⟩
  /-- Rotation in degrees (Euler angles) -/
  rotate : Vector3 := ⟨0, 0, 0⟩
  /-- Scale factors per axis -/
  scale : Vector3 := ⟨1, 1, 1⟩
  /-- Shear values -/
  shear : Vector3 := ⟨0, 0, 0⟩
  /-- Pivot point for rotation and scaling -/
  pivot : Vector3 := ⟨0, 0, 0⟩
  /-- Pivot rotation applied before main rotation -/
  pivotRotate : Vector3 := ⟨0, 0, 0⟩
  /-- Transform order: 0=SRT, 1=STR, 2=RST, 3=RTS, 4=TSR, 5=TRS -/
  xOrd : Int := 0
  /-- Rotation order: 0=XYZ, 1=XZY, 2=YXZ, 3=YZX, 4=ZXY, 5=ZYX -/
  rOrd : Int := 0

/-- Group selection parameters -/
structure GroupSelection where
  /-- Name of the group to operate on (empty = all geometry) -/
  group : String := ""
  /-- Type of group: 0=guess, 1=breakpoints, 2=edges, 3=points, 4=primitives -/
  groupType : Int := 0

/-- Bounding box specification -/
structure BoundingBox where
  /-- Size of the bounding box -/
  size : Vector3 := ⟨1, 1, 1⟩
  /-- Center position of the bounding box -/
  center : Vector3 := ⟨0, 0, 0⟩

/-- Plane specification -/
structure Plane where
  /-- Origin point on the plane -/
  origin : Vector3 := ⟨0, 0, 0⟩
  /-- Normal direction of the plane -/
  normal : Vector3 := ⟨0, 1, 0⟩
  /-- Distance along normal from origin -/
  dist : Float := 0


-- ============================================================================
-- Transform Node
-- ============================================================================

/-- Parameters for the Transform SOP -/
structure XformParams where
  /-- Group selection -/
  group : GroupSelection := {}
  /-- Primary transform -/
  xform : Transform := {}
  /-- Pre-transform applied before main transform -/
  prexform : Transform := {}
  /-- Space-separated list of attributes to transform (e.g., "N v") -/
  attribs : String := ""
  /-- Update point normals after transformation -/
  updateNormals : Bool := true
  /-- Update normals only for affected points -/
  updateAffectedNormals : Bool := false
  /-- Preserve vector attribute lengths during transformation -/
  vlength : Bool := false
  /-- Apply the inverse of the transformation -/
  invertXform : Bool := false
  /-- Create an attribute storing the transformation matrix -/
  addAttrib : Bool := false
  /-- Name of the attribute to store transformation -/
  outputAttrib : String := "xform"
  /-- Merge transformation with existing attribute -/
  outputMerge : Bool := false

/-- Transform geometry using translation, rotation, and scaling
  
Example:
```lean
let transformed = xform { 
  xform.translate := ⟨0, 1, 0⟩,
  xform.rotate := ⟨0, 45, 0⟩ 
} myGeo
```
-/
def xform (params : XformParams := {}) (geo : Geometry) : Geometry :=
  sop_xform geo
    params.group.group
    params.group.groupType
    params.xform.xOrd
    params.xform.rOrd
    params.xform.translate
    params.xform.rotate
    params.xform.scale
    params.xform.shear
    1.0 -- scale factor
    params.xform.pivot
    params.xform.pivotRotate
    params.prexform.xOrd
    params.prexform.rOrd
    params.prexform.translate
    params.prexform.rotate
    params.prexform.scale
    params.prexform.shear
    params.attribs
    params.updateNormals.toInt
    params.updateAffectedNormals.toInt
    params.vlength.toInt
    params.invertXform.toInt
    params.addAttrib.toInt
    params.outputAttrib
    params.outputMerge.toInt

-- ============================================================================
-- Boolean Node
-- ============================================================================

/-- Parameters for the Boolean SOP (version 2.0) -/
structure BooleanParams where
  /-- Group A primitive pattern -/
  aGroup : String := ""
  /-- Treat group A as a surface (rather than individual primitives) -/
  aSurface : Bool := true
  /-- Resolve internal intersections in A -/
  resolveA : Bool := true
  /-- Group B primitive pattern -/
  bGroup : String := ""
  /-- Treat group B as a surface (rather than individual primitives) -/
  bSurface : Bool := true
  /-- Resolve internal intersections in B -/
  resolveB : Bool := true
  /-- Boolean operation: 0=union, 1=intersect, 2=subtract A-B, 3=subtract B-A, 4=shatter -/
  operation : Int := 0
  /-- Subtract choices: 0=A-B, 1=B-A -/
  subtractChoices : Int := 0
  /-- Shatter choices: 0=all pieces, 1=unique, 2=fragment -/
  shatterChoices : Int := 0
  /-- Only consider open curves for boolean -/
  openCurvesOnly : Bool := false
  /-- Generate seam edges where A intersects itself -/
  generateAASeams : Bool := false
  /-- Generate seam edges where B intersects itself -/
  generateBBSeams : Bool := false
  /-- Generate seam edges where A intersects B -/
  generateABSeams : Bool := false
  /-- Winding operation: 0=default, 1=complement -/
  windingOp : Int := 0
  /-- Merge neighboring polygons with same attribute values -/
  mergeNeighbors : Bool := false
  /-- Remove triangulation from coplanar triangles -/
  detriangulate : Bool := true
  /-- Remove inline points along edges -/
  removeInlinePoints : Bool := true
  /-- Ensure seam edges are unique (no duplicates) -/
  uniqueSeams : Bool := false
  /-- Fix polygon normals to point outward -/
  correctNormals : Bool := true
  /-- Output group for A-A intersection polygons -/
  useAXAPolys : Bool := false
  axaPolys : String := ""
  /-- Output group for A-B intersection polygons -/
  useAXBPolys : Bool := false
  axbPolys : String := ""
  /-- Output group for A-A intersection seam edges -/
  useAXAList : Bool := false
  axaList : String := ""
  /-- Output group for A-B intersection seam edges -/
  useAXBList : Bool := false
  axbList : String := ""
  /-- Collapse tiny edges below threshold -/
  collapseTinyEdges : Bool := false
  /-- Length threshold for collapsing edges -/
  lengthThreshold : Float := 0.001
  /-- Output all polygons from A -/
  useAPolys : Bool := false
  aPolys : String := ""
  /-- Output A polygons that are inside B -/
  useAInsideB : Bool := false
  aInsideB : String := ""
  /-- Output A polygons that are outside B -/
  useAOutsideB : Bool := false
  aOutsideB : String := ""
  /-- Output all polygons from B -/
  useBPolys : Bool := false
  bPolys : String := ""
  /-- Output B polygons that are inside A -/
  useBInsideA : Bool := false
  bInsideA : String := ""
  /-- Output B polygons that are outside A -/
  useBOutsideA : Bool := false
  bOutsideA : String := ""
  /-- Output polygons in A-B overlap region -/
  useABOverlap : Bool := false
  abOverlap : String := ""
  /-- Output pieces unique to A -/
  useAOnlyPieces : Bool := false
  aOnlyPieces : String := ""
  /-- Output pieces unique to B -/
  useBOnlyPieces : Bool := false
  bOnlyPieces : String := ""
  /-- Output pieces in both A and B -/
  useABPieces : Bool := false
  abPieces : String := ""
  /-- Output reversed polygons -/
  useReversedPolys : Bool := false
  reversedPolys : String := ""
  /-- Output A-A seam edges -/
  useAASeamEdges : Bool := false
  aaSeamEdges : String := ""
  /-- Output B-B seam edges -/
  useBBSeamEdges : Bool := false
  bbSeamEdges : String := ""
  /-- Output A-B seam edges -/
  useABSeamEdges : Bool := false
  abSeamEdges : String := ""
  /-- Depth range for A (for visualization/filtering) -/
  aDepth : Vector2 := ⟨0, 0⟩
  /-- Depth range for B (for visualization/filtering) -/
  bDepth : Vector2 := ⟨0, 0⟩

/-- Perform boolean operations (union, intersect, subtract) between two geometries

Example:
```lean
let result = boolean { 
  operation := 0  -- union
} sphereGeo cubeGeo
```
-/
def boolean (params : BooleanParams := {}) (geoA geoB : Geometry) : Geometry :=
  sop_boolean_2_0 geoA geoB
    params.aGroup
    params.aSurface.toInt
    params.resolveA.toInt
    params.bGroup
    params.bSurface.toInt
    params.resolveB.toInt
    params.operation
    params.subtractChoices
    params.shatterChoices
    params.openCurvesOnly.toInt
    params.generateAASeams.toInt
    params.generateBBSeams.toInt
    params.generateABSeams.toInt
    params.windingOp
    params.mergeNeighbors.toInt
    params.detriangulate.toInt
    params.removeInlinePoints.toInt
    params.uniqueSeams.toInt
    params.correctNormals.toInt
    params.useAXAPolys.toInt
    params.axaPolys
    params.useAXBPolys.toInt
    params.axbPolys
    params.useAXAList.toInt
    params.axaList
    params.useAXBList.toInt
    params.axbList
    params.collapseTinyEdges.toInt
    params.lengthThreshold
    params.useAPolys.toInt
    params.aPolys
    params.useAInsideB.toInt
    params.aInsideB
    params.useAOutsideB.toInt
    params.aOutsideB
    params.useBPolys.toInt
    params.bPolys
    params.useBInsideA.toInt
    params.bInsideA
    params.useBOutsideA.toInt
    params.bOutsideA
    params.useABOverlap.toInt
    params.abOverlap
    params.useAOnlyPieces.toInt
    params.aOnlyPieces
    params.useBOnlyPieces.toInt
    params.bOnlyPieces
    params.useABPieces.toInt
    params.abPieces
    params.useReversedPolys.toInt
    params.reversedPolys
    params.useAASeamEdges.toInt
    params.aaSeamEdges
    params.useBBSeamEdges.toInt
    params.bbSeamEdges
    params.useABSeamEdges.toInt
    params.abSeamEdges
    params.aDepth
    params.bDepth

-- ============================================================================
-- Box Node
-- ============================================================================

/-- Parameters for the Box SOP
See: https://www.sidefx.com/docs/houdini/nodes/sop/box.html
-/
structure BoxParams where
  /-- Primitive type: 0=Polygon, 1=Polygon Mesh, 2=Mesh, 3=NURBS Curve, 4=Bezier Curve -/
  type : Int := 0
  /-- Surface type: 0=Rows, 1=Rows and Columns, 2=Triangles, 3=Quadrilaterals, 4=Alternating Triangles, 5=Revolved -/
  surfaceType : Int := 0
  /-- Consolidate points at corners (removes duplicate points) -/
  consolidatePoints : Bool := true
  /-- Size of the box along X, Y, and Z axes -/
  size : Vector3 := ⟨1, 1, 1⟩
  /-- Center position of the box in world space -/
  center : Vector3 := ⟨0, 0, 0⟩
  /-- Rotation angles in degrees around X, Y, and Z axes -/
  rotate : Vector3 := ⟨0, 0, 0⟩
  /-- Uniform scale factor applied after size -/
  scale : Float := 1.0
  /-- Division rate for rows and columns (legacy parameter) -/
  divRate : Vector3 := ⟨1, 1, 1⟩
  /-- Order for NURBS/Bezier curves (degree + 1) -/
  orderRate : Vector3 := ⟨4, 4, 4⟩
  /-- Enable manual control of divisions -/
  doDivs : Bool := true
  /-- Number of divisions along X, Y, and Z (rows, columns, height divisions) -/
  divs : Vector3 := ⟨1, 1, 1⟩
  /-- Display rebar visualization (wireframe overlay) -/
  rebar : Bool := false
  /-- Create an oriented bounding box from reference geometry -/
  orientedBBox : Bool := false
  /-- Add per-vertex normals (instead of per-point) -/
  vertexNormals : Bool := false

/-- Create a box or cube primitive
Creates a six-sided box with customizable divisions and orientation.

Example:
```lean
-- Simple unit cube
let cube = box {}

-- Rectangular box with subdivisions
let subdividedBox = box { 
  size := ⟨2, 1, 0.5⟩, 
  divs := ⟨4, 2, 1⟩ 
}

-- Rotated box
let rotatedBox = box {
  rotate := ⟨0, 45, 0⟩,
  center := ⟨0, 1, 0⟩
}
```

See: https://www.sidefx.com/docs/houdini/nodes/sop/box.html
-/
def box (params : BoxParams := {}) (refGeo : Geometry := default) : Geometry :=
  sop_box refGeo
    params.type
    params.surfaceType
    params.consolidatePoints.toInt
    params.size
    params.center
    params.rotate
    params.scale
    params.divRate
    params.orderRate
    params.doDivs.toInt
    params.divs
    params.rebar.toInt
    params.orientedBBox.toInt
    params.vertexNormals.toInt

-- ============================================================================
-- Sphere Node
-- ============================================================================

/-- Parameters for the Sphere SOP
See: https://www.sidefx.com/docs/houdini/nodes/sop/sphere.html
-/
structure SphereParams where
  /-- Primitive type: 0=Polygon, 1=Polygon Mesh, 2=Mesh, 3=NURBS, 4=Bezier -/
  type : Int := 0
  /-- Surface type: varies by primitive type -/
  surfaceType : Int := 0
  /-- Radius along X, Y, and Z axes (use different values for ellipsoids) -/
  radius : Vector3 := ⟨1, 1, 1⟩
  /-- Center position of the sphere in world space -/
  center : Vector3 := ⟨0, 0, 0⟩
  /-- Rotation angles in degrees around X, Y, and Z axes -/
  rotate : Vector3 := ⟨0, 0, 0⟩
  /-- Uniform scale factor applied after radius -/
  scale : Float := 1.0
  /-- Orientation axis: 0=X axis, 1=Y axis, 2=Z axis -/
  orient : Int := 2
  /-- Frequency: number of unique points (1=standard, higher=more variation) -/
  frequency : Int := 1
  /-- U order for NURBS/Bezier (degree + 1) -/
  orderU : Int := 4
  /-- V order for NURBS/Bezier (degree + 1) -/
  orderV : Int := 4
  /-- Create imperfect sphere (adds slight irregularities) -/
  imperfect : Bool := false
  /-- Add point at top pole (for polygon mesh) -/
  upole : Bool := false
  /-- Use accurate sphere tessellation (vs. fast approximation) -/
  accurate : Bool := true
  /-- Use triangles at poles instead of n-gons -/
  triangularPoles : Bool := false
  /-- Number of rows (latitude divisions) -/
  rows : Int := 13
  /-- Number of columns (longitude divisions) -/
  cols : Int := 24

/-- Create a sphere or ellipsoid primitive
Creates a spherical surface with customizable tessellation and shape.

Example:
```lean
-- Standard UV sphere
let sphere = sphere {}

-- High-resolution sphere
let hiResSphere = sphere { 
  rows := 32, 
  cols := 64 
}

-- Ellipsoid
let ellipsoid = sphere {
  radius := ⟨1, 2, 1⟩  -- stretched along Y
}

-- Geodesic-style with triangular poles
let geodesic = sphere {
  triangularPoles := true,
  rows := 20,
  cols := 20
}
```

See: https://www.sidefx.com/docs/houdini/nodes/sop/sphere.html
-/
def sphere (params : SphereParams := {}) (refGeo : Geometry := default) : Geometry :=
  sop_sphere refGeo
    params.type
    params.surfaceType
    params.radius
    params.center
    params.rotate
    params.scale
    params.orient
    params.frequency
    params.orderU
    params.orderV
    params.imperfect.toInt
    params.upole.toInt
    params.accurate.toInt
    params.triangularPoles.toInt
    params.rows
    params.cols

-- ============================================================================
-- Grid Node
-- ============================================================================

/-- Parameters for the Grid SOP
See: https://www.sidefx.com/docs/houdini/nodes/sop/grid.html
-/
structure GridParams where
  /-- Primitive type: 0=Polygon, 1=Polygon Mesh, 2=Mesh, 3=NURBS, 4=Bezier -/
  type : Int := 0
  /-- Surface type: varies by primitive type -/
  surfaceType : Int := 0
  /-- Orientation plane: 0=XY plane, 1=YZ plane, 2=ZX plane -/
  orientation : Int := 2
  /-- Size of the grid along its two axes -/
  size : Vector2 := ⟨1, 1⟩
  /-- Center position of the grid in world space -/
  center : Vector3 := ⟨0, 0, 0⟩
  /-- Rotation angles in degrees around X, Y, and Z axes -/
  rotate : Vector3 := ⟨0, 0, 0⟩
  /-- Number of rows (divisions along first axis) -/
  rows : Int := 10
  /-- Number of columns (divisions along second axis) -/
  cols : Int := 10
  /-- U order for NURBS/Bezier (degree + 1) -/
  orderU : Int := 4
  /-- V order for NURBS/Bezier (degree + 1) -/
  orderV : Int := 4
  /-- U interpolation end conditions: 0=off, 1=on -/
  interpU : Int := 0
  /-- V interpolation end conditions: 0=off, 1=on -/
  interpV : Int := 0

/-- Create a planar grid of polygons
Creates a flat rectangular grid useful for ground planes, particle emission surfaces, or as a base for terrain.

Example:
```lean
-- Standard XY grid
let grid = grid {}

-- Ground plane (ZX oriented)
let ground = grid { 
  orientation := 2,  -- ZX plane
  size := ⟨10, 10⟩,
  rows := 20,
  cols := 20
}

-- High-resolution grid for displacement
let hiResGrid = grid {
  size := ⟨5, 5⟩,
  rows := 100,
  cols := 100
}
```

See: https://www.sidefx.com/docs/houdini/nodes/sop/grid.html
-/
def grid (params : GridParams := {}) : Geometry :=
  sop_grid
    params.type
    params.surfaceType
    params.orientation
    params.size
    params.center
    params.rotate
    params.rows
    params.cols
    params.orderU
    params.orderV
    params.interpU
    params.interpV

-- ============================================================================
-- Scatter Node
-- ============================================================================

/-- Parameters for the Scatter SOP (version 2.0)
See: https://www.sidefx.com/docs/houdini/nodes/sop/scatter.html
-/
structure ScatterParams where
  /-- Primitive group to scatter points onto -/
  group : String := ""
  /-- Generate by: 0=Total Count, 1=Density, 2=Density Texture -/
  generateBy : Int := 0
  /-- Treat each voxel independently for volumes -/
  indepVoxel : Bool := false
  /-- Force the exact total count (may exceed density limits) -/
  forceTotal : Bool := true
  /-- Total number of points to generate (when generateBy=0) -/
  npts : Int := 1000
  /-- Global density scale multiplier -/
  densityScale : Float := 1.0
  /-- Use a point/primitive attribute to control density -/
  useDensityAttrib : Bool := false
  /-- Name of density attribute (higher values = more points) -/
  densityAttrib : String := "density"
  /-- Use primitive area attribute for density weighting -/
  useAreaAttrib : Bool := false
  /-- Name of area attribute -/
  areaAttrib : String := "area"
  /-- Use primitive area for volumes (vs. just voxel count) -/
  useAreaForVolumes : Bool := false
  /-- Use a texture map to control density distribution -/
  useDensityTexture : Bool := false
  /-- Path to density texture (white=dense, black=sparse) -/
  densityTexture : String := ""
  /-- UV attribute name for texture lookup -/
  uvAttrib : String := "uv"
  /-- Attribute storing how many points were created per primitive -/
  primCountAttrib : String := ""
  /-- Enable emergency limit to prevent excessive point generation -/
  useEmergencyLimit : Bool := false
  /-- Maximum points to generate (safety limit) -/
  emergencyLimit : Int := 10000000
  /-- Random seed for point distribution -/
  seed : Float := 0
  /-- Use per-primitive seed attribute instead of global seed -/
  overridePrimSeed : Bool := false
  /-- Name of per-primitive seed attribute -/
  primSeedAttrib : String := "primseed"
  /-- Randomize final point order (vs. primitive order) -/
  randomizeOrder : Bool := false
  /-- Apply relaxation to push points apart -/
  relaxPoints : Bool := false
  /-- Number of relaxation iterations (higher = more even distribution) -/
  relaxIterations : Int := 10
  /-- Use geometric (face) normals instead of vertex normals -/
  useGeometricNormals : Bool := false
  /-- Store source primitive number on scattered points -/
  usePrimNumAttrib : Bool := false
  /-- Name of primitive number attribute -/
  primNumAttrib : String := "prim"
  /-- Store primitive UVW coordinates on scattered points -/
  usePrimUVWAttrib : Bool := false
  /-- Name of primitive UVW attribute -/
  primUVWAttrib : String := "primuvw"
  /-- Create attribute with actual point density achieved -/
  useOutputDensityAttrib : Bool := false
  /-- Name of output density attribute -/
  outputDensityAttrib : String := "density"
  /-- Create attribute with point radius (for relaxation) -/
  useOutputRadiusAttrib : Bool := false
  /-- Name of output radius attribute (typically "pscale") -/
  outputRadiusAttrib : String := "pscale"
  /-- Create unique ID attribute for each scattered point -/
  useOutputIdAttrib : Bool := false
  /-- Name of output ID attribute -/
  outputIdAttrib : String := "id"
  /-- Compute radius in texture space (vs. world space) -/
  radiusInTextureSpace : Bool := false
  /-- Point attributes to copy from surface to scattered points -/
  pointAttribs : String := ""
  /-- Vertex attributes to interpolate onto scattered points -/
  vertAttribs : String := ""
  /-- Primitive attributes to copy to scattered points -/
  primAttribs : String := ""
  /-- Detail attributes to copy to scattered points -/
  detailAttribs : String := ""
  /-- Detail attributes to keep as detail (not per-point) -/
  detailAttribsAsDetail : String := ""
  /-- Scale factor for computed radii -/
  scaleRadiiBy : Float := 1.0
  /-- Enforce maximum radius limit -/
  useMaxRadius : Bool := false
  /-- Maximum allowed radius value -/
  maxRadius : Float := 0.0

/-- Scatter points randomly across surface geometry
Creates randomly distributed points on surfaces, useful for instancing, particle emission, or procedural placement.

Example:
```lean
-- Scatter 1000 points on geometry
let scattered = scatter { npts := 1000 } surfaceGeo

-- Density-based scattering with relaxation
let relaxed = scatter {
  generateBy := 1,  -- density mode
  densityScale := 10.0,
  relaxPoints := true,
  relaxIterations := 20,
  useOutputRadiusAttrib := true
} surfaceGeo

-- Texture-controlled scattering
let textured = scatter {
  generateBy := 2,  -- texture mode
  useDensityTexture := true,
  densityTexture := "density_map.exr",
  npts := 5000
} surfaceGeo
```

See: https://www.sidefx.com/docs/houdini/nodes/sop/scatter.html
-/
def scatter (params : ScatterParams := {}) (geo : Geometry) : Geometry :=
  sop_scatter_2_0 geo
    params.group
    params.generateBy
    params.indepVoxel.toInt
    params.forceTotal.toInt
    params.npts
    params.densityScale
    params.useDensityAttrib.toInt
    params.densityAttrib
    params.useAreaAttrib.toInt
    params.areaAttrib
    params.useAreaForVolumes.toInt
    params.useDensityTexture.toInt
    params.densityTexture
    params.uvAttrib
    params.primCountAttrib
    params.useEmergencyLimit.toInt
    params.emergencyLimit
    params.seed
    params.overridePrimSeed.toInt
    params.primSeedAttrib
    params.randomizeOrder.toInt
    params.relaxPoints.toInt
    params.relaxIterations
    params.useGeometricNormals.toInt
    params.usePrimNumAttrib.toInt
    params.primNumAttrib
    params.usePrimUVWAttrib.toInt
    params.primUVWAttrib
    params.useOutputDensityAttrib.toInt
    params.outputDensityAttrib
    params.useOutputRadiusAttrib.toInt
    params.outputRadiusAttrib
    params.useOutputIdAttrib.toInt
    params.outputIdAttrib
    params.radiusInTextureSpace.toInt
    params.pointAttribs
    params.vertAttribs
    params.primAttribs
    params.detailAttribs
    params.detailAttribsAsDetail
    params.scaleRadiiBy
    params.useMaxRadius.toInt
    params.maxRadius

-- ============================================================================
-- Copy to Points Node
-- ============================================================================

/-- Parameters for the Copy to Points SOP (version 2.0)
See: https://www.sidefx.com/docs/houdini/nodes/sop/copytopoints.html
-/
structure CopyToPointsParams where
  /-- Source geometry primitive group to copy -/
  sourceGroup : String := ""
  /-- Source group type: 0=Guess, 3=Points, 4=Primitives -/
  sourceGroupType : Int := 4
  /-- Target points group to copy onto -/
  targetGroup : String := ""
  /-- Use ID attribute to match source pieces to target points -/
  useIdAttrib : Bool := false
  /-- Name of ID attribute for matching (e.g., "id", "piece") -/
  idAttrib : String := "id"
  /-- Pack each copy into a packed primitive (more memory efficient) -/
  pack : Bool := false
  /-- Pivot location: 0=Origin, 1=Centroid -/
  pivot : Int := 0
  /-- Viewport LOD for packed primitives -/
  viewportLod : Int := 0
  /-- Apply point transforms (position, rotation, scale) to copies -/
  transform : Bool := true
  /-- Use implicit normal for orientation if no explicit N attribute -/
  useImplicitN : Bool := true
  /-- Array of dictionaries specifying target attribute behavior -/
  targetAttribs : DictArray := default

/-- Copy source geometry onto target points
Creates copies of source geometry at each target point location, with optional transformation and attribute transfer.

Example:
```lean
-- Simple copy
let copied = copyToPoints {} sourceGeo targetPoints

-- Copy with packing (efficient for many copies)
let packedCopies = copyToPoints {
  pack := true,
  pivot := 1  -- use centroid
} sourceGeo targetPoints

-- Copy with ID matching (multiple source pieces)
let matched = copyToPoints {
  useIdAttrib := true,
  idAttrib := "piece"
} piecesGeo targetPoints
```

See: https://www.sidefx.com/docs/houdini/nodes/sop/copytopoints.html
-/
def copyToPoints (params : CopyToPointsParams := {}) 
    (source target : Geometry) : Geometry :=
  sop_copytopoints_2_0 source target
    params.sourceGroup
    params.sourceGroupType
    params.targetGroup
    params.useIdAttrib.toInt
    params.idAttrib
    params.pack.toInt
    params.pivot
    params.viewportLod
    params.transform.toInt
    params.useImplicitN.toInt
    params.targetAttribs

-- ============================================================================
-- Subdivide Node
-- ============================================================================

/-- Parameters for the Subdivide SOP
See: https://www.sidefx.com/docs/houdini/nodes/sop/subdivide.html
-/
structure SubdivideParams where
  /-- Primitive group to subdivide -/
  subdivideGroup : String := ""
  /-- Crease edges group (edges to keep sharp) -/
  creasesGroup : String := ""
  /-- Number of subdivision iterations (each doubles polygon count) -/
  iterations : Int := 1
  /-- Override automatic crease weight detection -/
  overrideCrease : Bool := false
  /-- Crease weight (0=smooth, 1=sharp) -/
  creaseWeight : Float := 0.0
  /-- Output creased edges to a group -/
  outputCrease : Bool := false
  /-- Name of output crease edges group -/
  outCreaseGroup : String := "creasegroup"
  /-- Close holes in open polygons before subdividing -/
  closeHoles : Bool := true
  /-- Add surrounding polygons to group automatically -/
  surroundPoly : Bool := false
  /-- Bias factor for adaptive subdivision -/
  bias : Float := 0.0
  /-- Smooth vertex positions (in addition to subdividing) -/
  smoothVertex : Bool := false
  /-- Maintain consistent topology (required for animation) -/
  consistTopology : Bool := false
  /-- Treat creases linearly (simpler crease behavior) -/
  linearCreases : Bool := false
  /-- Subdivision algorithm: 0=Catmull-Clark (quads), 1=Loop (triangles), 2=Bilinear -/
  algorithm : Int := 0
  /-- Output as polygon soups (faster for large meshes) -/
  buildPolySoups : Bool := false
  /-- Subdivide curves independently of surfaces -/
  indepCurves : Bool := false
  /-- Recompute point normals after subdivision -/
  updateNormals : Bool := true
  /-- Remove holes in mesh -/
  removeHoles : Bool := false
  /-- Vertex boundary interpolation: "edge only", "edge and corner" -/
  vtxBoundary : String := "edge only"
  /-- Face-varying interpolation: "none", "corners only", "corners plus1", "boundaries", "all" -/
  fvarLinear : String := "corners plus1"
  /-- Crease method: "uniform", "chaikin" -/
  creaseMethod : String := "uniform"
  /-- Triangle subdivision scheme: "catmull-clark", "loop" -/
  triangleSubd : String := "catmull-clark"

/-- Subdivide polygons for smooth surfaces
Applies Catmull-Clark or other subdivision schemes to create smooth surfaces from coarse polygon meshes.

Example:
```lean
-- Simple subdivision
let smooth = subdivide { iterations := 2 } coarseMesh

-- Subdivision with creases
let creased = subdivide {
  iterations := 3,
  creasesGroup := "sharp_edges",
  creaseWeight := 1.0
} mesh

-- Loop subdivision for triangles
let loopSubdiv = subdivide {
  algorithm := 1,  -- Loop
  iterations := 2
} triangleMesh
```

See: https://www.sidefx.com/docs/houdini/nodes/sop/subdivide.html
-/
def subdivide (params : SubdivideParams := {}) 
    (geo refGeo : Geometry := default) : Geometry :=
  sop_subdivide geo refGeo
    params.subdivideGroup
    params.creasesGroup
    params.iterations
    params.overrideCrease.toInt
    params.creaseWeight
    params.outputCrease.toInt
    params.outCreaseGroup
    params.closeHoles.toInt
    params.surroundPoly.toInt
    params.bias
    params.smoothVertex.toInt
    params.consistTopology.toInt
    params.linearCreases.toInt
    params.algorithm
    params.buildPolySoups.toInt
    params.indepCurves.toInt
    params.updateNormals.toInt
    params.removeHoles.toInt
    params.vtxBoundary
    params.fvarLinear
    params.creaseMethod
    params.triangleSubd

-- ============================================================================
-- Polyextrude Node
-- ============================================================================

/-- Parameters for the PolyExtrude SOP (version 2.0)
See: https://www.sidefx.com/docs/houdini/nodes/sop/polyextrude.html
-/
structure PolyExtrudeParams where
  /-- Primitive group to extrude -/
  group : String := ""
  /-- How to split geometry: 0=Connected, 1=All Polygons, 2=Specified Group -/
  splitType : Int := 0
  /-- Use split group (when splitType=2) -/
  useSplitGroup : Bool := false
  /-- Name of split group -/
  splitGroup : String := ""
  /-- Extrusion mode: 0=Point, 1=Primitive -/
  extrusionMode : Int := 0
  /-- Point normal source: 0=Attribute, 1=Vertex -/
  ptNormalSrc : Int := 0
  /-- Point normal attribute name -/
  ptNormalAttrib : String := "N"
  /-- Extrusion distance along normal -/
  dist : Float := 0.1
  /-- Inset amount before extrusion -/
  inset : Float := 0.0
  /-- Twist angle in degrees during extrusion -/
  twist : Float := 0.0
  /-- Number of divisions along extrusion -/
  divs : Int := 1
  /-- Spine type: 0=None, 1=Input 2 -/
  spineType : Int := 0
  /-- Transform the front cap -/
  xformFront : Bool := true
  /-- Transform space: 0=Local, 1=Global -/
  xformSpace : Int := 0
  /-- Apply rotation/scale/translation -/
  rst : Bool := false
  /-- Apply XYZ transform -/
  xyz : Bool := false
  /-- Translation of extruded geometry -/
  translate : Vector3 := ⟨0, 0, 0⟩
  /-- Rotation in degrees -/
  rotate : Vector3 := ⟨0, 0, 0⟩
  /-- Scale factors per axis -/
  scale : Vector3 := ⟨1, 1, 1⟩
  /-- Shear values -/
  shear : Vector3 := ⟨0, 0, 0⟩
  /-- Pivot point -/
  pivot : Vector3 := ⟨0, 0, 0⟩
  /-- Pivot rotation -/
  pivotRotate : Vector3 := ⟨0, 0, 0⟩
  /-- Apply pre-transform rotation/scale/translation -/
  prexformRst : Bool := false
  /-- Apply pre-transform XYZ -/
  prexformXyz : Bool := false
  /-- Pre-transform translation -/
  prexformTranslate : Vector3 := ⟨0, 0, 0⟩
  /-- Pre-transform rotation -/
  prexformRotate : Vector3 := ⟨0, 0, 0⟩
  /-- Pre-transform scale -/
  prexformScale : Vector3 := ⟨1, 1, 1⟩
  /-- Pre-transform shear -/
  prexformShear : Vector3 := ⟨0, 0, 0⟩
  /-- Output front cap geometry -/
  outputFront : Bool := true
  /-- Create front cap group -/
  outputFrontGrp : Bool := false
  /-- Front cap group name -/
  frontGrp : String := "front"
  /-- Output back cap geometry -/
  outputBack : Bool := true
  /-- Create back cap group -/
  outputBackGrp : Bool := false
  /-- Back cap group name -/
  backGrp : String := "back"
  /-- Output side geometry -/
  outputSide : Bool := true
  /-- Create side group -/
  outputSideGrp : Bool := false
  /-- Side group name -/
  sideGrp : String := "side"
  /-- Create front seam edges group -/
  outputFrontSeamGrp : Bool := false
  /-- Front seam group name -/
  frontSeamGrp : String := "frontseam"
  /-- Create back seam edges group -/
  outputBackSeamGrp : Bool := false
  /-- Back seam group name -/
  backSeamGrp : String := "backseam"
  /-- Preserve original primitive groups on output -/
  preserveGroups : Bool := true
  /-- Limit inset to prevent self-intersection -/
  limitInset : Bool := false
  /-- Use common limit for all pieces -/
  commonLimit : Bool := true
  /-- Add vertex normals to output -/
  addVertexNormals : Bool := false
  /-- Cusp angle in degrees for normal computation -/
  cuspAngle : Float := 60.0
  /-- Apply cusp angle to front cap -/
  cuspFront : Bool := false
  /-- Apply cusp angle to back cap -/
  cuspBack : Bool := false
  /-- Generate UV coordinates -/
  genUVs : Bool := false
  /-- UV generation style -/
  uvStyle : Int := 0
  /-- UV scaling method -/
  uvScaling : Int := 0
  /-- Front cap UV magnitude -/
  frontMagnitude : Float := 0.0
  /-- Back cap UV magnitude -/
  backMagnitude : Float := 0.0
  /-- Front cap UV stiffness -/
  frontStiffness : Float := 0.0
  /-- Back cap UV stiffness -/
  backStiffness : Float := 0.0
  /-- Interpolation type -/
  interpolation : Int := 0
  /-- Spacing type -/
  spacing : Int := 0
  /-- Reverse spine direction -/
  reverseSpineDirection : Bool := false
  /-- Axial rotation in degrees -/
  axialRotation : Float := 0.0
  /-- Front blend amount -/
  frontBlend : Float := 0.0
  /-- Back blend amount -/
  backBlend : Float := 0.0
  /-- Global thickness scale -/
  thicknessScale : Float := 1.0
  /-- Use per-point/primitive thickness attribute -/
  useThicknessAttrib : Bool := false
  /-- Thickness attribute name -/
  thicknessAttrib : String := "thickness"
  /-- Use thickness ramp -/
  useThicknessRamp : Bool := false
  /-- Thickness ramp curve -/
  thicknessRamp : FloatRamp := default
  /-- Use twist attribute -/
  useTwistAttrib : Bool := false
  /-- Twist attribute name -/
  twistAttrib : String := "twist"
  /-- Use twist ramp -/
  useTwistRamp : Bool := false
  /-- Twist ramp curve -/
  twistRamp : FloatRamp := default
  /-- Twist scale factor -/
  twistScale : Float := 0.0
  /-- Use local Z scale attribute -/
  useLocalZScaleAttrib : Bool := false
  /-- Local Z scale attribute name -/
  localZScaleAttrib : String := "zscale"
  /-- Use local inset scale attribute -/
  useLocalInsetScaleAttrib : Bool := false
  /-- Local inset scale attribute name -/
  localInsetScaleAttrib : String := "insetscale"
  /-- Use local twist attribute -/
  useLocalTwistAttrib : Bool := false
  /-- Local twist scale attribute name -/
  localTwistScaleAttrib : String := "twistscale"
  /-- Use local divisions attribute -/
  useLocalDivsAttrib : Bool := false
  /-- Local divisions scale attribute name -/
  localDivsScaleAttrib : String := "divsscale"
  /-- Use local X attribute -/
  useLocalXAttrib : Bool := false
  /-- Local X attribute name -/
  localXAttrib : String := "xlocal"
  /-- Use local Z attribute -/
  useLocalZAttrib : Bool := false
  /-- Local Z attribute name -/
  localZAttrib : String := "zlocal"
  /-- Use local center attribute -/
  useLocalCtrAttrib : Bool := false
  /-- Local center attribute name -/
  localCtrAttrib : String := "ctrlocal"

/-- Extrude polygon faces
Extrudes polygon faces along normals or a custom direction, with inset, twist, and subdivision options.

Example:
```lean
-- Simple extrusion
let extruded = polyExtrude { dist := 0.5 } mesh

-- Extrude with inset
let insetExtruded = polyExtrude {
  dist := 1.0,
  inset := 0.1,
  divs := 3
} mesh

-- Twisted extrusion
let twisted = polyExtrude {
  dist := 2.0,
  twist := 180.0,
  divs := 10
} mesh
```

See: https://www.sidefx.com/docs/houdini/nodes/sop/polyextrude.html
-/
def polyExtrude (params : PolyExtrudeParams := {}) 
    (geo spine : Geometry := default) : Geometry :=
  sop_polyextrude_2_0 geo spine
    params.group
    params.splitType
    params.useSplitGroup.toInt
    params.splitGroup
    params.extrusionMode
    params.ptNormalSrc
    params.ptNormalAttrib
    params.dist
    params.inset
    params.twist
    params.divs
    params.spineType
    params.xformFront.toInt
    params.xformSpace
    params.rst.toInt
    params.xyz.toInt
    params.translate
    params.rotate
    params.scale
    params.shear
    params.pivot
    params.pivotRotate
    params.prexformRst.toInt
    params.prexformXyz.toInt
    params.prexformTranslate
    params.prexformRotate
    params.prexformScale
    params.prexformShear
    params.outputFront.toInt
    params.outputFrontGrp.toInt
    params.frontGrp
    params.outputBack.toInt
    params.outputBackGrp.toInt
    params.backGrp
    params.outputSide.toInt
    params.outputSideGrp.toInt
    params.sideGrp
    params.outputFrontSeamGrp.toInt
    params.frontSeamGrp
    params.outputBackSeamGrp.toInt
    params.backSeamGrp
    params.preserveGroups.toInt
    params.limitInset.toInt
    params.commonLimit.toInt
    params.addVertexNormals.toInt
    params.cuspAngle
    params.cuspFront.toInt
    params.cuspBack.toInt
    params.genUVs.toInt
    params.uvStyle
    params.uvScaling
    params.frontMagnitude
    params.backMagnitude
    params.frontStiffness
    params.backStiffness
    params.interpolation
    params.spacing
    params.reverseSpineDirection.toInt
    params.axialRotation
    params.frontBlend
    params.backBlend
    params.thicknessScale
    params.useThicknessAttrib.toInt
    params.thicknessAttrib
    params.useThicknessRamp.toInt
    params.thicknessRamp
    params.useTwistAttrib.toInt
    params.twistAttrib
    params.useTwistRamp.toInt
    params.twistRamp
    params.twistScale
    params.useLocalZScaleAttrib.toInt
    params.localZScaleAttrib
    params.useLocalInsetScaleAttrib.toInt
    params.localInsetScaleAttrib
    params.useLocalTwistAttrib.toInt
    params.localTwistScaleAttrib
    params.useLocalDivsAttrib.toInt
    params.localDivsScaleAttrib
    params.useLocalXAttrib.toInt
    params.localXAttrib
    params.useLocalZAttrib.toInt
    params.localZAttrib
    params.useLocalCtrAttrib.toInt
    params.localCtrAttrib

-- ============================================================================
-- Remesh Node
-- ============================================================================

/-- Parameters for the Remesh SOP -/
structure RemeshParams where
  /-- Primitive group to remesh -/
  group : String := ""
  /-- Edge group to preserve as hard edges -/
  hardEdges : String := ""
  /-- Point group to preserve as hard points -/
  hardPoints : String := ""
  /-- Preserve UV seam boundaries as hard edges -/
  hardenUVSeams : Bool := false
  /-- Name of UV attribute to use for seam detection -/
  uvAttribV : String := "uv"
  /-- Number of smoothing iterations -/
  iterations : Int := 10
  /-- Smoothing strength per iteration (0-1) -/
  smoothing : Float := 0.5
  /-- Only use input points as initial seed points -/
  inputPtsOnly : Bool := false
  /-- Detach remeshed region from non-group geometry -/
  detachFromNonGroup : Bool := false
  /-- Recompute normals after remeshing -/
  recomputeNormals : Bool := true
  /-- Sizing mode: 0=uniform, 1=adaptive -/
  sizing : Int := 0
  /-- Target edge length for remeshed triangles -/
  targetSize : Float := 0.1
  /-- Enable maximum edge length constraint -/
  useMaxSize : Bool := false
  /-- Maximum allowed edge length -/
  maxSize : Float := 1.0
  /-- Enable minimum edge length constraint -/
  useMinSize : Bool := false
  /-- Minimum allowed edge length -/
  minSize : Float := 0.0
  /-- Overall mesh density multiplier -/
  density : Float := 1.0
  /-- Gradation between small and large elements (adaptive mode) -/
  gradation : Float := 1.5
  /-- Use point attribute to control target mesh size -/
  useMeshSizeAttrib : Bool := false
  /-- Name of target mesh size attribute -/
  meshSizeAttrib : String := "targetmeshsize"
  /-- Use point attribute to control minimum mesh size -/
  useMinSizeAttrib : Bool := false
  /-- Name of minimum mesh size attribute -/
  minSizeAttrib : String := "minmeshsize"
  /-- Use point attribute to control maximum mesh size -/
  useMaxSizeAttrib : Bool := false
  /-- Name of maximum mesh size attribute -/
  maxSizeAttrib : String := "maxmeshsize"
  /-- Create edge group for detected hard edges -/
  useOutHardEdgesGroup : Bool := false
  /-- Name of output hard edges group -/
  outHardEdgesGroup : String := "hardedges"
  /-- Create point attribute with actual mesh size -/
  useOutMeshSizeAttrib : Bool := false
  /-- Name of output mesh size attribute -/
  outMeshSizeAttrib : String := "meshsize"
  /-- Create primitive attribute with mesh quality metric -/
  useOutMeshQualityAttrib : Bool := false
  /-- Name of output mesh quality attribute -/
  outMeshQualityAttrib : String := "quality"

/-- Rebuild input surface with a cleaner, more uniform triangular mesh

Example:
```lean
let remeshed = remesh { 
  targetSize := 0.1,
  iterations := 10,
  sizing := 0  -- uniform
} myGeo
```
-/
def remesh (params : RemeshParams := {}) 
    (geo sizing : Geometry := default) : Geometry :=
  sop_remesh_2_0 geo sizing
    params.group
    params.hardEdges
    params.hardPoints
    params.hardenUVSeams.toInt
    params.uvAttribV
    params.iterations
    params.smoothing
    params.inputPtsOnly.toInt
    params.detachFromNonGroup.toInt
    params.recomputeNormals.toInt
    params.sizing
    params.targetSize
    params.useMaxSize.toInt
    params.maxSize
    params.useMinSize.toInt
    params.minSize
    params.density
    params.gradation
    params.useMeshSizeAttrib.toInt
    params.meshSizeAttrib
    params.useMinSizeAttrib.toInt
    params.minSizeAttrib
    params.useMaxSizeAttrib.toInt
    params.maxSizeAttrib
    params.useOutHardEdgesGroup.toInt
    params.outHardEdgesGroup
    params.useOutMeshSizeAttrib.toInt
    params.outMeshSizeAttrib
    params.useOutMeshQualityAttrib.toInt
    params.outMeshQualityAttrib

-- ============================================================================
-- Blast (Delete) Node
-- ============================================================================

/-- Parameters for the Blast SOP -/
structure BlastParams where
  /-- Group pattern to delete -/
  group : String := ""
  /-- Group type: 0=guess, 2=edges, 3=points, 4=primitives -/
  groupType : Int := 4
  /-- Recompute point normals after deletion -/
  computeNormals : Bool := false
  /-- Keep group and delete everything else -/
  negate : Bool := false
  /-- Fill holes created by deletion -/
  fillHole : Bool := false
  /-- Remove group definition after deletion -/
  removeGroup : Bool := false

/-- Delete geometry by group selection

Example:
```lean
let cleaned = blast { 
  group := "deleteme",
  groupType := 4  -- primitives
} myGeo
```
-/
def blast (params : BlastParams := {}) (geo : Geometry) : Geometry :=
  sop_blast geo
    params.group
    params.groupType
    params.computeNormals.toInt
    params.negate.toInt
    params.fillHole.toInt
    params.removeGroup.toInt

-- ============================================================================
-- Fuse Node
-- ============================================================================

/-- Parameters for the Fuse SOP -/
structure FuseParams where
  /-- Point group to fuse -/
  group : String := ""
  /-- Enable distance-based fusing -/
  useDist : Bool := true
  /-- Maximum distance for points to be fused -/
  distance : Float := 0.001
  /-- Delete degenerate primitives after fusing -/
  delDegen : Bool := true
  /-- Keep points that are not referenced by primitives -/
  keepUnusedPoints : Bool := false
  /-- Keep consolidated point copies -/
  keepConsolidatedPoints : Bool := false
  /-- How to propagate groups: 0=union, 1=intersection -/
  groupPropagation : Int := 0
  /-- Snap mode: 0=average, 1=first, 2=last -/
  snapType : Int := 0
  /-- 3D tolerance for snapping -/
  tol3d : Float := 0.001
  /-- Snap point positions when fusing -/
  snapPointPos : Bool := false
  /-- Snap point attributes when fusing -/
  snapPointAttribs : Bool := false
  /-- Space-separated list of attributes to snap -/
  pointAttribNames : String := ""
  /-- Grid snap type: 0=spacing, 1=lines, 2=power of 2 -/
  gridType : Int := 0
  /-- Grid spacing in X, Y, Z -/
  gridSpacing : Vector3 := ⟨1, 1, 1⟩
  /-- Number of grid lines in X, Y, Z -/
  gridLines : Vector3 := ⟨10, 10, 10⟩
  /-- Power of 2 grid divisions in X, Y, Z -/
  gridPow2 : Vector3 := ⟨10, 10, 10⟩
  /-- Grid offset in X, Y, Z -/
  gridOffset : Vector3 := ⟨0, 0, 0⟩
  /-- Round to grid instead of snapping -/
  gridRound : Bool := false
  /-- Tolerance for grid snapping -/
  gridTol : Float := 0.0
  /-- Update point normals after fusing -/
  updateNormal : Bool := true
  /-- Use accurate but slower fusing algorithm -/
  accurate : Bool := true

/-- Merge points within a specified distance and consolidate overlapping points

Example:
```lean
let fused = fuse { 
  distance := 0.001,
  delDegen := true
} myGeo
```
-/
def fuse (params : FuseParams := {}) (geo : Geometry) : Geometry :=
  sop_fuse geo
    params.group
    params.useDist.toInt
    params.distance
    params.delDegen.toInt
    params.keepUnusedPoints.toInt
    params.keepConsolidatedPoints.toInt
    params.groupPropagation
    0 -- switcher
    params.snapType
    params.tol3d
    params.snapPointPos.toInt
    params.snapPointAttribs.toInt
    params.pointAttribNames
    0 -- snapswitcher
    params.gridType
    params.gridSpacing
    params.gridLines
    params.gridPow2
    params.gridOffset
    params.gridRound.toInt
    params.gridTol
    params.updateNormal.toInt
    params.accurate.toInt


-- ============================================================================
-- Attribute Create Node
-- ============================================================================

/-- Entry for a single attribute to create -/
structure AttribCreateEntry where
  /-- Attribute name -/
  name : String
  /-- Attribute class: 0=detail, 1=prim, 2=point, 3=vertex -/
  class' : Int := 0
  /-- Attribute type: 0=float, 1=int, 2=vector, 3=string -/
  type : Int := 0
  /-- Number of components (for multi-component attributes) -/
  size : Int := 1
  /-- Default numeric value -/
  value : Vector4 := ⟨0, 0, 0, 0⟩
  /-- Default string value -/
  string : String := ""

/-- Parameters for the Attribute Create SOP -/
structure AttribCreateParams where
  /-- Group to apply attribute to -/
  group : String := ""
  /-- Group type: 0=guess, 1=breakpoints, 2=edges, 3=points, 4=prims -/
  groupType : Int := 0
  /-- Array of attributes to create -/
  attributes : Array AttribCreateEntry := #[]

/-- Create or modify attributes on geometry

Example:
```lean
let withAttribs = attribCreate {
  attributes := #[
    { name := "Cd", class' := 2, type := 2, size := 3, value := ⟨1, 0, 0, 0⟩ }
  ]
} myGeo
```
-/
def attribCreate (params : AttribCreateParams := {}) (geo : Geometry) : Geometry :=
  -- Build DictArray from attributes array
  let numattr := params.attributes.foldl (init := DictArray.default) fun acc entry =>
    let dict := Dict.default
      |>.set "name" entry.name
      |>.set "class" entry.class'
      |>.set "type" entry.type
      |>.set "size" entry.size
      |>.set "default" entry.value
      |>.set "string" entry.string
    acc.append dict
  
  sop_attribcreate_2_0 geo
    params.group
    params.groupType
    0 -- encodenames
    numattr


-- ============================================================================
-- Convenience Namespaces
-- ============================================================================

namespace Geometry

abbrev xform (params : XformParams := {}) (geo : Geometry) : Geometry :=
  SOP.xform params geo

abbrev box (params : BoxParams := {}) : Geometry :=
  SOP.box params

abbrev sphere (params : SphereParams := {}) : Geometry :=
  SOP.sphere params

abbrev grid (params : GridParams := {}) : Geometry :=
  SOP.grid params

abbrev scatter (params : ScatterParams := {}) (geo : Geometry) : Geometry :=
  SOP.scatter params geo

abbrev copyToPoints (params : CopyToPointsParams := {}) 
    (source target : Geometry) : Geometry :=
  SOP.copyToPoints params source target

abbrev subdivide (params : SubdivideParams := {}) 
    (geo : Geometry) (refGeo : Geometry := default) : Geometry :=
  SOP.subdivide params geo refGeo

abbrev polyExtrude (params : PolyExtrudeParams := {}) 
    (geo : Geometry) (spine : Geometry := default) : Geometry :=
  SOP.polyExtrude params geo spine

abbrev remesh (params : RemeshParams := {}) 
    (geo : Geometry) (sizing : Geometry := default) : Geometry :=
  SOP.remesh params geo sizing

abbrev blast (params : BlastParams := {}) (geo : Geometry) : Geometry :=
  SOP.blast params geo

abbrev fuse (params : FuseParams := {}) (geo : Geometry) : Geometry :=
  SOP.fuse params geo

abbrev boolean (params : BooleanParams := {}) 
    (geoA geoB : Geometry) : Geometry :=
  SOP.boolean params geoA geoB

end Geometry

end HouLean.Apex.SOP
