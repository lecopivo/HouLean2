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
  surfaceType : Int := 4
  /-- Radius along X, Y, and Z axes (use different values for ellipsoids) -/
  radius : Vector3 := ⟨1, 1, 1⟩
  /-- Center position of the sphere in world space -/
  center : Vector3 := ⟨0, 0, 0⟩
  /-- Rotation angles in degrees around X, Y, and Z axes -/
  rotate : Vector3 := ⟨0, 0, 0⟩
  /-- Uniform scale factor applied after radius -/
  scale : Float := 1.0
  /-- Orientation axis: 0=X axis, 1=Y axis, 2=Z axis -/
  orient : Int := 1
  /-- Frequency: number of unique points (1=standard, higher=more variation) -/
  frequency : Int := 2
  /-- U order for NURBS/Bezier (degree + 1) -/
  orderU : Int := 4
  /-- V order for NURBS/Bezier (degree + 1) -/
  orderV : Int := 4
  /-- Create imperfect sphere (adds slight irregularities) -/
  imperfect : Bool := true
  /-- Add point at top pole (for polygon mesh) -/
  upole : Bool := false
  /-- Use accurate sphere tessellation (vs. fast approximation) -/
  accurate : Bool := true
  /-- Use triangles at poles instead of n-gons -/
  triangularPoles : Bool := true
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

--/-- Create or modify attributes on geometry
--
--Example:
--```lean
--let withAttribs = attribCreate {
--  attributes := #[
--    { name := "Cd", class' := 2, type := 2, size := 3, value := ⟨1, 0, 0, 0⟩ }
--  ]
--} myGeo
--```
---/
-- def attribCreate (params : AttribCreateParams := {}) (geo : Geometry) : Geometry :=
--   -- Build DictArray from attributes array
--   sorry
  -- let numattr := params.attributes.foldl (init := DictArray.default) fun acc entry =>
  --   let dict := Dict.default
  --     |>.set "name" entry.name
  --     |>.set "class" entry.class'
  --     |>.set "type" entry.type
  --     |>.set "size" entry.size
  --     |>.set "default" entry.value
  --     |>.set "string" entry.string
  --   acc.append dict
  
  -- sop_attribcreate_2_0 geo
  --   params.group
  --   params.groupType
  --   0 -- encodenames
  --   numattr

-- ============================================================================
-- Error Node
-- ============================================================================

/-- Parameters for the Error SOP -/
structure ErrorParams where
  /-- Array of error/warning messages to display -/
  errors : DictArray := default

/-- Display error or warning messages
Used to create controlled error conditions in your network for debugging or validation.

Example:
```lean
let errorGeo = error {
  errors := -- DictArray with error messages
} myGeo
```
-/
def error (params : ErrorParams := {}) (geo : Geometry) : Geometry :=
  sop_error geo params.errors

-- ============================================================================
-- Extract Centroid Node
-- ============================================================================

/-- Parameters for the Extract Centroid SOP -/
structure ExtractCentroidParams where
  /-- Partition type: 0=whole geometry, 1=by attribute -/
  partitionType : Int := 0
  /-- Attribute name for partitioning pieces -/
  pieceAttrib : String := "name"
  /-- Attribute class: 0=detail, 1=primitive, 2=point, 3=vertex -/
  class' : Int := 1
  /-- Centroid computation method: 0=bounding box center, 1=center of mass -/
  method : Int := 1
  /-- Output mode: 0=points only, 1=points with geometry -/
  output : Int := 0
  /-- Name of attribute to store centroid position -/
  centroidAttrib : String := "centroid"
  /-- Space-separated list of attributes to transfer to centroids -/
  transferAttributes : String := ""
  /-- Space-separated list of groups to transfer to centroids -/
  transferGroups : String := ""

/-- Extract centroids (center points) from geometry pieces

Example:
```lean
-- Extract centroids from pieces
let centroids = extractCentroid {
  partitionType := 1,
  pieceAttrib := "name",
  method := 1  -- center of mass
} piecesGeo

-- Get bounding box centers
let boxCenters = extractCentroid {
  method := 0,  -- bbox center
  output := 0   -- points only
} myGeo
```
-/
def extractCentroid (params : ExtractCentroidParams := {}) (geo : Geometry) : Geometry :=
  sop_extractcentroid geo
    params.partitionType
    params.pieceAttrib
    params.class'
    params.method
    params.output
    params.centroidAttrib
    params.transferAttributes
    params.transferGroups

-- ============================================================================
-- Extract Contours Node
-- ============================================================================

/-- Parameters for the Extract Contours SOP -/
structure ExtractContoursParams where
  /-- Primitive group to extract contours from -/
  group : String := ""
  /-- Camera path for view-dependent extraction -/
  camPath : String := "/obj/cam1"
  /-- Enable frustum culling -/
  frustumCulling : Bool := false
  /-- Normal tolerance in degrees for silhouette detection -/
  normalTolerance : Float := 90.0
  /-- Extraction mode: 0=silhouettes, 1=creases, 2=borders, 3=all -/
  mode : Int := 0
  /-- Name of output edge group -/
  outputEdgeGroup : String := "contours"

/-- Extract contour edges (silhouettes, creases, borders) from geometry

Example:
```lean
-- Extract silhouette edges
let silhouettes = extractContours {
  mode := 0,
  camPath := "/obj/cam1",
  normalTolerance := 90.0
} myGeo

-- Extract all contour types
let allContours = extractContours {
  mode := 3
} myGeo
```
-/
def extractContours (params : ExtractContoursParams := {}) (geo : Geometry) : Geometry :=
  sop_extractcontours geo
    params.group
    params.camPath
    params.frustumCulling.toInt
    params.normalTolerance
    params.mode
    params.outputEdgeGroup

-- ============================================================================
-- Extract Transform Node
-- ============================================================================

/-- Parameters for the Extract Transform SOP -/
structure ExtractTransformParams where
  /-- Use piece attribute to match geometry pieces -/
  usePieceAttrib : Bool := true
  /-- Name of piece attribute for matching -/
  pieceAttrib : String := "name"
  /-- Piece attribute class: 0=detail, 1=primitive, 2=point -/
  pieceAttribClass : Int := 1
  /-- Extraction method: 0=point-based, 1=primitive-based -/
  extractionMethod : Int := 0
  /-- Output transform attributes: 0=matrix, 1=transform components, 2=both -/
  outputAttribs : Int := 0
  /-- Compute deformation/distortion measure -/
  computeDistortion : Bool := false
  /-- Name of distortion attribute -/
  distortionAttrib : String := "distortion"

/-- Extract transformation between two geometries
Computes the transform that takes the first input to match the second input.

Example:
```lean
-- Extract transform between rest and deformed geometry
let withTransform = extractTransform {
  usePieceAttrib := true,
  pieceAttrib := "name",
  outputAttribs := 2  -- output both matrix and components
} restGeo deformedGeo

-- Compute distortion
let withDistortion = extractTransform {
  computeDistortion := true,
  distortionAttrib := "distortion"
} geo1 geo2
```
-/
def extractTransform (params : ExtractTransformParams := {}) 
    (geo0 geo1 : Geometry) : Geometry :=
  sop_extracttransform geo0 geo1
    params.usePieceAttrib.toInt
    params.pieceAttrib
    params.pieceAttribClass
    params.extractionMethod
    params.outputAttribs
    params.computeDistortion.toInt
    params.distortionAttrib

-- ============================================================================
-- Facet Node
-- ============================================================================

/-- Parameters for the Facet SOP -/
structure FacetParams where
  /-- Primitive group to facet -/
  group : String := ""
  /-- Group type: 0=guess, 4=primitives -/
  groupType : Int := 4
  /-- Pre-compute normals before faceting -/
  preNormal : Bool := false
  /-- Make normals unit length -/
  unitNormals : Bool := false
  /-- Make unique points (no sharing between primitives) -/
  uniquePoints : Bool := false
  /-- Consolidate points within distance -/
  consolidatePoints : Bool := false
  /-- Distance threshold for consolidation -/
  distance : Float := 0.001
  /-- Use accurate (but slower) consolidation -/
  accurate : Bool := true
  /-- Remove inline points along edges -/
  removeInlinePoints : Bool := false
  /-- Distance threshold for inline point removal -/
  inlineDistance : Float := 0.001
  /-- Fix polygon winding order -/
  orientPolys : Bool := false
  /-- Add cusp normals at sharp angles -/
  cuspNormals : Bool := true
  /-- Cusp angle threshold in degrees -/
  cuspAngle : Float := 60.0
  /-- Remove primitive normals (use point normals only) -/
  removePrimNormals : Bool := false
  /-- Make primitives planar -/
  makePlanar : Bool := false
  /-- Post-compute normals after faceting -/
  postNormal : Bool := true
  /-- Reverse all normals -/
  reverseNormals : Bool := false

/-- Control point/primitive normals and geometry cleanup
A versatile node for normal computation, point consolidation, and polygon cleanup.

Example:
```lean
-- Compute vertex normals with cusps
let withNormals = facet {
  postNormal := true,
  cuspNormals := true,
  cuspAngle := 45.0
} myGeo

-- Make unique points (faceted look)
let faceted = facet {
  uniquePoints := true,
  postNormal := true
} myGeo

-- Clean up geometry
let cleaned = facet {
  consolidatePoints := true,
  distance := 0.001,
  removeInlinePoints := true,
  orientPolys := true
} messyGeo
```
-/
def facet (params : FacetParams := {}) (geo : Geometry) : Geometry :=
  sop_facet geo
    params.group
    params.groupType
    params.preNormal.toInt
    params.unitNormals.toInt
    params.uniquePoints.toInt
    params.consolidatePoints.toInt
    params.distance
    params.accurate.toInt
    params.removeInlinePoints.toInt
    params.inlineDistance
    params.orientPolys.toInt
    params.cuspNormals.toInt
    params.cuspAngle
    params.removePrimNormals.toInt
    params.makePlanar.toInt
    params.postNormal.toInt
    params.reverseNormals.toInt

-- ============================================================================
-- File Node
-- ============================================================================

/-- Parameters for the File SOP -/
structure FileParams where
  /-- File mode: 0=read, 1=write -/
  fileMode : Int := 0
  /-- File path to read from or write to -/
  file : String := ""
  /-- Object pattern filter for loading -/
  objPattern : String := "*"
  /-- Geometry data path within file -/
  geoDataPath : String := ""
  /-- Missing frame behavior: 0=no geometry, 1=report error -/
  missingFrame : Int := 0
  /-- Load type: 0=all, 1=packed, 2=points -/
  loadType : Int := 0
  /-- Viewport LOD for packed geometry -/
  viewportLod : Int := 0
  /-- Edit packed primitives in viewport -/
  packedViewerEdit : Bool := false
  /-- Keep packed primitives expanded -/
  packExpanded : Bool := false
  /-- Delay load geometry data -/
  delayLoad : Bool := false
  /-- Create directory path if it doesn't exist (for writing) -/
  mkPath : Bool := true
  /-- Cache size in MB -/
  cacheSize : Int := 100
  /-- Enable prefetching for animation -/
  prefetch : Bool := false
  /-- Frame range for sequences -/
  frameRange : Vector2 := ⟨1, 100⟩
  /-- Index for single frame loading -/
  index : Float := 1.0
  /-- Frame wrapping: 0=clamp, 1=cycle, 2=mirror -/
  wrap : Int := 0
  /-- Retry on load failure -/
  retry : Bool := false

/-- Load or save geometry files
Supports various formats including .bgeo, .obj, .fbx, .abc, etc.

Example:
```lean
-- Load geometry file
let loaded = file {
  file := "$HIP/geo/model.bgeo"
} default

-- Load with packed primitives
let packed = file {
  file := "$HIP/geo/scatter.bgeo",
  loadType := 1,  -- packed
  viewportLod := 2
} default

-- Load sequence
let sequence = file {
  file := "$HIP/geo/anim.$F4.bgeo",
  index := 10.0
} default
```
-/
def file (params : FileParams := {}) (geo : Geometry := default) : Geometry :=
  sop_file geo
    params.fileMode
    params.file
    params.objPattern
    params.geoDataPath
    params.missingFrame
    params.loadType
    params.packedViewerEdit.toInt
    params.viewportLod
    params.packExpanded.toInt
    params.delayLoad.toInt
    params.mkPath.toInt
    params.cacheSize
    params.prefetch.toInt
    params.frameRange
    params.index
    params.wrap
    params.retry.toInt

-- ============================================================================
-- Fit Node
-- ============================================================================

/-- Parameters for the Fit SOP -/
structure FitParams where
  /-- Point group to fit surface to -/
  group : String := ""
  /-- Fitting method: 0=least squares, 1=global interpolate -/
  method : Int := 0
  /-- Surface type: 0=NURBS, 1=Bezier -/
  type : Int := 0
  /-- Surface topology: 0=rows, 1=columns, 2=rows & columns, 3=triangles -/
  surfaceType : Int := 2
  /-- U direction order (degree + 1) -/
  orderU : Int := 4
  /-- V direction order (degree + 1) -/
  orderV : Int := 4
  /-- Fitting tolerance (for least squares) -/
  tolerance : Float := 0.1
  /-- Smoothing factor (0=interpolate, higher=smoother) -/
  smoothing : Float := 0.0
  /-- U direction knot multiplicity -/
  multipleU : Int := 1
  /-- V direction knot multiplicity -/
  multipleV : Int := 1
  /-- Scope: 0=fit all, 1=fit primitive -/
  scope : Int := 0
  /-- U parametrization: 0=uniform, 1=chord length, 2=centripetal -/
  dataParamU : Int := 1
  /-- V parametrization: 0=uniform, 1=chord length, 2=centripetal -/
  dataParamV : Int := 1
  /-- Close surface in U direction -/
  closeU : Bool := false
  /-- Close surface in V direction -/
  closeV : Bool := false
  /-- Include corner points -/
  corners : Bool := true

/-- Fit a spline surface (NURBS/Bezier) to points

Example:
```lean
-- Fit NURBS surface to points
let surface = fit {
  method := 0,  -- least squares
  orderU := 4,
  orderV := 4,
  tolerance := 0.1
} pointCloud

-- Interpolating surface
let interpolated = fit {
  method := 1,  -- global interpolate
  smoothing := 0.0
} points
```
-/
def fit (params : FitParams := {}) (geo : Geometry) : Geometry :=
  sop_fit geo
    params.group
    params.method
    params.type
    params.surfaceType
    params.orderU
    params.orderV
    params.tolerance
    params.smoothing
    params.multipleU
    params.multipleV
    params.scope
    params.dataParamU
    params.dataParamV
    params.closeU.toInt
    params.closeV.toInt
    params.corners.toInt

-- ============================================================================
-- Font Node
-- ============================================================================

/-- Parameters for the Font SOP -/
structure FontParams where
  /-- Output type: 0=polygon, 1=NURBS curves, 2=Bezier curves -/
  type : Int := 0
  /-- Font file path (.ttf, .otf, etc.) -/
  fontFile : String := "Helvetica Bold"
  /-- Text string to generate -/
  text : String := "Hello"
  /-- Horizontal alignment: 0=left, 1=center, 2=right -/
  hAlign : Int := 1
  /-- Vertical alignment: 0=bottom, 1=baseline, 2=middle, 3=top -/
  vAlign : Int := 2
  /-- Use descender for vertical alignment -/
  useDescender : Bool := true
  /-- Translation offset -/
  translate : Vector3 := ⟨0, 0, 0⟩
  /-- Rotation in degrees -/
  rotate : Vector3 := ⟨0, 0, 0⟩
  /-- Scale in X and Y -/
  scale : Vector2 := ⟨1, 1⟩
  /-- Font size -/
  fontSize : Float := 1.0
  /-- Character spacing (tracking) -/
  tracking : Vector2 := ⟨0, 0⟩
  /-- Enable automatic kerning -/
  autoKern : Bool := true
  /-- Oblique/italic angle in degrees -/
  oblique : Float := 0.0
  /-- Level of detail (for polygon output) -/
  lod : Float := 0.1
  /-- Add holes to characters -/
  addHoles : Bool := true
  /-- Add primitive attribute for character identification -/
  addCharAttrib : Bool := false

/-- Generate 3D text from fonts

Example:
```lean
-- Simple text
let text = font {
  text := "Hello World",
  fontSize := 2.0
}

-- Styled text
let styled = font {
  text := "HouLean",
  fontFile := "$HH/fonts/Arial.ttf",
  fontSize := 5.0,
  oblique := 15.0,
  tracking := ⟨0.1, 0⟩
}

-- NURBS curves for further processing
let curves = font {
  type := 1,  -- NURBS
  text := "Path",
  fontSize := 1.0
}
```
-/
def font (params : FontParams := {}) : Geometry :=
  sop_font
    params.type
    params.fontFile
    params.text
    params.hAlign
    params.vAlign
    params.useDescender.toInt
    params.translate
    params.rotate
    params.scale
    params.fontSize
    params.tracking
    params.autoKern.toInt
    params.oblique
    params.lod
    params.addHoles.toInt
    params.addCharAttrib.toInt

-- ============================================================================
-- Fractal Node
-- ============================================================================

/-- Parameters for the Fractal SOP -/
structure FractalParams where
  /-- Point group to apply fractal displacement to -/
  group : String := ""
  /-- Number of fractal subdivisions (octaves) -/
  divisions : Int := 3
  /-- Smoothing factor between octaves (0-1) -/
  smoothing : Float := 0.5
  /-- Overall displacement scale -/
  scale : Float := 1.0
  /-- Random seed for fractal pattern -/
  seed : Int := 0
  /-- Fix boundary points (don't displace) -/
  fixBoundary : Bool := false
  /-- Add vertex normals -/
  addVertexNormals : Bool := false
  /-- Normal attribute name to use for displacement direction -/
  normalAttrib : String := "N"
  /-- Displacement direction vector (if no normal attribute) -/
  direction : Vector3 := ⟨0, 1, 0⟩

/-- Add fractal noise displacement to geometry
Creates natural-looking terrain and organic surface detail.

Example:
```lean
-- Terrain from grid
let terrain = fractal {
  divisions := 5,
  scale := 2.0,
  smoothing := 0.6
} gridGeo

-- Organic displacement
let organic = fractal {
  divisions := 4,
  scale := 0.5,
  seed := 42,
  normalAttrib := "N"
} sphereGeo

-- Fixed boundary displacement
let bordered = fractal {
  divisions := 3,
  fixBoundary := true
} planeGeo
```
-/
def fractal (params : FractalParams := {}) (geo : Geometry) : Geometry :=
  sop_fractal geo
    params.group
    params.divisions
    params.smoothing
    params.scale
    params.seed
    params.fixBoundary.toInt
    params.addVertexNormals.toInt
    params.normalAttrib
    params.direction


-- ============================================================================
-- Solidify Node
-- ============================================================================

/-- Parameters for the Solidify SOP -/
structure SolidifyParams where
  /-- Tetrahedron group to check for solidity -/
  tetGroup : String := ""
  /-- Polygon group (surface geometry) -/
  polyGroup : String := ""
  /-- Keep input polygons in output -/
  keepPolygons : Bool := true
  /-- Boundary threshold for solid detection -/
  solidBoundary : Float := 0.0
  /-- Output solidity attribute -/
  outputSolidity : Bool := false
  /-- Name of solidity attribute -/
  solidityAttrib : String := "solidity"

/-- Determine which tetrahedra are inside solid regions

Example:
```lean
let solidified = solidify {
  tetGroup := "tets",
  keepPolygons := false,
  outputSolidity := true
} tetMesh
```
-/
def solidify (params : SolidifyParams := {}) (geo : Geometry) : Geometry :=
  sop_solidify geo
    params.tetGroup
    params.polyGroup
    params.keepPolygons.toInt
    params.solidBoundary
    params.outputSolidity.toInt
    params.solidityAttrib

-- ============================================================================
-- Sort Node
-- ============================================================================

/-- Parameters for the Sort SOP -/
structure SortParams where
  /-- Point group to sort -/
  ptGroup : String := ""
  /-- Point sort mode: 0=no change, 1=by X, 2=by Y, 3=by Z, 4=shift, 5=reverse, 6=random, 7=proximity, 8=vertex order, 9=by attribute, 10=by expression -/
  ptSort : Int := 0
  /-- Random seed for point sorting -/
  pointSeed : Int := 0
  /-- Shift offset for point sorting -/
  pointOffset : Int := 0
  /-- Proximity position for point sorting -/
  pointProximity : Vector3 := ⟨0, 0, 0⟩
  /-- Object path for proximity reference -/
  pointObjPath : String := ""
  /-- Direction vector for directional sorting -/
  pointDirection : Vector3 := ⟨0, 1, 0⟩
  /-- Expression value for point sorting -/
  pointExpr : Float := 0.0
  /-- Attribute name for attribute-based sorting -/
  pointAttrib : String := ""
  /-- Attribute component to sort by -/
  pointAttribComp : Int := 0
  /-- Point order string (space-separated indices) -/
  pointOrder : String := ""
  /-- Reverse point sort order -/
  pointReverse : Bool := false
  /-- Use explicit point indices -/
  usePointIndices : Bool := false
  /-- Point indices string -/
  pointIndices : String := ""
  /-- Combine with existing point order -/
  combinePointIndices : Bool := false
  /-- Primitive group to sort -/
  primGroup : String := ""
  /-- Primitive sort mode (same options as point sort) -/
  primSort : Int := 0
  /-- Random seed for primitive sorting -/
  primSeed : Int := 0
  /-- Shift offset for primitive sorting -/
  primOffset : Int := 0
  /-- Proximity position for primitive sorting -/
  primProximity : Vector3 := ⟨0, 0, 0⟩
  /-- Object path for primitive proximity reference -/
  primObjPath : String := ""
  /-- Direction vector for primitive sorting -/
  primDirection : Vector3 := ⟨0, 1, 0⟩
  /-- Expression value for primitive sorting -/
  primExpr : Float := 0.0
  /-- Attribute name for primitive attribute-based sorting -/
  primAttrib : String := ""
  /-- Primitive attribute component to sort by -/
  primAttribComp : Int := 0
  /-- Primitive order string -/
  primOrder : String := ""
  /-- Reverse primitive sort order -/
  primReverse : Bool := false
  /-- Use explicit primitive indices -/
  usePrimIndices : Bool := false
  /-- Primitive indices string -/
  primIndices : String := ""
  /-- Combine with existing primitive order -/
  combinePrimIndices : Bool := false
  /-- Vertex primitive order: 0=unchanged, 1=reverse -/
  vertexPrimOrder : Int := 0

/-- Sort points and primitives by various criteria

Example:
```lean
-- Sort points by Y coordinate
let sortedByY = sort {
  ptSort := 2  -- by Y
} myGeo

-- Random point order
let randomized = sort {
  ptSort := 6,  -- random
  pointSeed := 42
} myGeo

-- Sort by distance from origin
let byProximity = sort {
  ptSort := 7,  -- proximity
  pointProximity := ⟨0, 0, 0⟩
} myGeo

-- Sort by attribute
let byAttrib = sort {
  ptSort := 9,  -- by attribute
  pointAttrib := "id"
} myGeo
```
-/
def sort (params : SortParams := {}) (geo : Geometry) : Geometry :=
  sop_sort geo
    params.ptGroup
    params.ptSort
    params.pointSeed
    params.pointOffset
    params.pointProximity
    params.pointObjPath
    params.pointDirection
    params.pointExpr
    params.pointAttrib
    params.pointAttribComp
    params.pointOrder
    params.pointReverse.toInt
    params.usePointIndices.toInt
    params.pointIndices
    params.combinePointIndices.toInt
    params.primGroup
    params.primSort
    params.primSeed
    params.primOffset
    params.primProximity
    params.primObjPath
    params.primDirection
    params.primExpr
    params.primAttrib
    params.primAttribComp
    params.primOrder
    params.primReverse.toInt
    params.usePrimIndices.toInt
    params.primIndices
    params.combinePrimIndices.toInt
    params.vertexPrimOrder

-- ============================================================================
-- Split Points Node
-- ============================================================================

/-- Parameters for the Split Points SOP -/
structure SplitPointsParams where
  /-- Group of points to potentially split -/
  group : String := ""
  /-- Group type: 0=guess, 3=points -/
  groupType : Int := 3
  /-- Use attribute to determine splitting -/
  useAttribute : Bool := false
  /-- Attribute name to compare for splitting -/
  attributeName : String := "N"
  /-- Tolerance for attribute comparison -/
  tolerance : Float := 0.0001
  /-- Promote vertex attributes to points after splitting -/
  promoteAttributes : Bool := false

/-- Split shared points into unique points per primitive
Useful for creating hard edges or preventing attribute blending.

Example:
```lean
-- Split all points (hard edges everywhere)
let hardEdges = splitPoints {} myGeo

-- Split based on normal difference
let splitByNormal = splitPoints {
  useAttribute := true,
  attributeName := "N",
  tolerance := 0.01
} myGeo

-- Split specific group
let splitGroup = splitPoints {
  group := "seam_points"
} myGeo
```
-/
def splitPoints (params : SplitPointsParams := {}) (geo : Geometry) : Geometry :=
  sop_splitpoints geo
    params.group
    params.groupType
    params.useAttribute.toInt
    params.attributeName
    params.tolerance
    params.promoteAttributes.toInt

-- ============================================================================
-- Stash Node
-- ============================================================================

/-- Parameters for the Stash SOP -/
structure StashParams where
  /-- Stashed data item -/
  stash : DataItem := default
  /-- File path for stashed data -/
  stashFile : String := ""

/-- Store geometry in a cached state for later retrieval

Example:
```lean
let stashed = stash {
  stashFile := "$HIP/cache/stash.bgeo"
} myGeo
```
-/
def stash (params : StashParams := {}) (geo : Geometry) : Geometry :=
  sop_stash geo params.stash params.stashFile

-- ============================================================================
-- Surface Splat Node
-- ============================================================================

/-- Parameters for the Surface Splat SOP -/
structure SurfaceSplatParams where
  /-- Mask attribute binding -/
  bindMask : String := "mask"
  /-- Negate mask values -/
  negateMask : Bool := false
  /-- Width attribute binding -/
  bindWidth : String := "width"
  /-- Alpha attribute binding -/
  bindAlpha : String := "alpha"
  /-- Soft edge attribute binding -/
  bindSoftEdge : String := "softedge"
  /-- Hit attribute binding (stores if splat hit surface) -/
  bindHit : String := "hit"
  /-- Hit primitive attribute binding -/
  bindHitPrim : String := "hitprim"
  /-- Hit UV attribute binding -/
  bindHitUV : String := "hituv"

/-- Project and splat attributes from points onto a surface
Used for painting, texture projection, and attribute transfer.

Example:
```lean
-- Simple surface splatting
let splatted = surfaceSplat {
  bindWidth := "pscale",
  bindAlpha := "Alpha"
} surfaceGeo splatPointsGeo

-- With hit detection
let withHits = surfaceSplat {
  bindHit := "hit",
  bindHitPrim := "hitprim"
} surfaceGeo pointsGeo
```
-/
def surfaceSplat (params : SurfaceSplatParams := {}) 
    (surface splat : Geometry) : Geometry :=
  sop_surfacesplat surface splat
    params.bindMask
    params.negateMask.toInt
    params.bindWidth
    params.bindAlpha
    params.bindSoftEdge
    params.bindHit
    params.bindHitPrim
    params.bindHitUV

-- ============================================================================
-- Sweep Node
-- ============================================================================

/-- Parameters for the Sweep SOP (version 2.0) -/
structure SweepParams where
  /-- Curve group to use as backbone -/
  curveGroup : String := ""
  /-- Cross-section group -/
  crossSectionGroup : String := ""
  /-- Surface shape: 0=polygon, 1=NURBS, 2=Bezier -/
  surfaceShape : Int := 0
  /-- Surface type: varies by shape -/
  surfaceType : Int := 0
  /-- Global scale factor -/
  scale : Float := 1.0
  /-- Number of columns (divisions along backbone) -/
  cols : Int := 10
  /-- Radius for circular cross-section -/
  radius : Float := 0.1
  /-- Width for rectangular cross-section -/
  width : Float := 1.0
  /-- Reverse cross-section orientation -/
  reverseCrossSections : Bool := false
  /-- Stretch cross-section around tight curves -/
  stretchAroundTurns : Bool := false
  /-- Maximum stretch factor -/
  maxStretchAroundTurns : Float := 2.0
  /-- End cap type: 0=none, 1=flat, 2=rounded -/
  endCapType : Int := 0
  /-- Number of divisions in end caps -/
  capDivs : Int := 4
  /-- Use triangular poles in rounded caps -/
  triangularPoles : Bool := false
  /-- End cap scale factor -/
  capScale : Float := 1.0
  /-- End cap roundness (for rounded caps) -/
  capRoundness : Float := 1.0
  /-- Create group for end caps -/
  addEndCapsGroup : Bool := false
  /-- Name of end caps group -/
  endCapsGroup : String := "caps"
  /-- Apply scale ramp along backbone -/
  applyScale : Bool := false
  /-- Scale ramp curve -/
  scaleRamp : FloatRamp := default
  /-- Rotation order: 0=XYZ, 1=XZY, 2=YXZ, 3=YZX, 4=ZXY, 5=ZYX -/
  rotationOrder : Int := 0
  /-- Apply roll (twist) along backbone -/
  applyRoll : Bool := false
  /-- Initial roll angle in degrees -/
  roll : Float := 0.0
  /-- Number of full twists -/
  fullTwists : Int := 0
  /-- Incremental roll per segment -/
  incrementalRoll : Float := 0.0
  /-- Roll increment type: 0=per segment, 1=per unit length -/
  rollPer : Int := 0
  /-- Roll attribute name -/
  rollAttrib : String := "roll"
  /-- Apply yaw rotation -/
  applyYaw : Bool := false
  /-- Yaw angle in degrees -/
  yaw : Float := 0.0
  /-- Incremental yaw per segment -/
  incrementalYaw : Float := 0.0
  /-- Yaw increment type -/
  yawPer : Int := 0
  /-- Yaw attribute name -/
  yawAttrib : String := "yaw"
  /-- Apply pitch rotation -/
  applyPitch : Bool := false
  /-- Pitch angle in degrees -/
  pitch : Float := 0.0
  /-- Incremental pitch per segment -/
  incrementalPitch : Float := 0.0
  /-- Pitch increment type -/
  pitchPer : Int := 0
  /-- Pitch attribute name -/
  pitchAttrib : String := "pitch"
  /-- Copy order: 0=cross-section then curve, 1=curve then cross-section -/
  copyOrder : Int := 0
  /-- Cross-section identification attribute -/
  crossSectionAttrib : String := ""
  /-- Primitive type for output -/
  primType : Int := 0
  /-- Unroll closed surfaces -/
  unrollClosedRowCol : Bool := false
  /-- Swap row and column directions -/
  swapRowCol : Bool := false
  /-- Close surface if no curve input -/
  closeIfNoCurveInput : Bool := false
  /-- Tangent calculation type -/
  tangentType : Int := 0
  /-- Continuous closed curves -/
  continuousClosed : Bool := true
  /-- Extrapolate end tangents -/
  extrapolateEndTangents : Bool := false
  /-- Transform by attributes on backbone -/
  transformByAttribs : Bool := false
  /-- Compute UV coordinates -/
  computeUVs : Bool := true
  /-- Override existing UVs -/
  overrideExistingUVs : Bool := false
  /-- Use length-weighted UVs -/
  lengthWeightedUVs : Bool := true
  /-- Normalize U coordinates to 0-1 -/
  normalizeU : Bool := true
  /-- Normalize V coordinates to 0-1 -/
  normalizeV : Bool := true
  /-- Flip U direction -/
  flipU : Bool := false
  /-- UV scale factors -/
  uvScale : Vector2 := ⟨1, 1⟩
  /-- Use mesh edge lengths for UVs -/
  useMeshEdgeLengths : Bool := false
  /-- Proportional scale per curve -/
  propScalePerCurve : Bool := false
  /-- Wrap U coordinates -/
  wrapU : Bool := false
  /-- Wrap V coordinates -/
  wrapV : Bool := false
  /-- Attributes to transfer from backbone -/
  attribsFromBackbone : String := ""
  /-- Attributes to transfer from cross-section -/
  attribsFromCrossSection : String := ""
  /-- Add point row attribute -/
  addPtRow : Bool := false
  /-- Point row attribute name -/
  ptRowAttrib : String := "row"
  /-- Add point column attribute -/
  addPtCol : Bool := false
  /-- Point column attribute name -/
  ptColAttrib : String := "col"
  /-- Add primitive row attribute -/
  addPrimRow : Bool := false
  /-- Primitive row attribute name -/
  primRowAttrib : String := "row"
  /-- Add primitive column attribute -/
  addPrimCol : Bool := false
  /-- Primitive column attribute name -/
  primColAttrib : String := "col"
  /-- Add cross-section number attribute -/
  addCrossSectionNum : Bool := false
  /-- Cross-section number attribute name -/
  crossSectionNumAttrib : String := "crosssection"
  /-- Add curve number attribute -/
  addCurveNum : Bool := false
  /-- Curve number attribute name -/
  curveNumAttrib : String := "curve"
  /-- Up vector type: 0=vector, 1=attribute, 2=curve normal -/
  upVectorType : Int := 0
  /-- Use up vector at start -/
  upVectorAtStart : Bool := true
  /-- Use different end up vector -/
  useEndUpVector : Bool := false
  /-- Up vector attribute name -/
  upVectorAttrib : String := "up"
  /-- End up vector attribute name -/
  endUpVectorAttrib : String := "up"
  /-- Up vector direction -/
  upVector : Vector3 := ⟨0, 1, 0⟩
  /-- End up vector direction -/
  endUpVector : Vector3 := ⟨0, 1, 0⟩

/-- Sweep a cross-section along a backbone curve
Creates surfaces like tubes, pipes, extrusions, and complex swept forms.

Example:
```lean
-- Simple tube sweep
let tube = sweep {
  cols := 20,
  radius := 0.5,
  computeUVs := true
} backboneCurve circleCrossSection

-- Twisted sweep
let twisted = sweep {
  cols := 30,
  applyRoll := true,
  fullTwists := 2
} curve crossSection

-- With end caps
let capped = sweep {
  endCapType := 1,  -- flat caps
  addEndCapsGroup := true,
  endCapsGroup := "caps"
} curve crossSection
```
-/
def sweep (params : SweepParams := {}) 
    (backbone crossSection : Geometry) : Geometry :=
  sop_sweep_2_0 backbone crossSection
    params.curveGroup
    params.crossSectionGroup
    params.surfaceShape
    params.surfaceType
    params.scale
    params.cols
    params.radius
    params.width
    params.reverseCrossSections.toInt
    params.stretchAroundTurns.toInt
    params.maxStretchAroundTurns
    params.endCapType
    params.capDivs
    params.triangularPoles.toInt
    params.capScale
    params.capRoundness
    params.addEndCapsGroup.toInt
    params.endCapsGroup
    params.applyScale.toInt
    params.scaleRamp
    params.rotationOrder
    params.applyRoll.toInt
    params.roll
    params.fullTwists
    params.incrementalRoll
    params.rollPer
    params.rollAttrib
    params.applyYaw.toInt
    params.yaw
    params.incrementalYaw
    params.yawPer
    params.yawAttrib
    params.applyPitch.toInt
    params.pitch
    params.incrementalPitch
    params.pitchPer
    params.pitchAttrib
    params.copyOrder
    params.crossSectionAttrib
    params.primType
    params.unrollClosedRowCol.toInt
    params.swapRowCol.toInt
    params.closeIfNoCurveInput.toInt
    params.tangentType
    params.continuousClosed.toInt
    params.extrapolateEndTangents.toInt
    params.transformByAttribs.toInt
    params.computeUVs.toInt
    params.overrideExistingUVs.toInt
    params.lengthWeightedUVs.toInt
    params.normalizeU.toInt
    params.normalizeV.toInt
    params.flipU.toInt
    params.uvScale
    params.useMeshEdgeLengths.toInt
    params.propScalePerCurve.toInt
    params.wrapU.toInt
    params.wrapV.toInt
    params.attribsFromBackbone
    params.attribsFromCrossSection
    params.addPtRow.toInt
    params.ptRowAttrib
    params.addPtCol.toInt
    params.ptColAttrib
    params.addPrimRow.toInt
    params.primRowAttrib
    params.addPrimCol.toInt
    params.primColAttrib
    params.addCrossSectionNum.toInt
    params.crossSectionNumAttrib
    params.addCurveNum.toInt
    params.curveNumAttrib
    params.upVectorType
    params.upVectorAtStart.toInt
    params.useEndUpVector.toInt
    params.upVectorAttrib
    params.endUpVectorAttrib
    params.upVector
    params.endUpVector

-- ============================================================================
-- Switch Node
-- ============================================================================

/-- Parameters for the Switch SOP -/
structure SwitchParams where
  /-- Array of auxiliary geometries to choose from -/
  auxGeo : GeometryArray := default
  /-- Index of input to select (0-based) -/
  input : Int := 0

/-- Select one geometry from multiple inputs

Example:
```lean
let selected = switch {
  auxGeo := #[geo1, geo2, geo3],
  input := 1  -- select geo2
} geo0
```
-/
def switch (params : SwitchParams := {}) (geo : Geometry) : Geometry :=
  sop_switch geo params.auxGeo params.input

-- ============================================================================
-- Switch If Node
-- ============================================================================

/-- Parameters for the Switch If SOP -/
structure SwitchIfParams where
  /-- Merge condition: 0=use test results, 1=always merge -/
  mergeCondition : Int := 0
  /-- Test input index -/
  testInput : Int := 0
  /-- Array of test conditions -/
  tests : DictArray := default

/-- Conditionally merge geometry based on tests

Example:
```lean
let conditional = switchIf {
  mergeCondition := 0,
  testInput := 0
} geo0 geo1
```
-/
def switchIf (params : SwitchIfParams := {}) 
    (geo0 geo1 : Geometry) : Geometry :=
  sop_switchif geo0 geo1
    params.mergeCondition
    params.testInput
    params.tests

-- ============================================================================
-- Tetrahedralize Node
-- ============================================================================

/-- Parameters for the Tetrahedralize SOP -/
structure TetrahedralizeParams where
  /-- Primitive group to tetrahedralize -/
  group : String := ""
  /-- Process geometry in batches by piece -/
  batch : Bool := false
  /-- Piece attribute for batching -/
  pieceAttrib : String := "name"
  /-- Remove input geometry -/
  removeInput : Bool := true
  /-- Tetrahedralization mode: 0=solid, 1=surface -/
  mode : Int := 0
  /-- Output type: 0=tets, 1=surface -/
  output : Int := 0
  /-- Keep input primitives -/
  keepPrims : Bool := false
  /-- No boundary modification -/
  noBoundaryModification : Bool := false
  /-- One face per tet (for attributes) -/
  oneFacePerTet : Bool := false
  /-- Propagate normals to tets -/
  propagateNormals : Bool := false
  /-- Add interior/exterior attribute -/
  interiorAttrib : Bool := false
  /-- Use quality constraints -/
  useQuality : Bool := false
  /-- Radius-edge tolerance for quality -/
  radiusEdgeTolerance : Float := 2.0
  /-- Minimum dihedral angle in degrees -/
  minDihedralAngle : Float := 0.0
  /-- Use target size attribute -/
  useTargetSizeAttrib : Bool := false
  /-- Target size attribute name -/
  targetSizeAttrib : String := "targetsize"
  /-- Use uniform max size -/
  useUniformMaxSize : Bool := false
  /-- Uniform maximum tet size -/
  uniformMaxSize : Float := 1.0
  /-- Use max size attribute -/
  useMaxSizeAttrib : Bool := false
  /-- Max size attribute name -/
  maxSizeAttrib : String := "maxsize"
  /-- Use maximum iterations -/
  useMaxIter : Bool := false
  /-- Maximum refinement iterations -/
  maxIter : Int := 1000
  /-- Use maximum Steiner points -/
  useMaxSteiner : Bool := false
  /-- Maximum Steiner points to add -/
  maxSteiner : Int := 10000
  /-- Optimization iterations -/
  optIterations : Int := 0
  /-- Optimize edge/face flips -/
  optEdgeFace : Bool := false
  /-- Optimize vertex smoothing -/
  optVertexSmooth : Bool := false
  /-- Optimize vertex modification -/
  optVertexMod : Bool := false
  /-- Use intersection color -/
  useIntersectionColor : Bool := false
  /-- Intersection polygon color -/
  intersectionPolyColor : Vector3 := ⟨1, 0, 0⟩
  /-- Use intersection group -/
  useIntersectionGroup : Bool := false
  /-- Intersection polygon group -/
  intersectionPolyGroup : String := "intersect"
  /-- Track failures -/
  trackFailures : Bool := false
  /-- Random seed for tetrahedralization -/
  randomSeed : Int := 0
  /-- Precision tolerance -/
  precisionTolerance : Float := 1e-8
  /-- Dihedral angle tolerance -/
  dihedralAngleTolerance : Float := 0.0
  /-- Maximum attempts -/
  maxAttempts : Int := 10
  /-- Use invalid primitive color -/
  useInvalidColor : Bool := false
  /-- Invalid primitive color -/
  invalidPrimColor : Vector3 := ⟨1, 0, 1⟩
  /-- Use invalid primitive group -/
  useInvalidGroup : Bool := false
  /-- Invalid primitive group -/
  invalidPrimGroup : String := "invalid"

/-- Convert geometry into tetrahedral mesh
Used for FEM simulation, volume meshing, and solid object representation.

Example:
```lean
-- Simple tetrahedralization
let tets = tetrahedralize {
  mode := 0,  -- solid
  output := 0  -- tets
} surfaceGeo

-- With quality constraints
let qualityTets = tetrahedralize {
  useQuality := true,
  radiusEdgeTolerance := 2.0,
  minDihedralAngle := 10.0
} surfaceGeo refGeo

-- Size-controlled tetrahedralization
let sizedTets = tetrahedralize {
  useUniformMaxSize := true,
  uniformMaxSize := 0.1,
  optIterations := 10
} surfaceGeo refGeo
```
-/
def tetrahedralize (params : TetrahedralizeParams := {}) 
    (geo refGeo : Geometry := default) : Geometry :=
  sop_tetrahedralize geo refGeo
    params.group
    params.batch.toInt
    params.pieceAttrib
    params.removeInput.toInt
    params.mode
    params.output
    params.keepPrims.toInt
    params.noBoundaryModification.toInt
    params.oneFacePerTet.toInt
    params.propagateNormals.toInt
    params.interiorAttrib.toInt
    params.useQuality.toInt
    params.radiusEdgeTolerance
    params.minDihedralAngle
    params.useTargetSizeAttrib.toInt
    params.targetSizeAttrib
    params.useUniformMaxSize.toInt
    params.uniformMaxSize
    params.useMaxSizeAttrib.toInt
    params.maxSizeAttrib
    params.useMaxIter.toInt
    params.maxIter
    params.useMaxSteiner.toInt
    params.maxSteiner
    params.optIterations
    params.optEdgeFace.toInt
    params.optVertexSmooth.toInt
    params.optVertexMod.toInt
    params.useIntersectionColor.toInt
    params.intersectionPolyColor
    params.useIntersectionGroup.toInt
    params.intersectionPolyGroup
    params.trackFailures.toInt
    params.randomSeed
    params.precisionTolerance
    params.dihedralAngleTolerance
    params.maxAttempts
    params.useInvalidColor.toInt
    params.invalidPrimColor
    params.useInvalidGroup.toInt
    params.invalidPrimGroup

-- ============================================================================
-- Tetrasurface Node
-- ============================================================================

/-- Parameters for the Tetrasurface SOP -/
structure TetrasurfaceParams where
  /-- Keep interior primitives -/
  keepPrimitives : Bool := false
  /-- Keep interior points -/
  keepPoints : Bool := false
  /-- Build output as polygon soup -/
  buildPolySoup : Bool := false

/-- Extract surface from tetrahedral mesh

Example:
```lean
-- Extract surface from tets
let surface = tetrasurface {} tetMesh

-- Keep interior for visualization
let withInterior = tetrasurface {
  keepPrimitives := true
} tetMesh
```
-/
def tetrasurface (params : TetrasurfaceParams := {}) (geo : Geometry) : Geometry :=
  sop_tetrasurface geo
    params.keepPrimitives.toInt
    params.keepPoints.toInt
    params.buildPolySoup.toInt

-- ============================================================================
-- Texture Node
-- ============================================================================

/-- Parameters for the Texture SOP -/
structure TextureParams where
  /-- UV attribute name -/
  uvAttrib : String := "uv"
  /-- Primitive group to apply texturing to -/
  group : String := ""
  /-- Projection type: 0=perspective, 1=orthographic, 2=polar, 3=cylindrical, 4=spherical -/
  type : Int := 0
  /-- Projection axis: 0=X, 1=Y, 2=Z -/
  axis : Int := 2
  /-- Camera path for perspective projection -/
  camPath : String := ""
  /-- Coordinate space: 0=object, 1=world -/
  coordinateSpace : Int := 0
  /-- Scale factors for UV coordinates -/
  scale : Vector3 := ⟨1, 1, 1⟩
  /-- Offset for UV coordinates -/
  offset : Vector3 := ⟨0, 0, 0⟩
  /-- Rotation angle in degrees -/
  angle : Float := 0.0
  /-- Fix UV seams -/
  fixSeams : Bool := false

/-- Generate or modify UV texture coordinates

Example:
```lean
-- Perspective projection from camera
let withUVs = texture {
  type := 0,  -- perspective
  camPath := "/obj/cam1"
} myGeo

-- Cylindrical mapping
let cylindrical = texture {
  type := 3,  -- cylindrical
  axis := 1,  -- Y axis
  scale := ⟨1, 2, 1⟩
} myGeo

-- Orthographic projection
let ortho = texture {
  type := 1,  -- orthographic
  axis := 2,  -- Z axis
  fixSeams := true
} myGeo
```
-/
def texture (params : TextureParams := {}) (geo : Geometry) : Geometry :=
  sop_texture geo
    params.uvAttrib
    params.group
    params.type
    params.axis
    params.camPath
    params.coordinateSpace
    params.scale
    params.offset
    params.angle
    params.fixSeams.toInt


-- ============================================================================
-- Convenience Namespaces
-- ============================================================================

end SOP

namespace Geometry
open SOP

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

abbrev error (params : ErrorParams := {}) (geo : Geometry) : Geometry :=
  SOP.error params geo

abbrev extractCentroid (params : ExtractCentroidParams := {}) (geo : Geometry) : Geometry :=
  SOP.extractCentroid params geo

abbrev extractContours (params : ExtractContoursParams := {}) (geo : Geometry) : Geometry :=
  SOP.extractContours params geo

abbrev extractTransform (params : ExtractTransformParams := {}) 
    (geo0 geo1 : Geometry) : Geometry :=
  SOP.extractTransform params geo0 geo1

abbrev facet (params : FacetParams := {}) (geo : Geometry) : Geometry :=
  SOP.facet params geo

abbrev file (params : FileParams := {}) (geo : Geometry := default) : Geometry :=
  SOP.file params geo

abbrev fit (params : FitParams := {}) (geo : Geometry) : Geometry :=
  SOP.fit params geo

abbrev font (params : FontParams := {}) : Geometry :=
  SOP.font params

abbrev fractal (params : FractalParams := {}) (geo : Geometry) : Geometry :=
  SOP.fractal params geo

abbrev solidify (params : SolidifyParams := {}) (geo : Geometry) : Geometry :=
  SOP.solidify params geo

abbrev sort (params : SortParams := {}) (geo : Geometry) : Geometry :=
  SOP.sort params geo

abbrev splitPoints (params : SplitPointsParams := {}) (geo : Geometry) : Geometry :=
  SOP.splitPoints params geo

abbrev stash (params : StashParams := {}) (geo : Geometry) : Geometry :=
  SOP.stash params geo

abbrev surfaceSplat (params : SurfaceSplatParams := {}) 
    (surface splat : Geometry) : Geometry :=
  SOP.surfaceSplat params surface splat

abbrev sweep (params : SweepParams := {}) 
    (backbone crossSection : Geometry) : Geometry :=
  SOP.sweep params backbone crossSection

abbrev switch (params : SwitchParams := {}) (geo : Geometry) : Geometry :=
  SOP.switch params geo

abbrev switchIf (params : SwitchIfParams := {}) 
    (geo0 geo1 : Geometry) : Geometry :=
  SOP.switchIf params geo0 geo1

abbrev tetrahedralize (params : TetrahedralizeParams := {}) 
    (geo refGeo : Geometry := default) : Geometry :=
  SOP.tetrahedralize params geo refGeo

abbrev tetrasurface (params : TetrasurfaceParams := {}) (geo : Geometry) : Geometry :=
  SOP.tetrasurface params geo

abbrev texture (params : TextureParams := {}) (geo : Geometry) : Geometry :=
  SOP.texture params geo

end Geometry

end HouLean.Apex
