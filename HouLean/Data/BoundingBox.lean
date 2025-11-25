import HouLean.Math
import HouLean.Data.Defs
import HouLean.Data.Vector3
import HouLean.Data.Matrix3
import HouLean.Data.Matrix4
import HouLean.Data.Float
import HouLean.Data.RigidTransform

open HouLean.Math

namespace HouLean.BoundingBox

-- ============================================================================
-- Construction
-- ============================================================================

def empty : BoundingBox := ⟨⟨0, 0, 0⟩, ⟨0, 0, 0⟩⟩

def unit : BoundingBox := ⟨⟨1, 1, 1⟩, ⟨0, 0, 0⟩⟩

def fromMinMax (min max : Vector3) : BoundingBox :=
  let center := 0.5 * (min + max)
  let size := max - min
  ⟨size, center⟩

def fromCenterSize (center size : Vector3) : BoundingBox :=
  ⟨size, center⟩

def fromCenterExtents (center extents : Vector3) : BoundingBox :=
  ⟨2.0 * extents, center⟩

def fromPoint (p : Vector3) : BoundingBox :=
  ⟨⟨0, 0, 0⟩, p⟩

def fromPoints (points : Array Vector3) : BoundingBox :=
  if points.isEmpty then empty
  else
    let min := points.foldl Vector3.min points[0]!
    let max := points.foldl Vector3.max points[0]!
    fromMinMax min max

def fromSphere (center : Vector3) (radius : Float) : BoundingBox :=
  let size := ⟨2 * radius, 2 * radius, 2 * radius⟩
  ⟨size, center⟩

-- ============================================================================
-- Queries
-- ============================================================================

/-- Get minimum corner -/
def min (bb : BoundingBox) : Vector3 :=
  bb.center - 0.5 * bb.size

/-- Get maximum corner -/
def max (bb : BoundingBox) : Vector3 :=
  bb.center + 0.5 * bb.size

/-- Get extents (half-size) -/
def extents (bb : BoundingBox) : Vector3 :=
  0.5 * bb.size

/-- Get the 8 corner points -/
def corners (bb : BoundingBox) : Array Vector3 :=
  let e := bb.extents
  let c := bb.center
  #[c + ⟨-e.x, -e.y, -e.z⟩,
    c + ⟨ e.x, -e.y, -e.z⟩,
    c + ⟨-e.x,  e.y, -e.z⟩,
    c + ⟨ e.x,  e.y, -e.z⟩,
    c + ⟨-e.x, -e.y,  e.z⟩,
    c + ⟨ e.x, -e.y,  e.z⟩,
    c + ⟨-e.x,  e.y,  e.z⟩,
    c + ⟨ e.x,  e.y,  e.z⟩]

/-- Volume of the bounding box -/
def volume (bb : BoundingBox) : Float :=
  let s := bb.size
  s.x * s.y * s.z

/-- Surface area -/
def surfaceArea (bb : BoundingBox) : Float :=
  let s := bb.size
  2.0 * (s.x * s.y + s.y * s.z + s.z * s.x)

/-- Diagonal length -/
def diagonal (bb : BoundingBox) : Float :=
  bb.size.length

/-- Longest side -/
def maxDimension (bb : BoundingBox) : Float :=
  Float.max bb.size.x (Float.max bb.size.y bb.size.z)

/-- Shortest side -/
def minDimension (bb : BoundingBox) : Float :=
  Float.min bb.size.x (Float.min bb.size.y bb.size.z)

/-- Index of longest axis (0=x, 1=y, 2=z) -/
def maxAxis (bb : BoundingBox) : Nat :=
  if bb.size.x >= bb.size.y && bb.size.x >= bb.size.z then 0
  else if bb.size.y >= bb.size.z then 1
  else 2

/-- Is the bounding box empty (zero volume)? -/
def isEmpty (bb : BoundingBox) : Bool :=
  bb.size.x <= 0 || bb.size.y <= 0 || bb.size.z <= 0

/-- Is the bounding box valid (all positive dimensions)? -/
def isValid (bb : BoundingBox) : Bool :=
  bb.size.x >= 0 && bb.size.y >= 0 && bb.size.z >= 0

-- ============================================================================
-- Containment tests
-- ============================================================================

/-- Does the box contain a point? -/
def contains (bb : BoundingBox) (p : Vector3) : Bool :=
  p.insideBox bb.min bb.max

/-- Does this box fully contain another box? -/
def containsBox (bb other : BoundingBox) : Bool :=
  let minA := bb.min
  let maxA := bb.max
  let minB := other.min
  let maxB := other.max
  minB.x >= minA.x && maxB.x <= maxA.x &&
  minB.y >= minA.y && maxB.y <= maxA.y &&
  minB.z >= minA.z && maxB.z <= maxA.z

/-- Does the box contain a sphere? -/
def containsSphere (bb : BoundingBox) (center : Vector3) (radius : Float) : Bool :=
  let minPt := bb.min
  let maxPt := bb.max
  center.x - radius >= minPt.x && center.x + radius <= maxPt.x &&
  center.y - radius >= minPt.y && center.y + radius <= maxPt.y &&
  center.z - radius >= minPt.z && center.z + radius <= maxPt.z

-- ============================================================================
-- Intersection tests
-- ============================================================================

/-- Do two bounding boxes overlap? -/
def intersects (bb other : BoundingBox) : Bool :=
  let minA := bb.min
  let maxA := bb.max
  let minB := other.min
  let maxB := other.max
  minA.x <= maxB.x && maxA.x >= minB.x &&
  minA.y <= maxB.y && maxA.y >= minB.y &&
  minA.z <= maxB.z && maxA.z >= minB.z

/-- Does the box intersect with a sphere? -/
def intersectsSphere (bb : BoundingBox) (center : Vector3) (radius : Float) : Bool :=
  let closest := clamp center bb.min bb.max
  distance2 center closest <= radius * radius

/-- Compute intersection of two bounding boxes -/
def intersection (bb other : BoundingBox) : Option BoundingBox :=
  let minA := bb.min
  let maxA := bb.max
  let minB := other.min
  let maxB := other.max
  let minInter := Vector3.max minA minB
  let maxInter := Vector3.min maxA maxB
  if minInter.x <= maxInter.x && minInter.y <= maxInter.y && minInter.z <= maxInter.z then
    some (fromMinMax minInter maxInter)
  else
    none

-- ============================================================================
-- Distance queries
-- ============================================================================

/-- Closest point on or in the box to a given point -/
def closestPoint (bb : BoundingBox) (p : Vector3) : Vector3 :=
  clamp p bb.min bb.max

/-- Squared distance from point to box (0 if point is inside) -/
def distanceSquared (bb : BoundingBox) (p : Vector3) : Float :=
  distance2 p (bb.closestPoint p)

/-- Distance from point to box (0 if point is inside) -/
def distance (bb : BoundingBox) (p : Vector3) : Float :=
  Float.sqrt (bb.distanceSquared p)

-- ============================================================================
-- Combination operations
-- ============================================================================

/-- Union of two bounding boxes (smallest box containing both) -/
def union (bb other : BoundingBox) : BoundingBox :=
  if bb.isEmpty then other
  else if other.isEmpty then bb
  else
    let minUnion := Vector3.min bb.min other.min
    let maxUnion := Vector3.max bb.max other.max
    fromMinMax minUnion maxUnion

/-- Expand box to include a point -/
def expandByPoint (bb : BoundingBox) (p : Vector3) : BoundingBox :=
  if bb.isEmpty then fromPoint p
  else
    let newMin := Vector3.min bb.min p
    let newMax := Vector3.max bb.max p
    fromMinMax newMin newMax

/-- Expand box by a margin in all directions -/
def expand (bb : BoundingBox) (margin : Float) : BoundingBox :=
  ⟨bb.size + ⟨2 * margin, 2 * margin, 2 * margin⟩, bb.center⟩

/-- Expand box by different margins per axis -/
def expandPerAxis (bb : BoundingBox) (margins : Vector3) : BoundingBox :=
  ⟨bb.size + 2.0 * margins, bb.center⟩

/-- Scale box around its center -/
def scale (bb : BoundingBox) (s : Float) : BoundingBox :=
  ⟨s * bb.size, bb.center⟩

/-- Scale box by different factors per axis -/
def scalePerAxis (bb : BoundingBox) (s : Vector3) : BoundingBox :=
  ⟨bb.size.compMul s, bb.center⟩

-- ============================================================================
-- Transformation
-- ============================================================================

/-- Transform bounding box by a rigid transformation -/
def transform (bb : BoundingBox) (xform : RigidTransform) : BoundingBox :=
  let corners := bb.corners
  let transformedCorners := corners.map (xform.transformPoint ·)
  fromPoints transformedCorners

-- /-- Transform bounding box by a 4x4 matrix -/
-- def transformByMatrix (bb : BoundingBox) (m : Matrix4) : BoundingBox :=
--   let corners := bb.corners
--   let transformedCorners := corners.map (fun p =>
--     let p4 : Vector4 := ⟨p.x, p.y, p.z, 1.0⟩
--     let result : Vector4 := m * p4
--     ⟨result.x, result.y, result.z⟩)
--   fromPoints transformedCorners

/-- Translate bounding box -/
def translate (bb : BoundingBox) (offset : Vector3) : BoundingBox :=
  ⟨bb.size, bb.center + offset⟩

-- ============================================================================
-- Comparison operations
-- ============================================================================

defun beq (a b : BoundingBox) : Bool :=
  a.size.beq b.size && a.center.beq b.center

instance : BEq BoundingBox := ⟨beq⟩

-- /-- Approximately equal with tolerance -/
-- def approxEq (a b : BoundingBox) (tolerance : Float := 0.0001) : Bool :=
--   distance a.center b.center <= tolerance &&
--   distance a.size b.size <= tolerance

-- ============================================================================
-- Subdivision
-- ============================================================================

/-- Split box into 8 octants -/
def subdivide (bb : BoundingBox) : Array BoundingBox :=
  let halfSize := 0.5 * bb.size
  let quarterSize := 0.25 * bb.size
  let c := bb.center
  #[⟨halfSize, c + ⟨-quarterSize.x, -quarterSize.y, -quarterSize.z⟩⟩,
    ⟨halfSize, c + ⟨ quarterSize.x, -quarterSize.y, -quarterSize.z⟩⟩,
    ⟨halfSize, c + ⟨-quarterSize.x,  quarterSize.y, -quarterSize.z⟩⟩,
    ⟨halfSize, c + ⟨ quarterSize.x,  quarterSize.y, -quarterSize.z⟩⟩,
    ⟨halfSize, c + ⟨-quarterSize.x, -quarterSize.y,  quarterSize.z⟩⟩,
    ⟨halfSize, c + ⟨ quarterSize.x, -quarterSize.y,  quarterSize.z⟩⟩,
    ⟨halfSize, c + ⟨-quarterSize.x,  quarterSize.y,  quarterSize.z⟩⟩,
    ⟨halfSize, c + ⟨ quarterSize.x,  quarterSize.y,  quarterSize.z⟩⟩]

/-- Split box along an axis at normalized position t (0 to 1) -/
def splitAt (bb : BoundingBox) (axis : Nat) (t : Float) : BoundingBox × BoundingBox :=
  let minPt := bb.min
  let maxPt := bb.max
  match axis with
  | 0 => -- Split along X
    let splitX := minPt.x + t * bb.size.x
    (fromMinMax minPt ⟨splitX, maxPt.y, maxPt.z⟩,
     fromMinMax ⟨splitX, minPt.y, minPt.z⟩ maxPt)
  | 1 => -- Split along Y
    let splitY := minPt.y + t * bb.size.y
    (fromMinMax minPt ⟨maxPt.x, splitY, maxPt.z⟩,
     fromMinMax ⟨minPt.x, splitY, minPt.z⟩ maxPt)
  | _ => -- Split along Z
    let splitZ := minPt.z + t * bb.size.z
    (fromMinMax minPt ⟨maxPt.x, maxPt.y, splitZ⟩,
     fromMinMax ⟨minPt.x, minPt.y, splitZ⟩ maxPt)

-- ============================================================================
-- Utility operations
-- ============================================================================

/-- Set center while keeping size -/
def withCenter (bb : BoundingBox) (c : Vector3) : BoundingBox :=
  ⟨bb.size, c⟩

/-- Set size while keeping center -/
def withSize (bb : BoundingBox) (s : Vector3) : BoundingBox :=
  ⟨s, bb.center⟩

/-- Clamp point to be inside the bounding box -/
def clampPoint (bb : BoundingBox) (p : Vector3) : Vector3 :=
  bb.closestPoint p

end HouLean.BoundingBox
