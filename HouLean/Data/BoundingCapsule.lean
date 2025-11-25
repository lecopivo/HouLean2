import HouLean.Math
import HouLean.Data.Defs
import HouLean.Data.Vector3
import HouLean.Data.Float
import HouLean.Data.RigidTransform
import HouLean.Data.BoundingBox
import HouLean.Data.BoundingSphere

open HouLean.Math


namespace HouLean.Capsule

-- ============================================================================
-- Construction
-- ============================================================================

def empty : Capsule := ⟨⟨0, 0, 0⟩, ⟨0, 0, 0⟩, 0, 0⟩

def unit : Capsule := ⟨⟨0, 0, 0⟩, ⟨0, 1, 0⟩, 0.5, 0.5⟩

/-- Create uniform capsule (same radius at both ends) -/
def fromPointsUniform (start finish : Vector3) (radius : Float) : Capsule :=
  ⟨start, finish, radius, radius⟩

/-- Create tapered capsule (different radii) -/
def fromPointsTapered (start finish : Vector3) (startRadius endRadius : Float) : Capsule :=
  ⟨start, finish, startRadius, endRadius⟩

/-- Create capsule from center, axis direction, height, and uniform radius -/
def fromCenterAxisHeight (center axis : Vector3) (height radius : Float) : Capsule :=
  let halfAxis := (height / 2.0) * axis.normalized
  ⟨center - halfAxis, center + halfAxis, radius, radius⟩

/-- Create capsule from center, axis direction, height, and tapered radii -/
def fromCenterAxisHeightTapered (center axis : Vector3) (height startRadius endRadius : Float) : Capsule :=
  let halfAxis := (height / 2.0) * axis.normalized
  ⟨center - halfAxis, center + halfAxis, startRadius, endRadius⟩

/-- Create capsule from bounding sphere (zero-length capsule) -/
def fromBoundingSphere (bs : BoundingSphere) : Capsule :=
  ⟨bs.center, bs.center, bs.radius, bs.radius⟩

/-- Create capsule from line segment with uniform radius -/
def fromSegment (start finish : Vector3) (radius : Float) : Capsule :=
  ⟨start, finish, radius, radius⟩

-- ============================================================================
-- Queries
-- ============================================================================

/-- Length of the central axis -/
def height (c : Capsule) : Float :=
  distance c.start c.finish

/-- Direction vector along the axis (not normalized) -/
def axis (c : Capsule) : Vector3 :=
  c.finish - c.start

/-- Normalized direction vector -/
def direction (c : Capsule) : Vector3 :=
  c.axis.normalized

/-- Center point of the capsule -/
def center (c : Capsule) : Vector3 :=
  0.5 * (c.start + c.finish)

/-- Average radius -/
def averageRadius (c : Capsule) : Float :=
  (c.startRadius + c.endRadius) / 2.0

/-- Maximum radius -/
def maxRadius (c : Capsule) : Float :=
  max c.startRadius c.endRadius

/-- Minimum radius -/
def minRadius (c : Capsule) : Float :=
  min c.startRadius c.endRadius

/-- Is the capsule uniform (same radius at both ends)? -/
def isUniform (c : Capsule) : Bool :=
  c.startRadius == c.endRadius

/-- Is the capsule a sphere (zero length)? -/
def isSphere (c : Capsule) : Bool :=
  c.height == 0

/-- Is the capsule empty (negative or zero radius)? -/
def isEmpty (c : Capsule) : Bool :=
  c.startRadius <= 0 || c.endRadius <= 0

/-- Is the capsule valid (non-negative radii)? -/
def isValid (c : Capsule) : Bool :=
  c.startRadius >= 0 && c.endRadius >= 0

/-- Volume of the capsule -/
def volume (c : Capsule) : Float :=
  let h := c.height
  let r1 := c.startRadius
  let r2 := c.endRadius

  if h == 0 then
    -- Sphere
    (4.0 / 3.0) * Float.pi * r1 * r1 * r1
  else if r1 == r2 then
    -- Uniform cylinder + 2 hemispheres
    let cylinderVol := Float.pi * r1 * r1 * h
    let sphereVol := (4.0 / 3.0) * Float.pi * r1 * r1 * r1
    cylinderVol + sphereVol
  else
    -- Truncated cone + 2 spherical caps
    let coneVol := (Float.pi * h / 3.0) * (r1 * r1 + r1 * r2 + r2 * r2)
    let sphere1Vol := (4.0 / 3.0) * Float.pi * r1 * r1 * r1
    let sphere2Vol := (4.0 / 3.0) * Float.pi * r2 * r2 * r2
    coneVol + sphere1Vol + sphere2Vol

/-- Surface area of the capsule (approximate for tapered) -/
def surfaceArea (c : Capsule) : Float :=
  let h := c.height
  let r1 := c.startRadius
  let r2 := c.endRadius

  if h == 0 then
    -- Sphere
    4.0 * Float.pi * r1 * r1
  else if r1 == r2 then
    -- Uniform cylinder + sphere
    let cylinderArea := 2.0 * Float.pi * r1 * h
    let sphereArea := 4.0 * Float.pi * r1 * r1
    cylinderArea + sphereArea
  else
    -- Approximate: truncated cone lateral + 2 sphere areas
    let slantHeight := Float.sqrt (h * h + (r1 - r2) * (r1 - r2))
    let coneArea := Float.pi * (r1 + r2) * slantHeight
    let sphere1Area := 4.0 * Float.pi * r1 * r1
    let sphere2Area := 4.0 * Float.pi * r2 * r2
    coneArea + sphere1Area + sphere2Area

-- ============================================================================
-- Point queries
-- ============================================================================

/-- Get radius at parametric position t along axis (0 = start, 1 = end) -/
def radiusAt (c : Capsule) (t : Float) : Float :=
  c.startRadius + t * (c.endRadius - c.startRadius)

/-- Get point on axis at parametric position t -/
def pointOnAxis (c : Capsule) (t : Float) : Vector3 :=
  c.start.lerp c.finish t

/-- Closest point on the capsule axis to a given point -/
def closestPointOnAxis (c : Capsule) (p : Vector3) : Vector3 :=
  p.projectToSegment c.start c.finish

/-- Parametric position (0 to 1) of closest point on axis -/
def closestT (c : Capsule) (p : Vector3) : Float :=
  let ab := c.axis
  let ap := p - c.start
  let len2 := ab.length2
  if len2 == 0 then 0
  else (ap.dot ab / len2).clamp 0.0 1.0

/-- Closest point on capsule surface to a given point -/
def closestPoint (c : Capsule) (p : Vector3) : Vector3 :=
  let t := c.closestT p
  let axisPoint := c.pointOnAxis t
  let radius := c.radiusAt t
  let dir := p - axisPoint
  let len := dir.length
  if len == 0 then
    -- Point is on axis, return any point on surface
    axisPoint + ⟨radius, 0, 0⟩
  else
    axisPoint + (radius / len) * dir

-- ============================================================================
-- Distance queries
-- ============================================================================

/-- Signed distance from point to capsule surface (negative inside) -/
def signedDistance (c : Capsule) (p : Vector3) : Float :=
  let t := c.closestT p
  let axisPoint := c.pointOnAxis t
  let radius := c.radiusAt t
  distance p axisPoint - radius

/-- Distance from point to capsule surface (0 if inside) -/
def distance (c : Capsule) (p : Vector3) : Float :=
  max 0 (c.signedDistance p)

/-- Squared distance from point to capsule surface -/
def distanceSquared (c : Capsule) (p : Vector3) : Float :=
  let d := c.distance p
  d * d

-- ============================================================================
-- Containment tests
-- ============================================================================

/-- Does the capsule contain a point? -/
def contains (c : Capsule) (p : Vector3) : Bool :=
  c.signedDistance p <= 0

/-- Does this capsule fully contain another capsule? (approximate) -/
def containsCapsule (c other : Capsule) : Bool :=
  -- Check if both endpoints of other are inside c
  c.contains other.start && c.contains other.finish &&
  -- Check radius constraints
  let t1 := c.closestT other.start
  let t2 := c.closestT other.finish
  let r1 := c.radiusAt t1
  let r2 := c.radiusAt t2
  (Math.distance c.start other.start + other.startRadius <= r1) &&
  (Math.distance c.finish other.finish + other.endRadius <= r2)

/-- Does the capsule contain a sphere? -/
def containsSphere (c : Capsule) (center : Vector3) (radius : Float) : Bool :=
  c.signedDistance center <= -radius

-- ============================================================================
-- Intersection tests
-- ============================================================================

/-- Do two capsules intersect? -/
def intersects (c other : Capsule) : Bool :=
  -- Approximate: check distance between axes vs sum of radii
  -- For exact test, need more complex line segment distance with radius interpolation
  let t1 := c.closestT other.start
  let t2 := c.closestT other.finish
  let p1 := c.pointOnAxis t1
  let p2 := c.pointOnAxis t2

  let d1 := Math.distance p1 other.start
  let r1 := c.radiusAt t1 + other.startRadius

  let d2 := Math.distance p2 other.finish
  let r2 := c.radiusAt t2 + other.endRadius

  d1 <= r1 || d2 <= r2

/-- Does the capsule intersect with a sphere? -/
def intersectsSphere (c : Capsule) (center : Vector3) (radius : Float) : Bool :=
  let t := c.closestT center
  let axisPoint := c.pointOnAxis t
  let capsuleRadius := c.radiusAt t
  Math.distance center axisPoint <= capsuleRadius + radius

/-- Does the capsule intersect with a bounding box? -/
def intersectsBox (c : Capsule) (bb : BoundingBox) : Bool :=
  -- Check if closest point on axis to box is within range
  let axisClosest := bb.closestPoint c.center
  let t := c.closestT axisClosest
  let radius := c.radiusAt t
  bb.distanceSquared (c.pointOnAxis t) <= radius * radius

/-- Does the capsule intersect with a ray? Returns (hit, tMin, tMax) -/
def intersectsRay (c : Capsule) (origin direction : Vector3) : Bool × Float × Float :=
  -- Simplified: treat as cylinder + sphere caps for uniform case
  -- Full implementation would require solving quartic equation for tapered case
  if c.isUniform then
    -- Ray-cylinder intersection
    let axis := c.axis
    let r := c.startRadius
    let oc := origin - c.start

    -- Project to find closest approach
    let dirDotAxis := direction.dot axis
    let ocDotAxis := oc.dot axis

    -- Perpendicular distance calculation
    let a := direction.length2 - dirDotAxis * dirDotAxis / axis.length2
    let b := 2.0 * (oc.dot direction - dirDotAxis * ocDotAxis / axis.length2)
    let c := oc.length2 - ocDotAxis * ocDotAxis / axis.length2 - r * r

    let discriminant := b * b - 4.0 * a * c

    if discriminant < 0 then
      (false, 0, 0)
    else
      let sqrtD := Float.sqrt discriminant
      let t1 := (-b - sqrtD) / (2.0 * a)
      let t2 := (-b + sqrtD) / (2.0 * a)
      (true, t1, t2)
  else
    -- Approximate for tapered case
    (false, 0, 0)

-- ============================================================================
-- Combination operations
-- ============================================================================

/-- Union of two capsules (smallest capsule containing both) -/
def union (c other : Capsule) : Capsule :=
  -- Simple approximation: use bounding sphere approach
  let allPoints := #[c.start, c.finish, other.start, other.finish]
  let center : Vector3 := 0.25 * (allPoints.foldl (· + ·) ⟨0, 0, 0⟩)

  -- Find furthest endpoints
  let maxDist := allPoints.foldl (fun acc p => max acc (Math.distance center p)) 0
  let direction := (other.center - c.center).normalized

  let newStart := center - maxDist * direction
  let newFinish := center + maxDist * direction
  let newRadius := max (max c.startRadius c.endRadius) (max other.startRadius other.endRadius)

  ⟨newStart, newFinish, newRadius, newRadius⟩

/-- Expand capsule by a margin -/
def expand (c : Capsule) (margin : Float) : Capsule :=
  ⟨c.start, c.finish, c.startRadius + margin, c.endRadius + margin⟩

/-- Scale capsule around its center -/
def scale (c : Capsule) (s : Float) : Capsule :=
  let center := c.center
  let newStart := center + s * (c.start - center)
  let newFinish := center + s * (c.finish - center)
  ⟨newStart, newFinish, s * c.startRadius, s * c.endRadius⟩

/-- Scale radii only -/
def scaleRadii (c : Capsule) (s : Float) : Capsule :=
  ⟨c.start, c.finish, s * c.startRadius, s * c.endRadius⟩

-- ============================================================================
-- Transformation
-- ============================================================================

/-- Transform capsule by a rigid transformation -/
def transform (c : Capsule) (xform : RigidTransform) : Capsule :=
  ⟨xform.transformPoint c.start,
   xform.transformPoint c.finish,
   c.startRadius,
   c.endRadius⟩

/-- Transform capsule with uniform scale -/
def transformWithScale (c : Capsule) (xform : RigidScaleTransform) : Capsule :=
  ⟨xform.transformPoint c.start,
   xform.transformPoint c.finish,
   xform.scale * c.startRadius,
   xform.scale * c.endRadius⟩

/-- Translate capsule -/
def translate (c : Capsule) (offset : Vector3) : Capsule :=
  ⟨c.start + offset, c.finish + offset, c.startRadius, c.endRadius⟩

-- ============================================================================
-- Comparison operations
-- ============================================================================

defun beq (a b : Capsule) : Bool :=
  a.start.beq b.start &&
  a.finish.beq b.finish &&
  a.startRadius == b.startRadius &&
  a.endRadius == b.endRadius

instance : BEq Capsule := ⟨beq⟩

/-- Approximately equal with tolerance -/
def approxEq (a b : Capsule) (tolerance : Float := 0.0001) : Bool :=
  Math.distance a.start b.start <= tolerance &&
  Math.distance a.finish b.finish <= tolerance &&
  Float.abs (a.startRadius - b.startRadius) <= tolerance &&
  Float.abs (a.endRadius - b.endRadius) <= tolerance

-- ============================================================================
-- Conversion
-- ============================================================================

/-- Convert to bounding box -/
def toBoundingBox (c : Capsule) : BoundingBox :=
  let maxR := c.maxRadius
  let corners := #[
    c.start + ⟨maxR, maxR, maxR⟩,
    c.start - ⟨maxR, maxR, maxR⟩,
    c.finish + ⟨maxR, maxR, maxR⟩,
    c.finish - ⟨maxR, maxR, maxR⟩
  ]
  BoundingBox.fromPoints corners

/-- Convert to bounding sphere -/
def toBoundingSphere (c : Capsule) : BoundingSphere :=
  let center := c.center
  let halfHeight := c.height / 2.0
  let maxR := c.maxRadius
  let radius := Float.sqrt (halfHeight * halfHeight + maxR * maxR)
  ⟨center, radius⟩

/-- Get start sphere -/
def startSphere (c : Capsule) : BoundingSphere :=
  ⟨c.start, c.startRadius⟩

/-- Get end sphere -/
def endSphere (c : Capsule) : BoundingSphere :=
  ⟨c.finish, c.endRadius⟩

-- ============================================================================
-- Utility operations
-- ============================================================================

/-- Set start point -/
def withStart (c : Capsule) (start : Vector3) : Capsule :=
  ⟨start, c.finish, c.startRadius, c.endRadius⟩

/-- Set end point -/
def withFinish (c : Capsule) (finish : Vector3) : Capsule :=
  ⟨c.start, finish, c.startRadius, c.endRadius⟩

/-- Set start radius -/
def withStartRadius (c : Capsule) (r : Float) : Capsule :=
  ⟨c.start, c.finish, r, c.endRadius⟩

/-- Set end radius -/
def withEndRadius (c : Capsule) (r : Float) : Capsule :=
  ⟨c.start, c.finish, c.startRadius, r⟩

/-- Set both radii to the same value (make uniform) -/
def makeUniform (c : Capsule) (radius : Float) : Capsule :=
  ⟨c.start, c.finish, radius, radius⟩

/-- Reverse the capsule direction (swap start and end) -/
def reverse (c : Capsule) : Capsule :=
  ⟨c.finish, c.start, c.endRadius, c.startRadius⟩

/-- Lerp between two capsules -/
def lerp (a b : Capsule) (t : Float) : Capsule :=
  ⟨a.start.lerp b.start t,
   a.finish.lerp b.finish t,
   a.startRadius + t * (b.startRadius - a.startRadius),
   a.endRadius + t * (b.endRadius - a.endRadius)⟩

end HouLean.Capsule
