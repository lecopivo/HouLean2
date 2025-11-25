import HouLean.Math
import HouLean.Data.Defs
import HouLean.Data.Vector3
import HouLean.Data.Float
import HouLean.Data.RigidTransform
import HouLean.Data.BoundingBox

open HouLean.Math

namespace HouLean.BoundingSphere

-- ============================================================================
-- Construction
-- ============================================================================

def empty : BoundingSphere := ⟨⟨0, 0, 0⟩, 0⟩

def unit : BoundingSphere := ⟨⟨0, 0, 0⟩, 1⟩

def fromCenterRadius (center : Vector3) (radius : Float) : BoundingSphere :=
  ⟨center, radius⟩

def fromPoint (p : Vector3) : BoundingSphere :=
  ⟨p, 0⟩

def fromPoints (points : Array Vector3) : BoundingSphere :=
  if points.isEmpty then empty
  else
    -- Compute centroid
    let sum : Vector3 := points.foldl (· + ·) ⟨0.0, 0.0, 0.0⟩
    let center := sum / points.size.toFloat
    -- Find max distance from centroid
    let maxDistSq := points.foldl (fun acc p => max acc (distance2 center p)) 0
    let radius := Float.sqrt maxDistSq
    ⟨center, radius⟩

def fromBoundingBox (bb : BoundingBox) : BoundingSphere :=
  ⟨bb.center, bb.diagonal / 2.0⟩

/-- Minimal bounding sphere from two points -/
def fromTwoPoints (a b : Vector3) : BoundingSphere :=
  let center := 0.5 * (a + b)
  let radius := 0.5 * distance a b
  ⟨center, radius⟩

/-- Ritter's bounding sphere algorithm (fast approximation) -/
def ritterSphere (points : Array Vector3) : BoundingSphere :=
  if points.isEmpty then empty
  else if points.size == 1 then fromPoint points[0]!
  else Id.run do
    -- Find most distant pair along each axis
    let p0 := points[0]!
    let (xMin, xMax) := points.foldl (fun (mn, mx) p =>
      (if p.x < mn.x then p else mn, if p.x > mx.x then p else mx)) (p0, p0)
    let (yMin, yMax) := points.foldl (fun (mn, mx) p =>
      (if p.y < mn.y then p else mn, if p.y > mx.y then p else mx)) (p0, p0)
    let (zMin, zMax) := points.foldl (fun (mn, mx) p =>
      (if p.z < mn.z then p else mn, if p.z > mx.z then p else mx)) (p0, p0)

    -- Pick the most distant pair
    let dx := distance2 xMin xMax
    let dy := distance2 yMin yMax
    let dz := distance2 zMin zMax
    let (a, b) := if dx >= dy && dx >= dz then (xMin, xMax)
                  else if dy >= dz then (yMin, yMax)
                  else (zMin, zMax)

    -- Start with sphere from most distant pair
    let mut center := 0.5 * (a + b)
    let mut radiusSq := 0.25 * distance2 a b

    -- Expand to include all points
    for p in points do
      let distSq := distance2 center p
      if distSq > radiusSq then
        let dist := Float.sqrt distSq
        let newRadius := (Float.sqrt radiusSq + dist) / 2.0
        let t := (newRadius - Float.sqrt radiusSq) / dist
        center := center + t * (p - center)
        radiusSq := newRadius * newRadius

    ⟨center, Float.sqrt radiusSq⟩

-- ============================================================================
-- Queries
-- ============================================================================

/-- Volume of the sphere -/
def volume (bs : BoundingSphere) : Float :=
  (4.0 / 3.0) * Float.pi * bs.radius * bs.radius * bs.radius

/-- Surface area -/
def surfaceArea (bs : BoundingSphere) : Float :=
  4.0 * Float.pi * bs.radius * bs.radius

/-- Diameter -/
def diameter (bs : BoundingSphere) : Float :=
  2.0 * bs.radius

/-- Is the sphere empty (zero or negative radius)? -/
def isEmpty (bs : BoundingSphere) : Bool :=
  bs.radius <= 0

/-- Is the sphere valid (non-negative radius)? -/
def isValid (bs : BoundingSphere) : Bool :=
  bs.radius >= 0

-- ============================================================================
-- Containment tests
-- ============================================================================

/-- Does the sphere contain a point? -/
def contains (bs : BoundingSphere) (p : Vector3) : Bool :=
  distance2 bs.center p <= bs.radius * bs.radius

/-- Does this sphere fully contain another sphere? -/
def containsSphere (bs other : BoundingSphere) : Bool :=
  let centerDist := distance bs.center other.center
  centerDist + other.radius <= bs.radius

/-- Does the sphere contain a bounding box? -/
def containsBox (bs : BoundingSphere) (bb : BoundingBox) : Bool :=
  -- All 8 corners must be inside
  bb.corners.all (bs.contains ·)

-- ============================================================================
-- Intersection tests
-- ============================================================================

/-- Do two spheres overlap? -/
def intersects (bs other : BoundingSphere) : Bool :=
  let centerDistSq := distance2 bs.center other.center
  let radiusSum := bs.radius + other.radius
  centerDistSq <= radiusSum * radiusSum

/-- Does the sphere intersect with a bounding box? -/
def intersectsBox (bs : BoundingSphere) (bb : BoundingBox) : Bool :=
  let closest := clamp bs.center bb.min bb.max
  distance2 bs.center closest <= bs.radius * bs.radius

/-- Does the sphere intersect with a ray? Returns (hit, tMin, tMax) -/
def intersectsRay (bs : BoundingSphere) (origin direction : Vector3) : Bool × Float × Float :=
  let oc := origin - bs.center
  let a := direction.length2
  let b := 2.0 * (oc.dot direction)
  let c := oc.length2 - bs.radius * bs.radius
  let discriminant := b * b - 4.0 * a * c

  if discriminant < 0 then
    (false, 0, 0)
  else
    let sqrtD := Float.sqrt discriminant
    let t1 := (-b - sqrtD) / (2.0 * a)
    let t2 := (-b + sqrtD) / (2.0 * a)
    (true, t1, t2)

-- ============================================================================
-- Distance queries
-- ============================================================================

/-- Distance from point to sphere surface (negative if inside) -/
def distance (bs : BoundingSphere) (p : Vector3) : Float :=
  HouLean.Vector3.distance bs.center p - bs.radius

/-- Squared distance from point to sphere surface -/
def distanceSquared (bs : BoundingSphere) (p : Vector3) : Float :=
  let d := bs.distance p
  d * d

/-- Closest point on sphere surface to a given point -/
def closestPoint (bs : BoundingSphere) (p : Vector3) : Vector3 :=
  let dir := p - bs.center
  let len := dir.length
  if len == 0 then
    bs.center + ⟨bs.radius, 0, 0⟩  -- arbitrary point on surface
  else
    bs.center + (bs.radius / len) * dir

-- ============================================================================
-- Combination operations
-- ============================================================================

/-- Union of two spheres (smallest sphere containing both) -/
def union (bs other : BoundingSphere) : BoundingSphere :=
  if bs.isEmpty then other
  else if other.isEmpty then bs
  else
    let centerDist := Math.distance bs.center other.center

    -- Check if one contains the other
    if centerDist + other.radius <= bs.radius then bs
    else if centerDist + bs.radius <= other.radius then other
    else
      -- Compute enclosing sphere
      let newRadius := (bs.radius + other.radius + centerDist) / 2.0
      let t := (newRadius - bs.radius) / centerDist
      let newCenter := bs.center + t * (other.center - bs.center)
      ⟨newCenter, newRadius⟩

/-- Expand sphere to include a point -/
def includePoint (bs : BoundingSphere) (p : Vector3) : BoundingSphere :=
  if bs.isEmpty then fromPoint p
  else
    let dist := Math.distance bs.center p
    if dist <= bs.radius then bs
    else
      let newRadius := (bs.radius + dist) / 2.0
      let t := (newRadius - bs.radius) / dist
      let newCenter := bs.center + t * (p - bs.center)
      ⟨newCenter, newRadius⟩

/-- Expand sphere by a margin -/
def expand (bs : BoundingSphere) (margin : Float) : BoundingSphere :=
  ⟨bs.center, bs.radius + margin⟩

/-- Scale sphere around its center -/
def scale (bs : BoundingSphere) (s : Float) : BoundingSphere :=
  ⟨bs.center, s * bs.radius⟩

-- ============================================================================
-- Transformation
-- ============================================================================

/-- Transform sphere by a rigid transformation -/
def transform (bs : BoundingSphere) (xform : RigidTransform) : BoundingSphere :=
  ⟨xform.transformPoint bs.center, bs.radius⟩

/-- Transform sphere by a rigid transformation with uniform scale -/
def transformWithScale (bs : BoundingSphere) (xform : RigidScaleTransform) : BoundingSphere :=
  ⟨xform.transformPoint bs.center, xform.scale * bs.radius⟩

/-- Translate sphere -/
def translate (bs : BoundingSphere) (offset : Vector3) : BoundingSphere :=
  ⟨bs.center + offset, bs.radius⟩

-- ============================================================================
-- Comparison operations
-- ============================================================================

defun beq (a b : BoundingSphere) : Bool :=
  a.center.beq b.center && a.radius == b.radius

instance : BEq BoundingSphere := ⟨beq⟩

/-- Approximately equal with tolerance -/
def approxEq (a b : BoundingSphere) (tolerance : Float := 0.0001) : Bool :=
  Math.distance a.center b.center <= tolerance &&
  Float.abs (a.radius - b.radius) <= tolerance

-- ============================================================================
-- Projection and sampling
-- ============================================================================

/-- Project point onto sphere surface -/
def projectPoint (bs : BoundingSphere) (p : Vector3) : Vector3 :=
  bs.closestPoint p

/-- Get point on sphere surface from spherical coordinates (theta, phi) -/
def pointAt (bs : BoundingSphere) (theta phi : Float) : Vector3 :=
  let x := bs.radius * Float.sin phi * Float.cos theta
  let y := bs.radius * Float.sin phi * Float.sin theta
  let z := bs.radius * Float.cos phi
  bs.center + ⟨x, y, z⟩

/-- Get random point on sphere surface (requires random theta in [0, 2π] and phi in [0, π]) -/
def sampleSurface (bs : BoundingSphere) (theta phi : Float) : Vector3 :=
  bs.pointAt theta phi

/-- Get random point inside sphere (requires random values) -/
def sampleVolume (bs : BoundingSphere) (theta phi r : Float) : Vector3 :=
  let radius := bs.radius * Float.pow r (1.0 / 3.0)  -- cubic root for uniform distribution
  let x := radius * Float.sin phi * Float.cos theta
  let y := radius * Float.sin phi * Float.sin theta
  let z := radius * Float.cos phi
  bs.center + ⟨x, y, z⟩

-- ============================================================================
-- Subdivision
-- ============================================================================

/-- Split sphere into two spheres along a plane through center -/
def split (bs : BoundingSphere) (normal : Vector3) : BoundingSphere × BoundingSphere :=
  let offset := (bs.radius / 2.0) * normal.normalized
  (⟨bs.center - offset, bs.radius / 2.0⟩,
   ⟨bs.center + offset, bs.radius / 2.0⟩)

-- ============================================================================
-- Conversion
-- ============================================================================

/-- Convert to bounding box (axis-aligned box containing sphere) -/
def toBoundingBox (bs : BoundingSphere) : BoundingBox :=
  let size := ⟨2 * bs.radius, 2 * bs.radius, 2 * bs.radius⟩
  ⟨size, bs.center⟩

-- ============================================================================
-- Utility operations
-- ============================================================================

/-- Set center while keeping radius -/
def withCenter (bs : BoundingSphere) (c : Vector3) : BoundingSphere :=
  ⟨c, bs.radius⟩

/-- Set radius while keeping center -/
def withRadius (bs : BoundingSphere) (r : Float) : BoundingSphere :=
  ⟨bs.center, r⟩

/-- Clamp point to be inside the sphere -/
def clampPoint (bs : BoundingSphere) (p : Vector3) : Vector3 :=
  let dir := p - bs.center
  let len := dir.length
  if len <= bs.radius then p
  else bs.center + (bs.radius / len) * dir

/-- Get tangent plane at point on sphere surface -/
def tangentPlane (bs : BoundingSphere) (p : Vector3) : Vector3 × Float :=
  let normal := (p - bs.center).normalized
  let d := -(normal.dot p)
  (normal, d)

end HouLean.BoundingSphere
