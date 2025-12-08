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
-- Basic Queries
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

-- ============================================================================
-- Geometry helpers for tapered capsules
-- ============================================================================

/-- Compute tapered capsule geometry parameters.
    Returns (sinA, cosA, hFrust, isDegenerate) where:
    - sinA, cosA: sine/cosine of the cone half-angle
    - hFrust: height of the frustum (negative if degenerate)
    - isDegenerate: true if the capsule degenerates to overlapping spheres -/
def taperGeometry (c : Capsule) : Float × Float × Float × Bool :=
  let h := c.height
  let r1 := c.startRadius
  let r2 := c.endRadius
  let dr := Float.abs (r1 - r2)

  if h == 0 then
    (0, 1, 0, true)
  else if dr == 0 then
    (0, 1, h, false)
  else
    let s := Float.sqrt (h * h + dr * dr)
    let sinA := dr / s
    let cosA := h / s
    let hFrust := h - (r1 + r2) * sinA
    (sinA, cosA, hFrust, hFrust <= 0)

/-- Is this a degenerate tapered capsule where one sphere dominates? -/
def isDegenerate (c : Capsule) : Bool :=
  let (_, _, _, deg) := c.taperGeometry
  deg

/-- For degenerate capsules, get the index of the dominant sphere.
    Returns 0 for start sphere, 1 for end sphere. -/
def dominantSphereIndex (c : Capsule) : Nat :=
  if c.startRadius >= c.endRadius then 0 else 1

/-- Get the center of the dominant sphere (for degenerate capsules) -/
def dominantCenter (c : Capsule) : Vector3 :=
  if c.startRadius >= c.endRadius then c.start else c.finish

/-- Get the radius of the dominant sphere (for degenerate capsules) -/
def dominantRadius (c : Capsule) : Float :=
  max c.startRadius c.endRadius

-- ============================================================================
-- Volume and Surface Area
-- ============================================================================

/-- Volume of the capsule -/
def volume (c : Capsule) : Float :=
  let h := c.height
  let r1 := c.startRadius
  let r2 := c.endRadius

  if h == 0 then
    -- Zero length: sphere with larger radius
    let r := max r1 r2
    (4.0 / 3.0) * Math.pi * r * r * r
  else if r1 == r2 then
    -- Uniform capsule: cylinder + sphere (two hemispheres)
    Math.pi * r1 * r1 * h + (4.0 / 3.0) * Math.pi * r1 * r1 * r1
  else
    let (sinA, cosA, hFrust, degenerate) := c.taperGeometry

    if degenerate then
      -- Degenerate: compute union of two spheres
      -- V_union = V1 + V2 - V_intersection
      let d := h  -- distance between centers
      if d >= r1 + r2 then
        -- No overlap (shouldn't happen if degenerate, but handle it)
        (4.0 / 3.0) * Math.pi * (r1*r1*r1 + r2*r2*r2)
      else if d <= Float.abs (r1 - r2) then
        -- One sphere entirely inside the other
        let r := max r1 r2
        (4.0 / 3.0) * Math.pi * r * r * r
      else
        -- Partial overlap: use lens formula
        -- V_lens = π(r1+r2-d)²(d² + 2d(r1+r2) - 3(r1-r2)²) / (12d)
        let sum := r1 + r2
        let diff := r1 - r2
        let vLens := Math.pi * (sum - d) * (sum - d) *
                     (d*d + 2*d*sum - 3*diff*diff) / (12 * d)
        let v1 := (4.0 / 3.0) * Math.pi * r1 * r1 * r1
        let v2 := (4.0 / 3.0) * Math.pi * r2 * r2 * r2
        v1 + v2 - vLens
    else
      -- Non-degenerate tapered capsule
      -- Tangent circle radii
      let rt1 := r1 * cosA
      let rt2 := r2 * cosA

      -- Frustum volume (between tangent circles)
      let vFrust := (Math.pi * hFrust / 3.0) * (rt1*rt1 + rt1*rt2 + rt2*rt2)

      -- Spherical cap heights
      let hCap1 := r1 * (1.0 - sinA)
      let hCap2 := r2 * (1.0 - sinA)

      -- Cap volumes: V = π h² (3R - h) / 3
      let vCap1 := Math.pi * hCap1 * hCap1 * (3.0 * r1 - hCap1) / 3.0
      let vCap2 := Math.pi * hCap2 * hCap2 * (3.0 * r2 - hCap2) / 3.0

      vFrust + vCap1 + vCap2

-- /-- Surface area of the capsule -/
-- def surfaceArea (c : Capsule) : Float :=
--   let h := c.height
--   let r1 := c.startRadius
--   let r2 := c.endRadius

--   if h == 0 then
--     let r := max r1 r2
--     4.0 * Float.pi * r * r
--   else if r1 == r2 then
--     -- Cylinder lateral + sphere
--     2.0 * Float.pi * r1 * h + 4.0 * Float.pi * r1 * r1
--   else
--     let (sinA, cosA, hFrust, degenerate) := c.taperGeometry

--     if degenerate then
--       -- Degenerate: surface area of sphere union
--       let d := h
--       if d >= r1 + r2 then
--         4.0 * Float.pi * (r1*r1 + r2*r2)
--       else if d <= Float.abs (r1 - r2) then
--         let r := max r1 r2
--         4.0 * Float.pi * r * r
--       else
--         -- Each sphere contributes a spherical cap to the exterior
--         -- Cap height for sphere i: h_i = r_i - (d² + r_i² - r_j²)/(2d)
--         let h1 := r1 - (d*d + r1*r1 - r2*r2) / (2*d)
--         let h2 := r2 - (d*d + r2*r2 - r1*r1) / (2*d)
--         -- Exposed area = full sphere - cap that's inside the other
--         let a1 := 4.0 * Float.pi * r1 * r1 - 2.0 * Float.pi * r1 * (r1 - h1)
--         let a2 := 4.0 * Float.pi * r2 * r2 - 2.0 * Float.pi * r2 * (r2 - h2)
--         -- Simplifies to: 2πr₁(r₁ + h₁) + 2πr₂(r₂ + h₂)
--         2.0 * Float.pi * r1 * (r1 + h1) + 2.0 * Float.pi * r2 * (r2 + h2)
--     else
--       -- Non-degenerate tapered
--       -- Slant height of frustum
--       let slant := Float.sqrt (hFrust * hFrust + (r1 * cosA - r2 * cosA) * (r1 * cosA - r2 * cosA))
--       -- Actually for a proper cone frustum with tangent circles:
--       let rt1 := r1 * cosA
--       let rt2 := r2 * cosA
--       let slant2 := Float.sqrt (hFrust * hFrust + (rt1 - rt2) * (rt1 - rt2))

--       -- Frustum lateral area
--       let aFrust := Float.pi * (rt1 + rt2) * slant2

--       -- Cap surface areas: A = 2πRh
--       let hCap1 := r1 * (1.0 - sinA)
--       let hCap2 := r2 * (1.0 - sinA)
--       let aCap1 := 2.0 * Float.pi * r1 * hCap1
--       let aCap2 := 2.0 * Float.pi * r2 * hCap2

--       aFrust + aCap1 + aCap2

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
  projectToSegment p c.start c.finish

/-- Parametric position (0 to 1) of closest point on axis -/
def closestT (c : Capsule) (p : Vector3) : Float :=
  let ab := c.axis
  let ap := p - c.start
  let len2 := ab.length2
  if len2 == 0 then 0
  else clamp (ap.dot ab / len2) 0.0 1.0

/-- Closest point on capsule surface to a given point -/
def closestPoint (c : Capsule) (p : Vector3) : Vector3 :=
  if c.isDegenerate then
    -- Handle as two spheres
    let d1 := Math.distance p c.start
    let d2 := Math.distance p c.finish
    let onSphere1 := d1 - c.startRadius <= d2 - c.endRadius
    let (center, radius) := if onSphere1 then (c.start, c.startRadius) else (c.finish, c.endRadius)
    let dir := p - center
    let len := dir.length
    if len == 0 then center + ⟨radius, 0, 0⟩
    else center + (radius / len) * dir
  else
    let t := c.closestT p
    let axisPoint := c.pointOnAxis t
    let radius := c.radiusAt t
    let dir := p - axisPoint
    let len := dir.length
    if len == 0 then axisPoint + ⟨radius, 0, 0⟩
    else axisPoint + (radius / len) * dir

-- ============================================================================
-- Distance queries
-- ============================================================================

/-- Signed distance from point to capsule surface (negative inside) -/
def signedDistance (c : Capsule) (p : Vector3) : Float :=
  if c.isDegenerate then
    -- Distance to union of two spheres = min of distances to each sphere
    let sd1 := Math.distance p c.start - c.startRadius
    let sd2 := Math.distance p c.finish - c.endRadius
    min sd1 sd2
  else
    let t := c.closestT p
    let axisPoint := c.pointOnAxis t
    let radius := c.radiusAt t
    Math.distance p axisPoint - radius

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
  if c.isDegenerate then
    -- Check if both spheres of 'other' fit in either of our spheres
    let containsInStart :=
      Math.distance c.start other.start + other.startRadius <= c.startRadius &&
      Math.distance c.start other.finish + other.endRadius <= c.startRadius
    let containsInEnd :=
      Math.distance c.finish other.start + other.startRadius <= c.endRadius &&
      Math.distance c.finish other.finish + other.endRadius <= c.endRadius
    containsInStart || containsInEnd
  else
    -- Check endpoints with their radii
    c.signedDistance other.start <= -other.startRadius &&
    c.signedDistance other.finish <= -other.endRadius

/-- Does the capsule contain a sphere? -/
def containsSphere (c : Capsule) (center : Vector3) (radius : Float) : Bool :=
  c.signedDistance center <= -radius

-- ============================================================================
-- Intersection tests
-- ============================================================================

/-- Do two capsules intersect? -/
def intersects (c other : Capsule) : Bool :=
  if c.isDegenerate || other.isDegenerate then
    -- Check all sphere-sphere pairs
    let d11 := Math.distance c.start other.start
    let d12 := Math.distance c.start other.finish
    let d21 := Math.distance c.finish other.start
    let d22 := Math.distance c.finish other.finish
    d11 <= c.startRadius + other.startRadius ||
    d12 <= c.startRadius + other.endRadius ||
    d21 <= c.endRadius + other.startRadius ||
    d22 <= c.endRadius + other.endRadius
  else
    -- Use signed distance for more accurate test
    let sd1 := c.signedDistance other.start
    let sd2 := c.signedDistance other.finish
    let sd3 := other.signedDistance c.start
    let sd4 := other.signedDistance c.finish
    sd1 <= other.startRadius || sd2 <= other.endRadius ||
    sd3 <= c.startRadius || sd4 <= c.endRadius

/-- Does the capsule intersect with a sphere? -/
def intersectsSphere (c : Capsule) (center : Vector3) (radius : Float) : Bool :=
  if c.isDegenerate then
    Math.distance center c.start <= c.startRadius + radius ||
    Math.distance center c.finish <= c.endRadius + radius
  else
    let t := c.closestT center
    let axisPoint := c.pointOnAxis t
    let capsuleRadius := c.radiusAt t
    Math.distance center axisPoint <= capsuleRadius + radius

/-- Does the capsule intersect with a bounding box? -/
partial def intersectsBox (c : Capsule) (bb : BoundingBox) : Bool :=
  if c.isDegenerate then
    -- Check both spheres
    bb.distanceSquared c.start <= c.startRadius * c.startRadius ||
    bb.distanceSquared c.finish <= c.endRadius * c.endRadius
  else
    -- Sample multiple points along the axis
    let n := 4  -- Number of samples
    let rec checkSamples (i : Nat) : Bool :=
      if i > n then false
      else
        let t := i.toFloat / n.toFloat
        let pt := c.pointOnAxis t
        let r := c.radiusAt t
        if bb.distanceSquared pt <= r * r then true
        else checkSamples (i + 1)
    checkSamples 0

/-- Does the capsule intersect with a ray? Returns (hit, tMin, tMax) -/
def intersectsRay (c : Capsule) (origin direction : Vector3) : Bool × Float × Float :=
  if c.isDegenerate then
    -- Check ray against both spheres, return closest hit
    let checkSphere (center : Vector3) (radius : Float) : Bool × Float × Float :=
      let oc := origin - center
      let a := direction.length2
      let b := 2.0 * oc.dot direction
      let cc := oc.length2 - radius * radius
      let discriminant := b * b - 4.0 * a * cc
      if discriminant < 0 then (false, 0, 0)
      else
        let sqrtD := Float.sqrt discriminant
        let t1 := (-b - sqrtD) / (2.0 * a)
        let t2 := (-b + sqrtD) / (2.0 * a)
        (true, t1, t2)
    let (hit1, t1min, t1max) := checkSphere c.start c.startRadius
    let (hit2, t2min, t2max) := checkSphere c.finish c.endRadius
    if hit1 && hit2 then (true, min t1min t2min, max t1max t2max)
    else if hit1 then (true, t1min, t1max)
    else if hit2 then (true, t2min, t2max)
    else (false, 0, 0)
  else if c.isUniform then
    -- Ray-cylinder intersection
    let ax := c.axis
    let r := c.startRadius
    let oc := origin - c.start

    let dirDotAxis := direction.dot ax
    let ocDotAxis := oc.dot ax
    let axLen2 := ax.length2

    let a := direction.length2 - dirDotAxis * dirDotAxis / axLen2
    let b := 2.0 * (oc.dot direction - dirDotAxis * ocDotAxis / axLen2)
    let cc := oc.length2 - ocDotAxis * ocDotAxis / axLen2 - r * r

    let discriminant := b * b - 4.0 * a * cc

    if discriminant < 0 then (false, 0, 0)
    else
      let sqrtD := Float.sqrt discriminant
      let t1 := (-b - sqrtD) / (2.0 * a)
      let t2 := (-b + sqrtD) / (2.0 * a)
      (true, t1, t2)
  else
    -- Approximate for non-degenerate tapered case
    (false, 0, 0)

-- ============================================================================
-- Combination operations
-- ============================================================================

/-- Union of two capsules (smallest capsule containing both) -/
def union (c other : Capsule) : Capsule := Id.run do
  -- Find the axis that spans both capsules
  let points := #[
    (c.start, c.startRadius),
    (c.finish, c.endRadius),
    (other.start, other.startRadius),
    (other.finish, other.endRadius)
  ]

  -- Find the two most distant point+radius combinations
  let mut maxDist := 0.0
  let mut p1 := c.start
  let mut r1 := c.startRadius
  let mut p2 := c.finish
  let mut r2 := c.endRadius

  for i in [:points.size] do
    for j in [i+1:points.size] do
      let (pi, ri) := points[i]!
      let (pj, rj) := points[j]!
      let d := Math.distance pi pj + ri + rj
      if d > maxDist then
        maxDist := d
        p1 := pi
        r1 := ri
        p2 := pj
        r2 := rj

  ⟨p1, p2, r1, r2⟩

/-- Expand capsule by a margin -/
def expand (c : Capsule) (margin : Float) : Capsule :=
  ⟨c.start, c.finish, c.startRadius + margin, c.endRadius + margin⟩

/-- Scale capsule around its center -/
def scale (c : Capsule) (s : Float) : Capsule :=
  let ctr := c.center
  let newStart := ctr + s * (c.start - ctr)
  let newFinish := ctr + s * (c.finish - ctr)
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

def beq (a b : Capsule) : Bool :=
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
  -- Use correct radius at each endpoint
  let r1 := ⟨c.startRadius, c.startRadius, c.startRadius⟩
  let r2 := ⟨c.endRadius, c.endRadius, c.endRadius⟩
  let corners := #[
    c.start + r1, c.start - r1,
    c.finish + r2, c.finish - r2
  ]
  BoundingBox.fromPoints corners

/-- Convert to bounding sphere -/
def toBoundingSphere (c : Capsule) : BoundingSphere :=
  if c.isDegenerate then
    -- For degenerate, find sphere enclosing both endpoint spheres
    let d := c.height
    if d == 0 then
      ⟨c.start, max c.startRadius c.endRadius⟩
    else if c.startRadius >= c.endRadius + d then
      -- Start sphere contains end sphere
      ⟨c.start, c.startRadius⟩
    else if c.endRadius >= c.startRadius + d then
      -- End sphere contains start sphere
      ⟨c.finish, c.endRadius⟩
    else
      -- Compute minimal enclosing sphere of two spheres
      let dir := c.direction
      let p1 := c.start - c.startRadius * dir
      let p2 := c.finish + c.endRadius * dir
      let ctr := 0.5 * (p1 + p2)
      let rad := 0.5 * Math.distance p1 p2
      ⟨ctr, rad⟩
  else
    -- Non-degenerate: enclosing sphere
    let ctr := c.center
    let halfH := c.height / 2.0
    -- Distance from center to furthest point on surface
    let d1 := halfH + c.startRadius
    let d2 := halfH + c.endRadius
    let rad := max d1 d2
    ⟨ctr, rad⟩

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
