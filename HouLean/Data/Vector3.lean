import HouLean.Math
import HouLean.Data.Defs
import HouLean.Data.Float

open HouLean.Math

namespace HouLean.Vector3

-- ============================================================================
-- Arithmetic operations
-- ============================================================================

defun add (a b : Vector3) : Vector3 :=
  ⟨a.x + b.x, a.y + b.y, a.z + b.z⟩

defun sub (a b : Vector3) : Vector3 :=
  ⟨a.x - b.x, a.y - b.y, a.z - b.z⟩

defun neg (a : Vector3) : Vector3 :=
  ⟨-a.x, -a.y, -a.z⟩

def smul (a : Vector3) (s : Float) : Vector3 :=
  ⟨s * a.x, s * a.y, s * a.z⟩

instance : HMul Float Vector3 Vector3 := ⟨fun s v => smul v s⟩
instance : HMul Vector3 Float Vector3 := ⟨smul⟩

defun hDiv (a : Vector3) (s : Float) : Vector3 :=
  ⟨a.x / s, a.y / s, a.z / s⟩

defun div (a : Vector3) (b : Vector3) : Vector3 :=
  ⟨a.x / b.x, a.y / b.y, a.z / b.z⟩


-- ============================================================================
-- Vector operations
-- ============================================================================

defun dot (a b : Vector3) : Float :=
  a.x * b.x + a.y * b.y + a.z * b.z

def cross (a b : Vector3) : Vector3 :=
  ⟨a.y * b.z - a.z * b.y,
   a.z * b.x - a.x * b.z,
   a.x * b.y - a.y * b.x⟩

defun length (v : Vector3) : Float :=
  Float.sqrt (v.x * v.x + v.y * v.y + v.z * v.z)

defun length2 (v : Vector3) : Float :=
  v.x * v.x + v.y * v.y + v.z * v.z

defun normalize (v : Vector3) : Vector3 × Float :=
  let len := v.length
  if len == 0 then (v, 0) else (⟨v.x / len, v.y / len, v.z / len⟩, len)

defun normalized (v : Vector3) : Vector3 :=
  v.normalize.1

defun distance (a b : Vector3) : Float :=
  (a.sub b).length

defun distance2 (a b : Vector3) : Float :=
  (a.sub b).length2

defun lerp (a b : Vector3) (t : Float) : Vector3 :=
  ⟨a.x + (b.x - a.x) * t,
   a.y + (b.y - a.y) * t,
   a.z + (b.z - a.z) * t⟩

defun reflect (v n : Vector3) : Vector3 :=
  let d := v.dot n
  ⟨v.x - 2.0 * d * n.x,
   v.y - 2.0 * d * n.y,
   v.z - 2.0 * d * n.z⟩

defun refract (v n : Vector3) (eta : Float) : Vector3 :=
  let dt := v.dot n
  let k := 1.0 - eta * eta * (1.0 - dt * dt)
  if k < 0.0 then ⟨0.0, 0.0, 0.0⟩
  else
    let s := eta * dt + k.sqrt
    ⟨eta * v.x - s * n.x,
     eta * v.y - s * n.y,
     eta * v.z - s * n.z⟩

defun compMul (a b : Vector3) : Vector3 :=
  ⟨a.x * b.x, a.y * b.y, a.z * b.z⟩

defun compDiv (a b : Vector3) : Vector3 :=
  ⟨a.x / b.x, a.y / b.y, a.z / b.z⟩


-- ============================================================================
-- Comparison operations
-- ============================================================================

defun beq (a b : Vector3) : Bool := a.x == b.x && a.y == b.y && a.z == b.z

defun blt (a b : Vector3) : Bool := a.x < b.x && a.y < b.y && a.z < b.z

defun ble (a b : Vector3) : Bool := a.x <= b.x && a.y <= b.y && a.z <= b.z

instance : BEq Vector3 := ⟨beq⟩
instance : LT Vector3 := ⟨fun x y => blt x y = true⟩
instance : LE Vector3 := ⟨fun x y => ble x y = true⟩
instance : DecidableLT Vector3 := by
  intros; simp[DecidableLT, DecidableRel, LT.lt]; infer_instance
instance : DecidableLE Vector3 := by
  intros; simp[DecidableLE, DecidableRel, LE.le]; infer_instance


-- ============================================================================
-- Component-wise operations
-- ============================================================================

defun abs (v : Vector3) : Vector3 :=
  ⟨Float.abs v.x, Float.abs v.y, Float.abs v.z⟩

defun min (a b : Vector3) : Vector3 :=
  ⟨Min.min a.x b.x, Min.min a.y b.y, Min.min a.z b.z⟩

defun max (a b : Vector3) : Vector3 :=
  ⟨Max.max a.x b.x, Max.max a.y b.y, Max.max a.z b.z⟩

defun sign (v : Vector3) : Vector3 :=
  ⟨v.x.sign, v.y.sign, v.z.sign⟩

defun clamp (v lo hi : Vector3) : Vector3 :=
  ⟨v.x.clamp lo.x hi.x, v.y.clamp lo.y hi.y, v.z.clamp lo.z hi.z⟩

defun floor (v : Vector3) : Vector3 :=
  ⟨v.x.floor, v.y.floor, v.z.floor⟩

defun ceil (v : Vector3) : Vector3 :=
  ⟨v.x.ceil, v.y.ceil, v.z.ceil⟩

defun round (v : Vector3) : Vector3 :=
  ⟨v.x.round, v.y.round, v.z.round⟩

defun trunc (v : Vector3) : Vector3 :=
  ⟨v.x.trunc, v.y.trunc, v.z.trunc⟩

defun fract (v : Vector3) : Vector3 :=
  ⟨v.x.fract, v.y.fract, v.z.fract⟩

defun mod (v w : Vector3) : Vector3 :=
  ⟨v.x.mod w.x, v.y.mod w.y, v.z.mod w.z⟩

-- ============================================================================
-- Coordinate system conversions
-- ============================================================================
-- Note: Following Houdini's right-handed coordinate system where Y is up

/-- Convert Cartesian coordinates (x, y, z) to spherical coordinates (r, θ, φ).

Returns: ⟨radius, azimuthal_angle, polar_angle⟩
- r ≥ 0: radial distance
- θ (theta) ∈ (-π, π]: azimuthal angle in the xz-plane from the x-axis
- φ (phi) ∈ [0, π]: polar angle from the positive y-axis (up) -/
def toSpherical (v : Vector3) : Vector3 :=
  let r := v.length
  let theta := Float.atan2 v.z v.x  -- Azimuthal angle: angle in xz-plane
  let phi := Float.acos (v.y / r)   -- Polar angle: angle from y-axis (up)
  ⟨r, theta, phi⟩

/-- Convert spherical coordinates (r, θ, φ) to Cartesian coordinates (x, y, z).

Input: ⟨radius, azimuthal_angle, polar_angle⟩
- r ≥ 0: radial distance
- θ ∈ ℝ: azimuthal angle measured from the x-axis in the xz-plane
- φ ∈ ℝ: polar angle measured from the positive y-axis (up) -/
def fromSpherical (spherical : Vector3) : Vector3 :=
  let r := spherical.x     -- Radial distance
  let theta := spherical.y -- Azimuthal angle
  let phi := spherical.z   -- Polar angle
  ⟨r * Float.sin phi * Float.cos theta,  -- x component
   r * Float.cos phi,                    -- y component (up)
   r * Float.sin phi * Float.sin theta⟩  -- z component

/-- Convert Cartesian coordinates (x, y, z) to geodetic coordinates (lon, lat, alt).

Returns: ⟨longitude, latitude, altitude⟩
- lon ∈ (-π, π]: longitude measured in the xz-plane from the x-axis
- lat ∈ [-π/2, π/2]: latitude measured from the xz-plane toward the y-axis (up)
- alt ≥ 0: altitude as the distance from the origin -/
def toGeodetic (v : Vector3) : Vector3 :=
  let lon := Float.atan2 v.z v.x  -- Longitude: angle in xz-plane
  let lat := Float.atan2 v.y (Float.sqrt (v.x * v.x + v.z * v.z))  -- Latitude: angle from xz-plane
  let alt := v.length  -- Altitude: radial distance
  ⟨lon, lat, alt⟩

/-- Convert geodetic coordinates (lon, lat, alt) to Cartesian coordinates (x, y, z).

Input: ⟨longitude, latitude, altitude⟩
- lon ∈ ℝ: longitude in radians
- lat ∈ ℝ: latitude in radians
- alt ≥ 0: altitude as the radial distance from the origin -/
def fromGeodetic (geodetic : Vector3) : Vector3 :=
  let lon := geodetic.x  -- Longitude
  let lat := geodetic.y  -- Latitude
  let alt := geodetic.z  -- Altitude (radial distance)
  ⟨alt * Float.cos lat * Float.cos lon,  -- x component
   alt * Float.sin lat,                  -- y component (up)
   alt * Float.cos lat * Float.sin lon⟩  -- z component

-- ============================================================================
-- Trigonometric Functions (elementwise)
-- ============================================================================

defun sin (v : Vector3) : Vector3 :=
  ⟨v.x.sin, v.y.sin, v.z.sin⟩

defun cos (v : Vector3) : Vector3 :=
  ⟨v.x.cos, v.y.cos, v.z.cos⟩

defun tan (v : Vector3) : Vector3 :=
  ⟨v.x.tan, v.y.tan, v.z.tan⟩

defun asin (v : Vector3) : Vector3 :=
  ⟨v.x.asin, v.y.asin, v.z.asin⟩

defun acos (v : Vector3) : Vector3 :=
  ⟨v.x.acos, v.y.acos, v.z.acos⟩

defun atan (v : Vector3) : Vector3 :=
  ⟨v.x.atan, v.y.atan, v.z.atan⟩

defun atan2 (v w : Vector3) : Vector3 :=
  ⟨v.x.atan2 w.x, v.y.atan2 w.y, v.z.atan2 w.z⟩

defun sinh (v : Vector3) : Vector3 :=
  ⟨v.x.sinh, v.y.sinh, v.z.sinh⟩

defun cosh (v : Vector3) : Vector3 :=
  ⟨v.x.cosh, v.y.cosh, v.z.cosh⟩

defun tanh (v : Vector3) : Vector3 :=
  ⟨v.x.tanh, v.y.tanh, v.z.tanh⟩

-- ============================================================================
-- Exponential and Logarithmic Functions (elementwise)
-- ============================================================================

defun exp (v : Vector3) : Vector3 :=
  ⟨v.x.exp, v.y.exp, v.z.exp⟩

defun exp2 (v : Vector3) : Vector3 :=
  ⟨v.x.exp2, v.y.exp2, v.z.exp2⟩

defun log (v : Vector3) : Vector3 :=
  ⟨v.x.log, v.y.log, v.z.log⟩

defun log2 (v : Vector3) : Vector3 :=
  ⟨v.x.log2, v.y.log2, v.z.log2⟩

defun log10 (v : Vector3) : Vector3 :=
  ⟨v.x.log10, v.y.log10, v.z.log10⟩

defun pow (v w : Vector3) : Vector3 :=
  ⟨v.x.pow w.x, v.y.pow w.y, v.z.pow w.z⟩

defun sqrt (v : Vector3) : Vector3 :=
  ⟨v.x.sqrt, v.y.sqrt, v.z.sqrt⟩

defun invsqrt (v : Vector3) : Vector3 :=
  ⟨v.x.invsqrt, v.y.invsqrt, v.z.invsqrt⟩

-- ============================================================================
-- Interpolation and Smoothing (elementwise)
-- ============================================================================

defun smoothstep (edge0 edge1 v : Vector3) : Vector3 :=
  ⟨edge0.x.smoothstep edge1.x v.x,
   edge0.y.smoothstep edge1.y v.y,
   edge0.z.smoothstep edge1.z v.z⟩

defun step (edge v : Vector3) : Vector3 :=
  ⟨edge.x.step v.x, edge.y.step v.y, edge.z.step v.z⟩

defun hermite (p0 p1 t0 t1 : Vector3) (t : Float) : Vector3 :=
  ⟨p0.x.hermite p1.x t0.x t1.x t,
   p0.y.hermite p1.y t0.y t1.y t,
   p0.z.hermite p1.z t0.z t1.z t⟩

defun catmullRom (p0 p1 p2 p3 : Vector3) (t : Float) : Vector3 :=
  ⟨p0.x.catmullRom p1.x p2.x p3.x t,
   p0.y.catmullRom p1.y p2.y p3.y t,
   p0.z.catmullRom p1.z p2.z p3.z t⟩

defun slerp (v w : Vector3) (t : Float) : Vector3 :=
  let dot := v.normalized.dot w.normalized
  let dot := dot.clamp (-1.0) 1.0
  let theta := dot.acos
  if theta.abs < 0.001 then v.lerp w t
  else
    let s := theta.sin
    let a := ((1.0 - t) * theta).sin / s
    let b := (t * theta).sin / s
    ⟨a * v.x + b * w.x,
     a * v.y + b * w.y,
     a * v.z + b * w.z⟩

-- ============================================================================
-- Conversion and Construction
-- ============================================================================

defun radians (v : Vector3) : Vector3 :=
  ⟨v.x.radians, v.y.radians, v.z.radians⟩

defun degrees (v : Vector3) : Vector3 :=
  ⟨v.x.degrees, v.y.degrees, v.z.degrees⟩

-- ============================================================================
-- Color and HSV Operations
-- ============================================================================

def rgbToHsv (rgb : Vector3) : Vector3 :=
  let r := rgb.x
  let g := rgb.y
  let b := rgb.z
  let cmax := Max.max (Max.max r g) b
  let cmin := Min.min (Min.min r g) b
  let delta := cmax - cmin

  let h := if delta == 0 then 0
           else if cmax == r then 60.0 * (((g - b) / delta).mod 6.0)
           else if cmax == g then 60.0 * (((b - r) / delta) + 2.0)
           else 60.0 * (((r - g) / delta) + 4.0)

  let s := if cmax == 0 then 0 else delta / cmax
  let v := cmax

  ⟨h, s, v⟩

def hsvToRgb (hsv : Vector3) : Vector3 :=
  let h := hsv.x
  let s := hsv.y
  let v := hsv.z
  let c := v * s
  let x := c * (1.0 - ((h / 60.0).mod 2.0 - 1.0).abs)
  let m := v - c

  let (r', g', b') :=
    if h < 60 then (c, x, 0.0)
    else if h < 120 then (x, c, 0.0)
    else if h < 180 then (0.0, c, x)
    else if h < 240 then (0.0, x, c)
    else if h < 300 then (x, 0.0, c)
    else (c, 0.0, x)

  ⟨r' + m, g' + m, b' + m⟩

def luminance (rgb : Vector3) : Float :=
  0.2126 * rgb.x + 0.7152 * rgb.y + 0.0722 * rgb.z

-- ============================================================================
-- Geometric Queries
-- ============================================================================

defun insideBox (point boxMin boxMax : Vector3) : Bool :=
  point.x >= boxMin.x && point.x <= boxMax.x &&
  point.y >= boxMin.y && point.y <= boxMax.y &&
  point.z >= boxMin.z && point.z <= boxMax.z

defun projectToSegment (point a b : Vector3) : Vector3 :=
  let ab := b - a
  let ap := point - a
  let t := ((ap.dot ab) / (ab.dot ab)).clamp 0.0 1.0
  ⟨a.x + t * ab.x, a.y + t * ab.y, a.z + t * ab.z⟩

-- ============================================================================
-- Additional Vector3 Specific Functions
-- ============================================================================

/-- Triple product: (a × b) · c -/
def tripleProduct (a b c : Vector3) : Float :=
  (a.cross b).dot c

/-- Angle between two vectors in radians. -/
def angleBetween (v w : Vector3) : Float :=
  let dot := v.normalized.dot w.normalized
  dot.clamp (-1.0) 1.0 |> (·.acos)

/-- Project vector v onto w. -/
def project (v w : Vector3) : Vector3 :=
  let d := v.dot w
  let len2 := w.length2
  if len2 == 0.0 then ⟨0.0, 0.0, 0.0⟩
  else ⟨w.x * d / len2, w.y * d / len2, w.z * d / len2⟩

/-- Quantize vector to grid. -/
def quantize (v : Vector3) (step : Float) : Vector3 :=
  ⟨Float.round (v.x / step) * step,
   Float.round (v.y / step) * step,
   Float.round (v.z / step) * step⟩

end HouLean.Vector3
