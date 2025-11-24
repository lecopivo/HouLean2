import HouLean.Meta.OverloadedFunction
import HouLean.Data.Defs

namespace HouLean.Math

set_option linter.unusedVariables false

-- ============================================================================
-- Trigonometric Functions
-- ============================================================================

/-- Sine function.

Applied elementwise for vectors and matrices. -/
declfun sin {α} (x : α) : α

/-- Cosine function.

Applied elementwise for vectors and matrices. -/
declfun cos {α} (x : α) : α

/-- Tangent function.

Applied elementwise for vectors and matrices. -/
declfun tan {α} (x : α) : α

/-- Arcsine function. Returns angle in radians.

Applied elementwise for vectors and matrices. -/
declfun asin {α} (x : α) : α

/-- Arccosine function. Returns angle in radians.

Applied elementwise for vectors and matrices. -/
declfun acos {α} (x : α) : α

/-- Arctangent function. Returns angle in radians.

Applied elementwise for vectors and matrices. -/
declfun atan {α} (x : α) : α

/-- Two-argument arctangent. Returns `atan2(y, x)` in radians.

Useful for converting Cartesian coordinates to polar coordinates. -/
declfun atan2 {α} (y x : α) : α

/-- Hyperbolic sine function.

Applied elementwise for vectors and matrices. -/
declfun sinh {α} (x : α) : α

/-- Hyperbolic cosine function.

Applied elementwise for vectors and matrices. -/
declfun cosh {α} (x : α) : α

/-- Hyperbolic tangent function.

Applied elementwise for vectors and matrices. -/
declfun tanh {α} (x : α) : α

-- ============================================================================
-- Exponential and Logarithmic Functions
-- ============================================================================

/-- Exponential function. Returns `e^x`.

Applied elementwise for vectors and matrices. -/
declfun exp {α} (x : α) : α

/-- Base-2 exponential. Returns `2^x`.

Applied elementwise for vectors and matrices. -/
declfun exp2 {α} (x : α) : α

/-- Natural logarithm (base e).

Applied elementwise for vectors and matrices. -/
declfun log {α} (x : α) : α

/-- Base-2 logarithm.

Applied elementwise for vectors and matrices. -/
declfun log2 {α} (x : α) : α

/-- Base-10 logarithm.

Applied elementwise for vectors and matrices. -/
declfun log10 {α} (x : α) : α

/-- Square root function.

Applied elementwise for vectors and matrices. -/
declfun sqrt {α} (x : α) : α

/-- Inverse square root. Returns `1/sqrt(x)`.

Applied elementwise for vectors and matrices. -/
declfun invsqrt {α} (x : α) : α

-- ============================================================================
-- Basic Arithmetic and Comparison
-- ============================================================================

/-- Absolute value function.

Applied elementwise for vectors and matrices. -/
declfun abs {α} (x : α) : α

/-- Sign function. Returns -1, 0, or 1.

Applied elementwise for vectors and matrices. -/
declfun sign {α} (x : α) : α

/-- Clamp value between minimum and maximum.

`clamp x lo hi` returns `x` clamped to the range `[lo, hi]`.
Applied elementwise for vectors and matrices. -/
declfun clamp {α} (x lo hi : α) : α

/-- Floor function. Returns largest integer ≤ x.

Applied elementwise for vectors and matrices. -/
declfun floor {α} (x : α) : α

/-- Ceiling function. Returns smallest integer ≥ x.

Applied elementwise for vectors and matrices. -/
declfun ceil {α} (x : α) : α

/-- Round to nearest integer.

Applied elementwise for vectors and matrices. -/
declfun round {α} (x : α) : α

/-- Truncate towards zero.

Applied elementwise for vectors and matrices. -/
declfun trunc {α} (x : α) : α

/-- Fractional part. Returns `x - floor(x)`.

Applied elementwise for vectors and matrices. -/
declfun fract {α} (x : α) : α

/- Modulo operation. Returns remainder of `x/y`.

Applied elementwise for vectors and matrices. -/
-- declfun mod {α} (x y : α) : α

-- ============================================================================
-- Vector Operations
-- ============================================================================

/-- Dot product between vectors or matrices.

For vectors, computes the sum of component-wise products.
For matrices, computes trace of `xᵀ * y`. -/
declfun dot {α} (x y : α) : Float

/-- Cross product of 3D vectors.

Returns a vector perpendicular to both input vectors. -/
declfun cross (x y : Vector3) : Vector3

/-- Length of a vector or matrix. Also known as l₂ norm or Euclidean norm.

For vectors, returns `sqrt(dot(x, x))`.
For matrices, returns Frobenius norm. -/
declfun length {α} (x : α) : Float

/-- Squared length of a vector or matrix.

More efficient than `length` when only comparing magnitudes.
Returns `dot(x, x)`. -/
declfun length2 {α} (x : α) : Float

/-- Distance between two points.

Returns `length(y - x)`. -/
declfun distance {α} (x y : α) : Float

/-- Squared distance between two points.

More efficient than `distance` when only comparing distances.
Returns `length2(y - x)`. -/
declfun distance2 {α} (x y : α) : Float

/-- Normalize a vector and return both the normalized vector and its original length.

`normalize x = (normalized x, length x)` -/
declfun normalize {α} (x : α) : α × Float

/-- Normalize a vector to unit length.

Returns zero vector if input length is zero. -/
declfun normalized {α} (x : α) : α

/-- Reflect vector across a normal.

`reflect v n` reflects vector `v` across surface with normal `n`.
Normal `n` should be normalized. -/
declfun reflect {α} (v n : α) : α

/-- Refract vector through a surface.

`refract v n eta` refracts vector `v` through surface with normal `n`
using ratio of indices of refraction `eta`.
Normal `n` should be normalized.
Returns zero vector if total internal reflection occurs. -/
declfun refract {α} (v n : α) (eta : Float) : α

/-- Component-wise multiplication.

Also known as Hadamard product or Schur product. -/
declfun compMul {α} (x y : α) : α

/-- Component-wise division. -/
declfun compDiv {α} (x y : α) : α


-- ============================================================================
-- Comparison operations
-- ============================================================================

/-- Exact equality test. -/
declfun beq {α} (x y : α) : Bool

/-- Less than. -/
declfun blt {α} (x y : α) : Bool

/-- Less or equal than. -/
declfun ble {α} (x y : α) : Bool

-- ============================================================================
-- Interpolation and Smoothing
-- ============================================================================

/-- Linear interpolation between `x` and `y`.

`lerp x y 0 = x` and `lerp x y 1 = y`.
Applied elementwise for vectors and matrices. -/
declfun lerp {α} (x y : α) (t : Float) : α

/-- Smooth Hermite interpolation.

Returns smooth interpolation between 0 and 1 when `x` goes from `edge0` to `edge1`.
`smoothstep edge0 edge1 x` is 0 when `x ≤ edge0`, 1 when `x ≥ edge1`,
and smoothly interpolates between using Hermite polynomial when `edge0 < x < edge1`. -/
declfun smoothstep {α} (edge0 edge1 x : α) : α

/-- Step function.

Returns 0 if `x < edge`, otherwise 1.
Applied elementwise for vectors and matrices. -/
declfun step {α} (edge x : α) : α

/-- Cubic Hermite spline interpolation.

Interpolates between points with specified tangents. -/
declfun hermite {α} (p0 p1 t0 t1 : α) (t : Float) : α

/-- Catmull-Rom spline interpolation.

Interpolates smoothly through control points. -/
declfun catmullRom {α} (p0 p1 p2 p3 : α) (t : Float) : α

/-- Spherical linear interpolation for unit vectors.

Interpolates along the great circle between two unit vectors.
More appropriate than `lerp` for rotations and directions. -/
declfun slerp {α} (x y : α) (t : Float) : α


-- ============================================================================
-- Conversion and Construction
-- ============================================================================

/-- Convert degrees to radians. -/
declfun radians {α} (degrees : α) : α

/-- Convert radians to degrees. -/
declfun degrees {α} (radians : α) : α


-- ============================================================================
-- Color and HSV Operations
-- ============================================================================

/-- Convert RGB color to HSV.

Input/output ranges: R,G,B in [0,1], H in [0,360], S,V in [0,1]. -/
declfun rgbToHsv (rgb : Vector3) : Vector3

/-- Convert HSV color to RGB.

Input/output ranges: H in [0,360], S,V in [0,1], R,G,B in [0,1]. -/
declfun hsvToRgb (hsv : Vector3) : Vector3

/-- Luminance of RGB color using perceptual weights. -/
declfun luminance (rgb : Vector3) : Float

-- ============================================================================
-- Geometric Queries
-- ============================================================================

/-- Check if point is inside axis-aligned bounding box. -/
declfun insideBox {α} (point boxMin boxMax : α) : Bool

/-- Project point onto line segment.

Returns the closest point on segment from `a` to `b`. -/
declfun projectToSegment {α} (point a b : α) : α


-- ============================================================================
-- Transformations
-- ============================================================================

/-- Transform point. -/
declfun transformPoint {T} {P} (transform : T) (point : P) : P

/-- Transform vector. This usually ignores translation vector of a trasformation. -/
declfun transformVector {T} {V} (transform : T) (vector : V) : V

/-- Transform normal. -/
declfun transformNormal {T} {N} (transform : T) (normal : N) : N


end HouLean.Math
