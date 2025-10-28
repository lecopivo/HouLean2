import HouLean.Math

namespace HouLean.Math

-- ============================================================================
-- Trigonometric Functions
-- ============================================================================

-- Built-in Float methods
defun sin (x : Float) := x.sin
defun cos (x : Float) := x.cos
defun tan (x : Float) := x.tan
defun asin (x : Float) := x.asin
defun acos (x : Float) := x.acos
defun atan (x : Float) := x.atan
defun atan2 (y x : Float) := y.atan2 x
defun sinh (x : Float) := x.sinh
defun cosh (x : Float) := x.cosh
defun tanh (x : Float) := x.tanh

-- ============================================================================
-- Exponential and Logarithmic Functions
-- ============================================================================

-- Built-in Float methods
defun exp (x : Float) := x.exp
defun exp2 (x : Float) := x.exp2
defun log (x : Float) := x.log
defun log2 (x : Float) := x.log2
defun log10 (x : Float) := x.log10
defun pow (x y : Float) := x.pow y
defun sqrt (x : Float) := x.sqrt

-- Custom implementations
defun Float.invsqrt (x : Float) : Float := 1.0 / x.sqrt

-- ============================================================================
-- Comparison operations
-- ============================================================================

defun beq (x y : Float) : Bool := x.beq y
defun Float.blt (x y : Float) : Bool := (x < y : Bool)
defun Float.ble (x y : Float) : Bool := (x <= y : Bool)

-- ============================================================================
-- Basic Arithmetic and Comparison
-- ============================================================================

-- Custom implementations
defun abs (x : Float) : Float := x.abs

defun Float.sign (x : Float) : Float := 
  if x < 0.0 then -1.0 else if x > 0.0 then 1.0 else 0.0

defun Float.min (x y : Float) : Float := _root_.Min.min x y

defun Float.max (x y : Float) : Float := _root_.Max.max x y

defun Float.clamp (x lo hi : Float) : Float := 
  if x < lo then lo else if x > hi then hi else x

-- Built-in Float methods
defun floor (x : Float) := x.floor
defun ceil (x : Float) := x.ceil

-- Custom implementations
defun round (x : Float) : Float := x.round

defun Float.trunc (x : Float) : Float := 
  if x >= 0.0 then x.floor else x.ceil

defun Float.fract (x : Float) : Float := x - x.floor

defun Float.mod (x y : Float) : Float :=
  let q := (x / y).floor
  x - q * y

-- ============================================================================
-- Vector Operations (scalar versions)
-- ============================================================================

defun Float.dot (x y : Float) : Float := x * y

defun Float.length (x : Float) : Float := x.abs

defun Float.length2 (x : Float) : Float := x * x

defun Float.distance (x y : Float) : Float := (y - x).abs

defun Float.distance2 (x y : Float) : Float := 
  let d := y - x
  d * d

defun Float.normalize (x : Float) : Float × Float := 
  let len := x.abs
  if len == 0.0 then (0.0, 0.0) else (x / len, len)

defun Float.normalized (x : Float) : Float := 
  if x == 0.0 then 0.0 else if x > 0.0 then 1.0 else -1.0

defun Float.reflect (v n : Float) : Float := v - 2.0 * (v * n) * n

defun Float.refract (v n : Float) (eta : Float) : Float := 
  let dt := v * n
  let k := 1.0 - eta * eta * (1.0 - dt * dt)
  if k < 0.0 then 0.0 else eta * v - (eta * dt + k.sqrt) * n

defun Float.compMul (x y : Float) : Float := x * y

defun Float.compDiv (x y : Float) : Float := x / y

-- ============================================================================
-- Interpolation and Smoothing
-- ============================================================================

defun Float.lerp (x y : Float) (t : Float) : Float := x + (y - x) * t

defun Float.smoothstep (edge0 edge1 x : Float) : Float := 
  let t := x.clamp edge0 edge1
  let t := (t - edge0) / (edge1 - edge0)
  t * t * (3.0 - 2.0 * t)

defun Float.step (edge x : Float) : Float := if x < edge then 0.0 else 1.0

defun Float.hermite (p0 p1 t0 t1 : Float) (t : Float) : Float := 
  let t2 := t * t
  let t3 := t2 * t
  let h00 := 2.0 * t3 - 3.0 * t2 + 1.0
  let h10 := t3 - 2.0 * t2 + t
  let h01 := -2.0 * t3 + 3.0 * t2
  let h11 := t3 - t2
  h00 * p0 + h10 * t0 + h01 * p1 + h11 * t1

defun Float.catmullRom (p0 p1 p2 p3 : Float) (t : Float) : Float := 
  let t2 := t * t
  let t3 := t2 * t
  0.5 * ((2.0 * p1) +
         (-p0 + p2) * t +
         (2.0 * p0 - 5.0 * p1 + 4.0 * p2 - p3) * t2 +
         (-p0 + 3.0 * p1 - 3.0 * p2 + p3) * t3)

defun Float.slerp (x y : Float) (t : Float) : Float := x.lerp y t


-- ============================================================================
-- Conversion and Construction
-- ============================================================================

defun Float.radians (degrees : Float) : Float := degrees * (3.141592653589793 / 180.0)

defun Float.degrees (radians : Float) : Float := radians * (180.0 / 3.141592653589793)

-- ============================================================================
-- Geometric Queries (scalar versions)
-- ============================================================================

defun Float.insideBox (point boxMin boxMax : Float) : Bool := 
  point >= boxMin && point <= boxMax

defun Float.projectToSegment (point a b : Float) : Float := 
  let ab := b - a
  let ap := point - a
  let t := ((ap * ab) / (ab * ab)).clamp 0.0 1.0
  a + t * ab

end HouLean.Math


