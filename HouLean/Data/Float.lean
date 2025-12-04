import HouLean.Math

namespace HouLean

open Math

/-- Interface for `Float32` and `Float64` -/
class FloatType (R : Type) extends
    Add R, Sub R, Neg R, Mul R, Div R, One R, Zero R, Pow R R,
    -- comparison
    LT R, LE R, BEq R,
    -- trigonometric
    Sin R, Cos R, Tan R, Asin R, Acos R, Atan R, Atan2 R, Sinh R, Cosh R, Tanh R,
    -- exponential and log
    Exp R, Exp2 R, Log R, Log2 R, Log10 R, Sqrt R,
    -- rounding
    Abs R, Floor R, Ceil R, Round R,
    -- scientific notation
    OfScientific R
  where

  decLt : DecidableLT R
  decLe : DecidableLE R

variable {R} [FloatType R]

instance : DecidableLT R := FloatType.decLt
instance : DecidableLE R := FloatType.decLe
instance : Min R := minOfLe
instance : Max R := maxOfLe

instance : FloatType Float where
  sin := .sin
  cos := .cos
  tan := .tan
  asin := .asin
  acos := .acos
  atan := .atan
  atan2 := .atan2
  sinh := .sinh
  cosh := .cosh
  tanh := .tanh
  exp := .exp
  exp2 := .exp2
  log := .log
  log2 := .log2
  log10 := .log10
  sqrt := .sqrt
  abs := .abs
  floor := .floor
  ceil := .ceil
  round := .round
  decLt := by infer_instance
  decLe := by infer_instance

instance : FloatType Float32 where
  sin := .sin
  cos := .cos
  tan := .tan
  asin := .asin
  acos := .acos
  atan := .atan
  atan2 := .atan2
  sinh := .sinh
  cosh := .cosh
  tanh := .tanh
  exp := .exp
  exp2 := .exp2
  log := .log
  log2 := .log2
  log10 := .log10
  sqrt := .sqrt
  abs := .abs
  floor := .floor
  ceil := .ceil
  round := .round
  decLt := by infer_instance
  decLe := by infer_instance

def Math.pi {R : Type} [FloatType R] : R := Math.acos (-1.0)

instance : Mod R  where
  mod x y :=
    let q := floor (x / y)
    x - q * y

instance : Sign R where
  sign x :=
    if x < 0.0 then -1.0
    else if x > 0.0 then 1.0
    else 0.0

instance : Clamp R where
  clamp x lo hi :=
    if x < lo then lo
    else if x > hi then hi
    else x

-- defun clamp (x lo hi : R) : R :=
--   if x < lo then lo
--   else if x > hi then hi
--   else x


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

instance : Lerp R R where
  lerp x y t := x + (y - x) * t

instance : Smoothstep R where
  smoothstep x edge0 edge1 :=
    let t := clamp x edge0 edge1
    let t := (t - edge0) / (edge1 - edge0)
    t * t * (3.0 - 2.0 * t)

instance : Step R where
  step x edge := if x < edge then 0.0 else 1.0

instance : Hermite R R where
  hermite p0 p1 t0 t1 t :=
    let t2 := t * t
    let t3 := t2 * t
    let h00 := 2.0 * t3 - 3.0 * t2 + 1.0
    let h10 := t3 - 2.0 * t2 + t
    let h01 := -2.0 * t3 + 3.0 * t2
    let h11 := t3 - t2
    h00 * p0 + h10 * t0 + h01 * p1 + h11 * t1

instance : CatmullRom R R where
  catmullRom p0 p1 p2 p3 t :=
    let t2 := t * t
    let t3 := t2 * t
    0.5 * ((2.0 * p1) +
           (-p0 + p2) * t +
           (2.0 * p0 - 5.0 * p1 + 4.0 * p2 - p3) * t2 +
           (-p0 + 3.0 * p1 - 3.0 * p2 + p3) * t3)

instance : Slerp R R where
  slerp x y t := lerp x y t


-- ============================================================================
-- Conversion and Construction
-- ============================================================================

defun Float.radians (degrees : Float) : Float := degrees * (3.141592653589793 / 180.0)
defun Float.degrees (radians : Float) : Float := radians * (180.0 / 3.141592653589793)


-- ============================================================================
-- Geometric Queries (scalar versions)
-- ============================================================================

instance : InsideBox R where
  insideBox point boxMin boxMax :=
    boxMin <= point && point <= boxMax

instance : ProjectToSegment R where
  projectToSegment point a b :=
    clamp point (min a b) (max a b)
