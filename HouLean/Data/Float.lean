import HouLean.Math
import HouLean.Init

namespace HouLean

open Math

inductive _root_.Float.Precision where
  -- | quater -- 8 bit
  -- | half -- 16 bit
  | single -- 32 bit
  | double -- 64 bit
  -- | quadruple -- 128 bit
  -- | octuple -- 256 bit

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
  prec : Float.Precision

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
  prec := .single

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
  prec := .double

def Math.pi {R : Type} [FloatType R] : R := Math.acos (-1.0)

instance : Mod R where
  mod x y :=
    let q := floor (x / y)
    x - q * y

instance : Sign R where
  sign x :=
    if x < 0.0 then -1.0
    else if x > 0.0 then 1.0
    else 0.0

defun clamp (x lo hi : R) : R :=
  if x < lo then lo else if x > hi then hi else x


-- ============================================================================
-- Approximatelly equal
-- ============================================================================

instance : ApproxEqual Float where
  defaultTol := 1e-9
  approxEqual x y tol := (x - y).abs ≤ tol

instance : ApproxEqual Float32 where
  defaultTol := 1e-6
  approxEqual x y tol := (x - y).abs ≤ tol


-- ============================================================================
-- Basic Arithmetic and Comparison
-- ============================================================================

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

defun abs (x : Float32) : Float32 := x.abs

defun Float32.sign (x : Float32) : Float32 :=
  if x < 0.0 then -1.0 else if x > 0.0 then 1.0 else 0.0

defun Float32.min (x y : Float32) : Float32 := _root_.Min.min x y

defun Float32.max (x y : Float32) : Float32 := _root_.Max.max x y

defun Float32.clamp (x lo hi : Float32) : Float32 :=
  if x < lo then lo else if x > hi then hi else x

-- Built-in Float32 methods
defun floor (x : Float32) := x.floor
defun ceil (x : Float32) := x.ceil

-- Custom implementations
defun round (x : Float32) : Float32 := x.round

defun Float32.trunc (x : Float32) : Float32 :=
  if x >= 0.0 then x.floor else x.ceil

defun Float32.fract (x : Float32) : Float32 := x - x.floor

defun Float32.mod (x y : Float32) : Float32 :=
  let q := (x / y).floor
  x - q * y


-- ============================================================================
-- Vector Operations (scalar versions)
-- ============================================================================

-- todo: generalize to `R` [FloatType R]
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

defun lerp (x y t : R) := x + (y - x) * t

defun smoothstep (x edge0 edge1 : R) :=
  let t := clamp x edge0 edge1
  let t := (t - edge0) / (edge1 - edge0)
  t * t * (3.0 - 2.0 * t)

defun step (x edge : R) : R :=
  if x < edge then 0.0 else 1.0

defun hermite (p0 p1 t0 t1 t : R) :=
  let t2 := t * t
  let t3 := t2 * t
  let h00 := 2.0 * t3 - 3.0 * t2 + 1.0
  let h10 := t3 - 2.0 * t2 + t
  let h01 := -2.0 * t3 + 3.0 * t2
  let h11 := t3 - t2
  h00 * p0 + h10 * t0 + h01 * p1 + h11 * t1

defun catmullRom (p0 p1 p2 p3 t : R) :=
  let t2 := t * t
  let t3 := t2 * t
  0.5 * ((2.0 * p1) +
         (-p0 + p2) * t +
         (2.0 * p0 - 5.0 * p1 + 4.0 * p2 - p3) * t2 +
         (-p0 + 3.0 * p1 - 3.0 * p2 + p3) * t3)

defun slerp (x y t : R) := lerp x y t


-- ============================================================================
-- Conversion and Construction
-- ============================================================================

-- todo: generalize to `R` [FloatType R]
defun Float.radians (degrees : Float) : Float := degrees * (3.141592653589793 / 180.0)
defun Float.degrees (radians : Float) : Float := radians * (180.0 / 3.141592653589793)


-- ============================================================================
-- Geometric Queries (scalar versions)
-- ============================================================================

defun insideBox (point boxMin boxMax : R) :=
  boxMin <= point && point <= boxMax

defun projectToSegment (point a b : R) :=
  clamp point (min a b) (max a b)
