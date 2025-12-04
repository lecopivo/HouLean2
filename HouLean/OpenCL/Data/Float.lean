import HouLean.OpenCL.Compiler.Main
import HouLean.Data.Float
import HouLean.OpenCL.Data.Bool

namespace HouLean.OpenCL

open Compiler Qq

-- ============================================================================
-- Trigonometric Functions
-- ============================================================================

-- Built-in Float methods
implemented_by : Float.sin = oclFunction _ "sin"
implemented_by : Float.cos = oclFunction _ "cos"
implemented_by : Float.tan = oclFunction _ "tan"
implemented_by : Float.asin = oclFunction _ "asin"
implemented_by : Float.acos = oclFunction _ "acos"
implemented_by : Float.atan = oclFunction _ "atan"
implemented_by : Float.atan2 = oclFunction _ "atan2"
implemented_by : Float.sinh = oclFunction _ "sinh"
implemented_by : Float.cosh = oclFunction _ "cosh"
implemented_by : Float.tanh = oclFunction _ "tanh"

implemented_by : Float32.sin = oclFunction _ "sin"
implemented_by : Float32.cos = oclFunction _ "cos"
implemented_by : Float32.tan = oclFunction _ "tan"
implemented_by : Float32.asin = oclFunction _ "asin"
implemented_by : Float32.acos = oclFunction _ "acos"
implemented_by : Float32.atan = oclFunction _ "atan"
implemented_by : Float32.atan2 = oclFunction _ "atan2"
implemented_by : Float32.sinh = oclFunction _ "sinh"
implemented_by : Float32.cosh = oclFunction _ "cosh"
implemented_by : Float32.tanh = oclFunction _ "tanh"


-- ============================================================================
-- Exponential and Logarithmic Functions
-- ============================================================================

-- Built-in Float methods
implemented_by : Float.exp = oclFunction _ "exp"
implemented_by : Float.exp2 = oclFunction _ "exp2"
implemented_by : Float.log = oclFunction _ "log"
implemented_by : Float.log2 = oclFunction _ "log2"
implemented_by : Float.log10 = oclFunction _ "log10"
implemented_by : Float.pow = oclFunction _ "pow"
implemented_by : Float.sqrt = oclFunction _ "sqrt"
-- run_meta compileFunction q(Float.invsqrt)

implemented_by : Float32.exp = oclFunction _ "exp"
implemented_by : Float32.exp2 = oclFunction _ "exp2"
implemented_by : Float32.log = oclFunction _ "log"
implemented_by : Float32.log2 = oclFunction _ "log2"
implemented_by : Float32.log10 = oclFunction _ "log10"
implemented_by : Float32.pow = oclFunction _ "pow"
implemented_by : Float32.sqrt = oclFunction _ "sqrt"


-- ============================================================================
-- Comparison operations
-- ============================================================================

--run_meta addOCLFunction q(fun x y : Float => x = y) "==" (kind := .infix)
implemented_by (x y : Float) : (x == y) = (oclFunction (Float → Float → Bool) " == " .infix) x y
implemented_by (x y : Float) : decide (x < y) = (oclFunction (Float → Float → Bool) " < " .infix) x y
implemented_by (x y : Float) : decide (x ≤ y) = (oclFunction (Float → Float → Bool) " <= " .infix) x y
implemented_by (x y : Float) : decide (x > y) = (oclFunction (Float → Float → Bool) " > " .infix) x y
implemented_by (x y : Float) : decide (x ≥ y) = (oclFunction (Float → Float → Bool) " >= " .infix) x y

implemented_by (x y : Float32) : (x == y) = (oclFunction (Float32 → Float32 → Bool) " == " .infix) x y
implemented_by (x y : Float32) : decide (x < y) = (oclFunction (Float32 → Float32 → Bool) " < " .infix) x y
implemented_by (x y : Float32) : decide (x ≤ y) = (oclFunction (Float32 → Float32 → Bool) " <= " .infix) x y
implemented_by (x y : Float32) : decide (x > y) = (oclFunction (Float32 → Float32 → Bool) " > " .infix) x y
implemented_by (x y : Float32) : decide (x ≥ y) = (oclFunction (Float32 → Float32 → Bool) " >= " .infix) x y


-- ============================================================================
-- Basic Arithmetic and Comparison
-- ============================================================================

-- Custom implementations
implemented_by : Float.abs = oclFunction _ "fabs"
implemented_by (x y : Float) : min x y = (oclFunction (Float → Float → Float) "min") x y
implemented_by (x y : Float) : max x y = (oclFunction (Float → Float → Float) "max") x y
implemented_by : Float.floor = oclFunction _ "floor"
implemented_by : Float.ceil = oclFunction _ "ceil"
implemented_by : Float.round = oclFunction _ "round"

implemented_by : Float32.abs = oclFunction _ "fabs"
implemented_by (x y : Float32) : min x y = (oclFunction (Float32 → Float32 → Float32) "min") x y
implemented_by (x y : Float32) : max x y = (oclFunction (Float32 → Float32 → Float32) "max") x y
implemented_by : Float32.floor = oclFunction _ "floor"
implemented_by : Float32.ceil = oclFunction _ "ceil"
implemented_by : Float32.round = oclFunction _ "round"

-- implemented_by : Float.trunc = oclFunction _ "trunc"
-- implemented_by : Float.fract = oclFunction _ "fract"
-- implemented_by : Float.mod = oclFunction _ "fmod"


-- ============================================================================
-- Vector Operations (scalar versions)
-- ============================================================================

-- attribute [opencl_compile]
--   Float.dot
--   Float.length
--   Float.length2
--   Float.distance
--   Float.distance2
--   Float.normalize
--   Float.normalized
--   Float.reflect
--   Float.refract
--   Float.compMul
--   Float.compDiv

-- #opencl_compile Float.dot
-- #opencl_compile Float.length
-- #opencl_compile Float.length2
-- #opencl_compile Float.distance
-- #opencl_compile Float.distance2
-- -- #opencl_compile Float.normalize
-- -- #opencl_compile Float.normalized
-- #opencl_compile Float.reflect
-- -- #opencl_compile Float.refract
-- #opencl_compile Float.compMul
-- #opencl_compile Float.compDiv


-- ============================================================================
-- Interpolation and Smoothing
-- ============================================================================

-- #opencl_compile Float.lerp
-- -- #opencl_compile Float.smoothstep
-- -- #opencl_compile Float.step
-- #opencl_compile Float.hermite
-- #opencl_compile Float.catmullRom
-- #opencl_compile Float.slerp


-- ============================================================================
-- Conversion and Construction
-- ============================================================================

-- #opencl_compile Float.radians
-- #opencl_compile Float.degrees

-- ============================================================================
-- Geometric Queries (scalar versions)
-- ============================================================================

-- #opencl_compile Float.insideBox
-- #opencl_compile Float.projectToSegment
