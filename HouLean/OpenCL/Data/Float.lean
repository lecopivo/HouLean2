import HouLean.OpenCL.Compiler.Main
import HouLean.Data.Float
import HouLean.OpenCL.Data.Bool

namespace HouLean.OpenCL

open Compiler Qq

-- bootstrap types
run_meta addOCLType q(Float) (.atom "double" "d")
run_meta addOCLType q(Float32) (.atom "float" "f")

run_meta addOCLFunction q(Float.neg) " -" (kind := .prefix)
run_meta addOCLFunction q(Float.add) " + " (kind := .infix)
run_meta addOCLFunction q(Float.sub) " - " (kind := .infix)
run_meta addOCLFunction q(Float.mul) " * " (kind := .infix)
run_meta addOCLFunction q(Float.div) " / " (kind := .infix)

run_meta addOCLFunction q(fun x y : Float => x + y) " + " (kind := .infix)
run_meta addOCLFunction q(fun x y : Float => x - y) " - " (kind := .infix)
run_meta addOCLFunction q(fun x y : Float => x * y) " * " (kind := .infix)
run_meta addOCLFunction q(fun x y : Float => x / y) " / " (kind := .infix)
run_meta addOCLFunction q(fun x : Float => - x) " -" (kind := .prefix)



-- ============================================================================
-- Trigonometric Functions
-- ============================================================================

-- Built-in Float methods
run_meta addOCLFunction q(Float.sin) "sin"
run_meta addOCLFunction q(Float.cos) "cos"
run_meta addOCLFunction q(Float.tan) "tan"
run_meta addOCLFunction q(Float.asin) "asin"
run_meta addOCLFunction q(Float.acos) "acos"
run_meta addOCLFunction q(Float.atan) "atan"
run_meta addOCLFunction q(Float.atan2) "atan2"
run_meta addOCLFunction q(Float.sinh) "sinh"
run_meta addOCLFunction q(Float.cosh) "cosh"
run_meta addOCLFunction q(Float.tanh) "tanh"


-- ============================================================================
-- Exponential and Logarithmic Functions
-- ============================================================================

-- Built-in Float methods
run_meta addOCLFunction q(Float.exp) "exp"
run_meta addOCLFunction q(Float.exp2) "exp2"
run_meta addOCLFunction q(Float.log) "log"
run_meta addOCLFunction q(Float.log2) "log2"
run_meta addOCLFunction q(Float.log10) "log10"
run_meta addOCLFunction q(Float.pow) "pow"
run_meta addOCLFunction q(Float.sqrt) "sqrt"

-- run_meta compileFunction q(Float.invsqrt)

-- ============================================================================
-- Comparison operations
-- ============================================================================

--run_meta addOCLFunction q(fun x y : Float => x = y) "==" (kind := .infix)
run_meta addOCLFunction q(fun x y : Float => x == y) "==" (kind := .infix)
-- run_meta addOCLFunction q(fun x y : Float => x < y) "<" (kind := .infix)
-- run_meta addOCLFunction q(fun x y : Float => x ≤ y) "<=" (kind := .infix

-- ============================================================================
-- Basic Arithmetic and Comparison
-- ============================================================================

-- Custom implementations
run_meta addOCLFunction q(Float.abs) "fabs"
-- run_meta compileFunction q(Float.sign)
run_meta addOCLFunction q(Float.abs) "fabs"
run_meta addOCLFunction q(fun x y : Float => min x y) "min"
run_meta addOCLFunction q(fun x y : Float => max x y) "max"
-- run_meta compileFunction q(Float.clamp)
run_meta addOCLFunction q(Float.floor) "floor"
run_meta addOCLFunction q(Float.ceil) "ceil"
run_meta addOCLFunction q(Float.round) "round"
run_meta addOCLFunction q(Float.trunc) "trunc"
run_meta addOCLFunction q(Float.fract) "fract"
run_meta addOCLFunction q(Float.mod) "fmod"

-- ============================================================================
-- Vector Operations (scalar versions)
-- ============================================================================

run_meta compileFunction q(Float.dot)
run_meta compileFunction q(Float.length)
run_meta compileFunction q(Float.length2)
run_meta compileFunction q(Float.distance)
run_meta compileFunction q(Float.distance2)
-- run_meta compileFunction q(Float.noramlize)
-- run_meta compileFunction q(Float.noramlized)
-- run_meta compileFunction q(Float.reflect)
-- run_meta compileFunction q(Float.refract)
run_meta compileFunction q(Float.compMul)
run_meta compileFunction q(Float.compDiv)


-- ============================================================================
-- Interpolation and Smoothing
-- ============================================================================

run_meta compileFunction q(Float.lerp)
-- run_meta compileFunction q(Float.smoothstep)
-- run_meta compileFunction q(Float.step)
-- run_meta compileFunction q(Float.hermite)
-- run_meta compileFunction q(Float.catmullRom)
run_meta compileFunction q(Float.slerp)


-- ============================================================================
-- Conversion and Construction
-- ============================================================================

-- run_meta compileFunction q(Float.radians)
-- run_meta compileFunction q(Float.degrees)


-- ============================================================================
-- Geometric Queries (scalar versions)
-- ============================================================================

-- run_meta compileFunction q(Float.insideBox)
-- run_meta compileFunction q(Float.projectToSegment)
