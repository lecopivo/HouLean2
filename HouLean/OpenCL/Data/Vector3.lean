import HouLean.Data.Vector3
import HouLean.OpenCL.Data.Float
import HouLean.OpenCL.Data.Bool

namespace HouLean.OpenCL

open Compiler Qq HouLean Math

run_meta addOCLType q(Vector3) (.atom "float3" "f3")


run_meta addOCLFunction q(Vector3.mk) "(float3)" (kind := .constructor)
run_meta addOCLFunction q(Vector3.x) ".x" (kind := .postfix)
run_meta addOCLFunction q(Vector3.y) ".y" (kind := .postfix)
run_meta addOCLFunction q(Vector3.z) ".z" (kind := .postfix)


run_meta addOCLFunction q(Vector3.add) "+" (kind :=.infix)
run_meta addOCLFunction q(fun x y : Vector3 => x + y) "+" (kind :=.infix)

run_meta addOCLFunction q(Vector3.sub) "-" (kind :=.infix)
run_meta addOCLFunction q(fun x y : Vector3 => x - y) "-" (kind :=.infix)

run_meta addOCLFunction q(Vector3.neg) "-" (kind :=.prefix)
run_meta addOCLFunction q(fun x : Vector3 => - x) "-" (kind :=.prefix)

run_meta addOCLFunction q(Vector3.smul) "*" (kind :=.infix)
run_meta addOCLFunction q(fun (x : Vector3) (s : Float) => x * s) "*" (kind :=.infix)
run_meta addOCLFunction q(fun (s : Float) (x : Vector3) => s * x) "*" (kind :=.infix)

run_meta addOCLFunction q(Vector3.hDiv) "/" (kind :=.infix)
run_meta addOCLFunction q(fun (x : Vector3) (s : Float) => x / s) "/" (kind :=.infix)

run_meta addOCLFunction q(Vector3.div) "/" (kind :=.infix)
run_meta addOCLFunction q(fun (x y : Vector3) => x / y) "/" (kind :=.infix)


-- Vector operations

run_meta compileFunction q(Vector3.dot)
run_meta compileFunction q(Vector3.cross)
run_meta compileFunction q(Vector3.length)
run_meta compileFunction q(Vector3.length2)
-- run_meta compileFunction q(Vector3.normalize)
-- run_meta compileFunction q(Vector3.normalized)
run_meta compileFunction q(Vector3.distance)
run_meta compileFunction q(Vector3.distance2)
run_meta compileFunction q(Vector3.lerp)
-- run_meta compileFunction q(Vector3.reflect)
-- run_meta compileFunction q(Vector3.refract)
run_meta compileFunction q(Vector3.compMul)
run_meta compileFunction q(Vector3.compDiv)


-- Comparison operations--

-- run_meta compileFunction q(Vector3.beq)
-- run_meta compileFunction q(Vector3.blt)
-- run_meta compileFunction q(Vector3.ble)


-- Component-wise operations

run_meta compileFunction q(Vector3.abs)
run_meta compileFunction q(Vector3.min)
run_meta compileFunction q(Vector3.max)
-- run_meta compileFunction q(Vector3.sign)
-- run_meta compileFunction q(Vector3.clamp)
run_meta compileFunction q(Vector3.floor)
run_meta compileFunction q(Vector3.ceil)
-- run_meta compileFunction q(Vector3.round)
-- run_meta compileFunction q(Vector3.trunc)
-- run_meta compileFunction q(Vector3.fract)
-- run_meta compileFunction q(Vector3.mod)



-- Coordinate system conversions--

run_meta compileFunction q(Vector3.toSpherical)
run_meta compileFunction q(Vector3.fromSpherical)
run_meta compileFunction q(Vector3.toGeodetic)
run_meta compileFunction q(Vector3.fromGeodetic)


-- Trigonometric Functions (elementwise)

run_meta compileFunction q(Vector3.sin)
run_meta compileFunction q(Vector3.cos)
run_meta compileFunction q(Vector3.tan)
run_meta compileFunction q(Vector3.asin)
run_meta compileFunction q(Vector3.acos)
run_meta compileFunction q(Vector3.atan)
run_meta compileFunction q(Vector3.atan2)
run_meta compileFunction q(Vector3.sinh)
run_meta compileFunction q(Vector3.cosh)
run_meta compileFunction q(Vector3.tanh)


-- Exponential and Logarithmic Functions (elementwise)

run_meta compileFunction q(Vector3.exp)
run_meta compileFunction q(Vector3.exp2)
run_meta compileFunction q(Vector3.log)
run_meta compileFunction q(Vector3.log2)
run_meta compileFunction q(Vector3.log10)
run_meta compileFunction q(Vector3.pow)
run_meta compileFunction q(Vector3.sqrt)
-- run_meta compileFunction q(Vector3.invsqrt)


-- Interpolation and Smoothing (elementwise)

-- run_meta compileFunction q(Vector3.smoothstep)
-- run_meta compileFunction q(Vector3.step)
-- run_meta compileFunction q(Vector3.hermite)
-- run_meta compileFunction q(Vector3.catmullRom)
-- run_meta compileFunction q(Vector3.slerp)


-- Conversion and Construction

-- run_meta compileFunction q(Vector3.radians)
-- run_meta compileFunction q(Vector3.degrees)


-- Color and HSV Operations

-- run_meta compileFunction q(Vector3.rgbToHsv)
-- run_meta compileFunction q(Vector3.hsvToRgb)


-- Geometric Queries

-- run_meta compileFunction q(Vector3.insideBox)
-- run_meta compileFunction q(Vector3.projectToSegment)


-- Additional Vector3 Specific Functions

run_meta compileFunction q(Vector3.tripleProduct)
-- run_meta compileFunction q(Vector3.angleBetween)
-- run_meta compileFunction q(Vector3.project)
-- run_meta compileFunction q(Vector3.quantize)
