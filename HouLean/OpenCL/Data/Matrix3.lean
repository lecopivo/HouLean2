import HouLean.OpenCL.Data.Vector3
import HouLean.Data.Matrix3

namespace HouLean.OpenCL

open Compiler Qq


run_meta addOCLType q(Matrix3) (.struct {
  name := "matrix3f",
  shortName := "f33",
  fields := #[
    { name := "row0", type := .atom "float3" "f3" },
    { name := "row1", type := .atom "float3" "f3" },
    { name := "row2", type := .atom "float3" "f3" },
  ]})

run_meta addOCLFunction q(Matrix3.mk) "(matrix3f)" (kind := .constructor)
run_meta addOCLFunction q(Matrix3.row0) ".row0" (kind := .postfix)
run_meta addOCLFunction q(Matrix3.row1) ".row1" (kind := .postfix)
run_meta addOCLFunction q(Matrix3.row2) ".row2" (kind := .postfix)

--- Matrix
run_meta compileFunction q(Matrix3.add)
run_meta compileFunction q(fun (A B : Matrix3) => A + B)

run_meta compileFunction q(Matrix3.sub)
run_meta compileFunction q(fun (A B : Matrix3) => A - B)

run_meta compileFunction q(Matrix3.neg)
run_meta compileFunction q(fun (A : Matrix3) => - A)

run_meta compileFunction q(Matrix3.smul)
run_meta compileFunction q(fun (s : Float) (m : Matrix3) => s * m)
run_meta compileFunction q(fun (m : Matrix3) (s : Float) => m * s)


run_meta compileFunction q(Matrix3.matmul)
run_meta compileFunction q(fun (A B : Matrix3) => A * B)

run_meta compileFunction q(Matrix3.mulVec)
run_meta compileFunction q(fun (A : Matrix3) (v : Vector3) => A * v)

run_meta compileFunction q(Matrix3.vecMul)
run_meta compileFunction q(fun (v : Vector3) (A : Matrix3) => v * A)


-- Matrix operations

run_meta compileFunction q(Matrix3.dot)
run_meta compileFunction q(Matrix3.length)
run_meta compileFunction q(Matrix3.length2)
-- run_meta compileFunction q(Matrix3.normalize)
-- run_meta compileFunction q(Matrix3.normalized)
run_meta compileFunction q(Matrix3.distance)
run_meta compileFunction q(Matrix3.distance2)
run_meta compileFunction q(Matrix3.lerp)
-- run_meta compileFunction q(Matrix3.reflect)
-- run_meta compileFunction q(Matrix3.refract)
run_meta compileFunction q(Matrix3.compMul)
run_meta compileFunction q(Matrix3.compDiv)


-- Comparison operations--

-- run_meta compileFunction q(Matrix3.beq)
-- run_meta compileFunction q(Matrix3.blt)
-- run_meta compileFunction q(Matrix3.ble)


-- Component-wise operations

run_meta compileFunction q(Matrix3.abs)
run_meta compileFunction q(Matrix3.min)
run_meta compileFunction q(Matrix3.max)
-- run_meta compileFunction q(Matrix3.sign)
-- run_meta compileFunction q(Matrix3.clamp)
run_meta compileFunction q(Matrix3.floor)
run_meta compileFunction q(Matrix3.ceil)
-- run_meta compileFunction q(Matrix3.round)
-- run_meta compileFunction q(Matrix3.trunc)
-- run_meta compileFunction q(Matrix3.fract)
-- run_meta compileFunction q(Matrix3.mod)


-- Trigonometric Functions (elementwise)

run_meta compileFunction q(Matrix3.sin)
run_meta compileFunction q(Matrix3.cos)
run_meta compileFunction q(Matrix3.tan)
run_meta compileFunction q(Matrix3.asin)
run_meta compileFunction q(Matrix3.acos)
run_meta compileFunction q(Matrix3.atan)
run_meta compileFunction q(Matrix3.atan2)
run_meta compileFunction q(Matrix3.sinh)
run_meta compileFunction q(Matrix3.cosh)
run_meta compileFunction q(Matrix3.tanh)


-- Exponential and Logarithmic Functions (elementwise)

run_meta compileFunction q(Matrix3.exp)
run_meta compileFunction q(Matrix3.exp2)
run_meta compileFunction q(Matrix3.log)
run_meta compileFunction q(Matrix3.log2)
run_meta compileFunction q(Matrix3.log10)
run_meta compileFunction q(Matrix3.pow)
run_meta compileFunction q(Matrix3.sqrt)
-- run_meta compileFunction q(Matrix3.invsqrt)


-- Interpolation and Smoothing (elementwise)

-- run_meta compileFunction q(Matrix3.smoothstep)
-- run_meta compileFunction q(Matrix3.step)
-- run_meta compileFunction q(Matrix3.hermite)
-- run_meta compileFunction q(Matrix3.catmullRom)
-- run_meta compileFunction q(Matrix3.slerp)


-- Conversion and Construction

-- run_meta compileFunction q(Matrix3.radians)
-- run_meta compileFunction q(Matrix3.degrees)


-- Geometric Queries

-- run_meta compileFunction q(Matrix3.insideBox)
-- run_meta compileFunction q(Matrix3.projectToSegment)


-- Additional Matrix3 Specific Functions
