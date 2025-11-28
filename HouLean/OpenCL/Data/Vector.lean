import HouLean.Data.Vector3
import HouLean.OpenCL.Basic
import HouLean.OpenCL.Compiler.Extension
-- import HouLean.OpenCL.Data.Float
-- import HouLean.OpenCL.Data.Int
-- import HouLean.OpenCL.Data.Unit

namespace HouLean.OpenCL

open Qq HouLean Math

variable {α : Type} {n : Nat}

-- todo: move this and add proper error message when grind fails!!!
def _root_.Vector.x (a : Vector α n) (h : n > 0 := by grind) : α := a[0]
def _root_.Vector.y (a : Vector α n) (h : n > 1 := by grind) : α := a[1]
def _root_.Vector.z (a : Vector α n) (h : n > 2 := by grind) : α := a[2]
def _root_.Vector.w (a : Vector α n) (h : n > 3 := by grind) : α := a[3]


-- should we require these instances as a prerequisite in the following instances?
-- [AtomicOpenCLType α] [AllowedVectorSize n]

instance : OpenCLFunction (@Vector.x α n) where
  name := ".x"
  kind := .postfix

instance : OpenCLFunction (@Vector.y α n) where
  name := ".y"
  kind := .postfix

instance : OpenCLFunction (@Vector.z α n) where
  name := ".z"
  kind := .postfix

instance : OpenCLFunction (@Vector.w α n) where
  name := ".w"
  kind := .postfix

instance [AtomicOpenCLType α] [AllowedVectorSize n] : OpenCLFunction (Vector.mk (α:=α) (n:=n)) where
  name :=
    let t : OpenCLType (Vector α n) := by infer_instance
    s!"({t.name})"
  kind := .constructor

open Lean Meta Compiler
run_meta

  let some data ← Compiler.getOpenCLApp? q(#v[(1.0:Float32),2.0,3.0]) | logError "failed"
  logInfo data.name
  pure ()
##exit

run_meta addOCLType q(Vector Float32 2) (.atom "float2" "f2")
run_meta addOCLType q(Vector Float32 3) (.atom "float3" "f3")
run_meta addOCLType q(Vector Float32 4) (.atom "float4" "f4")
run_meta addOCLType q(Vector Float32 8) (.atom "float8" "f8")
run_meta addOCLType q(Vector Float32 16) (.atom "float16" "f16")

run_meta addOCLType q(Vector Float 2) (.atom "double2" "d2")
run_meta addOCLType q(Vector Float 3) (.atom "double3" "d3")
run_meta addOCLType q(Vector Float 4) (.atom "double4" "d4")
run_meta addOCLType q(Vector Float 8) (.atom "double8" "f8")
run_meta addOCLType q(Vector Float 16) (.atom "double16" "f16")

run_meta addOCLType q(Vector Int32 2) (.atom "int2" "i2")
run_meta addOCLType q(Vector Int32 3) (.atom "int3" "i3")
run_meta addOCLType q(Vector Int32 4) (.atom "int4" "i4")

run_meta addOCLType q(Vector Int64 2) (.atom "long2" "l2")
run_meta addOCLType q(Vector Int64 3) (.atom "long3" "l3")
run_meta addOCLType q(Vector Int64 4) (.atom "long4" "l4")

run_meta addOCLType q(Vector UInt32 2) (.atom "uint2" "ui2")
run_meta addOCLType q(Vector UInt32 3) (.atom "uint3" "ui3")
run_meta addOCLType q(Vector UInt32 4) (.atom "uint4" "ui4")

run_meta addOCLType q(Vector UInt64 2) (.atom "ulong2" "ul2")
run_meta addOCLType q(Vector UInt64 3) (.atom "ulong3" "ul3")
run_meta addOCLType q(Vector UInt64 4) (.atom "ulong4" "ul4")

#exit
run_meta addOCLFunction q(Vector3.mk) "(float3)" (kind := .constructor)
run_meta addOCLFunction q(Vector3.x) ".x" (kind := .postfix)
run_meta addOCLFunction q(Vector3.y) ".y" (kind := .postfix)
run_meta addOCLFunction q(Vector3.z) ".z" (kind := .postfix)


opaque Vector3.vload3 (idx : UInt64) (array : ArrayRef Vector3) : OpenCLM Vector3
opaque Vector3.vstore3 (val : Vector3) (idx : UInt64) (array : ArrayRef Vector3) : OpenCLM Unit

instance : ArrayType Vector3 where
  get array idx := Vector3.vload3 idx array
  set array idx val := Vector3.vstore3 val idx array

run_meta addOCLType q(ArrayRef Vector3) (.atom "global float restrict *" "v3[]")

run_meta addOCLFunction q(Vector3.vload3) "vload3"
run_meta addOCLFunction q(Vector3.vstore3) "vstore3"

-- todo: remove these, they should be automatic
run_meta compileFunction q(fun (arr : ArrayRef Vector3) idx => ArrayType.get arr idx)
set_option trace.HouLean.OpenCL.compiler true in
run_meta compileFunction q(fun (arr : ArrayRef Vector3) idx val => ArrayType.set arr idx val)

run_meta addOCLFunction q(Vector3.add) " + " (kind :=.infix)
run_meta addOCLFunction q(fun x y : Vector3 => x + y) " + " (kind :=.infix)

run_meta addOCLFunction q(Vector3.sub) " - " (kind :=.infix)
run_meta addOCLFunction q(fun x y : Vector3 => x - y) " - " (kind :=.infix)

run_meta addOCLFunction q(Vector3.neg) " -" (kind :=.prefix)
run_meta addOCLFunction q(fun x : Vector3 => - x) " -" (kind :=.prefix)

run_meta addOCLFunction q(Vector3.smul) " * " (kind :=.infix)
run_meta addOCLFunction q(fun (x : Vector3) (s : Float) => x * s) " * " (kind :=.infix)
run_meta addOCLFunction q(fun (s : Float) (x : Vector3) => s * x) " * " (kind :=.infix)

run_meta addOCLFunction q(Vector3.hDiv) " / " (kind :=.infix)
run_meta addOCLFunction q(fun (x : Vector3) (s : Float) => x / s) " / " (kind :=.infix)

run_meta addOCLFunction q(Vector3.div) " / " (kind :=.infix)
run_meta addOCLFunction q(fun (x y : Vector3) => x / y) " / " (kind :=.infix)


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
