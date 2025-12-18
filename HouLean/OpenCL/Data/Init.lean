import HouLean.OpenCL.Basic
import HouLean.OpenCL.Compiler
import HouLean.Math

namespace HouLean.OpenCL

-- attribute [opencl_csimp]
--   Math.transformNormal
--   Math.transformVector
--   Math.transformPoint
--   Math.projectToSegment
--   Math.insideBox
--   Math.degrees
--   Math.radians
--   Math.slerp
--   Math.catmullRom
--   Math.hermite
--   Math.step
--   Math.smoothstep
--   Math.lerp
--   Math.ble
--   Math.blt
--   Math.beq
--   Math.compDiv
--   Math.compMul
--   Math.refract
--   Math.reflect
--   Math.normalized
--   Math.normalize
--   Math.distance2
--   Math.distance
--   Math.length2
--   Math.length
--   Math.cross
--   Math.dot
--   Math.fract
--   Math.trunc
--   Math.round
--   Math.ceil
--   Math.floor
--   Math.clamp
--   Math.sign

--   Math.invsqrt
--   Math.sqrt
--   Math.log10
--   Math.log2
--   Math.log
--   Math.exp2
--   Math.exp
--   Math.tanh
--   Math.cosh
--   Math.sinh
--   Math.atan2
--   Math.atan
--   Math.acos
--   Math.asin
--   Math.tan

attribute [opencl_csimp]
  Bool.decide_eq_true
  Bool.decide_eq_false
  Bool.decide_true_eq
  Bool.decide_false_eq


-- todo: probably make a custom version of `pure_bind` `pure x >>= f x  ==>  let x' := x:= f x'`
attribute [opencl_csimp]
  bind_assoc bind_pure pure_bind

-- todo: maybe make a special unsafe simp attribute for these
@[opencl_csimp] theorem add_zero [Add α] [Zero α] (a : α) : 0 + a = a := sorry_proof
@[opencl_csimp] theorem zero_add [Add α] [Zero α] (a : α) : a + 0 = a := sorry_proof

@[opencl_csimp] theorem one_mul [Mul α] [One α] (a : α) : 1 * a = a := sorry_proof
@[opencl_csimp] theorem mul_one [Mul α] [One α] (a : α) : a * 1 = a := sorry_proof


instance [t : OpenCLType α] : OpenCLType (Id α) := t
attribute [opencl_csimp] Id.run

attribute [opencl_csimp] sum



open Compiler
impl_by {α : Type} : Id α ==> do
  let t ← compileType α
  return t

impl_by {α : Type} : OpenCLM α ==> do
  let t ← compileType α
  return t
