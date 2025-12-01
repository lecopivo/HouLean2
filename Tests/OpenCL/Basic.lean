import HouLean.Data.Vector
import HouLean.OpenCL.Data.Vector
import HouLean.OpenCL.Compiler.Main
import HouLean.OpenCL.WorkItemFunctions

open HouLean OpenCL



instance : Coe UInt32 UInt64 := ⟨fun i => i.toUInt64⟩

def t1 (a b c : Pointer Float32) : OpenCLM Unit := do

  let i ← getGlobalId 0

  let ai : Vector Float32 3 ← ArrayType.get a.toConst i.toUInt64
  let bi : Vector Float32 3 ← ArrayType.get b.toConst i.toUInt64

  let u := ai.cross3 bi
  let v := ai - (u.dot bi)*u

  ArrayType.set c i v

open Compiler Qq

set_option trace.HouLean.OpenCL.compiler true in


#opencl_compile t1
