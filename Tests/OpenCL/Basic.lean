import HouLean.OpenCL.Data.Vector3
import HouLean.OpenCL.WorkItemFunctions

open HouLean OpenCL


instance : Coe UInt32 UInt64 := ⟨fun i => i.toUInt64⟩

def t1 (a b c : ArrayRef Vector3) : OpenCLM Unit := do

  let i ← getGlobalId 0

  let ai ← ArrayType.get a i.toUInt64
  let bi ← ArrayType.get b i.toUInt64

  let u := ai.cross bi
  let v := ai - (u.dot bi)*u

  ArrayType.set c i v

open Compiler Qq

set_option trace.HouLean.OpenCL.compiler true in
run_meta compileFunction q(t1)
