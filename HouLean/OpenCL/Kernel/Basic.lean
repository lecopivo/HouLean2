import HouLean.OpenCL.Compiler.Main
import HouLean.Data.Geometry
import HouLean.Data.FloatP
import HouLean.OpenCL.Data.ArrayPointer
import HouLean.OpenCL.Data.NanoVDB
import HouLean.Apex.Generated.Defs

namespace HouLean.OpenCL

structure Attribute (cls : AttributeClass) (name : String) (α : Type) (input : Nat := 0) {A} [ArrayType α A] where
  size : Int
  ptr : ArrayPointer α

structure Volume (name : String) (α : Type) (dim := 3) (input : Nat := 0) {A} [ArrayType α A] where
  ptr : ArrayPointer α

structure VDB (name : String) (α : Type) [NanoVDB.ValueType α](input : Nat := 0) where
  ptr : NanoVDB.ReadAccessor α

set_option linter.unusedVariables false

def Param (name : String) (T : Type) [Inhabited T] (val := (default : T)) := T

def kernel
  (P : Attribute .point "P" (Vector Float32 3))
  (sdf : VDB "surface" (NanoVDB.Vec3F) (input:=1))
  (maxiter : Param "maxiter" Int)
  : OpenCLM Unit := do
  pure ()



namespace BindingSOP


inductive Precision where
  | half | single | double

inductive TimeScale where
  | none | timestep | invtimestep | exptimestep

inductive AttributeType where
  | float | int | floatarray | intarray

inductive VolumeOptions where
  | aligned
  | nonaligned (resolution : Bool) (voxelsize : Bool) (voxeltoworld : Bool) (worldtovoxel : Bool)

inductive VDBOptions where
  | nonaligned (resolution : Bool) (voxelsize : Bool) (voxeltoworld : Bool) (worldtovoxel : Bool)

inductive BindingOptions where
  | int (prec : Precision) (value : Int)
  | float (prec : Float.Precision) (value : FloatP prec) (timescale : TimeScale := .none)
  | vector3 (prec : Float.Precision) (value : Vector (FloatP prec) 3)
  | vector4 (prec : Float.Precision) (value : Vector (FloatP prec) 4)
  | ramp
  | attrib (input : Nat) (name : String) (cls : AttributeClass) (type : AttributeType) (size : Nat := 1)
           (prec : Precision := .single) (read : Bool := true) (write : Bool := true) (optional : Bool := false)
  | volume (input : Nat) (name : String) (options : VolumeOptions)
           (prec : Precision := .single) (read : Bool := true) (write : Bool := true) (optional : Bool := false)
  | vdb    (input : Nat) (name : String) (voxelsize voxeltoworld worldtovoxel : Bool)
           (prec : Precision := .single) (write : Bool := true) (optional : Bool := false)

end BindingSOP

open BindingSOP in
structure BindingSOP where
  parameterName : String
  options : BindingOptions

-- def bindingsToDict (bindings : Array BindingSOP) : Apex.Dict := sorry

-- /-- Gadget struct that is used turn `kernel` into `code` and `bindings` -/
-- structure CompiledKernel {Kernel} (kernel : Kernel) (code : String) (bindings : Apex.Dict) : Prop where
--   success : True

-- -- This will be function compilable from APEX
-- def runKernel {Kernel} (kernel : Kernel)
--   {code : String} {bindings : Apex.Dict} (h : CompiledKernel kernel code bindings)
--   (geo : Apex.Geometry) : Apex.Geometry := sorry
