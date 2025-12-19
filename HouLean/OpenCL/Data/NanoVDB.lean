import HouLean.Math
import HouLean.Data.Defs
import HouLean.Data.Float
import HouLean.Data.Vector
import HouLean.OpenCL.Basic
import HouLean.OpenCL.Data.Vector
import HouLean.OpenCL.Data.Pointer
import HouLean.Meta.DoNotation

namespace HouLean.OpenCL

def NanoVDB.Vec3 (T : Type) : Type := CArray T 3
abbrev NanoVDB.Coord := Vec3 Int32
abbrev NanoVDB.Vec3F := Vec3 Float32
abbrev NanoVDB.Vec3D := Vec3 Float64

-- class NanoVDB.ValueType (T : Type) where
--   suffix : String
--   -- allowed types
--   valid : T = Float32 ∨
--           T = Vec3 Float32 ∨
--           T = Int32 := by simp

-- /-- Represents a pointer to NanoVDB grid with values of type `α` -/
abbrev NanoVDB (T : Type) (const : Bool := false) := DPointer Unit (addr:=.global) (const:=const) (restrict:=true)

namespace NanoVDB

-- instance [t : ValueType α] : OpenCLType (NanoVDB α) where
--   name := "void *"
--   shortName := s!"vdb{t.suffix}"

-- instance [Inhabited T] : Inhabited (Vec3 T) := ⟨⟨default⟩⟩

-- instance : ValueType (Float32) where
--   suffix := "F"

-- instance : ValueType (Vec3 Float32) where
--   suffix := "F3"

-- instance : ValueType Int32 where
--   suffix := "I"

-- instance : OpenCLType (Vec3 Float32) where
--   name := "cnanovdb_Vec3F"
--   shortName := "F3"

-- instance : OpenCLType Coord where
--   name := "cnanovdb_coord"
--   shortName := "I3"

-- noncomputable section

-- abbrev vec3FToVector : Vec3 Float32 → Vector Float32 3 := oclFunction _ "cnanovdb_Vec3F_to_float3"
-- abbrev vectorToVec3F : Vector Float32 3 → Vec3 Float32 := oclFunction _ "cnanovdb_float3_to_Vec3F"
-- abbrev coordToVector : Coord → Vector Int32 3 := oclFunction _ "cnanovdb_coord_to_int3"
-- abbrev vectorToCoord : Vector Int32 3 → Coord := oclFunction _ "cnanovdb_int3_to_coord"

-- instance : Coe (Vec3 Float32) (Vector Float32 3) := ⟨vec3FToVector⟩
-- instance : Coe (Vector Float32 3) (Vec3 Float32) := ⟨vectorToVec3F⟩
-- instance : Coe Coord (Vector Int32 3) := ⟨coordToVector⟩
-- instance : Coe (Vector Int32 3) Coord  := ⟨vectorToCoord⟩


-- cnanovdb_readaccessor
opaque ReadAccessor : Type := Unit
impl_type_by ReadAccessor ==> cnanovdb_readaccessor

abbrev ReadAccessorPtr (T : Type) := DPointer ReadAccessor (addr:=.default) (const:=false)


-- @[opencl_extern "cnanovdb_getreadaccessor"]
opaque getReadAccessor (grid : NanoVDB T) (acc : ReadAccessorPtr T) : OpenCLM Unit
impl_by {T} (grid : NanoVDB T) (acc : ReadAccessorPtr T) :
  getReadAccessor grid acc ==> cnanovdb_getreadaccessor(grid, acc)

noncomputable
opaque getReadAccessor' (grid : NanoVDB T) : OpenCLM (ReadAccessorPtr T)
impl_by {T} (grid : NanoVDB T) :
  getReadAccessor' grid ==> cnanovdb_getreadaccessor(grid)


open Lean Meta Compiler Qq
def vdbTypeSuffix (t : Expr) : MetaM String := do
  if ← isDefEq t q(Float32) then
    return "F"
  else if ← isDefEq t q(Vector Float32 3) then
    return "V3"
  else
    throwError "not a valid value type!"


opaque ReadAccessorPtr.isActive (acc : ReadAccessorPtr T) (coord : Vector Int32 3) : OpenCLM Bool
impl_by {T} (acc : ReadAccessorPtr T) (coord : Vector Int32 3) :
    acc.isActive coord ==> do
  let acc ← compileExpr acc
  let coord ← compileExpr coord
  let suffix ← vdbTypeSuffix T
  let funId := mkIdent <| Name.appendAfter `cnanovdb_readaccessor_isActive suffix
  return ← `(clExpr| $funId:ident($acc, $coord))


opaque ReadAccessorPtr.get {T} [Inhabited T] (acc : ReadAccessorPtr T) (coord : Vector Int32 3) : OpenCLM T
impl_by {T} [Inhabited T] (acc : ReadAccessorPtr T) (coord : Vector Int32 3) :
    acc.get coord ==> do
  let acc ← compileExpr acc
  let coord ← compileExpr coord
  let suffix ← vdbTypeSuffix T
  let funId := mkIdent <| Name.appendAfter `cnanovdb_readaccessor_getValue suffix
  return ← `(clExpr| $funId:ident($acc, $coord))


opaque ReadAccessorPtr.set (acc : ReadAccessorPtr T) (coord : Vector Int32 3) (value : T) : OpenCLM Unit
impl_by {T} (acc : ReadAccessorPtr T) (coord : Vector Int32 3) (value : T) :
    acc.set coord value ==> do
  let acc ← compileExpr acc
  let coord ← compileExpr coord
  let val ← compileExpr value
  let suffix ← vdbTypeSuffix T
  let funId := mkIdent <| Name.appendAfter `cnanovdb_readaccessor_setValue suffix
  return ← `(clExpr| $funId:ident($acc, $coord, $val))



-- attribute [opencl_csimp] Id.run
-- impl_by {α : Type} (a : α) : ForInStep.yield a ==> a


-- #opencl_compile (fun (acc : ReadAccessorPtr (Vector Float32 3)) => do
--   let v ← acc.get #v[0,0,0]
--   let u ← acc.get #v[1,0,0]
--   acc.set #v[2,0,0] (u+v)

--   let mut s := 0
--   for i in [0:10] do
--     for j in [0:10] do
--       for k in [0:10] do
--         let v ← acc.get #v[i.toInt32,j.toInt32,k.toInt32]
--         s += v

--   return s)
