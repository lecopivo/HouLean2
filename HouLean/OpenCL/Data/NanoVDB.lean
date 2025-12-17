#exit
import HouLean.Math
import HouLean.Data.Defs
import HouLean.Data.Float
import HouLean.Data.Vector
import HouLean.OpenCL.Basic

namespace HouLean.OpenCL

def NanoVDB.Vec3 (T : Type) : Type := CArray T 3
abbrev NanoVDB.Coord := Vec3 Int32
abbrev NanoVDB.Vec3F := Vec3 Float32
abbrev NanoVDB.Vec3D := Vec3 Float64

class NanoVDB.ValueType (T : Type) where
  suffix : String
  -- allowed types
  valid : T = Float32 ∨
          T = Vec3 Float32 ∨
          T = Int32 := by simp

/-- Represents a pointer to NanoVDB grid with values of type `α` -/
opaque NanoVDB (T : Type) [NanoVDB.ValueType T] : Type

namespace NanoVDB

instance [t : ValueType α] : OpenCLType (NanoVDB α) where
  name := "void *"
  shortName := s!"vdb{t.suffix}"

instance [Inhabited T] : Inhabited (Vec3 T) := ⟨⟨default⟩⟩

instance : ValueType (Float32) where
  suffix := "F"

instance : ValueType (Vec3 Float32) where
  suffix := "F3"

instance : ValueType Int32 where
  suffix := "I"

instance : OpenCLType (Vec3 Float32) where
  name := "cnanovdb_Vec3F"
  shortName := "F3"

instance : OpenCLType Coord where
  name := "cnanovdb_coord"
  shortName := "I3"

noncomputable section

abbrev vec3FToVector : Vec3 Float32 → Vector Float32 3 := oclFunction _ "cnanovdb_Vec3F_to_float3"
abbrev vectorToVec3F : Vector Float32 3 → Vec3 Float32 := oclFunction _ "cnanovdb_float3_to_Vec3F"
abbrev coordToVector : Coord → Vector Int32 3 := oclFunction _ "cnanovdb_coord_to_int3"
abbrev vectorToCoord : Vector Int32 3 → Coord := oclFunction _ "cnanovdb_int3_to_coord"

instance : Coe (Vec3 Float32) (Vector Float32 3) := ⟨vec3FToVector⟩
instance : Coe (Vector Float32 3) (Vec3 Float32) := ⟨vectorToVec3F⟩
instance : Coe Coord (Vector Int32 3) := ⟨coordToVector⟩
instance : Coe (Vector Int32 3) Coord  := ⟨vectorToCoord⟩


-- cnanovdb_readaccessor
opaque ReadAccessor (T : Type) [ValueType T] : Type := Unit
instance [ValueType T] : Inhabited (ReadAccessor T) := ⟨cast sorry_proof ()⟩

-- I think this type should have pointer semantics the same as NanoVDB
--
instance [ValueType T]: OpenCLType (ReadAccessor T) where
  name := "cnanovdb_readaccessor"
  shortName := "cnanovdb_readaccessor"


variable {T} [vt : ValueType T]

-- todo: I should somehow specify the calling sematics is:
--          void(const global void * grid, cnanovdb_readaccessor * acc)
--       and not
--          cnanovdb_readaccessor(const global void * grid)
abbrev getReadAccessor : (grid : NanoVDB T) → OpenCLM (ReadAccessor T) :=
  oclFunction _ "cnanovdb_getreadaccessor"

set_option linter.unusedVariables false

abbrev ReadAccessor.isActive : (acc : ReadAccessor T) → (coord : Coord) → OpenCLM Bool :=
  oclFunction _ s!"cnanovdb_readaccessor_isActive{vt.suffix}"

abbrev ReadAccessor.set : (acc : ReadAccessor T) → (coord : Coord) → (value : T) → OpenCLM Bool :=
  oclFunction _ s!"cnanovdb_readaccessor_setValue{vt.suffix}"
