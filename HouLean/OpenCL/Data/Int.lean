import HouLean.OpenCL.Compiler.Main

namespace HouLean.OpenCL

open Compiler Qq


implemented_by : Nat.toUInt32    = oclFunction _ "(uint)"
implemented_by : UInt64.toUInt32 = oclFunction _ "(uint)"
implemented_by : UInt32.toNat    = oclFunction _ "" .infix

implemented_by : Nat.toUInt64    = oclFunction _ "(ulong)"
implemented_by : UInt32.toUInt64 = oclFunction _ "(ulong)"

implemented_by : UInt32.toInt32 = oclFunction _ "(int)"

implemented_by : Int32.toFloat32   = oclFunction _ "(float)"
implemented_by : Int32.toFloat    = oclFunction _ "(double)"
implemented_by : Nat.toFloat32    = oclFunction _ "(float)"
implemented_by : Nat.toFloat      = oclFunction _ "(double)"
-- implemented_by : Int.toFloat32    = oclFunction _ "(float)"
-- implemented_by : Int.toFloat      = oclFunction _ "(double)"
