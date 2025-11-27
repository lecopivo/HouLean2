import HouLean.OpenCL.Compiler.Main
import HouLean.Data.Float
import HouLean.OpenCL.Data.Bool

namespace HouLean.OpenCL

open Compiler Qq

-- bootstrap types
run_meta addOCLType q(Nat) (.atom "uint" "i")
