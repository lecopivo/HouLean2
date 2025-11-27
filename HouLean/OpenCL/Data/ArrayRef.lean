import HouLean.OpenCL.Compiler.Main
import HouLean.OpenCL.Basic
import HouLean.OpenCL.Data.Int

namespace HouLean.OpenCL

open Compiler Qq


run_meta addOCLType q(ArrayRef Float) (.atom "float *" "f[]")
