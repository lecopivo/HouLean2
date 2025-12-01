import HouLean.OpenCL.Compiler.Main

namespace HouLean.OpenCL

open Compiler Qq

-- bootstrap types
instance : OpenCLType Bool where
  name := "bool"
  shortName := "b"


instance : OpenCLFunction Bool.and where
  name := " && "
  kind := .infix

instance : OpenCLFunction Bool.or where
  name := " || "
  kind := .infix

run_meta compileFunctionRef.set compileFunctionCore

set_option trace.HouLean.OpenCL.compiler true
#opencl_compile fun a b : Bool => a && b || a
