import HouLean.OpenCL.Compiler.Main

namespace HouLean.OpenCL

open Compiler Qq

-- bootstrap types
run_meta addOCLType q(Bool) (.atom "bool" "b")


run_meta addOCLFunction q(Bool.and) "&&" (kind := .infix)
run_meta addOCLFunction q(fun a b : Bool => a && b) "&&" (kind := .infix)

run_meta addOCLFunction q(Bool.or) "||" (kind := .infix)
run_meta addOCLFunction q(fun a b : Bool => a || b) "||" (kind := .infix)
