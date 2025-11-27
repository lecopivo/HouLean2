import HouLean.OpenCL.Compiler.Main

namespace HouLean.OpenCL

open Compiler Qq


run_meta addOCLType q(Int) (.atom "int" "i")

run_meta addOCLType q(Int16) (.atom "short" "s")
run_meta addOCLType q(Int32) (.atom "int" "i")
run_meta addOCLType q(Int64) (.atom "long" "l")


run_meta addOCLType q(Nat) (.atom "uint" "ui")

run_meta addOCLType q(UInt16) (.atom "uint" "us")
run_meta addOCLType q(UInt32) (.atom "uint" "ui")
run_meta addOCLType q(UInt64) (.atom "ulong" "ul")


-- just erace OfNat.ofNat, this ia a bit of a hack and housl be done differently
run_meta addOCLFunction q(fun n => OfNat.ofNat (α:=Nat) n) "" (kind := .prefix)
run_meta addOCLFunction q(fun n => OfNat.ofNat (α:=UInt16) n) "" (kind := .prefix)
run_meta addOCLFunction q(fun n => OfNat.ofNat (α:=UInt32) n) "" (kind := .prefix)
run_meta addOCLFunction q(fun n => OfNat.ofNat (α:=UInt64) n) "" (kind := .prefix)

run_meta addOCLFunction q(fun n => OfNat.ofNat (α:=Int) n) "" (kind := .prefix)
run_meta addOCLFunction q(fun n => OfNat.ofNat (α:=Int16) n) "" (kind := .prefix)
run_meta addOCLFunction q(fun n => OfNat.ofNat (α:=Int32) n) "" (kind := .prefix)
run_meta addOCLFunction q(fun n => OfNat.ofNat (α:=Int64) n) "" (kind := .prefix)


-- casts
run_meta addOCLFunction q(Nat.toUInt64) "(ulong)" (kind := .prefix)
run_meta addOCLFunction q(UInt32.toUInt64) "(ulong)" (kind := .prefix)

run_meta addOCLFunction q(Int.toInt64) "(long)" (kind := .prefix)
run_meta addOCLFunction q(Int32.toInt64) "(long)" (kind := .prefix)
