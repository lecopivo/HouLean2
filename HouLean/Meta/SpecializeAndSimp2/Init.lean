import Lean

open Lean

initialize registerTraceClass `HouLean.sas
initialize registerTraceClass `HouLean.sas.simp

register_simp_attr encodespec
