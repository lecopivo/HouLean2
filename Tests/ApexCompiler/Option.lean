import HouLean

open HouLean Apex Compiler Qq

run_meta
  let e := q(fun x : Int => some x)
  let g ← programToApexGraph e
  IO.println g

run_meta
  let e := q(fun x : Option Int => x)
  let g ← programToApexGraph e
  IO.println g

-- set_option maxRecDepth 20
set_option trace.HouLean.Apex.compiler true in
run_meta
  let e := q(fun x : Option Int => x.getD default)
  let g ← programToApexGraph e
  IO.println g

open Qq

open Lean
