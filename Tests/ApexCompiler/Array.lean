import HouLean
import HouLean.Apex.Lean.Array.Prod

open HouLean Apex Compiler

open Lean Qq

run_meta
  let e := q(fun (x : Array Float) => x)
  let g ← programToApexGraph e
  IO.println g

run_meta
  let e := q(fun (x : Array Float) (y : Float) => x.push y)
  let g ← programToApexGraph e
  IO.println g

run_meta
  let e := q(fun (x : Array Float) => x + x)
  let g ← programToApexGraph e
  IO.println g

run_meta
  let e := q(fun (x : Array Float) => x * x)
  let g ← programToApexGraph e
  IO.println g

run_meta
  let e := q(fun (x : Array Float) => x[0]?)
  let g ← programToApexGraph e
  IO.println g

run_meta
  let e := q(fun (x : Array Float) => x[0]!)
  let g ← programToApexGraph e
  IO.println g

run_meta
  let e := q(fun (x : Array Float) (i : Nat) (h : i < x.size) => x[i])
  let g ← programToApexGraph e
  IO.println g

run_meta
  let e := q(fun (x : Array (Float × Float)) => x + x)
  let g ← programToApexGraph e
  IO.println g

run_meta
  let e := q(fun (x : Array (Float × Float × Int)) (a : Float) (i : Int) => (x + x).push (a,a,i))
  let g ← programToApexGraph e
  IO.println g


