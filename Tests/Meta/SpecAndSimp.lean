import HouLean.Meta.SpecializeAndSimp.Main
import Qq

open Lean Meta Qq HouLean Meta

def foo (x : Nat) :=
  let a := x*x
  let b := x+x
  a + b + x

run_meta
  let e :=
    q(fun x y : Nat =>
      let a :=
        let a1 := x + foo y
        let a2 := 0 + foo (a1 + x)
        a1 * a2
      let b :=
        let b1 := y + 3
        let b2 := 0 + foo a + 0
        b1 * a * b2
      a + b)

  lambdaTelescope e fun _ e => do
  let e' ← specializeAndSimp e {} { zeta := false } #[`simp]

  logInfo e'
