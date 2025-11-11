import HouLean.Apex.Compile.Main

open Lean Meta HouLean Apex Compiler


set_option pp.notation false
open Qq Apex Compiler Lean Meta
run_meta
  let e := q(do
             let a ← pure (1,2,3)
             let b := a.1
             let c := a.2.1
             pure (f:=Id) (b + c))
  let e' ← whnfC e
  let e'' ← letBind e

  -- if e.isAppOfArity' ``Bind.bind 6 then
  --   logError "hihi"

  logInfo e
  logInfo e'
  logInfo e''

