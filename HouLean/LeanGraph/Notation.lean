import Lean

-- todo: this is Lean notation that will allows up to specify node position while writing
--       lean code

open Lean Elab Term
elab:min x:term "at" "(" ("-")? (num <|> scientific) ", " ("-")? (num <|> scientific) ")" : term => do
  let x ← elabTerm x none

  -- data storing coordinates
  let data := (default : MData)
    |>.insert `xcoord (.ofNat 10)
    |>.insert `xcoord (.ofNat 43)

  -- attach the data to the head function
  let x := x.withApp fun fn args =>
    (fn.mdata data).beta args

  return x

/--
info: let a := 1 + 2;
let b := 10 + a;
a + b : Nat
-/
#guard_msgs in
#check
  let a := 1 + 2     at (10.123, 23.143)
  let b := 10 + a    at (-15, -5.132)
  a + b


--- Potentially notations for structural typing/assignment
-- (shuffle% ...)
-- (braid% ...)
-- (reorg% ...)
-- (iso% ...)
-- (struct_cast% ...)
-- (cast% ...)
-- (scast% ...)
