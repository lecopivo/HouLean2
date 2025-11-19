import HouLean.Apex.Compile.Meta


open Lean Meta Qq


/- Example demonstrating `asContextChange'` with two expressions that mutate different
variables from a shared context.

Given:
- Free variables: `a, b : Int` and `x, y : Float`
- Expression 1: `(a * b, x + y + Float.ofInt a)` - computes new values for `a` and `x`
- Expression 2: `(a + b, x * y)` - computes new values for `a` and `x`

The function identifies that `a` and `x` are mutated (they appear in the result type),
while `b` and `y` remain unchanged.
-/


/--
info: ctx: (a, b, x, y)
---
info: ctx': (b, y)
---
info: split: fun ctx => ((ctx.1, PUnit.unit, ctx.2.2.1), ctx.snd.fst, ctx.snd.snd.snd)
---
info: es: [fun ctx =>
   let r := (ctx.fst * ctx.snd.fst, (), ctx.snd.snd.fst + ctx.snd.snd.snd + Float.ofInt ctx.fst);
   (r.1, ctx.2.1, r.2.2, ctx.2.2.2),
 fun ctx =>
   let r := (ctx.fst + ctx.snd.fst, (), ctx.snd.snd.fst * ctx.snd.snd.snd);
   (r.1, ctx.2.1, r.2.2, ctx.2.2.2)]
-/
#guard_msgs in
run_meta
  withLocalDeclDQ `a q(Int) fun a => do
  withLocalDeclDQ `b q(Int) fun b => do
  withLocalDeclDQ `x q(Float) fun x => do
  withLocalDeclDQ `y q(Float) fun y => do

  let e1 := q(($a * $b, (), $x + $y + Float.ofInt $a))
  let e2 := q(($a + $b, (), $x * $y))

  let some (ctx, ctx', split, es) ← asContextChange' #[e1, e2]
    | throwError "failed!"

  logInfo m!"ctx: {ctx}"
  logInfo m!"ctx': {ctx'}"
  logInfo m!"split: {split}"
  logInfo m!"es: {es}"
