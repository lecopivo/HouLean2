import HouLean.OpenCL.Compiler.Extension2
import HouLean.OpenCL.Basic
import Qq

open Lean Meta Simp

namespace HouLean.OpenCL.Compiler2

namespace SpecializeAndSimp


/-- Give expression `fn arg₁ .. argₙ` the `apecializeFunction` returns `fn' arg'₁ ... arg'ₘ`
which is equal to the orignal expression but all arguments known at compile time are consumed in `fn'`
 -/
structure FunSpecializationResult where
  funName : Name

  /-- original function -/
  fn : Expr
  /-- original arguments -/
  args : Array Expr

  fn' : Expr
  args' : Array Expr

  mangledName : Name

def exprToString (e : Expr) : MetaM String := do
  -- reduce before printing
  let e ← whnf e
  let s := toString (← ppExpr e)
  return s


-- todo: should I also add support for "macro types"?
--       arguments of type `Optional α` could specialize for `some _` and `.none` and
--       it would be required to completely eliminate these types from the runtime
def specializeFunApp (fn : Expr) (args : Array Expr) : MetaM FunSpecializationResult := do
  let funName := fn.constName!
  let (fn', args', ss) ← go fn args.toList #[] #[] #[]
  let ss := ss.foldl (·++"_"++·) ""
  let mangledName := funName.appendAfter ss
  let fn' := fn'.eta
  return { funName, fn, args, fn', args', mangledName }
where
  go (e : Expr) (args : List Expr)
     (vars : Array Expr) (args' : Array Expr) (ss : Array String) :
     MetaM (Expr × Array Expr × Array String) := do
    match args with
    | [] =>
      let f ← mkLambdaFVars vars e
      return (f, args', ss)
    | arg :: args =>
      if !(arg.hasFVar || arg.hasMVar) then
        let type ← inferType arg
        let mut ss := ss

        -- ignore instances and proofs
        if (← isClass? type).isNone &&
           !(← inferType type).isProp then
          ss := ss.push (← exprToString arg)

        go (e.beta #[arg]) args vars args' ss
      else
        forallBoundedTelescope (← inferType e) (some 1) fun xs _ => do
          let x := xs[0]!
          go (e.beta #[x]) args (vars.push x) (args'.push arg) ss
