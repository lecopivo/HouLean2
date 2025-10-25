import Lean
import Qq

namespace Lean.Meta

partial def splitProdType (type : Expr) : List Expr :=
  if type.isAppOfArity ``Prod 2 then
    type.getArg! 0 :: splitProdType (type.getArg! 1)
  else
    [type]

def getExplicitArgs (fn : Expr) (xs : Array Expr) : MetaM (Array Expr) := do
  forallTelescope (← inferType fn) fun ys _ => do
    let mut xs' : Array Expr := #[]
    for y in ys, x in xs do
      if (← y.fvarId!.getBinderInfo) == .default then
        xs' := xs'.push x
    return xs'

