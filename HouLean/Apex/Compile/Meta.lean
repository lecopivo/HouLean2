import Lean
import Qq
import HouLean.Init

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



def mkAppFoldrM (const : Name) (xs : Array Expr) : MetaM Expr := do
  if xs.size = 0 then
    return default
  if xs.size = 1 then
    return xs[0]!
  else
    xs.joinrM pure
      λ x p =>
        mkAppM const #[x,p]

def mkAppFoldlM (const : Name) (xs : Array Expr) : MetaM Expr := do
  if xs.size = 0 then
    return default
  if xs.size = 1 then
    return xs[0]!
  else
    xs.joinlM pure
      λ p x =>
        mkAppM const #[p,x]

/--
For `#[x₁, .., xₙ]` create `(x₁, .., xₙ)`.
-/
def mkProdElem (xs : Array Expr) (mk := ``Prod.mk) : MetaM Expr := mkAppFoldrM mk xs

def mkProdFst (x : Expr) : MetaM Expr := mkAppM ``Prod.fst #[x]
def mkProdSnd (x : Expr) : MetaM Expr := mkAppM ``Prod.snd #[x]

/--
For `(x₀, .., xₙ₋₁)` return `xᵢ` but as a product projection.

We need to know the total size of the product to be considered.

For example for `xyz : X × Y × Z`
  - `mkProdProj xyz 1 3` returns `xyz.snd.fst`.
  - `mkProdProj xyz 1 2` returns `xyz.snd`.
-/
def mkProdProj (x : Expr) (i : Nat) (n : Nat) (fst := ``Prod.fst) (snd := ``Prod.snd) : MetaM Expr := do
  -- let X ← inferType x
  -- if X.isAppOfArity ``Prod 2 then
  match i, n with
  | _, 0 => pure x
  | _, 1 => pure x
  | 0, _ => mkAppM fst #[x]
  | i'+1, n'+1 => mkProdProj (← withTransparency .all <| mkAppM snd #[x]) i' n'
  -- else
  --   if i = 0 then
  --     return x
  --   else
  --     throwError "Failed `mkProdProj`, can't take {i}-th element of {← ppExpr x}. It has type {← ppExpr X} which is not a product type!"


def mkProdSplitElem (xs : Expr) (n : Nat) (fst := ``Prod.fst) (snd := ``Prod.snd) : MetaM (Array Expr) :=
  (Array.replicate n 0)
    |>.mapIdx (λ i _ => i)
    |>.mapM (λ i => mkProdProj xs i n fst snd)

/-- Make local declarations is we have an array of names and types. -/
def mkLocalDecls [MonadControlT MetaM n] [Monad n]
  (names : Array Name) (bi : BinderInfo) (types : Array Expr) : Array (Name × BinderInfo × (Array Expr → n Expr)) :=
  types.mapIdx (fun i type => (names[i]!, bi, fun _ : Array Expr => pure type))


/-- Simpler version of `withLocalDecls` that can't deal with dependent types but has simpler signature -/
def withLocalDecls' [Inhabited α] [MonadControlT MetaM n] [Monad n]
  (names : Array Name) (bi : BinderInfo) (types : Array Expr) (k : Array Expr → n α) : n α :=
  withLocalDecls (mkLocalDecls names bi types) k


/-- Take an expression `e` and abstract all free variables, turning `e` into a closed term.

Returns a function that accepts all the removed free variables and a product of all the free variables.

For example calling it on 
`(fun a b c => a + b + c + x + y + z)`
will product
`((fun ctx a b c => a + b + c + ctx.1 + ctx.2.1 + ctx.2.2),
  (x, y, z))` -/
def abstractAllFVars (e : Expr) : MetaM (Expr × Expr) := do

  let (_,s) ← e.collectFVars.run {}

  let ids := s.fvarIds
  let ctxVals := ids.map Expr.fvar
  let ctx ← mkProdElem ctxVals
  let ctx := if ctxVals.size != 0 then ctx else (.const ``Unit.unit [])
  let ctxType ← inferType ctx

  let e' ← withLocalDeclD `ctx ctxType fun ctxVar => do
    let ctxVars ← mkProdSplitElem ctxVar (max 1 ctxVals.size)
    let e := e.replaceFVars ctxVals ctxVars
    let e ← mkLambdaFVars #[ctxVar] e
    return e

  return (e', ctx)

