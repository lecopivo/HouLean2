import Lean
import HouLean.Init

open Lean Meta

namespace HouLean.Meta

def withStructureFieldTypes (structName : Name) (k : Array Expr → Array StructureFieldInfo → Array Expr → MetaM α) :
    MetaM α := do
  let env ← getEnv
  if isStructure (← getEnv) structName then
    let info := getStructureInfo env structName

    -- get field infos in the order as they appear in the struct
    let fieldInfos := info.fieldNames.filterMap (fun n => getFieldInfo? env structName n)

    let c ← mkConstWithFreshMVarLevels structName
    return ← forallTelescope (← inferType c) fun params _ => do
      withLocalDeclD `s (mkAppN c params) fun s => do
      let fieldTypes ← fieldInfos.mapM (fun finfo => (mkAppM finfo.projFn #[s]) >>= inferType)

      k params fieldInfos fieldTypes
  else
    throwError s!"Expected structrue!"

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
    return (.const ``Unit.unit [])
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


/-- Creates constructor and projection functions for a product type.

Given a type that is a (possibly nested) product of types, this function generates:
- A constructor function that takes all component values and builds the product
- An array of projection functions that extract each component from the product

## Behavior
- **Unit types** (`PUnit`): Returns unit constructor and empty projections array
- **Non-product types**: Returns identity functions for both constructor and projection
- **Product types**: Recursively flattens nested products and generates:
  - Constructor: `fun x₁ x₂ ... xₙ => ((...((x₁, x₂), x₃)...), xₙ)`
  - Projections: Array of functions extracting each leaf component

## Example
For type `Unit × (Int × Unit × Float) × String`:
- Constructor: `fun x₁ x₂ x₃ => (PUnit.unit, (x₁, PUnit.unit, x₂), x₃)`
  - Takes 3 arguments (skipping Unit types in the parameter list)
  - Builds the nested product structure
- Projections: `[fun x => x.2.1.1, fun x => x.2.1.2.2, fun x => x.2.2]`
  - Three functions extracting `Int`, `Float`, and `String` components

-/
partial def mkProdMkProjs (type : Expr) : MetaM (Expr × Array Expr) := do
  let type ← whnf type

  -- Handle Unit type: return unit constructor with no projections
  if type.isAppOfArity ``PUnit 0 then
    return ((.const ``PUnit.unit type.constLevels!), #[])

  -- Handle non-product types: return identity functions
  unless type.isAppOfArity ``Prod 2 do
    return (.lam `x type (.bvar 0) default, #[.lam `x type (.bvar 0) default])

  -- Extract product components
  let mkApp2 _ X Y := type
    | throwError "Bug in {decl_name%}, invalid arity check!"

  -- Recursively process left and right components
  let (mkX, projsX) ← mkProdMkProjs X
  let (mkY, projsY) ← mkProdMkProjs Y

  -- Combine constructors: merge parameter lists and build Prod.mk
  let mk ←
    lambdaTelescope mkX fun xs rx =>
    lambdaTelescope mkY fun ys ry => do
      mkLambdaFVars (xs ++ ys) (← mkAppM ``Prod.mk #[rx, ry])

  -- Adjust left projections to account for product nesting
  -- Replace body's bvar 0 with .proj ``Prod 0 (.bvar 0)
  let projsX := projsX.map fun proj =>
    match proj with
    | .lam n _ b bi =>
      Expr.lam n type (b.instantiate1 (.proj ``Prod 0 (.bvar 0))) bi
    | _ => default

  -- Adjust right projections to account for product nesting
  -- Replace body's bvar 0 with .proj ``Prod 1 (.bvar 0)
  let projsY := projsY.map fun proj =>
    match proj with
    | .lam n _ b bi =>
      Expr.lam n type (b.instantiate1 (.proj ``Prod 1 (.bvar 0))) bi
    | _ => default

  return (mk, projsX ++ projsY)


/-- Make local declarations is we have an array of names and types. -/
def mkLocalDecls [MonadControlT MetaM n] [Monad n]
  (names : Array Name) (bi : BinderInfo) (types : Array Expr) : Array (Name × BinderInfo × (Array Expr → n Expr)) :=
  types.mapIdx (fun i type => (names[i]!, bi, fun _ : Array Expr => pure type))


/-- Simpler version of `withLocalDecls` that can't deal with dependent types but has simpler signature -/
def withLocalDecls' [Inhabited α] [MonadControlT MetaM n] [Monad n]
  (names : Array Name) (bi : BinderInfo) (types : Array Expr) (k : Array Expr → n α) : n α :=
  withLocalDecls (mkLocalDecls names bi types) k


/-- If `e` is a match expression then all discriminants are turned into a let bindings
if they are not free variable already.

For example the expression
```
let (x,y) := f x
x + y
```
gets elaborated to
```
match f x with
| (x,y) => x + y
```
and running `whnf` with `iota := true` and `zeta := false` turns it into
```
(f x).1 + (f x).2
```
which is undesirable. Therefore this function turns
```
match f x with
| (x,y) => x + y
```
into
```
let r := f x
match r with
| (x,y) => x + y
```
and now running whnf on the let body will produce
```
let r := f x
r.1 + r.2
```
which preserves the let binding of the original expression

 -/
partial def letBindMatchDiscrs (e : Expr) (doUnfold := false) : MetaM Expr := do
  let some info := isMatcherAppCore? (← getEnv) e
    | return e
  let (fn, args) := e.withApp (fun fn args => (fn,args))
  go fn args info 0 #[]
where
  go (fn : Expr) (args : Array Expr) (info : MatcherInfo) (i : Nat) (xs : Array Expr) : MetaM Expr := do
    if i = info.numDiscrs then
      let mut e := fn.beta args
      if doUnfold then
        -- todo: maybe check that the proof of unfolding is rfl as we use it int whnf
        e := (← unfold e fn.constName!).expr
      return ← mkLambdaFVars xs e
    else
      let xi := args[i + info.numParams + 1]!
      if xi.isFVar then
        -- do nothing if `xi` is already fvar
        go fn args info (i+1) xs
      else
        let name := Name.appendAfter `r (toString i)
        withLetDecl name (← inferType xi) xi fun xvar => do
          let args := args.set! (i + info.numParams + 1) xvar
          let xs := xs.push xvar
          go fn args info (i+1) xs
