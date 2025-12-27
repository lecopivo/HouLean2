import Lean
import Qq
import HouLean.Meta.Basic
import HouLean.Meta.RunInterpreter
import HouLean.Meta.SpecializeAndSimp2.Types

namespace HouLean.Meta.Sas

open Lean Meta Qq

variable {α} [Inhabited α]

def simplify (e : Expr) : SasM Expr := do
    withTimeIt `simp do
      if e.isLet || e.isFVar || e.isForall then
        return e
      let e' := (← Simp.simp e).expr
      if e' != e then
        trace[HouLean.sas.simp] "simplified:{indentExpr e}\n==>{indentExpr e'}"
      return e'


def mkIdentity (type : Expr) : Expr :=
  Expr.lam `x type (.bvar 0) default

def maybeLetBindValue (name : Name) (val : Expr) (k : Bool → Expr → SasM α) : SasM α := do
  let type ← inferType val
  if val.isFVar || !val.hasFVar then
    k false val
  else
    withLetDecl name type val fun var =>
      k true var

-- turn some values into let fvars if if they are complicated enough
def maybeLetBindValues (name : Name) (vals : Array Expr) (k : Array Expr → Array Expr → SasM α) : SasM α := do
  go 0 vals.toList #[] #[]
where
  go (i : Nat) (xs : List Expr) (vars : Array Expr) (xs' : Array Expr) : SasM α := do
    match xs with
    | [] => k vars xs'
    | x :: xs =>
      maybeLetBindValue (name.appendAfter (toString i)) x fun didBind x' => do
        if didBind then
          go (i+1) xs (vars.push x') (xs'.push x')
        else
          go (i+1) xs vars (xs'.push x')

def withLetVars (vars : Array Expr) (x : SasM α) :=
  withReader (fun ctx => { ctx with letVars := ctx.letVars ++ vars }) x

def withNewScope (x : SasM α) :=
  withReader (fun ctx => { ctx with letVars := #[] }) x


structure DecodeTelescopeArgs where
  -- encoded arguments, all are new fvars
  ys : Array (Array Expr)

  -- original argumetns, expressed in terms of `ys`
  xs : Array Expr

  -- encoded return type
  ret : Expr

  argEncodes : Array (Array Expr)
  argDecodes : Array Expr

  retEncode : Array Expr
  retDecode : Expr


-- def forallDecodeTelescope (type : Expr) (k : DecodeTelescopeArgs → SasM α) : SasM α :=
--   sorry


def Option.decode (x : α) (valid : Bool) : Option α := if valid then some x else none
@[simp]
theorem decode_true {α} (x : α) : (Option.decode x true) = some x := by simp[Option.decode]
@[simp]
theorem decode_false {α} (x : α) : (Option.decode x false) = none := by simp[Option.decode]

@[simp]
theorem decode_getD {α} (x y : α) (valid : Bool) : (Option.decode x valid).getD y = if valid then x else y := by cases valid <;> simp[Option.decode]
@[simp]
theorem decode_isSome {α} (x : α) (valid : Bool) : (Option.decode x valid).isSome = valid := by cases valid <;> simp[Option.decode]
@[simp]
theorem decode_none {α} (x : α) (valid : Bool) : (Option.decode x valid).isNone = !valid := by cases valid <;> simp[Option.decode]


partial def typeEncoding (type : Expr) : SasM (Array Expr × Expr) := do
  withLocalDeclD `x type fun x => do

  if type.isAppOfArity ``Option 1 then
    let t := type.appArg!
    let (encodings, decode) ← typeEncoding t
    let encodings ← liftM <| encodings.mapM fun encode => do
      encode.beta #[← mkAppM ``Option.getD #[x, ← mkAppOptM ``default #[t, none]]]
      |>
      mkLambdaFVars #[x]
    let valid ← liftM <| mkAppM ``Option.isSome #[x] >>= mkLambdaFVars #[x]
    let decode ← liftM <|
      forallTelescope (← inferType decode) fun xs _ =>
        withLocalDeclD `valid q(Bool) fun valid => do
          mkAppM ``Option.decode #[decode.beta xs, valid]
          >>=
          mkLambdaFVars (xs.push valid)
    return (encodings.push valid, decode)

  return (#[mkIdentity type], mkIdentity type)


-- This will turn `Vector Float 3` to `VectorFloat3` or `Unit` to and empty array etc.
def withEncodedVal (val : Expr) (k : Array Expr → Expr → SasM α) : SasM α := do
  let type ← liftM <| inferType val >>= whnf
  let (encodings, decode) ← typeEncoding type
  -- todo: the simplify here might need different setting than the simp in `main`
  --       as we run simp *after* encoding not before!
  let vals ← encodings.mapM (fun enc => simplify (enc.beta #[val]))
  k vals decode

/-- Transform a curried function into one taking a single product argument. -/
def mkUncurryFun (f : Expr) : MetaM Expr := do
  forallTelescope (← inferType f) fun xs _ => do
    let x ← mkProdElem xs
    withLocalDeclD `x (← inferType x) fun x => do
      let xs ← mkProdSplitElem x xs.size
      mkLambdaFVars #[x] (f.beta xs)

def uncurriedTypeEncoding (type : Expr) : SasM (Expr × Expr) := do
  let (encodings, decode) ← typeEncoding type

  let decode ← mkUncurryFun decode

  unless encodings.size >= 0 do
    throwError m!"Unexpected type encoding for {type}!"

  forallTelescope (← inferType encodings[0]!) fun xs _ => do
    let e ← mkProdElem (encodings.map (fun enc => enc.beta xs))
    let encode ← mkLambdaFVars xs e
    return (encode, decode)


set_option pp.funBinderTypes true in
open Qq in
run_meta
  let (encodings, decode) ← (typeEncoding q(Option (Option Nat))).run #[]
  for encode in encodings do
    logInfo encode
  logInfo decode


def withVarsToSpecialize' (ys : Array Expr) (decode : Expr)
    (k : Array Expr → Array Expr → Expr → String → SasM α) : SasM α := do
  go ys.toList #[] #[] #[] default
where
  go (ys : List Expr) (vars vals ys' : Array Expr) (suffix : String) : SasM α := do
    match ys with
    | [] => k vars vals (← simplify <| decode.beta ys') suffix
    | y :: ys => do
      if !y.hasFVar then
        let type ← inferType y
        let s := s!"_{← ppExpr y}"
        let suffix :=
          if (← isClass? type).isSome then
            suffix
          else
            suffix ++ s
        go ys vars vals (ys'.push y) suffix
      else
        withLocalDeclD `a (← inferType y) fun var => do
          go ys (vars.push var) (vals.push y) (ys'.push var) suffix


def withVarsToSpecialize (yss : Array (Array Expr)) (decodes : Array Expr)
    (k : Array Expr → Array Expr → Array Expr → String → SasM α) : SasM α := do
 go yss.toList decodes.toList #[] #[] #[] default
where
  go (yss : List (Array Expr)) (decodes : List Expr) (vars vals xs : Array Expr) (suffix : String) : SasM α :=
    match yss, decodes with
    | [], [] => k vars vals xs suffix
    | ys :: yss, decode :: decodes =>
      withVarsToSpecialize' ys decode (fun vars' vals' x suffix' => do
        go yss decodes (vars ++ vars') (vals ++ vals') (xs.push x) (suffix.append suffix'))
    | _, _ => throwError m!"Invalid use of {decl_name%}, `yss` and `decodes` must have the same length!"



def requestSpecialization (funToSpecialize : Expr) (funName : Name) (specSuffix : String) : SasM Expr := do

  let specName := funName.append (.mkSimple <| "spec" ++ specSuffix.replace "." "_")
  let specType ← inferType funToSpecialize

  if !(← getEnv).contains specName then
    let decl : Declaration := .opaqueDecl {
      name := specName
      levelParams := []
      type := specType
      value := ← mkSorry specType (synthetic := true)
      isUnsafe := false
    }
    addDecl decl

  return (.const specName [])

def doSpecialize (fname : Name) : SasM Bool := do
  if let .ctorInfo a ← getConstInfo fname then
    return false
  return true


-- `cont` is run on the encoded return value with decode function i.e. `cont ys decode`
partial def main (e : Expr) (cont : Array Expr → Expr → SasM Expr) : SasM Expr := do
  let type ← inferType e
  let id' := mkIdentity type

  if type.isSort then
    return ← cont #[e] id'

  -- try running interpreter
  if let some val ← runInterpreterForPrimitiveTypes? e then
    let e := toExpr val
    return ← cont #[e] id'

  -- run simplifier
  let e ← simplify e

  match e with
  | .bvar .. => cont #[e] id'
  | .fvar .. => cont #[e] id'
  | .mvar .. => instantiateMVars e >>= (main · cont)
  | .sort .. => cont #[e] id'
  | .const .. => constCase e
  | .app .. =>
    let (fn, args) := e.withApp (·,·)
    -- todo: back stop for
    --       - for
    --       - ite
    --       - projection function
    --       - constructor
    --       - match
    appCase fn args
  | .lam .. =>
    lamCase e
  | .forallE .. => cont #[e] id' -- should we do something here???
  | .letE .. => letCase e
  | .lit .. => cont #[e] id'
  | .mdata _ e => main e cont
  | .proj .. => projCase e
where

  main'' (xs : List Expr) (xs' : Array (Array Expr)) (decodes : Array Expr) (k : Array (Array Expr) → Array Expr → SasM Expr) : SasM Expr := do
    match xs with
    | [] => k xs' decodes
    | x :: xs =>
      main x fun x' decode => main'' xs (xs'.push x') (decodes.push decode) k

  main' (xs : Array Expr) (k : Array (Array Expr) → Array Expr → SasM Expr) : SasM Expr := do
    main'' xs.toList #[] #[] k

  constCase (e : Expr) : SasM Expr := do
    let type ← inferType e
    let id' := mkIdentity type
    cont #[e] id'

  appCase (fn : Expr) (xs : Array Expr) : SasM Expr := do
    main' xs fun yss decodes => do

      if let some fname := fn.constName? then
        if ← doSpecialize fname then

          -- introduce new variables `vars` that correspond to values `vals` that were elements for `ys`
          -- and produce xs'
          return ← withVarsToSpecialize yss decodes fun vars vals xs' specSuffix => do
            let body := fn.beta xs'

            -- todo: instead of applying encode, apss encode separatelly and use it as `cont` when compiling the function
            --       this way we can propagate encoding to the leafs of the expression more easily
            let (encode, decode) ← uncurriedTypeEncoding (← inferType body)
            let funToSpecialize ← mkLambdaFVars vars (encode.beta #[body])
            let fn'' ← requestSpecialization funToSpecialize fname specSuffix

            -- todo: if `encoding.size > 1` then we whould introduce new let binding with the valie `fn''.beta vals`
            let (encodings, decode) ← typeEncoding (← inferType body)
            let e ← mkProdSplitElem (fn''.beta vals) encodings.size
            cont e decode

      let mut xs' : Array Expr := #[]
      for ys in yss, decode in decodes do
        xs' := xs'.push (decode.beta ys)
      withEncodedVal (fn.beta xs') cont
  -- bindCase
  --
  -- iteCase

  lamCase (e : Expr) : SasM Expr := do
    let e ←
      lambdaTelescope e fun xs e => do
        let body ←
          withNewScope do
            main e fun es decode => do
              mkLetFVars (←read).letVars (decode.beta es) (generalizeNondepLet := false)
        mkLambdaFVars xs body (generalizeNondepLet := false)
    let type ← inferType e
    let id' := mkIdentity type
    cont #[e] id'

  letCase (e : Expr) : SasM Expr := do
    let .letE n t v b nondep := e | panic! s!"bug in {decl_name%}"

    main v fun ys decode => do
      maybeLetBindValues n ys fun letVars ys' => do
        let ys := ys'
        withLetVars letVars do
        -- store letVars to the current scope
        let v' := decode.beta ys
        let b' ← main (b.instantiate1 v') cont
        return b'

  projCase (e : Expr) : SasM Expr := do
    let type ← inferType e
    let id' := mkIdentity type
    cont #[(← reduceProj? e).getD e] id'






def sas (e : Expr) (attrs : Array Name) : MetaM Expr := do
  (do
    withTimeIt `main do
    main e (fun es decode => do
      mkLetFVars (← read).letVars (decode.beta es) (generalizeNondepLet := false)))
  |>.run attrs
