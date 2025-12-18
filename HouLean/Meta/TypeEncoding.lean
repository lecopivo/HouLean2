import Lean
import Qq
import HouLean.Meta.Basic
import HouLean.Meta.TypeEncodingInit
import HouLean.Meta.TimeIt

namespace HouLean.Meta

open Lean Meta Qq

/-! # Type Encoding and Function Specialization

This module provides machinery for encoding/decoding compound types (products, options)
into flat representations suitable for code generation targets like OpenCL and VEX.

## Overview

The encoding transforms types as follows:
- `A × B` becomes separate values for `A` and `B`
- `Option A` becomes a pair of `(A, Bool)` where the bool indicates validity

Functions are then specialized to work with these flat representations.
-/

section Decoding

/-- Flatten a value of compound type into an array of primitive values.

For example, a value of type `Option (Nat × Nat)` becomes `#[fst, snd, valid]`. -/
partial def withDecodedValue (val : Expr) (k : Array Expr → MetaM α) : MetaM α := do
  let type ← inferType val
  -- TODO: Make this extensible via type class or attribute
  if type.isAppOfArity ``Prod 2 then
    let a ← mkAppM ``Prod.fst #[val]
    let b ← mkAppM ``Prod.snd #[val]
    withDecodedValue a fun as =>
      withDecodedValue b fun bs =>
        k (as ++ bs)
  else if type.isAppOfArity ``Option 1 then
    let A := type.appArg!
    let valid ← mkAppM ``Option.isSome #[val]
    let deflt ← mkAppOptM ``default #[A, none]
    let val ← mkAppM ``Option.getD #[val, deflt]
    withDecodedValue val fun as =>
      k (as.push valid)
  else
    k #[val]

/-- Apply `withDecodedValue` to multiple values, concatenating results. -/
partial def withDecodedValues (vals : Array Expr) (k : Array Expr → MetaM α) : MetaM α :=
  go vals.toList #[]
where
  go : List Expr → Array Expr → MetaM α
    | [], ys => k ys
    | v :: vs, ys => withDecodedValue v fun ys' => go vs (ys ++ ys')

/-- Helper for constructing optional values from a validity flag. -/
abbrev mkOption (x : α) (valid : Bool) : Option α :=
  if valid then some x else none

/-- Introduce fresh free variables for the decoded representation of a type.

Returns both the reconstructed value and the array of introduced variables. -/
partial def withDecoded1 (n : Name) (type : Expr) (k : Expr → Array Expr → MetaM α) : MetaM α := do
  -- TODO: Make this extensible via type class or attribute
  if type.isAppOfArity ``Prod 2 then
    let A := type.getRevArg! 1
    let B := type.getRevArg! 0
    withDecoded1 (n.appendAfter "1") A fun a as =>
      withDecoded1 (n.appendAfter "2") B fun b bs => do
        let x ← mkAppM ``Prod.mk #[a, b]
        k x (as ++ bs)
  else if type.isAppOfArity ``Option 1 then
    let A := type.appArg!
    withDecoded1 n A fun a as =>
      withLocalDeclD (n.appendAfter "valid") q(Bool) fun valid => do
        let x ← mkAppM ``mkOption #[a, valid]
        k x (as.push valid)
  else
    withLocalDeclD n type fun x =>
      k x #[x]

/-- Process multiple named types, introducing decoded variables for each.

Returns arrays of both the reconstructed values and the flat variables. -/
def withDecoded (ts : Array (Name × Expr)) (k : Array Expr → Array Expr → MetaM α) : MetaM α :=
  go ts.toList #[] #[]
where
  go : List (Name × Expr) → Array Expr → Array Expr → MetaM α
    | [], xs, ys => k xs ys
    | (n, t) :: ts, xs, ys =>
      withDecoded1 n t fun x' ys' =>
        go ts (xs.push x') (ys ++ ys')

end Decoding

section Uncurrying

/-- Transform a curried function into one taking a single product argument. -/
def mkUncurryFun (f : Expr) : MetaM Expr := do
  forallTelescope (← inferType f) fun xs _ => do
    let x ← mkProdElem xs
    withLocalDeclD `x (← inferType x) fun x => do
      let xs ← mkProdSplitElem x xs.size
      mkLambdaFVars #[x] (f.beta xs)

end Uncurrying

section ForallDecodeTelescope

/-- Forall telescope that decodes input types and provides an encoder for the return type.

For a type like `∀ (x : Option Nat) (y : Nat × Nat), Option Nat`, this provides:
- `xs`: Original arguments reconstructed from decoded variables
- `ys`: Fresh free variables for the flat representation
- `retType`: The encoded return type
- `encode`: Function to encode a return value
- `decode`: Function to decode an encoded return value

Example for the type above:
- `xs = #[mkOption x xvalid, (y1, y2)]`
- `ys = #[x : Nat, xvalid : Bool, y1 : Nat, y2 : Nat]`
- `encode = fun r => (r.getD default, r.isSome)`
- `decode` reconstructs the `Option Nat` from the flat form
-/
partial def forallDecodeTelescopeImpl
    (type : Expr)
    (k : Array Expr → Array Expr → Expr → Expr → Expr → MetaM α) : MetaM α :=
  go type #[] #[]
where
  go (e : Expr) (xs : Array Expr) (ys : Array Expr) : MetaM α := do
    match e with
    | .forallE n t b _ =>
      let t := t.instantiateRev xs
      withDecoded1 n t fun x' ys' =>
        go (b.instantiate1 x') (xs.push x') (ys ++ ys')
    | t =>
      let (t', encode) ← withLocalDeclD `r t fun r =>
        withDecodedValue r fun rs => do
          let b ← mkProdElem rs
          pure (← inferType b, ← mkLambdaFVars #[r] b)
      let decode ← withDecoded1 `x t fun x ys => mkLambdaFVars ys x
      let decode ← mkUncurryFun decode
      k xs ys t' encode decode

/-- Lifted version of `forallDecodeTelescopeImpl` for use in transformed monads. -/
def forallDecodeTelescope [Monad n] [MonadControlT MetaM n]
    (type : Expr)
    (k : Array Expr → Array Expr → Expr → Expr → Expr → n α) : n α :=
  map3MetaM
    (fun {α} (k : Array Expr → Array Expr → Expr × Expr × Expr → MetaM α) =>
      forallDecodeTelescopeImpl type fun xs ys r f g => k xs ys (r, f, g))
    (fun xs ys (r, f, g) => k xs ys r f g)

end ForallDecodeTelescope

section ReturnValueTransform

/-- Apply a function to the return value of an expression, handling let-bindings and conditionals.

This traverses through `let` expressions and `if`/`dite` branches to apply `f` at all
return points. -/
def applyToReturnValue (e : Expr) (f : Expr) (r : Expr) : Expr :=
  match e with
  | .letE n t v b nondep =>
    .letE n t v (applyToReturnValue b f r) nondep
  | Lean.mkApp5 (.const ``ite lvls) _α P inst t e =>
    Lean.mkApp5 (.const ``ite lvls) r P inst
      (applyToReturnValue t f r)
      (applyToReturnValue e f r)
  | Lean.mkApp5 (.const ``dite lvls) _α P inst (.lam nt tt bt tbi) (.lam ne te be ebi) =>
    Lean.mkApp5 (.const ``ite lvls) r P inst
      (.lam nt tt (applyToReturnValue bt f r) tbi)
      (.lam ne te (applyToReturnValue be f r) ebi)
  | _ => f.beta #[e]

end ReturnValueTransform

section SimpHelpers

/-- Retrieve simp theorems for the given attribute. -/
def getTheorems (attr : Name) : MetaM SimpTheorems := do
  if attr == `simp then
    getSimpTheorems
  else
    let some ext ← Lean.Meta.getSimpExtension? attr
      | throwError "simp attribute '{attr}' not found"
    ext.getTheorems

/-- Retrieve simprocs for the given attribute. -/
def getSimprocs (attr : Name) : MetaM Simprocs := do
  if attr == `simp then
    Simp.getSimprocs
  else
    let some ext ← Simp.getSimprocExtension? attr
      | throwError "simproc attribute '{attr}' not found"
    ext.getSimprocs

end SimpHelpers

section LetBinding

/-- Determine whether a value should remain inline or be let-bound.

Small/simple expressions stay inline; complex ones get bound to temporaries. -/
private def shouldStayInline (v : Expr) : MetaM Bool := do
  if v.isFVar || v.isLit then return true
  if (← inferType v).isType then return true
  if v.isAppOf ``OfNat.ofNat then return true
  if v.isAppOf ``OfScientific.ofScientific then return true
  if (← isClass? (← inferType v)).isSome then return true
  if ← isConstructorApp v then return true
  -- if ¬v.hasFVar && ¬v.hasMVar then return true
  if v.approxDepth < 8 then return true
  return false

/-- Conditionally let-bind values that are too complex to inline.

Returns the introduced let-variables and the (possibly simplified) values. -/
def maybeLetBindImpl (vals : Array Expr) (k : Array Expr → Array Expr → MetaM α) : MetaM α :=
  go vals.toList #[] #[]
where
  go : List Expr → Array Expr → Array Expr → MetaM α
    | [], letVars, vals' => k letVars vals'
    | v :: vals, letVars, vals' =>
      letTelescope v fun letVars' v => do
        if ← shouldStayInline v then
          go vals (letVars ++ letVars') (vals'.push v)
        else
          withLetDecl `tmp (← inferType v) v fun v' =>
            go vals (letVars ++ letVars'.push v') (vals'.push v')

/-- Lifted version of `maybeLetBindImpl`. -/
def maybeLetBind [Monad n] [MonadControlT MetaM n]
    (vals : Array Expr)
    (k : Array Expr → Array Expr → n α) : n α :=
  map2MetaM (maybeLetBindImpl vals) k

end LetBinding

section Specialization

structure SpecializationRequest where
  oldName : Name
  newName : Name
  newValue : Expr
  equality : Expr

/-- Specialize a function application to use decoded argument types.

This creates a new definition `fname_spec` that takes flat arguments and
wraps the call site to encode/decode as needed. -/
def specializeApp (e : Expr) : TimeItT SimpM Expr := do
  let (fn, args) := e.withApp (·, ·)
  let some (fname, lvls) := fn.const?
    | throwError "specializeApp: expected constant application, got {fn}"

  let .defnInfo info ← getConstInfo fname
    | return e  -- Not a definition we can specialize

  forallDecodeTelescope info.type fun xs ys r encode decode => do
    -- No change in arguments means no specialization needed
    if xs == ys then
      return e

    let b := applyToReturnValue (info.value.beta xs) encode r
    let newValue ← mkLambdaFVars ys b >>= instantiateMVars
    let type ← inferType newValue
    let fname' := fname.appendAfter "_spec"

    unless (← getEnv).contains fname' do
      let decl : Declaration := .defnDecl {
        name := fname'
        levelParams := info.levelParams
        type := type
        value := newValue -- ← mkAppOptM ``default #[type, none]
        hints := .regular newValue.approxDepth
        safety := .safe
      }
      addAndCompile decl

    let equality ←
      forallTelescope info.type fun xs r =>
        withDecodedValues xs fun xs' => do
          let lhs ← mkAppM fname xs
          -- todo: maybe add some let bindings for some xs as they might appear multiple times on rhs
          let rhs' ← mkAppM fname' xs'
          let rhs ← withLetDecl `tmp r rhs' fun r => do
            mkLetFVars #[r] (decode.beta #[r]) (generalizeNondepLet := false)
          mkForallFVars xs (← mkEq lhs rhs)

    let req : SpecializationRequest := {
      oldName := fname
      newName := fname'
      newValue := newValue
      equality := equality
    }

    logInfo m!"new specialization equality: {equality}"

    withDecodedValues args fun args' => do
      let e' := (Expr.const fname' lvls).beta args'
      withLetDecl `tmp r e' fun y => do
        mkLetFVars #[y] (decode.beta #[y]) (generalizeNondepLet := false)

/-- Recursively specialize all function applications in an expression. -/
partial def specialize' (e : Expr) : TimeItT SimpM Expr := do
  withTraceNode `HouLean.encodespec (fun _ => return m!"{e}") do
    unless e.isApp do
      return e

    let (fn, args) := e.withApp (·, ·)
    let args' ← args.mapM specialize'
    maybeLetBind args' fun letVars args'' => do
      let e ← withTimeIt `spec <| specializeApp (fn.beta args'')
      let e ← withTimeIt `simp <| Simp.simp e
      mkLetFVars letVars e.expr (generalizeNondepLet := false)

open Simp in
/-- Main entry point: specialize an expression using type encoding. -/
def specialize (e : Expr) : MetaM Expr := do
  let simpCtx ← Simp.mkContext
    (config := { zeta := false, zetaDelta := false, iota := true })
    (simpTheorems := #[← getTheorems `encodespec])
  let simpState : Simp.State := {}
  let simpMthds := Simp.mkDefaultMethodsCore #[]
  let ((r, _), _) ← ((specialize' e).run {}).run simpCtx simpState simpMthds
  return r

end Specialization

/-! ## Test Functions -/
section TestFunctions

def foo (x : Option Float) (y : Float × Float) (z : (Option Float) × Float) : Option Float :=
  match x, y, z with
  | some x, (y1, y2), (some z1, z2) => some (x + y1 + z1 * z2)
  | _, _, _ => some (y.1 + y.2)

def bar (x : Float) (p : Float × Float) (q : Option Float) : Option Float :=
  let a := p.1 + p.2
  if a < 0 then
    let b := x * p.2
    some (x + p.1 * p.2 + b)
  else
    none

def foobar (x : Float) (p : Float × Float) : Option Float :=
  let a := p.1 + p.2
  if a < 0 then
    let b := x * p.2
    some (x + p.1 * p.2 + b)
  else
    none

end TestFunctions

/-! ## Simp Lemmas for Encoding -/
section SimpLemmas

attribute [encodespec] Option.getD_some Option.getD_none Option.isSome_none

@[encodespec]
theorem mkOption_getD {α} (x y : α) (valid : Bool) :
    (mkOption x valid).getD y = if valid then x else y := by
  cases valid <;> rfl

@[encodespec]
theorem mkOption_isSome {α} (x : α) (valid : Bool) :
    (mkOption x valid).isSome = valid := by
  cases valid <;> rfl

/-- Simproc that stops simplification at let-bindings. -/
simproc_decl let_stop_simproc (_) := fun e => do
  if e.isLet then return .done { expr := e }
  return .continue

attribute [encodespec] let_stop_simproc

end SimpLemmas

/-! ## Tests -/
section Tests

/--
info: new specialization equality: ∀ (x : Float) (p : Float × Float),
  foobar x p =
    let tmp := foobar_spec x p.fst p.snd;
    mkOption (Prod.fst tmp) (Prod.snd tmp)
---
info: new specialization equality: ∀ (x : Float) (p : Float × Float),
  foobar x p =
    let tmp := foobar_spec x p.fst p.snd;
    mkOption (Prod.fst tmp) (Prod.snd tmp)
---
info: new specialization equality: ∀ (x : Option Float) (y : Float × Float) (z : Option Float × Float),
  foo x y z =
    let tmp := foo_spec (x.getD default) x.isSome y.fst y.snd (z.fst.getD default) z.fst.isSome z.snd;
    mkOption (Prod.fst tmp) (Prod.snd tmp)
---
info: new specialization equality: ∀ (x : Float) (p : Float × Float) (q : Option Float),
  bar x p q =
    let tmp := bar_spec x p.fst p.snd (q.getD default) q.isSome;
    mkOption (Prod.fst tmp) (Prod.snd tmp)
---
info:
  bar 1 (1.0, 42.0) (foo (foobar 0 (0, 0)) (1.0, -1.0) (foobar (-3) (100, 10), 7.0))
  ==> ⏎
  have tmp := foobar_spec 0 0 0;
  have tmp_1 := foobar_spec (-3) 100 10;
  have tmp :=
    foo_spec (if tmp.snd = true then tmp.fst else default) tmp.snd 1.0 (-1.0)
      (if tmp_1.snd = true then tmp_1.fst else default) tmp_1.snd 7.0;
  have tmp := bar_spec 1 1.0 42.0 (if tmp.snd = true then tmp.fst else default) tmp.snd;
  mkOption tmp.fst tmp.snd
-/
#guard_msgs in
run_meta
  let e := q(bar 1 (1.0, 42.0) (foo (foobar 0 (0, 0)) (1.0, -1.0) ((foobar (-3) (100, 10)), 7.0)))
  let e' ← specialize e
  logInfo m!"{indentExpr e}\n  ==> {indentExpr e'}"

end Tests

end HouLean.Meta
