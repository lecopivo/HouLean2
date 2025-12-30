import Lean
import Qq
import HouLean.Meta.Basic
import HouLean.Meta.RunInterpreter
import HouLean.Meta.SpecializeAndSimp2.Types
import HouLean.Meta.SpecializeAndSimp2.Encoding
import HouLean.Meta.SpecializeAndSimp2.Encoding.Vector

namespace HouLean.Meta.Sas

open Lean Meta Qq

variable {α} [Inhabited α]

/-! ## Core Utilities -/

def simplify (e : Expr) : SasM Expr := do
  withTimeIt `simp do
    if e.isLet || e.isFVar || e.isForall then
      return e
    let e' := (← Simp.simp e).expr
    if e' != e then
      trace[HouLean.sas.simp] "simplified:{indentExpr e}\n==>{indentExpr e'}"
    return e'

def mkIdentity (type : Expr) : Expr :=
  .lam `x type (.bvar 0) default

/-! ## Let Binding Helpers -/

def maybeLetBindValue (name : Name) (val : Expr) (k : Bool → Expr → SasM α) : SasM α := do
  if val.isFVar || !val.hasFVar then
    k false val
  else
    let type ← inferType val
    withLetDecl name type val fun var => k true var

def maybeLetBindValues (name : Name) (vals : Array Expr)
    (k : Array Expr → Array Expr → SasM α) : SasM α := do
  let rec go (i : Nat) : List Expr → Array Expr → Array Expr → SasM α
    | [], vars, xs' => k vars xs'
    | x :: xs, vars, xs' =>
      maybeLetBindValue (name.appendAfter (toString i)) x fun didBind x' =>
        let vars' := if didBind then vars.push x' else vars
        go (i + 1) xs vars' (xs'.push x')
  go 0 vals.toList #[] #[]

def withLetVars (vars : Array Expr) (x : SasM α) : SasM α :=
  withReader (fun ctx => { ctx with letVars := ctx.letVars ++ vars }) x

def withNewScope (x : SasM α) : SasM α :=
  withReader ({ · with letVars := #[] }) x

def withMaybeLetDecl (name : Name) (val : Expr) (k : Expr → SasM α) : SasM α :=
  maybeLetBindValue name val fun doBind var =>
    if doBind then withLetVars #[var] (k var) else k var

/-! ## Option Encoding -/
def Option.decode (x : α) (valid : Bool) : Option α :=
  if valid then some x else none

@[simp] theorem decode_true (x : α) : Option.decode x true = some x := by simp [Option.decode]
@[simp] theorem decode_false (x : α) : Option.decode x false = none := by simp [Option.decode]
@[simp] theorem decode_getD (x y : α) (valid : Bool) :
    (Option.decode x valid).getD y = if valid then x else y := by
  cases valid <;> simp [Option.decode]
@[simp] theorem decode_isSome (x : α) (valid : Bool) :
    (Option.decode x valid).isSome = valid := by
  cases valid <;> simp [Option.decode]
@[simp] theorem decode_isNone (x : α) (valid : Bool) :
    (Option.decode x valid).isNone = !valid := by
  cases valid <;> simp [Option.decode]

/-! ## Type Encoding -/

open Qq in
partial def typeEncoding (type : Expr) : SasM (Array Expr × Expr) := do

  if type.isAppOfArity ``Option 1 then
    return ← withLocalDeclD `x type fun x => do
      let t := type.appArg!
      let (encodings, decode) ← typeEncoding t
      let encodings ← encodings.mapM fun encode => do
        let body := encode.beta #[← mkAppM ``Option.getD #[x, ← mkAppOptM ``default #[t, none]]]
        mkLambdaFVars #[x] body
      let valid ← liftM <| mkAppM ``Option.isSome #[x] >>= mkLambdaFVars #[x]
      let decode ← forallTelescope (← inferType decode) fun xs _ =>
        withLocalDeclD `valid q(Bool) fun valid => do
          let body ← mkAppM ``Option.decode #[decode.beta xs, valid]
          mkLambdaFVars (xs.push valid) body
      return (encodings.push valid, decode)

  if type.isAppOfArity ``Vector 2 then
    if let some n ← runInterpreter? Nat type.appArg! then
      let type ← whnf (type.getArg! 0)
      let sname := Name.append `HouLean.Meta.Sas (← vectorStructName type n)
      return (#[.const (sname.append `fromVector) []], .const (sname.append `toVector) [])
    else
      throwError s!"size of the vector has to be known! got {← whnf type.appArg!}"

  return (#[mkIdentity type], mkIdentity type)

def mkUncurryFun (f : Expr) : MetaM Expr := do
  forallTelescope (← inferType f) fun xs _ => do
    let x ← mkProdElem xs
    withLocalDeclD `x (← inferType x) fun x => do
      let xs ← mkProdSplitElem x xs.size
      mkLambdaFVars #[x] (f.beta xs)

def uncurriedTypeEncoding (type : Expr) : SasM (Expr × Expr) := do
  let (encodings, decode) ← typeEncoding type
  let decode ← mkUncurryFun decode
  unless encodings.size > 0 do
    throwError "Unexpected empty type encoding for {type}!"
  forallTelescope (← inferType encodings[0]!) fun xs _ => do
    let e ← mkProdElem (encodings.map (·.beta xs))
    let encode ← mkLambdaFVars xs e
    return (encode, decode)

/-! ## Specialization Variables -/

private def withVarsToSpecializeSingle (ys : Array Expr) (decode : Expr)
    (k : Array Expr → Array Expr → Expr → String → SasM α) : SasM α := do
  let rec go : List Expr → Array Expr → Array Expr → Array Expr → String → SasM α
    | [], vars, vals, ys', suffix => do k vars vals (← simplify <| decode.beta ys') suffix
    | y :: ys, vars, vals, ys', suffix => do
      if !y.hasFVar then
        let type ← inferType y
        let s := s!"_{← ppExpr y}"
        let suffix := if (← isClass? type).isSome || (← inferType type).isProp || type.isForall then suffix else suffix ++ s
        go ys vars vals (ys'.push y) suffix
      else
        withLocalDeclD `a (← inferType y) fun var =>
          go ys (vars.push var) (vals.push y) (ys'.push var) suffix
  go ys.toList #[] #[] #[] ""

def withVarsToSpecialize (yss : Array (Array Expr)) (decodes : Array Expr)
    (k : Array Expr → Array Expr → Array Expr → String → SasM α) : SasM α := do
  unless yss.size == decodes.size do
    throwError "`yss` and `decodes` must have the same length!"
  let rec go : List (Array Expr) → List Expr → Array Expr → Array Expr → Array Expr → String → SasM α
    | [], [], vars, vals, xs, suffix => k vars vals xs suffix
    | ys :: yss, decode :: decodes, vars, vals, xs, suffix =>
      withVarsToSpecializeSingle ys decode fun vars' vals' x suffix' =>
        go yss decodes (vars ++ vars') (vals ++ vals') (xs.push x) (suffix ++ suffix')
    | _, _, _, _, _, _ => unreachable!
  go yss.toList decodes.toList #[] #[] #[] ""

/-! ## Specialization Request -/

def requestSpecialization (funToSpecialize : Expr) (funName : Name) (specSuffix : String) : SasM Expr := do
  let specName := funName.append (.mkSimple <| "spec" ++ specSuffix.replace "." "_" |>.replace " " "_")
  let specType ← inferType funToSpecialize
  trace[HouLean.sas] m!"specialization request:\n{specName}\n{funToSpecialize}\n{← isTypeCorrect funToSpecialize}"
  if !(← getEnv).contains specName then
    let decl : Declaration := .opaqueDecl {
      name := specName
      levelParams := []
      type := specType
      value := ← mkAppOptM ``default #[specType, none]
      isUnsafe := false
    }
    addDecl decl
  return .const specName []

def shouldSpecialize (fname : Name) : SasM Bool := do
  match ← getConstInfo fname with
  | .ctorInfo _ => return false
  | _ =>
    if let some info ← getProjectionFnInfo? fname then
      return info.fromClass
    return true


/-! ## Main Transformation -/

partial def forallEncodedTelescope (type : Expr) (k : Array (Array Expr) → Array Expr → Expr → SasM Expr) : SasM Expr := do
  go type #[] #[]
where
  go (t : Expr) (ys : Array (Array Expr)) (xs : Array Expr) : SasM Expr := do
    match t with
    | .forallE n t b _ =>
      withLocalDeclD `x t fun x => do
      let (encodings, decode) ← typeEncoding t
      let decls ← encodings.mapM (fun enc => do pure (n, (← inferType (enc.app x))))
      withLocalDeclsDND decls fun ys' => do
        let x' := decode.beta ys'
        go (b.instantiate1 x') (ys.push ys') (xs.push x')
    | _ => k ys xs type

def withEncodedVal (val : Expr) (k : Array Expr → Expr → SasM α) : SasM α := do
  let type ← liftM <| inferType val >>= whnf
  let (encodings, decode) ← typeEncoding type
  -- Note: simplify here may need different settings than the main simp pass
  let vals ← encodings.mapM fun enc => simplify (enc.beta #[val])
  k vals decode

partial def main (e : Expr) (cont : Array Expr → Expr → SasM Expr) : SasM Expr := do
  let type ← inferType e
  let id' := mkIdentity type

  -- trace[HouLean.sas] m!"processing {e}"

  -- Try interpreter for primitive types
  if let some val ← runInterpreterForPrimitiveTypes? e then
    return ← cont #[toExpr val] id'

  -- Early exits for types and typeclasses
  if type.isSort then return ← cont #[e] id'
  if (← isClass? type).isSome then return ← cont #[e] id'

  -- custom call

  let e ← simplify e


  -- trace[HouLean.sas] m!"proceeding to cases"

  match e with
  | .bvar .. | .fvar .. | .sort .. | .lit .. => withEncodedVal e cont
  | .mvar .. => main (← instantiateMVars e) cont
  | .const .. => withEncodedVal e cont
  | .mdata _ e => withEncodedVal e cont
  | .forallE .. => withEncodedVal e cont
  | .app .. =>

    -- if let some e' ← customSpec e then
    --   trace[HouLean.specialize] m!"custom specialization {e} ==> {e'}"
    --   return e'

    let (fn, args) :=  e.withApp fun fn args => (fn, args)
    appCase fn args
  | .lam .. => lamCase e
  | .letE .. => letCase e
  | .proj .. => projCase e
where
  processArgs (xs : Array Expr) (k : Array (Array Expr) → Array Expr → SasM Expr) : SasM Expr := do
    let rec go : List Expr → Array (Array Expr) → Array Expr → SasM Expr
      | [], xs', decodes => k xs' decodes
      | x :: xs, xs', decodes =>
        main x fun x' decode => do
          -- trace[HouLean.sas] m!"processed arg {x} : {← inferType x} ==> {x'} : {← liftM <| x'.mapM inferType} / {decode}"
          go xs (xs'.push x') (decodes.push decode)
    go xs.toList #[] #[]

  appCase (fn : Expr) (xs : Array Expr) : SasM Expr := do
    processArgs xs fun yss decodes => do
      if let some fname := fn.constName? then
        if ← shouldSpecialize fname then
          return ← withVarsToSpecialize yss decodes fun vars vals xs' specSuffix => do
            let body := fn.beta xs'
            let (encode, _) ← uncurriedTypeEncoding (← inferType body)
            let (encodings, decode) ← typeEncoding (← inferType body)
            let body ← simplify <| encode.beta #[body]

            -- if `fname` has been eliminated by simp then we do not specialize
            if (body.find? (·.constName? == some fname)).isNone then
              return ← cont #[body.replaceFVars vars vals] decode

            let funToSpecialize ← mkLambdaFVars vars body
            let fn'' ← requestSpecialization funToSpecialize fname specSuffix
            withMaybeLetDecl `tmp (fn''.beta vals) fun body => do
              let e ← mkProdSplitElem body encodings.size
              cont e decode

      -- Fallback: decode and re-encode
      let xs' := (yss.zip decodes).map fun (ys, decode) => decode.beta ys
      withEncodedVal (fn.beta xs') cont

  lamCase (e : Expr) : SasM Expr := do
    let e ← lambdaTelescope e fun xs body => do
      let body' ← withNewScope do
        main body fun es decode => do
          mkLetFVars (← read).letVars (decode.beta es) (generalizeNondepLet := false)
      mkLambdaFVars xs body' (generalizeNondepLet := false)
    withEncodedVal e cont

  letCase (e : Expr) : SasM Expr := do
    let .letE n _t v b _nondep := e | panic! "expected let expression"
    main v fun ys decode =>
      maybeLetBindValues n ys fun letVars ys' =>
        withLetVars letVars do
          let v' := decode.beta ys'
          main (b.instantiate1 v') cont

  projCase (e : Expr) : SasM Expr := do
    let e' := (← reduceProj? e).getD e
    withEncodedVal e' cont

/-! ## Entry Point -/

def sas (e : Expr) (attrs : Array Name) : MetaM Expr := do
  let result ← (show SasM Expr from do
    forallEncodedTelescope (← inferType e) fun ys xs _ => do
      let body := e.beta xs
      let body' ←
        main body fun es decode => do
          mkLetFVars (← read).letVars (← mkProdElem es) (generalizeNondepLet := false)
      pure (← mkLambdaFVars ys.flatten body')
  ).run attrs
  return result

end HouLean.Meta.Sas
