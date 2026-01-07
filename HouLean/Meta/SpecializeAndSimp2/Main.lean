import Lean
import Qq
import HouLean.Meta.Basic
import HouLean.Meta.RunInterpreter
import HouLean.Meta.SpecializeAndSimp2.Types
import HouLean.Meta.SpecializeAndSimp2.Encoding
import HouLean.Meta.SpecializeAndSimp2.Encoding.Vector
import HouLean.Meta.SpecializeAndSimp2.Encoding.Option

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

def getForallNames (t : Expr) : Array Name :=
  go t #[]
where
  go (t : Expr) (ns : Array Name) :=
    match t with
    | .forallE n _ b _ => go b (ns.push n)
    | .mdata _ t => go t ns
    | _ => ns

mutual

/-- Maybe bind `val` into a let fvar(s) then runs
```
k letVars x
```
where `letVars` are newly introduced fvars and `x` uses them and is equal to `val` -/
partial def maybeLetBindVal (name : Name) (val : Expr) (k : Array Expr → Expr → SasM α) : SasM α := do
  if val.isFVar || !val.hasFVar || (← inferType val).isForall then
    k #[] val
  else if let some (ctor, xs) ← constructorApp? val then
    let params := xs[0:ctor.numParams]
    let xs := xs[ctor.numParams:]
    let fn := val.getAppFn'
    let names := (getForallNames (← inferType fn))[ctor.numParams:].toArray
    let names := names.map (fun n => name.append n)
    maybeLetBindVals names xs fun letVars xs' => k letVars (fn.beta (params ++ xs'))
  else
    let type ← inferType val
    withLetDecl name type val fun var => k #[var] var

partial def maybeLetBindVals (names : Array Name) (vals : Array Expr) (k : Array Expr → Array Expr → SasM α) := do
  go 0 (.emptyWithCapacity names.size) (.emptyWithCapacity vals.size)
where
  go (i : Nat) (letVars xs : Array Expr) : SasM α := do
    if h : i < vals.size ∧ i < names.size then
      maybeLetBindVal names[i] vals[i] (fun letVars' x' =>
        go (i+1) (letVars ++ letVars') (xs.push x'))
    else
      k letVars xs

end

-- /-- Maybe bind `val` into a let fvar(s) -/
-- def maybeLetBindValue (name : Name) (val : Expr) (k : Bool → Expr → SasM α) : SasM α := do
--   if val.isFVar || !val.hasFVar || (← inferType val).isForall then
--     k false val
--   else
--     let type ← inferType val
--     withLetDecl name type val fun var => k true var

-- def maybeLetBindValues (name : Name) (vals : Array Expr)
--     (k : Array Expr → Array Expr → SasM α) : SasM α := do
--   let rec go (i : Nat) : List Expr → Array Expr → Array Expr → SasM α
--     | [], vars, xs' => k vars xs'
--     | x :: xs, vars, xs' =>
--       maybeLetBindValue (name.appendAfter (toString i)) x fun didBind x' =>
--         let vars' := if didBind then vars.push x' else vars
--         go (i + 1) xs vars' (xs'.push x')
--   go 0 vals.toList #[] #[]

def withLetVars (vars : Array Expr) (x : SasM α) : SasM α :=
  withReader (fun ctx => { ctx with letVars := ctx.letVars ++ vars }) x

def withNewScope (x : SasM α) : SasM α :=
  withReader ({ · with letVars := #[] }) x

def withMaybeLetDecl (name : Name) (val : Expr) (k : Expr → SasM α) : SasM α :=
  maybeLetBindVal name val fun letVars val' =>
    withLetVars letVars (k val')

/-! ## Type Encoding -/

open Qq in
partial def typeEncoding (type : Expr) : SasM (Expr × Expr) := do
  -- let type ← simplify type
  try
    let X := type
    let Y ← mkFreshTypeMVar
    let cls ← mkAppM ``TypeEncoding #[X,Y]
    let inst ← synthInstance cls

    let encode ← mkAppOptM ``TypeEncoding.encode #[X, Y, inst]
    let decode ← mkAppOptM ``TypeEncoding.decode #[X, Y, inst]

    return (encode, decode)
  catch _ =>
    return (mkIdentity type, mkIdentity type)

  -- if type.isAppOfArity ``Option 1 then
  --   return ← withLocalDeclD `x type fun x => do
  --     let t := type.appArg!
  --     let (encodings, decode) ← typeEncoding t
  --     let encodings ← encodings.mapM fun encode => do
  --       let body := encode.beta #[← mkAppM ``Option.encodeVal #[x]]
  --       mkLambdaFVars #[x] body
  --     let valid ← liftM <| mkAppM ``Option.encodeValid #[x] >>= mkLambdaFVars #[x]
  --     let decode ← forallTelescope (← inferType decode) fun xs _ =>
  --       withLocalDeclD `valid q(Bool) fun valid => do
  --         let body ← mkAppM ``Option.decode #[decode.beta xs, valid]
  --         mkLambdaFVars (xs.push valid) body
  --     return (encodings.push valid, decode)

  -- if type.isAppOfArity ``Vector 2 then
  --   if let some n ← runInterpreter? Nat type.appArg! then
  --     let type ← whnf (type.getArg! 0)
  --     let sname := Name.append `HouLean.Meta.Sas (← vectorStructName type n)
  --     return (#[.const (sname.append `fromVector) []], .const (sname.append `toVector) [])
  --   else
  --     throwError s!"size of the vector has to be known! got {← whnf type.appArg!}"


def mkUncurryFun (f : Expr) : MetaM Expr := do
  forallTelescope (← inferType f) fun xs _ => do
    let x ← mkProdElem xs
    withLocalDeclD `x (← inferType x) fun x => do
      let xs ← mkProdSplitElem x xs.size
      mkLambdaFVars #[x] (f.beta xs)

-- def uncurriedTypeEncoding (type : Expr) : SasM (Expr × Expr) := do
--   let (encodings, decode) ← typeEncoding type
--   let decode ← mkUncurryFun decode
--   unless encodings.size > 0 do
--     throwError "Unexpected empty type encoding for {type}!"
--   forallTelescope (← inferType encodings[0]!) fun xs _ => do
--     let e ← mkProdElem (encodings.map (·.beta xs))
--     let encode ← mkLambdaFVars xs e
--     return (encode, decode)

/-! ## Specialization Variables -/

private def withVarsToSpecializeSingle (y : Expr) (decode : Expr)
    (k : Array Expr → Array Expr → Expr → String → SasM α) : SasM α := do
      if !y.hasFVar then
        let type ← inferType y
        let s := s!"_{← ppExpr y}"
        let suffix := if (← isClass? type).isSome || (← inferType type).isProp || type.isForall then "" else s
        k #[] #[] (decode.beta #[y]) suffix
      else
        -- todo: somehow handle dependent types here
        --       the newly introduced fvar might have to appear in types of some `ys`
        withLocalDeclD `a (← inferType y) fun var =>
           k #[var] #[y] (decode.beta #[var]) ""


-- todo: somehow handle dependent types
def withVarsToSpecialize (yss : Array Expr) (decodes : Array Expr)
    (k : Array Expr → Array Expr → Array Expr → String → SasM α) : SasM α := do
  unless yss.size == decodes.size do
    throwError "`yss` and `decodes` must have the same length!"
  let rec go : List Expr → List Expr → Array Expr → Array Expr → Array Expr → String → SasM α
    | [], [], vars, vals, xs, suffix => k vars vals xs suffix
    | ys :: yss, decode :: decodes, vars, vals, xs, suffix =>
      withVarsToSpecializeSingle ys decode fun vars' vals' x suffix' =>
        go yss decodes (vars ++ vars') (vals ++ vals') (xs.push x) (suffix ++ suffix')
    | _, _, _, _, _, _ => unreachable!
  go yss.toList decodes.toList #[] #[] #[] ""

/-! ## Specialization Request -/

def requestSpecialization (specType funToSpecialize : Expr) (funName : Name) (specSuffix : String) : SasM Expr := do
  if funToSpecialize.hasFVar then
    let fvars := (← funToSpecialize.collectFVars.run {}).2.fvarIds.map (Expr.fvar ·)
    throwError m!"can't specialize {funToSpecialize} it has fvars: {fvars}"

  let specName := funName.append (.mkSimple <| specSuffix.dropWhile (·=='_') |>.replace "." "_" |>.replace " " "_")
  -- let specType ← inferType funToSpecialize

  if !(← getEnv).contains specName then
    trace[HouLean.sas] m!"specialization request:\n{specName}\n{funToSpecialize}\n{← isTypeCorrect funToSpecialize}"
    let decl : Declaration := .opaqueDecl {
      name := specName
      levelParams := []
      type := specType
      -- todo: generate a message if this fails, as random "can't syntasize `Inhabited ...`" error is very confusing
      value := ← mkAppOptM ``default #[specType, none]
      isUnsafe := false
    }
    addDecl decl
    let req : SpecializationRequest := {
      funName := funName
      specName := specName
      funToSpecialize := funToSpecialize
    }
    modify (fun s => {s with requests := req :: s.requests })
  return .const specName []

def shouldSpecialize (fname : Name) : MetaM Bool := do
  if fname == ``TypeEncoding.decode then
    return false

  if fname == ``TypeEncoding.encode then
    return false

  if fname == ``getElem then
    return false

  if let .defnInfo _info ← getConstInfo fname then

    if let some info ← getProjectionFnInfo? fname then
      return info.fromClass
    if fname == ``ite then
      return false

    if ← isMatcher fname then
      return false

    -- if fname == ``VectorFloat3.toVector then
    --   return false

    -- if fname == ``VectorFloat2.toVector then
    --   return false

    if fname == ``Option.decode then
      return false

    return true
  return false


partial def forallEncodedTelescope (type : Expr) (k : Array Expr → Array Expr → Expr → SasM Expr) : SasM Expr := do
  go type #[] #[]
where
  go (t : Expr) (ys : Array Expr) (xs : Array Expr) : SasM Expr := do
    match t with
    | .forallE n t b _ =>
      withLocalDeclD `x t fun x => do
      let (encode, decode) ← typeEncoding t
      let t' ← inferType (encode.app x)
      withLocalDeclD n t' fun y' => do
        let x' := decode.beta #[y']
        go (b.instantiate1 x') (ys.push y') (xs.push x')
    | _ => k ys xs type

def withEncodedVal (val : Expr) (k : Expr → Expr → SasM α) : SasM α := do
  let type ← liftM <| inferType val >>= whnf
  let (encode, decode) ← typeEncoding type
  -- Note: simplify here may need different settings than the main simp pass
  k (← simplify (encode.beta #[val])) decode

/-! ## Specialize only args -/
open Qq

/-- Replace `lhs` with `rhs` and specialize its arguments. -/
structure Override where
  keys : Array DiscrTree.Key
  lhs : Expr -- fun x₁ ... xₙ => e
  rhs : Expr -- fun x₁ ... xₙ => e'
  -- maybe store proof? that `e = e'`
  priority : Nat := eval_prio default
deriving BEq

def addIdentityOverride (e : Expr) (t : DiscrTree Override) : MetaM (DiscrTree Override) := do
  let (_,_,b) ← lambdaMetaTelescope e
  let keys ← DiscrTree.mkPath b

  let ovrd : Override := {
    keys := keys
    lhs := e
    rhs := e
  }
  return t.insertCore keys ovrd

def overrideTree : MetaM (DiscrTree Override) :=
  pure DiscrTree.empty
  >>= addIdentityOverride q(fun x : Float => -x)
  >>= addIdentityOverride q(fun x y : Float => x + y)
  >>= addIdentityOverride q(fun x y : Float => x - y)
  >>= addIdentityOverride q(fun x y : Float => x * y)
  >>= addIdentityOverride q(fun x y : Float => x / y)
  >>= addIdentityOverride q(fun x : Float32 => -x)
  >>= addIdentityOverride q(fun x y : Float32 => x + y)
  >>= addIdentityOverride q(fun x y : Float32 => x - y)
  >>= addIdentityOverride q(fun x y : Float32 => x * y)
  >>= addIdentityOverride q(fun x y : Float32 => x / y)
  >>= addIdentityOverride q(fun (n i : Nat) [OfNat (Fin n) i] => OfNat.ofNat (α := Fin n) i)
  >>= addIdentityOverride q(fun (i : Nat) [OfNat Nat i] => OfNat.ofNat (α := Nat) i)


def applyOverride? (e : Expr) (cont : Array Expr → Expr → SasM Expr) : SasM (Option Expr) := do

  let tree ← overrideTree
  let candidates ← tree.getMatch e

  trace[HouLean.sas] "override candidates for {e}: {candidates.map (fun c => c.lhs)}"

  for c in candidates do

    trace[HouLean.sas] "trying candidate {c.lhs}"

    let (xs,_,lhs) ← lambdaMetaTelescope c.lhs

    -- does `e` unify with `lhs`?
    unless ← isDefEq e lhs do
      continue

    trace[HouLean.sas] "does unify"

    -- ensure that all arguments are infered
    unless ← synthArgs xs do
      continue

    trace[HouLean.sas] "all arguments are good!"

    return ← cont xs c.rhs

  return none
where
  synthArgs (xs : Array Expr) : MetaM Bool := do
    for x in xs do
      let x ← instantiateMVars x
      if ¬x.isMVar then
        continue
      let t ← inferType x

      -- only class arguments can be automatically synthesized right now
      unless (← isClass? t).isSome do
        return false

      -- find an instance
      let some inst ← synthInstance? t
        | return false

      -- assign instance to `x`
      if ¬(← isDefEq x inst) then
        return false

    return true


/-- Unfold a definition, handling projections specially. -/
private def unfoldDef (e : Expr) (declName : Name) : SasM Expr := do
  let unfolded := (← unfold e declName).expr
  if ← isProjectionFn declName then
    unfolded.withApp fun fn args => do
      let fn ← whnfI fn
      return fn.beta args
  else
    return unfolded


/-! ## Main Transformation -/

partial def main (e : Expr) (cont : Expr → Expr → SasM Expr) : SasM Expr := do
  -- withIncRecDepth do
  let type ← inferType e
  let id' := mkIdentity type

  -- Try interpreter for primitive types
  if let some val ← runInterpreterForPrimitiveTypes? e then
    return ← cont (toExpr val) id'

  -- Early exits for types, typeclasses and proofs
  if type.isSort then return ← cont e id'
  if (← isClass? type).isSome then return ← cont e id'
  if (← inferType type).isProp then return ← cont e id'

  withTraceNode `HouLean.sas (collapsed := false) (fun r => do return m!"[{exceptEmoji r}] {e}") do

  let e' := e
  let e ← simplify e

  trace[HouLean.sas] "hihi: {e'} ==> {e}"

  match e with
  | .bvar .. | .fvar .. | .sort .. | .lit .. => withEncodedVal e cont
  | .mvar .. => main (← instantiateMVars e) cont
  | .const .. => withEncodedVal e cont
  | .mdata _ e => withEncodedVal e cont
  | .forallE .. => withEncodedVal e cont
  | .app .. =>

    if let some e' ← appOverride? e then
      return ← withEncodedVal e cont

    let (fn, args) :=  e.withApp fun fn args => (fn, args)
    appCase fn args
  | .lam .. => lamCase e
  | .letE .. => letCase e
  | .proj .. => projCase e
where
  processArgs (xs : Array Expr) (k : Array Expr → Array Expr → SasM Expr) : SasM Expr := do
    let rec go : List Expr → Array Expr → Array Expr → SasM Expr
      | [], xs', decodes => k xs' decodes
      | x :: xs, xs', decodes =>
        main x fun x' decode => do
          -- trace[HouLean.sas] m!"processed arg {x} : {← inferType x} ==> {x'} : {← liftM <| x'.mapM inferType} / {decode}"
          go xs (xs'.push x') (decodes.push decode)
    go xs.toList #[] #[]

  processArgs' (xs : Array Expr) (k : Array Expr → SasM Expr) : SasM Expr := do
    let rec go (i : Nat) (xs' : Array Expr) : SasM Expr := do
      if h : i < xs.size then
        main xs[i] fun y decode => do
          go (i+1) (xs'.push (decode.beta #[y]))
      else
        k xs'
    go 0 #[]

  appCase (fn : Expr) (xs : Array Expr) : SasM Expr := do
    -- let funInfo ← getFunInfo fn -- will be usefull at some point
    processArgs xs fun ys decodes => do
      if let some fname := fn.constName? then
        trace[HouLean.sas] m!"should specialize {fname}: {← shouldSpecialize fname}"
        if ← shouldSpecialize fname then
          return ← withVarsToSpecialize ys decodes fun vars vals xs' specSuffix => do
            let body := fn.beta xs'
            let funToSpecialize ← mkLambdaFVars vars body

            trace[HouLean.sas] "specializing: {e}\n\
                       yss: {ys}\n\
                       spec vars: {vars}\n\
                       args: {xs} ==> {xs'}\n\
                       to specialize: {funToSpecialize}"

            unless ← isTypeCorrect funToSpecialize do
              throwError m!"function to specialize is not type correct{indentExpr funToSpecialize}"

            let (encode, decode) ← typeEncoding (← inferType body)
            let body ← simplify <| encode.beta #[body]

            -- -- if `fname` has been eliminated by simp then we do not specialize
            -- if (body.find? (·.constName? == some fname)).isNone then
            --   return ← withEncodedVal (decode.beta #[body.replaceFVars vars vals]) cont

            let specType ← mkForallFVars vars (← inferType body)

            -- logInfo m!"specializing: {e}\n\
            --            yss: {yss}\n\
            --            spec vars: {vars}\n\
            --            args: {xs} ==> {xs'}\n\
            --            to specialize: {funToSpecialize}"

            let fn'' ← requestSpecialization specType funToSpecialize fname specSuffix
            withMaybeLetDecl `tmp (fn''.beta vals) fun body => do
              cont body decode

        if fname == ``ite then
          let c := xs[1]!
          let t := xs[3]!
          let e := xs[4]!
          -- todo: we need to deal with dependent types
          -- let c' ← main (← mkAppOptM ``decide #[c,none]) cont
          -- let c' ← mkAppM ``Eq #[c', (.const ``Bool.true [])]
          let c' ← main c fun e decode => do
             mkLetFVars (← read).letVars (decode.beta #[e]) (generalizeNondepLet := false)
          let t' ← main t cont
          let e' ← main e cont
          return ← mkAppM ``ite #[c',t',e']

        -- custom call
        if let some e ← letBindMatchDiscrs? (fn.beta xs) (doUnfold:=true) then
          trace[HouLean.sas] m!"potentialy match simplification:{indentExpr (fn.beta xs)}\n==>{indentExpr e}"


      -- Fallback: decode and re-encode
      let xs' := (ys.zip decodes).map fun (y, decode) => decode.beta #[y]
      withEncodedVal (fn.beta xs') cont

  -- custom override for `e`
  appOverride? (e : Expr) : SasM (Option Expr) := do
    applyOverride? e fun xs rhs => do
      processArgs' xs fun xs' => do
        return rhs.beta xs'

  lamCase (e : Expr) : SasM Expr := do
    let e ← lambdaTelescope e fun xs body => do
      let body' ← withNewScope do
        main body fun e decode => do
          mkLetFVars (← read).letVars (decode.beta #[e]) (generalizeNondepLet := false)
      mkLambdaFVars xs body' (generalizeNondepLet := false)
    withEncodedVal e cont

  letCase (e : Expr) : SasM Expr := do
    let .letE n _t v b _nondep := e | panic! "expected let expression"
    main v fun v' decode =>
      maybeLetBindVal n v' fun letVars v'' =>
        withLetVars letVars do
          let v' := decode.beta #[v'']
          main (b.instantiate1 v') cont

  projCase (e : Expr) : SasM Expr := do
    let e' := (← reduceProj? e).getD e
    withEncodedVal e' cont



def processRequest (req : SpecializationRequest) : SasM Unit := do

  withTraceNode `HouLean.sas (fun r => do return m!"[{exceptEmoji r} {req.specName}{indentExpr req.funToSpecialize}]") do

  forallTelescope (← inferType req.funToSpecialize) fun xs _ => do

    let body ← unfoldDef (req.funToSpecialize.beta xs) req.funName
    let type ← inferType body
    let (encode, decode) ← typeEncoding type
    let body ← main body fun e _decode => do
          mkLetFVars (← read).letVars e (generalizeNondepLet := false)

    let implValue ← mkLambdaFVars xs body >>= instantiateMVars
    let implType ← inferType implValue
    let implName := req.specName.append `impl

    let decl : Declaration := .defnDecl {
      name := implName
      levelParams := []
      type := implType
      value := implValue
      hints := .regular implValue.approxDepth
      safety := .safe
    }

    addDecl decl

    modify (fun s => {s with specs := s.specs.push {req, implName}})

    trace[HouLean.sas] "result: {implValue}"


/-! ## Entry Point -/

def sas (e : Expr) (attrs : Array Name) : MetaM (Expr × Array Specialization) := do

  let go : SasM _ := do
    let result ← forallEncodedTelescope (← inferType e) fun ys xs _r => do
      let body := e.beta xs
      let body' ←
        main body fun e _decode => do
          mkLetFVars (← read).letVars e (generalizeNondepLet := false)
      pure (← mkLambdaFVars ys body')

    while !(← get).requests.isEmpty do
      let req :: requests := (← get).requests | continue
      modify (fun s => {s with requests := requests})
      processRequest req

      -- match (← get).requests with
      -- | [] => pure ()


    return (result, (← get).specs)

  let result ← go.run attrs
  return result

end HouLean.Meta.Sas

-- open Lean Meta
-- run_meta
--   let mut i : Nat := 0
--   while i < 10 do
--     logInfo m!"{i}"
--     i := i + 1

-- open Lean Meta
-- run_meta
--   let mut i? : Option Nat := some 0
--   while i?.isSome do
--     logInfo m!"{i?}"
--     i? := i?.map (·+1)
--     if i? == some 10 then
--       i? := none
