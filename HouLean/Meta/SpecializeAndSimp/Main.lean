import HouLean.Meta.SpecializeAndSimp.Types
import HouLean.Meta.OverloadedFunction
import HouLean.Meta.RunInterpreter
import Lean
import Qq

open Lean Meta

namespace HouLean.Meta.SpecializeAndSimp

initialize registerTraceClass `HouLean.specialize
initialize registerTraceClass `HouLean.specialize.detail
initialize registerTraceClass `HouLean.specialize.simplify
initialize registerTraceClass `HouLean.specialize.liftLets

/-- Result of specializing a function application `fn arg₁ .. argₙ` to `fn' arg'₁ ... arg'ₘ`
    where compile-time known arguments are consumed in `fn'`. -/
structure FunSpecializationResult where
  funName : Name
  fn : Expr
  args : Array Expr
  fn' : Expr
  args' : Array Expr
  mangledName : Name

private def exprToString (e : Expr) : MetaM String := do
  return toString (← ppExpr e) |>.replace " " "_" |>.replace "." "_" |>.replace "-" "_"

/-- Check if an argument should contribute to the mangled name. -/
private def shouldAddToMangledName (type : Expr) : MetaM Bool := do
  if (← isClass? type).isSome then return false
  if (← inferType type).isProp then return false
  return true

open Qq in
def maybeEvalWithInterpreter (e : Expr) : MetaM Expr := do
  if let some val ← runInterpreterForPrimitiveTypes? e then
    return toExpr val
  return e

/-- Specialize a function application by consuming compile-time known arguments. -/
def specializeFunApp (fn : Expr) (args : Array Expr) : MetaM FunSpecializationResult := do
  let some funName := fn.constName?
    | throwError "specializeFunApp: expected const function, got {fn.ctorName}"

  trace[HouLean.specialize.detail] "specializing {funName} with {args.size} args"

  let (fn', args', nameParts) ← go fn args.toList #[] #[] #[]
  let suffix := nameParts.foldl (· ++ "_" ++ ·) ""
  let mangledName := funName.appendAfter suffix

  trace[HouLean.specialize.detail] "mangled name: {mangledName}, remaining args: {args'.size}"

  return { funName, fn, args, fn' := fn'.eta, args', mangledName }
where
  go (e : Expr) (args : List Expr) (vars : Array Expr) (remainingArgs : Array Expr)
     (nameParts : Array String) : MetaM (Expr × Array Expr × Array String) := do
    match args with
    | [] =>
      let f ← mkLambdaFVars vars e
      return (f, remainingArgs, nameParts)
    | arg :: rest =>
      if !(arg.hasFVar || arg.hasMVar) then
        let arg ← maybeEvalWithInterpreter arg
        -- Arg is compile-time known, consume it via beta reduction
        let type ← inferType arg
        let nameParts ←
          if ← shouldAddToMangledName type then
            pure (nameParts.push (← exprToString arg))
          else
            pure nameParts
        go (e.beta #[arg]) rest vars remainingArgs nameParts
      else
        -- Arg has free/meta vars, keep it as a parameter
        forallBoundedTelescope (← inferType e) (some 1) fun xs _ => do
          let some x := xs[0]? | throwError m!"bug in {decl_name%}, very odd case: {e} : {← inferType e}"
          go (e.beta #[x]) rest (vars.push x) (remainingArgs.push arg) nameParts

def addSpecialization (s : Specialization) : M Unit :=
  modify fun state => { state with
    specializations := state.specializations.insertCore s.keys s
    specOrder := state.specOrder.push s.specializationName
  }

/-- Unfold a definition, handling projections specially. -/
private def unfoldDef (e : Expr) (declName : Name) : MetaM Expr := do
  let unfolded := (← unfold e declName).expr
  if ← isProjectionFn declName then
    unfolded.withApp fun fn args => do
      let fn := (← reduceProj? fn).getD fn
      return fn.beta args
  else
    return unfolded

def isInlineType (type : Expr) : MetaM Bool := do
  if type.isForall then
    forallTelescope type fun _ r => do
      if r.isSort then
        return false
      else if (← isClass? r).isSome then
        return false
      else
        return true
  else
    return false

def shouldInline (fname : Name) (args : Array Expr) : MetaM Bool := do
  if fname == ``Bind.bind then
    return false

  for arg in args do
    let t ← inferType arg
    if ← isInlineType t then
      trace[HouLean.specialize] "inlining {fname} because argument {arg} is function type!"
      return true
  return false

def inline? (e : Expr) : MetaM (Option Expr) := do
  if let some fname := e.getAppFn.constName? then
    let args := e.getAppArgs
    if ← shouldInline fname args then
      let e' := (← unfold e fname).expr
      if e' != e then
        trace[HouLean.specialize] m!"inlined {fname}: {indentExpr e}\n  ==> {indentExpr e'}"
        return e'
  return none

/-- Define a new specialized function and register it. -/
partial def defineNewSpecialization (r : FunSpecializationResult) : M (Option Name) := do
  withTraceNode `HouLean.specialize (fun res => return m!"[{exceptEmoji res}] Specializing {r.fn'} : {← inferType r.fn'}") do
  forallTelescope (← inferType r.fn') fun xs _ => do
    let body := r.fn'.beta xs
    let body' ← unfoldDef body r.funName

    if body == body' then
      trace[HouLean.specialize] "cannot unfold {r.funName}, skipping specialization"
      return none

    let body'' ← specializeAndSimp body'
    let value ← mkLambdaFVars xs body'' >>= instantiateMVars
    let type ← inferType value
    let name ← Meta.mkUniqueDeclName r.mangledName

    let decl : Declaration := .defnDecl {
      name := name
      levelParams := []
      type := type
      value := value
      hints := .regular value.approxDepth
      safety := .safe
    }

    addAndCompile decl
    trace[HouLean.specialize] "compiled {name}"

    let (xs,_,_) ← forallMetaTelescope (← inferType r.fn')
    let keys ← DiscrTree.mkPath (r.fn'.beta xs)

    addSpecialization {
      keys := keys
      originalName := r.funName
      specializationName := name
      fn := r.fn'
    }
    return some name

/-- Check if specialization should be skipped for this expression. -/
private def skipSpecialization (e : Expr) : M Bool := do
  let type ← inferType e
  if (← isClass? type).isSome then return true
  if (← inferType type).isProp then return true
  if ← isMatcherApp e then return true
  if let some decl := e.getAppFn.constName? then
    if ← isRecursiveDefinition decl then
      return true
  return false

def specializeArgsOnly (funName : Name) : M Bool := do
  if [``ite, ``dite, ``forIn, ``LT.lt, ``LE.le, ``Bind.bind].contains funName then
    return true

  if let some info ← getProjectionFnInfo? funName then
    if ¬info.fromClass then
      return true

  if ← isMatcher funName then
    return true

  if let .defnInfo _ ← getConstInfo funName then
    return false

  return true

/-- Specialize a function application. -/
partial def specializeApp (fn : Expr) (args : Array Expr) : M Expr := do
  withConfig (fun cfg => {cfg with zeta := false, zetaDelta := false}) do

  match fn with
  | .const fname _ =>
    if ← specializeArgsOnly fname then
      return fn.beta args

    let r ← specializeFunApp fn args

    -- Check for existing specialization
    let specs := (← get).specializations
    let candidates ←
      forallTelescope (← inferType r.fn') fun xs _ =>
        specs.getMatch (r.fn'.beta xs)
    for c in candidates do
      if ← isDefEq r.fn' c.fn then
        trace[HouLean.specialize.detail] "reusing specialization {c.specializationName}"
        return ← mkAppOptM c.specializationName (r.args'.map some)

    -- Create new specialization
    if let some specName ← defineNewSpecialization r then
      try
        return ← mkAppOptM specName (r.args'.map some)
      catch _ =>
        throwError m!"failed applying {args} to {specName}"
    else
      return r.fn'.beta r.args'

  | other =>
    throwError "specializeApp: unhandled function kind {other.ctorName}, {fn} {args}"

/-- Main entry point: specialize and simplify an expression. -/
partial def specializeAndSimpImpl (e : Expr) : M Expr := do
  withIncRecDepth do
  withTraceNode `HouLean.specialize (fun r => return m!"[{exceptEmoji r}] {e}") do
  trace[HouLean.specialize.detail] "processing {e.ctorName}"

  let e ← instantiateMVars e

  if ← skipSpecialization e then
    return e

  if let some val ← runInterpreterForPrimitiveTypes? e then
    return toExpr val

  match e with
  | .letE n t v b nonDep =>
    let v' ← specializeAndSimp v

    -- Float out nested lets
    if v'.isLet then
      return ← letTelescope v' fun xs v'' => do
        let e' ← specializeAndSimp (.letE n t v'' b nonDep)
        mkLetFVars xs e'

    withLetDecl n t v' fun v'' => do
      let b' ← specializeAndSimp (b.instantiate1 v'')
      mkLetFVars #[v''] b'

  | .app .. =>
    let mut e := e
    e ← simplify e
    e ← liftLets e

    if e.isLet then
      return ← specializeAndSimp e

    if let some e' ← customSpec e then
      trace[HouLean.specialize] m!"custom specialization {e} ==> {e'}"
      return e'

    -- inline function?
    if let some e' ← inline? e then
      return ← specializeAndSimp e'

    match e with
    | .app .. =>
      let mut (fn, args) := e.withApp (·,·)
      let mut hasLet := false
      for arg in args, i in [0:args.size] do
        let arg' ← specializeAndSimp arg
        if arg'.isLet then
          hasLet := true
        args := args.set! i arg'

      if hasLet then
        return ← specializeAndSimp (fn.beta args)

      return ← specializeApp fn args

    | .fvar _ =>
      return e

    | _ =>
      throwError "specializeAndSimpImpl: unhandled case {e.ctorName}"

  | _ => return e
where
  simplify (e : Expr) : M Expr := do
    withTraceNode `HouLean.specialize.simplify (fun _ => return m!"simplify") do
      let e' := (← Simp.simp e).expr
      if e != e' then
        trace[HouLean.specialize] "simplified {indentExpr e}\n  ==> {indentExpr e'}"
      return e'

  liftLets (e : Expr) : M Expr := do
    withTraceNode `HouLean.specialize.liftLets (fun _ => return m!"liftLets") do
      Meta.liftLets e

initialize specializeAndSimpRef.set specializeAndSimpImpl

end HouLean.Meta.SpecializeAndSimp

#exit

import HouLean.Meta.SpecializeAndSimp.Types
import HouLean.Meta.OverloadedFunction
import HouLean.Meta.RunInterpreter
import Lean
import Qq

open Lean Meta

namespace HouLean.Meta.SpecializeAndSimp

initialize registerTraceClass `HouLean.specialize
initialize registerTraceClass `HouLean.specialize.detail
initialize registerTraceClass `HouLean.specialize.simplify
initialize registerTraceClass `HouLean.specialize.liftLets

/-- Result of specializing a function application `fn arg₁ .. argₙ` to `fn' arg'₁ ... arg'ₘ`
    where compile-time known arguments are consumed in `fn'`. -/
structure FunSpecializationResult where
  funName : Name
  fn : Expr
  args : Array Expr
  fn' : Expr
  args' : Array Expr
  mangledName : Name

private def exprToString (e : Expr) : MetaM String := do
  return toString (← ppExpr e) |>.replace " " "_" |>.replace "." "_" |>.replace "-" "_"

/-- Check if an argument should contribute to the mangled name. -/
private def shouldAddToMangledName (type : Expr) : MetaM Bool := do
  if (← isClass? type).isSome then return false
  if (← inferType type).isProp then return false
  return true

open Qq in
def maybeEvalWithInterpreter (e : Expr) : MetaM Expr := do
  if let some val ← runInterpreterForPrimitiveTypes? e then
    return toExpr val
  return e

/-- Specialize a function application by consuming compile-time known arguments. -/
def specializeFunApp (fn : Expr) (args : Array Expr) : MetaM FunSpecializationResult := do
  let some funName := fn.constName?
    | throwError "specializeFunApp: expected const function, got {fn.ctorName}"

  trace[HouLean.specialize.detail] "specializing {funName} with {args.size} args"

  let (fn', args', nameParts) ← go fn args.toList #[] #[] #[]
  let suffix := nameParts.foldl (· ++ "_" ++ ·) ""
  let mangledName := funName.appendAfter suffix

  trace[HouLean.specialize.detail] "mangled name: {mangledName}, remaining args: {args'.size}"

  return { funName, fn, args, fn' := fn'.eta, args', mangledName }
where
  go (e : Expr) (args : List Expr) (vars : Array Expr) (remainingArgs : Array Expr)
     (nameParts : Array String) : MetaM (Expr × Array Expr × Array String) := do
    match args with
    | [] =>
      let f ← mkLambdaFVars vars e
      return (f, remainingArgs, nameParts)
    | arg :: rest =>
      if !(arg.hasFVar || arg.hasMVar) then
        let arg ← maybeEvalWithInterpreter arg
        -- Arg is compile-time known, consume it via beta reduction
        let type ← inferType arg
        let nameParts ←
          if ← shouldAddToMangledName type then
            pure (nameParts.push (← exprToString arg))
          else
            pure nameParts
        go (e.beta #[arg]) rest vars remainingArgs nameParts
      else
        -- Arg has free/meta vars, keep it as a parameter
        forallBoundedTelescope (← inferType e) (some 1) fun xs _ => do
          let some x := xs[0]? | throwError m!"bug in {decl_name%}, very odd case: {e} : {← inferType e}"
          go (e.beta #[x]) rest (vars.push x) (remainingArgs.push arg) nameParts

def addSpecialization (s : Specialization) : M Unit :=
  modify fun state => { state with
    specializations := state.specializations.insertCore s.keys s
    specOrder := state.specOrder.push s.specializationName
  }

/-- Unfold a definition, handling projections specially. -/
private def unfoldDef (e : Expr) (declName : Name) : MetaM Expr := do
  let unfolded := (← unfold e declName).expr
  if ← isProjectionFn declName then
    unfolded.withApp fun fn args => do
      let fn := (← reduceProj? fn).getD fn
      return fn.beta args
  else
    return unfolded

def isInlineType (type : Expr) : MetaM Bool := do
  if type.isForall then
    forallTelescope type fun _ r => do
      if r.isSort then
        return false
      else if (← isClass? r).isSome then
        return false
      else
        return true
  else
    return false

def shouldInline (fname : Name) (args : Array Expr) : MetaM Bool := do
  if fname == ``Bind.bind then
    return false

  for arg in args do
    let t ← inferType arg
    if ← isInlineType t then
      trace[HouLean.specialize] "inling {fname} because argument {arg} is function type!"
      return true
  return false


def inline? (e : Expr) : MetaM (Option Expr) := do
  if let some fname := e.getAppFn.constName? then
    let args := e.getAppArgs
    if ← shouldInline fname args then
      let e' := (← unfold e fname).expr
      if e' != e then
        trace[HouLean.specialize] m!"inlined {fname}: {indentExpr e}\n  ==> {indentExpr e'}"
        return e'
  return none


/-- Define a new specialized function and register it. -/
partial def defineNewSpecialization (r : FunSpecializationResult) : M (Option Name) := do
  withTraceNode `HouLean.specialize (fun res => return m!"[{exceptEmoji res}] Specializing {r.fn'} : {← inferType r.fn'}") do
  forallTelescope (← inferType r.fn') fun xs _ => do
    let body := r.fn'.beta xs
    let body' ← unfoldDef body r.funName

    if body == body' then
      trace[HouLean.specialize] "cannot unfold {r.funName}, skipping specialization"
      return none

    let body'' ← specializeAndSimp body'
    let value ← mkLambdaFVars xs body'' >>= instantiateMVars
    let type ← inferType value
    let name ← Meta.mkUniqueDeclName r.mangledName

    let decl : Declaration := .defnDecl {
      name := name
      levelParams := []
      type := type
      value := value
      hints := .regular value.approxDepth
      safety := .safe
    }

    addAndCompile decl
    trace[HouLean.specialize] "compiled {name}"

    let (xs,_,_) ← forallMetaTelescope (← inferType r.fn')
    let keys ← DiscrTree.mkPath (r.fn'.beta xs)

    addSpecialization {
      keys := keys
      originalName := r.funName
      specializationName := name
      fn := r.fn'
    }
    return some name

/-- Check if specialization should be skipped for this expression. -/
private def skipSpecialization (e : Expr) : M Bool := do
  let type ← inferType e
  if (← isClass? type).isSome then return true
  if (← inferType type).isProp then return true
  if ← isMatcherApp e then return true
  if let some decl := e.getAppFn.constName? then
    if ← isRecursiveDefinition decl then
      return true
  return false


def specializeArgsOnly (funName : Name) : M Bool := do
  if [``ite, ``dite, ``forIn, ``LT.lt, ``LE.le, ``Bind.bind].contains funName then
    return true

  if let some info ← getProjectionFnInfo? funName then
    if ¬info.fromClass then
      return true

  if ← isMatcher funName then
    return true

  if let .defnInfo _ ← getConstInfo funName then
    return false

  return true

#check ConstantInfo

/-- Recursively specialize an expression. -/
partial def specializeExprImpl (e : Expr) : M Expr := do
  -- there is some zetaDelta reduction happening somewhere :( not sure where
  withConfig (fun cfg => {cfg with zeta := false, zetaDelta := false}) do
  let e ← instantiateMVars e

  if let some val ← runInterpreterForPrimitiveTypes? e then
    return toExpr val

  if ← skipSpecialization e then
    return e

  if let some e ← customSpec e then
    return e

  match e with
  | .app .. =>
    let (fn, args) := e.withApp (·, ·)
    match fn with
    | .const fname _ =>
      let r ← specializeFunApp fn args

      -- run specialization on the arguments
      let args' ← r.args'.mapM specializeExpr

      if ← specializeArgsOnly r.funName then
        return r.fn'.beta args'

      -- the current function call might not need specialization but functions inside might
      -- so we continue specializing!
      -- -- No specialization of the function needed
      -- if (← isDefEq fn r.fn') then
      --   return r.fn'.beta args'

      -- Check for existing specialization
      let specs := (← get).specializations
      let candidates ←
        forallTelescope (← inferType r.fn') fun xs _ =>
          specs.getMatch (r.fn'.beta xs)
      for c in candidates do
        if ← isDefEq r.fn' c.fn then
          trace[HouLean.specialize.detail] "reusing specialization {c.specializationName}"
          return ← mkAppOptM c.specializationName (args'.map some)

      -- Create new specialization
      if let some specName ← defineNewSpecialization r then
        return ← mkAppOptM specName (args'.map some)
      else
        return r.fn'.beta args'

    | .proj typeName idx struct =>
      if let some struct' ← Meta.reduceProj? fn then
        specializeExpr (mkAppN struct' args)
      else
        let args' ← args.mapM specializeExpr
        return mkAppN (.proj typeName idx struct) args'

    | .fvar .. =>
      let args' ← args.mapM specializeExpr
      return mkAppN fn args'

    | other =>
      throwError "specializeExpr: unhandled function kind {other.ctorName}, {e}"
  | _ => return e

initialize specializeExprRef.set specializeExprImpl


/-- Recursively specialize an expression. -/
partial def specializeApp (fn : Expr) (args : Array Expr) : M Expr := do
  -- there is some zetaDelta reduction happening somewhere :( not sure where
  withConfig (fun cfg => {cfg with zeta := false, zetaDelta := false}) do

  match fn with
  | .const fname _ =>
    if ← specializeArgsOnly fname then
      return fn.beta args

    let r ← specializeFunApp fn args

    -- the current function call might not need specialization but functions inside might
    -- so we continue specializing!
    -- -- No specialization of the function needed
    -- if (← isDefEq fn r.fn') then
    --   return r.fn'.beta args'

    -- Check for existing specialization
    let specs := (← get).specializations
    let candidates ←
      forallTelescope (← inferType r.fn') fun xs _ =>
        specs.getMatch (r.fn'.beta xs)
    for c in candidates do
      if ← isDefEq r.fn' c.fn then
        trace[HouLean.specialize.detail] "reusing specialization {c.specializationName}"
        return ← mkAppOptM c.specializationName (r.args'.map some)

    -- Create new specialization
    if let some specName ← defineNewSpecialization r then
      try
        return ← mkAppOptM specName (r.args'.map some)
      catch er =>
        throwError m!"failed applying {args} to {specName}"
    else
      return r.fn'.beta r.args'

  -- | .proj typeName idx struct =>
  --   if let some struct' ← Meta.reduceProj? fn then
  --     specializeExpr (mkAppN struct' args)
  --   else
  --     let args' ← args.mapM specializeExpr
  --     return mkAppN (.proj typeName idx struct) args'

  -- | .fvar .. =>
  --   let args' ← args.mapM specializeExpr
  --   return mkAppN fn args'

  | other =>
    throwError "specializeExpr: unhandled function kind {other.ctorName}, {fn} {args}"

/-- Main entry point: specialize and simplify an expression. -/
partial def specializeAndSimpImpl (e : Expr) : M Expr := do
  withIncRecDepth do
  withTraceNode `HouLean.specialize (fun r => return m!"[{exceptEmoji r}] {e}") do
  trace[HouLean.specialize.detail] "processing {e.ctorName}"

  let e ← instantiateMVars e

  if ← skipSpecialization e then
    return e

  if let some val ← runInterpreterForPrimitiveTypes? e then
    return toExpr val

  match e with
  | .letE n t v b nonDep =>
    let v' ← specializeAndSimp v

    -- Float out nested lets
    if v'.isLet then
      return ← letTelescope v' fun xs v'' => do
        let e' ← specializeAndSimp (.letE n t v'' b nonDep)
        mkLetFVars xs e'

    withLetDecl n t v' fun v'' => do
      let b' ← specializeAndSimp (b.instantiate1 v'')
      mkLetFVars #[v''] b'

  | .app .. =>
    let mut e := e
    e ← simplify e
    e ← liftLets e

    if e.isLet then
      return ← specializeAndSimp e

    if let some e' ← customSpec e then
      trace[HouLean.specialize] m!"custom specialization {e} ==> {e'}"
      return e'

    -- inline function?
    if let some e' ← inline? e then
      return ← specializeAndSimp e'

    match e with
    | .app .. =>
      let mut (fn, args) := e.withApp (·,·)
      let mut hasLet := false
      for arg in args, i in [0:args.size] do
        let arg' ← specializeAndSimp arg
        if arg'.isLet then
          hasLet := true
        args := args.set! i arg'

      if hasLet then
        return ← specializeAndSimp (fn.beta args)

      return ← specializeApp fn args

      -- match e.getAppFn' with
      -- | .const .. => specializeExpr e
      -- -- | .proj .. => reduceProj e
      -- | fn => throwError "specializeAndSimpImpl: unhandled app function {fn.ctorName}"
    | _ =>
      throwError "specializeAndSimpImpl: unhandled case {e.ctorName}"

  | _ => return e
where
  simplify (e : Expr) : M Expr := do
    withTraceNode `HouLean.specialize.simplify (fun _ => return m!"simplify") do
      let e' := (← Simp.simp e).expr
      if e != e' then
        trace[HouLean.specialize] "simplified {indentExpr e}\n  ==> {indentExpr e'}"
      return e'

  liftLets (e : Expr) : M Expr := do
    withTraceNode `HouLean.specialize.liftLets (fun _ => return m!"liftLets") do
      Meta.liftLets e

  reduceProj (e : Expr) : M Expr := do
    let fn := e.getAppFn'
    if let some fn' ← Meta.reduceProj? fn then
      trace[HouLean.specialize.detail] "reduced projection {fn} → {fn'}"
      specializeAndSimp (mkAppN fn' e.getAppArgs)
    else
      trace[HouLean.specialize.detail] "cannot reduce projection {fn}, returning as-is"
      return e

initialize specializeAndSimpRef.set specializeAndSimpImpl

end HouLean.Meta.SpecializeAndSimp
