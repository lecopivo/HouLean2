import HouLean.OpenCL.Compiler.RewriteRules
import HouLean.OpenCL.Compiler.Grammar
import HouLean.OpenCL.Compiler.Types
import HouLean.Meta.RunInterpreter

namespace HouLean.OpenCL.Compiler

open Lean Meta Qq

initialize registerTraceClass `HouLean.OpenCL.compiler

/-! # Utility Functions -/

/-- Convert a Lean name to a valid OpenCL identifier by replacing dots with underscores. -/
def nameToString (n : Name) : String :=
  n.eraseMacroScopes.toString.replace "." "_"

/-! # Primitive Value Compilation -/

/-- Convert a primitive value to OpenCL syntax. -/
def _root_.HouLean.Meta.PrimitiveValue.toOpenCLSyntax (val : PrimitiveValue) : MetaM (TSyntax `clExpr) :=
  match val with
  | .bool b => if b then `(clExpr| true) else `(clExpr| false)
  | .int x | .nat x | .usize x | .int16 x | .uint16 x
  | .int32 x | .uint32 x | .int64 x | .uint64 x =>
    `(clExpr| $(Syntax.mkNumLit (toString x)):num)
  | .float32 x => `(clExpr| $(Syntax.mkScientificLit (Float.toString' x.toFloat (precision := 7))):scientific)
  | .float64 x => `(clExpr| $(Syntax.mkScientificLit (Float.toString' x)):scientific)
  | .unit => `(clExpr| null)

/-! # Variable Management -/

/-- Introduce a fresh variable name and run computation with it bound. -/
def withFVar (x : Expr) (go : Ident → CompileM α) : CompileM α := do
  let baseName := nameToString (← x.fvarId!.getUserName)
  let ctx ← read
  let (name, usedNames) := match ctx.usedNames[baseName]? with
    | some count => (s!"{baseName}{count + 1}", ctx.usedNames.insert baseName (count + 1))
    | none => (baseName, ctx.usedNames.insert baseName 0)
  trace[HouLean.OpenCL.compiler] "Introduced variable: {name}"
  withReader (fun _ => { fvarMap := ctx.fvarMap.insert x name, usedNames }) do
    go (mkIdent (.mkSimple name))

/-- Introduce multiple fresh variables with properly nested scopes. -/
def withFVars (xs : Array Expr) (go : Array Ident → CompileM α) : CompileM α :=
  go' xs.toList #[]
where
  go' : List Expr → Array Ident → CompileM α
    | [], ids => go ids
    | x :: xs, ids => withFVar x fun id => go' xs (ids.push id)

/-- Add a statement to the current block. -/
def addStatement (stmt : TSyntax `clStmtLike) : CompileM Unit := do
  modify fun s => { s with statements := s.statements.push stmt }

/-! # Application Classification -/

/--
Classification of application expressions for compilation.
-/
inductive AppCase where
  | bind (mx f : Expr)
  | forLoop (start stop step init f rest : Expr)
  | ite (cond t e : Expr)
  | app
  | value (e : Expr)
  | matchE

/-- Eta-expand an expression by one argument. -/
def etaExpand1 (e : Expr) : MetaM Expr := do
  forallBoundedTelescope (← inferType e) (some 1) fun xs _ => mkLambdaFVars xs (e.beta xs)

/-- Classify an application expression to determine compilation strategy. -/
def appCase (e : Expr) : MetaM AppCase := do
  if e.isAppOfArity ``bind 6 then
    let mx := e.appFn!.appArg!
    let f ← etaExpand1 e.appArg!
    if mx.isAppOfArity ``forIn 9 then
      let range := mx.getRevArg! 2
      return .forLoop
        (← mkAppM ``Std.Range.start #[range] >>= whnfI)
        (← mkAppM ``Std.Range.stop #[range] >>= whnfI)
        (← mkAppM ``Std.Range.step #[range] >>= whnfI)
        (mx.getRevArg! 1)
        (mx.getRevArg! 0)
        f
    return .bind mx f
  if e.isAppOfArity ``ite 5 then
    return .ite (e.getRevArg! 3) (e.getRevArg! 1) (e.getRevArg! 0)
  if e.isAppOfArity ``pure 4 || e.isAppOfArity ``ForInStep.yield 2 then
    return .value e.appArg!
  if ← isMatcherApp e then
    return .matchE
  return .app

/-! # Bind Classification -/

inductive BindCase where
  | normal
  | statement
  | remove

def bindCase (val : Expr) : MetaM BindCase := do
  let type ← inferType val
  let m ← mkFreshExprMVarQ q(Type → Type)
  if ← isDefEq type q($m Unit) then
    let _ ← mkFreshExprMVarQ q(Monad $m)
    return if ← isDefEq val q(pure (f := $m) ()) then .remove else .statement
  else
    return .normal

/-! # Expression Compilation -/

mutual

/-- Try to find and apply an `implemented_by` rule for the expression. -/
partial def tryImplementedBy (e : Expr) : CompileM (Option (TSyntax `clExpr)) := do
  let rules := (compilerExt.getState (← getEnv)).implementedBy
  for rule in ← rules.getMatch e do
    let (args, _, _) ← forallMetaTelescope (← inferType rule.lhs)
    if ← isDefEq (rule.lhs.beta args) e then
      return some (← applyRule rule args e)
  return none
where
  applyRule (impl : ImplementedBy) (args : Array Expr) (e : Expr) : CompileM (TSyntax `clExpr) := do
    let mut compiledArgs : Array ((TSyntax `clExpr)×(TSyntax `clExpr)) := {}
    for (n, i) in impl.argsToCompile do
      let some arg := args[i]? | throwError "Invalid argument index {i} in implemented_by for {e}"
      let arg' ← compileExpr arg
      compiledArgs := compiledArgs.push (n,arg')
    let compiled ← impl.rhs.raw.replaceM fun s => do
      let some (_,r) := compiledArgs.find? (·.1 == s)
        | return none
      return r
    trace[HouLean.OpenCL.compiler] "Applying implemented_by: {e} ==> {compiled}\n  args: {compiledArgs}\n{impl.rhs} ==> {compiled}"
    return ⟨compiled⟩

/-- Try to find and run an `implemented_by` builder for the expression. -/
partial def tryImplementedByBuilder (e : Expr) : CompileM (Option (TSyntax `clExpr)) := do
  let builders := (compilerExt.getState (← getEnv)).implementedByBuilders
  for builder in ← builders.getMatch e do
    if builder.typeBuilder then continue
    let (args, _, _) ← forallMetaTelescope (← inferType builder.lhs)
    if ← isDefEq (builder.lhs.beta args) e then
      return some (← runBuilder args builder)
  return none

/-- Try to find and run an `implemented_by` builder for the expression. -/
partial def tryTypeImplementedByBuilder (e : Expr) : CompileM (Option OpenCLTypeSyntax) := do
  let builders := (compilerExt.getState (← getEnv)).implementedByBuilders
  for builder in ← builders.getMatch e do
    unless builder.typeBuilder do continue
    let (args, _, _) ← forallMetaTelescope (← inferType builder.lhs)
    if ← isDefEq (builder.lhs.beta args) e then
      return some (← runTypeBuilder args builder)
  return none

/-- Compile a Lean expression to OpenCL syntax. -/
partial def compileExpr (e : Expr) : CompileM (TSyntax `clExpr) := do
  withConfig (fun cfg => { cfg with zeta := false, zetaDelta := false }) do
  withTraceNode `HouLean.OpenCL.compiler (fun r => return m!"[{exceptEmoji r}] {e}") do
    -- Try interpreting as primitive
    if let some val ← runInterpreterForPrimitiveTypes? e then
      return ← val.toOpenCLSyntax
    let e ← instantiateMVars e
    let e := e.headBeta
    match e with
    | .fvar .. =>
      let some id := (← read).fvarMap[e]? | throwError "Unrecognized free variable: {e}"
      `(clExpr| $(mkIdent (.mkSimple id)):ident)
    | _ =>
      -- Try implemented_by rules
      if let some result ← tryImplementedBy e then return result
      if let some result ← tryImplementedByBuilder e then return result
      -- Handle special cases
      if let some fname := e.getAppFn.constName? then
        if ← isProjectionFn fname then
          throwError "Projection encountered; should compile structure: {e}"
        if ← isConstructorApp e then
          throwError "Constructor encountered; should compile structure/enum: {e}"
        if ← isMatcher fname then
          return ← compileExpr (← unfold e fname).expr
        if isCasesOnRecursor (← getEnv) fname then
          throwError "Cases-on recursor not supported: {e}"
      throwError "Don't know how to compile: {e}"

end

/-! # Type Compilation -/

mutual

/-- Compile a structure type with concrete parameters. -/
partial def compileStructure (type : Expr) (structName : Name) (params : Array Expr) : CompileM OpenCLTypeSyntax := do
  let ps ← params.mapM compileType
  let name := ps.foldl (init := nameToString structName)
    fun s p => s ++ "_" ++ (toString p.name).replace " " ""
  let clId := mkIdent (.mkSimple name)
  addOpenCLType type name none -- todo: add definition
  -- Generate projection implementations
  let info := getStructureInfo (← getEnv) structName
  withLocalDeclD `x type fun x => do
    for fieldName in info.fieldNames, i in [0:info.fieldNames.size] do
      let projExpr := x.proj structName i
      let lhs ← mkLambdaFVars #[x] projExpr
      let e ← `(clExpr| $(mkIdent `x):ident)
      let rhs ← `(clExpr| $e.$(mkIdent fieldName):ident)
      let _ ← addImplementedBy lhs rhs #[(e, 0)]
  -- Generate constructor implementation
  let ctor := getStructureCtor (← getEnv) structName
  let mkLhs ← mkAppOptM ctor.name (params.map some)
  let argNames ← forallTelescope (← inferType mkLhs) fun xs _ =>
    xs.mapM fun x => x.fvarId!.getUserName
  let args ← argNames.mapM (fun n => `(clExpr| $(mkIdent n):ident))
  let mkRhs ← `(clExpr| ($clId){$[$args:clExpr],*})
  let _ ← addImplementedBy mkLhs mkRhs (args.zip (.range args.size))
  return {
    quals := #[]
    name := .mkSimple name
    pointer := false
  }

/-- Compile a Lean type to an OpenCL type identifier. -/
partial def compileType (e : Expr) : CompileM OpenCLTypeSyntax := do
  let e ← instantiateMVars e
  let e ← liftM <| e.withApp fun fn args => do pure ((← whnfR fn).beta (← args.mapM whnfR))
  let s := (compilerExt.getState (← getEnv)).clTypes
  if let some t := s[e]? then
    return t.clType
  if let some t ← tryTypeImplementedByBuilder e then
    return t
  if let some fname := e.getAppFn.constName? then
    if isStructure (← getEnv) fname then
      return ← compileStructure e fname e.getAppArgs
  throwError "Don't know how to compile type: {e}"

end

/-! # Block Compilation -/

/-- Check if a let binding should be inlined (functions or Unit type). -/
def shouldInlineLet (type : Expr) : CompileM Bool := do
  if type.isForall then return true
  isDefEq type q(Unit)

/-- Default continuation: emit a return statement. -/
def finishBlockDefault (x : TSyntax `clExpr) : CompileM Unit := do
  let x := ⟨← PrettyPrinter.parenthesizeTerm x⟩
  let stmt ← `(clStmtLike| return $x:clExpr;)
  addStatement stmt

mutual

/-- Compile a block of statements with a continuation for the final value. -/
partial def compileBlock (e : Expr) (cont : TSyntax `clExpr → CompileM Unit := finishBlockDefault) : CompileM Unit := do
  match e with
  | .letE n t v b _ =>
    if ← shouldInlineLet t then
      compileBlock (b.instantiate1 v) cont
    else
      let t' ← compileType t
      let v' ← compileExpr v
      withLetDecl n t v fun var => withFVar var fun varId => do
        addStatement (← t'.mkDeclaration (const:=true) varId v')
        compileBlock (b.instantiate1 var) cont
  | _ => compileAppCase e cont

/-- Compile based on application case classification. -/
partial def compileAppCase (e : Expr) (cont : TSyntax `clExpr → CompileM Unit) : CompileM Unit := do
  match ← appCase e with
  | .bind mx f => compileBind mx f cont
  | .forLoop start stop step init f rest => compileForLoop start stop step init f rest cont
  | .ite cond t e => compileIte cond t e cont
  | .matchE => throwError "TODO: implement match compilation:\n{e}"
  | .app => cont (← compileExpr e)
  | .value e => cont (← compileExpr e)

/-- Compile a monadic bind. -/
partial def compileBind (mx f : Expr) (cont : TSyntax `clExpr → CompileM Unit) : CompileM Unit := do
  let .lam name t body _ := f | throwError "Invalid bind: expected lambda, got {f}"
  match ← bindCase mx with
  | .remove => compileBlock (body.instantiate1 q(())) cont
  | .statement =>
    addStatement (← `(clStmtLike| $(← compileExpr mx):clExpr;))
    compileBlock (body.instantiate1 q(())) cont
  | .normal =>
    let t' ← compileType t
    let mx' ← compileExpr mx
    withLocalDeclD name t fun var => withFVar var fun varId => do
      addStatement (← t'.mkDeclaration (const:=true) varId mx')
      compileBlock (body.instantiate1 var) cont

/-- Compile a for loop. -/
partial def compileForLoop (start stop step init f rest : Expr)
    (cont : TSyntax `clExpr → CompileM Unit) : CompileM Unit := do
  let initType ← inferType init
  let initType' ← compileType initType
  let init' ← compileExpr init
  withLocalDeclD `state initType fun state => withFVar state fun stateId => do
    addStatement (← initType'.mkDeclaration stateId init' (const:=false))
    let _ ← forallBoundedTelescope (← inferType f) (some 1) fun xs _ => do
      let idx := xs[0]!
      withFVar idx fun idxId => do
        let body := f.beta #[idx, state]
        let body' ← compileScope body (fun r => do
          addStatement (← `(clStmtLike| $stateId:ident = $r:clExpr;)))
        let start' ← compileExpr start
        let stop' ← compileExpr stop
        let step' ← compileExpr step
        addStatement (← `(clStmtLike|
          for (uint $idxId:ident = $start':clExpr; $idxId:ident < $stop'; $idxId:ident += $step') $body'))
    compileBlock (rest.beta #[state]) cont

/-- Compile an if-then-else. -/
partial def compileIte (cond t e : Expr) (cont : TSyntax `clExpr → CompileM Unit) : CompileM Unit := do
  let decCond ← mkAppOptM ``decide #[cond, none]
  let cond' ← compileExpr decCond
  let thnBody ← compileScope t cont
  let elsBody ← compileScope e cont
  addStatement (← `(clStmtLike| if ($cond') $thnBody else $elsBody))

/-- Compile a scoped block, saving and restoring statement state. -/
partial def compileScope (e : Expr) (cont : TSyntax `clExpr → CompileM Unit) : CompileM (TSyntax `clStmt) := do
  let saved ← get
  set { : State }
  compileBlock e cont
  let stmts := (← get).statements
  set saved
  `(clStmt| {$stmts*})

end

/-! # Declaration Compilation -/

/-- Register an OpenCL function definition and its implementation rule. -/
def addOpenCLFunDef (leanName : Name) (clName : Name) (funDef : TSyntax ``clFunction) : MetaM Unit := do
  let info ← getConstInfo leanName
  compilerExt.add (.clFunDef { funDef, clName, leanName })
  let namesAndBinders ← forallTelescope info.type fun xs _ =>
    xs.mapM fun x => do return (← x.fvarId!.getUserName, ← x.fvarId!.getBinderInfo)
  let mut argsToCompile : Array (TSyntax `clExpr × Nat) := #[]
  for (n, bi) in namesAndBinders, i in [0:namesAndBinders.size] do
    if bi.isExplicit then
      let mut e ← `(clExpr| $(mkIdent n):ident)
      e ← if argsToCompile.any (·.1 == e)
        then `(clExpr| $(mkIdent (n.appendAfter (toString i))):ident)
        else `(clExpr| $(mkIdent n):ident)
      argsToCompile := argsToCompile.push (e, i)
  let args := argsToCompile.map (·.1)
  let rhs ← `(clExpr| $(mkIdent clName):ident($[$args:clExpr],*))
  let lhs ← mkConstWithFreshMVarLevels leanName
  let _ ← addImplementedBy lhs rhs argsToCompile

/-- Compile a Lean declaration to an OpenCL function. -/
def compileDecl (declName : Name) : MetaM (TSyntax ``clFunction) := do
  let info ← getConstInfo declName
  let some val := info.value? | throwError "Cannot compile {declName}: not a definition"
  let funName := Name.mkSimple <|
    declName.eraseMacroScopes.toString.toLower.replace "." "_"
  let funId := mkIdent funName
  forallTelescope (← inferType val) fun xs _ => do
    let body := val.beta xs
    let go : CompileM (TSyntax ``clFunction) := withFVars xs fun varIds => do
      let ts' ← xs.mapM (inferType · >>= compileType)
      compileBlock body
      let stmts := (← get).statements
      let args ← (ts'.zip varIds).mapM fun (t,v) => t.mkParamDecl v
      let rt' ← compileType (← inferType body)
      rt'.mkFunction funId args stmts
    let (fundef, _) ← go {} {}
    addOpenCLFunDef declName funName fundef
    return fundef

end HouLean.OpenCL.Compiler
