import HouLean.OpenCL.Compiler.RewriteRules
import HouLean.OpenCL.Compiler.Grammar
import HouLean.OpenCL.Compiler.Types
import HouLean.Meta.RunInterpreter

namespace HouLean.OpenCL.Compiler

open Lean Meta Qq

/-! # Utility Functions -/

/-- Convert a Lean name to a valid OpenCL identifier by replacing dots with underscores. -/
def nameToString (n : Name) : String :=
  n.eraseMacroScopes.toString.replace "." "_"

/-! # Primitive Value Compilation -/

/-- Convert a primitive value to OpenCL syntax. -/
def _root_.HouLean.Meta.PrimitiveValue.toOpenCLSyntax (val : PrimitiveValue) : MetaM (TSyntax `clExpr) :=
  match val with
  | .bool b => if b then `(clExpr| true) else `(clExpr| false)
  | .int x | .nat x | .usize x
  | .int16 x | .uint16 x
  | .int32 x | .uint32 x
  | .int64 x | .uint64 x =>
    let lit := Syntax.mkNumLit (toString x)
    `(clExpr| $lit:num)
  | .float32 x =>
    let lit := Syntax.mkScientificLit (Float.toString' x.toFloat (precision := 7))
    `(clExpr| $lit:scientific)
  | .float64 x =>
    let lit := Syntax.mkScientificLit (Float.toString' x)
    `(clExpr| $lit:scientific)
  | .unit => `(clExpr| null)

/-! # Variable Management -/

/-- Introduce a fresh variable name and run computation with it bound. -/
def withFVar (x : Expr) (go : Ident → CompileM α) : CompileM α := do
  let baseName := nameToString (← x.fvarId!.getUserName)
  let ctx ← read

  let (name, usedNames) :=
    match ctx.usedNames[baseName]? with
    | some count => (s!"{baseName}{count + 1}", ctx.usedNames.insert baseName (count + 1))
    | none => (baseName, ctx.usedNames.insert baseName 0)

  let fvarMap := ctx.fvarMap.insert x name
  trace[HouLean.OpenCL.compiler] "Introduced variable: {name}"

  withReader (fun _ => { fvarMap := fvarMap, usedNames := usedNames }) (go (mkIdent (.mkSimple name)))

/-- Introduce multiple fresh variables with properly nested scopes. -/
def withFVars (xs : Array Expr) (go : Array Ident → CompileM α) : CompileM α :=
  loop xs.toList #[]
where
  loop : List Expr → Array Ident → CompileM α
    | [], ids => go ids
    | x :: xs, ids => withFVar x fun id => loop xs (ids.push id)

/-- Add a statement to the current block. -/
def addStatement (stmt : TSyntax `clStmtLike) : CompileM Unit :=
  modify fun s => { s with statements := s.statements.push stmt }

/-! # Application Classification -/

/--
Classification of application expressions for compilation.

- `bind`: Monadic bind operation `mx >>= f`
- `forLoop`: For-in loop with range `(start, stop, step, init, body)`
- `ite`: If-then-else conditional
- `app`: General function application
- `value`: Pure value (wrapped in `pure`/`yield`)
- `matchE`: Match expression
-/
inductive AppCase where
  | bind (mx f : Expr)
  | forLoop (start stop step init f : Expr)
  | ite (cond t e : Expr)
  | app
  | value (e : Expr)
  | matchE

/-- Eta-expand an expression by one argument. -/
def etaExpand1 (e : Expr) : MetaM Expr := do
  forallBoundedTelescope (← inferType e) (some 1) fun xs _ =>
    mkLambdaFVars xs (e.beta xs)

/-- Classify an application expression to determine compilation strategy. -/
def appCase (e : Expr) : MetaM AppCase := do
  if e.isAppOfArity ``bind 6 then
    return .bind e.appFn!.appArg! (← etaExpand1 e.appArg!)

  if e.isAppOfArity ``forIn 9 then
    let range := e.getRevArg! 2
    return .forLoop
      (← mkAppM ``Std.Range.start #[range])
      (← mkAppM ``Std.Range.stop #[range])
      (← mkAppM ``Std.Range.step #[range])
      (e.getRevArg! 1)  -- init
      (e.getRevArg! 0)  -- f

  if e.isAppOfArity ``ite 5 then
    return .ite (e.getRevArg! 3) (e.getRevArg! 1) (e.getRevArg! 0)

  if e.isAppOfArity ``pure 4 || e.isAppOfArity ``ForInStep.yield 2 then
    return .value e.appArg!

  if ← isMatcherApp e then
    return .matchE

  return .app

/-! # Expression Compilation -/

mutual
/-- Try to find and apply an `implemented_by` rule for the expression. -/
partial def tryImplementedBy (e : Expr) : CompileM (Option (TSyntax `clExpr)) := do
  let s := (compilerExt.getState (← getEnv)).implementedBy
  for c in ← s.getMatch e do
    let (args, _, _) ← forallMetaTelescope (← inferType c.lhs)
    let body := c.lhs.beta args
    if ← isDefEq body e then
      return some (← applyImpl c args e)
  return none
where
  applyImpl (impl : ImplementedBy) (args : Array Expr) (e : Expr) : CompileM (TSyntax `clExpr) := do
    trace[HouLean.OpenCL.compiler] "Applying implemented_by: {impl.lhs} ==> {impl.rhs}"

    let mut compiledArgs : NameMap (TSyntax `clExpr) := {}
    for (n, i) in impl.argsToCompile do
      let some arg := args[i]? | throwError "Invalid argument index {i} in implemented_by for {e}"
      compiledArgs := compiledArgs.insert n (← compileExpr arg)

    let compiled ← impl.rhs.raw.replaceM fun
      | `(clExpr| $id:ident) => return compiledArgs.get? id.getId
      | _ => return none

    return ⟨compiled⟩

/-- Try to find and run an `implemented_by` builder for the expression. -/
partial def tryImplementedByBuilder (e : Expr) : CompileM (Option (TSyntax `clExpr)) := do
  let s := (compilerExt.getState (← getEnv)).implementedByBuilders
  for b in ← s.getMatch e do
    let (args, _, _) ← forallMetaTelescope (← inferType b.lhs)
    let body := b.lhs.beta args
    if ← isDefEq body e then
      return some (← runBuilder args b)
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
partial def compileStructure (type : Expr) (structName : Name) (params : Array Expr) : CompileM Ident := do
  let ps ← params.mapM compileType
  let name := ps.foldl (init := nameToString structName)
    fun s p => s ++ "_" ++ (toString (Syntax.prettyPrint p)).replace " " ""

  let clId := mkIdent (.mkSimple name)
  let _ ← addImpementedBy type (← `(clExpr| $clId:ident))

  -- Generate projection implementations
  let info := getStructureInfo (← getEnv) structName
  withLocalDeclD `x type fun x => do
    for fieldName in info.fieldNames, i in [0:info.fieldNames.size] do
      let projExpr := x.proj structName i
      let lhs ← mkLambdaFVars #[x] projExpr
      let rhs ← `(clExpr| $(mkIdent `x):ident.$(mkIdent fieldName):ident)
      let _ ← addImpementedBy lhs rhs

  -- Generate constructor implementation
  let ctor := getStructureCtor (← getEnv) structName
  let mkLhs ← mkAppOptM ctor.name (params.map some)
  let mkRhs ← forallTelescope (← inferType mkLhs) fun xs _ => do
    let ids ← xs.mapM fun x => return mkIdent (← x.fvarId!.getUserName)
    `(clExpr| ($clId){$[$ids:ident],*})
  let _ ← addImpementedBy mkLhs mkRhs

  return clId

/-- Compile a Lean type to an OpenCL type identifier. -/
partial def compileType (e : Expr) : CompileM Ident := do
  try
    match ← compileExpr e with
    | `(clExpr| $id:ident) => return id
    | stx => throwError "Unexpected non-identifier when compiling type: {stx}"
  catch _ =>
    if let some fname := e.getAppFn.constName? then
      if isStructure (← getEnv) fname then
        return ← compileStructure e fname e.getAppArgs
    throwError "Don't know how to compile type: {e}"

end

/-! # Block Compilation -/

/-- Check if a let binding should be inlined (functions or Unit type). -/
def shouldInlineLet (type : Expr) : CompileM Bool := do
  if type.isForall then return true
  if ← isDefEq type q(Unit) then return true
  return false

/-- Default continuation: emit a return statement. -/
def finishBlockDefault (x : TSyntax `clExpr) : CompileM Unit := do
  addStatement (← `(clStmtLike| return $x;))

mutual

/-- Compile a block of statements with a continuation for the final value. -/
partial def compileBlock (e : Expr) (cont : TSyntax `clExpr → CompileM Unit := finishBlockDefault) : CompileM Unit := do
  match e with
  | .letE n t v b _ =>
    if ← shouldInlineLet t then
      return ← compileBlock (b.instantiate1 v) cont

    let t' ← compileType t
    let v' ← compileExpr v
    withLetDecl n t v fun var => do
      withFVar var fun varId => do
        let c ← `(clTypeQ| const)
        addStatement (← `(clStmtLike| $c:clTypeQ $t':ident $varId:ident = $v':clExpr;))
        compileBlock (b.instantiate1 var) cont

  | e =>
    match ← appCase e with
    | .bind mx f =>
      let .lam name t body _ := f | throwError "Invalid bind: expected lambda, got {f}"
      let mx' ← compileExpr mx
      if ← isDefEq t q(Unit) then
        addStatement (← `(clStmtLike| $mx':clExpr;))
        compileBlock (body.instantiate1 q(())) cont
      else
        let t' ← compileType t
        withLocalDeclD name t fun var => do
          withFVar var fun varId => do
            let c ← `(clTypeQ| const)
            addStatement (← `(clStmtLike| $c:clTypeQ $t':ident $varId:ident = $mx':clExpr;))
            compileBlock (body.instantiate1 var) cont

    | .forLoop .. => throwError "TODO: implement for loop compilation"
    | .ite cond t e =>
      let decCond ← mkAppOptM ``decide #[cond, none]
      let cond' ← compileExpr decCond
      let thnBody ← compileScope t cont
      let elseBody ← compileScope e cont
      addStatement (← `(clStmtLike| if ($cond') $thnBody else $elseBody))
    | .matchE => throwError "TODO: implement match compilation"
    | .app => cont (← compileExpr e)
    | .value e => cont (← compileExpr e)

/-- Compile a scoped block, saving and restoring statement state. -/
partial def compileScope (e : Expr) (cont : TSyntax `clExpr → CompileM Unit) : CompileM (TSyntax `clStmt) := do
  let saved ← get
  set { : State}

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

  let rhs ← forallTelescope info.type fun xs _ => do
    let names ← xs.filterMapM fun x => do
      let id := x.fvarId!
      if (← id.getBinderInfo).isExplicit then
        return some (← id.getUserName)
      else
        return none
    let args := names.map mkIdent
    `(clExpr| $(mkIdent clName):ident($[$args:ident],*))

  let lhs ← mkConstWithFreshMVarLevels leanName
  let _ ← addImpementedBy lhs rhs

/-- Compile a Lean declaration to an OpenCL function. -/
def compileDecl (declName : Name) : MetaM (TSyntax ``clFunction) := do
  let info ← getConstInfo declName
  let some val := info.value? | throwError "Cannot compile {declName}: not a definition"

  let funName := Name.mkSimple <|
    declName.eraseMacroScopes.toString.toLower.replace "." "_"
  let funId := mkIdent funName

  forallTelescope (← inferType val) fun xs _ => do
    let body := val.beta xs

    let go : CompileM (TSyntax ``clFunction) :=
      withFVars xs fun varIds => do
        let rt' ← compileType (← inferType body)
        let ts' ← xs.mapM (inferType · >>= compileType)

        compileBlock body

        let returnType : TSyntax `clDeclSpec ← `(clDeclSpec| $rt':ident)
        let argTypes ← ts'.mapM fun t => `(clTypeSpec| $t:ident)
        let stmts := (← get).statements

        `(clFunction| $returnType $funId:ident($[$argTypes:clTypeSpec $varIds:ident],*) { $stmts* })

    let (fundef, _) ← go {} {}
    addOpenCLFunDef declName funName fundef
    return fundef

end HouLean.OpenCL.Compiler
