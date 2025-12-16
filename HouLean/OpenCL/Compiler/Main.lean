import HouLean.OpenCL.Compiler.RewriteRules
import HouLean.OpenCL.Compiler.Grammar
import HouLean.OpenCL.Compiler.Types
import HouLean.Meta.RunInterpreter

namespace HouLean.OpenCL.Compiler

open Lean Meta

def nameToString (n : Name) : String := n.eraseMacroScopes.toString.replace "." "_"


def _root_.HouLean.Meta.PrimitiveValue.toOpenCLSyntax (val : PrimitiveValue) : MetaM (TSyntax `clExpr) :=
  match val with
  | .bool x => if x then `(clExpr| true) else `(clExpr| false)
  | .int x
  | .nat x
  | .usize x
  | .int16 x
  | .uint16 x
  | .int32 x
  | .uint32 x
  | .int64 x
  | .uint64 x =>
    let x := Syntax.mkNumLit (toString x)
    `(clExpr| $x:num)
  | .float32 x =>
    let x := Syntax.mkScientificLit (Float.toString' x.toFloat (precision:=7))
    `(clExpr| $x:scientific)
  | .float64 x =>
    let x := Syntax.mkScientificLit (Float.toString' x)
    `(clExpr| $x:scientific)
  | .unit => throwError m!"Trying to emit value for `void`!"


partial def compileExpr (e : Expr) : CompileM (TSyntax `clExpr) := do
  -- there is something doing `zetaDelta` reduction for some reason
  -- maybe unification?
  withConfig (fun cfg => {cfg with zeta  := false, zetaDelta := false}) do
  withTraceNode `HouLean.OpenCL.compiler (fun r => return m!"[{exceptEmoji r}] {e}") do

  if let some val ← runInterpreterForPrimitiveTypes? e then
    return ← val.toOpenCLSyntax

  let e ← instantiateMVars e
  match e with
  | .fvar id =>
    let name ← id.getUserName
    let id := mkIdent name
    `(clExpr| $id:ident)
  | _ =>
   -- try existing implemented_by
   let s := (compilerExt.getState (← getEnv)).implementedBy
   let candidates ← s.getMatch e
   for c in candidates do

     let (args,_,_) ← forallMetaTelescope (← inferType c.lhs)
     let body := c.lhs.beta args

     unless ← isDefEq body e do
       continue

     -- todo: check that all arguments has been synthesized! most importantly all typeclases has been!

     return ← applyImpl c args

   -- try implemented_by builders
   let s := (compilerExt.getState (← getEnv)).implementedByBuilders
   let candidates ← s.getMatch e
   for b in candidates do

     let (args,_,_) ← forallMetaTelescope (← inferType b.lhs)
     let body := b.lhs.beta args

     unless ← isDefEq body e do
       continue
     -- todo: check that all arguments has been synthesized! most importantly all typeclases has been!

     return ← runBuilder args b

   if let some fname := e.getAppFn.constName? then
     if ← isProjectionFn fname then
       throwError m!"projection encountered, should compile a structure!"
     if ← isConstructorApp e then
       throwError m!"constructor encountered, should compile a structure/enum!"
     if ← isConstructorApp e then
       throwError m!"constructor encountered, should compile a structure/enum!"
     if ← isMatcher fname then
       let e := (← unfold e fname).expr
       return ← compileExpr e
       -- throwError m!"match encountered"
     if isCasesOnRecursor (← getEnv) fname then
       throwError m!"cases on recursor, {e}"

   throwError "Don't know how to compile {e}!"

where
  applyImpl (impl : ImplementedBy) (args : Array Expr) : CompileM (TSyntax `clExpr) := do

     trace[HouLean.OpenCL.compiler] m!"Applying implemented_by, {impl.lhs} ==> {impl.rhs}"

     -- compile all arguments
     let mut compiledArgs : NameMap (TSyntax `clExpr) := {}
     for (n, i) in impl.argsToCompile do
       let some arg := args[i]?  | throwError m!"Can't apply {impl.lhs} to {e}. Invalid argument index {i}!"

       let arg ← compileExpr arg
       compiledArgs := compiledArgs.insert n arg

     -- replace compiled arguments in rhs
     let compiled ← impl.rhs.raw.replaceM fun s =>
       match s with
       | `(clExpr| $id:ident) =>
         return compiledArgs.get? id.getId
       | _ =>
         return none

     return ⟨compiled⟩

mutual

/-- Compiles structure with concrete parameters -/
partial def compileStructure (type : Expr) (structName : Name) (params : Array Expr) : CompileM Ident := do

  let ps ← params.mapM compileType
  let name := ps.foldl (init:=nameToString structName)
    (fun s p => s ++ "_" ++ (toString (Syntax.prettyPrint p)).replace " " "")

  let clId := mkIdent (.mkSimple name)
  let _ ← addImpementedBy type (← `(clExpr| $clId:ident))

  -- Projections
  let info := getStructureInfo (← getEnv) structName
  withLocalDeclD `x type fun x => do
  for fieldName in info.fieldNames, i in [0:info.fieldNames.size] do

    let projExpr := x.proj structName i
    let lhs ← mkLambdaFVars #[x] projExpr
    let xId := mkIdent `x
    let fieldId := mkIdent fieldName
    let rhs ← `(clExpr| $xId:ident.$fieldId:ident)
    let _ ← addImpementedBy lhs rhs

  -- Constructor
  let ctor := getStructureCtor (← getEnv) structName
  let mkLhs ← mkAppOptM ctor.name (params.map some)
  let mkRhs ←
    forallTelescope (← inferType mkLhs) fun xs _ => do
      let ns ← xs.mapM (fun x => x.fvarId!.getUserName)
      let ids := ns.map mkIdent
      let stx : TSyntax `clExpr ← `(clExpr| ($clId){$[$ids:ident],*})
      pure stx
  let _ ← addImpementedBy mkLhs mkRhs

  return clId


partial def compileType (e : Expr) : CompileM Ident := do

  try
    match ← compileExpr e with
    | `(clExpr| $id:ident) => return id
    | stx =>
      throwError m!"Unexpected result when compiling type {stx}!"
  catch _ =>
    -- let e : Expr ← liftM <| e.withApp (fun fn args => do pure ((← whnfR fn).beta (← args.mapM whnfR)))
    if let some fname := e.getAppFn.constName? then
      if isStructure (← getEnv) fname then
        return ← compileStructure e fname e.getAppArgs
    throwError m!"Don't know how to compile type {e}!"
end


def withFVar (x : Expr) (go : Ident → CompileM α) : CompileM α := do
  fun ctx => do

    let mut fvarMap  := ctx.fvarMap
    let mut usedNames := ctx.usedNames

    let mut name ← nameToString <$> x.fvarId!.getUserName
    if let some count := ctx.usedNames[name]? then
      usedNames := usedNames.insert name (count + 1)
      name := s!"{name}{count + 1}"
    else
      usedNames := usedNames.insert name 0

    fvarMap := fvarMap.insert x name

    trace[HouLean.OpenCL.compiler] "Introduced variables: {name}"
    go (mkIdent (.mkSimple name)) { fvarMap, usedNames }

def withFVars (x : Array Expr) (go : Array Ident → CompileM α) : CompileM α :=
  go' x.toList #[]
where
  go' (xs : List Expr) (ids : Array Ident) : CompileM α := do
    match xs with
    | [] => go ids
    | x :: xs =>
      withFVar x (fun id => go' xs (ids.push id))

def addStatement (stmt : TSyntax `clStmtLike) : CompileM Unit := do
  modify (fun s => {s with statements := s.statements.push stmt})




open Qq in
partial def compileBlock (e : Expr) : CompileM Unit := do
  match e with
  | .letE n t v b _ => do
    let t' ← compileType t
    let v' ← compileExpr v
    withLetDecl n t v fun var => do
    withFVar var fun varId => do

      if ← isDefEq t q(Unit) then
        addStatement (← `(clStmtLike| $v':clExpr;))
      else
        addStatement (← `(clStmtLike| $t':ident $varId:ident = $v':clExpr;))

      compileBlock (b.instantiate1 var)

  -- todo: handle, bind, for loop, if statement, pure/return
  | e =>
    let e' ← compileExpr e
    addStatement (← `(clStmtLike| return $e';))


def addOpenCLFunDef (leanName : Name) (clName : Name) (funDef : TSyntax ``clFunction) : MetaM Unit := do

  let info ← getConstInfo leanName

  let clType : OpenCLFunction := {
    funDef := funDef
    clName := clName
    leanName := leanName
  }

  compilerExt.add (.clFunDef clType)

  let rhs ← forallTelescope (info.type) fun xs _ => do
    let names ← xs.filterMapM (fun x => do
      let id := x.fvarId!
      let bi ← id.getBinderInfo
      let name ← id.getUserName
      if bi.isExplicit then pure (some name) else pure none)
    let funId := mkIdent clName
    let args := names.map mkIdent
    let stx ← `(clExpr| $funId:ident($[$args:ident],*))
    pure stx

  let lhs ← mkConstWithFreshMVarLevels leanName
  let _ ← addImpementedBy lhs rhs

def compileDecl (declName : Name) : MetaM (TSyntax ``clFunction) := do

  let info ← getConstInfo declName
  let some val := info.value? | throwError "Can't compile {declName}, not a definition!"

  let funName := Name.mkSimple <| declName.eraseMacroScopes |>.toString |>.toLower |>.replace "." "_"
  let funId := mkIdent funName

  forallTelescope (← inferType val) fun xs _ => do
    let body := val.beta xs

    let go : CompileM (TSyntax ``clFunction) :=
      withFVars xs fun varIds => do
        let rt ← inferType body
        let rt' ← compileType rt
        let ts ← liftM <| xs.mapM inferType
        let ts' ← liftM <| ts.mapM compileType

        compileBlock body

        let returnType : TSyntax `clDeclSpec ← `(clDeclSpec| $rt':ident)
        let argTypes : Array (TSyntax `clTypeSpec) ← ts'.mapM (fun t => `(clTypeSpec| $t:ident))
        let stmts := (← get).statements

        let fundef ← `(clFunction| $returnType $funId:ident($[$argTypes:clTypeSpec $varIds:ident],*) { $stmts* })
        pure fundef

    let (fundef, _) ← go {} {}

    addOpenCLFunDef declName funName fundef

    return fundef
