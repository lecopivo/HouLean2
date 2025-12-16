import HouLean.OpenCL.Compiler.Grammar2
import HouLean.Meta.RunInterpreter

open HouLean.Meta

namespace HouLean.OpenCL.Compiler3

open Lean Meta Math

structure ImplementedBy where
  keys : Array DiscrTree.Key
  argsToCompile : Array (Name × Nat)
  lhs : Expr
  rhs : TSyntax `clExpr
deriving Inhabited, BEq

structure ImplementedByBuilder where
  keys : Array DiscrTree.Key
  lhs : Expr
  declName : Name
  arity : Nat
deriving Inhabited, BEq

structure OpenCLFunction where
  funDef : TSyntax ``clFunction
  clName : Name
  leanName : Name

structure OpenCLType where
  typeDef? : Option (TSyntax `clTypeSpec)
  clType : Name
  leanType : Expr

inductive SingleExtension where
  | implementedBy (impl : ImplementedBy)
  | implementedByBuilder (b : ImplementedByBuilder)
  | clFunDef (val : OpenCLFunction)
  | clTypeDef (val : OpenCLType)
deriving Inhabited

/-- Enviroment extension that holds all necessary information for the APEX compiler. -/
structure Extension where
  implementedBy : DiscrTree ImplementedBy
  implementedByBuilders : DiscrTree ImplementedByBuilder
  clFunctions : NameMap OpenCLFunction
  clTypes : ExprMap OpenCLType
deriving Inhabited


abbrev CompilerExt := SimpleScopedEnvExtension SingleExtension Extension

initialize compilerExt : CompilerExt ←
  registerSimpleScopedEnvExtension {
    name := by exact decl_name%
    initial := default
    addEntry := fun es e =>
      match e with
      | .implementedBy impl =>
        {es with implementedBy := es.implementedBy.insertCore impl.keys impl}
      | .implementedByBuilder b =>
        {es with implementedByBuilders := es.implementedByBuilders.insertCore b.keys b}
      | .clFunDef x =>
        {es with clFunctions := es.clFunctions.insert x.leanName x}
      | .clTypeDef x =>
        {es with clTypes := es.clTypes.insert x.leanType x}
  }


open Lean Meta
def addImpementedBy (lhs : Expr) (rhs : TSyntax `clExpr) : MetaM ImplementedBy := do

  let lhs ← instantiateMVars lhs
  let type ← inferType lhs

  if lhs.hasMVar then
    let mvars ← lhs.getMVars
    throwError m!"Can't add implemented_by `{lhs} ==> {rhs}`. Lhs contains mvars: {mvars}"

  if lhs.hasFVar then
    let fvars ← lhs.getFVars
    throwError m!"Can't add implemented_by `{lhs} ==> {rhs}`. Lhs contains mvars: {fvars}"

  -- figure out which arguments of lhs appear on the rhs
  -- store their name and index
  let argsToCompile ←
    forallTelescope type fun args _ => do
      args.zip (.range args.size)
        |>.filterMapM (fun (arg,i) => do
          if arg.isFVar then
            let name ← arg.fvarId!.getUserName
            -- arguments that appear on the rhs should be compiled!
            if rhs.raw.hasIdent name then
              return some (name, i)
          return none)


  let (xs,_,_) ← forallMetaTelescope (← inferType lhs)
  let body := lhs.beta xs
  let keys ← DiscrTree.mkPath body

  let impl : ImplementedBy := {
    keys := keys
    argsToCompile := argsToCompile
    lhs := lhs
    rhs := rhs
  }

  compilerExt.add (.implementedBy impl)

  return impl

open Qq in
unsafe def getBuilder (declName : Name) : MetaM (Array Expr → MetaM (TSyntax `clExpr)) := do
  let env ← getEnv
  let opts ← getOptions
  match env.find? declName with
  | none      => throwError m!"Unknown constant `{declName}`"
  | some info =>
    if ← isDefEq q(Array Expr → MetaM (TSyntax `clExpr)) info.type then
      return (← IO.ofExcept <| env.evalConst (Array Expr → MetaM (TSyntax `clExpr)) opts declName)
    else
      throwError m!"ImplementedByBuilder `{privateToUserName declName}` has an unexpected type: Expected `ImplementedByBuilder`, but found `{info.type}`"


def runBuilder (xs : Array Expr) (builder : ImplementedByBuilder) : MetaM (TSyntax `clExpr) := do
  let b ← unsafe getBuilder builder.declName
  return ← b xs


-- def runInterpreter? (α : Type) [t : ToExpr α] (val : Expr) : MetaM (Option α) := do
--   try
--     let val ← unsafe evalExpr α t.toTypeExpr val
--     return some val
--   catch _ =>
--     return none

open Lean Elab Term Command
elab "impl_by" bs:bracketedBinder* " : " lhs:term  " ==> " rhs:clExpr : command => do

  runTermElabM fun ctx => do
    elabBinders bs fun xs => do
      let e ← elabTerm lhs none
      -- let args := e.getAppArgs

      let e ← mkLambdaFVars xs e >>= instantiateMVars
      let ctx' := ctx.filter (fun c => e.containsFVar c.fvarId!)
      let e ← mkLambdaFVars ctx' e >>= instantiateMVars

      if e.hasMVar || e.hasFVar then
        let fvars := (← e.collectFVars.run {}).2.fvarIds.map Expr.fvar
        let mvars := (e.collectMVars {}).result.map (Expr.mvar)
        throwError s!"Bug in {decl_name%}, failed to build lhs expression! fvars: {fvars}, mvars: {mvars}, {e} : {← inferType e}"

      let _ ← addImpementedBy e rhs


/-- Turns function of type `argType → ... → argType → α` to `Array argType → α`. -/
def arrayUncurry (argType : Expr) (f : Expr) : MetaM Expr := do

  forallTelescope (← inferType f) fun xs _ => do
    let ts ← xs.mapM inferType
    unless ← ts.allM (isDefEq · argType) do
      throwError m!"Expecting function with arugments of type {argType}, got argument types {ts}!"

    withLocalDeclD `x (← mkAppM ``Array #[argType]) fun a => do

      let mut xs' : Array Expr := #[]
      for i in [0:xs.size] do
        let i := mkNatLit i
        xs' := xs'.push (← mkAppM ``getElem! #[a,i])

      mkLambdaFVars #[a] (f.beta xs')



open Lean Elab Term Command Qq
elab "impl_by" bs:bracketedBinder* " : " lhs:term  " ==> " "do" rhs:doElem* : command => do

  runTermElabM fun ctx => do
    elabBinders bs fun xs => do
      let e ← elabTerm lhs none

      let e ← mkLambdaFVars xs e >>= instantiateMVars
      let ctx' := ctx.filter (fun c => e.containsFVar c.fvarId!)
      let e ← mkLambdaFVars ctx' e >>= instantiateMVars
      let lhs ← instantiateMVars e

      -- Declare implemented_by builder
      let decls ← (ctx'++xs).mapM (fun x => do
        return (← x.fvarId!.getUserName, .default, fun _ => pure q(Expr)))
      let builder ← liftM <|
        withLocalDecls decls fun ys => do
          let rhs ← elabTermAndSynthesize (← `(term| do $[$rhs:doElem]*)) q(MetaM (TSyntax `clExpr))
          mkLambdaFVars ys rhs
      let builder ← arrayUncurry q(Expr) builder >>= instantiateMVars
      let builderType ← inferType builder >>= instantiateMVars
      let builderDeclName ← mkAuxDeclName `implStxBuilder


      let decl : Declaration := .defnDecl {
        name := builderDeclName
        levelParams := []
        type := builderType
        value := builder
        hints := .regular builder.approxDepth
        safety := .safe
      }

      addAndCompile decl

      -- Register implemented_by builder
      let (_,_,body) ← lambdaMetaTelescope e
      let keys ← DiscrTree.mkPath body

      let b : ImplementedByBuilder := {
        keys := keys
        lhs := lhs
        declName := builderDeclName
        arity := decls.size
      }

      if lhs.hasMVar then
        let mvars ← lhs.getMVars
        let ts ← liftM <| mvars.mapM inferType
        throwError m!"Can't add implemented_by builder `{lhs} ==> {rhs}`. Lhs contains mvars: {mvars} : {ts}"

      if lhs.hasFVar then
        let fvars ← lhs.getFVars

        throwError m!"Can't add implemented_by builder `{lhs} ==> {rhs}`. Lhs contains mvars: {fvars}"

      compilerExt.add (.implementedByBuilder b)
