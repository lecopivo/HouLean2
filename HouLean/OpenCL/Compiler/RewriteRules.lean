import HouLean.OpenCL.Compiler.Grammar
import HouLean.Meta.RunInterpreter

open HouLean.Meta

namespace HouLean.OpenCL.Compiler3

/-- Gadget identity function that will stop `simp` from simplifying an expression.

This is useful when the lhs of simp theorem appears on the rhs. You can wrap the occurence
in `no_simp` an prevent simp from an infinite loop.

The main use is for `simp` based compiler. For example for compiling to C we might define this
function, which indicates that `spec` should be replaced with C function with the name `cfun`
```
def cFunction (spec : α) (cfun : String) : α := spec
```
Then we add the following simp theorem
```
theorem compile_sin : Float.sin = cFunction (no_simp Float.sin) "sin" := rfl
```
where we wrapped `Float.sin` in `no_simp` to preven this theorem to be applied again on the `spec`
argument of `cFunction`. -/
def no_simp {α : Sort u} (a : α) := a

simproc_decl no_simp_simproc (no_simp _) := fun e =>
  return .done { expr := e }

open Lean Meta Math

structure ImplementedBy where
  keys : Array DiscrTree.Key
  argsToCompile : Array (Name × Nat)
  lhs : Expr
  rhs : TSyntax `oclExpr
deriving Inhabited, BEq

structure ImplementedByBuilder where
  keys : Array DiscrTree.Key
  lhs : Expr
  declName : Name
  arity : Nat
deriving Inhabited, BEq

inductive SingleExtension where
  | implementedBy (impl : ImplementedBy)
  | implementedByBuilder (b : ImplementedByBuilder)
deriving Inhabited

/-- Enviroment extension that holds all necessary information for the APEX compiler. -/
structure Extension where
  implementedBy : DiscrTree ImplementedBy
  implementedByBuilders : DiscrTree ImplementedByBuilder
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
  }


def _root_.Lean.Expr.getFVars (e : Expr) : MetaM (Array Expr) := do
  return (← e.collectFVars.run {}).2.fvarIds.map Expr.fvar

def _root_.Lean.Expr.getMVars (e : Expr) : MetaM (Array Expr) := do
  return (e.collectMVars {}).result.map Expr.mvar


open Lean Meta
def addImpementedBy (lhs : Expr) (rhs : TSyntax `oclExpr) : MetaM ImplementedBy := do

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
unsafe def getBuilder (declName : Name) : MetaM (Array Expr → MetaM (TSyntax `oclExpr)) := do
  let env ← getEnv
  let opts ← getOptions
  match env.find? declName with
  | none      => throwError m!"Unknown constant `{declName}`"
  | some info =>
    if ← isDefEq q(Array Expr → MetaM (TSyntax `oclExpr)) info.type then
      return (← IO.ofExcept <| env.evalConst (Array Expr → MetaM (TSyntax `oclExpr)) opts declName)
    else
      throwError m!"ImplementedByBuilder `{privateToUserName declName}` has an unexpected type: Expected `ImplementedByBuilder`, but found `{info.type}`"


def runBuilder (xs : Array Expr) (builder : ImplementedByBuilder) : MetaM (TSyntax `oclExpr) := do
  let b ← unsafe getBuilder builder.declName
  return ← b xs


-- def runInterpreter? (α : Type) [t : ToExpr α] (val : Expr) : MetaM (Option α) := do
--   try
--     let val ← unsafe evalExpr α t.toTypeExpr val
--     return some val
--   catch _ =>
--     return none


partial def compileExpr (e : Expr) : MetaM (TSyntax `oclExpr) := do
  withTraceNode `HouLean.OpenCL.compiler (fun r => return m!"[{exceptEmoji r}] {e}") do
  let e ← instantiateMVars e
  match e with
  | .fvar id =>
    let name ← id.getUserName
    let id := mkIdent name
    `(oclExpr| $id:ident)
  | .lit (.natVal val) =>
    let val := Syntax.mkNatLit val
    return ← `(oclExpr| $val:num)
  | _ =>

   if let some val ← runInterpreter? Nat e then
     let val := Syntax.mkNatLit val
     return ← `(oclExpr| $val:num)

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

   throwError "Don't know how to compile {e}!"

where
  applyImpl (impl : ImplementedBy) (args : Array Expr) : MetaM (TSyntax `oclExpr) := do

     trace[HouLean.OpenCL.compiler] m!"Applying implemented_by, {impl.lhs} ==> {impl.rhs}"

     -- compile all arguments
     let mut compiledArgs : NameMap (TSyntax `oclExpr) := {}
     for (n, i) in impl.argsToCompile do
       let some arg := args[i]?  | throwError m!"Can't apply {impl.lhs} to {e}. Invalid argument index {i}!"

       let arg ← compileExpr arg
       compiledArgs := compiledArgs.insert n arg

     -- replace compiled arguments in rhs
     let compiled ← impl.rhs.raw.replaceM fun s =>
       match s with
       | `(oclExpr| $id:ident) =>
         return compiledArgs.get? id.getId
       | _ =>
         return none

     return ⟨compiled⟩


def compileType (e : Expr) : MetaM Ident := do
  match ← compileExpr e with
  | `(oclExpr| $id:ident) => return id
  | stx =>
    throwError m!"Unexpected result when compiling type {stx}!"


open Lean Elab Term Command
elab "impl_by" bs:bracketedBinder* " : " lhs:term  " ==> " rhs:oclExpr : command => do

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
          let rhs ← elabTermAndSynthesize (← `(term| do $[$rhs:doElem]*)) q(MetaM (TSyntax `oclExpr))
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
