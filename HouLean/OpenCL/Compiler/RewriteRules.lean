import HouLean.OpenCL.Compiler.Grammar

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


def oclFunction' {type : Type} (spec : type) (name : String) (kind : OpenCLFunction.FunKind := .normal) : type := spec


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


partial def implementedBy (e : Expr) : MetaM (TSyntax `oclExpr) := do
  let e ← instantiateMVars e
  match e with
  | .fvar id =>
    let name ← id.getUserName
    let id := mkIdent name
    `(oclExpr| $id:ident)
  | _ =>
   -- let args := e.getAppArgs

   let s := (compilerExt.getState (← getEnv)).implementedBy
   let candidates ← s.getMatch e
   for c in candidates do

     let (args,_,body) ← lambdaMetaTelescope c.lhs

     unless ← isDefEq body e do
       continue

     let mut compiledArgs : NameMap (TSyntax `oclExpr) := {}
     for (n, i) in c.argsToCompile do
       let some arg := args[i]?  | throwError m!"Can't apply {c.lhs} to {e}. Invalid argument index {i}!"
       -- we might call `implementedBy` multiple times for one argument
       -- this seems wasteful but I'm not expecting more
       let arg ← implementedBy arg
       compiledArgs := compiledArgs.insert n arg

     -- todo: override for argList?

     -- replace
     let compiled ← c.rhs.raw.replaceM fun s =>
       match s with
       | `(oclExpr| $id:ident) =>
         return compiledArgs.get? id.getId
       | _ =>
         return none

     return ⟨compiled⟩

   throwError "Don't know how to compile {e}!"


open Lean Meta
def addImpementedBy (lhs : Expr) (rhs : TSyntax `oclExpr) : MetaM Unit := do
  let type ← inferType lhs

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

      addImpementedBy e rhs


open Lean Elab Term Command Qq
elab "impl_by" bs:bracketedBinder* " : " lhs:term  " ==> " "do" rhs:doElem* : command => do

  runTermElabM fun ctx => do
    elabBinders bs fun xs => do
      let e ← elabTerm lhs none

      let e ← mkLambdaFVars xs e >>= instantiateMVars
      let ctx' := ctx.filter (fun c => e.containsFVar c.fvarId!)
      let e ← mkLambdaFVars ctx' e >>= instantiateMVars


      let decls ← (ctx'++xs).mapM (fun x => do
        return (← x.fvarId!.getUserName, .default, fun _ => pure q(Expr)))
      let builder ← liftM <|
        withLocalDecls decls fun ys => do
          let rhs ← elabTermAndSynthesize (← `(term| do $[$rhs:doElem]*)) q(MetaM (TSyntax `oclExpr))
          mkLambdaFVars ys rhs

      let builderType ← inferType builder >>= instantiateMVars
      let builderDeclName ← mkUniqueDeclName `implStxBuilder

      let decl : Declaration := .defnDecl {
        name := builderDeclName
        levelParams := []
        type := builderType
        value := builder
        hints := .regular builder.approxDepth
        safety := .safe
      }

      addAndCompile decl

      let (_,_,body) ← lambdaMetaTelescope e
      let keys ← DiscrTree.mkPath body

      let b : ImplementedByBuilder := {
        keys := keys
        lhs := e
        declName := builderDeclName
        arity := decls.size
      }

      compilerExt.add (.implementedByBuilder b)
      logInfo m!"script:\n{rhs}"


#check Simp.getSimprocExtension?

  -- let ctx ← read
  -- match ctx.env.find? declName with
  -- | none      => throw <| IO.userError ("Unknown constant `" ++ toString declName ++ "`")
  -- | some info =>
  --   match info.type with
  --   | .const ``Simproc _ =>
  --     return .inl (← IO.ofExcept <| ctx.env.evalConst Simproc ctx.opts declName)
  --   | .const ``DSimproc _ =>
  --     return .inr (← IO.ofExcept <| ctx.env.evalConst DSimproc ctx.opts declName)
  --   | _ => throw <| IO.userError s!"Simproc `{privateToUserName declName}` has an unexpected type: Expected `Simproc` or `DSimproc`, but found `{info.type}`"


unsafe def getBuilder (declName : Name) : CoreM (Array Expr → MetaM (TSyntax `oclExpr)) := do
  let env ← getEnv
  let opts ← getOptions
  match env.find? declName with
  | none      => throwError m!"Unknown constant `{declName}`"
  | some info =>
    match info.type with
    | .const ``ImplementedByBuilder _ =>
      return (← IO.ofExcept <| env.evalConst (Array Expr → MetaM (TSyntax `oclExpr)) opts declName)
    | _ => throwError m!"ImplementedByBuilder `{privateToUserName declName}` has an unexpected type: Expected `ImplementedByBuilder`, but found `{info.type}`"


def instantiateBuilder (xs : Array Expr) (builder : ImplementedByBuilder) : MetaM Bool := do

  let lhs := builder.lhs.beta xs
  let b ← unsafe getBuilder builder.declName
  let rhs ← b xs

  sorry
