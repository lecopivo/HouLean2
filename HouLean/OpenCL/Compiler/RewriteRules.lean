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
  syntaxBuilder : Name
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
        throwError s!"Bug in {decl_name%}, failed to build lhs expression!"

      let args := ctx' ++ xs
      -- collect identifiers that appear on the rhs
      let argsToCompile : Array (Name × Nat) ←
        (args.zip (.range args.size))
        |>.filterMapM (fun (arg,i) => do
          if arg.isFVar then
            let name ← arg.fvarId!.getUserName
            if rhs.raw.hasIdent name then
              return some (name, i)
          return none)

      -- todo: add tracing
      -- logInfo m!"lhs: {e}\nargs to compile: {argsToCompile}"

      let (_,_,body) ← lambdaMetaTelescope e
      let keys ← DiscrTree.mkPath body

      -- let declName ← mkUniqueDeclName `implBy

      let impl : ImplementedBy := {
        keys := keys
        argsToCompile := argsToCompile
        lhs := e
        rhs := rhs
      }

      compilerExt.add (.implementedBy impl)


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
        syntaxBuilder := builderDeclName
      }

      compilerExt.add (.implementedByBuilder b)
      logInfo m!"script:\n{rhs}"
