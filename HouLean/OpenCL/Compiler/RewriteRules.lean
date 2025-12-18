import HouLean.OpenCL.Compiler.Extension

open HouLean.Meta

namespace HouLean.OpenCL.Compiler

open Lean Meta

syntax "impl_by" bracketedBinder* " : " term " ==> " clExpr : command


open Lean Elab Term Command Syntax in
elab_rules : command
| `(impl_by $bs:bracketedBinder*  :  $lhs:term  ==> $rhs:clExpr) => do

  -- strip any comments after this command
  -- todo: figure out how to set up the parser such that we do not have to do this!
  let rhs : TSyntax `clExpr := ⟨rhs.raw.setTailInfo .none⟩

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

      let args := ctx' ++ xs
      let mut argsToCompile : Array (TSyntax `clExpr × Nat) := #[]
      for arg in args, i in [0:args.size] do
        let id := arg.fvarId!
        let name ← id.getUserName
        if rhs.raw.hasIdent name then
          let id := mkIdent name
          argsToCompile := argsToCompile.push (← `(clExpr| $id:ident), i)

      let _ ← addImplementedBy e rhs argsToCompile


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
          let rhs ← elabTermAndSynthesize (← `(term| do $[$rhs:doElem]*)) q(CompileM (TSyntax `clExpr))
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
