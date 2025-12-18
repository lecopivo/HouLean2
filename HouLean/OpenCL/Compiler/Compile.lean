import HouLean.OpenCL.Compiler.SpecAndSimp
import HouLean.OpenCL.Compiler.Main


open Lean Meta

namespace HouLean.OpenCL.Compiler

open Meta

open SpecializeAndSimp in
def fullCompile (e : Expr) : MetaM MessageData := do
  withoutModifyingEnv do

    forallTelescope (← inferType e) fun xs r => do
      let body := e.beta xs

      let (body', s) ← (specializeAndSimp body).runInMeta
        {} { zeta := false } #[`opencl_csimp] specializeImplementedBy

      let go := do
        withFVars xs fun varIds => do
        compileBlock body'

        let rt' ← compileType r
        let ts' ← xs.mapM (fun x => inferType x >>= compileType)
        let returnType : TSyntax `clDeclSpec ← `(clDeclSpec| $rt':ident)
        let argTypes ← ts'.mapM fun t => `(clTypeSpec| $t:ident)
        let stmts := (← get).statements

        let mainId := mkIdent (.mkSimple "main")
        `(clFunction| $returnType $mainId:ident($[$argTypes:clTypeSpec $varIds:ident],*) { $stmts* })

      let funs ← s.specOrder.mapM compileDecl
      let (main, _) ← go {} {}

      let mut msg : MessageData := m!""

      for f in funs do
        msg := msg ++ m!"{f}\n\n"
      msg := msg ++ m!"{main}"

      return msg


open Lean Elab Command Meta in
elab c:"#opencl_compile " e:term : command => do
  liftTermElabM do
    let e ← Term.elabTermAndSynthesize e none
    let m ← fullCompile e
    logInfoAt c m
