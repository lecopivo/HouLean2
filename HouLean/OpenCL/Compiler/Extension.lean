import HouLean.OpenCL.Compiler.Grammar
import HouLean.OpenCL.Compiler.Types
import HouLean.Meta.Basic

open HouLean.Meta

namespace HouLean.OpenCL.Compiler

open Lean Meta

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
unsafe def getBuilder (declName : Name) : CompileM (Array Expr → CompileM (TSyntax `clExpr)) := do
  let env ← getEnv
  let opts ← getOptions
  match env.find? declName with
  | none      => throwError m!"Unknown constant `{declName}`"
  | some info =>
    if ← isDefEq q(Array Expr → CompileM (TSyntax `clExpr)) info.type then
      return (← IO.ofExcept <| env.evalConst (Array Expr → CompileM (TSyntax `clExpr)) opts declName)
    else
      throwError m!"ImplementedByBuilder `{privateToUserName declName}` has an unexpected type: Expected `ImplementedByBuilder`, but found `{info.type}`"


def runBuilder (xs : Array Expr) (builder : ImplementedByBuilder) : CompileM (TSyntax `clExpr) := do
  let b ← unsafe getBuilder builder.declName
  return ← b xs
