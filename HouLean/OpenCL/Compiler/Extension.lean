import HouLean.OpenCL.Compiler.Grammar
import HouLean.OpenCL.Compiler.Types
import HouLean.Meta.Basic

open HouLean.Meta

namespace HouLean.OpenCL.Compiler

open Lean Meta

structure ImplementedBy where
  keys : Array DiscrTree.Key
  argsToCompile : Array (TSyntax `clExpr × Nat)
  lhs : Expr
  rhs : TSyntax `clExpr
deriving Inhabited, BEq

structure ImplementedByBuilder where
  keys : Array DiscrTree.Key
  lhs : Expr
  declName : Name
  arity : Nat
  typeBuilder : Bool
deriving Inhabited, BEq

structure OpenCLFunction where
  funDef : TSyntax ``clFunction
  clName : Name
  leanName : Name

structure OpenCLTypeSyntax where
  quals : Array (TSyntax `clTypeQ) := #[]
  name : Name
  pointer : Bool := false
deriving Inhabited, BEq

structure OpenCLType where
  typeDef? : Option (TSyntax `clTypeSpec)
  clType : OpenCLTypeSyntax
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

def OpenCLTypeSyntax.mkDeclaration (t : OpenCLTypeSyntax) (varId : Ident) (val : TSyntax `clExpr)
    (const := false) :
    CompileM (TSyntax `clStmtLike) := do
  let spec ← `(clTypeSpec| $(mkIdent t.name):ident)
  let mut quals := t.quals
  if const then
    quals := quals.push (← `(clTypeQ| const))
  if t.pointer then
    `(clStmtLike| $[$quals:clTypeQ]* $spec:clTypeSpec * $varId:ident = $val:clExpr;)
  else
    `(clStmtLike| $[$quals:clTypeQ]* $spec:clTypeSpec $varId:ident = $val:clExpr;)

def OpenCLTypeSyntax.mkParamDecl (t : OpenCLTypeSyntax) (varId : Ident) :
    CompileM (TSyntax ``clParamDecl) := do
  let spec ← `(clTypeSpec| $(mkIdent t.name):ident)
  if t.pointer then
    `(clParamDecl| $[$(t.quals):clTypeQ]* $spec:clTypeSpec * $varId:ident)
  else
    `(clParamDecl| $[$(t.quals):clTypeQ]* $spec:clTypeSpec $varId:ident)

def OpenCLTypeSyntax.mkFunction (r : OpenCLTypeSyntax) (funId : Ident)
    (params : Array (TSyntax ``clParamDecl)) (stmts : Array (TSyntax `clStmtLike)) :
    CompileM (TSyntax ``clFunction) := do
  let spec ← `(clTypeSpec| $(mkIdent r.name):ident)
  if r.pointer then
    `(clFunction| $spec:clTypeSpec * $funId:ident($params:clParamDecl,*) { $stmts:clStmtLike* })
  else
    `(clFunction| $spec:clTypeSpec $funId:ident($params:clParamDecl,*) { $stmts:clStmtLike* })

open Lean Meta
def addImplementedBy (lhs : Expr) (rhs : TSyntax `clExpr) (argsToCompile : Array (TSyntax `clExpr × Nat)) : MetaM ImplementedBy := do

  let lhs ← instantiateMVars lhs
  -- let type ← inferType lhs

  if lhs.hasMVar then
    let mvars ← lhs.getMVars
    throwError m!"Can't add implemented_by `{lhs} ==> {rhs}`. Lhs contains mvars: {mvars}"

  if lhs.hasFVar then
    let fvars ← lhs.getFVars
    throwError m!"Can't add implemented_by `{lhs} ==> {rhs}`. Lhs contains mvars: {fvars}"

  trace[HouLean.OpenCL.compiler] "Added implemented by\n{lhs} ==> {rhs}\nargs to compile: {argsToCompile}"

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

def addOpenCLType (type : Expr) (clTypeName : String) (definition : Option (TSyntax `clTypeSpec)) : MetaM Unit := do

  let type ← whnfR type

  compilerExt.add (.clTypeDef {
    typeDef? := definition
    clType := {
      quals := #[]
      name := .mkSimple clTypeName
      pointer := false
    }
    leanType := type
  })

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

open Qq in
unsafe def getTypeBuilder (declName : Name) : CompileM (Array Expr → CompileM OpenCLTypeSyntax) := do
  let env ← getEnv
  let opts ← getOptions
  match env.find? declName with
  | none      => throwError m!"Unknown constant `{declName}`"
  | some info =>
    if ← isDefEq q(Array Expr → CompileM OpenCLTypeSyntax) info.type then
      return (← IO.ofExcept <| env.evalConst (Array Expr → CompileM OpenCLTypeSyntax) opts declName)
    else
      throwError m!"ImplementedByBuilder `{privateToUserName declName}` has an unexpected type: Expected `ImplementedByBuilder`, but found `{info.type}`"

def runBuilder (xs : Array Expr) (builder : ImplementedByBuilder) : CompileM (TSyntax `clExpr) := do
  let b ← unsafe getBuilder builder.declName
  return ← b (← xs.mapM instantiateMVars)

def runTypeBuilder (xs : Array Expr) (builder : ImplementedByBuilder) : CompileM OpenCLTypeSyntax := do
  let b ← unsafe getTypeBuilder builder.declName
  return ← b (← xs.mapM instantiateMVars)
