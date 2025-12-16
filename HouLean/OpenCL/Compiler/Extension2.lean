import Lean
import HouLean.OpenCL.Compiler.Grammar2
import HouLean.OpenCL.Basic

open Lean Meta

namespace HouLean.OpenCL.Compiler2

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
