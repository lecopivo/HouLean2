import HouLean.Apex.Compile.Types

open Lean Meta

namespace HouLean.Apex.Compiler

inductive SingleExtension where
  | apexType (leanType : Expr) (type : ApexType)
  /-- During compilation we replace `srcFun` with `trgFun`.

  These functions might not have the same Lean type but they should
  have the same `ApexFunType`.

  Note: `srcFun` and `trgFun` might have zero arguments, i.e. just constants -/
  | implementedBy (srcFun : Expr) (trgFun : Expr)
  /-- Translation of Lean functions to APEX nodes. -/
  | nodeType (fn : Expr) (nodeType : NodeType)
  /-- Unfold declaration during compilation. -/
  | unfold (toUfold : Name)

deriving Inhabited

/-- Enviroment extension that holds all necessary information for the APEX compiler. -/
structure Extension where
  apexTypes : ExprMap ApexType
  implementedBy : ExprMap Expr
  nodeTypes : ExprMap NodeType
  toUnfold : NameSet
deriving Inhabited

abbrev CompilerExt := SimpleScopedEnvExtension SingleExtension Extension

initialize compilerExt : CompilerExt ←
  registerSimpleScopedEnvExtension {
    name := by exact decl_name%
    initial := default
    addEntry := fun es e =>
      match e with
      | .apexType leanType apexType =>
        {es with apexTypes := es.apexTypes.insert leanType apexType}
      | .implementedBy srcFun trgFun =>
        {es with implementedBy := es.implementedBy.insert srcFun trgFun}
      | .nodeType fn nodeType =>
        {es with nodeTypes := es.nodeTypes.insert fn nodeType}
      | .unfold name =>
        {es with toUnfold := es.toUnfold.insert name}
  }
