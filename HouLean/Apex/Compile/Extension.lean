import HouLean.Apex.Compile.Types
import HouLean.Apex.Generated.Defs

open Lean Meta

namespace HouLean.Apex.Compiler

inductive SingleExtension where
  | apexBuiltinType (leanType : Name) (type : ApexTypeTag)
  /-- During compilation we replace `src` with `trg`.

  These functions might not have the same Lean type but they should
  have the same `ApexFunType`.

  This is also used for types. We might have `src`
  -/
  | implementedByName (src : Name) (trg : Name) (argMap : Array (Option Nat))
  /-- During compilation we replace `src` with `trg`.

  Replacing `Expr` with `Expr` is used when we need to
  replace `@Array.get Float` and `@Array.get Int` with different
  functions.

  `src` and `trg` must have the same APEX types
  -/
  | implementedByExpr (src : Expr) (trg : Expr)
  /-- Translation of Lean functions to APEX nodes. -/
  | nodeType (fn : Expr) (nodeType : NodeType)
  /-- Unfold declaration during compilation. -/
  | unfold (toUfold : Name)

deriving Inhabited


/-- Enviroment extension that holds all necessary information for the APEX compiler. -/
structure Extension where
  /-- Builtin APEX types, other types are registered with `ApexType` class -/
  apexTypes : NameMap ApexTypeTag
  implementedByName : NameMap (Name × Array (Option Nat))
  implementedByExpr : ExprMap Expr
  nodeTypes : ExprMap NodeType -- this should be just a `NameMap NodeType` !
  toUnfold : NameSet
deriving Inhabited

abbrev CompilerExt := SimpleScopedEnvExtension SingleExtension Extension

initialize compilerExt : CompilerExt ←
  registerSimpleScopedEnvExtension {
    name := by exact decl_name%
    initial := default
    addEntry := fun es e =>
      match e with
      | .apexBuiltinType leanType apexType =>
        {es with apexTypes := es.apexTypes.insert leanType apexType}
      | .implementedByName src trg argMap =>
        {es with implementedByName := es.implementedByName.insert src (trg, argMap)}
      | .implementedByExpr src trg =>
        {es with implementedByExpr := es.implementedByExpr.insert src trg}
      | .nodeType fn nodeType =>
        {es with nodeTypes := es.nodeTypes.insert fn nodeType}
      | .unfold name =>
        {es with toUnfold := es.toUnfold.insert name}
  }
