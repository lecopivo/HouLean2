import Lean

open Lean Meta

namespace HouLean.OpenCL.Compiler2

structure Specialization where
  keys : Array DiscrTree.Key
  originalName : Name
  specializationName : Name
  fn : Expr
  -- /-- `∀ x₁ ... xₙ, original = specialized`
  -- To apply specizalization we unify `origianal` with the current expression
  -- and replace it with `specialized` -/
  -- statement : Expr
deriving Inhabited, BEq

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
  | specialization (s : Specialization)
deriving Inhabited

/-- Enviroment extension that holds all necessary information for the APEX compiler. -/
structure Extension where
  implementedBy : DiscrTree ImplementedBy
  implementedByBuilders : DiscrTree ImplementedByBuilder
  specializations : DiscrTree Specialization
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
      | .specialization s =>
        {es with specializations := es.specializations.insertCore s.keys s}
  }
