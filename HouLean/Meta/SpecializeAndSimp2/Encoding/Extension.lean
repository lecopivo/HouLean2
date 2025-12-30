import Lean

open Lean Meta

structure TypeEncoding where
  encoding : Array Expr
  decode : Expr


#check SimpM


open Lean Meta

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
