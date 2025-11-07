import Lean

open Lean Meta

namespace HouLean.Meta

def withStructureFieldTypes (structName : Name) (k : Array Expr → Array StructureFieldInfo → Array Expr → MetaM α) : 
    MetaM α := do
  let env ← getEnv
  if isStructure (← getEnv) structName then
    let info := getStructureInfo env structName

    -- get field infos in the order as they appear in the struct
    let fieldInfos := info.fieldNames.filterMap (fun n => getFieldInfo? env structName n)

    let c ← mkConstWithFreshMVarLevels structName
    return ← forallTelescope (← inferType c) fun params _ => do
      withLocalDeclD `s (mkAppN c params) fun s => do
      let fieldTypes ← fieldInfos.mapM (fun finfo => (mkAppM finfo.projFn #[s]) >>= inferType)

      k params fieldInfos fieldTypes
  else
    throwError s!"Expected structrue!"
