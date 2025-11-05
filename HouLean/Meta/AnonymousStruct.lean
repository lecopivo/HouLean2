import Lean
import Std.Data.HashMap

open Lean Elab Command Term Meta Parser

namespace HouLean
namespace AnonymousStruct

initialize registerTraceClass `HouLean.anon_struct

-- Field information for the struct
structure FieldInfo where
  name : Name
  type : Expr  -- in case of parameters this is function like `fun (α : Type) => Array α`
  deriving Inhabited, BEq, Hashable

instance : ToString FieldInfo where
  toString fi := s!"{fi.name} : {fi.type}"

structure StructSignature where
  paramNum : Nat
  fields : Array FieldInfo
  deriving Inhabited, BEq, Hashable

instance : ToString StructSignature where
  toString sig := toString sig.fields

-- Entry in the environment extension
structure AnonStructEntry where
  signature : StructSignature
  structName : Name
  deriving Inhabited

structure Extension where
  sigToName : Std.HashMap StructSignature Name
  nameToSig : NameMap StructSignature
deriving Inhabited

-- Environment extension to track anonymous structs
initialize anonStructExt : SimplePersistentEnvExtension AnonStructEntry Extension ←
  registerSimplePersistentEnvExtension {
    addImportedFn := fun as => 
      as.foldl (fun acc a => a.foldl (fun m e => 
        {m with sigToName := m.sigToName.insert e.signature e.structName
                nameToSig := m.nameToSig.insert e.structName e.signature}) acc) default
    addEntryFn := fun m e => 
        {m with sigToName := m.sigToName.insert e.signature e.structName
                nameToSig := m.nameToSig.insert e.structName e.signature}
  }


open PrettyPrinter in
-- Get or create a structure for the given signature
def getOrCreateStruct (sig : StructSignature) : CommandElabM Name := do
  -- let normSig := sig.normalize
  
  -- Check if we already have this structure
  let env ← getEnv
  let structMap := (anonStructExt.getState env).sigToName
  
  if let some existingName := structMap[sig]? then
    return existingName
  
  -- Generate a fresh name for the structure
  let mut freshName := `AnonStruct
  let mut idx := structMap.size
  while (← getEnv).contains freshName do
    idx := idx + 1
    freshName := (`AnonStruct).appendIndexAfter idx
  
  -- Build field declarations for the structure
  let mut paramDecls : Array (TSyntax ``Term.bracketedBinder) := #[]
  let mut fieldDecls : Array (TSyntax ``Command.structExplicitBinder) := #[]

  if sig.fields.size != 0 then

    -- make field binders
    fieldDecls ← liftTermElabM <|
      lambdaBoundedTelescope (sig.fields[0]!.type) sig.fields.size fun params _ => do
      sig.fields.mapM (fun fi : FieldInfo => do
        let fieldName := mkIdent fi.name
        let fieldType := fi.type.beta params
        trace[HouLean.anon_struct] m!"field {fieldName} : {fieldType}"
        let fieldTypeStx ← 
            withOptions (fun opt => opt.setBool `pp.notation false) do
                  delab (fi.type.beta params)

        return (← `(Command.structExplicitBinder| ($fieldName:ident : $fieldTypeStx))))

    -- make parameter binders
    paramDecls ← liftTermElabM <|
      lambdaBoundedTelescope (sig.fields[0]!.type) sig.fields.size fun params _ => do
      params.mapM (fun param => do
        let name : Name ← param.fvarId!.getUserName
        let id := mkIdent name
        let type ← inferType param
        let typeStx ← 
            withOptions (fun opt => opt.setBool `pp.notation false) do
                  delab type
        `(bracketedBinder| ($id:ident : $typeStx)))

  
  -- Create and elaborate the structure command
  let id := mkIdent freshName
  let structCmd ← `(command| structure $id:ident $paramDecls* where
    $[$fieldDecls]*)

  trace[HouLean.anon_struct] m!"structure command:\n{structCmd}"
  
  elabCommand structCmd

  let cmds := #[
    ← `(command| deriving instance BEq for $id),
    ← `(command| deriving instance Inhabited for $id),
    ← `(command| deriving instance ToJson for $id),
    ← `(command| deriving instance FromJson for $id)]

  for cmd in cmds do
    try
      elabCommand cmd
    catch _ =>
      continue
  
  -- Register in our extension
  modifyEnv fun env => 
    anonStructExt.addEntry env { signature := sig, structName := freshName }
  
  return freshName


/-- Anonymous structure. -/
syntax (name := anonStructType) "struct " "{" (ident " : " term),* "}" : term

-- Macro to elaborate anonymous struct types
elab_rules : term
| `(struct { $[$names:ident : $types:term],* }) => do
  let types : Array Term := types
  let stx ← `(term| struct { $[$names:ident : $types:term],* })
  let types ← types.mapM (elabTermAndSynthesize · none)

  -- collect parameters fvars
  let (_,s) ← (for type in types do type.collectFVars).run {}
  let params := s.fvarSet.toArray.map (fun id => Expr.fvar id)
  let paramTypes ← liftM <| params.mapM inferType -- maybe instantiate mvars?

  trace[HouLean.anon_struct] m!"parameters {params}, types: {paramTypes}"

  -- check that types of parameters do not containt any fvars
  -- for now, do not support crazy dependently types structures!
  for type in paramTypes do
    if type.hasFVar then
      throwError s!"The type of structure field type can't depend on a parameter!"

  let parametrizedTypes ← liftM <|types.mapM (mkLambdaFVars params)
  
  let fields := names.zip parametrizedTypes |>.map fun (n, t) => 
    ({ name := n.getId, type := t } : FieldInfo)
  let sig : StructSignature := {
      paramNum := params.size
      fields := fields
    }

  trace[HouLean.anon_struct] m!"struct signature {sig}"

  let structName ← liftCommandElabM (getOrCreateStruct sig)
  let structId := mkIdent structName
  let cmd ← `(command|
    @[app_unexpander $structId]
    def unexpandAnonStruct : Lean.PrettyPrinter.Unexpander
    | _ => set_option hygiene false in `($stx)
    )
  let _ ← liftCommandElabM (elabCommand cmd)

  -- make metavariables for all parameters
  let t := (← mkConstWithFreshMVarLevels structName)
  let (paramMVars, _, _) ← forallMetaTelescope (← inferType t)
  let t := t.beta paramMVars

  -- now we fill all the parameter metavariables by unifying with the original field types
  withLocalDeclD `x t fun x => do
    -- go over all field types and unify to fill in the parameter metavariables
    for t in types, i in [0:types.size] do
      if (← isDefEq t (← inferType (.proj structName i x))) then
        continue
      else
        throwError m!"faild to back infer parameter types from the field of type {t}"

  return t


-- struct_push%: Add a field to a struct
-- Usage: struct_push% fieldName structVal newFieldVal
elab "struct" s:term "push%" field:ident ":=" val:term  : term => do
  -- Elaborate the struct value and the new field value
  let sExpr ← elabTermAndSynthesize s none  >>= instantiateMVars
  let sType ← inferType sExpr  >>= instantiateMVars
  let valExpr ← elabTermAndSynthesize val none >>= instantiateMVars
  let valType ← inferType valExpr >>= instantiateMVars

  if valType.hasFVar then
    throwError "New field's type can't depend on a parameter yet!"
  
  -- Get fields from existing struct type
  let env ← getEnv   
  let structMap := (anonStructExt.getState env).nameToSig
  
  let .const oldStructName _ := sType.getAppFn'
    | throwError "Expected anonymous struct type, got {sType}"
  
  let some oldSig := structMap.find? oldStructName
    | throwError "Struct not found in anonymous struct registry"
  let oldFields := oldSig.fields

  if oldSig.paramNum ≠ 0 then
    throwError "Pushing new elements to parametrized structures is not yet supported!"
  
  -- Check if field already exists
  if oldFields.any (·.name == field.getId) then
    throwError "Field {field.getId} already exists in struct"

  -- Add new field
  let newField : FieldInfo := { name := field.getId, type := valType }
  let newSig := {
    paramNum := 0
    fields := oldFields.push newField
    }

  -- Create new struct type
  let newStructName ← liftCommandElabM (getOrCreateStruct newSig)

  let newStructId := mkIdent newStructName
  let stx ← `({$s with $field:ident := $val : $newStructId:ident})
  elabTerm stx none


-- struct_pop%: Remove a field from a struct
-- Usage: struct% s pop field
elab "struct" s:term "pop%" field:ident : term => do
  -- Elaborate the struct value
  let sExpr ← elabTermAndSynthesize s none >>= instantiateMVars
  let sType ← inferType sExpr >>= instantiateMVars
  
  -- Get fields from existing struct type
  let env ← getEnv
  let structMap := (anonStructExt.getState env).nameToSig
  
  let .const oldStructName _ := sType.getAppFn'
    | throwError "Expected anonymous struct type, got {sType}"
  
  let some oldSig := structMap.find? oldStructName
    | throwError "Struct not found in anonymous struct registry"
  let oldFields := oldSig.fields

  if oldSig.paramNum ≠ 0 then
    throwError "Pop elements from parametrized structures is not yet supported!"

  
  -- Remove the specified field
  let newFields := oldFields.filter (·.name != field.getId)
  let newSig := {
    paramNum := 0
    fields := newFields
  }
  
  if newFields.size == oldFields.size then
    throwError "Field {field.getId} not found in struct"
  
  -- Create new struct type without that field
  let newStructName ← liftCommandElabM (getOrCreateStruct newSig)
  let newStructId := mkIdent newStructName

  let stx ← `({$s with : $newStructId})
  elabTerm stx none


end AnonymousStruct


