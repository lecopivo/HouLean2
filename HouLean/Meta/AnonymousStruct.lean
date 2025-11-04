import Lean
import Std.Data.HashMap

open Lean Elab Command Term Meta Parser

namespace AnonymousStruct

-- Field information for the struct
structure FieldInfo where
  name : Name
  type : Expr
  deriving Inhabited, BEq, Hashable

instance : ToString FieldInfo where
  toString fi := s!"{fi.name} : {fi.type}"

-- Struct signature for lookup
structure StructSignature where
  fields : Array FieldInfo
  deriving Inhabited, BEq, Hashable

-- Entry in the environment extension
structure AnonStructEntry where
  signature : StructSignature
  structName : Name
  deriving Inhabited

-- Environment extension to track anonymous structs
initialize anonStructExt : SimplePersistentEnvExtension AnonStructEntry (Std.HashMap StructSignature Name) ←
  registerSimplePersistentEnvExtension {
    addImportedFn := fun as => 
      as.foldl (fun acc a => a.foldl (fun m e => m.insert e.signature e.structName) acc) {}
    addEntryFn := fun m e => m.insert e.signature e.structName
  }

-- Get or create a structure for the given signature
def getOrCreateStruct (fields : Array FieldInfo) : CommandElabM Name := do
  let sig : StructSignature := { fields := fields }
  -- let normSig := sig.normalize
  
  -- Check if we already have this structure
  let env ← getEnv
  let structMap := anonStructExt.getState env
  
  if let some existingName := structMap[sig]? then
    return existingName
  
  -- Generate a fresh name for the structure
  let mut freshName := `AnonStruct
  let mut idx := structMap.size
  while (← getEnv).contains freshName do
    idx := idx + 1
    freshName := (`AnonStruct).appendIndexAfter idx
  
  -- Build field declarations for the structure
  let mut fieldDecls : Array (TSyntax ``Command.structExplicitBinder) := #[]
  for fi in fields do
    let fieldName := mkIdent fi.name
    let fieldType := fi.type
    let fieldTypeStx ← liftTermElabM 
      <| withOptions (fun opt => opt.setBool `pp.notation false) 
      <| PrettyPrinter.delab fieldType
    fieldDecls := fieldDecls.push (← `(Command.structExplicitBinder| ($fieldName:ident : $fieldTypeStx)))
  
  -- Create and elaborate the structure command
  let id := mkIdent freshName
  let structCmd ← `(command| structure $id:ident where
    $[$fieldDecls]*)
  
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
syntax (name := anonStructType) "struct " "{" (ident ":" term),* "}" : term

-- Macro to elaborate anonymous struct types
elab_rules : term
| `(struct { $[$names:ident : $types:term],* }) => do
  let types : Array Term := types
  let stx ← `(term| struct { $[$names:ident : $types:term],* })
  let types ← types.mapM (elabTerm · none)
  let fields := names.zip types |>.map fun (n, t) => 
    ({ name := n.getId, type := t } : FieldInfo)

  let structName ← liftCommandElabM (getOrCreateStruct fields)
  let structId := mkIdent structName
  let cmd ← `(command|
    @[app_unexpander $structId]
    def unexpandAnonStruct : Lean.PrettyPrinter.Unexpander
    | _ => `($stx)
    )
  let _ ← liftCommandElabM (elabCommand cmd)
  return (.const structName [])

-- add commands 
-- struct_push% field_name s val
-- struct_pop% field_name s

-- struct_push%: Add a field to a struct
-- Usage: struct_push% fieldName structVal newFieldVal
elab "struct%" s:term "push" field:ident ":=" val:term  : term => do
  -- Elaborate the struct value and the new field value
  let sExpr ← elabTermAndSynthesize s none  >>= instantiateMVars
  let sType ← inferType sExpr  >>= instantiateMVars
  let valExpr ← elabTermAndSynthesize val none >>= instantiateMVars
  let valType ← inferType valExpr >>= instantiateMVars
  
  -- Get fields from existing struct type
  let env ← getEnv
  let structMap := anonStructExt.getState env
  
  let mut oldFields : Array FieldInfo := #[]
  let mut oldStructName : Name := Name.anonymous
  
  match sType with
  | Expr.const structName _ =>
    -- Find in our map
    for (sig, name) in structMap.toList do
      if name == structName then
        oldFields := sig.fields
        oldStructName := structName
        break
  | _ => throwError "Expected anonymous struct type, got {sType}"

  if oldStructName.isAnonymous then
    throwError "Struct not found in anonymous struct registry"
  
  -- Check if field already exists
  if oldFields.any (·.name == field.getId) then
    throwError "Field {field.getId} already exists in struct"

  -- Add new field
  let newField : FieldInfo := { name := field.getId, type := valType }
  let newFields := oldFields.push newField

  -- Create new struct type
  let newStructName ← liftCommandElabM (getOrCreateStruct newFields)

  let newStructId := mkIdent newStructName
  let stx ← `({$s with $field:ident := $val : $newStructId:ident})
  elabTerm stx none


-- struct_pop%: Remove a field from a struct
-- Usage: struct% s pop field
elab "struct%" s:term "pop" field:ident : term => do
  -- Elaborate the struct value
  let sExpr ← elabTermAndSynthesize s none >>= instantiateMVars
  let sType ← inferType sExpr >>= instantiateMVars
  
  -- Get fields from existing struct type
  let env ← getEnv
  let structMap := anonStructExt.getState env
  
  let mut oldFields : Array FieldInfo := #[]
  let mut oldStructName : Name := Name.anonymous
  
  match sType with
  | Expr.const structName _ =>
    for (sig, name) in structMap.toList do
      if name == structName then
        oldFields := sig.fields
        oldStructName := structName
        break
  | _ => throwError "Expected anonymous struct type, got {sType}"
  
  if oldStructName.isAnonymous then
    throwError "Struct not found in anonymous struct registry"
  
  -- Remove the specified field
  let newFields := oldFields.filter (·.name != field.getId)
  
  if newFields.size == oldFields.size then
    throwError "Field {field.getId} not found in struct"
  
  -- Create new struct type without that field
  let newStructName ← liftCommandElabM (getOrCreateStruct newFields)
  let newStructId := mkIdent newStructName

  let stx ← `({$s with : $newStructId})
  elabTerm stx none


end AnonymousStruct


