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
  let structCmd ← `(command| structure $(mkIdent freshName):ident where
    $[$fieldDecls]*)
  
  elabCommand structCmd
  
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

end AnonymousStruct


