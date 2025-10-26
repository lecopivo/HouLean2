import HouLean.Vex.Parser
import HouLean.Vex.VexToTerm
import Lean
import Qq

namespace VEX

open Lean Parser.Category Syntax Elab Command Meta

/-- Information about a VEX function overload -/
structure VexFunctionInfo where
  fnName : Name
  declName : Name
  inputType : Expr
  returnType : Expr
  inst : Expr

def VexFunctionInfo.default : VexFunctionInfo := {
  fnName := .anonymous
  declName := .anonymous
  inputType := .const .anonymous []
  returnType := .const .anonymous []
  inst := .const .anonymous []
}

instance : Inhabited VexFunctionInfo where
  default := VexFunctionInfo.default

/-- Environment extension to track VEX function declarations -/
initialize vexFunctionExt : SimplePersistentEnvExtension VexFunctionInfo (Array VexFunctionInfo) ←
  registerSimplePersistentEnvExtension {
    addEntryFn := Array.push
    addImportedFn := fun arrs => arrs.foldl (fun acc arr => acc ++ arr) #[]
  }

/-- Register a VEX function overload -/
def registerVexFunction (name : Name) (declName : Name) (inputType returnType inst : Expr) : CoreM Unit := do
  let info : VexFunctionInfo := { fnName := name, declName := declName, inputType := inputType, returnType := returnType, inst := inst }
  modifyEnv fun env => vexFunctionExt.addEntry env info

/-- Get all overloads for a VEX function name -/
def getVexOverloads (name : Name) : CoreM (Array VexFunctionInfo) := do
  let infos := vexFunctionExt.getState (← getEnv)
  return infos.filter (·.fnName == name)

/-- Split VEX parameter list into type-identifier pairs -/
partial def vexParamListSplit (paramList : TSyntax `vexParamList) : Array (TSyntax `vexFullType × Ident) :=
  let rec go (stx : TSyntax `vexParamList) (acc : Array (TSyntax `vexFullType × Ident)) : 
      Array (TSyntax `vexFullType × Ident) :=
    match stx with
    | `(vexParamList| $param:vexParam) =>
      acc.push (extractParam param)
    | `(vexParamList| $param:vexParam ; $rest:vexParamList) =>
      go rest (acc.push (extractParam param))
    | _ => acc
  go paramList #[]
where
  extractParam (param : TSyntax `vexParam) : TSyntax `vexFullType × Ident :=
    match param with
    | `(vexParam| $ty:vexFullType $id:ident) => (ty, id)
    | `(vexParam| $ty:vexFullType $id:ident [ ]) => (ty, id)
    | _ => default

/-- Build a product type from a list of types -/
def mkProdType (ts : List Term) : MacroM Term := do
  match ts with
  | [] => `(Unit)
  | [t] => return t
  | t :: ts => 
    let r ← mkProdType ts
    `(term| $t × $r)

/-- Convert VEX parameter list to product type and identifier array -/
partial def vexParamListToParamsProd (paramList : TSyntax `vexParamList) : MacroM (Array Ident × Term) := do
  let (ts, ids) := (vexParamListSplit paramList).unzip
  let ts ← ts.mapM vexFullTypeToTerm 
  let ts ← mkProdType ts.toList
  return (ids, ts)

/-- Build a tuple pattern from identifiers -/
def mkIdentTuple (xs : List Ident) : MacroM Term := 
  match xs with
  | [] => `(term| ())
  | [x] => `(term| $x)
  | x :: xs => do
    let xs ← mkIdentTuple xs
    `(term| ($x,$xs))

/-- Build a tuple term from terms -/
def mkTermTuple (xs : List Term) : MacroM Term := 
  match xs with
  | [] => `(term| ())
  | [x] => `(term| $x)
  | x :: xs => do
    let xs ← mkTermTuple xs
    `(term| ($x,$xs))

/-- Check if a constant name exists in the environment -/
def constExists (name : Name) : CoreM Bool := do
  let env ← getEnv
  return env.contains name

/-- Declare a VEX function (without implementation) -/
elab "vexfunction" r:vexFullType id:ident "(" args:vexParamList ")" ";" : command => do
  let declId := mkIdent (id.getId.capitalize.appendAfter "Decl")
  let declName := declId.getId
  
  -- Only declare the class if it doesn't exist yet
  let ex ← liftCoreM <| constExists declName
  if !ex then
    let declCommand ← liftMacroM <| 
      `(command| 
         class $declId (InputType : Type) (ReturnType : Type) where   
           $id:ident : InputType → ReturnType)
    let exportCommand ← `(command| export $declId ($id))

    elabCommand declCommand
    elabCommand exportCommand
    
    -- let returnType ← liftMacroM <| vexFullTypeToTerm r
    -- let (_, inputType) ← liftMacroM <| vexParamListToParamsProd args
    -- logInfo m!"Declared VEX function: {id.getId} : {inputType} → {returnType}"

/-- Define a VEX function implementation -/
elab "vexfunction" r:vexFullType id:ident "(" args:vexParamList ")" "{" stmts:vexStmt* "}" : command => do
  let declId := mkIdent (id.getId.capitalize.appendAfter "Decl")
  let declName := declId.getId
  
  let returnType ← liftMacroM <| vexFullTypeToTerm r
  let (ids, inputType) ← liftMacroM <| vexParamListToParamsProd args

  -- Ensure class is declared (only if it doesn't exist)
  let ex ← liftCoreM <| constExists declName
  if !ex then
    let declCommand ← liftMacroM <| 
      `(command| 
         class $declId (InputType : Type) (ReturnType : Type) where   
           $id:ident : InputType → ReturnType)
    let exportCommand ← `(command| export $declId ($id))
    
    elabCommand declCommand
    elabCommand exportCommand

  -- Build the instance
  let command ← liftMacroM <| do
    let ctx : VexContext := ⟨.point, {}⟩
    let bodyElems ← ReaderT.run (show VexMacroM _ from do
      let mut elems : Array (TSyntax `doElem) := #[]
      for stmt in stmts do
        let stmtElems ← vexStmtToDoElem stmt
        elems := elems ++ stmtElems
      return elems
    ) ctx

    if ids.size == 0 then
      `(command| 
         instance : $declId $inputType $returnType where   
           $id:ident := Id.run do
             $[$bodyElems:doElem]*)
    else 
      let x ← mkIdentTuple (ids.toList)
      `(command| 
         instance : $declId $inputType $returnType where   
           $id:ident := fun $x => Id.run do
             $[$bodyElems:doElem]*)

  elabCommand command
  
  -- Register the overload - instantiate all metavariables first
  let inputTypeExpr ← liftTermElabM <| do
    let e ← Term.elabTerm inputType none
    instantiateMVars e
  let returnTypeExpr ← liftTermElabM <| do
    let e ← Term.elabTerm returnType none
    instantiateMVars e
  let instanceType := mkApp2 (.const declName []) inputTypeExpr returnTypeExpr
  
  liftCoreM <| registerVexFunction id.getId declName inputTypeExpr returnTypeExpr instanceType
  
  -- logInfo m!"Defined VEX function overload: {id.getId} : {inputType} → {returnType}"

/-- Format a VEX function signature for error messages -/
def formatSignature (info : VexFunctionInfo) : MetaM Format := do
  let inputFmt ← Meta.ppExpr info.inputType
  let returnFmt ← Meta.ppExpr info.returnType
  return f!"{info.fnName} : {inputFmt} → {returnFmt}"

/-- VEX function application elaborator -/
elab fn:ident noWs "(" xs:term,* ")" : term <= expectedType? => do
  let fnName := fn.getId
  
  -- Get all overloads for this function
  let overloads ← getVexOverloads fnName
  
  if overloads.isEmpty then
    throwError m!"unknown VEX function: {fnName}"
  
  -- Build the input tuple
  let x ← liftMacroM <| mkTermTuple xs.getElems.toList
  let xExpr ← Term.elabTerm x none
  let xType ← inferType xExpr
  -- Instantiate all metavariables in xType
  let xType ← instantiateMVars xType
  
  -- Find all matching overloads
  let mut matchesArray : Array (VexFunctionInfo × Expr × Expr) := #[]
  
  for info in overloads do
    -- Save the current message log state to suppress errors from failed attempts
    let savedMsgs ← Core.getMessageLog
    let match? : Option _ ← Lean.Elab.Term.withoutErrToSorry do 
      try
        -- Instantiate metavariables in stored types
        let inputType ← instantiateMVars info.inputType
        let returnType ← instantiateMVars info.returnType

        -- Try to elaborate the arguments with this specific input type
        let xElaborated ← Term.elabTermEnsuringType x inputType

        -- Check if return type matches expected type (if provided)
        let mut validReturn := true
        validReturn := ← isDefEq returnType expectedType?

        if validReturn then
          let fnType ← mkArrow inputType returnType
          let fnElaborated ← Term.elabTermEnsuringType fn fnType
          return some (info, fnElaborated, xElaborated)
      catch _ =>
        -- Failed to elaborate with this overload, restore message log and skip it
        Core.setMessageLog savedMsgs
      return none
    if let some m := match? then
      matchesArray := matchesArray.push m
  
  match matchesArray.size with
  | 0 =>
    -- No matches - show all potential overloads
    let mut msg := m!"no matching overload for {fnName}\n\nProvided arguments: {xType}\n\nAvailable overloads:"
    for info in overloads do
      let sig ← formatSignature info
      msg := msg ++ m!"\n  • {sig}"
    throwError msg
    
  | 1 =>
    -- Exactly one match
    let (_, fnExpr, xExpr) := matchesArray[0]!
    return fnExpr.app xExpr
    
  | _ =>
    -- Multiple matches - warn and use first one
    let mut msg := m!"ambiguous VEX function call: {fnName}\n\nMatching overloads:"
    for (info, _, _) in matchesArray do
      let sig ← formatSignature info
      msg := msg ++ m!"\n  • {sig}"
    logWarning msg
    
    let (_, fnExpr, xExpr) := matchesArray[0]!
    return fnExpr.app xExpr

end VEX
