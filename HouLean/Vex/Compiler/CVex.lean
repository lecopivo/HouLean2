import HouLean.Vex.Compiler.CollectBoundVariables

open Lean System Parser Category

namespace HouLean.Vex.Compiler

def snippetToCVex (snippet : TSyntax ``vexSnippet) (mainName : String) : 
    MacroM (TSyntax ``cvexProgram) := do
  let boundVars ← collectBoundVars snippet
  let boundVars := boundVars.mkUniqueStrNames snippet
  
  -- replace all bound variable accesses via @ with new variable names
  let snippet ← snippet.raw.replaceM fun stx => do
    unless stx.getKind == ``attrAccess do
      return .none
    let id := (stx.getArg 1).getId
    let some var := boundVars[id]?
      | Macro.throwErrorAt stx "Unrecognized bound variable {stx}!"
    let newId := mkIdent (Name.mkSimple var.strName)
    return some newId

  -- type cast back to vexSnippet
  let snippet : TSyntax `vexSnippet := ⟨snippet⟩

  let params ← boundVars.mkCVexParamList
  let mainId := mkIdent (Name.mkSimple mainName)      

  return ⟨← `(cvexProgram| 
    cvex $mainId($params)
    { 
      $snippet
    })⟩

