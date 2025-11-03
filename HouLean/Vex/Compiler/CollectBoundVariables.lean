import HouLean.Vex.Compiler.BoundVariable
open Lean

namespace HouLean.Vex.Compiler

namespace CollectBoundVariables

abbrev State := Std.HashMap Name BoundVariable
abbrev CollectM := StateT State MacroM

def addVarCore (ref : Syntax) (ident : Ident) (type? : Option AttribTypeTag) (isExport := false) : CollectM Unit := do
  let name := ident.getId
  let .str _ strName := ident.getId.eraseMacroScopes
    | Macro.throwErrorAt ref s!"Invalid name '{ident}' of a bound variable!"

  let s ← get
  if let some var := s[name]? then
    -- we already found variable of this name
    let mut var := var

    -- check the type is the same
    let t? ← 
      match var.type?, type? with
      | none, none => pure none
      | some t, none 
      | none, some t => pure (some t)
      | some t, some t' => 
        if t != t' then
          Macro.throwErrorAt ref 
            s!"Invalid type annotation '{t'.typeAnnotation}@{name}', \
               previously annotated with '{t.typeAnnotation}@{name}'!"
        else
          pure (some t)
    var := {var with type? := t?}

    -- update export
    var := {var with isExport := var.isExport || isExport}

    set (s.insert name var)
  else
    set (s.insert name {
      name := name
      strName := strName
      type? := type?
      isExport := isExport
    })

def addVar (s : TSyntax `attribAccess) (isExport := false) : CollectM Unit := do
  match s with
  -- string, integer, float
  | `(attrAccess| s@$x:ident)   => addVarCore s x (some .string) isExport
  | `(attrAccess| s[]@$x:ident) => addVarCore s x (some .stringArray) isExport
  | `(attrAccess| i@$x:ident)   => addVarCore s x (some .int) isExport
  | `(attrAccess| i[]@$x:ident) => addVarCore s x (some .intArray) isExport
  | `(attrAccess| f@$x:ident)   => addVarCore s x (some .float) isExport
  | `(attrAccess| f[]@$x:ident) => addVarCore s x (some .stringArray) isExport

  -- vectors
  | `(attrAccess| u@$x:ident)   => addVarCore s x (some .vector2) isExport
  | `(attrAccess| u[]@$x:ident) => addVarCore s x (some .vector2Array) isExport
  | `(attrAccess| v@$x:ident)   => addVarCore s x (some .vector3) isExport
  | `(attrAccess| v[]@$x:ident) => addVarCore s x (some .vector3Array) isExport
  | `(attrAccess| p@$x:ident)   => addVarCore s x (some .vector4) isExport
  | `(attrAccess| p[]@$x:ident) => addVarCore s x (some .vector4Array) isExport

  -- matrices
  | `(attrAccess| m2@$x:ident)   => addVarCore s x (some .matrix2) isExport
  | `(attrAccess| m2[]@$x:ident) => addVarCore s x (some .matrix2Array) isExport
  | `(attrAccess| m3@$x:ident)   => addVarCore s x (some .matrix3) isExport
  | `(attrAccess| m3[]@$x:ident) => addVarCore s x (some .matrix3Array) isExport
  | `(attrAccess| m4@$x:ident)   => addVarCore s x (some .matrix4) isExport
  | `(attrAccess| m4[]@$x:ident) => addVarCore s x (some .matrix4Array) isExport

  | `(attrAccess| d@$x:ident)   => addVarCore s x (some .dict) isExport
  | `(attrAccess| d[]@$x:ident) => addVarCore s x (some .dictArray) isExport

  -- unknown type
  | `(attrAccess| @$x:ident) => addVarCore s x none isExport

  | _ => Macro.throwErrorAt s s!"CollectBoundVariables: Unhandled attribute type case {s}!"

end CollectBoundVariables

private def _root_.Lean.Syntax.visitKind {m} [Monad m] 
    (s : Syntax) (kindPred : SyntaxNodeKind → Bool) (visitor : SyntaxNodeKind → Syntax → m Unit) : 
    m Unit := 
  match s with
  | .node _ kind args => do
    if kindPred kind then
      visitor kind s
    args.foldlM (init:=()) (fun _ a => a.visitKind kindPred visitor)
  | _ => pure ()

open CollectBoundVariables Parser Category in
/-- Collect information about bound variables in a VEX code snippet. -/
def collectBoundVars (stx : TSyntax ``vexSnippet) : MacroM BoundVariables := do

  let go : CollectM Unit := do
    stx.raw.visitKind (fun k => k == ``attrAccess || k == ``assignExpr) fun kind s => do
      if kind == ``assignExpr then
        let lhs := s.getArg 0
        if lhs.getKind == ``attrAccess then
          addVar ⟨s.getArg 0⟩ true
      else 
        addVar ⟨s⟩ false

  return (← go {}).2
