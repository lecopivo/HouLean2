import HouLean.Vex.Compiler.Grammar
import HouLean.TypeTag
import Qq

open Lean Parser Category

namespace HouLean.Vex.Compiler

variable {m} [Monad m]

private def _root_.Lean.Syntax.visitKind (s : Syntax) (kind : SyntaxNodeKind) (visitor : Syntax → m Unit) : m Unit := 
  match s with
  | .node _ kind' args => do
    if kind' == kind then
      visitor s
    args.foldlM (init:=()) (fun _ a => a.visitKind kind visitor)
  | _ => pure ()

structure BoundVariable where
  name : String
  ident : Ident
  isExport : Bool := false
  type? : Option AttribTypeTag
deriving Repr

namespace CollectBoundVars

abbrev State := Std.HashMap String BoundVariable
abbrev CollectM := StateT State CoreM


def addVar (ref : Syntax) (ident : Ident) (type? : Option AttribTypeTag) (isExport := false) : CollectM Unit := do
  let .str _ name := ident.getId.eraseMacroScopes
    | throwErrorAt ref "Invalid name '{ident}' of a bound variable!"

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
        throwErrorAt ref "Invalid type annotation '{t'.typeAnnotation}@{name}', \
                          previously annotated with '{t.typeAnnotation}@{name}'!"
    var := {var with type? := t?}

    -- update export
    var := {var with isExport := var.isExport || isExport}

    set (s.insert name var)
  else
    set (s.insert name {
      name := name
      ident := ident
      type? := type?
      isExport := isExport
    })
  
def knownTypes : Std.HashMap String AttribTypeTag :=
  Std.HashMap.emptyWithCapacity 10
  |>.insert "P" .vector3
  |>.insert "accel" .vector3
  |>.insert "Cd" .vector3
  |>.insert "N" .vector3
  |>.insert "scale" .vector3
  |>.insert "force" .vector3
  |>.insert "rest" .vector3
  |>.insert "torque" .vector3
  |>.insert "up" .vector3
  |>.insert "uv" .vector3
  |>.insert "v" .vector3
  |>.insert "center" .vector3
  |>.insert "dPdx" .vector3
  |>.insert "dPdy" .vector3
  |>.insert "dPdz" .vector3
  |>.insert "backtract" .vector4
  |>.insert "orient" .vector4
  |>.insert "pstate" .vector4
  |>.insert "id" .int
  |>.insert "nextid" .int
  |>.insert "pstate" .int
  |>.insert "elemnum" .int
  |>.insert "ptnum" .int
  |>.insert "primnum" .int
  |>.insert "vtxnum" .int
  |>.insert "numelem" .int
  |>.insert "numpt" .int
  |>.insert "numpt" .int
  |>.insert "numprim" .int
  |>.insert "numvtx" .int
  --|>.insert "group_*" .int
  |>.insert "ix" .int
  |>.insert "iy" .int
  |>.insert "iz" .int
  |>.insert "resx" .int
  |>.insert "resy" .int
  |>.insert "resz" .int
  |>.insert "name" .string
  |>.insert "instance" .string

/-- Go over collected bounda variables and update types based on known attribute names.

If known attribute name has been annotated with different type, we keep that type
and do not throw any error or produce any warning. -/
def fixKnownTypes : CollectM Unit := do
  let s ← get
  set (s.map (fun name var => 
    let t? := 
      match var.type? with
      | some t => some t
      | none => knownTypes[name]?
    { var with type? := t? }))

end CollectBoundVars


open CollectBoundVars in
def collectBoundVars (stx : TSyntax `vexSnippet) : CoreM (Std.HashMap String BoundVariable) := do

  let go : CollectM Unit := do
    stx.raw.visitKind ``attrAccess fun s => do
      match s with
      -- string, integer, float
      | `(attrAccess| s@$x:ident)   => addVar s x (some .string)
      | `(attrAccess| s[]@$x:ident) => addVar s x (some .stringArray)
      | `(attrAccess| i@$x:ident)   => addVar s x (some .int)
      | `(attrAccess| i[]@$x:ident) => addVar s x (some .intArray)
      | `(attrAccess| f@$x:ident)   => addVar s x (some .float)
      | `(attrAccess| f[]@$x:ident) => addVar s x (some .stringArray)

      -- vectors
      | `(attrAccess| u@$x:ident)   => addVar s x (some .vector2)
      | `(attrAccess| u[]@$x:ident) => addVar s x (some .vector2Array)
      | `(attrAccess| v@$x:ident)   => addVar s x (some .vector3)
      | `(attrAccess| v[]@$x:ident) => addVar s x (some .vector3Array)
      | `(attrAccess| p@$x:ident)   => addVar s x (some .vector4)
      | `(attrAccess| p[]@$x:ident) => addVar s x (some .vector4Array)

      -- matrices
      | `(attrAccess| m2@$x:ident)   => addVar s x (some .matrix2)
      | `(attrAccess| m2[]@$x:ident) => addVar s x (some .matrix2Array)
      | `(attrAccess| m3@$x:ident)   => addVar s x (some .matrix3)
      | `(attrAccess| m3[]@$x:ident) => addVar s x (some .matrix3Array)
      | `(attrAccess| m4@$x:ident)   => addVar s x (some .matrix4)
      | `(attrAccess| m4[]@$x:ident) => addVar s x (some .matrix4Array)

      | `(attrAccess| d@$x:ident)   => addVar s x (some .dict)
      | `(attrAccess| d[]@$x:ident) => addVar s x (some .dictArray)

      -- unknown type
      | `(attrAccess| @$x:ident) => addVar s x none

      | _ => throwError "Unhandled attribute type case!"

  return (← go {}).2

open Qq 
run_meta
  let snippet ← `(vexSnippet|
    int closept = nearpoint(1, @P);
    vector value = point(1, "P", closept);
    @dist = length(@P - value);
    @Cd = set(@dist, 0, 0);)

  let vars ← collectBoundVars snippet

  let vars := vars.values

  IO.println (repr vars)
  
