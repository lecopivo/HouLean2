import HouLean.TypeTag
import HouLean.Vex.Compiler.Grammar

open Lean Parser Category

namespace HouLean.Vex.Compiler

/-- Data for a bound variable like `v@P`, `@Cd`, `@name`, ... -/
structure BoundVariable where
  /-- Name of the identifier of a bound variable.
  Potentially containing macro scopes, namespaces etc. -/
  name : Name
  /-- String name that should be used when passing VEX code to VEX compiler.  -/
  strName : String
  /-- Is output/export variable i.e. it appeared on lhs of an assignment.  -/
  isExport : Bool := false
  /-- Type of the variable, infered from type annotation like `v@`, `s[]@`, ...  -/
  type? : Option AttribTypeTag
deriving Repr

def BoundVariable.toCVexParam (var : BoundVariable) : MacroM (TSyntax ``cvexParam) := do
  let id := mkIdent (Name.mkSimple var.strName)
  -- using array is a bit messy here, but I couldn't figure out how to use Option
  let ex : TSyntax `vexDeclSpec ← `(vexDeclSpec| export)
  let export? : TSyntaxArray `vexDeclSpec := if var.isExport then #[ex] else #[]
  match var.type?.getD .float with
  | .float        => `(cvexParam| $export?:vexDeclSpec* float $id:ident = 0)
  | .floatArray   => `(cvexParam| $export?:vexDeclSpec* float $id:ident[] = {})
  | .int          => `(cvexParam| $export?:vexDeclSpec* int $id:ident = 0)
  | .intArray     => `(cvexParam| $export?:vexDeclSpec* int $id:ident[] = {})
  | .string       => `(cvexParam| $export?:vexDeclSpec* string $id:ident = "")
  | .stringArray  => `(cvexParam| $export?:vexDeclSpec* string $id:ident[] = {})
  | .vector2      => `(cvexParam| $export?:vexDeclSpec* vector2 $id:ident = {0, 0})
  | .vector2Array => `(cvexParam| $export?:vexDeclSpec* vector2 $id:ident[] = {})
  | .vector3      => `(cvexParam| $export?:vexDeclSpec* vector $id:ident = {0, 0, 0})
  | .vector3Array => `(cvexParam| $export?:vexDeclSpec* vector $id:ident[] = {})
  | .vector4      => `(cvexParam| $export?:vexDeclSpec* vector4 $id:ident = {0, 0, 0, 0})
  | .vector4Array => `(cvexParam| $export?:vexDeclSpec* vector4 $id:ident[] = {})
  | .matrix2      => `(cvexParam| $export?:vexDeclSpec* matrix2 $id:ident = {})
  | .matrix2Array => `(cvexParam| $export?:vexDeclSpec* matrix2 $id:ident[] = {})
  | .matrix3      => `(cvexParam| $export?:vexDeclSpec* matrix3 $id:ident = {})
  | .matrix3Array => `(cvexParam| $export?:vexDeclSpec* matrix3 $id:ident[] = {})
  | .matrix4      => `(cvexParam| $export?:vexDeclSpec* matrix $id:ident = {})
  | .matrix4Array => `(cvexParam| $export?:vexDeclSpec* matrix $id:ident[] = {})
  | .dict         => `(cvexParam| $export?:vexDeclSpec* dict $id:ident = {})
  | .dictArray    => `(cvexParam| $export?:vexDeclSpec* dict $id:ident[] = {})


abbrev BoundVariables := Std.HashMap Name BoundVariable

def BoundVariables.knownTypes : Std.HashMap String AttribTypeTag :=
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


/-- Fix types for known variables names like "Cd", "P", "name", ..

If known attribute has been annotated with different type, we keep that type
and do not throw any error or produce any warning. -/
def BoundVariables.fixKnownTypes (vars : BoundVariables) :  BoundVariables :=
  vars.map (fun _ var =>
    let t? :=
      match var.type? with
      | some t => some t
      | none => knownTypes[var.strName]?
    { var with type? := t? })


/-- Make the value of `BoundVariable.strName` a uniqe string name such that they do not clash with
existing identifiers in `snippet`.

TODO: provide proper implementation of this function! -/
def BoundVariables.mkUniqueStrNames (vars : BoundVariables) (snippet : TSyntax ``vexSnippet) :
    BoundVariables :=
  vars.map (fun _ var => {var with strName := "__bound_var_" ++ var.strName})

def BoundVariables.mkCVexParamList (vars : BoundVariables) : MacroM (TSyntax ``cvexParamList) := do
  let params ← vars.valuesArray.mapM (·.toCVexParam)
  return Lean.mkNode ``cvexParamList #[Syntax.mkSep params (Lean.mkAtom ";")]
