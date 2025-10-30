import HouLean.Apex.Compile.Extension

open Lean Meta

namespace HouLean.Apex.Compiler

partial def getApexType? (type : Expr) : MetaM (Option ApexType) := do 

  let type ← whnfR type
  let (fn, args) := type.getAppFnArgs

  let m := (compilerExt.getState (← getEnv)).apexTypes

  -- is builtin type?
  if let some t := m[type]? then
    return t

  -- is variadic builtin type?
  if fn == ``VariadicArg ∧ args.size == 2 then
    let elemType ← whnfR (type.getArg! 0)
    let n ← whnfD (type.getArg! 1) -- reduce as much as possible
    -- special case for "VariadicArg<void>"
    if elemType == .const `HouLean.Apex.Untyped [] then
      return some (.variadic "void" n)

    let some (.builtin elemTypeName) := m[elemType]? 
      | return none
    return some (.variadic elemTypeName n)

  if (← inferType type).isProp then
    return some (.struct (.node #[]))

  -- implemented by
  let s := compilerExt.getState (← getEnv)
  if let some fn' := s.implementedByName.find? fn then
    try
      -- type constructores are assumet to have the same arguments
      let type' ← mkAppOptM fn' (args.map some)
      return ← getApexType? type'
    catch _ =>
      throwError m!"Failed replacing {fn} with {fn'} in {type}"

  
  -- Handle structure types
  let mut fields : Array (ArrayTree (String×TypeName)) := #[]
  if isStructure (← getEnv) fn then
    let info := getStructureInfo (← getEnv) fn

    for n in info.fieldNames do
      let some info := getFieldInfo? (← getEnv) fn n | return none

      let projFunType ← inferType (← mkAppOptM info.projFn (args.map some))
      let .some (_,t) := projFunType.arrow? | return none

      match ← getApexType? t with
      | .some (.builtin typeName) =>
        fields := fields.push (.leaf (info.fieldName.toString, typeName))
      | .some (.struct s) => 
        fields := fields.push (s.mapIdx (fun _ (fn, tn) => (info.fieldName.eraseMacroScopes.toString ++ "_" ++ fn, tn)))
      | _ => return none

    return some (.struct (.node fields))

  return none

/-- Is `type` structure *and* not compiler supported type.

Type like `Array String` is a structure by at some point we will treat it as an 
atomic type and used it instead of `StringArray`.
-/
def isStructureType (type : Expr) : MetaM Bool := do
  let s := compilerExt.getState (← getEnv)

  -- Apex compiler supported type
  if s.apexTypes.contains type then
    return false

  let .const fn _ := type.getAppFn | return false
  return isStructure (← getEnv) fn
  

/-- Tries to determine size of all variadic types. If fails it returns the `e : Expr` that
was not possible to turn into `Nat` literal. -/
def enforceStaticSize (type : ApexType) : MetaM (Except Expr ApexStaticType) := 
  match type with
  | .builtin n => return .ok (.leaf ("x", n))
  | .struct t => return .ok t
  | .variadic name n => do
    let .lit (Literal.natVal n) ← whnfD n
      | return .error n
    return .ok (.node (Array.range n |>.map (fun i =>  (.leaf (s!"x{i}",name)))))


def addBuiltinApexType (type : Expr) (apexName : String) : MetaM Unit := do
  try
    unless ← isDefEq (← inferType type) (.sort 1) do throwError ""
    compilerExt.add (.apexType type (.builtin apexName))
  catch _ =>
    throwError m!"Can't register {type} as APEX builtin type!"


syntax (name:=apex_type) "apex_type" str : attr
 
initialize apexTypeAttr : Unit ←
  registerBuiltinAttribute {
    name  := `apex_type
    descr := "Mark Lean type as builin APEX type."
    applicationTime := AttributeApplicationTime.afterCompilation
    add   := fun declName stx attrKind =>
      match stx with
      | `(apex_type| apex_type $name) => do
       discard <| MetaM.run do
         addBuiltinApexType (.const declName []) name.getString
      | _ => Elab.throwUnsupportedSyntax
    erase := fun _declName =>
      throwError "Can't remove `apex_type`, not implemented yet!"
  }
