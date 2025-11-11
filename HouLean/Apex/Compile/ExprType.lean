import HouLean.Apex.Compile.Extension
import HouLean.Meta.Basic
import HouLean.Meta.AnonymousStruct

namespace HouLean

open Lean Meta
  
namespace Apex.Compiler

open Qq
unsafe def getApexTypeTagListImpl (ts : Expr) : MetaM (List ApexTypeTag) := do
  evalExpr (List ApexTypeTag) q(List ApexTypeTag) ts

@[implemented_by getApexTypeTagListImpl]
def getApexTypeTagList (ts : Expr) : MetaM (List ApexTypeTag) := return []

partial def getApexTypeCore (type : Expr) (name : Name) : MetaM ApexStruct := do 

  let type ← whnf type
  let (fn, _) := type.getAppFnArgs

  -- is builtin type?
  if let some t := ApexTypeTag.fromName fn then
    return (.leaf (name, t))

  -- is VariadicArg
  if type.isAppOfArity ``VariadicArg 2 then
    let some t := (type.getArg! 0).constName?
      | throwError "Invalid variadic type {type}! Mut be of a built in type!"
    let some t := ApexTypeTag.fromName t
      | throwError "Invalid variadic type {type}! Not of a builtin type!"
    let some n := (← whnf (type.getArg! 1)).rawNatLit?
      | throwError "Invalid variadic type {type}! Size must be known at compile time!"
    let xs := Array.range n |>.map (fun i => ArrayTree.leaf (name.appendAfter (toString i), t))
    return .node xs

  -- is VariadicArg
  if type.isAppOfArity ``VariadicArg' 1 then
    let ts := (← getApexTypeTagList (type.getArg! 0)).toArray
    let xs := ts.zip (Array.range (ts.size)) |>.map (fun (t,i) => ArrayTree.leaf (name.appendAfter (toString i), t))    
    return .node xs

  if isStructure (← getEnv) fn then
    let info := getStructureInfo (← getEnv) fn
    let mut fields : Array ApexStruct := #[]
    for fieldName in info.fieldNames, i in [0:info.fieldNames.size] do
      let fieldType ← withLocalDeclD `s type fun s => do pure (← inferType (.proj fn i s))
      let field ← getApexTypeCore fieldType (name.append fieldName)
      fields := fields.push field
    return .node fields
  else
    throwError m!"Invalid APEX type {type}!"


partial def getApexType? (type : Expr) (name := Name.anonymous) : MetaM (Option ApexStruct) := do
  let mut type := type

  -- try replacing type with type' synthesizing `HouLean.ApexType 
  let type' ← mkFreshTypeMVar
  let cls := mkApp2 (← mkConstWithFreshMVarLevels ``HouLean.ApexType) type type'
  if let some _ ← synthInstance? cls then
    type ← instantiateMVars type'

  try
    getApexTypeCore type name
  catch e =>
    logError e.toMessageData
    return none


-- /-- Is `type` structure *and* not compiler supported type.

-- Type like `Array String` is a structure by at some point we will treat it as an 
-- atomic type and used it instead of `StringArray`.
-- -/
-- def isStructureType (type : Expr) : MetaM Bool := do
--   let s := compilerExt.getState (← getEnv)
--   let .const fn _ := type.getAppFn | return false
--   -- Apex compiler supported type
--   if s.apexTypes.contains type then
--     return false
--   return isStructure (← getEnv) fn
  

-- /-- Tries to determine size of all variadic types. If fails it returns the `e : Expr` that
-- was not possible to turn into `Nat` literal. -/
-- def enforceStaticSize (type : ApexType) : MetaM (Except Expr ApexStaticType) := 
--   match type with
--   | .builtin n => return .ok (.leaf ("x", n))
--   | .struct t => return .ok t
--   | .variadic name n => do
--     let .lit (Literal.natVal n) ← whnfD n
--       | return .error n
--     return .ok (.node (Array.range n |>.map (fun i =>  (.leaf (s!"x{i}",name)))))
-- run_meta


-- def addBuiltinApexType (type : Expr) (typeTag : ApexTypeTag) : MetaM Unit := do
--   try
--     unless (← inferType type).isSort do throwError ""
--     compilerExt.add (.apexBuiltinType type typeTag)
--   catch e =>
--     throwError m!"Can't register {type} as APEX builtin type!\n{e.toMessageData}"


-- syntax (name:=apex_builtin_type) "apex_builtin_type" term : attr
 
-- open Lean Elab Term in
-- initialize apexTypeAttr : Unit ←
--   registerBuiltinAttribute {
--     name  := `apex_builtin_type
--     descr := "Mark Lean type as builin APEX type."
--     applicationTime := AttributeApplicationTime.afterCompilation
--     add   := fun declName stx attrKind =>
--       match stx with
--       | `(apex_builtin_type| apex_builtin_type $name) => do
--        discard <| TermElabM.run do
--          addBuiltinApexType (← mkConstWithFreshMVarLevels declName) name.getString
--       | _ => Elab.throwUnsupportedSyntax
--     erase := fun _declName =>
--       throwError "Can't remove `apex_type`, not implemented yet!"
--   }

open Lean Elab Term Command in
/-- Check APEX type of Lean type -/
elab "#apex_type" x:term : command => do
  liftTermElabM do
    let x ← elabTerm x none
    let some t ← getApexType? x default
      | logError "{x} is not an APEX type"
    let t := t.mapIdx (fun _ (_,t) => t)
    logInfo m!"{t.toString}"
