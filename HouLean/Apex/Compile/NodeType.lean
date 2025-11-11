import HouLean.Apex.Compile.ExprType
import HouLean.Meta.AnonymousStruct

namespace HouLean.Apex.Compiler

open Lean Meta

partial def localPortsFromType (type : Expr) (name : Name) (off : Nat) (dir : PortDir) : MetaM (ArrayTree LocalPort) := do
  
  let type ← whnfR type
  let (fn, args) := type.getAppFnArgs

  -- is builtin type?
  if let some t := ApexTypeTag.fromName fn then
    return .leaf { 
      localId := off
      name := name
      type := .builtin t
      dir := dir
    }

  -- is VariadicArg
  if type.isAppOfArity ``VariadicArg' 1 then
    return .leaf {
      localId := off
      name := name
      type := .variadic none
      dir := dir
    }

  -- is variadic of builtin type?
  if fn == ``VariadicArg ∧ args.size == 2 then
    let (.const elemType _) ← whnfR (type.getArg! 0) 
      | throwError m!"invalid variadic type {type}"
    let type : PortType ← 
      match ApexTypeTag.fromName elemType with
      | some t => pure (PortType.variadic t)
      | none => 
        if elemType == ``HouLean.Apex.Untyped then
          pure (.variadic none)
        else
          throwError m!"invalid variadic type {type}"
    return .leaf {
      localId := off
      name := name
      type := type
      dir := dir
    }

  if isStructure (← getEnv) fn then
    let info := getStructureInfo (← getEnv) fn
    let mut ports : Array (ArrayTree LocalPort) := #[]
    let mut off := off
    for fieldName in info.fieldNames, i in [0:info.fieldNames.size] do
      let fieldType ← withLocalDeclD `s type fun s => do pure (← inferType (.proj fn i s))
      let port ← localPortsFromType fieldType (name.append fieldName) off dir
      off := off + port.leafNum
      ports := ports.push port
    return .node ports
  else
    throwError m!"Invalid port type {type}!"


/-- Makes APEX node type from Lean function. 

This is used for buildin APEX nodes to quickly initialize their Lean equivalent. -/
def mkNodeTypeFromLeanFn (decl : Name) (apexNodeName : String) (hasRunData := false) : MetaM NodeType := do

  let info ← getConstInfo decl
  forallTelescope info.type fun xs r => do

    -- filter only explicit arguments
    let xs ← xs.filterM (fun x => do pure ((← x.fvarId!.getBinderInfo) == .default))

    let mut off := 0 

    if hasRunData then 
        off := 1

    let mut inputs : Array (ArrayTree LocalPort) := #[]

    for x in xs do
      let name ← x.fvarId!.getUserName
      let xType ← inferType x
      let ports ← localPortsFromType xType name off .input
      off := off + ports.leafNum
      inputs := inputs.push ports
    
    let output ← localPortsFromType r .anonymous off .output

    -- right now all nodes with non-trivial variadic port groups are hand crafted
    let ports := (inputs.map (fun input => input.flatten)).flatten ++ output.flatten

    -- todo: for now lets assume that there is only one variadic group
    --       I don't have and an example of multiple variadic groups anyway
    let variadicPortGroups := ports.filterMap (fun p => if p.type.isVariadic then some #[p.localId] else none)
    let variadicPortGroups := if variadicPortGroups.size = 0 then #[] else #[variadicPortGroups.flatten]
    
    let type : NodeType := {
      name := apexNodeName
      leanDecl := decl
      hasRunData := hasRunData
      inputs := inputs
      output := output
      variadicPortGroups := variadicPortGroups
    }
    
    trace[HouLean.Apex.compiler] "{repr type}"
    return type


/-- Return `NodeType` corresponding to the given Lean function. -/
def getNodeType (fn : Expr) : MetaM (Option NodeType) := do
  let s := (compilerExt.getState (← getEnv)).nodeTypes
  return s[fn]?

def addNodeType (fn : Expr) (nodeType : NodeType) : CoreM Unit :=
  compilerExt.add (.nodeType fn nodeType)


----------------------------------------------------------------------------------------------------

syntax (name:=apex_node) "apex_node" str ("has_rundata")? : attr

initialize apexNodeAttr : Unit ←
  registerBuiltinAttribute {
    name  := `apex_node
    descr := "Mark Lean function as builin APEX function."
    applicationTime := AttributeApplicationTime.afterCompilation
    add   := fun declName stx attrKind =>
      match stx with
      | `(apex_node| apex_node $name $[has_rundata]?) => discard <| MetaM.run do
        let n := name.getString
        let rundata := stx[2].getOptional?.isSome
        let nodeType ← mkNodeTypeFromLeanFn declName n rundata
        addNodeType (.const declName []) nodeType
      | _ => Elab.throwUnsupportedSyntax
    erase := fun _declName =>
      throwError "Can't remove `apex_node`, not implemented yet!"
  }
