import HouLean.Apex.Compile.ExprType
import HouLean.Apex.Compile.Meta

namespace HouLean.Apex.Compiler

open Lean Meta


/-- Makes APEX node type from Lean function. 

This is used for buildin APEX nodes to quickly initialize their Lean equivalent. -/
def mkNodeTypeFromLeanFn (fn : Expr) (apexNodeName : String) (outNames : Array String := #[]) (hasRunData := false) : MetaM NodeType := do

  forallTelescope (← inferType fn) fun xs r => do

    -- filter only explicit arguments
    let xs ← xs.filterM (fun x => do pure ((← x.fvarId!.getBinderInfo) == .default))

    let mut ports : Array LocalPort := #[]
    let mut off := 0 

    if hasRunData then 
        ports := ports.push { localId := off, name := "rundata", type := .rundata, dir := .output }
        off := 1

    for x in xs do
      let n := (← x.fvarId!.getUserName).toString
      let xType ← inferType x
      let t? ← getApexType? xType
      
      match t? with
      | some (.builtin typeName) => 
        ports := ports.push { localId := off, name := n, type := .builtin typeName, dir := .input }
        off := off + 1
      | some (.variadic elemTypeName _) => 
        ports := ports.push { localId := off, name := n, type := .variadic elemTypeName, dir := .input }
        off := off + 1
      | _ => 
        throwError m!"Input argument {x} has invalid type {xType}"

    let yTypes := splitProdType r |>.toArray

    let outNames := 
      if outNames.size == yTypes.size then
        outNames
      else
        Array.range yTypes.size |>.map (fun i => s!"out{i}")

    for yType in yTypes, n in outNames do
      let t? ← getApexType? yType
      
      match t? with
      | some (.builtin typeName) => 
        ports := ports.push { localId := off, name := n, type := .builtin typeName, dir := .output }
        off := off + 1
      | some (.variadic ..) => 
        throwError m!"Variadic type, {yType}, in the output is not currently supported!"
      | _ => 
        throwError m!"Invalid ouput type {yType}"
    
    return {
      name := apexNodeName
      ports := ports
    }


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
        let nodeType ← mkNodeTypeFromLeanFn (.const declName []) n #[] rundata
        addNodeType (.const declName []) nodeType
      | _ => Elab.throwUnsupportedSyntax
    erase := fun _declName =>
      throwError "Can't remove `apex_node`, not implemented yet!"
  }
