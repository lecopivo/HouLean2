import HouLean.LeanGraph.LeanGraph

open Lean Meta Std

namespace HouLean.LeanGraph

inductive SingleExtension where
  | nodeType (name : Name) (t : NodeType)
  | portType (name : Name) (t : PortType)
deriving Inhabited

structure Extension where
  nodeTypes : NameMap NodeType 
  portTypes : NameMap PortType 
deriving Inhabited

abbrev LeanGraphExt := SimpleScopedEnvExtension SingleExtension Extension

initialize leanGraphExt : LeanGraphExt  ←
  registerSimpleScopedEnvExtension {
    name := by exact decl_name%
    initial := default
    addEntry := fun es e =>
      match e with
      | .nodeType n t =>
        {es with nodeTypes := es.nodeTypes.insert n t}
      | .portType n t =>
        {es with portTypes := es.portTypes.insert n t}
  }

-- todo: make this use env extension
def isKnownPortType (name : Name) : CoreM Bool := do
  let s := leanGraphExt.getState (← getEnv) 
  return s.portTypes.contains name
  
def nameToHoudiniString (name : Name) : String :=
  let s := name.toString.replace "." "_"
  s

def explicitArgsOfConstant (decl : Name) : MetaM (Array Nat) := do
  let info ← getConstInfo decl

  forallTelescope info.type fun xs _ => do
    let mut ids : Array Nat := Array.emptyWithCapacity xs.size
    for x in xs, i in [0:xs.size] do
      if (← x.fvarId!.getBinderInfo).isExplicit then
        ids := ids.push i
    return ids

def PortType.setName (t : PortType) (n : String) : PortType :=
  match t with
  | .builtin _ tn => .builtin n tn
  | .struct _ tn xs => .struct n tn xs


partial def mkPortType (t : Expr) (forceBuiltin := false) (userName? : Option String := none) : MetaM PortType := do
  unless (← whnf (← inferType t)).isSort do
    throwError m!"Can't produce port type for {t}, not a type!"
  if t.isMVar then
    return .builtin (userName?.getD "output") "?_" -- only output ports do not have userName? provided

  let (fn, args) := (← whnfR t).getAppFnArgs

  if fn.isAnonymous then
    throwError m!"Invalid expression {t} to get port type of!"

  let name := userName?.getD fn.getString!.toLower
  let typeName ← withOptions (fun opt => opt.setBool `pp.mvars false) do ppExpr t
  let typeName := toString typeName
  
  if ¬(isStructure (← getEnv) fn) || (← isKnownPortType fn) || forceBuiltin then
    return .builtin name typeName
  else
    let strInfo := getStructureInfo (← getEnv) fn
    
    let mut subports : Array PortType := #[]
    for field in strInfo.fieldNames do
      let some info := getFieldInfo? (← getEnv) fn field 
        | throwError m!"bug in {decl_name%}, can't get field info" 

      let name := info.fieldName
      let projType ← inferType (← mkAppOptM info.projFn (args.map some))
      let some (_,type) := projType.arrow?
        | throwError m!"bug in {decl_name%}, invalid projection type {projType}" 
      let portType ← mkPortType type false
      subports := subports.push (portType.setName name.toString)
    return .struct name typeName subports


def mkNodeType (e : Expr) (customName? : Option String := none) : MetaM NodeType := do
  let (fn, args) := e.getAppFnArgs
  let info ← getFunInfo (← mkConstWithFreshMVarLevels fn)

  unless args.size == info.getArity do
    throwError m!"In expression:\n  {e}\nmkNodeType currently assumes that the number of applied arguments, {args.size}, is equal to the arity of {fn}, {info.getArity}!"
  
  let cinfo ← getConstInfo fn

  -- get names of all arguments
  forallTelescope cinfo.type fun ys _ => do
  let names ← ys.mapM (fun y => y.fvarId!.getUserName)
  let names := names.map (fun n => n.eraseMacroScopes)

  -- filter explicit arguments
  let xs  := info.paramInfo.zip (args.zip names)
    |>.filterMap (fun (i,(arg,name)) => if i.isExplicit then some (arg,name) else none)

  let inputPortTypes ← xs.mapM (fun (x,name) => inferType x >>= (mkPortType · false name.toString))
  let outputPortType ← mkPortType (← inferType e)

  return {
    name := customName?.getD (nameToHoudiniString fn)
    leanConstant := fn
    inputs := inputPortTypes
    outputs := #[outputPortType]
  }

initialize registerTraceClass `HouLean.lean_graph

syntax (name:=lean_graph_type) "lean_graph_type" : attr
 
initialize leanGraphTypeAttr : Unit ←
  registerBuiltinAttribute {
    name  := `lean_graph_type
    descr := ""
    applicationTime := AttributeApplicationTime.afterCompilation
    add   := fun declName stx attrKind =>
       discard <| MetaM.run do
         let e ← mkConstWithFreshMVarLevels declName
         let (xs,_,r) ← forallMetaTelescope (← inferType e)
         let t ← mkPortType (e.beta xs)
         trace[HouLean.lean_graph] m!"New port type registered\n{t}"
         leanGraphExt.add (.portType declName t) attrKind
    erase := fun _declName =>
      throwError "Can't remove `apex_type`, not implemented yet!"
  }

syntax (name:=lean_graph_type_builtin) "lean_graph_type_builtin" : attr
 
initialize leanGraphTypeBuiltinAttr : Unit ←
  registerBuiltinAttribute {
    name  := `lean_graph_type_builtin
    descr := ""
    applicationTime := AttributeApplicationTime.afterCompilation
    add   := fun declName stx attrKind =>
       discard <| MetaM.run do
         let e ← mkConstWithFreshMVarLevels declName
         let (xs,_,r) ← forallMetaTelescope (← inferType e)
         let t ← mkPortType (e.beta xs) true
         trace[HouLean.lean_graph] m!"New port type registered\n{t}"
         leanGraphExt.add (.portType declName t) attrKind
    erase := fun _declName =>
      throwError "Can't remove `apex_type`, not implemented yet!"
  }


syntax (name:=lean_graph_node) "lean_graph_node" (str)? : attr
 
initialize leanGraphNodeAttr : Unit ←
  registerBuiltinAttribute {
    name  := `lean_graph_node
    descr := ""
    applicationTime := AttributeApplicationTime.afterCompilation
    add   := fun declName stx attrKind =>
       discard <| MetaM.run do
         let customName? := stx[1].getOptional?.map (fun s => s.isStrLit?) |>.join
         let e ← mkConstWithFreshMVarLevels declName
         let (xs,_,r) ← forallMetaTelescope (← inferType e)
         let t ← mkNodeType (e.beta xs) customName?
         trace[HouLean.lean_graph] m!"New node type registered\n{t}"
         leanGraphExt.add (.nodeType declName t) attrKind
    erase := fun _declName =>
      throwError "Can't remove `apex_type`, not implemented yet!"
  }

structure GraphTypes where
  nodeTypes : Array NodeType
  portTypes : Array PortType
deriving ToJson, FromJson

def getGraphTypes : CoreM GraphTypes := do
  let s := leanGraphExt.getState (← getEnv) 
  let a := s.nodeTypes.toArray.map (fun (_,t) => t)
  let b := s.portTypes.toArray.map (fun (_,t) => t)
  return {
    nodeTypes := a
    portTypes := b
  }

end LeanGraph

open Elab Term Command LeanGraph in
scoped elab "#port_type" x:term : command => do
  liftTermElabM <| do
  let t ← elabTerm x none
  let t ← mkPortType t
  logInfo m!"{t}"


-- syntax (name:=apex_type) "lean_graph_node" str : attr


-- def mkNodeTypeForDecl (decl : Name) : MetaM NodeType := do
--   let info ← getConstInfo decl
--   let (xs,_,retType) ← forallMetaTelescope info.type


--   return {
--     name := nameToHoudiniString decl
--     leanConstant := decl
--     inputs := #[]
--     outputs := #[]
--   }


-- open Qq 
-- run_meta 
--   let e := q((1,2))
--   let n ← mkNodeType e
--   logInfo (toString n)


-- open Qq 
-- run_meta
--   let e := q(Prod.mk 1 (Prod.mk 2.0 3.0))
--   let n ← mkNodeType e
--   logInfo (toString n)


-- open Lean Meta
-- run_meta 
--   let e ← mkFreshExprMVar (some (Expr.const ``Nat []))
--   let s ← withOptions (fun opt => opt.set ``pp.mvars false) do ppExpr e
--   IO.println s
