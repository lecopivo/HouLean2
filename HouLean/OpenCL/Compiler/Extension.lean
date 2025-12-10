import Lean
import HouLean.OpenCL.Basic
import HouLean.Meta.AnonymousStruct

open Lean Meta

namespace HouLean.OpenCL.Compiler

initialize registerTraceClass `HouLean.OpenCL.compiler

-- mutual

-- inductive OpenCLType where
--   /-- Atomic openCL type such as `float`, `float3`, `int` etc.

--   `shortName` is is used for name mangling e.g. for `float` we have short name `f`, for `int` we have `i` etc.
--   thus for example addition function will become `addfff` or `addiii`,
--   scalar multiplication of a vector would be `mulff3f3`
--   dot product could be `mulf3f3f` -/
--   | atom (name : String) (shortName : String)
--   | struct (s : Struct)
-- deriving BEq, Inhabited

-- structure StructField where
--   type : OpenCLType
--   name : String
-- deriving BEq, Inhabited

-- structure Struct where
--   name : String
--   shortName : String
--   fields : Array StructField
-- deriving BEq, Inhabited

-- end

-- def OpenCLType.name (t : OpenCLType) : String :=
--   match t with
--   | .atom n _ => n
--   | .struct { name := n, .. } => n

-- def OpenCLType.shortName (t : OpenCLType) : String :=
--   match t with
--   | .atom _ n => n
--   | .struct { shortName := n, .. } => n

-- -- todo: decide what variants are actually needed
-- inductive FunctionArgumentKind where
--   | input  -- e.g. const float x
--   | output  -- e.g. float * x
--   | ref  -- e.g. const float * x
-- deriving BEq, Inhabited

-- structure OpenCLFunction.Argument where
--   type : OpenCLType
--   name : String
--   kind : FunctionArgumentKind
-- deriving BEq, Inhabited

-- inductive OpenCLFunction.Kind where
--   | normal
--   | infix
--   | prefix
--   | postfix
--   | constructor
-- deriving BEq, Inhabited

-- inductive OpenCLFunction.Body where
--   | builtin
--   | code (s : String)
-- deriving BEq, Inhabited

-- open OpenCLFunction in
-- structure OpenCLFunction where
--   args : Array Argument
--   name : String
--   kind : Kind
--   returnType : OpenCLType
--   body : Body
-- deriving BEq, Inhabited


-- -- key; @add Float inst ?_ ?_  ->

-- inductive SingleExtension where
--   | type (keys : Array DiscrTree.Key) (oclType : OpenCLType)
--   /-- During compilation we replace `src` with `trg`.  -/
--   | implementedBy (src : Name) (trg : Name) (argMap : Array (Option Nat))
--   | function (keys : Array DiscrTree.Key) (oclFun : OpenCLFunction)
-- deriving Inhabited


-- /-- Enviroment extension that holds all necessary information for the APEX compiler. -/
-- structure Extension where
--   /-- Mapping from Lean types to OpenCL types -/
--   oclTypes : DiscrTree OpenCLType
--   /--  -/
--   implementedBy : NameMap (Name × Array (Option Nat))

--   oclFunctions : DiscrTree OpenCLFunction
-- deriving Inhabited



-- abbrev CompilerExt := SimpleScopedEnvExtension SingleExtension Extension


-- initialize compilerExt : CompilerExt ←
--   registerSimpleScopedEnvExtension {
--     name := by exact decl_name%
--     initial := default
--     addEntry := fun es e =>
--       match e with
--       | .type keys oclType =>
--         {es with oclTypes := es.oclTypes.insertCore keys oclType}
--       | .implementedBy src trg argMap =>
--         {es with implementedBy := es.implementedBy.insert src (trg, argMap)}
--       | .function keys oclFunc =>
--         {es with oclFunctions := es.oclFunctions.insertCore keys oclFunc}
--   }

private def _root_.Lean.isInstanceProjection (name : Name) : MetaM Bool := do
  unless ← isProjectionFn name do return false
  let some info ← getProjectionFnInfo? name | return false
  unless info.fromClass do return false
  return true

/-- Weak head normal forma for OpenCL compilation. -/
-- def whnfC (e : Expr) : MetaM Expr :=
--   withConfig (fun cfg => {cfg with zeta := false, zetaDelta := false, iota := false}) <|
--     whnfR e
def doUnfold (e : Expr) : MetaM Bool := do
  let .const name _ := e.getAppFn | return false
  if ← isReducible name then return true
  unless ← isInstanceProjection name do return false
  -- let s := compilerExt.getState (← getEnv)
  if name == ``Id then
    return true
  -- if s.implementedByName.contains name then
  --   return false
  else
    return true

def betaThroughLet (e : Expr) : MetaM Expr := do
  let (fn, args) := e.withApp (fun fn args => (fn,args))
  let e' ← letTelescope fn (preserveNondepLet := false) fun xs b =>
    mkLambdaFVars xs (b.beta args)
  return e'

partial def whnfC (e : Expr) : MetaM Expr :=
  withConfig (fun cfg => {cfg with iota := false, zeta := false, zetaDelta := false}) do
    let e' := e
    -- let e' ← letBind e'
    let e' ← whnfHeadPred e' doUnfold
    -- let e' ← letBindMatchDiscrs e' true
    if e.equal e' then
      return ← betaThroughLet e'
    else
      whnfC e'

open Lean Elab Command PrettyPrinter in
def addOpenCLType (type : Expr) (name : String) (shortName : String) (definition? : Option String) : CommandElabM Unit := do

  let name := Syntax.mkStrLit name
  let shortName := Syntax.mkStrLit shortName
  let definition? : Term ←
    match definition? with
    | some d => `(term| $(Syntax.mkStrLit d))
    | none => `(term| none)

  let typeStx ← liftTermElabM <| delab type

  let cmd ← `(command| instance : OpenCLType $typeStx where
    name := $name
    shortName := $shortName
    definition? := $definition?
  )

  elabCommand cmd

/-- Data stored in `OpenCLType α` -/
structure OCLType where
  name : String
  shortName : String
deriving Inhabited

/-- Data stored in `OpenCLFunction f` -/
structure OCLFunction where
  name : String
  kind : OpenCLFunction.FunKind
deriving Inhabited, BEq

open Qq
def getOpenCLType? (type : Expr) (doWhnf := false) :
    MetaM (Option OCLType) := do
  unless ← isTypeCorrect type do return none
  let mut type := type
  if doWhnf then
    type ← whnfC type
  let cls ← mkAppM ``OpenCL.OpenCLType #[type]
  let some inst ← synthInstance? cls | return none

  try
    let name ← unsafe evalExpr String q(String) (← mkAppOptM ``OpenCL.OpenCLType.name #[type, inst])
    let shortName ← unsafe evalExpr String q(String) (← mkAppOptM ``OpenCL.OpenCLType.shortName #[type, inst])
    let _definition? ← unsafe evalExpr (Option String) q(Option String) (← mkAppOptM ``OpenCL.OpenCLType.definition? #[type, inst])
    return .some { name, shortName }
  catch e =>
    throwError e.toMessageData

open Qq
def getOpenCLType (type : Expr) (doWhnf := false) : MetaM OCLType := do
  let some t ← getOpenCLType? type doWhnf
    | throwError m!"Not an OpenCL type {type}!"
  return t

def getOpenCLFunction? (f : Expr) (doWhnf := false) :
    MetaM (Option OCLFunction) := do
  let mut f := f
  if doWhnf then
    f ← forallTelescope (← inferType f) fun xs _ => do
      let body ← whnfC (f.beta xs)
      mkLambdaFVars xs body
  let cls ← mkAppM ``OpenCL.OpenCLFunction #[f]
  let some inst ← synthInstance? cls | return none
  try
    let name ← unsafe evalExpr String q(String) (← mkAppOptM ``OpenCL.OpenCLFunction.name #[none, f, inst])
    let kind ← unsafe evalExpr OpenCLFunction.FunKind q(OpenCLFunction.FunKind) (← mkAppOptM ``OpenCL.OpenCLFunction.kind #[none, f, inst])
    let _definition? ← unsafe evalExpr (Option String) q(Option String) (← mkAppOptM ``OpenCL.OpenCLFunction.definition? #[none, f, inst])
    return .some { name, kind }
  catch e =>
    throwError e.toMessageData


def getOpenCLApp? (e : Expr) (doWhnf := false) :
    MetaM (Option struct { oclFun : OCLFunction,
                           fn : Expr,
                           args : Array Expr }) := do
  let mut e := e
  if doWhnf then
    e ← whnfC e
  let (fn, args) := e.withApp fun fn args => (fn,args)
  let info ← getFunInfo fn
  let firstExplicit := info.paramInfo.findIdx (fun p => p.isExplicit)
  let fn := fn.beta (args[0:firstExplicit])
  let args := args[firstExplicit:].toArray
  let some oclFun ← getOpenCLFunction? fn false | return none

  return some { oclFun, fn, args }


def addOpenCLFunction (f : Expr) (name : String) (kind : OpenCLFunction.FunKind) (definition : String) :
    MetaM Unit := do

  let nameExpr := mkStrLit name
  let kindExpr := toExpr kind
  let defExpr ← mkAppM ``Option.some #[mkStrLit definition]

  let type ← mkAppM ``OpenCL.OpenCLFunction #[f] >>= instantiateMVars
  let val ← mkAppOptM ``OpenCL.OpenCLFunction.mk #[none, f, nameExpr, kindExpr, defExpr] >>= instantiateMVars

  let name := s!"instOpenCLFunction{name.capitalize}".toName

  let decl : Declaration := .defnDecl {
    name := name
    levelParams := []
    type := type
    value := val
    hints := .regular val.approxDepth
    safety := .safe
  }

  addAndCompile decl
  addInstance name .global 1000


-- #opencl_print
-- #opencl_compile

-- def addOCLType (type : Expr) (oclType : OpenCLType) : MetaM Unit := do
--   let keys ← DiscrTree.mkPath type
--   compilerExt.add (.type keys oclType)

-- def getOCLType (type : Expr) : MetaM OpenCLType := do
--   let mut type := type
--   if type.isAppOfArity ``OpenCLM 1 then
--     type := type.appArg!

--   let s := compilerExt.getState (← getEnv)
--   let m ← s.oclTypes.getMatch type
--   unless m.size = 1 do
--     if m.size = 0 then
--       throwError m!"{type} is not a valid OpenCL type!"
--     else
--       throwError m!"{type} has ambiguous OpenCL type! Candidates {m.map (fun x => x.name)}"
--   return m[0]!

-- def getOCLType? (type : Expr) : MetaM (Option OpenCLType) := do
--   let mut type := type
--   if type.isAppOfArity ``OpenCLM 1 then
--     type := type.appArg!

--   let s := compilerExt.getState (← getEnv)
--   let m ← s.oclTypes.getMatch type
--   unless m.size = 1 do
--     return none
--   return m[0]!

def nameToString (n : Name) : String := n.eraseMacroScopes.toString.replace "." "_"

-- open OpenCLFunction in
-- def addOCLFunction (f : Expr) (name : String) (kind := Kind.normal) (body := Body.builtin) : MetaM Unit := do
--   forallTelescope (← inferType f) fun xs _ => do
--     let returnType ← inferType (f.beta xs) >>= getOCLType
--     let argTypes ← xs.mapM inferType >>= (·.mapM getOCLType)
--     let argNames ← xs.mapM (fun x => nameToString <$> x.fvarId!.getUserName)
--     let args : Array Argument :=
--       argTypes.zip argNames |>.map (fun (t,n) => { type := t, name := n, kind := .input})

--     let oclFunction : OpenCLFunction := {
--       args := args
--       name := name
--       kind := kind
--       returnType := returnType
--       body := body
--     }
--     let (xs,_,_) ← forallMetaTelescope (← inferType f)
--     let keys ← DiscrTree.mkPath (f.beta xs)
--     compilerExt.add (.function keys oclFunction)


-- def getOCLFunApp (fx : Expr) : MetaM (OpenCLFunction × Array Expr) := do
--   let s := compilerExt.getState (← getEnv)
--   let m ← s.oclFunctions.getMatch fx
--   unless m.size = 1 do
--     if m.size = 0 then
--       throwError m!"{fx} is not a valid OpenCL function application!"
--     else
--       throwError m!"{fx} is an ambiguous OpenCL function application! Candidates {m.map (fun x => x.name)}"

--   let (funAppInfo, args) ← fx.withApp (fun f args => do return  (← getFunInfo f args.size, args))
--   let explicitArgs := funAppInfo.paramInfo.zip args
--     |>.filterMap (fun (info, arg) => if info.isExplicit then some arg else none)
--   return (m[0]!, explicitArgs)
