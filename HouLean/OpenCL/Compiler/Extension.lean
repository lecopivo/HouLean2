import Lean
import HouLean.OpenCL.Basic

open Lean Meta

namespace HouLean.OpenCL.Compiler

initialize registerTraceClass `HouLean.OpenCL.compiler

mutual

inductive OpenCLType where
  /-- Atomic openCL type such as `float`, `float3`, `int` etc.

  `shortName` is is used for name mangling e.g. for `float` we have short name `f`, for `int` we have `i` etc.
  thus for example addition function will become `addfff` or `addiii`,
  scalar multiplication of a vector would be `mulff3f3`
  dot product could be `mulf3f3f` -/
  | atom (name : String) (shortName : String)
  | struct (s : Struct)
deriving BEq, Inhabited

structure StructField where
  type : OpenCLType
  name : String
deriving BEq, Inhabited

structure Struct where
  name : String
  shortName : String
  fields : Array StructField
deriving BEq, Inhabited

end

def OpenCLType.name (t : OpenCLType) : String :=
  match t with
  | .atom n _ => n
  | .struct { name := n, .. } => n

def OpenCLType.shortName (t : OpenCLType) : String :=
  match t with
  | .atom _ n => n
  | .struct { shortName := n, .. } => n

-- todo: decide what variants are actually needed
inductive FunctionArgumentKind where
  | input  -- e.g. const float x
  | output  -- e.g. float * x
  | ref  -- e.g. const float * x
deriving BEq, Inhabited

structure OpenCLFunction.Argument where
  type : OpenCLType
  name : String
  kind : FunctionArgumentKind
deriving BEq, Inhabited

inductive OpenCLFunction.Kind where
  | normal
  | infix
  | prefix
  | postfix
  | constructor
deriving BEq, Inhabited

inductive OpenCLFunction.Body where
  | builtin
  | code (s : String)
deriving BEq, Inhabited

open OpenCLFunction in
structure OpenCLFunction where
  args : Array Argument
  name : String
  kind : Kind
  returnType : OpenCLType
  body : Body
deriving BEq, Inhabited


-- key; @add Float inst ?_ ?_  ->

inductive SingleExtension where
  | type (keys : Array DiscrTree.Key) (oclType : OpenCLType)
  /-- During compilation we replace `src` with `trg`.  -/
  | implementedBy (src : Name) (trg : Name) (argMap : Array (Option Nat))
  | function (keys : Array DiscrTree.Key) (oclFun : OpenCLFunction)
deriving Inhabited


/-- Enviroment extension that holds all necessary information for the APEX compiler. -/
structure Extension where
  /-- Mapping from Lean types to OpenCL types -/
  oclTypes : DiscrTree OpenCLType
  /--  -/
  implementedBy : NameMap (Name × Array (Option Nat))

  oclFunctions : DiscrTree OpenCLFunction
deriving Inhabited





abbrev CompilerExt := SimpleScopedEnvExtension SingleExtension Extension


initialize compilerExt : CompilerExt ←
  registerSimpleScopedEnvExtension {
    name := by exact decl_name%
    initial := default
    addEntry := fun es e =>
      match e with
      | .type keys oclType =>
        {es with oclTypes := es.oclTypes.insertCore keys oclType}
      | .implementedBy src trg argMap =>
        {es with implementedBy := es.implementedBy.insert src (trg, argMap)}
      | .function keys oclFunc =>
        {es with oclFunctions := es.oclFunctions.insertCore keys oclFunc}
  }


def addOCLType (type : Expr) (oclType : OpenCLType) : MetaM Unit := do
  let keys ← DiscrTree.mkPath type
  compilerExt.add (.type keys oclType)

def getOCLType (type : Expr) : MetaM OpenCLType := do
  let mut type := type
  if type.isAppOfArity ``OpenCLM 1 then
    type := type.appArg!

  let s := compilerExt.getState (← getEnv)
  let m ← s.oclTypes.getMatch type
  unless m.size = 1 do
    if m.size = 0 then
      throwError m!"{type} is not a valid OpenCL type!"
    else
      throwError m!"{type} has ambiguous OpenCL type! Candidates {m.map (fun x => x.name)}"
  return m[0]!

def getOCLType? (type : Expr) : MetaM (Option OpenCLType) := do
  let mut type := type
  if type.isAppOfArity ``OpenCLM 1 then
    type := type.appArg!

  let s := compilerExt.getState (← getEnv)
  let m ← s.oclTypes.getMatch type
  unless m.size = 1 do
    return none
  return m[0]!

-- def addOCLFunction (f : Expr) (oclFunction : OpenCLFunction) : MetaM Unit := do
--   let (xs,_,_) ← forallMetaTelescope (← inferType f)
--   let keys ← DiscrTree.mkPath (f.beta xs)
--   compilerExt.add (.function keys oclFunction)
def nameToString (n : Name) : String := n.eraseMacroScopes.toString.replace "." "_"

open OpenCLFunction in
def addOCLFunction (f : Expr) (name : String) (kind := Kind.normal) (body := Body.builtin) : MetaM Unit := do
  forallTelescope (← inferType f) fun xs _ => do
    let returnType ← inferType (f.beta xs) >>= getOCLType
    let argTypes ← xs.mapM inferType >>= (·.mapM getOCLType)
    let argNames ← xs.mapM (fun x => nameToString <$> x.fvarId!.getUserName )
    let args : Array Argument :=
      argTypes.zip argNames |>.map (fun (t,n) => { type := t, name := n, kind := .input})

    let oclFunction : OpenCLFunction := {
      args := args
      name := name
      kind := kind
      returnType := returnType
      body := body
    }

    let (xs,_,_) ← forallMetaTelescope (← inferType f)
    let keys ← DiscrTree.mkPath (f.beta xs)
    compilerExt.add (.function keys oclFunction)


def getOCLFunApp (fx : Expr) : MetaM (OpenCLFunction × Array Expr) := do
  let s := compilerExt.getState (← getEnv)
  let m ← s.oclFunctions.getMatch fx
  unless m.size = 1 do
    if m.size = 0 then
      throwError m!"{fx} is not a valid OpenCL function application!"
    else
      throwError m!"{fx} is an ambiguous OpenCL function application! Candidates {m.map (fun x => x.name)}"

  let (funAppInfo, args) ← fx.withApp (fun f args => do return  (← getFunInfo f args.size, args))
  let explicitArgs := funAppInfo.paramInfo.zip args
    |>.filterMap (fun (info, arg) => if info.isExplicit then some arg else none)
  return (m[0]!, explicitArgs)
