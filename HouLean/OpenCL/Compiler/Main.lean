import HouLean.Data.Defs
import HouLean.OpenCL.Compiler.Extension

open Lean Meta Qq HouLean

namespace HouLean.OpenCL.Compiler

structure Context where
  fvarMap : ExprMap String := {}

structure State where
  -- fvarMap
  -- lines : Array String

set_option linter.unusedVariables false

abbrev CompileM := ReaderT Compiler.Context <| StateT State <| MetaM

def withFVars (xs : Array Expr) (types : Array OpenCLType) (go : CompileM α) : CompileM α := do
  let names ← xs.mapM (fun x => nameToString <$> x.fvarId!.getUserName)
  fun ctx =>
  let ctx := { ctx with
      fvarMap := (xs.zip names).foldl (init := ctx.fvarMap) (fun m (x,n) => m.insert x n)
    }
  go ctx

inductive CodeExpr where
  | fvar (name : String)
  | app (fn : OpenCLFunction) (args : Array OpenCLCode)

-- bunch of let bindings, add support for `if then else` and `for(..){ .. }`
inductive CodeBody where
  | letE (name : String) (type : OpenCLType) (val : CodeExpr) (body : CodeBody)
  | ret (val : CodeExpr)


partial def compileExpr (e : Expr) : CompileM String := do
  match e with
  | .app .. =>
    let (fn, args) ← getOCLFunApp e
    let argCodes ← args.mapM compileExpr

    match fn.kind with
    | .normal =>
      let args := argCodes.joinl (map:=id) (·++", "++·)
      return s!"{fn.name}({args})"
    | .constructor =>
      let args := argCodes.joinl (map:=id) (·++", "++·)
      return s!"{fn.name}\{{args}}"
    | .infix =>
      let args := argCodes.joinl (map:=id) (·++fn.name++·)
      return s!"({args})"
    | .prefix =>
      unless args.size = 1 do
        throwError m!"Invalid application of prefix function '{fn.name}' to arguments {args}!"
      return s!"{fn.name}{argCodes[0]!}"
    | .postfix =>
      unless args.size = 1 do
        throwError m!"Invalid application of postfix function '{fn.name}' to arguments {args}!"
      return s!"{argCodes[0]!}{fn.name}"

  | .fvar .. =>
    let some varName := (← read).fvarMap[e]?
      | throwError m!"Unrecognized free variable {e}!"
    return varName

  | .letE name type val body _ =>
    throwError m!"can't have let binding in an expression {e}"
  | _ =>
    throwError m!"Do not know how to compile {e}"



partial def compileFunBody (e : Expr) : CompileM String := do
  match e with
  | .letE name type val body _ =>

    -- todo: we have to determine if `val` is compilable
    --       we might have to decompose it and introduce few more let bindings

    withLetDecl name type val fun var => do
      let oclType ← getOCLType type
      let body := body.instantiate1 var
      withFVars #[var] #[oclType] do
        pure s!"const {oclType.name} {name} = {← compileExpr val};\n\
                {← compileFunBody body}"
  | .app .. =>
    return s!"return {← compileExpr e};"
  | _ => throwError m!"Do not know how to compile {e}"



def compileFun (f : Expr) (name : String) : CompileM String := do
  forallTelescope (← inferType f) fun xs r => do
    let body := f.beta xs

    let returnType ← getOCLType r
    let argTypes ← liftM <| xs.mapM inferType >>= (·.mapM getOCLType)
    let argNames ← xs.mapM (fun x => nameToString <$> x.fvarId!.getUserName)
    let args : Array String :=
      argTypes.zip argNames |>.map (fun (t,n) => s!"const {t.name} {n}")

    let args := args.joinl (map:=id) (·++", "++·)

    withFVars xs argTypes do

      let bodyCode ← compileFunBody body
      let bodyCode := bodyCode.replace "\n" "\n  "

      let code : String :=
        s!"{returnType.name} {name}({args})\n\
           \{\n\
           {bodyCode}\n}"

      return code


def funNameOverrideMap : NameMap Name :=
  ({} : NameMap Name)
    |>.insert ``HMul.hMul `mul
    |>.insert ``HAdd.hAdd `add

def mangleName (funName : Name) (info : FunInfo) (args : Array Expr) : MetaM String := do
  let mut typeSuffix := ""

  for arg in args do
    let t ← inferType arg
    if t.isSort then
      let oclType ← getOCLType arg
      typeSuffix := typeSuffix ++ oclType.shortName

  let funName := funName.eraseMacroScopes
  let funName := funNameOverrideMap.get? funName |>.getD funName
  let prfx := funName.getPrefix
  let mut name := funName.getString!

  if let some t ← getOCLType? (.const prfx []) then
    name := t.name ++ "_" ++ name

  let r := name ++ typeSuffix

  return r


def compileFunction (f : Expr) : MetaM Unit := do

  forallTelescope (← inferType f) fun xs returnType => do
    let body := f.beta xs

    let (fn, args) := body.withApp (fun fn args => (fn,args))
    let .const funName _ := fn
      | throwError "Expected constant head function in {body}"

    let funInfo ← getFunInfo fn
    let name ← mangleName funName funInfo args

    -- unfold definition of the function and basic reduction (mainly to reduce instances)
    let body ← withConfig (fun cfg => {cfg with zetaDelta := false, zeta := false}) <|
      whnfI body
    let body := (← unfold body funName).expr

    let returnType ← getOCLType returnType
    let argTypes ← liftM <| xs.mapM inferType >>= (·.mapM getOCLType)
    let argNames ← xs.mapM (fun x => nameToString <$> x.fvarId!.getUserName)
    let args : Array String :=
      argTypes.zip argNames |>.map (fun (t,n) => s!"const {t.name} {n}")

    let args := args.joinl (map:=id) (·++", "++·)


    let go :=
      withFVars xs argTypes do

        let bodyCode ← compileFunBody body

        let code : String :=
          s!"{returnType.name} {name}({args})\n\
             \{\n\
             {bodyCode}\n}"

        return code

    let (code,_) ← go {} {}

    trace[HouLean.OpenCL.compiler] "compiled {f} to:\n{code}"

    addOCLFunction f name (body := .code code)
