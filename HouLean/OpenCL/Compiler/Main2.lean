import Lean
import Qq
import HouLean.Data.Matrix
import HouLean.Data.Vector
import HouLean.Meta.OverloadedFunction

open HouLean Lean Meta

namespace HouLean.OpenCL.Compiler2

structure Context where
  fvarMap : ExprMap String := {}
  usedNames : Std.HashMap String Nat := {}

structure Specialization where
  originalName : Name
  specName : Name
  argMap : Array Nat -- `i`-th argument of the specialization should be assigned `argMap[i]`-th argument of the original function
deriving BEq

structure State where
  specMap : ExprMap Name := {}
  spec : DiscrTree Specialization := {}

  -- compiledFunctions : Array CodeFunction := #[]
  -- statements : Array CodeStatement := #[]
  -- all functions that has been compiled in this run

abbrev CompileM := ReaderT Context <| StateT State <| SimpM

instance : MonadRecDepth CompileM where
  withRecDepth n x := fun ctx s => MonadRecDepth.withRecDepth n (x ctx s)
  getRecDepth := liftM (m := SimpM) <| MonadRecDepth.getRecDepth
  getMaxRecDepth := liftM (m := SimpM) <| MonadRecDepth.getMaxRecDepth


def exprToString (e : Expr) : MetaM String := do
  -- reduce before printing
  let e ← whnf e
  let s := toString (← ppExpr e)
  return s

/-- Give expression `fn arg₁ .. argₙ` the `apecializeFunction` returns `fn' arg'₁ ... arg'ₘ`
which is equal to the orignal expression but all arguments known at compile time are consumed in `fn'`
 -/
structure FunSpecializationResult where
  /-- original function -/
  fn : Expr
  /-- original arguments -/
  args : Array Expr

  fn' : Expr
  args' : Array Expr

  mangledName : Name

-- todo: should I also add support for "macro types"?
--       arguments of type `Optional α` could specialize for `some _` and `.none` and
--       it would be required to completely eliminate these types from the runtime
def specializeFunApp (fn : Expr) (args : Array Expr) : MetaM FunSpecializationResult := do
  let fname := fn.constName!
  -- let fnVal := (← unfold fn fname).expr
  let (fn', args', ss) ← go fn args.toList #[] #[] #[]
  let ss := ss.foldl (·++"_"++·) ""
  let mangledName := fname.appendAfter ss
  return { fn, args, fn', args', mangledName }
where
  go (e : Expr) (args : List Expr)
     (vars : Array Expr) (args' : Array Expr) (ss : Array String) :
     MetaM (Expr × Array Expr × Array String) := do
    match args with
    | [] =>
      let f ← mkLambdaFVars vars e
      return (f, args', ss)
    | arg :: args =>
      if !(arg.hasFVar || arg.hasMVar) then
        let type ← inferType arg
        let mut ss := ss

        -- ignore instances and proofs
        if (← isClass? type).isNone &&
           !(← inferType type).isProp then
          ss := ss.push (← exprToString arg)

        go (e.beta #[arg]) args vars args' ss
      else
        forallBoundedTelescope (← inferType e) (some 1) fun xs _ => do
          let x := xs[0]!
          go (e.beta #[x]) args (vars.push x) (args'.push arg) ss


def specializeAndDeclare (e : Expr) : MetaM Unit := do

  let (fn, args) := e.withApp (fun fn args => (fn, args))
  let r ← specializeFunApp fn args

  let e' := r.fn'.beta r.args'

  unless (← isDefEq e e') do
    throwError s!"Specialized expression is not equal to the original expression!"

  let val  ← instantiateMVars r.fn'
  let type ← inferType val

  let decl : Declaration := .defnDecl {
    name := r.mangledName
    levelParams := []
    type := type
    value := val
    hints := .regular val.approxDepth
    safety := .safe
  }

  addAndCompile decl

open Qq


/--
info: fn': fun a a_1 => a + a_1
---
info: name: HAdd.hAdd_Float_Float_Float
---
warning: unused variable `xs`

Note: This linter can be disabled with `set_option linter.unusedVariables false`
-/
#guard_msgs in
run_meta
  let e := q(fun x y : Float => x + y)
  lambdaTelescope e fun xs b => do
    let (fn,args) := b.withApp (fun fn args => (fn, args))
    let r ← specializeFunApp fn args
    logInfo m!"fn': {r.fn'}"
    logInfo m!"name: {r.mangledName}"

    specializeAndDeclare b



def foo (a b : Nat) := ((a + b) * a) % 3

-- use native decide to prove element indices for trivial inequalities like `0<4∧1<4`
macro_rules | `(tactic| get_elem_tactic_extensible) => `(tactic| sorry_proof)


/--
info: fn': fun a => a.col (foo 2 3) ⋯
---
info: name: HouLean.Matrix.col_Float_3_3_1
-/
#guard_msgs in
run_meta
  let e := q(fun A : Matrix Float 3 3 => A.col (foo 2 3))
  lambdaTelescope e fun _xs b => do
    let (fn,args) := b.withApp (fun fn args => (fn, args))
    let r ← specializeFunApp fn args
    logInfo m!"fn': {r.fn'}"
    logInfo m!"name: {r.mangledName}"

    specializeAndDeclare b


def bar {α β : Type u} [HAdd α β β] (x : α) (y : β) := x + (x + y)



/--
info: fn': fun {α} [HAdd α Int Int] x y => bar x y
---
info: name: HouLean.OpenCL.Compiler2.bar_Int
-/
#guard_msgs in
run_meta
  let e := q(fun {α : Type} [HAdd α Int Int] (x : α) (y : Int) => bar x y)
  lambdaTelescope e fun _xs b => do
    let (fn,args) := b.withApp (fun fn args => (fn, args))
    let r ← specializeFunApp fn args
    logInfo m!"fn': {r.fn'}"
    logInfo m!"name: {r.mangledName}"

    specializeAndDeclare b



partial def transform (e : Expr) : CompileM Expr := do
  withIncRecDepth do
  -- dbg_trace s!"{← ppExpr e}"
  match e with
  | .app .. =>
    let (fn, args) := e.withApp (fun fn args => (fn,args))
    match fn with
    | .const fname _ =>
      let r ← specializeFunApp fn args
      if ← isDefEq fn r.fn' then
        let args ← args.mapM transform
        return fn.beta args
      else
      let args' ← r.args'.mapM transform

      -- look up existing specializations
      -- todo: finish this and remove use of `specMap`
      let candidates ← (← get).spec.getMatch e
      if candidates.size ≠ 0 then
        logInfo m!"match {fname} --> {candidates.map (·.specName)}"

      if let some name := (←get).specMap[r.fn']? then
        return mkAppN (.const name []) args'
      else
        let (_,_,b) ← lambdaMetaTelescope r.fn'
        let key ← DiscrTree.mkPath b

        let fn' := (← unfold r.fn' fname).expr
        let fn'' ← transform fn'
        let type ← inferType fn''
        let name ← mkUniqueDeclName r.mangledName

        let decl : Declaration := .defnDecl {
          name := name
          levelParams := []
          type := type
          value := fn''
          hints := .regular fn''.approxDepth
          safety := .safe
        }

        logInfo m!"{name}:\n{fn''}"

        modify (fun s => {s with specMap := s.specMap.insert r.fn' name})

        let spec : Specialization := {
          originalName := fname
          specName := name
          argMap := #[] -- todo: this should be in `r`
        }
        modify (fun s => { s with spec := s.spec.insertCore key spec })

        addAndCompile decl

        return mkAppN (.const name []) args'
    | .proj .. =>
      let fn' := (← reduceProj? fn).getD fn
      if fn' == fn then
        return (fn'.beta args)
      else
        return ← transform (fn'.beta args)
    | _ =>
      throwError m!"don't know what to do about {e}"

  | .fvar _ =>
    return e

  | .letE n t v b _ =>
    let v' ← transform v
    withLetDecl n t v' fun var => do
      mkLambdaFVars #[var] (← transform (b.instantiate1 var))
  | .lam .. =>
    lambdaTelescope e fun xs b => do
      mkLambdaFVars xs (← transform b)
  | .proj .. => do
    let some e' ← reduceProj? e | return e
    return ← transform e'
  | _ =>
    throwError m!"don't know what to do about {e}"


def runTransform (e : Expr) : MetaM Expr := do

  let simpMthds := Simp.mkDefaultMethodsCore #[] --#[← getOpenCLSimprocs]
  let simpCtx : Simp.Context ← Simp.mkContext
    (config := {zetaDelta := false, zeta := false, iota := false})
    (simpTheorems := #[]) -- ← getOpenCLTheorems
  let simpState : Simp.State := {}
  let ctx : Context := {}
  let state : State := {}

  let ((e', _), _) ← transform e ctx state simpMthds.toMethodsRef simpCtx |>.run simpState
  return e'

open Qq

/--
info: Mul.mul_Float:
fun a a_1 => a.mul a_1
---
info: HMul.hMul_Float_Float_Float:
fun a a_1 => Mul.mul_Float a a_1
---
info: match HMul.hMul --> [HMul.hMul_Float_Float_Float]
---
info: Add.add_Float:
fun a a_1 => a.add a_1
---
info: HAdd.hAdd_Float_Float_Float1:
fun a a_1 => Add.add_Float a a_1
---
info: match HAdd.hAdd --> [HAdd.hAdd_Float_Float_Float1]
---
info: match HAdd.hAdd --> [HAdd.hAdd_Float_Float_Float1]
---
info: match HAdd.hAdd --> [HAdd.hAdd_Float_Float_Float1]
---
info: match HAdd.hAdd --> [HAdd.hAdd_Float_Float_Float1]
---
info: match HAdd.hAdd --> [HAdd.hAdd_Float_Float_Float1]
---
info: result:
fun x y =>
  let a := HMul.hMul_Float_Float_Float x y;
  let b := HMul.hMul_Float_Float_Float x a;
  HAdd.hAdd_Float_Float_Float1 (HAdd.hAdd_Float_Float_Float1 (HAdd.hAdd_Float_Float_Float1 x y) a)
    (HAdd.hAdd_Float_Float_Float1 (HAdd.hAdd_Float_Float_Float1 b a) (HAdd.hAdd_Float_Float_Float1 x b))
-/
#guard_msgs in
run_meta
  let e := q(fun x y : Float =>
    let a := x * y
    let b := x * a
    (x + y + a) + ((b + a) + (x + b)))
  let e' ← runTransform e
  logInfo m!"result:\n{e'}"



-- -- todo: somehow prevent infinite loop
-- run_meta
--   let e := q(fun x y : Vector Float 3 => x + y)
--   let e' ← runTransform e
--   logInfo m!"result:\n{e'}"


/-- Gadget identity function that will stop `simp` from simplifying an expression.

This is useful when the lhs of simp theorem appears on the rhs. You can wrap the occurence
in `no_simp` an prevent simp from an infinite loop.

The main use is for `simp` based compiler. For example for compiling to C we might define this
function, which indicates that `spec` should be replaced with C function with the name `cfun`
```
def cFunction (spec : α) (cfun : String) : α := spec
```
Then we add the following simp theorem
```
theorem compile_sin : Float.sin = cFunction (no_simp Float.sin) "sin" := rfl
```
where we wrapped `Float.sin` in `no_simp` to preven this theorem to be applied again on the `spec`
argument of `cFunction`.
 -/
def no_simp {α : Sort u} (a : α) := a

simproc_decl no_simp_simproc (no_simp _) := fun e =>
  return .done { expr := e }
