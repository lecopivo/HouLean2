import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Apex.Lean.Decidable
import HouLean.Apex.Lean.TwoWaySwitch


namespace HouLean.Apex.Compiler

open HouLean

namespace Generated

@[apex_node "IfBegin" has_rundata]
opaque IfBegin (condition : Bool) {ts} (spare : VariadicArg' ts) :
  Int × VariadicArg' ts := cast sorry_proof ()

@[apex_node "IfEnd" has_rundata]
opaque IfEnd (scope : Int) {ts} (spare : VariadicArg' ts) :
  VariadicArg' ts := cast sorry_proof ()

end Generated


open Generated TwoWaySwitch


noncomputable -- this confuses the compiler ... so we disable it
def _root_.HouLean.ifThenElse.apex_impl_simple {α Context ts} [Inhabited α] [ApexTypeFlatten (α×Context) ts]
    (condition : Bool) (onTrue onFalse : Context → α) (ctx : Context) : α :=

  let condition := condition
  let result : α := default
  let ctxAndResult := apexFlatten (result, ctx)

  -- true branch
  let r := IfBegin condition ctxAndResult
  let scope := r.1
  let ctxAndResult : α × Context := apexUnflatten r.2
  let ctx := ctxAndResult.2
  let result := onTrue ctx
  let ctxAndResult := IfEnd scope (apexFlatten (result, ctx))

  -- false branch
  let r := IfBegin (!condition) ctxAndResult
  let scope := r.1
  let ctxAndResult : α × Context := apexUnflatten r.2
  let ctx := ctxAndResult.2
  let result := onFalse ctx
  let ctxAndResult := IfEnd scope (apexFlatten (result,ctx))

  let ctxAndResult : α × Context := apexUnflatten ctxAndResult

  ctxAndResult.1


noncomputable -- this confuses the compiler ... so we disable it
def _root_.HouLean.ifThenElse.apex_impl_ctx_modify {α Context ts} [ApexTypeFlatten Context ts]
    (condition : Bool) (onTrue onFalse : Context → Context) (ctx : Context) (proj : Context → α) : α :=

  let condition := condition
  let ctx := apexFlatten ctx

  -- true branch
  let r := IfBegin condition ctx
  let scope := r.1
  let ctx := r.2
  let ctx := onTrue (apexUnflatten ctx)
  let ctx := IfEnd scope (apexFlatten ctx)

  -- false branch
  let r := IfBegin (!condition) ctx
  let scope := r.1
  let ctx := r.2
  let ctx := onFalse (apexUnflatten ctx)
  let ctx := IfEnd scope (apexFlatten ctx)

  proj (apexUnflatten ctx)


noncomputable -- this confuses the compiler ... so we disable it
def _root_.HouLean.ifThenElse.apex_impl_ctx_modify' {α Context Context' ts} [ApexTypeFlatten Context ts]
    (condition : Bool) (onTrue onFalse : Context → Context) (ctx : Context) (split : Context → α × Context') : α × Context' :=

  let condition := condition
  let ctx := apexFlatten ctx

  -- true branch
  let r := IfBegin condition ctx
  let scope := r.1
  let ctx := r.2
  let ctx := onTrue (apexUnflatten ctx)
  let ctx := IfEnd scope (apexFlatten ctx)

  -- false branch
  let r := IfBegin (!condition) ctx
  let scope := r.1
  let ctx := r.2
  let ctx := onFalse (apexUnflatten ctx)
  let ctx := IfEnd scope (apexFlatten ctx)

  split (apexUnflatten ctx)


/-- Basic if then else function -/
opaque _root_.HouLean.ifThenElse {α} (condition : Bool) (onTrue onFalse : α) : α := onTrue


def _root_.ite.apex_impl {α} (c : Prop) [Decidable c] (t e : α) : α :=
  let condition := decide c
  ifThenElse condition t e

def _root_.dite.apex_impl {α} (c : Prop) [Decidable c] (t : c → α) (e : ¬c → α) : α :=
  let condition := decide c
  ifThenElse condition (t sorry_proof) (e sorry_proof)

run_meta compilerExt.add (.implementedByName ``ite ``ite.apex_impl
  #[some 0, some 1, some 2, some 3, some 4])

run_meta compilerExt.add (.implementedByName ``dite ``dite.apex_impl
  #[some 0, some 1, some 2, some 3, some 4])



opaque _root_.HouLean.ctxIte {ts} {Context : Type} [ApexTypeFlatten Context ts] (condition : Bool) (init : Context) (onTrue onFalse : Context → Context) : Context := init

noncomputable
def _root_.HouLean.ctxIte.apex_impl {ts} {Context : Type} [ApexTypeFlatten Context ts] (condition : Bool) (init : Context) (onTrue onFalse : Context → Context) : Context :=
  _root_.HouLean.ifThenElse.apex_impl_ctx_modify (α:=Context) condition onTrue onFalse init id

run_meta compilerExt.add (.implementedByName ``ctxIte ``ctxIte.apex_impl
  #[some 0, some 1, some 2, some 3, some 4, some 5, some 6])
