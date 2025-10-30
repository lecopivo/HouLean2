import HouLean.Apex.Compile.ExprType

open Lean Meta

namespace HouLean.Apex.Compiler

----------------------------------------------------------------------------------------------------

-- Option α -> Maybe α ≃ (α × Bool)
-- inductive enums -> Int
-- Nat -> Int
-- Array Float -> FloatArray
-- Array String -> StringArray
-- ...
-- structure TypeIsomorphism where
--   srcType : Expr  -- ∀ (α : Type), Option α
--   trgType : Expr  -- ∀ (α : Type), α × Bool
--   encode : Expr   -- {α : Type} -> [Inhabited α] -> Option α -> (α × Bool)
--   decode : Expr   -- {α : Type} -> (α × Bool) → Option α

----------------------------------------------------------------------------------------------------

def getApexFunType? (funType : Expr) : MetaM (Option ApexFunType) := do

  forallTelescope funType fun xs b => do
  
    let mut inputs := #[]
    for x in xs do 

      -- process only explicit arguments
      unless (← x.fvarId!.getBinderInfo).isExplicit do
        continue

      let some t ← getApexType? (← inferType x)
        | return none

      inputs := inputs.push t

    let some output ← getApexType? b
      | return none

    return some {
      inputs := inputs
      output := output
    }


/-- Function `srcFun` is replaced by `trgFun` before passed to the APEX Compiler

The APEX types of these functions must match but Lean types might not.
-/
structure ImplementedBy where
  srcFun : Expr
  trgFun : Expr
deriving Inhabited

def addImplementedByName (src trg : Name) (kind : AttributeKind) : MetaM Unit := do
  let srcInfo ← getConstInfo src
  let trgExpr ← mkConstWithFreshMVarLevels src

  -- initializing the types for src and trg in different ways to 
  -- handle universes properly (we keep level parameters from `srcInfo` and `trgExpr` will
  -- get them from unification)
  forallTelescope (srcInfo.type) fun xs t => do
  forallTelescope (← inferType trgExpr) fun ys _ => do

    let typeConstructor := t.isSort

    unless xs.size = ys.size do
      throwError m!"Functions {src} and {trg} must have the same arity!"

    -- prepare all type arguments with `Float`
    let mut typeArgs : Array (Option Expr) := #[]
    for x in xs, y in ys do

      let srcBi ← x.fvarId!.getBinderInfo
      let trgBi ← y.fvarId!.getBinderInfo
      unless srcBi == trgBi do
        throwError m!"type argument {x} of {src} and type argumetn {y} of {trg} are expected to have the same binder type"

      -- once we hit explicit arguments we stop
      -- for type constructors we do not stop
      if srcBi.isExplicit ∧ ¬typeConstructor then
        break

      -- all type parameters are required to have the same type!
      unless ← isDefEq (← inferType x) (← inferType y) do
        throwError m!"type argument {x} of {src} and type argument {y} of {trg} must have the same type!"

      if (← inferType x).isSort then
        typeArgs := typeArgs.push (some (.const ``Float []))
      else
        typeArgs := typeArgs.push none

    let srcExpr ← mkAppOptM src typeArgs
    let trgExpr ← mkAppOptM trg typeArgs
  
    if typeConstructor then
      compilerExt.add (.implementedByName src trg) kind
    else

      let some srcType ← getApexFunType? (← inferType srcExpr)
        | throwError m!"Function {srcExpr} : {← inferType srcExpr} does not have a valid APEX type!"
      let some trgType ← getApexFunType? (← inferType trgExpr)
        | throwError m!"Function {trgExpr} : {← inferType trgExpr} does not have a valid APEX type!"

      unless srcType == trgType do
        throwError m!"Can't implement {srcExpr} with {trgExpr}, they have different APEX types"

      compilerExt.add (.implementedByName src trg) kind
    
  -- unless srcType == trgType do
  --   throwError m!"Can't implement {src} with {trg}, they have different APEX types"

  compilerExt.add (.implementedByName src trg) kind


def addImplementedBy (srcFun trgFun : Expr) (kind : AttributeKind) : MetaM Unit := do

  let some srcType ← getApexFunType? (← inferType srcFun)
    | throwError m!"Function {srcFun} : {← inferType srcFun} does not have a valid APEX type!"
  let some trgType ← getApexFunType? (← inferType trgFun)
    | throwError m!"Function {trgFun} : {← inferType trgFun} does not have a valid APEX type!"

  unless srcType == trgType do
    throwError m!"Can't implement {srcFun} with {trgFun}, they have different APEX types"

  if let .const trgName _ := trgFun then
    let info ← getConstInfo trgName
    compilerExt.add (.implementedByExpr srcFun info.value!) kind
  else
    compilerExt.add (.implementedByExpr srcFun trgFun) kind


syntax (name:=apex_implements) "apex_implements" ident : attr

initialize implementsAttr : Unit ←
  registerBuiltinAttribute {
    name  := `apex_implements
    descr := "todo: ..."
    applicationTime := AttributeApplicationTime.afterCompilation
    add   := fun trgName stx attrKind =>
      match stx with
      | `(apex_implements| apex_implements $srcIdent) => discard <| MetaM.run do
        addImplementedBy (.const srcIdent.getId []) (.const trgName []) attrKind
      | _ => Elab.throwUnsupportedSyntax
    erase := fun _declName =>
      throwError "Can't remove `apex_implements`, not implemented yet!"
  }

syntax (name:=apex_implemented_by) "apex_implemented_by" ident : attr

initialize implementedByAttr : Unit ←
  registerBuiltinAttribute {
    name  := `apex_implemented_by
    descr := "todo: ..."
    applicationTime := AttributeApplicationTime.afterCompilation
    add   := fun srcName stx attrKind =>
      match stx with
      | `(apex_implemented_by| apex_implemented_by $trgIdent) => discard <| MetaM.run do
        addImplementedBy (.const srcName []) (.const trgIdent.getId []) attrKind
      | _ => Elab.throwUnsupportedSyntax
    erase := fun _declName =>
      throwError "Can't remove `apex_implemented_by`, not implemented yet!"
  }
