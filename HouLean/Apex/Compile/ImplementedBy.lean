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

def addImplementedBy (srcFun trgFun : Expr) (kind : AttributeKind) : MetaM Unit := do

  let some srcType ← getApexFunType? (← inferType srcFun)
    | throwError m!"Function {srcFun} : {← inferType srcFun} does not have a valid APEX type!"
  let some trgType ← getApexFunType? (← inferType trgFun)
    | throwError m!"Function {trgFun} : {← inferType trgFun} does not have a valid APEX type!"

  unless srcType == trgType do
    throwError m!"Can't implement {srcFun} with {trgFun}, they have different APEX types"

  compilerExt.add (.implementedBy srcFun trgFun) kind


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
