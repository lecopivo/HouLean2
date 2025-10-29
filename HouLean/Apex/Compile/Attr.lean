import HouLean.Apex.Compile.Main

open Lean Meta

namespace HouLean.Apex.Compiler

syntax (name:=apex) "apex" : attr

initialize apexAttr : Unit ←
  registerBuiltinAttribute {
    name  := `apex
    descr := "Mark Lean function to be compiled with APEX."
    applicationTime := AttributeApplicationTime.afterCompilation
    add   := fun declName stx attrKind =>
      discard <| MetaM.run do
        let info ← getConstInfo declName
        let val := info.value?.getD default
        
        -- try to compile to warn user but we consume all implicit arguments
        
        let _ ← functionToApexGraph val default
        compilerExt.add (.unfold declName)
    erase := fun _declName =>
      throwError "Can't remove `apex`, not implemented yet!"
  }

syntax (name:=apex_unfold) "apex_unfold" : attr

initialize apexUnfoldAttr : Unit ←
  registerBuiltinAttribute {
    name  := `apex_unfold
    descr := "Mark Lean function to unfolded/inlined by the APEX compiler."
    applicationTime := AttributeApplicationTime.afterCompilation
    add   := fun declName stx attrKind =>
      discard <| MetaM.run do
        let info ← getConstInfo declName
        let val := info.value?.getD default
        compilerExt.add (.unfold declName)
    erase := fun _declName =>
      throwError "Can't remove `apex_unfold`, not implemented yet!"
  }

