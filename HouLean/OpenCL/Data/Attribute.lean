import HouLean.OpenCL.Data.ArrayType
import HouLean.Data.Geometry

namespace HouLean.OpenCL
set_option linter.unusedVariables false

/-- Geometry attribute pointer allows you to read and write attribute values.


Implementation detail:
This is just a pointer `ArrayPointer type` that is anotated with attribute information. -/
abbrev Attribute
    (cls : AttributeClass)
    (type : Type)
    (size : Nat)
    (name : String := "")
    (input : Nat := 0)
    (read := true)
    (write := false)
    {Ptr} [ArrayType type Ptr] :=
  ArrayPointer type

variable {cls : AttributeClass} {type : Type} {size : Nat} {name : String} {input : Nat}
  {Ptr} [ArrayType type Ptr]

instance {write : Bool} :
    GetElem (Attribute cls type size name input (read:=true) write) Nat (OpenCLM type) (fun _ i => i < size) where
  getElem attr i _ := attr[i]

instance {cls type size name input read Ptr} [ArrayType type Ptr] :
    SetElemM (Attribute cls type size name input read (write:=true)) Nat type OpenCLM (fun _ i => i < size) where
  setElemM attr i x _ := setElemM attr i x .intro
