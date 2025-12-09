import HouLean.OpenCL.Basic

namespace HouLean.OpenCL

def ArrayPointer (α : Type) {A} [ArrayType α A] := Pointer A

instance{α A} [ArrayType α A] : GetElem (ArrayPointer α) UInt64 (OpenCLM α) (fun _ _ => True) where
  getElem x i _ := ArrayType.get x i

instance {α A} [ArrayType α A] [t : OpenCLType A] : OpenCLType (ArrayPointer α) where
  name := s!"{t.name} *"
  shortName := s!"p{t.shortName}"

implemented_by [AtomicOpenCLType A] [ArrayType α A] (a : ArrayPointer α) (i : UInt64) :
  a[i] = ArrayType.get (α:=α) a i
