namespace HouLean


opaque OpenCL.RealWorld.nonemptyType : NonemptyType.{0}

@[expose] def OpenCL.RealWorld : Type := OpenCL.RealWorld.nonemptyType.type

instance OpenCL.RealWorld.instNonempty : Nonempty OpenCL.RealWorld :=
  by exact OpenCL.RealWorld.nonemptyType.property

abbrev OpenCLM := StateM OpenCL.RealWorld


namespace OpenCL

opaque ArrayRef.nonemptyType (α : Type) : NonemptyType.{0}

def ArrayRef (α : Type) : Type := (ArrayRef.nonemptyType α).type

variable {α T} [Inhabited α]

class ArrayType (α : Type) where
  get : ArrayRef α → UInt64 → OpenCLM α
  set : ArrayRef α → UInt64 → α → OpenCLM Unit
