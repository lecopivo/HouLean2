import HouLean.Meta.AnonymousStruct
import HouLean.Meta.ProdLike
import HouLean.Meta.RewriteBy
import HouLean.OpenCL.Basic

namespace HouLean.OpenCL

class ArrayType (Elem : Type) (Ptr : outParam Type) where
  get : Ptr → UInt64 → OpenCLM Elem
  set : Ptr → UInt64 → Elem → OpenCLM Unit

abbrev ArrayPointer (α : Type) {Ptr} [ArrayType α Ptr] := Ptr

instance {α Ptr} [ArrayType α Ptr] : GetElem (ArrayPointer α) UInt64 (OpenCLM α) (fun _ _ => True) where
  getElem x i _ := ArrayType.get x i

instance {α Ptr} [ArrayType α Ptr] : GetElem (ArrayPointer α) Nat (OpenCLM α) (fun _ _ => True) where
  getElem x i _ := ArrayType.get x i.toUInt64

instance {α Ptr} [ArrayType α Ptr] : SetElemM (ArrayPointer α) UInt64 α OpenCLM (fun _ _ => True) where
  setElemM ptr i x _ := ArrayType.set ptr i x

instance {α Ptr} [ArrayType α Ptr] : SetElemM (ArrayPointer α) Nat α OpenCLM (fun _ _ => True) where
  setElemM ptr i x _ := ArrayType.set ptr i.toUInt64 x

variable {α} [Inhabited α] [AtomicOpenCLType α] [AllowedVectorSize n]

instance : ArrayType α (Pointer α) where
  get := Pointer.get
  set := Pointer.set

instance : ArrayType (Vector α n) (Pointer α) where
  get ptr off := Pointer.vload off ptr
  set ptr off val := Pointer.vstore val off ptr

instance {PtrA PtrB} [ArrayType α PtrA] [ArrayType β PtrB] : ArrayType (α×β) (PtrA×PtrB) where
  get := fun (ptrA, ptrB) idx => do
    let a ← ArrayType.get ptrA idx
    let b ← ArrayType.get ptrB idx
    return (a,b)
  set := fun (ptrA, ptrB) idx (a, b) => do
    ArrayType.set ptrA idx a
    ArrayType.set ptrB idx b

instance {Ptr} [ProdLike α A] [ArrayType A Ptr] : ArrayType α Ptr where
  get := fun ptr idx => do
    let a ← ArrayType.get (Elem:=A) ptr idx
    let a := fromProdType a
    return a
  set := fun ptr idx a => do
    ArrayType.set ptr idx (toProdType a)
