import HouLean.Meta.AnonymousStruct
import HouLean.Meta.ProdLike
import HouLean.Meta.RewriteBy
import HouLean.OpenCL.Basic
import HouLean.OpenCL.Data.Prod

namespace HouLean.OpenCL

class ArrayType (Elem : Type) (Ptr : outParam Type) where
  get : Ptr → UInt64 → OpenCLM Elem
  set : Ptr → UInt64 → Elem → OpenCLM Unit

abbrev ArrayPointer (α : Type) {Ptr} [ArrayType α Ptr] := Ptr

instance {α Ptr} [ArrayType α Ptr] : GetElem (ArrayPointer α) UInt64 (OpenCLM α) (fun _ _ => True) where
  getElem x i _ := ArrayType.get x i

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


-- /-- Set element of a pointer -/
-- class SetElemM (Ptr : Type) (Idx : Type) (Elem : outParam Type)
--     (m : outParam (Type → Type)) (Valid : Ptr → Idx → Prop) where
--   setElemM : (ptr : Ptr) → (i : Idx) → Elem → Valid ptr i → m Unit

-- variable {Ptr} [ArrayType α Ptr] (ptr : ArrayPointer α) (i : UInt64) (val : α)

-- #check ptr[i]

-- variable (ptr : ArrayPointer (struct {a : Float, b : Int, c : Float32}))

-- #check ptr[i]
--   rewrite_by
--     simp only [getElem,ArrayType.get,fromProdType,bind_assoc, pure_bind, bind_pure]

-- #eval IO.println (OpenCLType.definition? (ArrayPointer (struct {x : Float, y : Float})) |>.get!)
