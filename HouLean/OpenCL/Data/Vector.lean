import HouLean.Data.Vector
import HouLean.OpenCL.Compiler.Main

namespace HouLean.OpenCL

open Qq HouLean Math

variable {α : Type} {n : Nat}

-- constructor
def vectorMk2 (x y : α) : Vector α 2 := #v[x,y]
def vectorMk3 (x y z : α) : Vector α 3 := #v[x,y,z]
def vectorMk4 (x y z w : α) : Vector α 4 := #v[x,y,z,w]
def vectorMk8 (x0 x1 x2 x3 x4 x5 x6 x7 : α) : Vector α 8 :=
  #v[x0, x1, x2, x3, x4, x5, x6, x7]
def vectorMk16 (x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 : α) : Vector α 16 :=
  #v[x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15]


implemented_by (x y : α) : #v[x,y] = vectorMk2 x y
implemented_by (x y z : α) : #v[x,y,z] = vectorMk3 x y z := by rfl
implemented_by (x y z w : α) : #v[x,y,z,w] = vectorMk4 x y z w := by rfl
implemented_by (x0 x1 x2 x3 x4 x5 x6 x7 : α) :
   #v[x0, x1, x2, x3, x4, x5, x6, x7] = (vectorMk8 x0 x1 x2 x3 x4 x5 x6 x7) := by rfl
implemented_by (x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 : α) :
  #v[x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15]
  =
  (vectorMk16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) := by rfl

section
variable [AtomicOpenCLType α]

instance : OpenCLFunction (vectorMk2 (α:=α)) where
  name :=
    let t : OpenCLType (Vector α 2) := by infer_instance
    s!"({t.name})"
  kind := .constructor

instance : OpenCLFunction (vectorMk3 (α:=α)) where
  name :=
    let t : OpenCLType (Vector α 3) := by infer_instance
    s!"({t.name})"
  kind := .constructor

instance : OpenCLFunction (vectorMk4 (α:=α)) where
  name :=
    let t : OpenCLType (Vector α 4) := by infer_instance
    s!"({t.name})"
  kind := .constructor

instance : OpenCLFunction (vectorMk8 (α:=α)) where
  name :=
    let t : OpenCLType (Vector α 8) := by infer_instance
    s!"({t.name})"
  kind := .constructor

instance : OpenCLFunction (vectorMk16 (α:=α)) where
  name :=
    let t : OpenCLType (Vector α 16) := by infer_instance
    s!"({t.name})"
  kind := .constructor


-- projections and element access
instance : OpenCLFunction (@Vector.x α n) where
  name := ".x"
  kind := .postfix

instance : OpenCLFunction (@Vector.y α n) where
  name := ".y"
  kind := .postfix

instance : OpenCLFunction (@Vector.z α n) where
  name := ".z"
  kind := .postfix

instance : OpenCLFunction (@Vector.w α n) where
  name := ".w"
  kind := .postfix
end


implemented_by (v : Vector α n) (h) : v[0]'h = v.x := by rfl
implemented_by (v : Vector α n) (h) : v[1]'h = v.y := by rfl
implemented_by (v : Vector α n) (h) : v[2]'h = v.z := by rfl
implemented_by (v : Vector α n) (h) : v[3]'h = v.w := by rfl

implemented_by (v : Vector α n) (h : 0 < n) : v[(⟨0,h⟩:Fin n)] = v.x := by rfl
implemented_by (v : Vector α n) (h : 1 < n) : v[(⟨1,h⟩:Fin n)] = v.y := by rfl
implemented_by (v : Vector α n) (h : 2 < n) : v[(⟨2,h⟩:Fin n)] = v.z := by rfl
implemented_by (v : Vector α n) (h : 3 < n) : v[(⟨3,h⟩:Fin n)] = v.w := by rfl

implemented_by (v : Vector α n) [OfNat (Fin n) 0] : v[(0 : Fin n)] = v.x sorry_proof
implemented_by (v : Vector α n) [OfNat (Fin n) 1] : v[(1 : Fin n)] = v.y sorry_proof
implemented_by (v : Vector α n) [OfNat (Fin n) 2] : v[(2 : Fin n)] = v.z sorry_proof
implemented_by (v : Vector α n) [OfNat (Fin n) 3] : v[(3 : Fin n)] = v.w sorry_proof

-- ofFn
implemented_by (f : (Fin 2) → α) : Vector.ofFn f = #v[f 0, f 1]
implemented_by (f : (Fin 3) → α) : Vector.ofFn f = #v[f 0, f 1, f 2]
implemented_by (f : (Fin 4) → α) : Vector.ofFn f = #v[f 0, f 1, f 2, f 3]

-- map
section Map
variable {β} (f : α → β)
implemented_by (v : Vector α 2) : v.map f = #v[f v.x, f v.y]
implemented_by (v : Vector α 3) : v.map f = #v[f v.x, f v.y, f v.z]
implemented_by (v : Vector α 4) : v.map f = #v[f v.x, f v.y, f v.z, f v.w]
implemented_by (v : Vector α 8) :
    v.map f = #v[f v[0],f v[1],f v[2],f v[3],
                 f v[4],f v[5],f v[6],f v[7]]
implemented_by (v : Vector α 16) :
    v.map f = #v[f v[0],f v[1],f v[2],f v[3],
                 f v[4],f v[5],f v[6],f v[7],
                 f v[8],f v[9],f v[10],f v[11],
                 f v[12],f v[13],f v[14],f v[15]]
end Map

section MapIdx
variable {β} (f : Nat → α → β)
implemented_by (v : Vector α 2) : v.mapIdx f = let v := v; #v[f 0 v.x, f 1 v.y]
implemented_by (v : Vector α 3) : v.mapIdx f = let v := v; #v[f 0 v.x, f 1 v.y, f 2 v.z]
implemented_by (v : Vector α 4) : v.mapIdx f = let v := v; #v[f 0 v.x, f 1 v.y, f 2 v.z, f 3 v.w]
end MapIdx

section MapFinIdx
variable {β}
implemented_by (v : Vector α 2) (f : (i : Nat) → α → (h : i < 2) → β) :
  v.mapFinIdx f
  =
  let v := v
  #v[f 0 v.x (by grind), f 1 v.y (by grind)]

implemented_by (v : Vector α 3) (f : (i : Nat) → α → (h : i < 3) → β) :
  v.mapFinIdx f
  =
  let v := v
  #v[f 0 v.x (by grind), f 1 v.y (by grind), f 2 v.z (by grind)]

implemented_by (v : Vector α 4) (f : (i : Nat) → α → (h : i < 4) → β) :
  v.mapFinIdx f
  =
  let v := v
  #v[f 0 v.x (by grind), f 1 v.y (by grind), f 2 v.z (by grind), f 3 v.w (by grind)]

end MapFinIdx


section ZipMap
variable (f : α×β → γ)
implemented_by (u : Vector α 2) (v : Vector β 2) :
    (u.zip v).map f
    =
    let u := u
    let v := v
    #v[f (u.x,v.x), f (u.y,v.y)] := by simp[Vector.x, Vector.y]; grind

implemented_by (u : Vector α 3) (v : Vector β 3) :
    (u.zip v).map f
    =
    let u := u
    let v := v
    #v[f (u.x,v.x), f (u.y,v.y), f (u.z,v.z)] := by simp[Vector.x, Vector.y, Vector.z]; grind

implemented_by (u : Vector α 4) (v : Vector β 4) :
    (u.zip v).map f
    =
    let u := u
    let v := v
    #v[f (u.x,v.x), f (u.y,v.y), f (u.z,v.z), f (u.w,v.w)] := by simp[Vector.x, Vector.y, Vector.z, Vector.w]; grind

implemented_by (u : Vector α 8) (v : Vector β 8) :
    (u.zip v).map f
    =
    let u := u
    let v := v
    #v[f (u[0],v[0]), f (u[1],v[1]), f (u[2],v[2]), f (u[3],v[3]),
       f (u[4],v[4]), f (u[5],v[5]), f (u[6],v[6]), f (u[7],v[7])] := by sorry_proof

implemented_by (u : Vector α 16) (v : Vector β 16) :
    (u.zip v).map f
    =
    let u := u
    let v := v
    #v[f (u[0],v[0]), f (u[1],v[1]), f (u[2],v[2]), f (u[3],v[3]),
       f (u[4],v[4]), f (u[5],v[5]), f (u[6],v[6]), f (u[7],v[7]),
       f (u[8],v[8]), f (u[9],v[9]), f (u[10],v[10]), f (u[11],v[11]),
       f (u[12],v[12]), f (u[13],v[13]), f (u[14],v[14]), f (u[15],v[15])] := sorry_proof
end ZipMap

section Foldl
variable {β} (f : β → α → β) (init : β)
implemented_by (u : Vector α 2) : u.foldl f init = let u:= u; f (f init u.x) u.y
implemented_by (u : Vector α 3) : u.foldl f init = let u:= u; f (f (f init u.x) u.y) u.z
implemented_by (u : Vector α 4) : u.foldl f init = let u:= u; f (f (f (f init u.x) u.y) u.z) u.w
end Foldl

section Foldr
variable {β} (f : α → β → β) (init : β)
implemented_by (u : Vector α 2) : u.foldr f init = let u:= u; f u.x (f u.y init)
implemented_by (u : Vector α 3) : u.foldr f init = let u:= u; f u.x (f u.y (f u.z init))
implemented_by (u : Vector α 4) : u.foldr f init = let u:= u; f u.x (f u.y (f u.z (f u.w init)))
end Foldr

section Sum
variable [Add α] [Zero α]
implemented_by (u : Vector α n) : u.sum = u.foldl (· + ·) 0
end Sum
