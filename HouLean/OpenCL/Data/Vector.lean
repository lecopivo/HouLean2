import HouLean.Data.Vector
import HouLean.OpenCL.Compiler.Main
import HouLean.OpenCL.Data.Float
import HouLean.OpenCL.Data.Fin

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

def componentProjection (i : Nat) : String :=
     (if i = 0 then
        ".x"
      else if i = 1 then
        ".y"
      else if i = 2 then
        ".y"
      else if i = 3 then
        ".w"
      else if i < 10 then
        s!".s{i}"
      else if i = 10 then
        ".sa"
      else if i = 11 then
        ".sb"
      else if i = 12 then
        ".sc"
      else if i = 13 then
        ".sd"
      else if i = 14 then
        ".se"
      else if i = 15 then
        ".sf"
      else
        panic! "The index has to be known at compile time")

@[opencl_csimp]
theorem opencl_rewrite_vector_getElem_nat [Inhabited α] (v : Vector α n) (i : Nat) (h) :
    v[i]'h
    =
    (v |> oclFunction (type := Vector α n → α) (componentProjection i) (kind := .postfix)) := sorry_proof

@[opencl_csimp]
theorem Vector.getElemFin_eq_getElemNat' (v : Vector α n) (i : Fin m) (h) :
  v[i]'h = v[i.1] := by rfl

-- ofFn
implemented_by (f : (Fin 2) → α) : Vector.ofFn f = #v[f 0, f 1]
implemented_by (f : (Fin 3) → α) : Vector.ofFn f = #v[f 0, f 1, f 2]
implemented_by (f : (Fin 4) → α) : Vector.ofFn f = #v[f 0, f 1, f 2, f 3]
implemented_by (f : (Fin 8) → α) : Vector.ofFn f = #v[f 0, f 1, f 2, f 3, f 4, f 5, f 6, f 7]
implemented_by (f : (Fin 16) → α) : Vector.ofFn f =
  #v[f 0, f 1, f 2, f 3, f 4, f 5, f 6, f 7,
     f 8, f 9, f 10, f 11, f 12, f 13, f 14, f 15]

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
implemented_by (v : Vector α 2) : v.mapIdx f = #v[f 0 v.x, f 1 v.y]
implemented_by (v : Vector α 3) : v.mapIdx f = #v[f 0 v.x, f 1 v.y, f 2 v.z]
implemented_by (v : Vector α 4) : v.mapIdx f = #v[f 0 v.x, f 1 v.y, f 2 v.z, f 3 v.w]
implemented_by (v : Vector α 8) :
  v.mapIdx f
  =
  #v[f 0 v[0], f 1 v[1], f 2 v[2], f 3 v[3],
     f 4 v[4], f 5 v[5], f 6 v[6], f 7 v[7]]
implemented_by (v : Vector α 16) :
  v.mapIdx f
  =
  #v[f 0 v[0], f 1 v[1], f 2 v[2], f 3 v[3],
     f 4 v[4], f 5 v[5], f 6 v[6], f 7 v[7],
     f 8 v[8], f 9 v[9], f 10 v[10], f 11 v[11],
     f 12 v[12], f 13 v[13], f 14 v[14], f 15 v[15]]
end MapIdx

section MapFinIdx
variable {β}
implemented_by (v : Vector α 2) (f : (i : Nat) → α → (h : i < 2) → β) :
  v.mapFinIdx f
  =
  #v[f 0 v.x (by grind), f 1 v.y (by grind)]

implemented_by (v : Vector α 3) (f : (i : Nat) → α → (h : i < 3) → β) :
  v.mapFinIdx f
  =
  #v[f 0 v.x (by grind), f 1 v.y (by grind), f 2 v.z (by grind)]

implemented_by (v : Vector α 4) (f : (i : Nat) → α → (h : i < 4) → β) :
  v.mapFinIdx f
  =
  #v[f 0 v.x (by grind), f 1 v.y (by grind), f 2 v.z (by grind), f 3 v.w (by grind)]

end MapFinIdx


section ZipMap
variable (f : α×β → γ)
implemented_by (u : Vector α 2) (v : Vector β 2) :
    (u.zip v).map f
    =
    #v[f (u.x,v.x), f (u.y,v.y)] := by simp[Vector.x, Vector.y]; grind

implemented_by (u : Vector α 3) (v : Vector β 3) :
    (u.zip v).map f
    =
    #v[f (u.x,v.x), f (u.y,v.y), f (u.z,v.z)] := by simp[Vector.x, Vector.y, Vector.z]; grind

implemented_by (u : Vector α 4) (v : Vector β 4) :
    (u.zip v).map f
    =
    #v[f (u.x,v.x), f (u.y,v.y), f (u.z,v.z), f (u.w,v.w)] := by simp[Vector.x, Vector.y, Vector.z, Vector.w]; grind

implemented_by (u : Vector α 8) (v : Vector β 8) :
    (u.zip v).map f
    =
    #v[f (u[0],v[0]), f (u[1],v[1]), f (u[2],v[2]), f (u[3],v[3]),
       f (u[4],v[4]), f (u[5],v[5]), f (u[6],v[6]), f (u[7],v[7])] := by sorry_proof

implemented_by (u : Vector α 16) (v : Vector β 16) :
    (u.zip v).map f
    =
    #v[f (u[0],v[0]), f (u[1],v[1]), f (u[2],v[2]), f (u[3],v[3]),
       f (u[4],v[4]), f (u[5],v[5]), f (u[6],v[6]), f (u[7],v[7]),
       f (u[8],v[8]), f (u[9],v[9]), f (u[10],v[10]), f (u[11],v[11]),
       f (u[12],v[12]), f (u[13],v[13]), f (u[14],v[14]), f (u[15],v[15])] := sorry_proof
end ZipMap

section Foldl
variable {β} (f : β → α → β) (init : β)
implemented_by (u : Vector α 2) : u.foldl f init = f (f init u.x) u.y
implemented_by (u : Vector α 3) : u.foldl f init = f (f (f init u.x) u.y) u.z
implemented_by (u : Vector α 4) : u.foldl f init = f (f (f (f init u.x) u.y) u.z) u.w
end Foldl


section Foldr
variable {β} (f : α → β → β) (init : β)
implemented_by (u : Vector α 2) : u.foldr f init = f u.x (f u.y init)
implemented_by (u : Vector α 3) : u.foldr f init = f u.x (f u.y (f u.z init))
implemented_by (u : Vector α 4) : u.foldr f init = f u.x (f u.y (f u.z (f u.w init)))
end Foldr

section Sum
variable [Add α] [Zero α]
implemented_by (u : Vector α n) : u.sum = u.foldl (· + ·) 0
end Sum


implemented_by [Zero α] : (0 : Vector α n) = Vector.zero (α:=α) (n:=n)
implemented_by (a : α) : Vector.replicate n a = .ofFn (fun _ => a)

-- -- -- open Math
-- -- -- instance {α} [Exp α] {n} : Exp (Vector α n) where
-- -- --   exp x := x.map exp
-- -- -- instance {α} [Sin α] {n} : Sin (Vector α n) where
-- -- --   sin x := x.map sin
-- -- -- defun sin (x : Vector Float 4) := x.map sin
-- -- -- set_option trace.HouLean.OpenCL.compiler true in
-- -- -- #opencl_compile (fun x : Float => Math.sin x)
-- -- -- set_option trace.HouLean.OpenCL.compiler true in
-- -- -- #opencl_compile (fun x : Vector Float 4 => Math.sin x)
-- -- -- #opencl_compile (fun x : Vector Float 3 => Math.exp x)
-- -- -- #opencl_compile (fun x : Vector Float 2 => Math.exp x)

-- #opencl_compile (fun (x : Vector Float 4) (s : Float) => x.sin / x)


--  (fun (x : Vector Float 4)  => decide (∀ i : Fin 4, x[i] < x[i]*x[i]))


-- -- set_option trace.HouLean.OpenCL.compiler true in
-- -- #opencl_compile (fun x : Float => decide (0 < x))

-- -- set_option trace.HouLean.OpenCL.compiler true in
-- -- #opencl_compile (fun x : Float => x.clamp 0 1)


-- #check Vector.normalize

-- variable [Add α] [Mul α] [Div α] [Zero α] [Sqrt α] [ApproxEqual α] [AtomicOpenCLType α] [Inhabited α]


-- instance [inst : OpenCLType α] : OpenCLType (OpenCLM α) where
--   name := inst.name
--   shortName := inst.shortName

-- instance [t : AtomicOpenCLType α] : OpenCLType (Pointer α) where
--   name := t.name ++ "*"
--   shortName := "p" ++ t.shortName

-- instance : OpenCLType Unit where
--   name := "void"
--   shortName := "v"


-- macro "*" ptr:term " := " val:term : doElem => `(doElem| Pointer.set $ptr 0 $val)

-- def Vector.OpenCL.normalize (u : Vector α n) (norm : Pointer α) : OpenCLM (Vector α n) := do
--   let len := u.length
--   if len ≈ 0 then
--     *norm := 0
--     return u
--   else
--     *norm := len
--     return u / len

-- opaque withPtr (val : α) (go : Pointer α → OpenCLM β) : OpenCLM β := sorry


-- def asdf (v : Vector Float 3) :=
--   let (u,len) := v.normalize
--   u.sum + len

-- instance : Inhabited (Pointer Float) := sorry

-- def asdf' (v : Vector Float 3) := do
--   let len : Pointer Float := default
--   let u ← Vector.OpenCL.normalize v len
--   let len ← len.toConst.get 0
--   return u.sum + len

-- def asdf'' (v : Vector Float 3) := do
--   let len : Pointer Float := default
--   let u ← Vector.OpenCL.normalize v len
--   let len ← len.toConst.get 0
--   let (u,len) := (u,len)
--   return u.sum + len
