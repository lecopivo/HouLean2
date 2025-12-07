import HouLean.Data.Vector
import HouLean.OpenCL.Compiler.Main
import HouLean.OpenCL.Data.Float
import HouLean.OpenCL.Data.Fin
import HouLean.OpenCL.Data.ArgList


namespace HouLean.OpenCL

open Qq HouLean Math

variable {α : Type} {n : Nat}

-- constructor
implemented_by [t : AtomicOpenCLType α] [Inhabited α] (n) (xs : List α) (h) :
  Vector.mk (n:=n) xs.toArray h
  =
  (oclFunction (ArgList α → Vector α n) s!"({t.name}{n})" .constructor)
  (ArgList.ofList xs)

-- ofFn
implemented_by [t : AtomicOpenCLType α] [Inhabited α] (n) (f : (Fin n) → α) :
  Vector.ofFn f
  =
  (oclFunction (ArgList α → Vector α n) s!"({t.name}{n})" .constructor)
  (ArgList.ofFn f)

attribute [opencl_csimp] inlinedLoop inlinedLoop.go

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

-- map
implemented_by {β} (f : α → β) (v : Vector α n) :
    v.map f = let v := v; .ofFn (fun i => f (v[i]))
implemented_by {β} (f : Nat → α → β) (v : Vector α n) :
    v.mapIdx f = let v := v; .ofFn (fun i => f i.1 (v[i]))
implemented_by {β} (f : (i : Nat) → α → i < n → β) (v : Vector α n) :
    v.mapFinIdx f = let v := v; .ofFn (fun i => f i.1 (v[i]) (by grind))

-- fold
implemented_by {β} (f : β → α → β) (init : β) (v : Vector α n) :
    v.foldl f init = let v := v; inlinedLoop n (fun i x => f x (v[i])) init
implemented_by {β} (f : α → β → β) (init : β) (v : Vector α n) :
    v.foldr f init = let v := v; inlinedLoop n (fun i x => f v[n-i-1] x) init

-- sum
implemented_by [Add α] [Zero α] (u : Vector α n) : u.sum = let u := u; u.foldl (· + ·) 0

-- zero
implemented_by [Zero α] : (0 : Vector α n) = Vector.zero (α:=α) (n:=n)
implemented_by (a : α) : Vector.replicate n a = .ofFn (fun _ => a)
