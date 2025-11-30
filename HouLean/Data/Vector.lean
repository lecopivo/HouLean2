import HouLean.Math
import HouLean.Data.Defs

open Qq HouLean Math

variable {α : Type} {n : Nat}

-- todo: move this and add proper error message when grind fails!!!
def _root_.Vector.x (a : Vector α n) (h : n > 0 := by grind) : α := a[0]
def _root_.Vector.y (a : Vector α n) (h : n > 1 := by grind) : α := a[1]
def _root_.Vector.z (a : Vector α n) (h : n > 2 := by grind) : α := a[2]
def _root_.Vector.w (a : Vector α n) (h : n > 3 := by grind) : α := a[3]


instance [Mul α] : Mul (Vector α n) := ⟨fun u v => u.mapFinIdx (fun i ui _ => ui * v[i])⟩
-- instance [Mul α] : HMul (Vector α n) α (Vector α n) := ⟨fun v s => v.map (fun vi => vi * s)⟩
instance [Div α] : HDiv (Vector α n) α (Vector α n) := ⟨fun v s => v.map (fun vi => vi / s)⟩
instance [Div α] : Div (Vector α n) := ⟨fun u v => u.mapFinIdx (fun i ui _ => ui / v[i])⟩

namespace Vector

variable {α : Type} {n : Nat} [Add α] [Sub α] [Mul α] [Div α] [Zero α]

class ApproxEqual (α : Type) where
  defaultTol : α
  approxEqual (x y : α) (tol : α) : Bool

macro x:term " ≈ " y:term : term => `(ApproxEqual.approxEqual $x $y ApproxEqual.defaultTol)
macro x:term " ≈[" tol:term "] " y:term : term => `(ApproxEqual.approxEqual $x $y $tol)

-- defun does not work :(
def dot (u v : Vector α n) : α := Vector.sum (u*v : Vector α n)

def cross2 (u v : Vector α 2) : α := u.x * v.y - u.y * v.x
def cross3 (u v : Vector α 3) : Vector α 3 :=
  #v[u.y * v.z - u.z * v.y,
     u.z * v.x - u.x * v.z,
     u.x * v.y - u.y * v.x]
def cross4 (u v : Vector α 3) : Vector α 8 := sorry

def length2 (u : Vector α n) : α := u.dot u
def length [Sqrt α] (u : Vector α n) : α := sqrt u.length2

def normalize [Sqrt α] [ApproxEqual α] (u : Vector α n) : Vector α n × α :=
  let len := u.length
  if len ≈ 0 then
    (u, 0)
  else
    (u / len, len)

end Vector
