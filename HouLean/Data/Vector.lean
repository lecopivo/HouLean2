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


-- Implementation of HouLean.Math interface
section Math
open Math

-- ============================================================================
-- Trigonometric Functions
-- ============================================================================

-- todo: make defun work for these

def sin [Sin α] (x : Vector α n) : Vector α n := x.map Math.sin
def cos [Cos α] (x : Vector α n) : Vector α n := x.map Math.cos
def tan [Tan α] (x : Vector α n) : Vector α n := x.map Math.tan
def asin [Asin α] (x : Vector α n) : Vector α n := x.map Math.asin
def acos [Acos α] (x : Vector α n) : Vector α n := x.map Math.acos
def atan [Atan α] (x : Vector α n) : Vector α n := x.map Math.atan
def atan2 [Atan2 α] (y x : Vector α n) : Vector α n := (y.zip x).map Math.atan2.uncurry
def sinh [Sinh α] (x : Vector α n) : Vector α n := x.map Math.sinh
def cosh [Cosh α] (x : Vector α n) : Vector α n := x.map Math.cosh
def tanh [Tanh α] (x : Vector α n) : Vector α n := x.map Math.tanh


-- ============================================================================
-- Exponential and Logarithmic Functions
-- ============================================================================

def exp [Exp α] (x : Vector α n) : Vector α n := x.map Math.exp
def exp2 [Exp2 α] (x : Vector α n) : Vector α n := x.map Math.exp2
def log [Log α] (x : Vector α n) : Vector α n := x.map Math.log
def log2 [Log2 α] (x : Vector α n) : Vector α n := x.map Math.log2
def log10 [Log10 α] (x : Vector α n) : Vector α n := x.map Math.log10
def sqrt [Sqrt α] (x : Vector α n) : Vector α n := x.map Math.sqrt
def invsqrt [Invsqrt α] (x : Vector α n) : Vector α n := x.map Math.invsqrt


-- ============================================================================
-- Basic Arithmetic and Comparison
-- ============================================================================

def abs [Abs α] (x : Vector α n) : Vector α n := x.map Math.abs
def sign [Sign α] (x : Vector α n) : Vector α n := x.map Math.sign
def clamp [Clamp α] (x : Vector α n) (lo hi : α) : Vector α n := x.map (Math.clamp · lo hi)
def floor [Floor α] (x : Vector α n) : Vector α n := x.map Math.floor
def ceil [Ceil α] (x : Vector α n) : Vector α n := x.map Math.ceil
def round [Round α] (x : Vector α n) : Vector α n := x.map Math.round
def trunc [Trunc α] (x : Vector α n) : Vector α n := x.map Math.trunc
def fract [Fract α] (x : Vector α n) : Vector α n := x.map Math.fract


-- ============================================================================
-- Approximatelly equal
-- ============================================================================

-- def approxEqual [Abs α] [Sub α] [LE α] (x y : Vector α n) (tol : α) : Bool :=
--   (x - y).abs ≤ Vector.replicate n tol


-- ============================================================================
-- Vector Operations
-- ============================================================================

variable [Add α] [Sub α] [Mul α] [Div α] [Zero α]

def dot (u v : Vector α n) : α := Vector.sum (u*v : Vector α n)
-- todo: unify the to `cross` once defun works for these
def cross2 (u v : Vector α 2) : α := u.x * v.y - u.y * v.x
def cross3 (u v : Vector α 3) : Vector α 3 :=
  #v[u.y * v.z - u.z * v.y,
     u.z * v.x - u.x * v.z,
     u.x * v.y - u.y * v.x]
-- def cross4 (u v : Vector α 3) : Vector α 8 := sorry
def length2 (u : Vector α n) : α := u.dot u
def length [Sqrt α] (u : Vector α n) : α := Math.sqrt u.length2
def distance2 (u v : Vector α n) : α := Vector.sum (u*v : Vector α n)
def distance [Sqrt α] (u v : Vector α n) : α := Math.sqrt (u.distance2 v)
def normalize [Sqrt α] [ApproxEqual α] (u : Vector α n) : Vector α n × α :=
  let len := u.length
  if len ≈ 0 then
    (u, 0)
  else
    (u / len, len)
def normalized [Sqrt α] [ApproxEqual α] (u : Vector α n) : Vector α n :=
  u.normalize.1


end Math
end Vector
