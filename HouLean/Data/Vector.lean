import HouLean.Math
import HouLean.Data.Defs
import HouLean.Data.Float

open Qq HouLean Math

variable {α : Type} {n : Nat}

-- todo: move this and add proper error message when grind fails!!!
def _root_.Vector.x (a : Vector α n) (h : 0 < n := by get_elem_tactic) : α := a[0]
def _root_.Vector.y (a : Vector α n) (h : 1 < n := by get_elem_tactic) : α := a[1]
def _root_.Vector.z (a : Vector α n) (h : 2 < n := by get_elem_tactic) : α := a[2]
def _root_.Vector.w (a : Vector α n) (h : 3 < n := by get_elem_tactic) : α := a[3]

instance : SetElem (Vector α n) Nat α (fun _ i => i < n) where
  setElem xs i x h := xs.set i x

instance [Mul α] : Mul (Vector α n) := ⟨fun u v => u.mapFinIdx (fun i ui _ => ui * v[i])⟩
-- instance [Mul α] : HMul (Vector α n) α (Vector α n) := ⟨fun v s => v.map (fun vi => vi * s)⟩
instance [Inv α] [Mul α] : HDiv (Vector α n) α (Vector α n) := ⟨fun v s => let is := s⁻¹; v.map (fun vi => is * vi)⟩
instance [Div α] : Div (Vector α n) := ⟨fun u v => u.mapFinIdx (fun i ui _ => ui / v[i])⟩


instance [Min α] : Min (Vector α n) := ⟨fun u v => u.mapFinIdx (fun i ui _ => min ui v[i])⟩
instance [Max α] : Max (Vector α n) := ⟨fun u v => u.mapFinIdx (fun i ui _ => max ui v[i])⟩

instance [ToString α] : ToString (Vector α n) := ⟨fun u => toString u.toArray⟩

def HouLean.basisVector (R : Type) [FloatType R] (n : Nat) (i : Nat) : Vector R n :=
  .ofFn (fun i' => if i = i' then 1 else 0)

namespace Vector

-- Implementation of HouLean.Math interface
section Math
open Math

-- ============================================================================
-- Trigonometric Functions
-- ============================================================================

defun sin [Sin α] (x : Vector α n) : Vector α n := x.map Math.sin
defun cos [Cos α] (x : Vector α n) : Vector α n := x.map Math.cos
defun tan [Tan α] (x : Vector α n) : Vector α n := x.map Math.tan
defun asin [Asin α] (x : Vector α n) : Vector α n := x.map Math.asin
defun acos [Acos α] (x : Vector α n) : Vector α n := x.map Math.acos
defun atan [Atan α] (x : Vector α n) : Vector α n := x.map Math.atan
defun atan2 [Atan2 α] (y x : Vector α n) : Vector α n := (y.zip x).map Math.atan2.uncurry
defun sinh [Sinh α] (x : Vector α n) : Vector α n := x.map Math.sinh
defun cosh [Cosh α] (x : Vector α n) : Vector α n := x.map Math.cosh
defun tanh [Tanh α] (x : Vector α n) : Vector α n := x.map Math.tanh


-- ============================================================================
-- Exponential and Logarithmic Functions
-- ============================================================================

defun exp [Exp α] (x : Vector α n) : Vector α n := x.map Math.exp
defun exp2 [Exp2 α] (x : Vector α n) : Vector α n := x.map Math.exp2
defun log [Log α] (x : Vector α n) : Vector α n := x.map Math.log
defun log2 [Log2 α] (x : Vector α n) : Vector α n := x.map Math.log2
defun log10 [Log10 α] (x : Vector α n) : Vector α n := x.map Math.log10
defun sqrt [Sqrt α] (x : Vector α n) : Vector α n := x.map Math.sqrt
defun invsqrt [Invsqrt α] (x : Vector α n) : Vector α n := x.map Math.invsqrt


-- ============================================================================
-- Basic Arithmetic and Comparison
-- ============================================================================

defun abs [Abs α] (x : Vector α n) : Vector α n := x.map Math.abs
defun sign [Sign α] (x : Vector α n) : Vector α n := x.map Math.sign
defun clamp [Clamp α α] (x : Vector α n) (lo hi : α) : Vector α n := x.map (Math.clamp · lo hi)
defun clamp [Clamp α α] (x : Vector α n) (lo hi : Vector α n) : Vector α n :=
  x.mapFinIdx (fun i xi _ => Math.clamp xi lo[i] hi[i])
defun floor [Floor α] (x : Vector α n) : Vector α n := x.map Math.floor
defun ceil [Ceil α] (x : Vector α n) : Vector α n := x.map Math.ceil
defun round [Round α] (x : Vector α n) : Vector α n := x.map Math.round
defun trunc [Trunc α] (x : Vector α n) : Vector α n := x.map Math.trunc
defun fract [Fract α] (x : Vector α n) : Vector α n := x.map Math.fract


-- ============================================================================
-- Approximatelly equal
-- ============================================================================

protected def compMin [Min α] [Inhabited α] (x : Vector α n) : α :=
  x.toArray.joinl (map:=fun a => a) (fun a b => min a b)

protected def compMax [Max α] [Inhabited α] (x : Vector α n) : α :=
  x.toArray.joinl (map:=fun a => a) (fun a b => max a b)

def approxEqual {R} [FloatType R] (x y : Vector R n) (tol : R) : Bool :=
  (x - y).abs.compMax ≤ tol

instance {R} [FloatType R] : ApproxEqual (Vector R n) R where
  defaultTol := ApproxEqual.defaultTol R
  approxEqual x y tol := approxEqual x y tol


-- ============================================================================
-- Vector Operations
-- ============================================================================

variable [Add α] [Sub α] [Mul α] [Div α] [Zero α] [Inv α]

defun dot (u v : Vector α n) : α := ∑ (i : Fin n), u[i]*v[i]
-- todo: unify the to `cross` once defun works for these
def cross2 (u v : Vector α 2) : α := u.x * v.y - u.y * v.x
def cross3 (u v : Vector α 3) : Vector α 3 :=
  #v[u.y * v.z - u.z * v.y,
     u.z * v.x - u.x * v.z,
     u.x * v.y - u.y * v.x]
-- def cross4 (u v : Vector α 3) : Vector α 8 := sorry
defun length2 (u : Vector α n) : α := HouLean.sum (fun i : Fin n => u[i]*u[i])
defun length [Sqrt α] (u : Vector α n) : α := Math.sqrt u.length2
defun distance2 (u v : Vector α n) : α := (u-v).length2
defun distance [Sqrt α] (u v : Vector α n) : α := Math.sqrt (u.distance2 v)
defun normalize [Sqrt α] [ApproxEqual α α] (u : Vector α n) : Vector α n × α :=
  let len := u.length
  if len ≈ 0 then
    (u, 0)
  else
    (u / len, len)
defun normalized [Sqrt α] [ApproxEqual α α] (u : Vector α n) : Vector α n :=
  u.normalize.1

defun reflect [OfNat α 2] (v normal : Vector α n) : Vector α n :=
  let d := v.dot normal
  v - 2 * d * normal

defun refract [One α] [Sqrt α] [LT α] [DecidableLT α] (v normal : Vector α n) (eta : α) : Vector α n :=
  let dt := v.dot normal
  let k := 1 - eta * eta * (1 - dt * dt)
  if k < 0 then 0
  else
    let s := eta * dt + Math.sqrt k
    eta * v - s * normal

defun compMul (x y : Vector α n) : Vector α n :=
  x.mapFinIdx (fun i xi _ => xi * y[i])

defun compDiv (x y : Vector α n) : Vector α n :=
  x.mapFinIdx (fun i xi _ => xi / y[i])


-- ============================================================================
-- Interpolation and Smoothing (elementwise)
-- ============================================================================

defun lerp [Lerp α α] (x y : Vector α n) (t : α) := x + t * (y - x)

defun smoothstep [Smoothstep α] (edge0 edge1 v : Vector α n) : Vector α n :=
  v.mapFinIdx (fun i vi _ => Math.smoothstep edge0[i] edge1[i] vi)

defun step [Step α] (edge v : Vector α n) : Vector α n :=
  v.mapFinIdx (fun i vi _ => Math.step edge[i] vi)

defun hermite [Hermite α α] (p0 p1 t0 t1 : Vector α n) (t : α) : Vector α n :=
  .ofFn fun i => Math.hermite p0[i] p1[i] t0[i] t1[i] t

defun catmullRom [CatmullRom α α] (p0 p1 t0 t1 : Vector α n) (t : α) : Vector α n :=
  .ofFn fun i => Math.catmullRom p0[i] p1[i] t0[i] t1[i] t

defun slerp {R : Type} [FloatType R]
    (v w : Vector R n) (t : R) : Vector R n :=
  let d := v.normalized.dot w.normalized
  let d := Math.clamp d (-1:R) (1:R)
  let theta := Math.acos d
  if theta ≈ 0 then
    Math.lerp v w t
  else
    let s := Math.sin theta
    let a := (Math.sin ((1 - t) * theta)) / s
    let b := (Math.sin (t * theta)) / s
    a * v + b * w



-- ============================================================================
-- Geometric Queries
-- ============================================================================

defun insideBox [LE α] [DecidableLE α] (point boxMin boxMax : Vector α n) : Bool :=
  decide (∀ i : Fin n, boxMin[i] ≤ point[i] ∧ point[i] ≤ boxMax[i])

defun projectToSegment [Clamp α α] [Zero α] [One α] (point a b : Vector α n) : Vector α n :=
  let ab := b - a
  let ap := point - a
  let t := Math.clamp ((ap.dot ab) / ab.length2) (0:α) (1:α)
  a + t * ab


end Math
end Vector
