import HouLean.Math
import HouLean.Data.Defs
import HouLean.Data.Float
import HouLean.Data.Vector

open HouLean Math

namespace HouLean

instance {α} {m n : Nat} : GetElem? (Matrix α m n) (Nat×Nat) α (fun _ ij => ij.1 < m ∧ ij.2 < n) where
  getElem A ij h := A.data[ij.1][ij.2]
  getElem? A ij := A.data[ij.1]? |>.map (fun row => row[ij.2]?) |>.join
  getElem! A ij := A.data[ij.1]![ij.2]!

instance {α} {m n : Nat} : SetElem? (Matrix α m n) (Nat×Nat) α (fun _ ij => ij.1 < m ∧ ij.2 < n) where
  setElem A ij x h :=
    let (i,j) := ij
    ⟨A.data.set i (A.data[i].set j x) (by grind)⟩
  setElem? A ij x :=
    let (i,j) := ij
    if h : i < m ∧ j < n then
      some ⟨A.data.set i (A.data[i].set j x) (by get_elem_tactic)⟩
    else
      none
  setElem! A ij x :=
    let (i,j) := ij
    ⟨A.data.set! i (A.data[i]!.set! j x)⟩

namespace Matrix

-- Row and column extraction
def row (a : Matrix α m n) (i : Nat) (h : i < m := by get_elem_tactic) :
    Vector α n := a.data[i]

def col (a : Matrix α m n) (j : Nat) (h : j < n := by get_elem_tactic) :
    Vector α m := Vector.ofFn fun i => a[i.1,j]


def xx (a : Matrix α m n) (h : m > 0 ∧ n > 0 := by get_elem_tactic) : α := a[0,0]
def xy (a : Matrix α m n) (h : m > 0 ∧ n > 1 := by get_elem_tactic) : α := a[0,1]
def xz (a : Matrix α m n) (h : m > 0 ∧ n > 2 := by get_elem_tactic) : α := a[0,2]
def xw (a : Matrix α m n) (h : m > 0 ∧ n > 3 := by get_elem_tactic) : α := a[0,3]

def yx (a : Matrix α m n) (h : m > 1 ∧ n > 0 := by get_elem_tactic) : α := a[1,0]
def yy (a : Matrix α m n) (h : m > 1 ∧ n > 1 := by get_elem_tactic) : α := a[1,1]
def yz (a : Matrix α m n) (h : m > 1 ∧ n > 2 := by get_elem_tactic) : α := a[1,2]
def yw (a : Matrix α m n) (h : m > 1 ∧ n > 3 := by get_elem_tactic) : α := a[1,3]

def zx (a : Matrix α m n) (h : m > 2 ∧ n > 0 := by get_elem_tactic) : α := a[2,0]
def zy (a : Matrix α m n) (h : m > 2 ∧ n > 1 := by get_elem_tactic) : α := a[2,1]
def zz (a : Matrix α m n) (h : m > 2 ∧ n > 2 := by get_elem_tactic) : α := a[2,2]
def zw (a : Matrix α m n) (h : m > 2 ∧ n > 3 := by get_elem_tactic) : α := a[2,3]

def wx (a : Matrix α m n) (h : m > 3 ∧ n > 0 := by get_elem_tactic) : α := a[3,0]
def wy (a : Matrix α m n) (h : m > 3 ∧ n > 1 := by get_elem_tactic) : α := a[3,1]
def wz (a : Matrix α m n) (h : m > 3 ∧ n > 2 := by get_elem_tactic) : α := a[3,2]
def ww (a : Matrix α m n) (h : m > 3 ∧ n > 3 := by get_elem_tactic) : α := a[3,3]


-- todo: add support for `;` to separate row `#m[xx,xy; yx,yy]`
macro "#m[" rows:term,* "]" : term => `(Matrix.mk #v[ $rows,* ])

-- Matrix operations
def ofFn (f : (i j : Nat) → (h : i < m ∧ j < n) → α) : Matrix α m n :=
  ⟨.ofFn (fun i => .ofFn fun j => f i j (by grind))⟩

def mapRowsFinIdx (f : (i : Nat) →  Vector α n → (h : i < m) → Vector β n) (a : Matrix α m n) : Matrix β m n :=
  { data := a.data.mapFinIdx f }

def mapRowsIdx (f : Nat →  Vector α n → Vector β n) (a : Matrix α m n) : Matrix β m n :=
  a.mapRowsFinIdx (fun i v _ => f i v)

def mapRows (f : Vector α n → Vector β n) (a : Matrix α m n) : Matrix β m n :=
  a.mapRowsFinIdx (fun _ v _ => f v)


def mapFinIdx (f : (i j : Nat) → α → (h : i < m ∧ j < n) → β) (a : Matrix α m n) : Matrix β m n :=
  a.mapRowsFinIdx (fun i v _ => v.mapFinIdx (fun j x _ => f i j x (by grind)))

def mapIdx (f : Nat → Nat → α → β) (a : Matrix α m n) : Matrix β m n :=
  a.mapRowsFinIdx (fun i v _ => v.mapFinIdx (fun j x _ => f i j x))

def map (f : α → β) (a : Matrix α m n) : Matrix β m n :=
  a.mapRowsFinIdx (fun _ v _ => v.mapFinIdx (fun _ x _ => f x))

def mapRows₂ (f : Vector α n → Vector β n → Vector γ n)
    (a : Matrix α m n) (b : Matrix β m n) : Matrix γ m n :=
  a.mapRowsFinIdx (fun i v _ => f v (b.row i))

def transpose (a : Matrix α m n) : Matrix α n m :=
  ofFn (fun j i _ => a[i,j])

-- Identity matrix
def identity (α : Type) [Zero α] [One α] (n : Nat) : Matrix α n n :=
  ofFn (fun i j _ => if i = j then 1 else 0)

-- Zero matrix
def zero (α : Type) [Zero α] (m n : Nat) : Matrix α m n :=
  .mk (.ofFn (fun _ => 0))

def add [Add α] (a b : Matrix α m n) : Matrix α m n :=
  mapRows₂ (· + ·) a b

def sub [Sub α] (a b : Matrix α m n) : Matrix α m n :=
  mapRows₂ (· - ·) a b

def smul [Mul α] (s : α) (a : Matrix α m n) : Matrix α m n :=
  mapRows (s*·) a

def sdiv [Div α] (a : Matrix α m n) (s : α) : Matrix α m n :=
  mapRows (·/s) a

def matMul [Add α] [Mul α] [Zero α] (a : Matrix α m k) (b : Matrix α k n) : Matrix α m n :=
  ofFn (fun i j _ => (a.row i).dot (b.col j))

def vecMul [Add α] [Mul α] [Zero α] (v : Vector α m) (a : Matrix α m n) : Vector α n :=
  .ofFn fun j => v.dot (a.col j)

def mulVec [Add α] [Mul α] [Zero α] (a : Matrix α m n) (v : Vector α n) : Vector α m :=
  .ofFn fun i => (a.row i).dot v

-- Instances
instance [Add α] : Add (Matrix α m n) := ⟨add⟩
instance [Sub α] : Sub (Matrix α m n) := ⟨sub⟩
instance [Mul α] : HMul α (Matrix α m n) (Matrix α m n) := ⟨smul⟩
instance [Div α] : HDiv (Matrix α m n) α (Matrix α m n) := ⟨sdiv⟩

instance [Add α] [Zero α] [Mul α] : HMul (Vector α m) (Matrix α m n) (Vector α n) := ⟨vecMul⟩
instance [Add α] [Zero α] [Mul α] : HMul (Matrix α m k) (Matrix α k n) (Matrix α m n) := ⟨matMul⟩

end Matrix
namespace NormalMatrixVecMul
scoped instance [Add α] [Zero α] [Mul α] : HMul (Matrix α m n) (Vector α n) (Vector α m) := ⟨Matrix.mulVec⟩
end NormalMatrixVecMul
namespace HoudiniMatrixVecMul
scoped instance [Add α] [Zero α] [Mul α] : HMul (Matrix α m n) (Vector α m) (Vector α n) := ⟨fun a v => Matrix.vecMul v a⟩
end HoudiniMatrixVecMul
namespace Matrix

instance [Zero α] : Zero (Matrix α m n) where
  zero := zero α m n

instance [Zero α] [One α] : One (Matrix α n n) where
  one := identity α n

-- ToString instance for debugging
instance [ToString α] : ToString (Matrix α m n) where
  toString a :=
    "[" ++ String.intercalate ",\n " (a.data.toList.map fun row =>
      "[" ++ String.intercalate ", " (row.toList.map toString) ++ "]") ++ "]"



-- ============================================================================
-- Trigonometric Functions
-- ============================================================================

-- todo: make defun work for these

variable {α : Type} {m n : Nat}

defun sin [Sin α] (x : Matrix α m n) : Matrix α m n := x.map Math.sin
defun cos [Cos α] (x : Matrix α m n) : Matrix α m n := x.map Math.cos
defun tan [Tan α] (x : Matrix α m n) : Matrix α m n := x.map Math.tan
defun asin [Asin α] (x : Matrix α m n) : Matrix α m n := x.map Math.asin
defun acos [Acos α] (x : Matrix α m n) : Matrix α m n := x.map Math.acos
defun atan [Atan α] (x : Matrix α m n) : Matrix α m n := x.map Math.atan
defun atan2 [Atan2 α] (y x : Matrix α m n) : Matrix α m n := y.mapRowsFinIdx (fun i yi _ => yi.atan2 (x.row i))
defun sinh [Sinh α] (x : Matrix α m n) : Matrix α m n := x.map Math.sinh
defun cosh [Cosh α] (x : Matrix α m n) : Matrix α m n := x.map Math.cosh
defun tanh [Tanh α] (x : Matrix α m n) : Matrix α m n := x.map Math.tanh


-- ============================================================================
-- Exponential and Logarithmic Functions
-- ============================================================================

defun exp [Exp α] (x : Matrix α m n) : Matrix α m n := x.map Math.exp
defun exp2 [Exp2 α] (x : Matrix α m n) : Matrix α m n := x.map Math.exp2
defun log [Log α] (x : Matrix α m n) : Matrix α m n := x.map Math.log
defun log2 [Log2 α] (x : Matrix α m n) : Matrix α m n := x.map Math.log2
defun log10 [Log10 α] (x : Matrix α m n) : Matrix α m n := x.map Math.log10
defun sqrt [Sqrt α] (x : Matrix α m n) : Matrix α m n := x.map Math.sqrt
defun invsqrt [Invsqrt α] (x : Matrix α m n) : Matrix α m n := x.map Math.invsqrt


-- ============================================================================
-- Basic Arithmetic and Comparison
-- ============================================================================

defun abs [Abs α] (x : Matrix α m n) : Matrix α m n := x.map Math.abs
defun sign [Sign α] (x : Matrix α m n) : Matrix α m n := x.map Math.sign
defun clamp [Clamp α α] (x : Matrix α m n) (lo hi : α) : Matrix α m n := x.map (Math.clamp · lo hi)
defun floor [Floor α] (x : Matrix α m n) : Matrix α m n := x.map Math.floor
defun ceil [Ceil α] (x : Matrix α m n) : Matrix α m n := x.map Math.ceil
defun round [Round α] (x : Matrix α m n) : Matrix α m n := x.map Math.round
defun trunc [Trunc α] (x : Matrix α m n) : Matrix α m n := x.map Math.trunc
defun fract [Fract α] (x : Matrix α m n) : Matrix α m n := x.map Math.fract


-- ============================================================================
-- Approximatelly equal
-- ============================================================================

-- protected def compMin [Min α] [Inhabited α] (x : Matrix α m n) : α :=
--   x.toArray.joinl (map:=fun a => a) (fun a b => min a b)

-- protected def compMax [Max α] [Inhabited α] (x : Matrix α m n) : α :=
--   x.toArray.joinl (map:=fun a => a) (fun a b => max a b)

-- def approxEqual [Abs α] [Sub α] [Inhabited α] [Max α] [LE α] [DecidableLE α]
--     (x y : Matrix α m n) (tol : α) : Bool :=
--   (x - y).abs.compMax ≤ tol


-- ============================================================================
-- Vector Operations
-- ============================================================================

variable [Add α] [Sub α] [Mul α] [Div α] [Zero α]

defun dot (u v : Matrix α m n) : α := HouLean.sum (fun i : Fin m => (u.row i).dot (v.row i))

defun reflect [OfNat α 2] (v normal : Matrix α m n) : Matrix α m n :=
  let d := v.dot normal
  v - 2 * d * normal

defun refract [One α] [Sqrt α] [LT α] [DecidableLT α] (v normal : Matrix α m n) (eta : α) : Matrix α m n :=
  let dt := v.dot normal
  let k := 1 - eta * eta * (1 - dt * dt)
  if k < 0 then 0
  else
    let s := eta * dt + Math.sqrt k
    eta * v - s * normal

defun compMul (x y : Matrix α m n) : Matrix α m n :=
  x.mapRowsFinIdx (fun i xi _ => xi.compMul (y.row i))

defun compDiv (x y : Matrix α m n) : Matrix α m n :=
  x.mapRowsFinIdx (fun i xi _ => xi.compDiv (y.row i))


-- ============================================================================
-- Interpolation and Smoothing (elementwise)
-- ============================================================================

defun lerp [Lerp α α] (a b : Matrix α m n) (t : α) := a + t * (b - a)

defun smoothstep [Smoothstep α] (edge0 edge1 v : Matrix α m n) : Matrix α m n :=
  v.mapRowsFinIdx (fun i vi _ => Vector.smoothstep (edge0.row i) (edge1.row i) vi)

defun step [Step α] (edge v : Matrix α m n) : Matrix α m n :=
  v.mapRowsFinIdx (fun i vi _ => Vector.step (edge.row i) vi)

defun hermite [Hermite α α] (p0 p1 t0 t1 : Matrix α m n) (t : α) : Matrix α m n :=
  .ofFn fun i j _ => Math.hermite p0[i,j] p1[i,j] t0[i,j] t1[i,j] t

defun catmullRom [CatmullRom α α] (p0 p1 t0 t1 : Matrix α m n) (t : α) : Matrix α m n :=
  .ofFn fun i j _ => Math.catmullRom p0[i,j] p1[i,j] t0[i,j] t1[i,j] t


-- ============================================================================
-- Geometric Queries
-- ============================================================================

variable [One α] [Div α]

/-- Transform point. Computes `(point 1) * transform`.  -/
def transformPointLeft (transform : Matrix α (n+1) (n+1)) (point : Vector α n) : Vector α n :=

  let point' := Vector.ofFn fun j : Fin n =>
    transform[n,j.1] + HouLean.sum (fun k : Fin n => point[k] * transform[k.1,j.1])
  let w := transform[n,n] + HouLean.sum (fun k : Fin n => point[k] * transform[k.1,n])

  point' / w

/-- Transform point. Computes `transform * (point 1)`.  -/
def transformPointRight (transform : Matrix α (n+1) (n+1)) (point : Vector α n) : Vector α n :=

  let point' := Vector.ofFn fun i : Fin n =>
    transform[i.1,n] + HouLean.sum (fun k : Fin n => transform[i.1,k.1] * point[k])
  let w := transform[n,n] + HouLean.sum (fun k : Fin n => point[k] * transform[k.1,n])

  point' / w

/-- Transform vector. Computes `(vector 0) * transform` -/
def transformVectorLeft (transform : Matrix α (n+1) (n+1)) (vector : Vector α n) : Vector α n :=
  Vector.ofFn fun j : Fin n =>
    HouLean.sum (fun k : Fin n => vector[k] * transform[k.1,j.1])

/-- Transform vector. Computes `transform * (vector 0)` -/
def transformVectorRight (transform : Matrix α (n+1) (n+1)) (vector : Vector α n) : Vector α n :=
  Vector.ofFn fun i : Fin n =>
    HouLean.sum (fun k : Fin n => transform[i.1, k.1] * vector[k])
