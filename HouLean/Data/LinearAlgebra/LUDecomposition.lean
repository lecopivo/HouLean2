import HouLean.LinearAlgebra
import HouLean.Data.Defs
import HouLean.Data.Float
import HouLean.Data.Vector
import HouLean.Data.Matrix

open HouLean Math LinearAlgebra

namespace HouLean.Matrix

variable {α : Type} {n : Nat}

-- ============================================================================
-- LU Decomposition (Doolittle algorithm: L has 1s on diagonal)
-- A = L * U
-- ============================================================================

/-- LU decomposition for 2x2 matrix without pivoting.
    Returns (L, U) where A = L * U -/
def lu2 [Mul α] [Div α] [Sub α] [One α] [Zero α] (a : Matrix α 2 2) : Matrix α 2 2 × Matrix α 2 2 :=
  -- U[0,0] = a[0,0], U[0,1] = a[0,1]
  -- L[1,0] = a[1,0] / U[0,0]
  -- U[1,1] = a[1,1] - L[1,0] * U[0,1]
  let u00 := a[0,0]
  let u01 := a[0,1]
  let l10 := a[1,0] / u00
  let u11 := a[1,1] - l10 * u01
  let L := #m[1, 0; l10, 1]
  let U := #m[u00, u01; 0, u11]
  (L, U)

/-- LU decomposition for 3x3 matrix without pivoting.
    Returns (L, U) where A = L * U -/
def lu3 [Mul α] [Div α] [Sub α] [One α] [Zero α] (a : Matrix α 3 3) : Matrix α 3 3 × Matrix α 3 3 :=
  -- Row 0 of U
  let u00 := a[0,0]
  let u01 := a[0,1]
  let u02 := a[0,2]
  -- Column 0 of L
  let l10 := a[1,0] / u00
  let l20 := a[2,0] / u00
  -- Row 1 of U
  let u11 := a[1,1] - l10 * u01
  let u12 := a[1,2] - l10 * u02
  -- Column 1 of L
  let l21 := (a[2,1] - l20 * u01) / u11
  -- Row 2 of U
  let u22 := a[2,2] - l20 * u02 - l21 * u12
  let L := #m[1,   0,   0;
              l10, 1,   0;
              l20, l21, 1]
  let U := #m[u00, u01, u02;
              0,   u11, u12;
              0,   0,   u22]
  (L, U)

/-- LU decomposition for 4x4 matrix without pivoting.
    Returns (L, U) where A = L * U -/
def lu4 [Mul α] [Div α] [Sub α] [One α] [Zero α] (a : Matrix α 4 4) : (Matrix α 4 4) × (Matrix α 4 4) :=
  -- Row 0 of U
  let u00 := a[0,0]
  let u01 := a[0,1]
  let u02 := a[0,2]
  let u03 := a[0,3]
  -- Column 0 of L
  let l10 := a[1,0] / u00
  let l20 := a[2,0] / u00
  let l30 := a[3,0] / u00
  -- Row 1 of U
  let u11 := a[1,1] - l10 * u01
  let u12 := a[1,2] - l10 * u02
  let u13 := a[1,3] - l10 * u03
  -- Column 1 of L
  let l21 := (a[2,1] - l20 * u01) / u11
  let l31 := (a[3,1] - l30 * u01) / u11
  -- Row 2 of U
  let u22 := a[2,2] - l20 * u02 - l21 * u12
  let u23 := a[2,3] - l20 * u03 - l21 * u13
  -- Column 2 of L
  let l32 := (a[3,2] - l30 * u02 - l31 * u12) / u22
  -- Row 3 of U
  let u33 := a[3,3] - l30 * u03 - l31 * u13 - l32 * u23
  let L := #m[1,   0,   0,   0;
              l10, 1,   0,   0;
              l20, l21, 1,   0;
              l30, l31, l32, 1]
  let U := #m[u00, u01, u02, u03;
              0,   u11, u12, u13;
              0,   0,   u22, u23;
              0,   0,   0,   u33]
  (L, U)


-- /-- General LU decomposition without pivoting using Doolittle algorithm.
--     Returns (L, U) where A = L * U -/
-- def luGeneral [Mul α] [Div α] [Sub α] [One α] [Zero α] (a : Matrix α n n) : Matrix α n n × Matrix α n n :=
--   let rec computeLU (k : Nat) (L U : Matrix α n n) : Matrix α n n × Matrix α n n :=
--     if h : k < n then
--       -- Compute U[k, j] for j >= k
--       let U := (List.range (n - k)).foldl (init := U) fun U j' =>
--         let j := k + j'
--         if hj : j < n then
--           let sum := (List.range k).foldl (init := (0 : α)) fun acc i =>
--             if hi : i < n then acc + L[k,i] * U[i,j] else acc
--           U := setElem U (k,j) (a[k,j] - sum)
--         else U
--       -- Compute L[i, k] for i > k
--       let L := (List.range (n - k - 1)).foldl (init := L) fun L i' =>
--         let i := k + 1 + i'
--         if hi : i < n then
--           let sum := (List.range k).foldl (init := (0 : α)) fun acc j =>
--             if hj : j < n then acc + L[i,j] * U[j,k] else acc
--           L[(i,k)] := (a[i,k] - sum) / U[k,k]
--         else L
--       computeLU (k + 1) L U
--     else (L, U)
--   termination_by n - k
--   let L := Matrix.identity α n
--   let U := Matrix.zero α n n
--   computeLU 0 L U

defun luDecompositionNoPivot [Mul α] [Div α] [Sub α] [One α] [Zero α]
    (a : Matrix α n n) : Matrix α n n × Matrix α n n :=
  match n with
  | 0 => (a, a)
  | 1 => (#m[#v[1]], #m[#v[a[0,0]]])
  | 2 => lu2 a
  | 3 => lu3 a
  | 4 => lu4 a
  | n + 5 =>
    have : Inhabited (Matrix α (n+5) (n+5)) := ⟨0⟩
    panic! "LU decomposition is supported only for small matrices, n≤4" -- luGeneral a

-- defun luLowerMatrix [Mul α] [Div α] [Sub α] [One α] [Zero α]
--     (a : Matrix α n n) : Matrix α n n :=
--   (luDecompositionNoPivot a).1

-- defun luUpperMatrix [Mul α] [Div α] [Sub α] [One α] [Zero α]
--     (a : Matrix α n n) : Matrix α n n :=
--   (luDecompositionNoPivot a).2

-- -- ============================================================================
-- -- Forward and Back Substitution
-- -- ============================================================================

/-- Forward substitution: solve Lx = b where L is lower triangular with 1s on diagonal -/
def forwardSub2 [Mul α] [Sub α] (L : Matrix α 2 2) (b : Vector α 2) : Vector α 2 :=
  let x0 := b[0]
  let x1 := b[1] - L[1,0] * x0
  #v[x0, x1]

def forwardSub3 [Mul α] [Sub α] (L : Matrix α 3 3) (b : Vector α 3) : Vector α 3 :=
  let x0 := b[0]
  let x1 := b[1] - L[1,0] * x0
  let x2 := b[2] - L[2,0] * x0 - L[2,1] * x1
  #v[x0, x1, x2]

def forwardSub4 [Mul α] [Sub α] (L : Matrix α 4 4) (b : Vector α 4) : Vector α 4 :=
  let x0 := b[0]
  let x1 := b[1] - L[1,0] * x0
  let x2 := b[2] - L[2,0] * x0 - L[2,1] * x1
  let x3 := b[3] - L[3,0] * x0 - L[3,1] * x1 - L[3,2] * x2
  #v[x0, x1, x2, x3]

-- def forwardSubGeneral [Mul α] [Sub α] [Zero α] (L : Matrix α n n) (b : Vector α n) : Vector α n :=
--   let rec solve (i : Nat) (x : Vector α n) : Vector α n :=
--     if h : i < n then
--       let sum := (List.range i).foldl (init := (0 : α)) fun acc j =>
--         if hj : j < n then acc + L[i,j] * x[j]! else acc
--       let x := x.set i (b[i]! - sum) (by omega)
--       solve (i + 1) x
--     else x
--   termination_by n - i
--   solve 0 (Vector.ofFn fun _ => 0)

defun forwardSubstitution [Mul α] [Sub α] [Zero α] [One α]
    (L : Matrix α n n) (b : Vector α n) : Vector α n :=
  match n with
  | 0 => b
  | 1 => b
  | 2 => forwardSub2 L b
  | 3 => forwardSub3 L b
  | 4 => forwardSub4 L b
  | n + 5 =>
    have : Inhabited (Vector α (n+5)) := ⟨0⟩
    panic! "Forward Substitution is supported only for small matrices, n≤4!"


/-- Back substitution: solve Ux = b where U is upper triangular -/
def backSub2 [Mul α] [Sub α] [Div α] (U : Matrix α 2 2) (b : Vector α 2) : Vector α 2 :=
  let x1 := b[1] / U[1,1]
  let x0 := (b[0] - U[0,1] * x1) / U[0,0]
  #v[x0, x1]

def backSub3 [Mul α] [Sub α] [Div α] (U : Matrix α 3 3) (b : Vector α 3) : Vector α 3 :=
  let x2 := b[2] / U[2,2]
  let x1 := (b[1] - U[1,2] * x2) / U[1,1]
  let x0 := (b[0] - U[0,1] * x1 - U[0,2] * x2) / U[0,0]
  #v[x0, x1, x2]

def backSub4 [Mul α] [Sub α] [Div α] (U : Matrix α 4 4) (b : Vector α 4) : Vector α 4 :=
  let x3 := b[3] / U[3,3]
  let x2 := (b[2] - U[2,3] * x3) / U[2,2]
  let x1 := (b[1] - U[1,2] * x2 - U[1,3] * x3) / U[1,1]
  let x0 := (b[0] - U[0,1] * x1 - U[0,2] * x2 - U[0,3] * x3) / U[0,0]
  #v[x0, x1, x2, x3]

-- def backSubGeneral [Mul α] [Sub α] [Div α] [Zero α] (U : Matrix α n n) (b : Vector α n) : Vector α n :=
--   let rec solve (i : Nat) (x : Vector α n) : Vector α n :=
--     if h : i < n then
--       let idx := n - 1 - i
--       if hidx : idx < n then
--         let sum := (List.range i).foldl (init := (0 : α)) fun acc j =>
--           let col := n - 1 - j
--           if hcol : col < n then acc + U[idx, col] * x[col]! else acc
--         let x := x.set idx ((b[idx]! - sum) / U[idx, idx]) (by omega)
--         solve (i + 1) x
--       else x
--     else x
--   termination_by n - i
--   solve 0 (Vector.ofFn fun _ => 0)

defun backSubstitution [Mul α] [Sub α] [Div α] [Zero α]
    (U : Matrix α n n) (b : Vector α n) : Vector α n :=
  match n with
  | 0 => b
  | 1 => #v[b[0] / U[0,0]]
  | 2 => backSub2 U b
  | 3 => backSub3 U b
  | 4 => backSub4 U b
  | n + 5 =>
    have : Inhabited (Vector α (n+5)) := ⟨0⟩
    panic! "Back Substitution is supported only for small matrices, n≤4!"

-- -- ============================================================================
-- -- Solve Linear System using LU
-- -- ============================================================================

/-- Solve Ax = b using LU decomposition (without pivoting) -/
defun solveLU [Mul α] [Div α] [Sub α] [One α] [Zero α]
    (A : Matrix α n n) (b : Vector α n) : Vector α n :=
  let (L, U) := luDecompositionNoPivot A
  -- Solve Ly = b
  let y := forwardSubstitution L b
  -- Solve Ux = y
  backSubstitution U y


-- -- ============================================================================
-- -- LU with Partial Pivoting (PA = LU)
-- -- ============================================================================

-- -- TODO: Implement LU with partial pivoting
-- -- This requires tracking row swaps and is more complex
-- -- defun luDecomposition (a : Matrix α n n) : Matrix α n n × Matrix α n n × Matrix α n n := sorry

-- end HouLean.Matrix
