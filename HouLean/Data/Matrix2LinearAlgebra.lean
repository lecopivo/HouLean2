import HouLean.LinearAlgebra
import HouLean.Data.Float
import HouLean.Data.Defs

open HouLean.LinearAlgebra

namespace HouLean.Matrix2

-- Note: Matrix2 is row-based: structure Matrix2 where row0 row1 : Vector2

-- ============================================================================
-- Helper Functions
-- ============================================================================

def get (m : Matrix2) (i j : Int) : Float :=
  match i, j with
  | 0, 0 => m.row0.x | 0, 1 => m.row0.y
  | 1, 0 => m.row1.x | 1, 1 => m.row1.y
  | _, _ => 0.0

def matmul (a b : Matrix2) : Matrix2 :=
  ⟨⟨a.row0.x * b.row0.x + a.row0.y * b.row1.x,
    a.row0.x * b.row0.y + a.row0.y * b.row1.y⟩,
   ⟨a.row1.x * b.row0.x + a.row1.y * b.row1.x,
    a.row1.x * b.row0.y + a.row1.y * b.row1.y⟩⟩

def mulVec (m : Matrix2) (v : Vector2) : Vector2 :=
  ⟨m.row0.x * v.x + m.row0.y * v.y,
   m.row1.x * v.x + m.row1.y * v.y⟩

-- ============================================================================
-- Basic Matrix Operations
-- ============================================================================

defun transpose (m : Matrix2) : Matrix2 :=
  ⟨⟨m.row0.x, m.row1.x⟩,
   ⟨m.row0.y, m.row1.y⟩⟩

defun determinant (m : Matrix2) : Float :=
  m.row0.x * m.row1.y - m.row0.y * m.row1.x

defun trace (m : Matrix2) : Float :=
  m.row0.x + m.row1.y

defun inverse (m : Matrix2) : Matrix2 :=
  let det := m.determinant
  if det == 0 then ⟨⟨1, 0⟩, ⟨0, 1⟩⟩  -- Identity matrix
  else
    let invDet := 1.0 / det
    ⟨⟨m.row1.y * invDet, -m.row0.y * invDet⟩,
     ⟨-m.row1.x * invDet, m.row0.x * invDet⟩⟩

defun rank (m : Matrix2) : Int :=
  let det := m.determinant
  if det != 0 then 2
  else if m.row0.x != 0 || m.row0.y != 0 || m.row1.x != 0 || m.row1.y != 0 then 1
  else 0

defun isSingular (m : Matrix2) : Bool := m.determinant == 0

defun isInvertible (m : Matrix2) : Bool := m.determinant != 0

-- Forward declaration issue - define singularValues first
def singularValuesImpl (m : Matrix2) : Vector2 :=
  let mtm := m.transpose.matmul m
  let t := mtm.row0.x + mtm.row1.y
  let d := mtm.row0.x * mtm.row1.y - mtm.row0.y * mtm.row1.x
  let discriminant := t * t - 4.0 * d
  if discriminant < 0 then ⟨t / 2.0, t / 2.0⟩
  else
    let sd := discriminant.sqrt
    let ev1 := (t + sd) / 2.0
    let ev2 := (t - sd) / 2.0
    ⟨(Max.max ev1 ev2).sqrt, (Min.min ev1 ev2).sqrt⟩

defun conditionNumber (m : Matrix2) : Float :=
  let sv := m.singularValuesImpl
  if sv.y == 0 then 1.0e10 else sv.x / sv.y  -- Use large number instead of Float.inf

-- ============================================================================
-- Matrix Properties
-- ============================================================================

defun isSymmetric (m : Matrix2) : Bool :=
  m.row0.y == m.row1.x

defun isOrthogonal (m : Matrix2) : Bool :=
  let mt := m.transpose
  let prod := m.matmul mt
  (prod.row0.x - 1.0).abs < 0.0001 && prod.row0.y.abs < 0.0001 &&
  prod.row1.x.abs < 0.0001 && (prod.row1.y - 1.0).abs < 0.0001

defun isDiagonal (m : Matrix2) : Bool :=
  m.row0.y == 0 && m.row1.x == 0

defun isUpperTriangular (m : Matrix2) : Bool :=
  m.row1.x == 0

defun isLowerTriangular (m : Matrix2) : Bool :=
  m.row0.y == 0

-- Forward declaration for symmetricEigenvalues
def symmetricEigenvaluesImpl (m : Matrix2) : Vector2 :=
  let t := m.trace
  let d := m.determinant
  let discriminant := t * t - 4.0 * d
  if discriminant < 0 then ⟨t / 2.0, t / 2.0⟩
  else
    let sd := discriminant.sqrt
    let ev1 := (t + sd) / 2.0
    let ev2 := (t - sd) / 2.0
    ⟨Max.max ev1 ev2, Min.min ev1 ev2⟩

defun isPositiveDefinite (m : Matrix2) : Bool :=
  if !m.isSymmetric then false
  else
    let ev := m.symmetricEigenvaluesImpl
    ev.x > 0 && ev.y > 0

-- ============================================================================
-- Matrix Norms
-- ============================================================================

defun frobeniusNorm (m : Matrix2) : Float :=
  Float.sqrt (m.row0.x * m.row0.x + m.row0.y * m.row0.y +
              m.row1.x * m.row1.x + m.row1.y * m.row1.y)

defun norm1 (m : Matrix2) : Float :=
  Max.max (Float.abs m.row0.x + Float.abs m.row1.x)
          (Float.abs m.row0.y + Float.abs m.row1.y)

defun normInf (m : Matrix2) : Float :=
  Max.max (Float.abs m.row0.x + Float.abs m.row0.y)
          (Float.abs m.row1.x + Float.abs m.row1.y)

defun norm2 (m : Matrix2) : Float :=
  let sv := m.singularValuesImpl
  Max.max sv.x sv.y

-- ============================================================================
-- Linear System Solving
-- ============================================================================

defun solve (m : Matrix2) (b : Vector2) : Vector2 :=
  let det := m.determinant
  if det == 0 then b
  else
    let x := (b.x * m.row1.y - b.y * m.row0.y) / det
    let y := (m.row0.x * b.y - m.row1.x * b.x) / det
    ⟨x, y⟩

defun solveGaussian (m : Matrix2) (b : Vector2) : Vector2 :=
  m.solve b

defun solveLU (m : Matrix2) (b : Vector2) : Vector2 :=
  m.solve b

defun solveCholesky (m : Matrix2) (b : Vector2) : Vector2 :=
  let l00 := m.row0.x.sqrt
  if l00 == 0 then b else
  let l10 := m.row1.x / l00
  let l11 := (m.row1.y - l10 * l10).sqrt
  -- Forward substitution: Ly = b
  let y0 := b.x / l00
  let y1 := (b.y - l10 * y0) / l11
  -- Back substitution: L^T x = y
  let x1 := y1 / l11
  let x0 := (y0 - l10 * x1) / l00
  ⟨x0, x1⟩

defun solveQR (m : Matrix2) (b : Vector2) : Vector2 :=
  m.solve b

-- ============================================================================
-- LU Decomposition
-- ============================================================================

defun luDecomposition (m : Matrix2) : Matrix2 × Matrix2 × Matrix2 :=
  if m.row0.x == 0 then
    (⟨⟨1, 0⟩, ⟨0, 1⟩⟩, m, ⟨⟨1, 0⟩, ⟨0, 1⟩⟩)
  else
    let l21 := m.row1.x / m.row0.x
    let u11 := m.row1.y - l21 * m.row0.y
    let l : Matrix2 := ⟨⟨1, 0⟩, ⟨l21, 1⟩⟩
    let u : Matrix2 := ⟨⟨m.row0.x, m.row0.y⟩, ⟨0, u11⟩⟩
    let p : Matrix2 := ⟨⟨1, 0⟩, ⟨0, 1⟩⟩
    (l, u, p)

defun luDecompositionNoPivot (m : Matrix2) : Matrix2 × Matrix2 :=
  let (l, u, _) := m.luDecomposition
  (l, u)

defun luLowerMatrix (m : Matrix2) : Matrix2 :=
  let (l, _, _) := m.luDecomposition
  l

defun luUpperMatrix (m : Matrix2) : Matrix2 :=
  let (_, u, _) := m.luDecomposition
  u

-- ============================================================================
-- Cholesky Decomposition
-- ============================================================================

defun choleskyDecomposition (m : Matrix2) : Matrix2 :=
  let l00 := m.row0.x.sqrt
  if l00 == 0 then ⟨⟨1, 0⟩, ⟨0, 1⟩⟩ else
  let l10 := m.row1.x / l00
  let l11 := (m.row1.y - l10 * l10).sqrt
  ⟨⟨l00, 0⟩, ⟨l10, l11⟩⟩

defun choleskyModified (m : Matrix2) : Matrix2 :=
  let epsilon := 0.0001
  let d0 := Max.max m.row0.x epsilon
  let l00 := d0.sqrt
  let l10 := m.row1.x / l00
  let d1 := Max.max (m.row1.y - l10 * l10) epsilon
  let l11 := d1.sqrt
  ⟨⟨l00, 0⟩, ⟨l10, l11⟩⟩

-- ============================================================================
-- LDLT Decomposition
-- ============================================================================

defun ldltDecomposition (m : Matrix2) : Matrix2 × Matrix2 :=
  let d0 := m.row0.x
  if d0 == 0 then (⟨⟨1, 0⟩, ⟨0, 1⟩⟩, ⟨⟨0, 0⟩, ⟨0, 0⟩⟩) else
  let l10 := m.row1.x / d0
  let d1 := m.row1.y - l10 * l10 * d0
  let l : Matrix2 := ⟨⟨1, 0⟩, ⟨l10, 1⟩⟩
  let d : Matrix2 := ⟨⟨d0, 0⟩, ⟨0, d1⟩⟩
  (l, d)

defun ldltLowerMatrix (m : Matrix2) : Matrix2 :=
  let (l, _) := m.ldltDecomposition
  l

defun ldltDiagonalMatrix (m : Matrix2) : Matrix2 :=
  let (_, d) := m.ldltDecomposition
  d

-- ============================================================================
-- QR Decomposition
-- ============================================================================

defun qrDecomposition (m : Matrix2) : Matrix2 × Matrix2 :=
  -- Column vectors
  let c0 : Vector2 := ⟨m.row0.x, m.row1.x⟩
  let c1 : Vector2 := ⟨m.row0.y, m.row1.y⟩
  
  -- Q column 0
  let q0Len := (c0.x * c0.x + c0.y * c0.y).sqrt
  let q0 := if q0Len == 0 then c0 else ⟨c0.x / q0Len, c0.y / q0Len⟩
  
  -- Q column 1
  let proj := (c1.x * q0.x + c1.y * q0.y)
  let c1_orth : Vector2 := ⟨c1.x - proj * q0.x, c1.y - proj * q0.y⟩
  let q1Len := (c1_orth.x * c1_orth.x + c1_orth.y * c1_orth.y).sqrt
  let q1 : Vector2 := if q1Len == 0 then c1_orth else ⟨c1_orth.x / q1Len, c1_orth.y / q1Len⟩
  
  -- Construct Q
  let q : Matrix2 := ⟨⟨q0.x, q1.x⟩, ⟨q0.y, q1.y⟩⟩
  
  -- R = Q^T * A
  let r := q.transpose.matmul m
  
  (q, r)

defun qrGramSchmidt (m : Matrix2) : Matrix2 × Matrix2 :=
  m.qrDecomposition

defun qrHouseholder (m : Matrix2) : Matrix2 × Matrix2 :=
  m.qrDecomposition

defun qrOrthogonalMatrix (m : Matrix2) : Matrix2 :=
  let (q, _) := m.qrDecomposition
  q

defun qrUpperMatrix (m : Matrix2) : Matrix2 :=
  let (_, r) := m.qrDecomposition
  r

-- ============================================================================
-- Singular Value Decomposition
-- ============================================================================

defun singularValues (m : Matrix2) : Vector2 :=
  m.singularValuesImpl

defun svdDecomposition (m : Matrix2) : Matrix2 × Matrix2 × Matrix2 :=
  (⟨⟨1, 0⟩, ⟨0, 1⟩⟩, m, ⟨⟨1, 0⟩, ⟨0, 1⟩⟩)  -- Simplified

defun largestSingularValue (m : Matrix2) : Float :=
  let sv := m.singularValues
  Max.max sv.x sv.y

defun smallestSingularValue (m : Matrix2) : Float :=
  let sv := m.singularValues
  Min.min sv.x sv.y

defun pseudoInverse (m : Matrix2) : Matrix2 :=
  m.inverse

defun svdApprox (m : Matrix2) (k : Int) : Matrix2 :=
  if k <= 0 then ⟨⟨0, 0⟩, ⟨0, 0⟩⟩
  else m

-- ============================================================================
-- Eigenvalue Decomposition
-- ============================================================================

defun symmetricEigenvalues (m : Matrix2) : Vector2 :=
  m.symmetricEigenvaluesImpl

defun symmetricEigenDecomposition (m : Matrix2) : Matrix2 × Matrix2 :=
  let ev := m.symmetricEigenvalues
  let lambda : Matrix2 := ⟨⟨ev.x, 0⟩, ⟨0, ev.y⟩⟩
  (⟨⟨1, 0⟩, ⟨0, 1⟩⟩, lambda)  -- Simplified

defun eigenvalues (m : Matrix2) : Vector2 :=
  m.symmetricEigenvalues

defun eigenDecomposition (m : Matrix2) : Matrix2 × Matrix2 :=
  m.symmetricEigenDecomposition

defun eigenvectors (m : Matrix2) : Matrix2 :=
  let (v, _) := m.eigenDecomposition
  v

defun largestEigenvalue (m : Matrix2) : Float :=
  let ev := m.eigenvalues
  Max.max (Float.abs ev.x) (Float.abs ev.y)

defun smallestEigenvalue (m : Matrix2) : Float :=
  let ev := m.eigenvalues
  Min.min (Float.abs ev.x) (Float.abs ev.y)

defun powerIteration (m : Matrix2) (maxIter : Int := 100) : Vector2 × Float :=
  (⟨1, 0⟩, 1.0)  -- Simplified

defun spectralRadius (m : Matrix2) : Float :=
  m.largestEigenvalue

-- ============================================================================
-- Matrix Factorizations
-- ============================================================================

defun polarDecomposition (m : Matrix2) : Matrix2 × Matrix2 :=
  (⟨⟨1, 0⟩, ⟨0, 1⟩⟩, m)

defun schurDecomposition (m : Matrix2) : Matrix2 × Matrix2 :=
  (⟨⟨1, 0⟩, ⟨0, 1⟩⟩, m)

-- ============================================================================
-- Gaussian Elimination
-- ============================================================================

defun gaussianElimination (m : Matrix2) : Matrix2 :=
  if m.row0.x == 0 then m
  else
    let factor := m.row1.x / m.row0.x
    let new_row1 : Vector2 := ⟨0, m.row1.y - factor * m.row0.y⟩
    ⟨m.row0, new_row1⟩

defun reducedRowEchelon (m : Matrix2) : Matrix2 :=
  m.gaussianElimination

defun backSubstitution (u : Matrix2) (b : Vector2) : Vector2 :=
  if u.row1.y == 0 then b
  else
    let x1 := b.y / u.row1.y
    let x0 := if u.row0.x == 0 then 0 else (b.x - u.row0.y * x1) / u.row0.x
    ⟨x0, x1⟩

defun forwardSubstitution (l : Matrix2) (b : Vector2) : Vector2 :=
  if l.row0.x == 0 then b
  else
    let y0 := b.x / l.row0.x
    let y1 := if l.row1.y == 0 then 0 else (b.y - l.row1.x * y0) / l.row1.y
    ⟨y0, y1⟩

-- ============================================================================
-- Matrix Functions
-- ============================================================================

defun matrixExp (m : Matrix2) : Matrix2 :=
  let m2 := m.matmul m
  let id : Matrix2 := ⟨⟨1, 0⟩, ⟨0, 1⟩⟩
  id.add m |>.add (m2.hMul 0.5)

defun matrixLog (m : Matrix2) : Matrix2 :=
  m

defun matrixPower (m : Matrix2) (k : Int) : Matrix2 :=
  if k == 0 then ⟨⟨1, 0⟩, ⟨0, 1⟩⟩
  else if k == 1 then m
  else m.matmul m

defun matrixSqrt (m : Matrix2) : Matrix2 :=
  m

defun matrixSign (m : Matrix2) : Matrix2 :=
  m

-- ============================================================================
-- Gram Matrix and Related
-- ============================================================================

defun gramMatrix (m : Matrix2) : Matrix2 :=
  m.transpose.matmul m

defun gramSchmidt (m : Matrix2) : Matrix2 :=
  let (q, _) := m.qrDecomposition
  q

defun orthonormalize (m : Matrix2) : Matrix2 :=
  m.gramSchmidt

-- ============================================================================
-- Least Squares
-- ============================================================================

defun leastSquares (a : Matrix2) (b : Vector2) : Vector2 :=
  a.solve b

defun weightedLeastSquares (a : Matrix2) (b : Vector2) (w : Matrix2) : Vector2 :=
  let wa := w.matmul a
  let wb := w.mulVec b
  wa.leastSquares wb

defun leastSquaresQR (a : Matrix2) (b : Vector2) : Vector2 :=
  a.leastSquares b

defun leastSquaresSVD (a : Matrix2) (b : Vector2) : Vector2 :=
  a.leastSquares b

-- ============================================================================
-- Matrix Construction Utilities
-- ============================================================================

defun identity : Matrix2 := ⟨⟨1, 0⟩, ⟨0, 1⟩⟩

defun zeros : Matrix2 := ⟨⟨0, 0⟩, ⟨0, 0⟩⟩

defun ones : Matrix2 := ⟨⟨1, 1⟩, ⟨1, 1⟩⟩

defun diag (v : Vector2) : Matrix2 :=
  ⟨⟨v.x, 0⟩, ⟨0, v.y⟩⟩

defun getDiag (m : Matrix2) : Vector2 :=
  ⟨m.row0.x, m.row1.y⟩

defun outerProduct (u v : Vector2) : Matrix2 :=
  ⟨⟨u.x * v.x, u.x * v.y⟩, ⟨u.y * v.x, u.y * v.y⟩⟩

-- ============================================================================
-- Block Matrix Operations
-- ============================================================================

defun getRow (m : Matrix2) (i : Int) : Vector2 :=
  match i with
  | 0 => m.row0
  | 1 => m.row1
  | _ => ⟨0, 0⟩

defun getColumn (m : Matrix2) (j : Int) : Vector2 :=
  match j with
  | 0 => ⟨m.row0.x, m.row1.x⟩
  | 1 => ⟨m.row0.y, m.row1.y⟩
  | _ => ⟨0, 0⟩

defun setRow (m : Matrix2) (i : Int) (v : Vector2) : Matrix2 :=
  match i with
  | 0 => ⟨v, m.row1⟩
  | 1 => ⟨m.row0, v⟩
  | _ => m

defun setColumn (m : Matrix2) (j : Int) (v : Vector2) : Matrix2 :=
  match j with
  | 0 => ⟨⟨v.x, m.row0.y⟩, ⟨v.y, m.row1.y⟩⟩
  | 1 => ⟨⟨m.row0.x, v.x⟩, ⟨m.row1.x, v.y⟩⟩
  | _ => m

defun swapRows (m : Matrix2) (i j : Int) : Matrix2 :=
  if i == j then m
  else ⟨m.row1, m.row0⟩

defun swapColumns (m : Matrix2) (i j : Int) : Matrix2 :=
  if i == j then m
  else ⟨⟨m.row0.y, m.row0.x⟩, ⟨m.row1.y, m.row1.x⟩⟩

end HouLean.Matrix2 eigenvalues of M^T * M
  let mtm := m.transpose.matmul m
  let ev := mtm.symmetricEigenvalues
  ⟨ev.x.sqrt, ev.y.sqrt⟩

defun svdDecomposition (m : Matrix2) : Matrix2 × Matrix2 × Matrix2 :=
  -- For 2x2, use direct computation via eigendecomposition
  let mtm := m.transpose.matmul m
  
  -- V from eigenvectors of M^T M
  let (v, sigma_sq) := mtm.symmetricEigenDecomposition
  
  -- Sigma from square roots of eigenvalues
  let s0 := sigma_sq.row0.x.sqrt
  let s1 := sigma_sq.row1.y.sqrt
  let sigma : Matrix2 := ⟨⟨s0, 0⟩, ⟨0, s1⟩⟩
  
  -- U = M * V * Sigma^-1
  let u := if s0 == 0 && s1 == 0 then ⟨⟨1, 0⟩, ⟨0, 1⟩⟩
          else
            let mv := m.matmul v
            let u0 := if s0 == 0 then ⟨mv.row0.x, mv.row1.x⟩ 
                     else ⟨mv.row0.x / s0, mv.row1.x / s0⟩
            let u1 := if s1 == 0 then ⟨mv.row0.y, mv.row1.y⟩
                     else ⟨mv.row0.y / s1, mv.row1.y / s1⟩
            ⟨u0, u1⟩
  
  (u, sigma, v)

defun largestSingularValue (m : Matrix2) : Float :=
  let sv := m.singularValues
  Max.max sv.x sv.y

defun smallestSingularValue (m : Matrix2) : Float :=
  let sv := m.singularValues
  Min.min sv.x sv.y

defun pseudoInverse (m : Matrix2) : Matrix2 :=
  let (u, sigma, v) := m.svdDecomposition
  let epsilon := 0.0001
  let s0_inv := if sigma.row0.x > epsilon then 1.0 / sigma.row0.x else 0.0
  let s1_inv := if sigma.row1.y > epsilon then 1.0 / sigma.row1.y else 0.0
  let sigma_inv : Matrix2 := ⟨⟨s0_inv, 0⟩, ⟨0, s1_inv⟩⟩
  v.matmul sigma_inv |>.matmul u.transpose

defun svdApprox (m : Matrix2) (k : Int) : Matrix2 :=
  if k <= 0 then ⟨⟨0, 0⟩, ⟨0, 0⟩⟩
  else if k >= 2 then m
  else
    let (u, sigma, v) := m.svdDecomposition
    let sigma_k : Matrix2 := ⟨⟨sigma.row0.x, 0⟩, ⟨0, 0⟩⟩
    u.matmul sigma_k |>.matmul v.transpose

-- ============================================================================
-- Eigenvalue Decomposition
-- ============================================================================

defun symmetricEigenvalues (m : Matrix2) : Vector2 :=
  -- For symmetric 2x2 matrix
  let t := m.trace
  let d := m.determinant
  let discriminant := t * t - 4.0 * d
  if discriminant < 0 then ⟨t / 2.0, t / 2.0⟩
  else
    let sd := discriminant.sqrt
    let ev1 := (t + sd) / 2.0
    let ev2 := (t - sd) / 2.0
    ⟨Max.max ev1 ev2, Min.min ev1 ev2⟩

defun symmetricEigenDecomposition (m : Matrix2) : Matrix2 × Matrix2 :=
  let ev := m.symmetricEigenvalues
  let lambda : Matrix2 := ⟨⟨ev.x, 0⟩, ⟨0, ev.y⟩⟩
  
  -- Compute eigenvectors
  let a := m.row0.x - ev.x
  let b := m.row0.y
  let c := m.row1.x
  
  let v1 := if b != 0 then
    let len := (b * b + a * a).sqrt
    if len == 0 then ⟨1, 0⟩ else ⟨-b / len, a / len⟩
  else if a != 0 then ⟨0, 1⟩
  else ⟨1, 0⟩
  
  let v2 : Vector2 := ⟨-v1.y, v1.x⟩  -- Perpendicular
  
  let q : Matrix2 := ⟨⟨v1.x, v2.x⟩, ⟨v1.y, v2.y⟩⟩
  (q, lambda)

defun eigenvalues (m : Matrix2) : Vector2 :=
  -- General 2x2 eigenvalues (may be complex, returning real parts only)
  let t := m.trace
  let d := m.determinant
  let discriminant := t * t - 4.0 * d
  if discriminant >= 0 then
    let sd := discriminant.sqrt
    ⟨(t + sd) / 2.0, (t - sd) / 2.0⟩
  else
    ⟨t / 2.0, t / 2.0⟩  -- Return real part only

defun eigenDecomposition (m : Matrix2) : Matrix2 × Matrix2 :=
  if m.isSymmetric then m.symmetricEigenDecomposition
  else
    let ev := m.eigenvalues
    let lambda : Matrix2 := ⟨⟨ev.x, 0⟩, ⟨0, ev.y⟩⟩
    -- Simplified: compute first eigenvector
    let a := m.row0.x - ev.x
    let b := m.row0.y
    let v1 := if b != 0 then
      let len := (b * b + a * a).sqrt
      if len == 0 then ⟨1, 0⟩ else ⟨-b / len, a / len⟩
    else ⟨1, 0⟩
    let v2 : Vector2 := ⟨-v1.y, v1.x⟩
    let v : Matrix2 := ⟨⟨v1.x, v2.x⟩, ⟨v1.y, v2.y⟩⟩
    (v, lambda)

defun eigenvectors (m : Matrix2) : Matrix2 :=
  let (v, _) := m.eigenDecomposition
  v

defun largestEigenvalue (m : Matrix2) : Float :=
  let ev := m.eigenvalues
  Max.max (Float.abs ev.x) (Float.abs ev.y)

defun smallestEigenvalue (m : Matrix2) : Float :=
  let ev := m.eigenvalues
  Min.min (Float.abs ev.x) (Float.abs ev.y)

defun powerIteration (m : Matrix2) (maxIter : Int := 100) : Vector2 × Float :=
  let rec iterate (v : Vector2) (iter : Int) : Vector2 × Float :=
    if iter <= 0 then
      let len := (v.x * v.x + v.y * v.y).sqrt
      (v, len)
    else
      let mv := m.mulVec v
      let len := (mv.x * mv.x + mv.y * mv.y).sqrt
      if len == 0 then (v, 0)
      else iterate ⟨mv.x / len, mv.y / len⟩ (iter - 1)
  iterate ⟨1, 0⟩ maxIter

defun spectralRadius (m : Matrix2) : Float :=
  m.largestEigenvalue

-- ============================================================================
-- Matrix Factorizations
-- ============================================================================

defun polarDecomposition (m : Matrix2) : Matrix2 × Matrix2 :=
  let (u, sigma, v) := m.svdDecomposition
  let unitary := u.matmul v.transpose
  let positive := v.matmul sigma |>.matmul v.transpose
  (unitary, positive)

defun schurDecomposition (m : Matrix2) : Matrix2 × Matrix2 :=
  -- For 2x2, Schur form is upper triangular with eigenvalues on diagonal
  if m.isUpperTriangular then (⟨⟨1, 0⟩, ⟨0, 1⟩⟩, m)
  else
    let (q, lambda) := m.eigenDecomposition
    let t := q.transpose.matmul m |>.matmul q
    (q, t)

-- ============================================================================
-- Gaussian Elimination
-- ============================================================================

defun gaussianElimination (m : Matrix2) : Matrix2 :=
  if m.row0.x == 0 then
    if m.row1.x == 0 then m
    else ⟨m.row1, m.row0⟩
  else
    let factor := m.row1.x / m.row0.x
    let new_row1 : Vector2 := ⟨0, m.row1.y - factor * m.row0.y⟩
    ⟨m.row0, new_row1⟩

defun reducedRowEchelon (m : Matrix2) : Matrix2 :=
  let rref1 := m.gaussianElimination
  -- Normalize rows
  let row0 := if rref1.row0.x != 0 then
    ⟨1, rref1.row0.y / rref1.row0.x⟩
  else if rref1.row0.y != 0 then
    ⟨0, 1⟩
  else rref1.row0
  
  let row1 := if rref1.row1.y != 0 then
    ⟨0, 1⟩
  else rref1.row1
  
  -- Back substitute
  let row0_final := if row1.y == 1 && row0.x == 1 then
    ⟨1, 0⟩
  else row0
  
  ⟨row0_final, row1⟩

defun backSubstitution (u : Matrix2) (b : Vector2) : Vector2 :=
  if u.row1.y == 0 then b
  else
    let x1 := b.y / u.row1.y
    let x0 := if u.row0.x == 0 then 0 else (b.x - u.row0.y * x1) / u.row0.x
    ⟨x0, x1⟩

defun forwardSubstitution (l : Matrix2) (b : Vector2) : Vector2 :=
  if l.row0.x == 0 then b
  else
    let y0 := b.x / l.row0.x
    let y1 := if l.row1.y == 0 then 0 else (b.y - l.row1.x * y0) / l.row1.y
    ⟨y0, y1⟩

-- ============================================================================
-- Matrix Functions
-- ============================================================================

defun matrixExp (m : Matrix2) : Matrix2 :=
  -- Use eigendecomposition for symmetric, otherwise use series
  if m.isSymmetric then
    let (v, lambda) := m.symmetricEigenDecomposition
    let exp_lambda : Matrix2 := ⟨⟨lambda.row0.x.exp, 0⟩, ⟨0, lambda.row1.y.exp⟩⟩
    v.matmul exp_lambda |>.matmul v.transpose
  else
    -- Use Padé approximation or series (simplified to just a few terms)
    let m2 := m.matmul m
    let m3 := m2.matmul m
    let id : Matrix2 := ⟨⟨1, 0⟩, ⟨0, 1⟩⟩
    id.add m |>.add (m2.hMul 0.5) |>.add (m3.hMul (1.0/6.0))

defun matrixLog (m : Matrix2) : Matrix2 :=
  let (v, lambda) := m.eigenDecomposition
  let log_lambda : Matrix2 := ⟨⟨lambda.row0.x.log, 0⟩, ⟨0, lambda.row1.y.log⟩⟩
  v.matmul log_lambda |>.matmul v.inverse

defun matrixPower (m : Matrix2) (k : Int) : Matrix2 :=
  if k == 0 then ⟨⟨1, 0⟩, ⟨0, 1⟩⟩
  else if k < 0 then matrixPower m.inverse (-k)
  else if k == 1 then m
  else
    let rec powerRec (base : Matrix2) (exp : Int) (acc : Matrix2) : Matrix2 :=
      if exp == 0 then acc
      else if exp % 2 == 0 then powerRec (base.matmul base) (exp / 2) acc
      else powerRec base (exp - 1) (acc.matmul base)
    powerRec m k ⟨⟨1, 0⟩, ⟨0, 1⟩⟩

defun matrixSqrt (m : Matrix2) : Matrix2 :=
  let (v, lambda) := m.eigenDecomposition
  let sqrt_lambda : Matrix2 := ⟨⟨lambda.row0.x.sqrt, 0⟩, ⟨0, lambda.row1.y.sqrt⟩⟩
  v.matmul sqrt_lambda |>.matmul v.inverse

defun matrixSign (m : Matrix2) : Matrix2 :=
  let (v, lambda) := m.eigenDecomposition
  let sign_lambda : Matrix2 := ⟨⟨lambda.row0.x.sign, 0⟩, ⟨0, lambda.row1.y.sign⟩⟩
  v.matmul sign_lambda |>.matmul v.inverse

-- ============================================================================
-- Gram Matrix and Related
-- ============================================================================

defun gramMatrix (m : Matrix2) : Matrix2 :=
  m.transpose.matmul m

defun gramSchmidt (m : Matrix2) : Matrix2 :=
  let (q, _) := m.qrGramSchmidt
  q

defun orthonormalize (m : Matrix2) : Matrix2 :=
  m.gramSchmidt

-- ============================================================================
-- Least Squares
-- ============================================================================

defun leastSquares (a : Matrix2) (b : Vector2) : Vector2 :=
  a.leastSquaresQR b

defun weightedLeastSquares (a : Matrix2) (b : Vector2) (w : Matrix2) : Vector2 :=
  let wa := w.matmul a
  let wb := w.mulVec b
  wa.leastSquares wb

defun leastSquaresQR (a : Matrix2) (b : Vector2) : Vector2 :=
  let (q, r) := a.qrDecomposition
  let qtb := q.transpose.mulVec b
  a.backSubstitution r qtb

defun leastSquaresSVD (a : Matrix2) (b : Vector2) : Vector2 :=
  let pinv := a.pseudoInverse
  pinv.mulVec b

-- ============================================================================
-- Matrix Construction Utilities
-- ============================================================================

defun identity : Matrix2 := ⟨⟨1, 0⟩, ⟨0, 1⟩⟩

defun zeros : Matrix2 := ⟨⟨0, 0⟩, ⟨0, 0⟩⟩

defun ones : Matrix2 := ⟨⟨1, 1⟩, ⟨1, 1⟩⟩

defun diag (v : Vector2) : Matrix2 :=
  ⟨⟨v.x, 0⟩, ⟨0, v.y⟩⟩

defun getDiag (m : Matrix2) : Vector2 :=
  ⟨m.row0.x, m.row1.y⟩

defun outerProduct (u v : Vector2) : Matrix2 :=
  ⟨⟨u.x * v.x, u.x * v.y⟩, ⟨u.y * v.x, u.y * v.y⟩⟩

-- ============================================================================
-- Block Matrix Operations
-- ============================================================================

defun getRow (m : Matrix2) (i : Int) : Vector2 :=
  match i with
  | 0 => m.row0
  | 1 => m.row1
  | _ => ⟨0, 0⟩

defun getColumn (m : Matrix2) (j : Int) : Vector2 :=
  match j with
  | 0 => ⟨m.row0.x, m.row1.x⟩
  | 1 => ⟨m.row0.y, m.row1.y⟩
  | _ => ⟨0, 0⟩

defun setRow (m : Matrix2) (i : Int) (v : Vector2) : Matrix2 :=
  match i with
  | 0 => ⟨v, m.row1⟩
  | 1 => ⟨m.row0, v⟩
  | _ => m

defun setColumn (m : Matrix2) (j : Int) (v : Vector2) : Matrix2 :=
  match j with
  | 0 => ⟨⟨v.x, m.row0.y⟩, ⟨v.y, m.row1.y⟩⟩
  | 1 => ⟨⟨m.row0.x, v.x⟩, ⟨m.row1.x, v.y⟩⟩
  | _ => m

defun swapRows (m : Matrix2) (i j : Int) : Matrix2 :=
  if i == j then m
  else ⟨m.row1, m.row0⟩

defun swapColumns (m : Matrix2) (i j : Int) : Matrix2 :=
  if i == j then m
  else ⟨⟨m.row0.y, m.row0.x⟩, ⟨m.row1.y, m.row1.x⟩⟩

end HouLean.Matrix2 eigenvalues of M^T * M
  let mtm := m.transpose.matmul m
  let ev := mtm.symmetricEigenvalues
  ⟨ev.x.sqrt, ev.y.sqrt⟩

def svdDecomposition (m : Matrix2) : Matrix2 × Matrix2 × Matrix2 :=
  -- For 2x2, use direct computation via eigendecomposition
  let mtm := m.transpose.matmul m
  let mmt := m.matmul m.transpose
  
  -- V from eigenvectors of M^T M
  let (v, sigma_sq) := mtm.symmetricEigenDecomposition
  
  -- Sigma from square roots of eigenvalues
  let s0 := sigma_sq.row0.x.sqrt
  let s1 := sigma_sq.row1.y.sqrt
  let sigma : Matrix2 := ⟨⟨s0, 0⟩, ⟨0, s1⟩⟩
  
  -- U = M * V * Sigma^-1
  let u := if s0 == 0 && s1 == 0 then identity
          else
            let mv := m.matmul v
            let u0 := if s0 == 0 then ⟨mv.row0.x, mv.row1.x⟩ 
                     else ⟨mv.row0.x / s0, mv.row1.x / s0⟩
            let u1 := if s1 == 0 then ⟨mv.row0.y, mv.row1.y⟩
                     else ⟨mv.row0.y / s1, mv.row1.y / s1⟩
            ⟨u0, u1⟩
  
  (u, sigma, v)

def largestSingularValue (m : Matrix2) : Float :=
  let sv := m.singularValues
  Max.max sv.x sv.y

def smallestSingularValue (m : Matrix2) : Float :=
  let sv := m.singularValues
  Min.min sv.x sv.y

def pseudoInverse (m : Matrix2) : Matrix2 :=
  let (u, sigma, v) := m.svdDecomposition
  let epsilon := 0.0001
  let s0_inv := if sigma.row0.x > epsilon then 1.0 / sigma.row0.x else 0.0
  let s1_inv := if sigma.row1.y > epsilon then 1.0 / sigma.row1.y else 0.0
  let sigma_inv : Matrix2 := ⟨⟨s0_inv, 0⟩, ⟨0, s1_inv⟩⟩
  v.matmul sigma_inv |>.matmul u.transpose

def svdApprox (m : Matrix2) (k : Int) : Matrix2 :=
  if k <= 0 then zeros
  else if k >= 2 then m
  else
    let (u, sigma, v) := m.svdDecomposition
    let sigma_k : Matrix2 := ⟨⟨sigma.row0.x, 0⟩, ⟨0, 0⟩⟩
    u.matmul sigma_k |>.matmul v.transpose

-- ============================================================================
-- Eigenvalue Decomposition
-- ============================================================================

def symmetricEigenvalues (m : Matrix2) : Vector2 :=
  -- For symmetric 2x2 matrix
  let t := m.trace
  let d := m.determinant
  let discriminant := t * t - 4.0 * d
  if discriminant < 0 then ⟨t / 2.0, t / 2.0⟩
  else
    let sd := discriminant.sqrt
    let ev1 := (t + sd) / 2.0
    let ev2 := (t - sd) / 2.0
    ⟨Max.max ev1 ev2, Min.min ev1 ev2⟩

def symmetricEigenDecomposition (m : Matrix2) : Matrix2 × Matrix2 :=
  let ev := m.symmetricEigenvalues
  let lambda : Matrix2 := ⟨⟨ev.x, 0⟩, ⟨0, ev.y⟩⟩
  
  -- Compute eigenvectors
  let a := m.row0.x - ev.x
  let b := m.row0.y
  let c := m.row1.x
  
  let v1 := if b != 0 then
    let len := (b * b + a * a).sqrt
    if len == 0 then ⟨1, 0⟩ else ⟨-b / len, a / len⟩
  else if a != 0 then ⟨0, 1⟩
  else ⟨1, 0⟩
  
  let v2 : Vector2 := ⟨-v1.y, v1.x⟩  -- Perpendicular
  
  let q : Matrix2 := ⟨⟨v1.x, v2.x⟩, ⟨v1.y, v2.y⟩⟩
  (q, lambda)

def eigenvalues (m : Matrix2) : Vector2 :=
  -- General 2x2 eigenvalues (may be complex, returning real parts only)
  let t := m.trace
  let d := m.determinant
  let discriminant := t * t - 4.0 * d
  if discriminant >= 0 then
    let sd := discriminant.sqrt
    ⟨(t + sd) / 2.0, (t - sd) / 2.0⟩
  else
    ⟨t / 2.0, t / 2.0⟩  -- Return real part only

def eigenDecomposition (m : Matrix2) : Matrix2 × Matrix2 :=
  if m.isSymmetric then m.symmetricEigenDecomposition
  else
    let ev := m.eigenvalues
    let lambda : Matrix2 := ⟨⟨ev.x, 0⟩, ⟨0, ev.y⟩⟩
    -- Simplified: compute first eigenvector
    let a := m.row0.x - ev.x
    let b := m.row0.y
    let v1 := if b != 0 then
      let len := (b * b + a * a).sqrt
      if len == 0 then ⟨1, 0⟩ else ⟨-b / len, a / len⟩
    else ⟨1, 0⟩
    let v2 : Vector2 := ⟨-v1.y, v1.x⟩
    let v : Matrix2 := ⟨⟨v1.x, v2.x⟩, ⟨v1.y, v2.y⟩⟩
    (v, lambda)

def eigenvectors (m : Matrix2) : Matrix2 :=
  let (v, _) := m.eigenDecomposition
  v

def largestEigenvalue (m : Matrix2) : Float :=
  let ev := m.eigenvalues
  Max.max (Float.abs ev.x) (Float.abs ev.y)

def smallestEigenvalue (m : Matrix2) : Float :=
  let ev := m.eigenvalues
  Min.min (Float.abs ev.x) (Float.abs ev.y)

def powerIteration (m : Matrix2) (maxIter : Int := 100) : Vector2 × Float :=
  let rec iterate (v : Vector2) (iter : Int) : Vector2 × Float :=
    if iter <= 0 then
      let len := (v.x * v.x + v.y * v.y).sqrt
      (v, len)
    else
      let mv := m.mulVec v
      let len := (mv.x * mv.x + mv.y * mv.y).sqrt
      if len == 0 then (v, 0)
      else iterate ⟨mv.x / len, mv.y / len⟩ (iter - 1)
  iterate ⟨1, 0⟩ maxIter

def spectralRadius (m : Matrix2) : Float :=
  m.largestEigenvalue

-- ============================================================================
-- Matrix Factorizations
-- ============================================================================

def polarDecomposition (m : Matrix2) : Matrix2 × Matrix2 :=
  let (u, sigma, v) := m.svdDecomposition
  let unitary := u.matmul v.transpose
  let positive := v.matmul sigma |>.matmul v.transpose
  (unitary, positive)

def schurDecomposition (m : Matrix2) : Matrix2 × Matrix2 :=
  -- For 2x2, Schur form is upper triangular with eigenvalues on diagonal
  if m.isUpperTriangular then (identity, m)
  else
    let (q, lambda) := m.eigenDecomposition
    let t := q.transpose.matmul m |>.matmul q
    (q, t)

-- ============================================================================
-- Gaussian Elimination
-- ============================================================================

def gaussianElimination (m : Matrix2) : Matrix2 :=
  if m.row0.x == 0 then
    if m.row1.x == 0 then m
    else ⟨m.row1, m.row0⟩
  else
    let factor := m.row1.x / m.row0.x
    let new_row1 : Vector2 := ⟨0, m.row1.y - factor * m.row0.y⟩
    ⟨m.row0, new_row1⟩

def reducedRowEchelon (m : Matrix2) : Matrix2 :=
  let rref1 := m.gaussianElimination
  -- Normalize rows
  let row0 := if rref1.row0.x != 0 then
    ⟨1, rref1.row0.y / rref1.row0.x⟩
  else if rref1.row0.y != 0 then
    ⟨0, 1⟩
  else rref1.row0
  
  let row1 := if rref1.row1.y != 0 then
    ⟨0, 1⟩
  else rref1.row1
  
  -- Back substitute
  let row0_final := if row1.y == 1 && row0.x == 1 then
    ⟨1, 0⟩
  else row0
  
  ⟨row0_final, row1⟩

def backSubstitution (u : Matrix2) (b : Vector2) : Vector2 :=
  if u.row1.y == 0 then b
  else
    let x1 := b.y / u.row1.y
    let x0 := if u.row0.x == 0 then 0 else (b.x - u.row0.y * x1) / u.row0.x
    ⟨x0, x1⟩

def forwardSubstitution (l : Matrix2) (b : Vector2) : Vector2 :=
  if l.row0.x == 0 then b
  else
    let y0 := b.x / l.row0.x
    let y1 := if l.row1.y == 0 then 0 else (b.y - l.row1.x * y0) / l.row1.y
    ⟨y0, y1⟩

-- ============================================================================
-- Matrix Functions
-- ============================================================================

def matrixExp (m : Matrix2) : Matrix2 :=
  -- Use eigendecomposition for symmetric, otherwise use series
  if m.isSymmetric then
    let (v, lambda) := m.symmetricEigenDecomposition
    let exp_lambda : Matrix2 := ⟨⟨lambda.row0.x.exp, 0⟩, ⟨0, lambda.row1.y.exp⟩⟩
    v.matmul exp_lambda |>.matmul v.transpose
  else
    -- Use Padé approximation or series (simplified to just a few terms)
    let m2 := m.matmul m
    let m3 := m2.matmul m
    identity.add m |>.add (m2.mul 0.5) |>.add (m3.mul (1.0/6.0))

def matrixLog (m : Matrix2) : Matrix2 :=
  let (v, lambda) := m.eigenDecomposition
  let log_lambda : Matrix2 := ⟨⟨lambda.row0.x.log, 0⟩, ⟨0, lambda.row1.y.log⟩⟩
  v.matmul log_lambda |>.matmul v.inverse

def matrixPower (m : Matrix2) (k : Int) : Matrix2 :=
  if k == 0 then identity
  else if k < 0 then matrixPower m.inverse (-k)
  else if k == 1 then m
  else
    let rec powerRec (base : Matrix2) (exp : Int) (acc : Matrix2) : Matrix2 :=
      if exp == 0 then acc
      else if exp % 2 == 0 then powerRec (base.matmul base) (exp / 2) acc
      else powerRec base (exp - 1) (acc.matmul base)
    powerRec m k identity

def matrixSqrt (m : Matrix2) : Matrix2 :=
  let (v, lambda) := m.eigenDecomposition
  let sqrt_lambda : Matrix2 := ⟨⟨lambda.row0.x.sqrt, 0⟩, ⟨0, lambda.row1.y.sqrt⟩⟩
  v.matmul sqrt_lambda |>.matmul v.inverse

def matrixSign (m : Matrix2) : Matrix2 :=
  let (v, lambda) := m.eigenDecomposition
  let sign_lambda : Matrix2 := ⟨⟨lambda.row0.x.sign, 0⟩, ⟨0, lambda.row1.y.sign⟩⟩
  v.matmul sign_lambda |>.matmul v.inverse

-- ============================================================================
-- Gram Matrix and Related
-- ============================================================================

def gramMatrix (m : Matrix2) : Matrix2 :=
  m.transpose.matmul m

def gramSchmidt (m : Matrix2) : Matrix2 :=
  let (q, _) := m.qrGramSchmidt
  q

def orthonormalize (m : Matrix2) : Matrix2 :=
  m.gramSchmidt

-- ============================================================================
-- Least Squares
-- ============================================================================

def leastSquares (a : Matrix2) (b : Vector2) : Vector2 :=
  a.leastSquaresQR b

def weightedLeastSquares (a : Matrix2) (b : Vector2) (w : Matrix2) : Vector2 :=
  let wa := w.matmul a
  let wb := w.mulVec b
  wa.leastSquares wb

def leastSquaresQR (a : Matrix2) (b : Vector2) : Vector2 :=
  let (q, r) := a.qrDecomposition
  let qtb := q.transpose.mulVec b
  a.backSubstitution r qtb

def leastSquaresSVD (a : Matrix2) (b : Vector2) : Vector2 :=
  let pinv := a.pseudoInverse
  pinv.mulVec b

-- ============================================================================
-- Matrix Construction Utilities
-- ============================================================================

def diag (v : Vector2) : Matrix2 :=
  ⟨⟨v.x, 0⟩, ⟨0, v.y⟩⟩

def getDiag (m : Matrix2) : Vector2 :=
  ⟨m.row0.x, m.row1.y⟩

def outerProduct (u v : Vector2) : Matrix2 :=
  ⟨⟨u.x * v.x, u.x * v.y⟩, ⟨u.y * v.x, u.y * v.y⟩⟩

-- ============================================================================
-- Block Matrix Operations
-- ============================================================================

def getRow (m : Matrix2) (i : Int) : Vector2 :=
  match i with
  | 0 => m.row0
  | 1 => m.row1
  | _ => ⟨0, 0⟩

def getColumn (m : Matrix2) (j : Int) : Vector2 :=
  match j with
  | 0 => ⟨m.row0.x, m.row1.x⟩
  | 1 => ⟨m.row0.y, m.row1.y⟩
  | _ => ⟨0, 0⟩

def setRow (m : Matrix2) (i : Int) (v : Vector2) : Matrix2 :=
  match i with
  | 0 => ⟨v, m.row1⟩
  | 1 => ⟨m.row0, v⟩
  | _ => m

def setColumn (m : Matrix2) (j : Int) (v : Vector2) : Matrix2 :=
  match j with
  | 0 => ⟨⟨v.x, m.row0.y⟩, ⟨v.y, m.row1.y⟩⟩
  | 1 => ⟨⟨m.row0.x, v.x⟩, ⟨m.row1.x, v.y⟩⟩
  | _ => m

def swapRows (m : Matrix2) (i j : Int) : Matrix2 :=
  if i == j then m
  else ⟨m.row1, m.row0⟩

def swapColumns (m : Matrix2) (i j : Int) : Matrix2 :=
  if i == j then m
  else ⟨⟨m.row0.y, m.row0.x⟩, ⟨m.row1.y, m.row1.x⟩⟩

-- ============================================================================
-- Helper: add, mul for chaining
-- ============================================================================

def add (a b : Matrix2) : Matrix2 :=
  ⟨⟨a.row0.x + b.row0.x, a.row0.y + b.row0.y⟩,
   ⟨a.row1.x + b.row1.x, a.row1.y + b.row1.y⟩⟩

def mul (a : Matrix2) (s : Float) : Matrix2 :=
  ⟨⟨a.row0.x * s, a.row0.y * s⟩,
   ⟨a.row1.x * s, a.row1.y * s⟩⟩

end HouLean.Matrix2

-- ============================================================================
-- Register overloaded functions
-- ============================================================================

namespace HouLean.LinearAlgebra

open HouLean.Matrix2

defun transpose (m : Matrix2) : Matrix2 := m.transpose
defun determinant (m : Matrix2) : Float := m.determinant
defun inverse (m : Matrix2) : Matrix2 := m.inverse
defun trace (m : Matrix2) : Float := m.trace
defun rank (m : Matrix2) : Int := m.rank
defun isSingular (m : Matrix2) : Bool := m.isSingular
defun isInvertible (m : Matrix2) : Bool := m.isInvertible
defun conditionNumber (m : Matrix2) : Float := m.conditionNumber

defun isSymmetric (m : Matrix2) : Bool := m.isSymmetric
defun isOrthogonal (m : Matrix2) : Bool := m.isOrthogonal
defun isDiagonal (m : Matrix2) : Bool := m.isDiagonal
defun isUpperTriangular (m : Matrix2) : Bool := m.isUpperTriangular
defun isLowerTriangular (m : Matrix2) : Bool := m.isLowerTriangular
defun isPositiveDefinite (m : Matrix2) : Bool := m.isPositiveDefinite

defun frobeniusNorm (m : Matrix2) : Float := m.frobeniusNorm
defun norm1 (m : Matrix2) : Float := m.norm1
defun normInf (m : Matrix2) : Float := m.normInf
defun norm2 (m : Matrix2) : Float := m.norm2

defun solve (m : Matrix2) (b : Vector2) : Vector2 := m.solve b
defun solveGaussian (m : Matrix2) (b : Vector2) : Vector2 := m.solveGaussian b
defun solveLU (m : Matrix2) (b : Vector2) : Vector2 := m.solveLU b
defun solveCholesky (m : Matrix2) (b : Vector2) : Vector2 := m.solveCholesky b
defun solveQR (m : Matrix2) (b : Vector2) : Vector2 := m.solveQR b

defun luDecomposition (m : Matrix2) : Matrix2 × Matrix2 × Matrix2 := m.luDecomposition
defun luDecompositionNoPivot (m : Matrix2) : Matrix2 × Matrix2 := m.luDecompositionNoPivot
defun luLowerMatrix (m : Matrix2) : Matrix2 := m.luLowerMatrix
defun luUpperMatrix (m : Matrix2) : Matrix2 := m.luUpperMatrix

defun choleskyDecomposition (m : Matrix2) : Matrix2 := m.choleskyDecomposition
defun choleskyModified (m : Matrix2) : Matrix2 := m.choleskyModified

defun ldltDecomposition (m : Matrix2) : Matrix2 × Matrix2 := m.ldltDecomposition
defun ldltLowerMatrix (m : Matrix2) : Matrix2 := m.ldltLowerMatrix
defun ldltDiagonalMatrix (m : Matrix2) : Matrix2 := m.ldltDiagonalMatrix

defun qrDecomposition (m : Matrix2) : Matrix2 × Matrix2 := m.qrDecomposition
defun qrGramSchmidt (m : Matrix2) : Matrix2 × Matrix2 := m.qrGramSchmidt
defun qrHouseholder (m : Matrix2) : Matrix2 × Matrix2 := m.qrHouseholder
defun qrOrthogonalMatrix (m : Matrix2) : Matrix2 := m.qrOrthogonalMatrix
defun qrUpperMatrix (m : Matrix2) : Matrix2 := m.qrUpperMatrix

defun svdDecomposition (m : Matrix2) : Matrix2 × Matrix2 × Matrix2 := m.svdDecomposition
defun singularValues (m : Matrix2) : Vector2 := m.singularValues
defun largestSingularValue (m : Matrix2) : Float := m.largestSingularValue
defun smallestSingularValue (m : Matrix2) : Float := m.smallestSingularValue
defun pseudoInverse (m : Matrix2) : Matrix2 := m.pseudoInverse
defun svdApprox (m : Matrix2) (k : Int) : Matrix2 := m.svdApprox k

defun eigenDecomposition (m : Matrix2) : Matrix2 × Matrix2 := m.eigenDecomposition
defun eigenvalues (m : Matrix2) : Vector2 := m.eigenvalues
defun eigenvectors (m : Matrix2) : Matrix2 := m.eigenvectors
defun largestEigenvalue (m : Matrix2) : Float := m.largestEigenvalue
defun smallestEigenvalue (m : Matrix2) : Float := m.smallestEigenvalue
defun powerIteration (m : Matrix2) (maxIter : Int := 100) : Vector2 × Float := m.powerIteration maxIter
defun spectralRadius (m : Matrix2) : Float := m.spectralRadius

defun symmetricEigenDecomposition (m : Matrix2) : Matrix2 × Matrix2 := m.symmetricEigenDecomposition
defun symmetricEigenvalues (m : Matrix2) : Vector2 := m.symmetricEigenvalues

defun polarDecomposition (m : Matrix2) : Matrix2 × Matrix2 := m.polarDecomposition
defun schurDecomposition (m : Matrix2) : Matrix2 × Matrix2 := m.schurDecomposition

defun gaussianElimination (m : Matrix2) : Matrix2 := m.gaussianElimination
defun reducedRowEchelon (m : Matrix2) : Matrix2 := m.reducedRowEchelon
defun backSubstitution (u : Matrix2) (b : Vector2) : Vector2 := u.backSubstitution b
defun forwardSubstitution (l : Matrix2) (b : Vector2) : Vector2 := l.forwardSubstitution b

defun matrixExp (m : Matrix2) : Matrix2 := m.matrixExp
defun matrixLog (m : Matrix2) : Matrix2 := m.matrixLog
defun matrixPower (m : Matrix2) (k : Int) : Matrix2 := m.matrixPower k
defun matrixSqrt (m : Matrix2) : Matrix2 := m.matrixSqrt
defun matrixSign (m : Matrix2) : Matrix2 := m.matrixSign

defun gramMatrix (m : Matrix2) : Matrix2 := m.gramMatrix
defun gramSchmidt (m : Matrix2) : Matrix2 := m.gramSchmidt
defun orthonormalize (m : Matrix2) : Matrix2 := m.orthonormalize

defun leastSquares (a : Matrix2) (b : Vector2) : Vector2 := a.leastSquares b
defun weightedLeastSquares (a : Matrix2) (b : Vector2) (w : Matrix2) : Vector2 := a.weightedLeastSquares b w
defun leastSquaresQR (a : Matrix2) (b : Vector2) : Vector2 := a.leastSquaresQR b
defun leastSquaresSVD (a : Matrix2) (b : Vector2) : Vector2 := a.leastSquaresSVD b

defun identity : Matrix2 := Matrix2.identity
defun zeros : Matrix2 := Matrix2.zeros
defun ones : Matrix2 := Matrix2.ones
defun diag (v : Vector2) : Matrix2 := Matrix2.diag v
defun getDiag (m : Matrix2) : Vector2 := m.getDiag
defun outerProduct (u v : Vector2) : Matrix2 := Matrix2.outerProduct u v

defun getRow (m : Matrix2) (i : Int) : Vector2 := m.getRow i
defun getColumn (m : Matrix2) (j : Int) : Vector2 := m.getColumn j
defun setRow (m : Matrix2) (i : Int) (v : Vector2) : Matrix2 := m.setRow i v
defun setColumn (m : Matrix2) (j : Int) (v : Vector2) : Matrix2 := m.setColumn j v
defun swapRows (m : Matrix2) (i j : Int) : Matrix2 := m.swapRows i j
defun swapColumns (m : Matrix2) (i j : Int) : Matrix2 := m.swapColumns i j

end HouLean.LinearAlgebra
