import HouLean.Data.Matrix3
import HouLean.Data.Vector3
import HouLean.LinearAlgebra

namespace HouLean.Matrix3

open HouLean.LinearAlgebra

-- ============================================================================
-- Basic Matrix Operations
-- ============================================================================

/-- Matrix transpose. -/
defun transpose (m : Matrix3) : Matrix3 :=
  ⟨⟨m.row0.x, m.row1.x, m.row2.x⟩,
   ⟨m.row0.y, m.row1.y, m.row2.y⟩,
   ⟨m.row0.z, m.row1.z, m.row2.z⟩⟩

/-- Matrix determinant. -/
defun determinant (m : Matrix3) : Float :=
  m.row0.x * (m.row1.y * m.row2.z - m.row1.z * m.row2.y) -
  m.row0.y * (m.row1.x * m.row2.z - m.row1.z * m.row2.x) +
  m.row0.z * (m.row1.x * m.row2.y - m.row1.y * m.row2.x)

/-- Matrix trace (sum of diagonal elements). -/
defun trace (m : Matrix3) : Float :=
  m.row0.x + m.row1.y + m.row2.z

/-- Identity matrix. -/
defun identity : Matrix3 :=
  ⟨⟨1.0, 0.0, 0.0⟩, ⟨0.0, 1.0, 0.0⟩, ⟨0.0, 0.0, 1.0⟩⟩

/-- Matrix inverse. Returns identity if singular. -/
defun inverse (m : Matrix3) : Matrix3 :=
  let det := m.determinant
  if det.abs < 1e-10 then identity
  else
    let invDet := 1.0 / det
    ⟨⟨invDet * (m.row1.y * m.row2.z - m.row1.z * m.row2.y),
      invDet * (m.row0.z * m.row2.y - m.row0.y * m.row2.z),
      invDet * (m.row0.y * m.row1.z - m.row0.z * m.row1.y)⟩,
     ⟨invDet * (m.row1.z * m.row2.x - m.row1.x * m.row2.z),
      invDet * (m.row0.x * m.row2.z - m.row0.z * m.row2.x),
      invDet * (m.row0.z * m.row1.x - m.row0.x * m.row1.z)⟩,
     ⟨invDet * (m.row1.x * m.row2.y - m.row1.y * m.row2.x),
      invDet * (m.row0.y * m.row2.x - m.row0.x * m.row2.y),
      invDet * (m.row0.x * m.row1.y - m.row0.y * m.row1.x)⟩⟩

/-- Matrix rank (simplified for 3x3). -/
defun rank (m : Matrix3) : Int :=
  let det := m.determinant.abs
  if det > 1e-10 then 3
  else
    -- Check 2x2 minors
    let m00 := (m.row0.x * m.row1.y - m.row0.y * m.row1.x).abs
    let m01 := (m.row0.x * m.row1.z - m.row0.z * m.row1.x).abs
    let m02 := (m.row0.y * m.row1.z - m.row0.z * m.row1.y).abs
    let m10 := (m.row0.x * m.row2.y - m.row0.y * m.row2.x).abs
    let m11 := (m.row0.x * m.row2.z - m.row0.z * m.row2.x).abs
    let m12 := (m.row0.y * m.row2.z - m.row0.z * m.row2.y).abs
    let m20 := (m.row1.x * m.row2.y - m.row1.y * m.row2.x).abs
    let m21 := (m.row1.x * m.row2.z - m.row1.z * m.row2.x).abs
    let m22 := (m.row1.y * m.row2.z - m.row1.z * m.row2.y).abs
    let maxMinor := Max.max (Max.max (Max.max m00 m01) (Max.max m02 m10)) 
                    (Max.max (Max.max m11 m12) (Max.max m20 (Max.max m21 m22)))
    if maxMinor > 1e-10 then 2
    else if m.row0.length2 > 1e-10 || m.row1.length2 > 1e-10 || m.row2.length2 > 1e-10 then 1
    else 0

-- Check if matrix is singular.
defun isSingular (m : Matrix3) : Bool :=
  (m.determinant.abs < 1e-10 : Bool)

/-- Check if matrix is invertible. -/
defun isInvertible (m : Matrix3) : Bool :=
  !m.isSingular

/-- Matrix condition number (using Frobenius norm approximation). -/
defun conditionNumber (m : Matrix3) : Float :=
  if m.isSingular then 1e10  -- Use large number instead of infinity
  else
    let normM := m.length
    let normInv := m.inverse.length
    normM * normInv

-- ============================================================================
-- Matrix Properties
-- ============================================================================

/-- Check if matrix is symmetric (M = Mᵀ). -/
defun isSymmetric (m : Matrix3) : Bool :=
  let eps := 1e-10
  (m.row0.y - m.row1.x).abs < eps &&
  (m.row0.z - m.row2.x).abs < eps &&
  (m.row1.z - m.row2.y).abs < eps

/-- Check if matrix is orthogonal (M * Mᵀ = I). -/
defun isOrthogonal (m : Matrix3) : Bool :=
  let prod := m * m.transpose
  let eps := 1e-6
  (prod.row0.x - 1.0).abs < eps && prod.row0.y.abs < eps && prod.row0.z.abs < eps &&
  prod.row1.x.abs < eps && (prod.row1.y - 1.0).abs < eps && prod.row1.z.abs < eps &&
  prod.row2.x.abs < eps && prod.row2.y.abs < eps && (prod.row2.z - 1.0).abs < eps

/-- Check if matrix is diagonal. -/
defun isDiagonal (m : Matrix3) : Bool :=
  let eps := 1e-10
  m.row0.y.abs < eps && m.row0.z.abs < eps &&
  m.row1.x.abs < eps && m.row1.z.abs < eps &&
  m.row2.x.abs < eps && m.row2.y.abs < eps

/-- Check if matrix is upper triangular. -/
defun isUpperTriangular (m : Matrix3) : Bool :=
  let eps := 1e-10
  m.row1.x.abs < eps && m.row2.x.abs < eps && m.row2.y.abs < eps

/-- Check if matrix is lower triangular. -/
defun isLowerTriangular (m : Matrix3) : Bool :=
  let eps := 1e-10
  m.row0.y.abs < eps && m.row0.z.abs < eps && m.row1.z.abs < eps

/-- Check if matrix is positive definite (simplified check). -/
defun isPositiveDefinite (m : Matrix3) : Bool :=
  if !m.isSymmetric then false
  else
    -- Sylvester's criterion: check leading principal minors
    let m1 := m.row0.x
    let m2 := m.row0.x * m.row1.y - m.row0.y * m.row1.x
    let m3 := m.determinant
    m1 > 0.0 && m2 > 0.0 && m3 > 0.0

-- ============================================================================
-- Matrix Norms
-- ============================================================================

/-- Frobenius norm. -/
defun frobeniusNorm (m : Matrix3) : Float :=
  m.length

/-- 1-norm (maximum absolute column sum). -/
defun norm1 (m : Matrix3) : Float :=
  let c0 := m.row0.x.abs + m.row1.x.abs + m.row2.x.abs
  let c1 := m.row0.y.abs + m.row1.y.abs + m.row2.y.abs
  let c2 := m.row0.z.abs + m.row1.z.abs + m.row2.z.abs
  Max.max (Max.max c0 c1) c2

/-- Infinity norm (maximum absolute row sum). -/
defun normInf (m : Matrix3) : Float :=
  let r0 := m.row0.x.abs + m.row0.y.abs + m.row0.z.abs
  let r1 := m.row1.x.abs + m.row1.y.abs + m.row1.z.abs
  let r2 := m.row2.x.abs + m.row2.y.abs + m.row2.z.abs
  Max.max (Max.max r0 r1) r2

/-- 2-norm approximation (for full implementation would need SVD). -/
defun norm2 (m : Matrix3) : Float :=
  m.frobeniusNorm

-- ============================================================================
-- Linear System Solving
-- ============================================================================
#exit
-- /-- Solve linear system Ax = b using Cramer's rule. -/
defun solve (A : Matrix3) (b : Vector3) : Vector3 :=
  let det := A.determinant
  if det.abs < 1e-10 then ⟨0.0, 0.0, 0.0⟩
  else
    let invDet := 1.0 / det
    let Ax : Matrix3 := ⟨⟨b.x, A.row0.y, A.row0.z⟩,
                         ⟨b.y, A.row1.y, A.row1.z⟩,
                         ⟨b.z, A.row2.y, A.row2.z⟩⟩
    let Ay : Matrix3 := ⟨⟨A.row0.x, b.x, A.row0.z⟩,
                         ⟨A.row1.x, b.y, A.row1.z⟩,
                         ⟨A.row2.x, b.z, A.row2.z⟩⟩
    let Az : Matrix3 := ⟨⟨A.row0.x, A.row0.y, b.x⟩,
                         ⟨A.row1.x, A.row1.y, b.y⟩,
                         ⟨A.row2.x, A.row2.y, b.z⟩⟩
    ⟨Ax.determinant * invDet, Ay.determinant * invDet, Az.determinant * invDet⟩

defun solveGaussian (A : Matrix3) (b : Vector3) : Vector3 :=
  A.solve b

defun solveLU (A : Matrix3) (b : Vector3) : Vector3 :=
  A.solve b

defun solveCholesky (A : Matrix3) (b : Vector3) : Vector3 :=
  A.solve b

defun solveQR (A : Matrix3) (b : Vector3) : Vector3 :=
  A.solve b

-- ============================================================================
-- Matrix Construction
-- ============================================================================

/-- Zero matrix. -/
defun zeros : Matrix3 :=
  ⟨⟨0.0, 0.0, 0.0⟩, ⟨0.0, 0.0, 0.0⟩, ⟨0.0, 0.0, 0.0⟩⟩

/-- Matrix with all ones. -/
defun ones : Matrix3 :=
  ⟨⟨1.0, 1.0, 1.0⟩, ⟨1.0, 1.0, 1.0⟩, ⟨1.0, 1.0, 1.0⟩⟩

/-- Create diagonal matrix from vector. -/
defun diag (v : Vector3) : Matrix3 :=
  ⟨⟨v.x, 0.0, 0.0⟩, ⟨0.0, v.y, 0.0⟩, ⟨0.0, 0.0, v.z⟩⟩

/-- Extract diagonal as vector. -/
defun getDiag (m : Matrix3) : Vector3 :=
  ⟨m.row0.x, m.row1.y, m.row2.z⟩

/-- Create matrix from outer product: A = u * vᵀ. -/
defun outerProduct (u v : Vector3) : Matrix3 :=
  ⟨⟨u.x * v.x, u.x * v.y, u.x * v.z⟩,
   ⟨u.y * v.x, u.y * v.y, u.y * v.z⟩,
   ⟨u.z * v.x, u.z * v.y, u.z * v.z⟩⟩

-- ============================================================================
-- Block Matrix Operations
-- ============================================================================

/-- Get row i as a vector. -/
defun getRow (m : Matrix3) (i : Int) : Vector3 :=
  if i == 0 then m.row0
  else if i == 1 then m.row1
  else m.row2

/-- Get column j as a vector. -/
defun getColumn (m : Matrix3) (j : Int) : Vector3 :=
  if j == 0 then ⟨m.row0.x, m.row1.x, m.row2.x⟩
  else if j == 1 then ⟨m.row0.y, m.row1.y, m.row2.y⟩
  else ⟨m.row0.z, m.row1.z, m.row2.z⟩

/-- Set row i to vector v. -/
defun setRow (m : Matrix3) (i : Int) (v : Vector3) : Matrix3 :=
  if i == 0 then ⟨v, m.row1, m.row2⟩
  else if i == 1 then ⟨m.row0, v, m.row2⟩
  else ⟨m.row0, m.row1, v⟩

/-- Set column j to vector v. -/
defun setColumn (m : Matrix3) (j : Int) (v : Vector3) : Matrix3 :=
  if j == 0 then ⟨⟨v.x, m.row0.y, m.row0.z⟩, ⟨v.y, m.row1.y, m.row1.z⟩, ⟨v.z, m.row2.y, m.row2.z⟩⟩
  else if j == 1 then ⟨⟨m.row0.x, v.x, m.row0.z⟩, ⟨m.row1.x, v.y, m.row1.z⟩, ⟨m.row2.x, v.z, m.row2.z⟩⟩
  else ⟨⟨m.row0.x, m.row0.y, v.x⟩, ⟨m.row1.x, m.row1.y, v.y⟩, ⟨m.row2.x, m.row2.y, v.z⟩⟩

/-- Swap rows i and j. -/
defun swapRows (m : Matrix3) (i j : Int) : Matrix3 :=
  if i == j then m
  else if i == 0 && j == 1 then ⟨m.row1, m.row0, m.row2⟩
  else if i == 0 && j == 2 then ⟨m.row2, m.row1, m.row0⟩
  else if i == 1 && j == 0 then ⟨m.row1, m.row0, m.row2⟩
  else if i == 1 && j == 2 then ⟨m.row0, m.row2, m.row1⟩
  else if i == 2 && j == 0 then ⟨m.row2, m.row1, m.row0⟩
  else ⟨m.row0, m.row2, m.row1⟩

/-- Swap columns i and j. -/
defun swapColumns (m : Matrix3) (i j : Int) : Matrix3 :=
  if i == j then m
  else
    let c0 := m.getColumn i
    let c1 := m.getColumn j
    m.setColumn i c1 |>.setColumn j c0

-- ============================================================================
-- Gram Matrix and Orthogonalization
-- ============================================================================

/-- Gram matrix: G = Aᵀ * A. -/
defun gramMatrix (m : Matrix3) : Matrix3 :=
  let mt := m.transpose
  mt * m

/-- Orthogonalize using Gram-Schmidt. -/
defun gramSchmidt (m : Matrix3) : Matrix3 :=
  let v0 := m.getColumn 0
  let u0 := v0
  
  let v1 := m.getColumn 1
  let proj1 := (v1.dot u0 / u0.dot u0) * u0
  let u1 := v1 - proj1
  
  let v2 := m.getColumn 2
  let proj2_0 := (v2.dot u0 / u0.dot u0) * u0
  let proj2_1 := (v2.dot u1 / u1.dot u1) * u1
  let u2 := v2 - proj2_0 - proj2_1
  
  zeros.setColumn 0 u0 |>.setColumn 1 u1 |>.setColumn 2 u2

