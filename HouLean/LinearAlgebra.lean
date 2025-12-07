import HouLean.Meta.OverloadedFunction

namespace HouLean.LinearAlgebra

set_option linter.unusedVariables false

-- ============================================================================
-- Basic Matrix Operations
-- ============================================================================

/-- Matrix transpose. -/
declfun transpose {α} (m : α) : α

/-- Matrix determinant. -/
declfun determinant {α} (m : α) : Float

/-- Matrix inverse.

Returns identity matrix if input is singular. -/
declfun inverse {α} (m : α) : α

/-- Matrix trace (sum of diagonal elements). -/
declfun trace {α} (m : α) : Float

/-- Matrix rank (number of linearly independent rows/columns). -/
declfun rank {α} (m : α) : Int

/-- Check if matrix is singular (determinant is zero). -/
declfun isSingular {α} (m : α) : Bool

/-- Check if matrix is invertible (non-singular). -/
declfun isInvertible {α} (m : α) : Bool

/-- Matrix condition number (ratio of largest to smallest singular value).

Large condition numbers indicate ill-conditioned matrices. -/
declfun conditionNumber {α} (m : α) : Float

-- ============================================================================
-- Matrix Properties
-- ============================================================================

/-- Check if matrix is symmetric (M = Mᵀ). -/
declfun isSymmetric {α} (m : α) : Bool

/-- Check if matrix is orthogonal (M * Mᵀ = I). -/
declfun isOrthogonal {α} (m : α) : Bool

/-- Check if matrix is diagonal (all off-diagonal elements are zero). -/
declfun isDiagonal {α} (m : α) : Bool

/-- Check if matrix is upper triangular. -/
declfun isUpperTriangular {α} (m : α) : Bool

/-- Check if matrix is lower triangular. -/
declfun isLowerTriangular {α} (m : α) : Bool

/-- Check if matrix is positive definite.

A matrix is positive definite if all eigenvalues are positive. -/
declfun isPositiveDefinite {α} (m : α) : Bool

-- ============================================================================
-- Matrix Norms
-- ============================================================================

/-- Frobenius norm (square root of sum of squared elements). -/
declfun frobeniusNorm {α} (m : α) : Float

/-- 1-norm (maximum absolute column sum). -/
declfun norm1 {α} (m : α) : Float

/-- Infinity norm (maximum absolute row sum). -/
declfun normInf {α} (m : α) : Float

/-- 2-norm (largest singular value). -/
declfun norm2 {α} (m : α) : Float

-- ============================================================================
-- Linear System Solving
-- ============================================================================

/-- Solve linear system Ax = b using appropriate method.

Returns solution vector x. Fails if system has no unique solution. -/
declfun solve {α β} (A : α) (b : β) : β

/-- Solve linear system Ax = b using Gaussian elimination with partial pivoting. -/
declfun solveGaussian {α β} (A : α) (b : β) : β

/-- Solve linear system Ax = b using LU decomposition. -/
declfun solveLU {α β} (A : α) (b : β) : β

/-- Solve linear system Ax = b using Cholesky decomposition (for positive definite A). -/
declfun solveCholesky {α β} (A : α) (b : β) : β

/-- Solve linear system Ax = b using QR decomposition. -/
declfun solveQR {α β} (A : α) (b : β) : β

-- ============================================================================
-- LU Decomposition
-- ============================================================================

variable {α : Type}

/-- LU decomposition: A = P * L * U.

Returns (L, U, P) where:
- L is lower triangular with 1s on diagonal
- U is upper triangular
- P is permutation matrix -/
declfun luDecomposition (m : α) : α × α × α

/-- LU decomposition without pivoting: A = L * U.

Returns (L, U). May be unstable for certain matrices. -/
declfun luDecompositionNoPivot (m : α) : α × α

/-- Extract L matrix from LU decomposition. -/
declfun luLowerMatrix (m : α) : α

/-- Extract U matrix from LU decomposition. -/
declfun luUpperMatrix (m : α) : α

-- ============================================================================
-- Cholesky Decomposition (LLT)
-- ============================================================================

/-- Cholesky decomposition: A = L * Lᵀ.

For symmetric positive definite matrices.
Returns lower triangular matrix L. -/
declfun choleskyDecomposition (m : α) : α

/-- Modified Cholesky decomposition that works for symmetric matrices.

Returns L such that A ≈ L * Lᵀ even if A is not positive definite. -/
declfun choleskyModified (m : α) : α

-- ============================================================================
-- LDLT Decomposition
-- ============================================================================

/-- LDLT decomposition: A = L * D * Lᵀ.

For symmetric matrices. More numerically stable than Cholesky.
Returns (L, D) where:
- L is lower triangular with 1s on diagonal
- D is diagonal matrix -/
declfun ldltDecomposition (m : α) : α × α

/-- Extract L matrix from LDLT decomposition. -/
declfun ldltLowerMatrix (m : α) : α

/-- Extract D matrix from LDLT decomposition. -/
declfun ldltDiagonalMatrix (m : α) : α

-- ============================================================================
-- QR Decomposition
-- ============================================================================

/-- QR decomposition: A = Q * R.

Returns (Q, R) where:
- Q is orthogonal matrix
- R is upper triangular matrix -/
declfun qrDecomposition (m : α) : α × α

/-- QR decomposition using Gram-Schmidt process. -/
declfun qrGramSchmidt (m : α) : α × α

/-- QR decomposition using Householder reflections (more stable). -/
declfun qrHouseholder (m : α) : α × α

/-- Extract Q matrix from QR decomposition. -/
declfun qrOrthogonalMatrix (m : α) : α

/-- Extract R matrix from QR decomposition. -/
declfun qrUpperMatrix (m : α) : α

-- ============================================================================
-- Singular Value Decomposition (SVD)
-- ============================================================================

/-- Singular Value Decomposition: A = U * Σ * Vᵀ.

Returns (U, Σ, V) where:
- U is orthogonal matrix (left singular vectors)
- Σ is diagonal matrix of singular values (in descending order)
- V is orthogonal matrix (right singular vectors) -/
declfun svdDecomposition (m : α) : α × α × α

variable {β : Type}

/-- Extract singular values as a vector. -/
declfun singularValues (m : α) : β

/-- Extract largest singular value. -/
declfun largestSingularValue (m : α) : Float

/-- Extract smallest singular value. -/
declfun smallestSingularValue (m : α) : Float

/-- Moore-Penrose pseudoinverse using SVD.

Works for non-square and singular matrices. -/
declfun pseudoInverse (m : α) : α

/-- Low-rank approximation using SVD.

`svdApprox m k` returns best rank-k approximation of m. -/
declfun svdApprox (m : α) (k : Int) : α

-- ============================================================================
-- Eigenvalue Decomposition
-- ============================================================================

/-- Eigenvalue decomposition: A = V * D * V⁻¹.

For diagonalizable matrices.
Returns (V, D) where:
- V is matrix of eigenvectors (columns)
- D is diagonal matrix of eigenvalues -/
declfun eigenDecomposition (m : α) : α × α

/-- Extract eigenvalues as a vector (may be complex for real matrices). -/
declfun eigenvalues (m : α) : β

/-- Extract eigenvectors as columns of a matrix. -/
declfun eigenvectors (m : α) : α

/-- Extract largest eigenvalue by magnitude. -/
declfun largestEigenvalue (m : α) : Float

/-- Extract smallest eigenvalue by magnitude. -/
declfun smallestEigenvalue (m : α) : Float

/-- Compute dominant eigenvector using power iteration.

Returns (eigenvector, eigenvalue) pair. -/
declfun powerIteration (m : α) (maxIter : Int := 100) : β × Float

/-- Spectral radius (maximum absolute eigenvalue). -/
declfun spectralRadius (m : α) : Float

-- ============================================================================
-- Symmetric Eigenvalue Decomposition
-- ============================================================================

/-- Eigenvalue decomposition for symmetric matrices: A = Q * Λ * Qᵀ.

More efficient and numerically stable than general eigenvalue decomposition.
Returns (Q, Λ) where:
- Q is orthogonal matrix of eigenvectors
- Λ is diagonal matrix of real eigenvalues -/
declfun symmetricEigenDecomposition (m : α) : α × α

/-- Compute eigenvalues for symmetric matrix (guaranteed to be real). -/
declfun symmetricEigenvalues (m : α) : β

-- ============================================================================
-- Matrix Factorizations
-- ============================================================================

/-- Polar decomposition: A = U * P.

Returns (U, P) where:
- U is orthogonal matrix
- P is symmetric positive semi-definite matrix -/
declfun polarDecomposition (m : α) : α × α

/-- Schur decomposition: A = Q * T * Qᵀ.

Returns (Q, T) where:
- Q is orthogonal matrix
- T is upper triangular (real Schur form) or block upper triangular -/
declfun schurDecomposition (m : α) : α × α

-- ============================================================================
-- Gaussian Elimination
-- ============================================================================

/-- Gaussian elimination with partial pivoting.

Returns row echelon form of the matrix. -/
declfun gaussianElimination (m : α) : α

/-- Gaussian elimination to reduced row echelon form (RREF). -/
declfun reducedRowEchelon (m : α) : α

/-- Back substitution for upper triangular system. -/
declfun backSubstitution (U : α) (b : β) : β

/-- Forward substitution for lower triangular system. -/
declfun forwardSubstitution (L : α) (b : β) : β

-- ============================================================================
-- Matrix Functions
-- ============================================================================

/-- Matrix exponential: exp(A) = I + A + A²/2! + A³/3! + ...

Computed using Padé approximation or scaling and squaring. -/
declfun matrixExp (m : α) : α

/-- Matrix logarithm: log(A) such that exp(log(A)) = A.

Only defined for matrices with no eigenvalues on negative real axis. -/
declfun matrixLog (m : α) : α

/-- Matrix power: A^k for integer k.

Uses repeated squaring for efficiency. -/
declfun matrixPower (m : α) (k : Int) : α

/-- Matrix square root: B such that B * B = A.

Uses principal square root. -/
declfun matrixSqrt (m : α) : α

/-- Matrix sign function: sign(A) computed via Newton iteration. -/
declfun matrixSign (m : α) : α

-- ============================================================================
-- Gram Matrix and Related
-- ============================================================================

/-- Gram matrix: Gᵢⱼ = ⟨vᵢ, vⱼ⟩ where vᵢ are columns of A.

Returns G = Aᵀ * A. -/
declfun gramMatrix (m : α) : α

/-- Orthogonalize matrix columns using Gram-Schmidt process. -/
declfun gramSchmidt (m : α) : α

/-- Orthonormalize matrix columns (Gram-Schmidt with normalization). -/
declfun orthonormalize (m : α) : α

-- ============================================================================
-- Least Squares
-- ============================================================================

/-- Solve least squares problem: minimize ‖Ax - b‖².

Returns x that minimizes the residual. -/
declfun leastSquares (A : α) (b : β) : β

/-- Solve weighted least squares: minimize ‖W(Ax - b)‖². -/
declfun weightedLeastSquares (A : α) (b : β) (W : α) : β

/-- QR-based least squares solution. -/
declfun leastSquaresQR (A : α) (b : β) : β

/-- SVD-based least squares solution (more stable for rank-deficient problems). -/
declfun leastSquaresSVD (A : α) (b : β) : β

-- ============================================================================
-- Matrix Construction Utilities
-- ============================================================================

/-- Create identity matrix. -/
declfun identity {α : Type} : α

/-- Create zero matrix. -/
declfun zeros {α : Type} : α

/-- Create matrix with all ones. -/
declfun ones {α : Type} : α

/-- Create diagonal matrix from vector. -/
declfun diag (v : β) : α

/-- Extract diagonal of matrix as vector. -/
declfun getDiag (m : α) : β

/-- Create matrix from outer product: A = u * vᵀ. -/
declfun outerProduct (u v : β) : α

-- ============================================================================
-- Block Matrix Operations
-- ============================================================================

/-- Get row i as a vector. -/
declfun getRow {α β} (m : α) (i : Int) : β

/-- Get column j as a vector. -/
declfun getColumn {α β} (m : α) (j : Int) : β

/-- Set row i to vector v. -/
declfun setRow {α β} (m : α) (i : Int) (v : β) : α

/-- Set column j to vector v. -/
declfun setColumn {α β} (m : α) (j : Int) (v : β) : α

/-- Swap rows i and j. -/
declfun swapRows {α} (m : α) (i j : Int) : α

/-- Swap columns i and j. -/
declfun swapColumns {α} (m : α) (i j : Int) : α

end HouLean.LinearAlgebra
