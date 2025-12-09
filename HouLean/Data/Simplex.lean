import HouLean.Data.Defs
import HouLean.Data.Matrix
import HouLean.Data.LinearAlgebra.Basic


namespace HouLean

open Math LinearAlgebra

/-- Simplex i.e. segment, triangle, tetrahedron, ... in 1, 2, 3, ... dimensions. -/
structure Simplex (R : Type) (dim : Nat) [FloatType R] where
  /-- Matrix from barycentric to world coordinates. -/
  toWorld : Matrix R (dim+1) (dim+1)
  /-- Matrix from barycentric to world coordinates. -/
  toBarycentric : Matrix R (dim+1) (dim+1)


variable {R} [FloatType R]


def Simplex.fromPoints (points : Fin (dim+1) → Vector R dim) : Simplex R dim :=
  let A := { data := .ofFn (fun i => (points i).push 1) }
  let iA := LinearAlgebra.inverse A
  {
    toWorld := A
    toBarycentric := iA
  }

def Simplex.point (s : Simplex R dim) (i : Nat) (h : i < dim+1 := by get_elem_tactic) : Vector R dim :=
  (s.toWorld.row i).drop 1


def Simplex.setPoint (s : Simplex R dim) (i : Nat) (p : Vector R dim) (h : i < dim+1 := by get_elem_tactic) :
    Simplex R dim :=
  let p := p.push 1
  let update := p - s.toWorld.row i
  {
    toWorld := s.toWorld
    toBarycentric := s.toBarycentric.inverseRowUpdate i update
  }
