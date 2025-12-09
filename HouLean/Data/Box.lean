import HouLean.Math
import HouLean.Data.Defs
import HouLean.Data.Vector
import HouLean.Data.Matrix
import HouLean.Data.Float
import HouLean.Data.RigidTransform

open HouLean.Math

namespace HouLean.Box

variable {R} [FloatType R] {dim : Nat}

-- ============================================================================
-- Construction
-- ============================================================================

variable (R dim) in
def empty : Box R dim := { center := 0, size := .ofFn fun _ => -1 }

variable (R dim) in
def unit : Box R dim := { center := 0, size := .ofFn (fun _ => 1) }

def mkMinMax (min max : Vector R dim) : Box R dim :=
  let center := (0.5:R) * (min + max)
  let size := max - min
  { center, size}

def fromPoints (points : Array (Vector R dim)) : Box R dim := Id.run do
  if h : points.size = 0 then
    Box.empty R dim
  else
    let mut bmin := points[0]
    let mut bmax := points[0]
    for h : i in [1:points.size] do
      let p := points[i]
      bmin := min bmin p
      bmax := max bmax p
    Box.mkMinMax bmin bmax


-- ============================================================================
-- Queries
-- ============================================================================

/-- Get minimum corner -/
def min (b : Box R dim) : Vector R dim :=
  b.center - (0.5 : R) * b.size

/-- Get maximum corner -/
def max (b : Box R dim) : Vector R dim :=
  b.center + (0.5 : R) * b.size
