import HouLean.Data.Float
import HouLean.Data.Defs

/-! Basic operations for geometrical shapes

-/

namespace HouLean.Shape

set_option linter.unusedVariables false

variable {S S' : Type} {R : outParam Type} [FloatType R] {dim : outParam Nat}

declfun sdf (s : S) (x : Vector R dim) : R
declfun sdfGrad (s : S) (x : Vector R dim) : R × Vector R dim
declfun closestPoint (s : S) (x : Vector R dim) : Vector R dim
declfun levelSet (s : S) (x : Vector R dim) : R
declfun levelSetGrad (s : S) (x : Vector R dim) : R × Vector R dim
/-- Distance between two shapes, i.e. `min (x ∈ s) (x' ∈ S'), |x - x'|` -/
declfun dist (s : S) (s' : S') : R

declfun boundingBox (s : S) : Box R dim
declfun boundingBall (s : S) : Ball R dim

declfun translate (s : S) (t : Vector R dim) : S
declfun rotate {S' : outParam Type} (s : S) (t : Vector R dim) : S'
declfun scale (s : S) (r : R) : S
declfun transform {T : Type} (s : S) (t : T) : S

section BooleanOperations
variable {S'' : outParam Type}

declfun union (s : S) (s' : S') : S''
declfun intersect (s : S) (s' : S') : S''
declfun subtract (s : S) (s' : S') : S''

end BooleanOperations
