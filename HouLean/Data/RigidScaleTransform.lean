import HouLean.Data.Vector3
import HouLean.Data.Vector4

namespace HouLean

namespace Transform

def orient (t : RigidScaleTransform) : Vector4 := 
  let (axis, angle) := t.axisAngle.normalize
  let c := Math.cos (angle/2)
  let s := Math.sin (angle/2)
  { x := s * axis.x
    y := s * axis.y
    z := s * axis.z
    w := c }

-- def compose (t s : RigidScaleTransform) : RigidScaleTransform := {

--   translate := 
-- }

-- def apply (t : Transform) (v : Vector3) : Vector3 :=
--   sorry

