import HouLean.Math
import HouLean.Data.Defs
import HouLean.Data.Vector3
import HouLean.Data.Vector4
import HouLean.Data.Float
import HouLean.Data.RigidTransform

open HouLean.Math

namespace HouLean.RigidVelocity

-- ============================================================================
-- Construction
-- ============================================================================

def zero : RigidVelocity := ⟨⟨0,0,0⟩, ⟨0,0,0⟩⟩

def fromVelocity (v : Vector3) : RigidVelocity :=
  ⟨v, ⟨0,0,0⟩⟩

def fromAngularVelocity (w : Vector3) : RigidVelocity :=
  ⟨⟨0,0,0⟩, w⟩

-- ============================================================================
-- Lie algebra operations (vector space structure)
-- ============================================================================

defun add (a b : RigidVelocity) : RigidVelocity :=
  ⟨a.velocity + b.velocity,
   a.angularVelocity + b.angularVelocity⟩

instance : HAdd RigidVelocity RigidVelocity RigidVelocity := ⟨add⟩

defun sub (a b : RigidVelocity) : RigidVelocity :=
  ⟨a.velocity - b.velocity,
   a.angularVelocity - b.angularVelocity⟩

instance : HSub RigidVelocity RigidVelocity RigidVelocity := ⟨sub⟩

defun neg (v : RigidVelocity) : RigidVelocity :=
  ⟨-v.velocity, -v.angularVelocity⟩

instance : Neg RigidVelocity := ⟨neg⟩

def smul (v : RigidVelocity) (t : Float) : RigidVelocity :=
  ⟨t * v.velocity, t * v.angularVelocity⟩

instance : HMul Float RigidVelocity RigidVelocity :=
  ⟨fun s v => smul v s⟩
instance : HMul RigidVelocity Float RigidVelocity := ⟨smul⟩

defun hDiv (v : RigidVelocity) (t : Float) : RigidVelocity :=
  smul v (1.0 / t)

-- ============================================================================
-- Integration (exponential map to RigidTransform)
-- ============================================================================

/-- Integrate velocity over time to get a transform (exponential map) -/
def toTransform (vel : RigidVelocity) (dt : Float) : RigidTransform :=
  let t := dt * vel.velocity
  let aa := dt * vel.angularVelocity
  let q := if aa.length == 0 then ⟨0,0,0,1⟩
           else aa.normalized.axisAngleToQuat aa.length
  ⟨t, q⟩


-- ============================================================================
-- Differentiation (logarithm map from RigidTransform)
-- ============================================================================

/-- Extract velocity from a transform with time step dt (logarithm map) -/
def fromTransform (xform : RigidTransform) (dt : Float := 1) : RigidVelocity :=
  let (axis, angle) := xform.orient.quatToAxisAngle
  let angVel := (angle / dt) * axis
  let vel := xform.translate / dt
  ⟨vel, angVel⟩

/-- Compute velocity needed to go from xform1 to xform2 in time dt -/
def fromTransformDelta (xform1 xform2 : RigidTransform) (dt : Float) : RigidVelocity :=
  let delta := xform1.inverse.compose xform2
  fromTransform delta dt

-- ============================================================================
-- Comparison operations
-- ============================================================================

defun beq (a b : RigidVelocity) : Bool :=
  a.velocity.beq b.velocity &&
  a.angularVelocity.beq b.angularVelocity

instance : BEq RigidVelocity := ⟨beq⟩

-- ============================================================================
-- Magnitude and norms
-- ============================================================================

/-- Total magnitude (Euclidean norm in velocity space) -/
def magnitude (vel : RigidVelocity) : Float :=
  Float.sqrt (vel.velocity.length2 + vel.angularVelocity.length2)

/-- Linear speed -/
def linearSpeed (vel : RigidVelocity) : Float :=
  vel.velocity.length

/-- Angular speed (magnitude of angular velocity) -/
def angularSpeed (vel : RigidVelocity) : Float :=
  vel.angularVelocity.length

/-- Normalize velocity to unit magnitude -/
def normalized (vel : RigidVelocity) : RigidVelocity :=
  let mag := vel.magnitude
  if mag == 0 then vel else vel.hDiv mag

/-- Distance between two velocities -/
def distance (a b : RigidVelocity) : Float :=
  (a - b).magnitude

-- ============================================================================
-- Interpolation
-- ============================================================================

defun lerp (a b : RigidVelocity) (t : Float) : RigidVelocity :=
  ⟨a.velocity.lerp b.velocity t,
   a.angularVelocity.lerp b.angularVelocity t⟩

-- ============================================================================
-- Component access and modification
-- ============================================================================

def withVelocity (vel : RigidVelocity) (v : Vector3) : RigidVelocity :=
  ⟨v, vel.angularVelocity⟩

def withAngularVelocity (vel : RigidVelocity) (w : Vector3) : RigidVelocity :=
  ⟨vel.velocity, w⟩

-- ============================================================================
-- Physical quantities
-- ============================================================================

/-- Kinetic energy (assuming unit mass/inertia) -/
def kineticEnergy (vel : RigidVelocity) : Float :=
  0.5 * (vel.velocity.length2 + vel.angularVelocity.length2)

/-- Dot product in velocity space -/
defun dot (a b : RigidVelocity) : Float :=
  a.velocity.dot b.velocity +
  a.angularVelocity.dot b.angularVelocity

-- ============================================================================
-- Screw motion utilities
-- ============================================================================

/-- Get screw axis (direction of angular velocity) -/
def screwAxis (vel : RigidVelocity) : Vector3 :=
  let angSpeed := vel.angularSpeed
  if angSpeed == 0 then ⟨0, 1, 0⟩
  else vel.angularVelocity.hDiv angSpeed

/-- Get pitch (linear velocity along screw axis per unit angular velocity) -/
def pitch (vel : RigidVelocity) : Float :=
  let angSpeed := vel.angularSpeed
  if angSpeed == 0 then 0
  else vel.velocity.dot (vel.screwAxis) / angSpeed

/-- Decompose into rotation about axis and translation along axis -/
def decompose (vel : RigidVelocity) : RigidVelocity × RigidVelocity :=
  let axis := vel.screwAxis
  let angSpeed := vel.angularSpeed

  if angSpeed == 0 then
    -- Pure translation
    (⟨⟨0,0,0⟩, ⟨0,0,0⟩⟩,
     ⟨vel.velocity, ⟨0,0,0⟩⟩)
  else
    -- Decompose velocity into parallel and perpendicular components
    let velParallel := (vel.velocity.dot axis) * axis
    let velPerp := vel.velocity - velParallel

    (⟨velPerp, vel.angularVelocity⟩,
     ⟨velParallel, ⟨0,0,0⟩⟩)

-- ============================================================================
-- Apply velocity to points (instantaneous change)
-- ============================================================================

/-- Compute instantaneous change in point position -/
def applyToPoint (vel : RigidVelocity) (p : Vector3) : Vector3 :=
  -- v_point = v_linear + ω × p
  vel.velocity + vel.angularVelocity.cross p

/-- Compute instantaneous change in vector (no translation) -/
def applyToVector (vel : RigidVelocity) (v : Vector3) : Vector3 :=
  vel.angularVelocity.cross v

-- ============================================================================
-- Clamping and limiting
-- ============================================================================

/-- Clamp velocity components to maximum values -/
def clamp (vel : RigidVelocity)
          (maxLinear maxAngular : Float) : RigidVelocity :=
  let linSpeed := vel.linearSpeed
  let angSpeed := vel.angularSpeed

  let linClamped := if linSpeed > maxLinear && linSpeed > 0 then
    (maxLinear / linSpeed) * vel.velocity
  else vel.velocity

  let angClamped := if angSpeed > maxAngular && angSpeed > 0 then
    (maxAngular / angSpeed) * vel.angularVelocity
  else vel.angularVelocity

  ⟨linClamped, angClamped⟩

/-- Clamp total magnitude while preserving direction -/
def clampMagnitude (vel : RigidVelocity) (maxMag : Float) : RigidVelocity :=
  let mag := vel.magnitude
  if mag > maxMag && mag > 0 then
    (maxMag / mag) * vel
  else vel

-- ============================================================================
-- Time stepping and simulation
-- ============================================================================

/-- Apply velocity to transform for one time step (forward Euler) -/
def step (vel : RigidVelocity) (xform : RigidTransform) (dt : Float) : RigidTransform :=
  let delta := vel.toTransform dt
  xform.compose delta

end HouLean.RigidVelocity
