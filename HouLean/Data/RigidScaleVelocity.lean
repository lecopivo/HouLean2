import HouLean.Math
import HouLean.Data.Defs
import HouLean.Data.Vector3
import HouLean.Data.Vector4
import HouLean.Data.Float
import HouLean.Data.RigidScaleTransform

open HouLean.Math

namespace HouLean.RigidScaleVelocity

-- ============================================================================
-- Construction
-- ============================================================================

def zero : RigidScaleVelocity := ⟨⟨0,0,0⟩, ⟨0,0,0⟩, 0⟩

def fromVelocity (v : Vector3) : RigidScaleVelocity :=
  ⟨v, ⟨0,0,0⟩, 0⟩

def fromAngularVelocity (w : Vector3) : RigidScaleVelocity :=
  ⟨⟨0,0,0⟩, w, 0⟩

def fromScaleVelocity (s : Float) : RigidScaleVelocity :=
  ⟨⟨0,0,0⟩, ⟨0,0,0⟩, s⟩

-- ============================================================================
-- Lie algebra operations (vector space structure)
-- ============================================================================

defun add (a b : RigidScaleVelocity) : RigidScaleVelocity :=
  ⟨a.velocity + b.velocity,
   a.angularVelocity + b.angularVelocity,
   a.scaleVelocity + b.scaleVelocity⟩

instance : HAdd RigidScaleVelocity RigidScaleVelocity RigidScaleVelocity := ⟨add⟩

defun sub (a b : RigidScaleVelocity) : RigidScaleVelocity :=
  ⟨a.velocity - b.velocity,
   a.angularVelocity - b.angularVelocity,
   a.scaleVelocity - b.scaleVelocity⟩

instance : HSub RigidScaleVelocity RigidScaleVelocity RigidScaleVelocity := ⟨sub⟩

defun neg (v : RigidScaleVelocity) : RigidScaleVelocity :=
  ⟨-v.velocity, -v.angularVelocity, -v.scaleVelocity⟩

instance : Neg RigidScaleVelocity := ⟨neg⟩

def smul (v : RigidScaleVelocity) (t : Float) : RigidScaleVelocity :=
  ⟨t * v.velocity, t * v.angularVelocity, t * v.scaleVelocity⟩

instance : HMul Float RigidScaleVelocity RigidScaleVelocity :=
  ⟨fun s v => smul v s⟩
instance : HMul RigidScaleVelocity Float RigidScaleVelocity := ⟨smul⟩

defun hDiv (v : RigidScaleVelocity) (t : Float) : RigidScaleVelocity :=
  smul v (1.0 / t)

-- ============================================================================
-- Integration (exponential map to RigidScaleTransform)
-- ============================================================================

/-- Integrate velocity over time to get a transform (exponential map) -/
def toTransform (vel : RigidScaleVelocity) (dt : Float) : RigidScaleTransform :=
  let t := dt * vel.velocity
  let aa := dt * vel.angularVelocity
  let q := if aa.length == 0 then ⟨0,0,0,1⟩
           else aa.normalized.axisAngleToQuat aa.length
  let s := 1.0 + dt * vel.scaleVelocity
  ⟨t, q, s⟩


-- ============================================================================
-- Differentiation (logarithm map from RigidScaleTransform)
-- ============================================================================

/-- Extract velocity from a transform with time step dt (logarithm map) -/
def fromTransform (xform : RigidScaleTransform) (dt : Float := 1) : RigidScaleVelocity :=
  let (axis, angle) := xform.orient.quatToAxisAngle
  let angVel := (angle / dt) * axis
  let vel := xform.translate / dt
  let scaleVel := (xform.scale - 1.0) / dt
  ⟨vel, angVel, scaleVel⟩

/-- Compute velocity needed to go from xform1 to xform2 in time dt -/
def fromTransformDelta (xform1 xform2 : RigidScaleTransform) (dt : Float) : RigidScaleVelocity :=
  let delta := xform1.inverse.compose xform2
  fromTransform delta dt

-- ============================================================================
-- Comparison operations
-- ============================================================================

defun beq (a b : RigidScaleVelocity) : Bool :=
  a.velocity.beq b.velocity &&
  a.angularVelocity.beq b.angularVelocity &&
  a.scaleVelocity == b.scaleVelocity

instance : BEq RigidScaleVelocity := ⟨beq⟩

-- ============================================================================
-- Magnitude and norms
-- ============================================================================

/-- Total magnitude (Euclidean norm in velocity space) -/
def magnitude (vel : RigidScaleVelocity) : Float :=
  Float.sqrt (vel.velocity.length2 +
              vel.angularVelocity.length2 +
              vel.scaleVelocity * vel.scaleVelocity)

/-- Linear speed -/
def linearSpeed (vel : RigidScaleVelocity) : Float :=
  vel.velocity.length

/-- Angular speed (magnitude of angular velocity) -/
def angularSpeed (vel : RigidScaleVelocity) : Float :=
  vel.angularVelocity.length

/-- Normalize velocity to unit magnitude -/
def normalized (vel : RigidScaleVelocity) : RigidScaleVelocity :=
  let mag := vel.magnitude
  if mag == 0 then vel else vel.hDiv mag

/-- Distance between two velocities -/
def distance (a b : RigidScaleVelocity) : Float :=
  (a - b).magnitude

-- ============================================================================
-- Interpolation
-- ============================================================================

defun lerp (a b : RigidScaleVelocity) (t : Float) : RigidScaleVelocity :=
  ⟨a.velocity.lerp b.velocity t,
   a.angularVelocity.lerp b.angularVelocity t,
   a.scaleVelocity + (b.scaleVelocity - a.scaleVelocity) * t⟩

-- ============================================================================
-- Component access and modification
-- ============================================================================

def withVelocity (vel : RigidScaleVelocity) (v : Vector3) : RigidScaleVelocity :=
  ⟨v, vel.angularVelocity, vel.scaleVelocity⟩

def withAngularVelocity (vel : RigidScaleVelocity) (w : Vector3) : RigidScaleVelocity :=
  ⟨vel.velocity, w, vel.scaleVelocity⟩

def withScaleVelocity (vel : RigidScaleVelocity) (s : Float) : RigidScaleVelocity :=
  ⟨vel.velocity, vel.angularVelocity, s⟩

-- ============================================================================
-- Physical quantities
-- ============================================================================

/-- Kinetic energy (assuming unit mass/inertia) -/
def kineticEnergy (vel : RigidScaleVelocity) : Float :=
  0.5 * (vel.velocity.length2 + vel.angularVelocity.length2)

/-- Dot product in velocity space -/
defun dot (a b : RigidScaleVelocity) : Float :=
  a.velocity.dot b.velocity +
  a.angularVelocity.dot b.angularVelocity +
  a.scaleVelocity * b.scaleVelocity

-- ============================================================================
-- Screw motion utilities
-- ============================================================================

/-- Get screw axis (direction of angular velocity) -/
def screwAxis (vel : RigidScaleVelocity) : Vector3 :=
  let angSpeed := vel.angularSpeed
  if angSpeed == 0 then ⟨0, 1, 0⟩
  else vel.angularVelocity.hDiv angSpeed

/-- Get pitch (linear velocity along screw axis per unit angular velocity) -/
def pitch (vel : RigidScaleVelocity) : Float :=
  let angSpeed := vel.angularSpeed
  if angSpeed == 0 then 0
  else vel.velocity.dot (vel.screwAxis) / angSpeed

/-- Decompose into rotation about axis and translation along axis -/
def decompose (vel : RigidScaleVelocity) : RigidScaleVelocity × RigidScaleVelocity :=
  let axis := vel.screwAxis
  let angSpeed := vel.angularSpeed

  if angSpeed == 0 then
    -- Pure translation
    (⟨⟨0,0,0⟩, ⟨0,0,0⟩, vel.scaleVelocity⟩,
     ⟨vel.velocity, ⟨0,0,0⟩, 0⟩)
  else
    -- Decompose velocity into parallel and perpendicular components
    let velParallel := (vel.velocity.dot axis) * axis
    let velPerp := vel.velocity - velParallel

    (⟨velPerp, vel.angularVelocity, vel.scaleVelocity⟩,
     ⟨velParallel, ⟨0,0,0⟩, 0⟩)

-- ============================================================================
-- Apply velocity to points (instantaneous change)
-- ============================================================================

/-- Compute instantaneous change in point position -/
def applyToPoint (vel : RigidScaleVelocity) (p : Vector3) : Vector3 :=
  -- v_point = v_linear + ω × p + s * p (where s is scale velocity)
  vel.velocity + vel.angularVelocity.cross p + vel.scaleVelocity * p

/-- Compute instantaneous change in vector (no translation) -/
def applyToVector (vel : RigidScaleVelocity) (v : Vector3) : Vector3 :=
  vel.angularVelocity.cross v + vel.scaleVelocity * v

-- ============================================================================
-- Clamping and limiting
-- ============================================================================

/-- Clamp velocity components to maximum values -/
def clamp (vel : RigidScaleVelocity)
          (maxLinear maxAngular maxScale : Float) : RigidScaleVelocity :=
  let linSpeed := vel.linearSpeed
  let angSpeed := vel.angularSpeed

  let linClamped := if linSpeed > maxLinear && linSpeed > 0 then
    (maxLinear / linSpeed) * vel.velocity
  else vel.velocity

  let angClamped := if angSpeed > maxAngular && angSpeed > 0 then
    (maxAngular / angSpeed) * vel.angularVelocity
  else vel.angularVelocity

  let scaleClamped := vel.scaleVelocity.clamp (-maxScale) maxScale

  ⟨linClamped, angClamped, scaleClamped⟩

/-- Clamp total magnitude while preserving direction -/
def clampMagnitude (vel : RigidScaleVelocity) (maxMag : Float) : RigidScaleVelocity :=
  let mag := vel.magnitude
  if mag > maxMag && mag > 0 then
    (maxMag / mag) * vel
  else vel

-- ============================================================================
-- Time stepping and simulation
-- ============================================================================

/-- Apply velocity to transform for one time step (forward Euler) -/
def step (vel : RigidScaleVelocity) (xform : RigidScaleTransform) (dt : Float) : RigidScaleTransform :=
  let delta := vel.toTransform dt
  xform.compose delta

end HouLean.RigidScaleVelocity
