import HouLean.Math
import HouLean.Data.Defs
import HouLean.Data.Vector3
import HouLean.Data.Vector4
import HouLean.Data.Float
import HouLean.Data.RigidTransform
import HouLean.Data.RigidScaleTransform
import HouLean.Data.RigidVelocity

open HouLean.Math

namespace HouLean.RigidScaleVelocity

-- ============================================================================
-- Construction
-- ============================================================================

def zero : RigidScaleVelocity := ⟨RigidVelocity.zero, 0⟩

def fromVelocity (v : Vector3) : RigidScaleVelocity :=
  ⟨RigidVelocity.fromVelocity v, 0⟩

def fromAngularVelocity (w : Vector3) : RigidScaleVelocity :=
  ⟨RigidVelocity.fromAngularVelocity w, 0⟩

def fromScaleVelocity (s : Float) : RigidScaleVelocity :=
  ⟨RigidVelocity.zero, s⟩

def fromRigidVelocity (rv : RigidVelocity) : RigidScaleVelocity :=
  ⟨rv, 0⟩

-- ============================================================================
-- Lie algebra operations (vector space structure)
-- ============================================================================

defun add (a b : RigidScaleVelocity) : RigidScaleVelocity :=
  ⟨a.toRigidVelocity.add b.toRigidVelocity,
   a.scaleVelocity + b.scaleVelocity⟩

instance : HAdd RigidScaleVelocity RigidScaleVelocity RigidScaleVelocity := ⟨add⟩

defun sub (a b : RigidScaleVelocity) : RigidScaleVelocity :=
  ⟨a.toRigidVelocity.sub b.toRigidVelocity,
   a.scaleVelocity - b.scaleVelocity⟩

instance : HSub RigidScaleVelocity RigidScaleVelocity RigidScaleVelocity := ⟨sub⟩

defun neg (v : RigidScaleVelocity) : RigidScaleVelocity :=
  ⟨v.toRigidVelocity.neg, -v.scaleVelocity⟩

instance : Neg RigidScaleVelocity := ⟨neg⟩

def smul (v : RigidScaleVelocity) (t : Float) : RigidScaleVelocity :=
  ⟨v.toRigidVelocity.smul t, t * v.scaleVelocity⟩

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
  let rigidPart := vel.toRigidVelocity.toTransform dt
  let s := 1.0 + dt * vel.scaleVelocity
  ⟨rigidPart, s⟩


-- ============================================================================
-- Differentiation (logarithm map from RigidScaleTransform)
-- ============================================================================

/-- Extract velocity from a transform with time step dt (logarithm map) -/
def fromTransform (xform : RigidScaleTransform) (dt : Float := 1) : RigidScaleVelocity :=
  let rigidVel := RigidVelocity.fromTransform xform.toRigidTransform dt
  let scaleVel := (xform.scale - 1.0) / dt
  ⟨rigidVel, scaleVel⟩

/-- Compute velocity needed to go from xform1 to xform2 in time dt -/
def fromTransformDelta (xform1 xform2 : RigidScaleTransform) (dt : Float) : RigidScaleVelocity :=
  let delta := xform1.inverse.compose xform2
  fromTransform delta dt

-- ============================================================================
-- Comparison operations
-- ============================================================================

defun beq (a b : RigidScaleVelocity) : Bool :=
  a.toRigidVelocity.beq b.toRigidVelocity &&
  a.scaleVelocity == b.scaleVelocity

instance : BEq RigidScaleVelocity := ⟨beq⟩

-- ============================================================================
-- Magnitude and norms
-- ============================================================================

/-- Total magnitude (Euclidean norm in velocity space) -/
def magnitude (vel : RigidScaleVelocity) : Float :=
  Float.sqrt (vel.toRigidVelocity.velocity.length2 +
              vel.toRigidVelocity.angularVelocity.length2 +
              vel.scaleVelocity * vel.scaleVelocity)

/-- Linear speed -/
def linearSpeed (vel : RigidScaleVelocity) : Float :=
  vel.toRigidVelocity.linearSpeed

/-- Angular speed (magnitude of angular velocity) -/
def angularSpeed (vel : RigidScaleVelocity) : Float :=
  vel.toRigidVelocity.angularSpeed

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
  ⟨a.toRigidVelocity.lerp b.toRigidVelocity t,
   a.scaleVelocity + (b.scaleVelocity - a.scaleVelocity) * t⟩

-- ============================================================================
-- Component access and modification
-- ============================================================================

def withVelocity (vel : RigidScaleVelocity) (v : Vector3) : RigidScaleVelocity :=
  ⟨vel.toRigidVelocity.withVelocity v, vel.scaleVelocity⟩

def withAngularVelocity (vel : RigidScaleVelocity) (w : Vector3) : RigidScaleVelocity :=
  ⟨vel.toRigidVelocity.withAngularVelocity w, vel.scaleVelocity⟩

def withScaleVelocity (vel : RigidScaleVelocity) (s : Float) : RigidScaleVelocity :=
  ⟨vel.toRigidVelocity, s⟩

-- ============================================================================
-- Physical quantities
-- ============================================================================

/-- Kinetic energy (assuming unit mass/inertia) -/
def kineticEnergy (vel : RigidScaleVelocity) : Float :=
  vel.toRigidVelocity.kineticEnergy

/-- Dot product in velocity space -/
defun dot (a b : RigidScaleVelocity) : Float :=
  a.toRigidVelocity.dot b.toRigidVelocity +
  a.scaleVelocity * b.scaleVelocity

-- ============================================================================
-- Screw motion utilities
-- ============================================================================

/-- Get screw axis (direction of angular velocity) -/
def screwAxis (vel : RigidScaleVelocity) : Vector3 :=
  vel.toRigidVelocity.screwAxis

/-- Get pitch (linear velocity along screw axis per unit angular velocity) -/
def pitch (vel : RigidScaleVelocity) : Float :=
  vel.toRigidVelocity.pitch

/-- Decompose into rotation about axis and translation along axis -/
def decompose (vel : RigidScaleVelocity) : RigidScaleVelocity × RigidScaleVelocity :=
  let (rotPart, transPart) := vel.toRigidVelocity.decompose
  (⟨rotPart, vel.scaleVelocity⟩,
   ⟨transPart, 0⟩)

-- ============================================================================
-- Apply velocity to points (instantaneous change)
-- ============================================================================

/-- Compute instantaneous change in point position -/
def applyToPoint (vel : RigidScaleVelocity) (p : Vector3) : Vector3 :=
  -- v_point = v_linear + ω × p + s * p (where s is scale velocity)
  vel.toRigidVelocity.applyToPoint p + vel.scaleVelocity * p

/-- Compute instantaneous change in vector (no translation) -/
def applyToVector (vel : RigidScaleVelocity) (v : Vector3) : Vector3 :=
  vel.toRigidVelocity.applyToVector v + vel.scaleVelocity * v

-- ============================================================================
-- Clamping and limiting
-- ============================================================================

/-- Clamp velocity components to maximum values -/
def clamp (vel : RigidScaleVelocity)
          (maxLinear maxAngular maxScale : Float) : RigidScaleVelocity :=
  let rigidClamped := vel.toRigidVelocity.clamp maxLinear maxAngular
  let scaleClamped := vel.scaleVelocity.clamp (-maxScale) maxScale
  ⟨rigidClamped, scaleClamped⟩

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
