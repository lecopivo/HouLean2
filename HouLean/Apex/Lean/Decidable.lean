import HouLean.Apex.Lean.TwoWaySwitch

open HouLean Apex Compiler

class ApexDecidable (c : Prop) where
  condition : Bool

instance (priority:=high) (a : Bool) : ApexDecidable (a = true) where
  condition := a

instance (priority:=high) (a : Bool) : ApexDecidable (true = a) where
  condition := a

instance (priority:=high) (a : Bool) : ApexDecidable (a = false) where
  condition := !a

instance (priority:=high) (a : Bool) : ApexDecidable (false = a) where
  condition := !a

instance (a b : Bool) : ApexDecidable (a = b) where
  condition := Generated.EqualsBool a b

instance (a b : Float) : ApexDecidable (a = b) where
  condition := Generated.EqualsFloat a b

instance (a b : Int) : ApexDecidable (a = b) where
  condition := Generated.EqualsInt a b

instance (a b : String) : ApexDecidable (a = b) where
  condition := Generated.EqualsString a b

instance (a b : Vector2) : ApexDecidable (a = b) where
  condition := Generated.EqualsVector2 a b

instance (a b : Vector3) : ApexDecidable (a = b) where
  condition := Generated.EqualsVector3 a b

instance (a b : Vector4) : ApexDecidable (a = b) where
  condition := Generated.EqualsVector4 a b


instance (a b : Bool) : ApexDecidable (a < b) where
  condition := Generated.LessThanBool a b

instance (a b : Float) : ApexDecidable (a < b) where
  condition := Generated.LessThanFloat a b

instance (a b : Int) : ApexDecidable (a < b) where
  condition := Generated.LessThanInt a b

-- instance (a b : Vector2) : ApexDecidable (a < b) where
--   condition := Generated.LessThanVector2 a b

instance (a b : Vector3) : ApexDecidable (a < b) where
  condition := Generated.LessThanVector3 a b

instance (a b : Bool) : ApexDecidable (a ≤ b) where
  condition := Generated.LessThanOrEqualBool a b

instance (a b : Float) : ApexDecidable (a ≤ b) where
  condition := Generated.LessThanOrEqualFloat a b

instance (a b : Int) : ApexDecidable (a ≤ b) where
  condition := Generated.LessThanOrEqualInt a b

-- instance (a b : Vector2) : ApexDecidable (a ≤ b) where
--   condition := Generated.LessThanOrEqualVector2 a b

instance (a b : Vector3) : ApexDecidable (a ≤ b) where
  condition := Generated.LessThanOrEqualVector3 a b


instance (a b : Bool) : ApexDecidable (a > b) where
  condition := Generated.GreaterThanBool a b

instance (a b : Float) : ApexDecidable (a > b) where
  condition := Generated.GreaterThanFloat a b

instance (a b : Int) : ApexDecidable (a > b) where
  condition := Generated.GreaterThanInt a b

-- instance (a b : Vector2) : ApexDecidable (a < b) where
--   condition := Generated.GreaterThanVector2 a b

instance (a b : Vector3) : ApexDecidable (a > b) where
  condition := Generated.GreaterThanVector3 a b

instance (a b : Bool) : ApexDecidable (a ≥ b) where
  condition := Generated.GreaterThanOrEqualBool a b

instance (a b : Float) : ApexDecidable (a ≥ b) where
  condition := Generated.GreaterThanOrEqualFloat a b

instance (a b : Int) : ApexDecidable (a ≥ b) where
  condition := Generated.GreaterThanOrEqualInt a b

-- instance (a b : Vector2) : ApexDecidable (a ≤ b) where
--   condition := Generated.GreaterThanOrEqualVector2 a b

instance (a b : Vector3) : ApexDecidable (a ≥ b) where
  condition := Generated.GreaterThanOrEqualVector3 a b



def decide.apex_impl (p : Prop) [inst : ApexDecidable p] : Bool := inst.condition

run_meta compilerExt.add (.implementedByName ``decide ``decide.apex_impl #[some 0, none]) default


-- unsafe instance : ApexType (Decidable P) Bool where
--   toApex dec :=
--     match dec with
--     | .isTrue _ => true
--     | .isFalse _ => false
--   -- hmm here we are breaking consistency ... lets mark it unsafe for now
--   fromApex b := if b then .isTrue sorry_proof else .isFalse sorry_proof

-- open TwoWaySwitch in
-- unsafe def Decidable.rec.apex_impl {p : Prop} {motive : Decidable p → Type u} [∀ t, TwoWaySwitch (motive t)]
--       (f : (h : ¬p) → motive (isFalse h)) (t : (h : p) → motive (isTrue h)) (dec : Decidable p) : motive dec :=
--   twoWaySwitch (α:=motive dec) (unsafeCast (f sorry_proof)) (unsafeCast (t sorry_proof)) (toApex dec)
  
-- run_meta compilerExt.add (.implementedByName ``Decidable.rec ``Decidable.rec.apex_impl #[some 0, some 1, none, some 2, some 3, some 4]) default

