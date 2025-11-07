import HouLean.Apex.Lean.TwoWaySwitch

open HouLean Apex Compiler

unsafe instance : ApexType (Decidable P) Bool where
  toApex dec :=
    match dec with
    | .isTrue _ => true
    | .isFalse _ => false
  -- hmm here we are breaking consistency ... lets mark it unsafe for now
  fromApex b := if b then .isTrue sorry_proof else .isFalse sorry_proof

open TwoWaySwitch in
unsafe def Decidable.rec.apex_impl {p : Prop} {motive : Decidable p → Type u} [∀ t, TwoWaySwitch (motive t)]
      (f : (h : ¬p) → motive (isFalse h)) (t : (h : p) → motive (isTrue h)) (dec : Decidable p) : motive dec :=
  twoWaySwitch (α:=motive dec) (unsafeCast (f sorry_proof)) (unsafeCast (t sorry_proof)) (toApex dec)
  
run_meta compilerExt.add (.implementedByName ``Decidable.rec ``Decidable.rec.apex_impl #[some 0, some 1, none, some 2, some 3, some 4]) default

