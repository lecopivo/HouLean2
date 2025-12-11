import HouLean.Data.Float

namespace HouLean

instance {prec} : FloatType (FloatP prec) :=
  match prec with
  | .single => inferInstanceAs (FloatType Float32)
  | .double => inferInstanceAs (FloatType Float64)

instance : Coe (FloatP .single) Float32 := ⟨fun x => x⟩
instance : Coe (FloatP .double) Float64 := ⟨fun x => x⟩
instance : Coe Float32 (FloatP .single) := ⟨fun x => x⟩
instance : Coe Float64 (FloatP .double) := ⟨fun x => x⟩

/-- Cast precision of a floating point number. Only cast to higher precisions is allowed. -/
@[inline]
def FloatP.precCast (x : FloatP p) (q : Precision) (h : p ≤ q := by (try simp (config := {zetaDelta := true}) [max,LE.le]); (try grind)) : FloatP q :=
  match p, q, h with
  | .single, .single, _ => x
  | .single, .double, _ => x.toFloat
  | .double, .double, _ => x

instance {q} : Coe (FloatP p) (FloatP (max p q)) := ⟨fun x => x.precCast (max p q)⟩
instance {q} : Coe (FloatP p) (FloatP (max q p)) := ⟨fun x => x.precCast (max q p)⟩

instance : Coe Float32 Float64 := ⟨fun x => x.toFloat⟩
instance : Coe Float32 Float64 := ⟨fun x => x.toFloat⟩


-- variable {p q} (x : FloatP p) (y : FloatP q)

-- #check
--   let p' := max p q
--   let x := x.precCast p'
--   let y := x.precCast p'
--   x * y + y
