
-- Additive structure
instance [Add α] [Add β] : Add (α × β) where
  add := fun xy xy' => (xy.1 + xy'.1, xy.2 + xy'.2)

instance [Zero α] [Zero β] : Zero (α × β) := ⟨(0, 0)⟩

instance [Neg α] [Neg β] : Neg (α × β) where
  neg := fun xy => (-xy.1, -xy.2)

instance [Sub α] [Sub β] : Sub (α × β) where
  sub := fun xy xy' => (xy.1 - xy'.1, xy.2 - xy'.2)

-- Multiplicative structure
instance [Mul α] [Mul β] : Mul (α × β) where
  mul := fun xy xy' => (xy.1 * xy'.1, xy.2 * xy'.2)

instance [One α] [One β] : One (α × β) := ⟨(1, 1)⟩

instance [Inv α] [Inv β] : Inv (α × β) where
  inv := fun xy => (xy.1⁻¹, xy.2⁻¹)

instance [Div α] [Div β] : Div (α × β) where
  div := fun xy xy' => (xy.1 / xy'.1, xy.2 / xy'.2)

-- Scalar multiplication
instance [SMul α β] [SMul α γ] : SMul α (β × γ) where
  smul := fun a xy => (a • xy.1, a • xy.2)
