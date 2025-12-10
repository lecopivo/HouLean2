import HouLean.OpenCL.Compiler.Main

namespace HouLean.OpenCL


-- todo: this all should just be `deriving OpenCLType for Prod`
--       Ideally `Prod` will be so called "macro type" that should be completely eliminated from
--       runtime
instance [a : OpenCLType α] [b : OpenCLType β] : OpenCLType (Prod α β) where
  name := s!"prod_{a.shortName}_{b.shortName}"
  shortName := s!"p{a.shortName}{b.shortName}" -- `Pointer α` already uses prefix `p`, so maybe use something different
  definition? :=
    s!"struct prod_{a.shortName}_{b.shortName}\n\
       \{\n\
         {a.name} fst;\n\
         {b.name} snd;\n\
       };"


implemented_by [Inhabited α] [Inhabited β] [t : OpenCLType (Prod α β)] :
  Prod.mk (α:=α) (β:=β)
  =
  oclFunction _ s!"({t.name})" .constructor

implemented_by [Inhabited α] :
  Prod.fst (α:=α) (β:=β)
  =
  oclFunction _ ".fst" .postfix

implemented_by [Inhabited β] :
  Prod.snd (α:=α) (β:=β)
  =
  oclFunction _ s!".snd" .postfix

-- the above are not sufficient as they do not deal with `Expr.proj`
-- we might fix that by reverting `Expr.proj` to projection functions in the compiler
implemented_by [Inhabited α] (x : Prod α β) :
  x.1
  =
  (oclFunction (Prod α β → α) ".fst" .postfix) x

implemented_by [Inhabited β] (x : Prod α β) :
  x.2
  =
  (oclFunction (Prod α β → β) ".snd" .postfix) x

-- casesOn
implemented_by {α : Type _} {β : Type _} {motive : Prod α β → Sort _}
  (t : Prod α β) (f : (fst : α) → (snd : β) → motive ⟨fst, snd⟩) :
  (Prod.casesOn t f : motive t)
  =
  f t.1 t.2 := by rfl
