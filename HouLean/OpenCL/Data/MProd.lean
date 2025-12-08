import HouLean.OpenCL.Compiler.Main

namespace HouLean.OpenCL


-- todo: this all should just be `deriving OpenCLType for MProd`
--       Ideally `MProd` will be so called "macro type" that should be completely eliminated from
--       runtime
instance [a : OpenCLType α] [b : OpenCLType β] : OpenCLType (MProd α β) where
  name := s!"mprod{a.shortName}{b.shortName}"
  shortName := s!"mp{a.shortName}{b.shortName}"
  definition? :=
    s!"struct prod{a.shortName}{b.shortName}\n\
       \{\n\
         {a.name} fst;\n\
         {b.name} snd;\n\
       };"


implemented_by [Inhabited α] [Inhabited β] [t : OpenCLType (MProd α β)] :
  MProd.mk (α:=α) (β:=β)
  =
  oclFunction _ s!"({t.name})" .constructor

implemented_by [Inhabited α] :
  MProd.fst (α:=α) (β:=β)
  =
  oclFunction _ ".fst" .postfix

implemented_by [Inhabited β] :
  MProd.snd (α:=α) (β:=β)
  =
  oclFunction _ s!".snd" .postfix

-- the above are not sufficient as they do not deal with `Expr.proj`
-- we might fix that by reverting `Expr.proj` to projection functions in the compiler
implemented_by [Inhabited α] (x : MProd α β) :
  x.1
  =
  (oclFunction (MProd α β → α) ".fst" .postfix) x

implemented_by [Inhabited β] (x : MProd α β) :
  x.2
  =
  (oclFunction (MProd α β → β) ".snd" .postfix) x
