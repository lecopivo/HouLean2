import HouLean.Meta.SpecializeAndSimp2.Encoding

open Lean

namespace HouLean.Meta.Sas

namespace OptionEncoding

-- open Lean Elab Term Command Qq
-- elab "type_encoding " bs:bracketedBinder* " : " r:term ":=" b:term : command => do
--   runTermElabM fun ctx => do
--   elabBinders bs fun xs => do
--     let ret ← elabTermAndSynthesize r none
--     let decls ← (ctx ++ xs).mapM (fun x => do pure (← x.fvarId!.getUserName, (Expr.const ``Expr [])))

--     Meta.withLocalDeclsDND decls fun xs' => do
--       let returnType := q(MetaM Unit)
--       let body ← elabTermAndSynthesize b returnType
--   pure ()

-- variable (a b : Nat)


-- open Lean Meta

-- type_encoding {F F' Xs Y} [Uncurry F Xs Y] [TypeEncoding F (Y:=Y)] [Uncurry F' (Bool × Xs) (Option Y)] :
--     TypeEncoding F' (Xs:=Bool×Xs) (Y:=Option Y) := do

--   let decoderType ← mkAppM ``TypeEncoding.Decoder #[enc]


--   forallTelescope decoderType fun xs r => do
--   withLocalDeclD `valid q(Bool) fun valid => do
--     let newDecoderType ← mkForallFVars (#[valid] ++ xs) (← mkAppM ``Option #[r])
--     -- let
--     let a := α
--     pure ()

instance {α : Type u} [Inhabited α] :
    TypeEncoding (α → Bool → Option α) where
  encode x := (x.getD default, x.isSome)
  decode x valid := if valid then some x else none
  valid := by funext x; cases x <;> simp[uncurry]


end OptionEncoding




structure TypeEncoding' (Y : Type) (X : Fin n → Type) where
  encode : Y → (i : Fin n) → X i
  decode : ((i : Fin n) → X i) → Y
  valid : decode ∘ encode = id



-- structure TypeEncoding (Y : Type) where
--   Decoder : Type
--   {Xs : Type}
--   [unc : Uncurry Decoder Xs Y]
--   encode : Y → Xs
--   decode : Decoder
--   valid : ∀ y, (uncurry decode) (encode y) = y

-- def asdf : TypeEncoding (Option Nat) := {
--   Decoder := Nat → Bool → Option Nat
--   decode := sorry
--   valid := sorry
