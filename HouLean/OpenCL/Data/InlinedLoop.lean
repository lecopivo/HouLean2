import HouLean.OpenCL.Basic
import HouLean.Meta.RewriteBy

namespace HouLean.OpenCL

@[opencl_csimp]
def inlinedLoopM {m} [Monad m] (n : Nat) (f : Fin n → α → m α) (init : α) : m α :=
  go n ⟨0, sorry_proof⟩ init
where
  @[opencl_csimp]
  go (fuel : Nat) (i : Fin n) (x : α) : m α := do
    match fuel with
    | 0 => return x
    | fuel+1 =>
      let a ← f i x
      go fuel ⟨i.1+1, sorry_proof⟩ a

@[opencl_csimp]
def inlinedLoop (n : Nat) (f : Fin n → α → α) (init : α) : α :=
  go n ⟨0, sorry_proof⟩ init
where
  @[opencl_csimp]
  go (fuel : Nat) (i : Fin n) (x : α) : α :=
    match fuel with
    | 0 => x
    | fuel+1 =>
      let a := f i x
      go fuel ⟨i.1+1, sorry_proof⟩ a


-- #check forIn
-- #check Std.Range

-- open Std
-- @[inline] def myForIn' [Monad m] (range : Range) (init : β)
--     (f : (i : Nat) → i ∈ range → β → m (ForInStep β)) : m β :=
--   let rec @[specialize] loop (b : β) (i : Nat)
--       (hs : (i - range.start) % range.step = 0) (hl : range.start ≤ i := by omega) : m β := do
--     if h : i < range.stop then
--       match (← f i ⟨hl, by omega, hs⟩ b) with
--       | .done b  => pure b
--       | .yield b =>
--         have := range.step_pos
--         loop b (i + range.step) (by rwa [Nat.add_comm, Nat.add_sub_assoc hl, Nat.add_mod_left])
--     else
--       pure b
--   have := range.step_pos
--   loop init range.start (by simp)


-- theorem inline_forIn {m : Type u₁ → Type u₂} {β : Type u₁} [Monad m]
--   (range : Range) (init : β) (f : (i : Nat) → i ∈ range → β → m (ForInStep β)) :
--   forIn' range init f
--   =
--   myForIn' range init f := sorry_proof

-- open Lean Meta in
-- simproc_decl inline_fun_let (_) := fun e => do
--   logInfo e
--   let .letE _ t v b _ := e | return .continue
--   logInfo t
--   if t.isForall then
--     return .visit { expr := b.instantiate1 v }
--   else
--     return .continue

-- open Lean Meta in
-- simproc_decl zetaBeta (_) := fun e => do
--   let .app .. := e | return .continue
--   let (fn,args) := e.withApp (fun fn args => (fn,args))
--   unless fn.isFVar do return .continue
--   let val := (← fn.fvarId!.getValue?).getD default
--   logInfo m!"{fn}, {val}"
--   let some f ← fn.fvarId!.getValue? | return .continue
--   let e' := f.beta args
--   logInfo e'
--   return .visit { expr := f.beta args }


-- simproc_decl zetaFVar (_) := fun e => do
--   let .letE _ _ (.fvar id) b _ := e | return .continue
--   return .visit { expr := b.instantiate1 (.fvar id) }

-- simproc_decl liftLets (_) := fun e => do
--   let e' ← Lean.Meta.liftLets e
--   if e == e' then
--     return .continue
--   else
--     return .visit { expr := e' }

-- -- state s = init;
-- -- for (int i = 0; i < n; i++)
-- -- {
-- --   s = f i s
-- -- }


-- variable (n : Nat) (s : Nat)
-- #check (Id.run do
--   let mut c := s
--   let mut d := s
--   for i in [0:5] do
--     -- if i = 2 then
--     --   continue
--     c := c + i
--     d := c*d
--   c+d)
--   rewrite_by
--     simp -zeta only [
--       inline_forIn,forIn,myForIn',
--       bind_pure_comp, map_pure, bind_pure, pure_bind, bind_assoc,
--       myForIn'.loop,
--       ↓reduceDIte, Nat.reduceLT, Nat.reduceAdd, liftLets]
--     -- simp -zeta [zetaBeta]


--     lift_lets
--     simp -zeta only [bind_pure_comp, map_pure, bind_pure, pure_bind, bind_assoc,Id.run]

--     -- unfold myForIn'.loop
--     -- unfold myForIn'.loop
--     -- unfold myForIn'.loop
--     -- simp only [bind_pure,bind_assoc,pure_bind]

--     unfold forIn
--     unfold instForInOfForIn'
--     simp only [forIn',Std.Range.forIn', Std.Range.forIn']




-- -- set_option pp.all true
-- -- open Qq
-- -- run_meta
-- --   let e := q(have a := 10; a + a)
-- --   IO.println e.isLet
