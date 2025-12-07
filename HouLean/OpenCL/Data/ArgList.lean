import HouLean.OpenCL.Basic

namespace HouLean.OpenCL

-- list with compile time support that will translate all elements to just a comma separated expression
inductive ArgList (α : Type u) where
  | nil : ArgList α
  | cons (head : α) (tail : ArgList α) : ArgList α
deriving Inhabited, BEq

@[opencl_csimp]
def ArgList.ofList (as : List α) : ArgList α :=
  match as with
  | [] => ArgList.nil
  | a :: as => ArgList.cons a (.ofList as)

@[opencl_csimp]
def ArgList.ofFn (f : Fin n → α) : ArgList α :=
  go n ⟨0, sorry_proof⟩ ArgList.nil
where
  @[opencl_csimp]
  go (fuel : Nat) (i : Fin n) (xs : ArgList α) : ArgList α :=
    match fuel with
    | 0 => xs
    | fuel+1 =>
      let x := f i
      .cons x (go fuel ⟨i.1+1, sorry_proof⟩ xs)

@[opencl_csimp]def ArgList.size (xs : ArgList α) : Nat :=
  match xs with
  | .nil => 0
  | .cons _ xs => xs.size + 1


@[opencl_csimp high]
theorem argList_one (x : α) :
  ArgList.cons (α:=α) x ArgList.nil = (oclFunction (α → ArgList α) "" (.infix 0)) x := sorry_proof


@[opencl_csimp]
theorem argList_cons (x : α) (xs : ArgList α):
  ArgList.cons (α:=α) x xs
  =
  (oclFunction (α → ArgList α → ArgList α) ", " (.infix 0)) x xs := sorry_proof


-- @[opencl_csimp]
-- theorem argList_cons2 (x y : α) (xs : ArgList α):
--   ArgList.cons (α:=α) x (ArgList.cons y xs)
--   =
--   (oclFunction (α → ArgList α → ArgList α) ", " (.infix 0)) x (.cons y xs) := sorry_proof
