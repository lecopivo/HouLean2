import Lean

open Lean Meta Simp

namespace HouLean.OpenCL.Compiler

structure Context where
  fvarMap : ExprMap String := {}
  usedNames : Std.HashMap String Nat := {}

structure State where
  statements : Array (TSyntax `clStmtLike) := #[]
  -- all functions that has been compiled in this run

abbrev CompileM := ReaderT Context <| StateT State <| MetaM

initialize compileFunctionRef : IO.Ref (Expr → CompileM (TSyntax `clFunction)) ←
  IO.mkRef (fun _ => pure default)

def compileFunction (f : Expr) : CompileM (TSyntax `clFunction) := do
  (← compileFunctionRef.get) f
