import HouLean.Data.Defs
import HouLean.OpenCL.Compiler.Extension
import HouLean.OpenCL.Compiler.Code

open Lean Meta Qq HouLean

namespace HouLean.OpenCL.Compiler
structure Context where
  fvarMap : ExprMap String := {}
  usedNames : Std.HashMap String Nat := {}

structure State where
  compiledFunctions : Array CodeFunction := #[]
  statements : Array CodeStatement := #[]
  -- all functions that has been compiled in this run

abbrev CompileM := ReaderT Compiler.Context <| StateT State <| SimpM

initialize compileFunctionRef : IO.Ref (Expr → CompileM CodeFunction) ←
  IO.mkRef (fun _ => pure default)

def compileFunction (f : Expr) : CompileM CodeFunction := do
  (← compileFunctionRef.get) f


initialize compileFunctionRef' : IO.Ref (Expr → CompileM CodeFunction') ←
  IO.mkRef (fun _ => pure default)

def compileFunction' (f : Expr) : CompileM CodeFunction' := do
  (← compileFunctionRef'.get) f
