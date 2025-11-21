import Lean

open Lean

namespace HouLean
namespace LeanGraph

initialize currentNodeRef : IO.Ref String ← IO.mkRef ""

/-- Enables `node_name%` elaborator inside `go` -/
def withCurrentNode {m} [Monad m] [MonadLiftT (ST IO.RealWorld) m]
    (name : String) (go : m α) : m α := do
  currentNodeRef.set name
  let a ← go
  currentNodeRef.set ""
  return a

elab "node_name%" : term => do
  let nodeName ← currentNodeRef.get
  if nodeName == "" then
    throwError "Current node has not been defined!"
  return mkStrLit (← currentNodeRef.get)
