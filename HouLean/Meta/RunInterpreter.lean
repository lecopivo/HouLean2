import Lean
import HouLean.Data.Defs
import HouLean.Meta.Float

open Lean Meta HouLean

namespace HouLean.Meta


def runInterpreter? (α : Type) [t : ToExpr α] (val : Expr) : MetaM (Option α) := do
  try
    let val ← unsafe evalExpr α t.toTypeExpr val
    return some val
  catch _ =>
    return none

def runInterpreter?' (α : Type) (t : Expr) (val : Expr) : MetaM (Option α) := do
  try
    let val ← unsafe evalExpr α t val
    return some val
  catch _ =>
    return none


inductive PrimitiveValue where
  | bool (x : Bool)
  | int (x : Int)
  | nat (x : Nat)
  | usize (x : USize)
  | int16 (x : Int16)
  | uint16 (x : UInt16)
  | int32 (x : Int32)
  | uint32 (x : UInt32)
  | int64 (x : Int64)
  | uint64 (x : UInt64)
  | float32 (x : Float32)
  | float64 (x : Float64)
  | unit

protected def PrimitiveValue.toExpr (v : PrimitiveValue) : Expr :=
  match v with
  | .bool x => toExpr x
  | .int x => toExpr x
  | .nat x => toExpr x
  | .usize x => toExpr x
  | .int16 x => toExpr x
  | .uint16 x => toExpr x
  | .int32 x => toExpr x
  | .uint32 x => toExpr x
  | .int64 x => toExpr x
  | .uint64 x => toExpr x
  | .float32 x => toExpr x
  | .float64 x => toExpr x
  | .unit => toExpr ()

instance : ToExpr PrimitiveValue where
  toExpr := PrimitiveValue.toExpr
  toTypeExpr := (.const ``PrimitiveValue [])

open Qq in
def runInterpreterForPrimitiveTypes? (val : Expr) : MetaM (Option PrimitiveValue) := do
  let type : Q(Type) ← inferType val >>= whnf
  match type with
  | ~q(Bool) => (·.map .bool) <$> runInterpreter? Bool val
  | ~q(Int) => (·.map .int) <$> runInterpreter? Int val
  | ~q(Nat) => (·.map .nat) <$> runInterpreter? Nat val
  | ~q(USize) => (·.map .usize) <$> runInterpreter? USize val
  | ~q(Int16) => (·.map .int16) <$> runInterpreter? Int16 val
  | ~q(UInt16) => (·.map .uint16) <$> runInterpreter? UInt16 val
  | ~q(Int32) => (·.map .int32) <$> runInterpreter? Int32 val
  | ~q(UInt32) => (·.map .uint32) <$> runInterpreter? UInt32 val
  | ~q(Int64) => (·.map .int64) <$> runInterpreter? Int64 val
  | ~q(UInt64) => (·.map .uint64) <$> runInterpreter? UInt64 val
  | ~q(Float32) => (·.map .float32) <$> runInterpreter?' Float32 q(Float32) val
  | ~q(Float64) => (·.map .float64) <$> runInterpreter?' Float64 q(Float64) val
  | ~q(Unit) => return some .unit
  | _ => return none
