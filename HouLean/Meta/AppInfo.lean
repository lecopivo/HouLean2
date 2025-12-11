import Lean

open Lean Meta

namespace HouLean.Meta

structure AppInfo where
  fn : Expr
  args : Array Expr
  funInfo : FunInfo
  nameMap : NameMap Nat

def getAppInfo (e : Expr) : MetaM AppInfo := do
  let (fn,args) := e.withApp (fun fn args => (fn,args))
  let funInfo ← getFunInfo fn args.size
  let nameMap : NameMap Nat ←
    forallBoundedTelescope (← inferType fn) args.size fun xs _ => do
      let nameIndex ← xs.mapIdxM (fun i x => do pure (← x.fvarId!.getUserName, i))
      pure (Std.TreeMap.ofArray nameIndex Name.quickCmp)
  return {fn, args, funInfo, nameMap}

def AppInfo.getArg! (info : AppInfo) (name : Name) : Expr := Id.run do
  let some idx := info.nameMap.get? name
    | panic! s!"Unrecognized argument name {name} for function {info.fn}!"
  return info.args[idx]!
