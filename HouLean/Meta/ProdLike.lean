import Lean

import HouLean.Meta.Basic

namespace HouLean


/-- This class will be used to transport instances onto structures

For every new class should provide ProdLike and Prod rules. -/
class ProdLike (A : Type u) (B : outParam (Type v)) where
  toProdType : A → B
  fromProdType : B → A
  
export ProdLike (toProdType fromProdType)

namespace Meta

open Lean Meta


/-- For and array of types creat ea big product type and array of projection indices. -/
partial def mkProdLikeProjs (ts : Array Expr) (off : Nat) (s : Expr) (n : Name) : MetaM (Expr × Expr × Array (List Nat)) := do
  if ts.size == 0 then
    return ((.const ``Unit []), (.const ``Unit.unit []), #[])
  else if ts.size == 1 then
    return (ts[0]!, s, #[[]])    
  else if ts.size == 2 then
    let p ← mkAppM ``Prod.mk #[s.proj n off, s.proj n (off+1)]
    return (← mkAppM ``Prod ts, p,#[[0],[1]])
  else if ts.size % 2 == 1 then
    let X := ts[0]!
    let (Y,p,projs) ← mkProdLikeProjs ts[1:] (off+1) s n
    let projs := projs.map (fun proj => 1 :: proj)
    let p ← mkAppM ``Prod.mk #[s.proj n off, p]
    return (← mkAppM ``Prod #[X, Y], p, #[[0]] ++ projs)
  else 
    let m := ts.size / 2
    let (X,p,projs) ← mkProdLikeProjs ts[0:m] off s n
    let (Y,p',projs') ← mkProdLikeProjs ts[m:] (off+m) s n
    let projs := projs.map (fun proj => 0 :: proj)
    let projs' := projs'.map (fun proj => 1 :: proj)
    let p ← mkAppM ``Prod.mk #[p,p']
    return (← mkAppM ``Prod #[X, Y], p, projs ++ projs')


open Elab Command Term
def mkProdLikeInstanceAux (structName : Name) : MetaM Unit := do

  let info ← getConstInfo structName
  let sinfo := getStructureInfo (← getEnv) structName

  let lvls := info.levelParams

  forallTelescope info.type fun parms _ => do
    withLocalDeclD `s (← mkAppM structName parms) fun s => do

    let types ← sinfo.fieldNames.mapIdxM (fun i _ => inferType (s.proj structName i))
    let (P, prod, projs) ← mkProdLikeProjs types 0 s structName

    withLocalDeclD `p P fun p => do

    let projs := projs.map (fun proj => proj.foldl (init:=p) (fun p i => p.proj ``Prod i))
    let mk ← mkAppM (structName.append `mk) projs

    let fromProdType ← mkLambdaFVars #[p] mk
    let toProdType ← mkLambdaFVars #[s] prod

    let inst ← mkAppM ``ProdLike.mk #[toProdType, fromProdType]
    let inst ← mkLambdaFVars parms inst >>= instantiateMVars
    let type ← inferType inst

    let declName := Name.appendAfter `inst s!"{structName}ProdLike"

    let decl : DefinitionVal :=  {
      name := declName
      levelParams := lvls
      type := type
      value := inst
      hints := ReducibilityHints.regular (getMaxHeight (← getEnv) inst + 1)
      safety := .safe
    }
    
    addDecl (Declaration.defnDecl decl)
    -- addDocString declId (mkNullNode bs) doc
    compileDecl (Declaration.defnDecl decl)
    addInstance declName .global 1000
    

def mkProdLikeInstance (declNames : Array Name) : CommandElabM Bool := do

  for declName in declNames do
    try 
      liftTermElabM <| mkProdLikeInstanceAux declName
    catch _ =>
      return false
  return true


initialize registerDerivingHandler ``ProdLike mkProdLikeInstance

