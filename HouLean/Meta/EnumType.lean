import Lean

import HouLean.Meta.Basic

namespace HouLean

class EnumType (A : Type u) where
  {n : Nat}
  toFin : A → Fin n
  fromFin : Fin n → A

namespace Meta


open Lean Parser.Term Elab Deriving Meta Command


def mkEnumTypeInstance (decl : Name) : CommandElabM Bool := do
  unless ← isEnumType decl do
    throwError m!"{decl} is not an enum type!"

  let ConstantInfo.inductInfo info ← getConstInfo decl | panic! "bug in {decl_name%}"

  let n := Syntax.mkNatLit info.ctors.length
  let declId := mkIdent decl

  let alts : Array (TSyntax ``matchAlt) ←
    info.ctors.toArray.mapIdxM (fun idx ctor =>
      let ctorId := Lean.mkIdent ctor
      let idx := Lean.Syntax.mkNatLit idx
      `(matchAltExpr| | $ctorId => ($idx : Fin $n)))
  let toFin ← `(fun x : $declId =>
                    match x with
                    $alts:matchAlt*)

  let alts : Array (TSyntax ``matchAlt) ←
    info.ctors.toArray.mapIdxM (fun idx ctor =>
      let ctorId := Lean.mkIdent ctor
      let idx := Lean.Syntax.mkNatLit idx
      `(matchAltExpr| | $idx => $ctorId ))
  let fromFin ← `(fun i : Fin $n =>
                    match i with
                    $alts:matchAlt*)

  let inst ← `(instance : EnumType $declId := EnumType.mk $toFin $fromFin)

  try
    elabCommand inst
    return true
  catch _ =>
    return false


def mkEnumTypeInstances (declNames : Array Name) : CommandElabM Bool := do
  declNames.allM mkEnumTypeInstance


initialize registerDerivingHandler ``EnumType mkEnumTypeInstances
