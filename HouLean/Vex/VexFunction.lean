import HouLean.Vex.Parser
import HouLean.Vex.VexToTerm
import Lean

namespace VEX

open Lean.Parser.Category

open Lean Syntax

partial def vexParamListSplit (paramList : TSyntax `vexParamList) : Array (TSyntax `vexFullType × Ident) :=
  let rec go (stx : TSyntax `vexParamList) (acc : Array (TSyntax `vexFullType × Ident)) : 
      Array (TSyntax `vexFullType × Ident) :=
    match stx with
    | `(vexParamList| $param:vexParam) =>
      acc.push (extractParam param)
    | `(vexParamList| $param:vexParam ; $rest:vexParamList) =>
      go rest (acc.push (extractParam param))
    | _ => acc
  go paramList #[]
where
  extractParam (param : TSyntax `vexParam) : TSyntax `vexFullType × Ident :=
    match param with
    | `(vexParam| $ty:vexFullType $id:ident) => (ty, id)
    | `(vexParam| $ty:vexFullType $id:ident [ ]) => (ty, id)
    | _ => panic! s!"Invalid vexParam: {param}"

def mkProdType (ts : List Term) : MacroM Term := do
  match ts with
  | [] => `(Unit)
  | [t] => return t
  | t :: ts => 
    let r ← mkProdType ts
    `(term| $t × $r)

partial def vexParamListToParamsProd (paramList : TSyntax `vexParamList) : MacroM (Array Ident × Term) := do
  let (ts, ids) := (vexParamListSplit paramList).unzip
  let ts ← ts.mapM vexFullTypeToTerm 
  let ts ← mkProdType ts.toList
  return (ids, ts)

open Elab Command
elab "vexfunction" r:vexFullType id:ident "(" _args:vexParamList ")" ";" : command => do
  let declId := mkIdent (id.getId.capitalize.appendAfter "Decl")
  let declCommand ← liftMacroM <| 
    `(command| 
       class $declId (InputType : Type) (ReturnType : Type) where   
         $id:ident : InputType → ReturnType)
  let exportCommand ← `(command| export $declId ($id))

  elabCommand declCommand
  elabCommand exportCommand


elab "vexfunction" r:vexFullType id:ident "(" args:vexParamList ")" "{" vexStmt* "}" : command => do
  let declId := mkIdent (id.getId.capitalize.appendAfter "Decl")
  let command ← liftMacroM <| do
    let binders ← vexParamListToParams args
    let returnType ← vexFullTypeToTerm r
    let (_, inputType) ← vexParamListToParamsProd args
    `(command| 
       instance : $declId $inputType $returnType where   
         $id:ident := default)
  elabCommand command
  elabCommand (← `(command| export $declId ($id)))

    
vexfunction 
float add(float x; float y);

vexfunction 
float add(float x; float y) {
  return x + y;
}

vexfunction 
int add(int x; int y) {
  vector a = v@P + @TimInc * v@v;
  return x * y;
}

#check instAddDeclProdInt
#check instAddDeclProdFloat

#eval (add (1.0, 2.0) : Float)
#eval (add ((1:Int), (2:Int)) : Int)


