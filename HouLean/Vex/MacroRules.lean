import HouLean.Vex.Parser

namespace VEX

syntax (name:=vextype_to_term) "vextype% " vexType : term

macro_rules (kind:=vextype_to_term)
| `(vextype_to_term| vextype% float) => `(Float)

syntax (name:=vex_to_term) "vexexpr% " vexExpr : term

macro_rules (kind:=vex_to_term)
| `(vex_to_term| vexexpr% $id:ident) => `(term| $id)
| `(vex_to_term| vexexpr% $n:num) => `(term| $n)
| `(vex_to_term| vexexpr% $x:scientific) => `(term| $x)
| `(vex_to_term| vexexpr% $x:vexExpr.$id:ident) => `(term| (vexexpr% $x:vexExpr).$id)
| `(vex_to_term| vexexpr% ( $x:vexExpr ) ) => `(term| vexexpr% $x)
| `(vex_to_term| vexexpr% $x:vexExpr + $y:vexExpr) => `(term| (vexexpr% $x) + (vexexpr% $y))


syntax (name:=vex_to_doElem) "vexsnippet% " vexSnippet : doElem

open Lean 
macro_rules (kind:=vex_to_doElem)
| `(vex_to_doElem| vexsnippet% return $x:vexExpr;) =>
  `(doElem| return (vexexpr% $x))
| `(vex_to_doElem| vexsnippet% $lhs:ident = $rhs:vexExpr;) =>
  `(doElem| $lhs:ident := vexexpr% $rhs:vexExpr)
| `(vex_to_doElem| vexsnippet% $ty:vexType $lhs:ident = $rhs:vexExpr;) =>
  `(doElem| let $lhs:ident : (vextype% $ty) := vexexpr% $rhs:vexExpr)
| `(vex_to_doElem| vexsnippet% $[$xs:vexStmt]*) => do
  let xs : Array (TSyntax _) ← xs.mapM (fun x => `(doElem| vexsnippet% $x:vexStmt))
  `(doElem| do $[$xs:doElem]*)

#check
  Id.run do
  let c : Float := 5
  let s : String := "hihi"
  vexsnippet%
    float a = 1.0;
    float b = 2.0 + c + s;
    return a + b;
