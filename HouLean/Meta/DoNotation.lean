import Lean
import Lean.Parser.Do
import Lean.Parser.Term
import HouLean.Init
import HouLean.Data.Vector
import HouLean.Data.Matrix

open Lean Parser

namespace HouLean.Meta

--- Syntax for: x += y, x -= y, x *= y
syntax atomic(Term.ident Term.optType) " += " term : doElem
syntax atomic(Term.ident Term.optType) " -= " term : doElem
syntax atomic(Term.ident Term.optType) " *= " term : doElem
syntax atomic(Term.ident Term.optType) " *.= " term : doElem
syntax atomic(Term.ident Term.optType) " /= " term : doElem

--- Rules for: x += y, x -= y, x *= y
macro_rules
| `(doElem| $x:ident $[: $ty]? += $e) => `(doElem| $x:ident $[: $ty]? := $x:ident + $e)
macro_rules
| `(doElem| $x:ident $[: $ty]? -= $e) => `(doElem| $x:ident $[: $ty]? := $x:ident - $e)
macro_rules
| `(doElem| $x:ident $[: $ty]? *= $e) => `(doElem| $x:ident $[: $ty]? := $x:ident * $e)
macro_rules
| `(doElem| $x:ident $[: $ty]? *.= $e) => `(doElem| $x:ident $[: $ty]? := $e * $x:ident)
macro_rules
| `(doElem| $x:ident $[: $ty]? /= $e) => `(doElem| $x:ident $[: $ty]? := $x:ident / $e)


macro (priority:=high) x:ident noWs "[" i:term "]" " := " xi:term : doElem => do
  `(doElem| $x:ident := setElem $x $i $xi (by get_elem_tactic))

macro (priority:=high) x:ident noWs "[" i:term ", " is:term,* "]" " := " xi:term : doElem => do
  `(doElem| $x:ident := setElem $x ($i,$is,*) $xi (by constructor <;> get_elem_tactic))
