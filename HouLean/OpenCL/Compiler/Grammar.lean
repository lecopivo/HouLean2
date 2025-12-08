import HouLean.OpenCL.Basic
import HouLean.OpenCL.Compiler.Main
import HouLean.Meta.RewriteBy

open Lean Meta PrettyPrinter

namespace HouLean.OpenCL.Compiler

/-!
## OpenCL Expression Syntax
-/

declare_syntax_cat oclExpr (behavior := symbol)

-- Literals
syntax:max num : oclExpr
syntax:max scientific (noWs (&"f" <|> &"F" <|> &"l" <|> &"L"))? : oclExpr
syntax:max char : oclExpr
syntax:max str : oclExpr

-- Primary expressions
syntax:max "{" oclExpr,*,? "}" : oclExpr
syntax:max "(" oclExpr ")" : oclExpr
syntax:1000 ident : oclExpr

-- Postfix expressions
syntax:1000 oclExpr:1000 "[" oclExpr "]" : oclExpr
syntax:1000 oclExpr:1000 "(" oclExpr,* ")" : oclExpr
syntax:1000 "(" ident ")" "{" oclExpr,* "}" : oclExpr
syntax:1000 oclExpr:1000 noWs "." noWs ident : oclExpr
syntax:1000 oclExpr:1000 "->" ident "(" oclExpr,* ")" : oclExpr
syntax:1000 oclExpr:1000 "++" : oclExpr
syntax:1000 oclExpr:1000 "--" : oclExpr

-- Unary expressions
syntax:500 "++" oclExpr:500 : oclExpr
syntax:500 "--" oclExpr:500 : oclExpr
syntax:500 "+" oclExpr:500 : oclExpr
syntax:500 "-" oclExpr:500 : oclExpr
syntax:500 "~" oclExpr:500 : oclExpr
syntax:500 "!" oclExpr:500 : oclExpr

-- Binary expressions
syntax:120 oclExpr:120 " * " oclExpr:121 : oclExpr
syntax:120 oclExpr:120 " / " oclExpr:121 : oclExpr
syntax:120 oclExpr:120 " % " oclExpr:121 : oclExpr
syntax:110 oclExpr:110 " + " oclExpr:111 : oclExpr
syntax:110 oclExpr:110 " - " oclExpr:111 : oclExpr
syntax:105 oclExpr:105 " << " oclExpr:106 : oclExpr
syntax:105 oclExpr:105 " >> " oclExpr:106 : oclExpr
syntax:100 oclExpr:100 " < " oclExpr:101 : oclExpr
syntax:100 oclExpr:100 " > " oclExpr:101 : oclExpr
syntax:100 oclExpr:100 " <= " oclExpr:101 : oclExpr
syntax:100 oclExpr:100 " >= " oclExpr:101 : oclExpr
syntax:90 oclExpr:90 " == " oclExpr:91 : oclExpr
syntax:90 oclExpr:90 " != " oclExpr:91 : oclExpr
syntax:80 oclExpr:80 " & " oclExpr:81 : oclExpr
syntax:70 oclExpr:70 " ^ " oclExpr:71 : oclExpr
syntax:60 oclExpr:60 " | " oclExpr:61 : oclExpr
syntax:50 oclExpr:50 " && " oclExpr:51 : oclExpr
syntax:40 oclExpr:40 " || " oclExpr:41 : oclExpr

/-!
## OpenCL Macro (oclExpr → Lean term)
-/

syntax (name := oclMacro) "ocl%(" oclExpr ")" : term

-- Identifier
macro_rules | `(ocl%( $id:ident )) => `($id)

-- constructor
macro_rules | `(ocl%( ($id:ident){ $xs,*} )) => do
  let name := Syntax.mkStrLit ("(" ++ id.getId.eraseMacroScopes.getString! ++ ")")
  `(oclFunction (_ → _) $name .constructor (argList [$[ocl%($xs)],*]))

-- projection
macro_rules | `(ocl%( $x:oclExpr.$field:ident )) => do
  let field := field.getId.eraseMacroScopes.getString!
  let name := Syntax.mkStrLit ("." ++ field)
  `(oclFunction (_ → _) $name .postfix (ocl%($x)))

-- Function call with arbitrary arguments
macro_rules
  | `(ocl%( $fn:ident( $args:oclExpr,* ) )) => do
    let funName := Syntax.mkStrLit fn.getId.eraseMacroScopes.getString!
    let argsExpanded ← args.getElems.mapM fun arg => `(ocl%($arg))
    let argsArray := argsExpanded.toList
    match argsArray with
    | [] => `(oclFunction Unit $funName .normal)
    | [a] => `(oclFunction (_ → _) $funName .normal $a)
    | [a, b] => `(oclFunction (_ → _ → _) $funName .normal $a $b)
    | [a, b, c] => `(oclFunction (_ → _ → _ → _) $funName .normal $a $b $c)
    | [a, b, c, d] => `(oclFunction (_ → _ → _ → _ → _) $funName .normal $a $b $c $d)
    | [a, b, c, d, e] => `(oclFunction (_ → _ → _ → _ → _ → _) $funName .normal $a $b $c $d $e)
    | [a, b, c, d, e, f] => `(oclFunction (_ → _ → _ → _ → _ → _ → _) $funName .normal $a $b $c $d $e $f)
    | _ => Macro.throwError s!"Too many arguments ({argsArray.length}), max supported is 6"

-- Binary operators
macro_rules | `(ocl%( $x + $y ))  => `(oclFunction (_ → _ → _) " + "  .infix ocl%($x) ocl%($y))
macro_rules | `(ocl%( $x - $y ))  => `(oclFunction (_ → _ → _) " - "  .infix ocl%($x) ocl%($y))
macro_rules | `(ocl%( $x * $y ))  => `(oclFunction (_ → _ → _) " * "  .infix ocl%($x) ocl%($y))
macro_rules | `(ocl%( $x / $y ))  => `(oclFunction (_ → _ → _) " / "  .infix ocl%($x) ocl%($y))
macro_rules | `(ocl%( $x % $y ))  => `(oclFunction (_ → _ → _) " % "  .infix ocl%($x) ocl%($y))
macro_rules | `(ocl%( $x << $y )) => `(oclFunction (_ → _ → _) " << " .infix ocl%($x) ocl%($y))
macro_rules | `(ocl%( $x >> $y )) => `(oclFunction (_ → _ → _) " >> " .infix ocl%($x) ocl%($y))
macro_rules | `(ocl%( $x < $y ))  => `(oclFunction (_ → _ → _) " < "  .infix ocl%($x) ocl%($y))
macro_rules | `(ocl%( $x > $y ))  => `(oclFunction (_ → _ → _) " > "  .infix ocl%($x) ocl%($y))
macro_rules | `(ocl%( $x <= $y )) => `(oclFunction (_ → _ → _) " <= " .infix ocl%($x) ocl%($y))
macro_rules | `(ocl%( $x >= $y )) => `(oclFunction (_ → _ → _) " >= " .infix ocl%($x) ocl%($y))
macro_rules | `(ocl%( $x == $y )) => `(oclFunction (_ → _ → _) " == " .infix ocl%($x) ocl%($y))
macro_rules | `(ocl%( $x != $y )) => `(oclFunction (_ → _ → _) " != " .infix ocl%($x) ocl%($y))
macro_rules | `(ocl%( $x & $y ))  => `(oclFunction (_ → _ → _) " & "  .infix ocl%($x) ocl%($y))
macro_rules | `(ocl%( $x ^ $y ))  => `(oclFunction (_ → _ → _) " ^ "  .infix ocl%($x) ocl%($y))
macro_rules | `(ocl%( $x | $y ))  => `(oclFunction (_ → _ → _) " | "  .infix ocl%($x) ocl%($y))
macro_rules | `(ocl%( $x && $y )) => `(oclFunction (_ → _ → _) " && " .infix ocl%($x) ocl%($y))
macro_rules | `(ocl%( $x || $y )) => `(oclFunction (_ → _ → _) " || " .infix ocl%($x) ocl%($y))

-- Unary operators
macro_rules | `(ocl%( + $x )) => `(oclFunction (_ → _) "+" .prefix ocl%($x))
macro_rules | `(ocl%( - $x )) => `(oclFunction (_ → _) "-" .prefix ocl%($x))
-- macro_rules | `(ocl%( ~ $x )) => `(oclFunction (_ → _) "~" .prefix ocl%($x))
macro_rules | `(ocl%( ! $x )) => `(oclFunction (_ → _) "!" .prefix ocl%($x))

/-!
## Unexpander (Lean term → oclExpr)
-/

/-- Extract the inner oclExpr from `ocl%($e)` or convert a raw ident -/
def unwrapOclArg (stx : Syntax) : UnexpandM (TSyntax `oclExpr) :=
  match stx with
  | `(ocl%( $e:oclExpr )) => pure e
  | `($id:ident) => `(oclExpr| $id:ident)
  | _ => throw ()

/-- Unwrap multiple arguments -/
def unwrapOclArgs (args : TSyntaxArray `term) : UnexpandM (Array (TSyntax `oclExpr)) :=
  args.mapM unwrapOclArg

/-- Create binary operator syntax from operator string -/
def mkBinOpExpr (op : String) (x y : TSyntax `oclExpr) : UnexpandM (TSyntax `oclExpr) :=
  match op with
  | " + "  => `(oclExpr| $x + $y)
  | " - "  => `(oclExpr| $x - $y)
  | " * "  => `(oclExpr| $x * $y)
  | " / "  => `(oclExpr| $x / $y)
  | " % "  => `(oclExpr| $x % $y)
  | " << " => `(oclExpr| $x << $y)
  | " >> " => `(oclExpr| $x >> $y)
  | " < "  => `(oclExpr| $x < $y)
  | " > "  => `(oclExpr| $x > $y)
  | " <= " => `(oclExpr| $x <= $y)
  | " >= " => `(oclExpr| $x >= $y)
  | " == " => `(oclExpr| $x == $y)
  | " != " => `(oclExpr| $x != $y)
  | " & "  => `(oclExpr| $x & $y)
  | " ^ "  => `(oclExpr| $x ^ $y)
  | " | "  => `(oclExpr| $x | $y)
  | " && " => `(oclExpr| $x && $y)
  | " || " => `(oclExpr| $x || $y)
  | _ => throw ()

/-- Create unary operator syntax from operator string -/
def mkUnaryOpExpr (op : String) (x : TSyntax `oclExpr) : UnexpandM (TSyntax `oclExpr) :=
  match op with
  | "+" => `(oclExpr| + $x)
  | "-" => `(oclExpr| - $x)
  -- | "~" => `(oclExpr| ~ $x)
  | "!" => `(oclExpr| ! $x)
  | _ => throw ()

/-- Create function call syntax with arbitrary arguments -/
def mkFunCallExpr (name : String) (args : Array (TSyntax `oclExpr)) : UnexpandM (TSyntax `oclExpr) := do
  let id := mkIdent (.mkSimple name)
  let sep := mkAtom ","
  let argsSep : Syntax.TSepArray `oclExpr "," := ⟨mkSepArray args sep⟩
  `(oclExpr| $id:ident( $argsSep,* ))


@[app_unexpander oclFunction]
def unexpandOclFunction : Unexpander
  -- Infix binary operators
  | `($_ $_ $opStr:str OpenCLFunction.FunKind.infix $lhs $rhs) => do
    let x ← unwrapOclArg lhs
    let y ← unwrapOclArg rhs
    let inner ← mkBinOpExpr opStr.getString x y
    `(ocl%( $inner:oclExpr ))

  -- Prefix unary operators
  | `($_ $_ $opStr:str OpenCLFunction.FunKind.prefix $arg) => do
    let x ← unwrapOclArg arg
    let inner ← mkUnaryOpExpr opStr.getString x
    `(ocl%( $inner:oclExpr ))

  -- constructor
  | `($_ $_ $n:str OpenCLFunction.FunKind.constructor $l) => do
    match l with
    | `(argList [$xs,*]) =>
      let ty := mkIdent (.mkSimple (n.getString.drop 1 |>.dropRight 1))
      let xs ← xs.getElems.mapM unwrapOclArg
      `(ocl%( ($ty){$xs,*} ))
    | _ => throw ()

  -- projection/postrif
  | `($_ $_ $n:str OpenCLFunction.FunKind.postfix $x) => do
    let field := mkIdent (.mkSimple (n.getString.drop 1))
    let x ← unwrapOclArg x
    `(ocl%( $x.$field ))

  -- todo: merge this into one case
  -- Normal function calls (0-6 arguments)
  | `($_ $_ $n:str OpenCLFunction.FunKind.normal) => do
    let inner ← mkFunCallExpr n.getString #[]
    `(ocl%( $inner:oclExpr ))

  | `($_ $_ $n:str OpenCLFunction.FunKind.normal $a) => do
    let args ← unwrapOclArgs #[a]
    let inner ← mkFunCallExpr n.getString args
    `(ocl%( $inner:oclExpr ))

  | `($_ $_ $n:str OpenCLFunction.FunKind.normal $a $b) => do
    let args ← unwrapOclArgs #[a, b]
    let inner ← mkFunCallExpr n.getString args
    `(ocl%( $inner:oclExpr ))

  | `($_ $_ $n:str OpenCLFunction.FunKind.normal $a $b $c) => do
    let args ← unwrapOclArgs #[a, b, c]
    let inner ← mkFunCallExpr n.getString args
    `(ocl%( $inner:oclExpr ))

  | `($_ $_ $n:str OpenCLFunction.FunKind.normal $a $b $c $d) => do
    let args ← unwrapOclArgs #[a, b, c, d]
    let inner ← mkFunCallExpr n.getString args
    `(ocl%( $inner:oclExpr ))

  | `($_ $_ $n:str OpenCLFunction.FunKind.normal $a $b $c $d $e) => do
    let args ← unwrapOclArgs #[a, b, c, d, e]
    let inner ← mkFunCallExpr n.getString args
    `(ocl%( $inner:oclExpr ))

  | `($_ $_ $n:str OpenCLFunction.FunKind.normal $a $b $c $d $e $f) => do
    let args ← unwrapOclArgs #[a, b, c, d, e, f]
    let inner ← mkFunCallExpr n.getString args
    `(ocl%( $inner:oclExpr ))

  | _ => throw ()



end HouLean.OpenCL.Compiler
