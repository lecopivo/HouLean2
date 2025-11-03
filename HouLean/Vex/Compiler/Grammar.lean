/-
VEX Grammar Parser for Lean 4
Based on Houdini VEX language specification
Following the structure of lean4-alloy's C parser
-/

import Lean
import HouLean.Vex.Compiler.ParserUtil

namespace HouLean.Vex.Compiler

open Lean Parser Alloy

register_option VEX.optSemicolon : Bool := {
  defValue := true
  descr := "Should semicolons be optional in VEX code?"
}

/-!
## Basic Abstractions

Forward declarations for syntax categories used in type system.
-/

/-- A `specifier-qualifier` of VEX (type specifiers and qualifiers). -/
declare_syntax_cat vexSpec (behavior := both)

/-- A `type-specifier` of VEX. -/
declare_syntax_cat vexTypeSpec (behavior := both)

syntax vexTypeSpec : vexSpec

/-- A `type-qualifier` of VEX. -/
declare_syntax_cat vexTypeQ (behavior := both)

syntax vexTypeQ : vexSpec

/-- A `direct-declarator` of VEX. -/
declare_syntax_cat vexDirectDeclarator (behavior := both)

/-- A `direct-abstract-declarator` of VEX. -/
declare_syntax_cat vexDirectAbsDeclarator (behavior := both)

/-- A `declarator` of VEX (no pointers in VEX). -/
syntax declarator := vexDirectDeclarator

/-- An `abstract-declarator` of VEX. -/
syntax absDeclarator := vexDirectAbsDeclarator

/-- A VEX type. -/
syntax type := vexSpec+ optional(absDeclarator)

/-!
### Function Parameters (Forward Declaration)

VEX uses semicolon-separated parameter groups where parameters
of the same type can be comma-separated.
-/

/-- A parameter declaration in VEX. -/
declare_syntax_cat vexParamDecl

/-- A comma-separated list of parameters of the same type. -/
declare_syntax_cat vexParamGroup

/-- A semicolon-separated list of parameter groups. -/
declare_syntax_cat vexParamList

/-- An `assignment-expression` (single expression, not comma-separated). -/
declare_syntax_cat vexExpr (behavior := both)

/-- A `constant-expression` of VEX. -/
syntax constExpr := vexExpr:20

/-- A `designator` of VEX. -/
declare_syntax_cat vexDesignator

/-- An `initializer` of VEX. -/
declare_syntax_cat vexInitializer

/-- Designates the index of a VEX array to initialize. -/
syntax "[" constExpr "]" : vexDesignator

/-- Designates the field of a VEX aggregate to initialize. -/
syntax "." ident : vexDesignator

/-- A VEX initializer that uses an expression. -/
syntax vexExpr : vexInitializer

/-- An element of a VEX initializer list. -/
syntax initializerElem := optional(vexDesignator+ "=") vexInitializer

/-- A VEX aggregate initializer that uses an initializer list. -/
syntax "{" initializerElem,*,? "}" : vexInitializer

/-- A VEX statement. -/
declare_syntax_cat vexStmt (behavior := both)

/-- A top-level VEX command. -/
declare_syntax_cat vexCmd (behavior := both)

--------------------------------------------------------------------------------
/-! ## Comments                                                               -/
--------------------------------------------------------------------------------

variable (pushMissingOnError : Bool) in
/-- Finish parsing a VEX block comment. -/
partial def finishCommentBlock (nesting : Nat) : ParserFn := fun c s =>
  let input := c.input
  let i     := s.pos
  if h : input.atEnd i then eoi s
  else
    let curr := input.get' i h
    let i    := input.next' i h
    if curr == '*' then
      if h : input.atEnd i then eoi s
      else
        let curr := input.get' i h
        if curr == '/' then
          if nesting == 1 then s.next' input i h
          else finishCommentBlock (nesting-1) c (s.next' input i h)
        else
          finishCommentBlock nesting c (s.setPos i)
    else if curr == '/' then
      if h : input.atEnd i then eoi s
      else
        let curr := input.get' i h
        if curr == '*' then finishCommentBlock (nesting+1) c (s.next' input i h)
        else finishCommentBlock nesting c (s.setPos i)
    else finishCommentBlock nesting c (s.setPos i)
where
  eoi s := s.mkUnexpectedError (pushMissing := pushMissingOnError) "unterminated comment"

def blockCommentBody :=
  raw (finishCommentBlock (pushMissingOnError := true) 1) (trailingWs := true)

/-- A VEX line comment. -/
syntax lineComment := "//" Alloy.line

/-- A VEX block comment. -/
syntax blockComment := "/*" blockCommentBody

attribute [vexCmd_parser, vexStmt_parser, vexExpr_parser]
  lineComment blockComment

syntax atomic(lineComment) vexStmt : vexStmt
syntax atomic(blockComment) vexStmt : vexStmt
syntax vexStmt lineComment : vexStmt
syntax vexStmt blockComment : vexStmt

syntax atomic(lineComment) vexExpr : vexExpr
syntax atomic(blockComment) vexExpr : vexExpr
syntax vexExpr lineComment : vexExpr
syntax vexExpr blockComment : vexExpr

--------------------------------------------------------------------------------
/-! ## Expressions                                                            -/
--------------------------------------------------------------------------------

/-!
### Primary Expressions
-/

/-- A VEX identifier. -/
syntax:max ident : vexExpr

/-- An integer suffix for VEX integer constants. -/
def intSuffix :=
  identSatisfy ["integer suffix"] fun
  | .str .anonymous s =>
    let s := s.toLower
    let s :=
      if s.front = 'u' then s.drop 1
      else if s.back = 'u' then s.dropRight 1
      else s
    s = "l" || s = "ll"
  | _ => false

/-- A VEX integer constant. -/
syntax:max (name := intConst) num (noWs intSuffix)? : vexExpr

/-- A VEX floating-point constant. -/
syntax:max (name := floatConst) scientific (noWs (&"f" <|> &"F" <|> &"l" <|> &"L"))? : vexExpr

/-- A VEX character constant. -/
syntax:max (name := charConst) char : vexExpr

/-- A VEX string literal. -/
syntax:max str : vexExpr

/-- VEX attribute access with type prefix. -/
syntax:max (name := attrAccess)
  ("s[]@" <|> "i[]@" <|> "f[]@" <|> "v[]@" <|> "p[]@" <|> "u[]@" <|> "d[]@" <|> 
   "m2[]@" <|> "m3[]@" <|> "m4[]@" <|>
   "s@" <|> "i@" <|> "f@" <|> "v@" <|> "p@" <|> "u@" <|> "d@" <|> 
   "m2@" <|> "m3@" <|> "m4@" <|> "@") ident : vexExpr

/-- VEX global variable access. -/
syntax:max "$$" ident : vexExpr

/-- A VEX vector/array literal. -/
syntax:max "{" vexExpr,*,? "}" : vexExpr

/-- A VEX parenthetical expression. -/
syntax:max "(" vexExpr ")" : vexExpr

/-!
### Postfix Expressions
-/

/-- A VEX subscript expression. -/
syntax:1000 vexExpr:1000 "[" vexExpr "]" : vexExpr

/-- A VEX function call expression. -/
syntax:1000 vexExpr:1000 "(" vexExpr,* ")" : vexExpr

/-- A VEX member access expression (including swizzling). -/
syntax:1000 vexExpr:1000 "." ident : vexExpr

/-- A VEX method call expression (for structs). -/
syntax:1000 vexExpr:1000 "->" ident "(" vexExpr,* ")" : vexExpr

/-- A VEX postfix increment expression. -/
syntax:1000 vexExpr:1000 "++" : vexExpr

/-- A VEX postfix decrement expression. -/
syntax:1000 vexExpr:1000 "--" : vexExpr

/-!
### Unary Expressions
-/

/-- A VEX prefix increment expression. -/
syntax:500 "++" vexExpr:500 : vexExpr

/-- A VEX prefix decrement expression. -/
syntax:500 "--" vexExpr:500 : vexExpr

/-- A VEX unary plus expression. -/
syntax:500 "+" vexExpr:500 : vexExpr

/-- A VEX unary minus expression. -/
syntax:500 "-" vexExpr:500 : vexExpr

/-- A VEX bitwise NOT expression. -/
syntax:500 "~" vexExpr:500 : vexExpr

/-- A VEX logical NOT expression. -/
syntax:500 "!" vexExpr:500 : vexExpr

/-- A VEX type cast expression. -/
syntax:500 atomic("(" type ")") vexExpr:500 : vexExpr

/-- A VEX type constructor (for built-in types only). -/
syntax:500 (name := typeConstructor)
  ("int" <|> "float" <|> "vector" <|> "vector2" <|> "vector4" <|> 
   "matrix" <|> "matrix2" <|> "matrix3" <|> "string" <|> "dict" <|> "bsdf")
  "(" vexExpr ")" : vexExpr

/-!
### Multiplicative Expressions
-/

/-- A VEX multiplication expression. -/
syntax:120 (name := mulExpr) vexExpr:120 " * " vexExpr:121 : vexExpr

/-- A VEX division expression. -/
syntax:120 (name := divExpr) vexExpr:120 " / " vexExpr:121 : vexExpr

/-- A VEX modulus expression. -/
syntax:120 (name := remExpr) vexExpr:120 " % " vexExpr:121 : vexExpr

/-!
### Additive Expressions
-/

/-- A VEX addition expression. -/
syntax:110 (name := addExpr) vexExpr:110 " + " vexExpr:111 : vexExpr

/-- A VEX subtraction expression. -/
syntax:110 (name := subExpr) vexExpr:110 " - " vexExpr:111 : vexExpr

/-!
### Shift Expressions
-/

/-- A VEX left shift expression. -/
syntax:105 (name := shlExpr) vexExpr:105 " << " vexExpr:106 : vexExpr

/-- A VEX right shift expression. -/
syntax:105 (name := shrExpr) vexExpr:105 " >> " vexExpr:106 : vexExpr

/-!
### Relational Expressions
-/

/-- A VEX less-than expression. -/
syntax:100 (name := ltExpr) vexExpr:100 " < " vexExpr:101 : vexExpr

/-- A VEX greater-than expression. -/
syntax:100 (name := gtExpr) vexExpr:100 " > " vexExpr:101 : vexExpr

/-- A VEX less-or-equal expression. -/
syntax:100 (name := leExpr) vexExpr:100 " <= " vexExpr:101 : vexExpr

/-- A VEX greater-or-equal expression. -/
syntax:100 (name := geExpr) vexExpr:100 " >= " vexExpr:101 : vexExpr

/-!
### Equality Expressions
-/

/-- A VEX equal-to expression. -/
syntax:90 (name := eqExpr) vexExpr:90 " == " vexExpr:91 : vexExpr

/-- A VEX not-equal-to expression. -/
syntax:90 (name := neExpr) vexExpr:90 " != " vexExpr:91 : vexExpr

/-- A VEX string match expression. -/
syntax:90 (name := matchExpr) vexExpr:90 " ~= " vexExpr:91 : vexExpr

/-!
### Bitwise Expressions
-/

/-- A VEX bitwise AND expression. -/
syntax:80 (name := andExpr) vexExpr:80 " & " vexExpr:81 : vexExpr

/-- A VEX bitwise XOR expression. -/
syntax:70 (name := xorExpr) vexExpr:70 " ^ " vexExpr:71 : vexExpr

/-- A VEX bitwise OR expression. -/
syntax:60 (name := orExpr) vexExpr:60 " | " vexExpr:61 : vexExpr

/-!
### Logical Expressions
-/

/-- A VEX logical AND expression. -/
syntax:50 (name := logicalAndExpr) vexExpr:50 " && " vexExpr:51 : vexExpr

/-- A VEX logical OR expression. -/
syntax:40 (name := logicalOrExpr) vexExpr:40 " || " vexExpr:41 : vexExpr

/-!
### Conditional Expression
-/

/-- A VEX ternary conditional expression. -/
syntax:30 (name := condExpr) vexExpr:31 " ? " vexExpr:10 " : " vexExpr:30 : vexExpr

/-!
### Assignment Expressions
-/

/-- An assignment operator in VEX. -/
declare_syntax_cat vexAssignOp

syntax " = " : vexAssignOp
syntax " *= " : vexAssignOp
syntax " /= " : vexAssignOp
syntax " %= " : vexAssignOp
syntax " += " : vexAssignOp
syntax " -= " : vexAssignOp
syntax " <<= " : vexAssignOp
syntax " >>= " : vexAssignOp
syntax " &= " : vexAssignOp
syntax " ^= " : vexAssignOp
syntax " |= " : vexAssignOp

/-- A VEX assignment expression. -/
syntax:20 (name := assignExpr) vexExpr:500 vexAssignOp vexExpr:20 : vexExpr

/-!
### Comma Expression
-/

/-- A VEX comma expression. -/
syntax:10 (name := commaExpr) vexExpr:10 " , " vexExpr:20 : vexExpr

--------------------------------------------------------------------------------
/-! ## Declaration Syntax                                                     -/
--------------------------------------------------------------------------------

/-!
### Specifiers
-/

/-- A declaration specifier in VEX. -/
declare_syntax_cat vexDeclSpec

syntax vexSpec : vexDeclSpec

/-!
#### Type Qualifiers
-/

/-- The VEX export qualifier (for writing to geometry). -/
syntax "export" : vexTypeQ

/-- The VEX const qualifier. -/
syntax "const" : vexTypeQ

/-- The VEX varying qualifier. -/
syntax "varying" : vexTypeQ

/-- The VEX uniform qualifier. -/
syntax "uniform" : vexTypeQ

/-!
#### Function Specifiers
-/

/-- The VEX function keyword (optional, for disambiguation). -/
syntax "function" : vexDeclSpec

/-!
### Declarators
-/

/-- The name of a declaration. -/
syntax:max ident : vexDirectDeclarator

/-- A parenthesized declarator. -/
syntax:max "(" declarator ")" : vexDirectDeclarator

/-- An array declarator. -/
syntax:arg vexDirectDeclarator:arg "[" (vexExpr)? "]" : vexDirectDeclarator

/-- A function declarator (VEX uses semicolon-separated parameters). -/
syntax:arg vexDirectDeclarator:arg "(" (vexParamList)? ")" : vexDirectDeclarator

/-- A parenthesized abstract declarator. -/
syntax:max "(" absDeclarator ")" : vexDirectAbsDeclarator

/-- An array abstract declarator. -/
syntax:max "[" (vexExpr)? "]" : vexDirectAbsDeclarator

/-- A function abstract declarator. -/
syntax:max "(" (vexParamList)? ")" : vexDirectAbsDeclarator

syntax:arg vexDirectAbsDeclarator:arg "[" (vexExpr)? "]" : vexDirectAbsDeclarator
syntax:arg vexDirectAbsDeclarator:arg "(" (vexParamList)? ")" : vexDirectAbsDeclarator

/-!
### Function Parameters

VEX uses semicolon-separated parameter groups where parameters
of the same type can be comma-separated.
-/

/-- Parameter declaration content. -/
syntax vexDeclSpec+ (atomic(declarator) <|> absDeclarator)? : vexParamDecl

/-- A comma-separated list of parameters of the same type. -/
syntax vexParamDecl ("," ident)* : vexParamGroup

/-- A semicolon-separated list of parameter groups. -/
syntax vexParamGroup (";" vexParamGroup)* : vexParamList

/-!
### Declarations
-/

/-- The semicolon terminator of a VEX statement/declaration. -/
def endSemi : Parser := leading_parser
  withFn (p := optional (symbol ";")) fun p c s =>
    if VEX.optSemicolon.get c.options then p c s else symbolFn ";" c s

/-- Ensure the previous syntax ended with a semicolon token. -/
def checkSemi : Parser :=
  checkStackTop (tailSyntax · |>.isToken ";") "expected ';'"

/-- An init-declarator in VEX. -/
syntax initDeclarator := declarator optional(" = " vexInitializer)

/-- A VEX declaration. -/
syntax declaration :=
  many1OptLookahead(vexDeclSpec, declarator) initDeclarator,* endSemi

-- Also allow declarations as statements
syntax declaration : vexStmt

--------------------------------------------------------------------------------
/-! ## Types                                                                  -/
--------------------------------------------------------------------------------

/-!
### Primitive Types
-/

/-- The VEX void type. -/
syntax "void" : vexTypeSpec

/-- The VEX int type. -/
syntax "int" : vexTypeSpec

/-- The VEX float type. -/
syntax "float" : vexTypeSpec

/-- The VEX vector type (3D vector). -/
syntax "vector" : vexTypeSpec

/-- The VEX vector2 type (2D vector). -/
syntax "vector2" : vexTypeSpec

/-- The VEX vector4 type (4D vector). -/
syntax "vector4" : vexTypeSpec

/-- The VEX matrix type (4x4 matrix). -/
syntax "matrix" : vexTypeSpec

/-- The VEX matrix2 type (2x2 matrix). -/
syntax "matrix2" : vexTypeSpec

/-- The VEX matrix3 type (3x3 matrix). -/
syntax "matrix3" : vexTypeSpec

/-- The VEX string type. -/
syntax "string" : vexTypeSpec

/-- The VEX dict type. -/
syntax "dict" : vexTypeSpec

/-- The VEX bsdf type. -/
syntax "bsdf" : vexTypeSpec

/-- A user-defined VEX type (struct). -/
syntax ident : vexTypeSpec

/-!
### Struct Types
-/

/-- A struct member declaration. -/
declare_syntax_cat vexStructMember

syntax vexTypeSpec ident ("=" vexExpr)? endSemi : vexStructMember
syntax vexTypeSpec ident "[" num "]" endSemi : vexStructMember

/-- A struct definition body. -/
declare_syntax_cat vexStructDef
syntax "{" (lineComment <|> blockComment <|> vexStructMember)* "}" : vexStructDef

/-- A struct signature (name with optional body). -/
declare_syntax_cat vexStructSig
syntax vexStructDef : vexStructSig
syntax ident (vexStructDef)? : vexStructSig

/-- A VEX struct declaration. -/
syntax "struct " vexStructSig : vexTypeSpec

--------------------------------------------------------------------------------
/-! ## Statements                                                             -/
--------------------------------------------------------------------------------

/-!
### Jump Statements
-/

/-- A VEX return statement. -/
syntax "return" (ppSpace vexExpr)? endSemi : vexStmt

/-- A VEX break statement. -/
syntax "break" endSemi : vexStmt

/-- A VEX continue statement. -/
syntax "continue" endSemi : vexStmt

/-!
### Compound Statements
-/

/-- Statement-like syntax (declarations and statements). -/
declare_syntax_cat vexStmtLike
syntax declaration : vexStmtLike
syntax vexStmt : vexStmtLike

/-- A sequence of statements (with optional indentation). -/
declare_syntax_cat vexStmtSeq
syntax many1Indent(vexStmtLike) : vexStmtSeq

/-- A VEX compound statement (block). -/
syntax "{" vexStmtLike* "}" : vexStmt

/-!
### Expression Statements
-/

/-- A VEX expression statement. -/
syntax vexExpr,+ endSemi : vexStmt

/-- A VEX null statement. -/
syntax ";" : vexStmt

/-!
### Iteration Statements
-/

/-- A VEX while loop. -/
syntax "while " "(" vexExpr,+ ")" vexStmt : vexStmt

/-- A VEX do-while loop. -/
syntax "do " vexStmt " while " "(" vexExpr,+ ")" endSemi : vexStmt

/-- A VEX for loop. -/
syntax "for " "(" ((atomic(declaration) checkSemi) <|> (vexExpr,* ";")) vexExpr,* ";" vexExpr,* ")" vexStmt : vexStmt

/-- A VEX foreach loop (iterates over array elements). -/
syntax "foreach" "(" vexTypeSpec ident ";" vexExpr ")" vexStmt : vexStmt

/-!
### Selection Statements
-/

/-- A VEX if statement. -/
syntax "if " "(" vexExpr,+ ")" vexStmt (" else " vexStmt)? : vexStmt

/-!
### VEX-Specific Statements
-/

/-- A VEX illuminance loop (shading contexts only). -/
syntax "illuminance" "(" vexExpr,* ")" vexStmt : vexStmt

/-- A VEX gather loop (shading contexts only). -/
syntax "gather" "(" vexExpr,* ")" vexStmt : vexStmt

--------------------------------------------------------------------------------
/-! ## Top-Level Declarations                                                 -/
--------------------------------------------------------------------------------

/-!
### External Declarations
-/

/-- An external declaration in VEX. -/
declare_syntax_cat vexExternDecl

/-- A VEX function definition. -/
declare_syntax_cat vexFunction
syntax (name := vexFunctionSyntax) (("function")?) many1Lookahead(vexDeclSpec, declarator) declarator "{" vexStmtLike* "}" : vexFunction

/-- A VEX function declaration (prototype). -/
declare_syntax_cat vexFunctionDecl
syntax (name := vexFunctionDeclSyntax) (("function")?) many1Lookahead(vexDeclSpec, declarator) declarator endSemi : vexFunctionDecl

syntax vexFunction : vexExternDecl
syntax vexFunctionDecl : vexExternDecl
syntax declaration : vexExternDecl

/-- A VEX struct definition at top level. -/
syntax "struct" ident vexStructDef endSemi : vexExternDecl

syntax vexExternDecl : vexCmd

/-!
### Context Functions

VEX programs have a main function whose return type is the context name.
-/

/-- A VEX context function (main function). -/
syntax ident ident "(" (vexParamList)? ")" "{" vexStmtLike* "}" : vexCmd

--------------------------------------------------------------------------------
/-! ## Program Structure                                                      -/
--------------------------------------------------------------------------------

/-- A complete VEX program (file). -/
declare_syntax_cat vexProgram
syntax vexCmd* : vexProgram

/-- A VEX snippet (for wrangle nodes). -/
declare_syntax_cat vexSnippetItem
syntax vexStmt : vexSnippetItem
syntax vexFunction : vexSnippetItem
syntax vexFunctionDecl : vexSnippetItem
syntax "struct" ident vexStructDef endSemi : vexSnippetItem

declare_syntax_cat vexSnippet
syntax vexSnippetItem* : vexSnippet

/-- Elaborator for VEX snippets (converts to string). -/
elab "vexsnippet%" code:vexSnippet : term =>
  let s := code.raw.prettyPrint
  let e := Lean.mkStrLit (toString s)
  pure e

