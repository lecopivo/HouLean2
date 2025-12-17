/-
Copyright (c) 2022 Mac Malone. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.

This file is based on Alloy/C/Grammar.lean by Mac Malone
https://github.com/tydeu/lean4-alloy/blob/be37dab91e3b0d62c64d180fa29ed3d3c4d8bf8b/Alloy/C/Grammar.lean
-/

import HouLean.Meta.Parser

open Alloy

/-!
# The OpenCL Grammar

This module contains a Lean DSL that encodes OpenCL C syntax.

Based on the OpenCL C Specification:
- [OpenCL 1.2 Specification](https://www.khronos.org/registry/OpenCL/specs/opencl-1.2.pdf)
- [OpenCL 2.0 C Language Specification](https://www.khronos.org/registry/OpenCL/specs/2.2/html/OpenCL_C.html)
- [OpenCL 3.0 Specification](https://www.khronos.org/registry/OpenCL/specs/3.0-unified/html/OpenCL_C.html)
-/

open Lean Parser

register_option HouLean.OpenCL.optSemicolon : Bool := {
  defValue := true
  descr := "Should semicolons be optional in HouLean OpenCL code?"
}

namespace HouLean.OpenCL.Compiler

open OpenCL

--------------------------------------------------------------------------------
/-! ## Basic Abstractions                                                     -/
--------------------------------------------------------------------------------

/-- A `specifier-qualifier` of the OpenCL grammar. -/
declare_syntax_cat clSpec (behavior := symbol)

/-- A `type-specifier` of the OpenCL grammar. -/
declare_syntax_cat clTypeSpec (behavior := symbol)

syntax clTypeSpec : clSpec

/-- A `type-qualifier` of the OpenCL grammar. -/
declare_syntax_cat clTypeQ (behavior := symbol)

syntax clTypeQ : clSpec

/-- A `pointer` of the OpenCL grammar. -/
syntax clPointer := (" * " clTypeQ*)+

/-- A `direct-declarator` of the OpenCL grammar. -/
declare_syntax_cat clDirectDeclarator (behavior := symbol)

/-- A `direct-abstract-declarator` of the OpenCL grammar. -/
declare_syntax_cat clDirectAbsDeclarator (behavior := symbol)

/-- A `declarator` of the OpenCL grammar. -/
syntax clDeclarator := (clPointer)? clDirectDeclarator

/-- An `abstract-declarator` of the OpenCL grammar. -/
syntax clAbsDeclarator := (clPointer (clDirectAbsDeclarator)?) <|> clDirectAbsDeclarator

/-- A `type` of the OpenCL grammar. -/
syntax clType := clSpec+ optional(clAbsDeclarator)

/-- An `assignment-expression` of the OpenCL grammar. -/
declare_syntax_cat clExpr (behavior := symbol)

/-- A `constant-expression` of the OpenCL grammar. -/
syntax clConstExpr := clExpr:20

/-- A `designator` of the OpenCL grammar. -/
declare_syntax_cat clDesignator

/-- An `initializer` of the OpenCL grammar. -/
declare_syntax_cat clInitializer

/-- Designates the index of an array to initialize. -/
syntax "[" clConstExpr "]" : clDesignator

/-- Designates the field of an aggregate to initialize. -/
syntax "." ident : clDesignator

/-- An initializer that uses an expression. -/
syntax clExpr : clInitializer

/-- An element of an initializer list. -/
syntax clInitializerElem := optional(clDesignator+ "=") clInitializer

/-- An aggregate initializer that uses an initializer list. -/
syntax "{" clInitializerElem,*,? "}" : clInitializer

/-- A `statement` of the OpenCL grammar. -/
declare_syntax_cat clStmt (behavior := symbol)

/-- A top-level OpenCL command (preprocessor directive or external declaration). -/
declare_syntax_cat clCmd (behavior := symbol)

--------------------------------------------------------------------------------
/-! ## Comments                                                               -/
--------------------------------------------------------------------------------

variable (pushMissingOnError : Bool) in
/-- Adaption of `Lean.Parser.finishCommentBlock`. -/
partial def finishCommentBlock (nesting : Nat) : ParserFn := fun c s =>
  let i     := s.pos
  if h : c.atEnd i then eoi s
  else
    let curr := c.get' i h
    let i    := c.next' i h
    if curr == '*' then
      if h : c.atEnd i then eoi s
      else
        let curr := c.get' i h
        if curr == '/' then
          if nesting == 1 then s.next' c i h
          else finishCommentBlock (nesting-1) c (s.next' c i h)
        else
          finishCommentBlock nesting c (s.setPos i)
    else if curr == '/' then
      if h : c.atEnd i then eoi s
      else
        let curr := c.get' i h
        if curr == '-' then finishCommentBlock (nesting+1) c (s.next' c i h)
        else finishCommentBlock nesting c (s.setPos i)
    else finishCommentBlock nesting c (s.setPos i)
where
  eoi s := s.mkUnexpectedError (pushMissing := pushMissingOnError) "unterminated comment"

def clBlockCommentBody :=
  raw (finishCommentBlock (pushMissingOnError := true) 1) (trailingWs := true)

/-- An OpenCL line comment. -/
syntax clLineComment := "//" Alloy.line

/-- An OpenCL block comment. -/
syntax clBlockComment := "/*" clBlockCommentBody

attribute [clCmd_parser, clStmt_parser, clExpr_parser]
  clLineComment clBlockComment

syntax atomic(clLineComment) clStmt : clStmt
syntax atomic(clBlockComment) clStmt : clStmt
syntax clStmt clLineComment : clStmt
syntax clStmt clBlockComment : clStmt

syntax atomic(clLineComment) clExpr : clExpr
syntax atomic(clBlockComment) clExpr : clExpr
syntax clExpr clLineComment : clExpr
syntax clExpr clBlockComment : clExpr

--------------------------------------------------------------------------------
/-! ## Expressions                                                            -/
--------------------------------------------------------------------------------

/-! ### Primary Expressions -/

/-- An OpenCL identifier. -/
syntax:max ident : clExpr

/-- An integer suffix (l, ll, u, ul, ull, etc.). -/
def clIntSuffix :=
  identSatisfy ["integer suffix"] fun
  | .str .anonymous s =>
    let s := s.toLower
    let s := if s.front = 'u' then s.drop 1
             else if s.back = 'u' then s.dropRight 1
             else s
    s = "l" || s = "ll"
  | _ => false

/-- An OpenCL integer constant. -/
syntax:max (name := clIntConst) num (noWs clIntSuffix)? : clExpr

/-- An OpenCL floating constant. -/
syntax:max (name := clFloatConst) scientific (noWs (&"f" <|> &"F" <|> &"l" <|> &"L" <|> &"h" <|> &"H"))? : clExpr

/-- An OpenCL character constant. -/
syntax:max (name := clCharConst) char : clExpr

/-- An OpenCL string literal. -/
syntax:max str : clExpr

/-- An OpenCL compound literal. -/
syntax:max "(" ident ")" "{" clInitializerElem,*,? "}" : clExpr

/-- A parenthetical expression. -/
syntax:max "(" clExpr ")" : clExpr

/-! ### Postfix Expressions -/

/-- A subscript expression. -/
syntax:1000 clExpr:1000 "[" clExpr "]" : clExpr

/-- A function call expression. -/
syntax:1000 clExpr:1000 "(" clExpr,* ")" : clExpr

/-- A member access expression. -/
syntax:1000 clExpr:1000 noWs "." noWs ident : clExpr

/-- A member access through pointer expression. -/
syntax:1000 clExpr:1000 "->" ident : clExpr

/-- A postfix increment expression. -/
syntax:1000 clExpr:1000 "++" : clExpr

-- /-- A postfix decrement expression. -/
-- syntax:1000 clExpr:1000 "--" : clExpr

/-! ### Vector Component Access -/

/-- Vector component accessor validation (.xyzw, .rgba style). -/
def clVectorComponent :=
  identSatisfy ["vector component"] fun
  | .str .anonymous s =>
    let xyzw := "xyzw"
    let rgba := "rgba"
    let validChars c := xyzw.any (· == c) || rgba.any (· == c)
    s.length > 0 && s.length <= 16 && s.all validChars
  | _ => false

/-- Vector component accessor validation (.sN style). -/
def clVectorSComponent :=
  identSatisfy ["vector s-component"] fun
  | .str .anonymous s =>
    let hexChars := "0123456789abcdefABCDEF"
    s.startsWith "s" && s.length > 1 && s.length <= 17 &&
    (s.drop 1).all (fun c => hexChars.any (· == c))
  | _ => false

/-- Vector swizzle access .xyzw/.rgba style. -/
syntax:1000 clExpr:1000 "." noWs clVectorComponent : clExpr

/-- Vector swizzle access .sN style. -/
syntax:1000 clExpr:1000 "." noWs clVectorSComponent : clExpr

/-- Vector .hi component (upper half). -/
syntax:1000 clExpr:1000 ".hi" : clExpr

/-- Vector .lo component (lower half). -/
syntax:1000 clExpr:1000 ".lo" : clExpr

/-- Vector .even components. -/
syntax:1000 clExpr:1000 ".even" : clExpr

/-- Vector .odd components. -/
syntax:1000 clExpr:1000 ".odd" : clExpr

/-! ### Unary Expressions -/

/-- A prefix increment expression. -/
syntax:500 "++" clExpr:500 : clExpr

-- /-- A prefix decrement expression. -/
-- syntax:500 "--" clExpr:500 : clExpr

/-- An address-of expression. -/
syntax:500 "&" clExpr:100 : clExpr

/-- A pointer dereference expression. -/
syntax:500 "*" clExpr:100 : clExpr

/-- A unary plus expression. -/
syntax:500 "+" clExpr:100 : clExpr

/-- A unary minus expression. -/
syntax:500 "-" clExpr:100 : clExpr

-- /-- A bitwise NOT expression. -/
-- syntax:500 "~" clExpr:100 : clExpr

/-- A logical NOT expression. -/
syntax:500 "!" clExpr:100 : clExpr

/-- A sizeof expression. -/
syntax:500 "sizeof" (atomic("(" clType ")") <|> clExpr:500) : clExpr

/-- An _Alignof expression. -/
syntax:500 "_Alignof" "(" clType ")" : clExpr

/-! ### Cast Expression -/

/-- A cast expression. -/
syntax clCastExpr := "(" clType ")" clExpr:100
attribute [clExpr_parser 100] clCastExpr

/-! ### Multiplicative Expressions -/

/-- A multiplication expression. -/
syntax:70 (name := clMulExpr) clExpr:70 " * " clExpr:71 : clExpr

/-- A division expression. -/
syntax:70 (name := clDivExpr) clExpr:70 " / " clExpr:71 : clExpr

/-- A remainder expression. -/
syntax:70 (name := clRemExpr) clExpr:70 " % " clExpr:71 : clExpr

/-! ### Additive Expressions -/

/-- An addition expression. -/
syntax:65 (name := clAddExpr) clExpr:65 " + " clExpr:66 : clExpr

/-- A subtraction expression. -/
syntax:65 (name := clSubExpr) clExpr:65 " - " clExpr:66 : clExpr

/-! ### Shift Expressions -/

/-- A left shift expression. -/
syntax:60 (name := clShlExpr) clExpr:60 " << " clExpr:61 : clExpr

/-- A right shift expression. -/
syntax:60 (name := clShrExpr) clExpr:60 " >> " clExpr:61 : clExpr

/-! ### Relational Expressions -/

/-- A less-than expression. -/
syntax:55 (name := clLtExpr) clExpr:55 " < " clExpr:56 : clExpr

/-- A greater-than expression. -/
syntax:55 (name := clGtExpr) clExpr:55 " > " clExpr:56 : clExpr

/-- A less-or-equal expression. -/
syntax:55 (name := clLeExpr) clExpr:55 " <= " clExpr:56 : clExpr

/-- A greater-or-equal expression. -/
syntax:55 (name := clGeExpr) clExpr:55 " >= " clExpr:56 : clExpr

/-! ### Equality Expressions -/

/-- An equal-to expression. -/
syntax:50 (name := clEqExpr) clExpr:50 " == " clExpr:51 : clExpr

/-- A not-equal-to expression. -/
syntax:50 (name := clNeExpr) clExpr:50 " != " clExpr:51 : clExpr

/-! ### Bitwise Expressions -/

/-- A bitwise AND expression. -/
syntax:45 (name := clAndExpr) clExpr:45 " & " clExpr:46 : clExpr

/-- A bitwise XOR expression. -/
syntax:43 (name := clXorExpr) clExpr:43 " ^ " clExpr:44 : clExpr

/-- A bitwise OR expression. -/
syntax:40 (name := clOrExpr) clExpr:40 " | " clExpr:41 : clExpr

/-! ### Logical Expressions -/

/-- A logical AND expression. -/
syntax:35 (name := clLogicalAndExpr) clExpr:35 " && " clExpr:36 : clExpr

/-- A logical OR expression. -/
syntax:30 (name := clLogicalOrExpr) clExpr:30 " || " clExpr:31 : clExpr

/-! ### Conditional Expression -/

/-- A conditional (ternary) expression. -/
syntax:20 (name := clCondExpr) clExpr:21 " ? " clExpr,+ " : " clExpr:20 : clExpr

/-! ### Assignment Expression -/

/-- An assignment operator. -/
declare_syntax_cat clAssignOp

syntax " = " : clAssignOp
syntax " *= " : clAssignOp
syntax " /= " : clAssignOp
syntax " %= " : clAssignOp
syntax " += " : clAssignOp
syntax " -= " : clAssignOp
syntax " <<= " : clAssignOp
syntax " >>= " : clAssignOp
syntax " &= " : clAssignOp
syntax " ^= " : clAssignOp
syntax " |= " : clAssignOp

/-- An assignment expression. -/
syntax:15 (name := clAssignExpr) clExpr:500 clAssignOp clExpr:15 : clExpr

--------------------------------------------------------------------------------
/-! ## Declaration Syntax                                                     -/
--------------------------------------------------------------------------------

/-! ### Specifiers -/

/-- A declaration specifier. -/
declare_syntax_cat clDeclSpec

syntax clSpec : clDeclSpec

/-- A GNU-style attribute specifier. -/
syntax "__attribute__" noWs "(" "(" (rawIdent ("(" clExpr,* ")")?),* ")" ")" : clDeclSpec

/-! #### Storage Class Specifiers -/

/-- A storage class specifier. -/
declare_syntax_cat clStorageClassSpec (behavior := symbol)

syntax "auto" : clStorageClassSpec
syntax "extern" : clStorageClassSpec
syntax "register" : clStorageClassSpec
syntax "static" : clStorageClassSpec
syntax "_Thread_local" : clStorageClassSpec
syntax "typedef" : clStorageClassSpec

syntax clStorageClassSpec : clDeclSpec

/-! #### Function Specifiers -/

/-- A function specifier. -/
declare_syntax_cat clFunSpec (behavior := symbol)

syntax "inline" : clFunSpec
syntax "_Noreturn" : clFunSpec

/-- The __kernel function qualifier for OpenCL kernel entry points. -/
syntax "__kernel" : clFunSpec
syntax "kernel" : clFunSpec

syntax clFunSpec : clDeclSpec

/-! ### Declarators -/

declare_syntax_cat clIndex

syntax clExpr : clIndex
syntax "static" clTypeQ* clExpr : clIndex
syntax clTypeQ+ "static"? clExpr : clIndex
syntax clTypeQ* "*" : clIndex

/-- A parameter declaration. -/
syntax clParamDecl := clDeclSpec+ (atomic(clDeclarator) <|> clAbsDeclarator)?

/-- A parameter type list. -/
syntax clParams := clParamDecl,+,? "..."?

/-- The name of a declaration. -/
syntax:max ident : clDirectDeclarator

syntax:max "(" clDeclarator ")" : clDirectDeclarator
syntax:arg clDirectDeclarator:arg "[" optional(clIndex) "]" : clDirectDeclarator
syntax:arg clDirectDeclarator:arg "(" clParams ")" : clDirectDeclarator
syntax:arg clDirectDeclarator:arg "(" ident* ")" : clDirectDeclarator

syntax:max "(" clAbsDeclarator ")" : clDirectAbsDeclarator
syntax:max "(" clParams ")" : clDirectAbsDeclarator
syntax:arg clDirectAbsDeclarator:arg "[" optional(clIndex) "]" : clDirectAbsDeclarator
syntax:arg clDirectAbsDeclarator:arg "(" optional(clParams) ")" : clDirectAbsDeclarator

/-! ### Declarations -/

/-- The semicolon terminator of a statement/declaration. -/
def clEndSemi : Parser := leading_parser
  withFn (p := optional (symbol ";")) fun p c s =>
    if optSemicolon.get c.options then p c s else symbolFn ";" c s

/-- Ensure the previous syntax ended with a semicolon token. -/
def clCheckSemi : Parser :=
  checkStackTop (tailSyntax · |>.isToken ";") "expected ';'"

/-- An init-declarator. -/
syntax clInitDeclarator := clDeclarator optional(" = " clInitializer)


/-- A declaration with declarators. -/
syntax clDeclWithDeclarators :=
  many1OptLookahead(clDeclSpec, clDeclarator) clInitDeclarator,+ clEndSemi

/-- A declaration without declarators (struct/enum/union definition only). -/
syntax clDeclWithoutDeclarators := clDeclSpec+ clEndSemi

/-- A declaration. -/
syntax clDeclaration := clDeclWithDeclarators <|> clDeclWithoutDeclarators


--------------------------------------------------------------------------------
/-! ## Types                                                                  -/
--------------------------------------------------------------------------------

/-! ### Standard Type Qualifiers -/

syntax "const" : clTypeQ
syntax "restrict" : clTypeQ
syntax "volatile" : clTypeQ
syntax "_Atomic" : clTypeQ

/-! ### Address Space Qualifiers -/

/-- Global memory, accessible by all work-items. -/
syntax "__global" : clTypeQ
syntax "global" : clTypeQ

/-- Local memory, shared within a work-group. -/
syntax "__local" : clTypeQ
syntax "local" : clTypeQ

/-- Constant memory, read-only global memory. -/
syntax "__constant" : clTypeQ
syntax "constant" : clTypeQ

/-- Private memory, per work-item (default). -/
syntax "__private" : clTypeQ
syntax "private" : clTypeQ

/-- Generic address space (OpenCL 2.0+). -/
syntax "__generic" : clTypeQ
syntax "generic" : clTypeQ

/-! ### Access Qualifiers -/

/-- Read-only access for images/pipes. -/
syntax "__read_only" : clTypeQ
syntax "read_only" : clTypeQ

/-- Write-only access for images/pipes. -/
syntax "__write_only" : clTypeQ
syntax "write_only" : clTypeQ

/-- Read-write access for images (OpenCL 2.0+). -/
syntax "__read_write" : clTypeQ
syntax "read_write" : clTypeQ

/-! ### Primitive Type Specifiers -/

syntax "void" : clTypeSpec
syntax "char" : clTypeSpec
syntax "short" : clTypeSpec
syntax "int" : clTypeSpec
syntax "long" : clTypeSpec
syntax "float" : clTypeSpec
syntax "double" : clTypeSpec
syntax "signed" : clTypeSpec
syntax "unsigned" : clTypeSpec
syntax "_Bool" : clTypeSpec
syntax "_Complex" : clTypeSpec
syntax "_Imaginary" : clTypeSpec

/-- A user-defined type. -/
syntax ident : clTypeSpec

/-! ### OpenCL Scalar Type Extensions -/

/-- Half-precision floating-point type. -/
syntax "half" : clTypeSpec

/-- Unsigned char shorthand. -/
syntax "uchar" : clTypeSpec

/-- Unsigned short shorthand. -/
syntax "ushort" : clTypeSpec

/-- Unsigned int shorthand. -/
syntax "uint" : clTypeSpec

/-- Unsigned long shorthand. -/
syntax "ulong" : clTypeSpec

/-- Size type. -/
syntax "size_t" : clTypeSpec

/-- Pointer difference type. -/
syntax "ptrdiff_t" : clTypeSpec

/-- Signed integer pointer type. -/
syntax "intptr_t" : clTypeSpec

/-- Unsigned integer pointer type. -/
syntax "uintptr_t" : clTypeSpec

/-! ### Vector Types -/

/-- Vector type suffix validation (2, 3, 4, 8, or 16). -/
def clVectorSuffix :=
  identSatisfy ["vector size"] fun
  | .str .anonymous s => s ∈ ["2", "3", "4", "8", "16"]
  | _ => false

/-- OpenCL built-in vector types (char2, float4, int16, etc.). -/
syntax (name := clVectorType)
  (&"char" <|> &"uchar" <|> &"short" <|> &"ushort" <|>
   &"int" <|> &"uint" <|> &"long" <|> &"ulong" <|>
   &"float" <|> &"double" <|> &"half") noWs clVectorSuffix : clTypeSpec


/-! ### Image Types -/

syntax "image1d_t" : clTypeSpec
syntax "image1d_buffer_t" : clTypeSpec
syntax "image1d_array_t" : clTypeSpec
syntax "image2d_t" : clTypeSpec
syntax "image2d_array_t" : clTypeSpec
syntax "image2d_depth_t" : clTypeSpec
syntax "image2d_array_depth_t" : clTypeSpec
syntax "image3d_t" : clTypeSpec
syntax "sampler_t" : clTypeSpec
syntax "event_t" : clTypeSpec
syntax "cl_mem_fence_flags" : clTypeSpec

/-! ### OpenCL 2.0+ Types -/

syntax "atomic_int" : clTypeSpec
syntax "atomic_uint" : clTypeSpec
syntax "atomic_long" : clTypeSpec
syntax "atomic_ulong" : clTypeSpec
syntax "atomic_float" : clTypeSpec
syntax "atomic_double" : clTypeSpec
syntax "atomic_intptr_t" : clTypeSpec
syntax "atomic_uintptr_t" : clTypeSpec
syntax "atomic_size_t" : clTypeSpec
syntax "atomic_ptrdiff_t" : clTypeSpec
syntax "atomic_flag" : clTypeSpec
syntax "queue_t" : clTypeSpec
syntax "ndrange_t" : clTypeSpec
syntax "clk_event_t" : clTypeSpec
syntax "reserve_id_t" : clTypeSpec

/-- Pipe type (OpenCL 2.0+). -/
syntax (name := clPipeType) "pipe" clTypeSpec : clTypeSpec

/-! ### Atomic Type Specifier -/

/-- An atomic type specifier. -/
syntax (name := clAtomicSpec) "_Atomic" "(" clType ")" : clTypeSpec

/-! ### Aggregates -/

/-- A bit field declaration. -/
syntax clAggrDeclBits := " : " clConstExpr

/-- A struct declarator. -/
syntax clAggrDeclarator := clAggrDeclBits <|> (clDeclarator optional(clAggrDeclBits))

/-- A struct declaration. -/
syntax clAggrDeclaration :=
  many1OptLookahead(clSpec, clAggrDeclarator) clAggrDeclarator,* clEndSemi

syntax clAggrDef := "{" (clLineComment <|> clBlockComment <|> clAggrDeclaration)* "}"
syntax clAggrSig := clAggrDef <|> (ident optional(clAggrDef))

/-- A struct declaration. -/
syntax (name := clStructSpec) "struct " clAggrSig : clTypeSpec

/-- A union declaration. -/
syntax (name := clUnionSpec) "union " clAggrSig : clTypeSpec

/-! ### Enums -/

/-- An enumerator. -/
syntax clEnumerator := ident optional(" = " clConstExpr)

syntax clEnumDef := "{" (clLineComment <|> clBlockComment <|> clEnumerator),+ "}"
syntax clEnumSig := clEnumDef <|> (ident optional(clEnumDef))

/-- An enum specifier. -/
syntax (name := clEnumSpec) "enum " clEnumSig : clTypeSpec

/-! ### Alignment -/

/-- An alignment specifier. -/
syntax (name := clAlignSpec) &"_Alignas" "(" (clType <|> clConstExpr) ")" : clSpec

--------------------------------------------------------------------------------
/-! ## Statements                                                             -/
--------------------------------------------------------------------------------

/-
These are the updated statement/declaration syntax rules with pretty printing hints.
Replace the corresponding sections in Grammar2.lean

Key insight: Don't put ppLine in individual statements - let the compound statement
handle all line breaks uniformly via a wrapper.
-/

--------------------------------------------------------------------------------
/-! ## Statements (with formatting)                                           -/
--------------------------------------------------------------------------------


/-! ### Jump Statements -/

/-- A goto statement. -/
syntax (name := clGotoStmt) &"goto " ident clEndSemi : clStmt

/-- A continue statement. -/
syntax (name := clContinueStmt) "continue" clEndSemi : clStmt

/-- A break statement. -/
syntax (name := clBreakStmt) "break" clEndSemi : clStmt

/-- A return statement. -/
syntax (name := clReturnStmt) "return" (ppSpace clExpr),* clEndSemi : clStmt

/-! ### Compound Statements -/

/-- Syntax usable in place of a statement in a compound statement. -/
declare_syntax_cat clStmtLike

-- ppLine at START of each statement-like, so we get newline before each
syntax ppLine clDeclaration : clStmtLike
syntax ppLine clStmt : clStmtLike

/-- A sequence of statements. -/
syntax clStmtSeq := many1Indent(clStmtLike)

/-- A compound statement (block). -/
syntax clCompStmt := "{" ppIndent(clStmtLike*) ppDedent(ppLine) "}"
attribute [clStmt_parser] clCompStmt

/-! ### Expression Statements -/

/-- An expression statement. -/
syntax (name := clExprStmt) clExpr,+ clEndSemi : clStmt

/-- A null statement. -/
syntax (name := clNullStmt) ";" : clStmt

/-! ### Iteration Statements -/

-- No ppSpace before clStmt - just regular space. Compound stmt handles its own formatting.

/-- A while loop. -/
syntax (name := clWhileStmt) "while " "(" clExpr,+ ") " clStmt : clStmt

/-- A do-while loop. -/
syntax (name := clDoWhileStmt) "do " clStmt "while " "(" clExpr,+ ")" : clStmt

/-- A for loop. -/
syntax clForStmt := "for " "(" ((atomic(clDeclaration) clCheckSemi) <|> (clExpr,* "; ")) clExpr,* "; " clExpr,* ") " clStmt
attribute [clStmt_parser] clForStmt

/-! ### Selection Statements -/

/-- An if statement. -/
syntax (name := clIfStmt) "if " "(" clExpr,+ ") " clStmt (" else " clStmt)? : clStmt

/-- A switch statement. -/
syntax (name := clSwitchStmt) &"switch " "(" clExpr,+ ") " clStmt : clStmt

/-! ### Labeled Statements -/

/-- A label for goto. -/
syntax (name := clLabelStmt) ident ": " clStmt : clStmt

/-- A case label in a switch statement. -/
syntax (name := clCaseStmt) &"case " clConstExpr ": " clStmt : clStmt

/-- A default label in a switch statement. -/
syntax (name := clDefaultStmt) &"default" ": " clStmt : clStmt

--------------------------------------------------------------------------------
/-! ## Top-Level Commands                                                     -/
--------------------------------------------------------------------------------

/-! ### External Declarations -/

/-- An external declaration. -/
declare_syntax_cat clExternDecl

/-- A function definition. -/
syntax clFunction :=
  many1Lookahead(clDeclSpec, clDeclarator) clDeclarator clDeclaration* clCompStmt

syntax clFunction : clExternDecl
syntax clDeclaration : clExternDecl

syntax clExternDecl : clCmd

/-! ### Kernel Attributes -/

/-- Work-group size hint attribute. -/
syntax (name := clWorkGroupSizeHint)
  "__attribute__" noWs "(" "(" "work_group_size_hint" "(" num "," num "," num ")" ")" ")" : clDeclSpec

/-- Required work-group size attribute. -/
syntax (name := clReqdWorkGroupSize)
  "__attribute__" noWs "(" "(" "reqd_work_group_size" "(" num "," num "," num ")" ")" ")" : clDeclSpec

/-- Vector type hint attribute. -/
syntax (name := clVecTypeHint)
  "__attribute__" noWs "(" "(" "vec_type_hint" "(" clType ")" ")" ")" : clDeclSpec

/-- Intel required sub-group size attribute. -/
syntax (name := clIntelReqdSubGroupSize)
  "__attribute__" noWs "(" "(" "intel_reqd_sub_group_size" "(" num ")" ")" ")" : clDeclSpec


--------------------------------------------------------------------------------
/-! ## Preprocessor Directives                                                -/
--------------------------------------------------------------------------------

/-! ### Headers -/

/-- An angle-bracket header name. -/
@[run_parser_attribute_hooks] def clAngleHeaderName :=
   raw (takeUntilFn fun c => c == '>')

syntax clAngleHeader := "<" clAngleHeaderName ">"

/-- A header name. -/
syntax clHeader := str <|> clAngleHeader

/-! ### Preprocessor Commands -/

namespace PP

/-- A preprocessor directive. -/
declare_syntax_cat clPPCmd

syntax clPPCmd : clCmd
syntax clPPCmd : clStmt

/-- Null directive. -/
syntax (name := clNullCmd) "#" : clPPCmd

/-- Include directive. -/
syntax (name := clIncludeCmd) "#include " clHeader : clPPCmd

/-- Define directive. -/
syntax (name := clDefineCmd) "#define " rawIdent (noWs "(" rawIdent,*,? "..."? ")")? line : clPPCmd

/-- Undef directive. -/
syntax (name := clUndefCmd) "#undef " rawIdent : clPPCmd

/-- Line directive. -/
syntax (name := clLineCmd) "#line " line : clPPCmd

/-- Error directive. -/
syntax (name := clErrorCmd) "#error " line : clPPCmd

/-- Warning directive. -/
syntax (name := clWarningCmd) "#warning " line : clPPCmd

/-- Pragma directive. -/
syntax (name := clPragmaCmd) "#pragma " line : clPPCmd

/-! #### Conditional Compilation -/

/-- A defined expression for preprocessor conditionals. -/
syntax:max "defined" (("(" ident ")") <|> ident) : clExpr

/-- #if directive. -/
syntax (name := clIfCmd) "#if " clConstExpr : clPPCmd

/-- #ifdef directive. -/
syntax (name := clIfdefCmd) "#ifdef " rawIdent : clPPCmd

/-- #ifndef directive. -/
syntax (name := clIfndefCmd) "#ifndef " rawIdent : clPPCmd

/-- #elif directive. -/
syntax (name := clElifCmd) "#elif " clConstExpr : clPPCmd

/-- #else directive. -/
syntax (name := clElseCmd) "#else" : clPPCmd

/-- #endif directive. -/
syntax (name := clEndifCmd) "#endif" : clPPCmd

/-! #### OpenCL-Specific Pragmas -/

/-- OpenCL extension pragma. -/
syntax (name := clExtensionPragma)
  "#pragma " "OPENCL" "EXTENSION" rawIdent ":" (&"enable" <|> &"disable" <|> &"require") : clPPCmd

/-- OpenCL FP contract pragma. -/
syntax (name := clFpContractPragma)
  "#pragma " "OPENCL" "FP_CONTRACT" (&"ON" <|> &"OFF" <|> &"DEFAULT") : clPPCmd

end PP

--------------------------------------------------------------------------------
/-! ## Program Structure                                                      -/
--------------------------------------------------------------------------------

/-- An OpenCL program (sequence of top-level commands). -/
syntax clProgram := clCmd*
