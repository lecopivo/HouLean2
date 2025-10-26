/-
VEX Grammar Parser for Lean 4
Based on https://www.sidefx.com/docs/houdini/vex/lang.html
Using precedence-based expression parsing similar to Alloy C grammar
-/

import Lean.Parser

namespace VEX

open Lean Parser

-- VEX types
declare_syntax_cat vexType
syntax "int" : vexType
syntax "float" : vexType
syntax "vector" : vexType
syntax "vector2" : vexType
syntax "vector4" : vexType
syntax "matrix" : vexType
syntax "matrix2" : vexType
syntax "matrix3" : vexType
syntax "string" : vexType
syntax "void" : vexType
syntax "dict" : vexType
syntax "bsdf" : vexType
syntax ident : vexType  -- User-defined types

-- Built-in types only (for type constructors)
declare_syntax_cat vexBuiltinType
syntax "int" : vexBuiltinType
syntax "float" : vexBuiltinType
syntax "vector" : vexBuiltinType
syntax "vector2" : vexBuiltinType
syntax "vector4" : vexBuiltinType
syntax "matrix" : vexBuiltinType
syntax "matrix2" : vexBuiltinType
syntax "matrix3" : vexBuiltinType
syntax "string" : vexBuiltinType
syntax "dict" : vexBuiltinType
syntax "bsdf" : vexBuiltinType

-- Type qualifiers
declare_syntax_cat vexTypeQual
syntax "export" : vexTypeQual
syntax "const" : vexTypeQual
syntax "varying" : vexTypeQual
syntax "uniform" : vexTypeQual

-- Full type with qualifiers
declare_syntax_cat vexFullType
syntax vexTypeQual* vexType : vexFullType
syntax vexTypeQual* vexType "[" "]" : vexFullType  -- Array type

-- Expression category with precedence support
declare_syntax_cat vexExpr (behavior := both)

--------------------------------------------------------------------------------
-- Special token categories (to protect special characters from Lean's parser)
--------------------------------------------------------------------------------

-- Channel/Attribute access
declare_syntax_cat vexChannelAccess
syntax "@" ident : vexChannelAccess
syntax "s@" ident : vexChannelAccess
syntax "v@" ident : vexChannelAccess
syntax "p@" ident : vexChannelAccess
syntax "i@" ident : vexChannelAccess
syntax "f@" ident : vexChannelAccess
syntax "u@" ident : vexChannelAccess
syntax "d@" ident : vexChannelAccess
syntax "m2@" ident : vexChannelAccess
syntax "m3@" ident : vexChannelAccess
syntax "m4@" ident : vexChannelAccess

-- Array attribute variants
syntax "[]@" ident : vexChannelAccess
syntax "s[]@" ident : vexChannelAccess
syntax "v[]@" ident : vexChannelAccess
syntax "p[]@" ident : vexChannelAccess
syntax "i[]@" ident : vexChannelAccess
syntax "f[]@" ident : vexChannelAccess
syntax "u[]@" ident : vexChannelAccess
syntax "d[]@" ident : vexChannelAccess
syntax "m2[]@" ident : vexChannelAccess
syntax "m3[]@" ident : vexChannelAccess
syntax "m4[]@" ident : vexChannelAccess

-- Global variables
declare_syntax_cat vexGlobalVar
syntax "$$" ident : vexGlobalVar

--------------------------------------------------------------------------------
-- Primary Expressions (VEX precedence 15, Lean precedence max/1000)
--------------------------------------------------------------------------------

-- Identifiers
syntax:max ident : vexExpr

-- Numeric literals
syntax:max num : vexExpr
syntax:max scientific : vexExpr

-- String literals  
syntax:max str : vexExpr

-- Array/vector literals
syntax:max "{" vexExpr,* "}" : vexExpr

-- Parenthesized expression
syntax:max "(" vexExpr ")" : vexExpr

-- Channel/Attribute access (via protected category)
syntax:max vexChannelAccess : vexExpr

-- Global variables (via protected category)
syntax:max vexGlobalVar : vexExpr

--------------------------------------------------------------------------------
-- Postfix Expressions (VEX precedence 15, Lean precedence 1000)
--------------------------------------------------------------------------------

-- Function call
syntax:1000 vexExpr:1000 "(" vexExpr,* ")" : vexExpr

-- Array subscript
syntax:1000 vexExpr:1000 "[" vexExpr "]" : vexExpr

-- Member access
syntax:1000 vexExpr:1000 "." ident : vexExpr

-- Method call (arrow operator)v
syntax:1000 vexExpr:1000 "->" ident "(" vexExpr,* ")" : vexExpr

-- Post-increment
syntax:1000 vexExpr:1000 "++" : vexExpr

-- Post-decrement  
syntax:1000 vexExpr:1000 (atomic("--")) : vexExpr

--------------------------------------------------------------------------------
-- Unary Expressions (VEX precedence 13, Lean precedence 500)
--------------------------------------------------------------------------------

-- Pre-increment
syntax:500 "++" vexExpr:500 : vexExpr

-- Pre-decrement
syntax:500 (atomic("--")) vexExpr:500 : vexExpr

-- Unary plus
syntax:500 "+" vexExpr:500 : vexExpr

-- Unary minus
syntax:500 "-" vexExpr:500 : vexExpr

-- Logical NOT
syntax:500 "!" vexExpr:500 : vexExpr

-- Bitwise NOT
syntax:500 "~" vexExpr:500 : vexExpr

-- Type cast (use atomic to commit once type is parsed)
syntax:500 atomic("(" vexFullType ")") vexExpr:500 : vexExpr

-- Type constructor (vector(x), int(x), etc.) - only for built-in types
syntax:500 vexBuiltinType "(" vexExpr ")" : vexExpr

--------------------------------------------------------------------------------
-- Multiplicative Expressions (VEX precedence 12, Lean precedence 120)
--------------------------------------------------------------------------------

syntax:120 vexExpr:120 " * " vexExpr:121 : vexExpr
syntax:120 vexExpr:120 " / " vexExpr:121 : vexExpr
syntax:120 vexExpr:120 " % " vexExpr:121 : vexExpr

--------------------------------------------------------------------------------
-- Additive Expressions (VEX precedence 11, Lean precedence 110)
--------------------------------------------------------------------------------

syntax:110 vexExpr:110 " + " vexExpr:111 : vexExpr
syntax:110 vexExpr:110 " - " vexExpr:111 : vexExpr

--------------------------------------------------------------------------------
-- Shift Expressions (VEX precedence 10, Lean precedence 105)
-- Note: VEX doesn't have shift operators in the precedence table,
-- but they're in the grammar, so placing them between relational and additive
--------------------------------------------------------------------------------

syntax:105 vexExpr:105 " << " vexExpr:106 : vexExpr
syntax:105 vexExpr:105 " >> " vexExpr:106 : vexExpr

--------------------------------------------------------------------------------
-- Relational Expressions (VEX precedence 10, Lean precedence 100)
--------------------------------------------------------------------------------

syntax:100 vexExpr:100 " < " vexExpr:101 : vexExpr
syntax:100 vexExpr:100 " > " vexExpr:101 : vexExpr
syntax:100 vexExpr:100 " <= " vexExpr:101 : vexExpr
syntax:100 vexExpr:100 " >= " vexExpr:101 : vexExpr

--------------------------------------------------------------------------------
-- Equality Expressions (VEX precedence 9, Lean precedence 90)
--------------------------------------------------------------------------------

syntax:90 vexExpr:90 " == " vexExpr:91 : vexExpr
syntax:90 vexExpr:90 " != " vexExpr:91 : vexExpr
syntax:90 vexExpr:90 " ~= " vexExpr:91 : vexExpr  -- String match

--------------------------------------------------------------------------------
-- Bitwise AND (VEX precedence 8, Lean precedence 80)
--------------------------------------------------------------------------------

syntax:80 vexExpr:80 " & " vexExpr:81 : vexExpr

--------------------------------------------------------------------------------
-- Bitwise XOR (VEX precedence 7, Lean precedence 70)
--------------------------------------------------------------------------------

syntax:70 vexExpr:70 " ^ " vexExpr:71 : vexExpr

--------------------------------------------------------------------------------
-- Bitwise OR (VEX precedence 6, Lean precedence 60)
--------------------------------------------------------------------------------

syntax:60 vexExpr:60 " | " vexExpr:61 : vexExpr

--------------------------------------------------------------------------------
-- Logical AND (VEX precedence 5, Lean precedence 50)
--------------------------------------------------------------------------------

syntax:50 vexExpr:50 " && " vexExpr:51 : vexExpr

--------------------------------------------------------------------------------
-- Logical OR (VEX precedence 4, Lean precedence 40)
--------------------------------------------------------------------------------

syntax:40 vexExpr:40 " || " vexExpr:41 : vexExpr

--------------------------------------------------------------------------------
-- Conditional (Ternary) Expression (VEX precedence 3, Lean precedence 30)
--------------------------------------------------------------------------------

syntax:30 vexExpr:31 " ? " vexExpr:10 " : " vexExpr:30 : vexExpr

--------------------------------------------------------------------------------
-- Assignment Expressions (VEX precedence 2, Lean precedence 20)
-- Right-to-left associativity
--------------------------------------------------------------------------------

syntax:20 vexExpr:500 " = " vexExpr:20 : vexExpr
syntax:20 vexExpr:500 " += " vexExpr:20 : vexExpr
syntax:20 vexExpr:500 " -= " vexExpr:20 : vexExpr
syntax:20 vexExpr:500 " *= " vexExpr:20 : vexExpr
syntax:20 vexExpr:500 " /= " vexExpr:20 : vexExpr
syntax:20 vexExpr:500 " %= " vexExpr:20 : vexExpr
syntax:20 vexExpr:500 " <<= " vexExpr:20 : vexExpr
syntax:20 vexExpr:500 " >>= " vexExpr:20 : vexExpr
syntax:20 vexExpr:500 " &= " vexExpr:20 : vexExpr
syntax:20 vexExpr:500 " ^= " vexExpr:20 : vexExpr
syntax:20 vexExpr:500 " |= " vexExpr:20 : vexExpr

--------------------------------------------------------------------------------
-- Comma Expression (VEX precedence 1, Lean precedence 10)
--------------------------------------------------------------------------------

syntax:10 vexExpr:10 " , " vexExpr:20 : vexExpr

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

declare_syntax_cat vexStmt

-- Compound statement
syntax "{" vexStmt* "}" : vexStmt

-- Expression statement
syntax vexExpr ";" : vexStmt

-- Empty statement
syntax ";" : vexStmt

-- Variable declaration
syntax vexFullType ident ("=" vexExpr)? ";" : vexStmt
syntax vexFullType ident "[" (vexExpr)? "]" ("=" vexExpr)? ";" : vexStmt
syntax vexFullType ident "[" (vexExpr)? "]" ("=" "{" vexExpr,* "}")? ";" : vexStmt

-- Control flow
syntax "if" "(" vexExpr ")" vexStmt ("else" vexStmt)? : vexStmt
syntax "while" "(" vexExpr ")" vexStmt : vexStmt
syntax "do" vexStmt "while" "(" vexExpr ")" ";" : vexStmt

-- For loops
syntax "for" "(" (vexExpr)? ";" (vexExpr)? ";" (vexExpr)? ")" vexStmt : vexStmt
syntax "for" "(" (vexStmt)? (vexExpr)? ";" (vexExpr)? ")" vexStmt : vexStmt

-- Foreach loop
syntax "foreach" "(" vexFullType ident ";" vexExpr ")" vexStmt : vexStmt

-- Jump statements
syntax "return" (vexExpr)? ";" : vexStmt
syntax "break" ";" : vexStmt
syntax "continue" ";" : vexStmt

--------------------------------------------------------------------------------
-- Function declarations
--------------------------------------------------------------------------------

-- Function parameters (semicolon-separated)
declare_syntax_cat vexParam
syntax vexFullType ident : vexParam
syntax vexFullType ident "[" "]" : vexParam

declare_syntax_cat vexParamList
syntax vexParam : vexParamList
syntax vexParam ";" vexParamList : vexParamList

-- Function declaration
declare_syntax_cat vexFuncDecl
syntax vexFullType ident "(" (vexParamList)? ")" ";" : vexFuncDecl
syntax vexFullType ident "(" (vexParamList)? ")" "{" vexStmt* "}" : vexFuncDecl
syntax "function" vexFullType ident "(" (vexParamList)? ")" "{" vexStmt* "}" : vexFuncDecl

--------------------------------------------------------------------------------
-- Struct definitions
--------------------------------------------------------------------------------

declare_syntax_cat vexStructMember
syntax vexFullType ident ";" : vexStructMember
syntax vexFullType ident "[" num "]" ";" : vexStructMember
syntax vexFullType ident " = " vexExpr ";" : vexStructMember  -- Default values (C++11 style)

declare_syntax_cat vexStructDef
syntax "struct" ident "{" vexStructMember* "}" ";" : vexStructDef

--------------------------------------------------------------------------------
-- Top-level declarations
--------------------------------------------------------------------------------

declare_syntax_cat vexTopDecl
syntax vexFuncDecl : vexTopDecl
syntax vexStructDef : vexTopDecl
syntax vexFullType ident ("=" vexExpr)? ";" : vexTopDecl

-- Snippet items (for wrangle nodes)
declare_syntax_cat vexSnippetItem
syntax vexStmt : vexSnippetItem
syntax vexFuncDecl : vexSnippetItem
syntax vexStructDef : vexSnippetItem

-- Program (full VEX file)
syntax vexProgram := vexTopDecl*

-- Snippet (wrangle node code)
syntax vexSnippet := vexSnippetItem*

end VEX
