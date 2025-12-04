import Lean

/-! In this file we define Python like syntax. This will allows us to write python like code
and translate it to Lean code.
-/

namespace HouLean.Python

-- Declare Python syntax categories
declare_syntax_cat pyExpr (behavior:=symbol)
declare_syntax_cat pyStmt (behavior:=symbol)
declare_syntax_cat pyBlock (behavior:=symbol)

-- Python expressions
syntax num : pyExpr
syntax str : pyExpr
syntax ident : pyExpr

-- Boolean and None literals
syntax "True" : pyExpr
syntax "False" : pyExpr
syntax "None" : pyExpr

-- Binary operations
syntax:50 pyExpr:50 "+" pyExpr:51 : pyExpr
syntax:50 pyExpr:50 "-" pyExpr:51 : pyExpr
syntax:60 pyExpr:60 "*" pyExpr:61 : pyExpr
syntax:60 pyExpr:60 "/" pyExpr:61 : pyExpr
syntax:60 pyExpr:60 "%" pyExpr:61 : pyExpr
syntax:70 pyExpr:71 "**" pyExpr:70 : pyExpr  -- right associative

-- Unary operations
syntax:75 "-" pyExpr:75 : pyExpr

-- Comparison operations
syntax:40 pyExpr:40 "==" pyExpr:41 : pyExpr
syntax:40 pyExpr:40 "!=" pyExpr:41 : pyExpr
syntax:40 pyExpr:40 "<" pyExpr:41 : pyExpr
syntax:40 pyExpr:40 ">" pyExpr:41 : pyExpr
syntax:40 pyExpr:40 "<=" pyExpr:41 : pyExpr
syntax:40 pyExpr:40 ">=" pyExpr:41 : pyExpr
syntax:40 pyExpr:40 "in" pyExpr:41 : pyExpr

-- Boolean operations
syntax:30 pyExpr:30 "and" pyExpr:31 : pyExpr
syntax:30 pyExpr:30 "or" pyExpr:31 : pyExpr
syntax:70 "not" pyExpr:70 : pyExpr

-- this clashes if conditional list comprehension
-- -- Ternary conditional
-- syntax:20 pyExpr:21 "if" pyExpr "else" pyExpr:20 : pyExpr

-- Function calls (noWs to distinguish from expr followed by tuple)
-- Supports positional args, keyword args, or mixed
syntax pyArg := pyExpr -- (ident " = ")?
syntax pyExpr noWs "(" pyExpr,* ")" : pyExpr

-- Lambda expressions
syntax "lambda" sepBy1(ident, ",") ":" pyExpr : pyExpr

-- List literals and comprehensions
syntax "[" pyExpr,* "]" : pyExpr
syntax "[" pyExpr "for" ident "in" pyExpr ("if" pyExpr)? "]" : pyExpr

-- Indexing (noWs to distinguish from expr followed by list)
syntax:80 pyExpr:80 noWs "[" pyExpr "]" : pyExpr

-- Attribute access
syntax:80 pyExpr:80 noWs "." (ident)? : pyExpr

-- -- Parenthesized expressions and tuple literals
-- syntax "(" pyExpr ")" : pyExpr
syntax "(" pyExpr,+ ")" : pyExpr

-- Python statements
syntax pyExpr : pyStmt

-- Assignment
-- the precedence of the type is 55 to beat the equal sign " = " which has precendence 50
syntax ident (":" term:55)? "=" pyExpr : pyStmt

-- Tuple unpacking assignment (nested Prod projection)
syntax ident "," sepBy1(ident, ",") "=" pyExpr : pyStmt

-- Struct unpacking (numeric projection .1, .2, ...)
syntax "{" sepBy1(ident, ",") "}" "=" pyExpr : pyStmt

-- Augmented assignment (all operators)
syntax ident "+=" pyExpr : pyStmt
syntax ident "-=" pyExpr : pyStmt
syntax ident "*=" pyExpr : pyStmt
syntax ident "/=" pyExpr : pyStmt
syntax ident "%=" pyExpr : pyStmt

-- If statement
syntax pyElif := colEq "elif" pyExpr ":" pyBlock
syntax pyElse := colEq "else" ":" pyBlock
syntax withPosition("if" pyExpr ":" pyBlock pyElif* (pyElse)?) : pyStmt

-- While loop
syntax withPosition("while" pyExpr ":" pyBlock) : pyStmt

-- For loop
syntax withPosition("for" ident "in" pyExpr ":" pyBlock) : pyStmt

-- Function definition (with optional type annotations and default values)
syntax pyParam := ident (":" term)? ("=" pyExpr)?
syntax withPosition("def" ident "(" sepBy(pyParam, ",") ")" ":" pyBlock) : pyStmt

-- Return statement
syntax "return" pyExpr : pyStmt
syntax "return" pyExpr "," sepBy1(pyExpr, ",") : pyStmt
syntax "return" : pyStmt

-- Break and continue
syntax "break" : pyStmt
syntax "continue" : pyStmt

-- Pass statement
syntax "pass" : pyStmt

-- Print statement
syntax "print" "(" sepBy(pyExpr, ",") ")" : pyStmt

-- Block (sequence of statements)
syntax (colGt pyStmt)* : pyBlock
