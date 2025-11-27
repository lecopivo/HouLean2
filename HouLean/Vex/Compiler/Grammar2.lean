import Lean

namespace VEX

open Lean Parser

-- Type specifiers
declare_syntax_cat vex_type (behavior := symbol)
syntax "int" : vex_type
syntax "float" : vex_type
syntax "matrix3" : vex_type
syntax "matrix" : vex_type
syntax "vector4" : vex_type
syntax "string" : vex_type
syntax "vector" : vex_type
syntax "dict" : vex_type
syntax "bsdf" : vex_type

-- Storage specifiers
declare_syntax_cat vex_storage (behavior := symbol)
syntax "export" : vex_storage

-- Constants
declare_syntax_cat vex_constant
syntax num : vex_constant
syntax scientific : vex_constant

-- Forward declare all expression categories
declare_syntax_cat vex_expr
declare_syntax_cat vex_assignment_expr
declare_syntax_cat vex_cast_expr
declare_syntax_cat vex_nc_cast_expr
declare_syntax_cat vex_unary_expr
declare_syntax_cat vex_postfix_expr
declare_syntax_cat vex_signature
declare_syntax_cat vex_argument_expr_list
declare_syntax_cat vex_constant_expr

-- Array syntax (needed early)
declare_syntax_cat vex_negconst
syntax "-" num : vex_negconst
syntax num : vex_negconst

declare_syntax_cat vex_arrayelement
declare_syntax_cat vex_arraydata
declare_syntax_cat vex_array

syntax vex_array : vex_arrayelement
syntax vex_negconst : vex_arrayelement
syntax str : vex_arrayelement

syntax vex_arraydata "," vex_arrayelement : vex_arraydata
syntax vex_arrayelement : vex_arraydata

syntax "{" vex_arraydata "}" : vex_array
syntax "{" vex_arraydata "," "}" : vex_array

-- Primary expressions (const)
declare_syntax_cat vex_const_primary_expr
syntax vex_array : vex_const_primary_expr
syntax str : vex_const_primary_expr

-- Primary expressions (non-const)
declare_syntax_cat vex_nc_primary_expr
syntax ident : vex_nc_primary_expr
syntax vex_constant : vex_nc_primary_expr
syntax "(" vex_expr ")" : vex_nc_primary_expr

-- Postfix expressions (const)
declare_syntax_cat vex_const_postfix_expr
syntax vex_const_primary_expr : vex_const_postfix_expr

-- Postfix expressions (lhs)
declare_syntax_cat vex_lhs_postfix_expr
syntax vex_type "(" vex_postfix_expr vex_signature ")" : vex_lhs_postfix_expr
syntax vex_postfix_expr vex_signature : vex_lhs_postfix_expr
syntax vex_postfix_expr "++" : vex_lhs_postfix_expr
syntax vex_postfix_expr "--" : vex_lhs_postfix_expr

-- Postfix expressions (non-const)
declare_syntax_cat vex_nc_postfix_expr
syntax vex_nc_primary_expr : vex_nc_postfix_expr
syntax vex_lhs_postfix_expr : vex_nc_postfix_expr
syntax vex_postfix_expr "." ident : vex_nc_postfix_expr
syntax vex_postfix_expr "[" vex_expr "]" : vex_nc_postfix_expr

-- Postfix expressions
syntax vex_const_postfix_expr : vex_postfix_expr
syntax vex_nc_postfix_expr : vex_postfix_expr

-- Unary expressions (const)
declare_syntax_cat vex_const_unary_expr
syntax vex_const_postfix_expr : vex_const_unary_expr

-- Unary expressions (lhs)
declare_syntax_cat vex_lhs_unary_expr
syntax vex_lhs_postfix_expr : vex_lhs_unary_expr
syntax "++" vex_unary_expr : vex_lhs_unary_expr
syntax "--" vex_unary_expr : vex_lhs_unary_expr

-- Unary expressions (non-const)
declare_syntax_cat vex_nc_unary_expr
syntax vex_nc_postfix_expr : vex_nc_unary_expr
syntax "++" vex_unary_expr : vex_nc_unary_expr
syntax "--" vex_unary_expr : vex_nc_unary_expr
syntax "+" vex_cast_expr : vex_nc_unary_expr
syntax "-" vex_nc_cast_expr : vex_nc_unary_expr
syntax "~" vex_cast_expr : vex_nc_unary_expr
syntax "!" vex_cast_expr : vex_nc_unary_expr

-- Unary expressions
syntax vex_const_unary_expr : vex_unary_expr
syntax vex_nc_unary_expr : vex_unary_expr

-- Cast expressions (const)
declare_syntax_cat vex_const_cast_expr
syntax vex_const_unary_expr : vex_const_cast_expr

-- Cast expressions (non-const)
syntax vex_nc_unary_expr : vex_nc_cast_expr
syntax "(" vex_type ")" vex_cast_expr : vex_nc_cast_expr

-- Cast expressions
syntax vex_const_cast_expr : vex_cast_expr
syntax vex_nc_cast_expr : vex_cast_expr

-- Binary expressions with precedence
declare_syntax_cat vex_multiplicative_expr
syntax vex_cast_expr : vex_multiplicative_expr
syntax:50 vex_multiplicative_expr:50 "*" vex_cast_expr:51 : vex_multiplicative_expr
syntax:50 vex_multiplicative_expr:50 "/" vex_cast_expr:51 : vex_multiplicative_expr
syntax:50 vex_multiplicative_expr:50 "%" vex_cast_expr:51 : vex_multiplicative_expr

declare_syntax_cat vex_additive_expr
syntax vex_multiplicative_expr : vex_additive_expr
syntax:45 vex_additive_expr:45 "+" vex_multiplicative_expr:46 : vex_additive_expr
syntax:45 vex_additive_expr:45 "-" vex_multiplicative_expr:46 : vex_additive_expr

declare_syntax_cat vex_shift_expr
syntax vex_additive_expr : vex_shift_expr

declare_syntax_cat vex_relational_expr
syntax vex_shift_expr : vex_relational_expr
syntax:40 vex_relational_expr:40 "<" vex_shift_expr:41 : vex_relational_expr
syntax:40 vex_relational_expr:40 ">" vex_shift_expr:41 : vex_relational_expr
syntax:40 vex_relational_expr:40 "<=" vex_shift_expr:41 : vex_relational_expr
syntax:40 vex_relational_expr:40 ">=" vex_shift_expr:41 : vex_relational_expr

declare_syntax_cat vex_equality_expr
syntax vex_relational_expr : vex_equality_expr
syntax:35 vex_equality_expr:35 "==" vex_relational_expr:36 : vex_equality_expr
syntax:35 vex_equality_expr:35 "!=" vex_relational_expr:36 : vex_equality_expr
syntax:35 vex_equality_expr:35 "~=" vex_relational_expr:36 : vex_equality_expr

declare_syntax_cat vex_and_expr
syntax vex_equality_expr : vex_and_expr
syntax:30 vex_and_expr:30 "&" vex_equality_expr:31 : vex_and_expr

declare_syntax_cat vex_exclusive_or_expr
syntax vex_and_expr : vex_exclusive_or_expr
syntax:25 vex_exclusive_or_expr:25 "^" vex_and_expr:26 : vex_exclusive_or_expr

declare_syntax_cat vex_inclusive_or_expr
syntax vex_exclusive_or_expr : vex_inclusive_or_expr
syntax:20 vex_inclusive_or_expr:20 "|" vex_exclusive_or_expr:21 : vex_inclusive_or_expr

declare_syntax_cat vex_logical_and_expr
syntax vex_inclusive_or_expr : vex_logical_and_expr
syntax:15 vex_logical_and_expr:15 "&&" vex_inclusive_or_expr:16 : vex_logical_and_expr

declare_syntax_cat vex_logical_or_expr
syntax vex_logical_and_expr : vex_logical_or_expr
syntax:10 vex_logical_or_expr:10 "||" vex_logical_and_expr:11 : vex_logical_or_expr

declare_syntax_cat vex_conditional_expr
syntax vex_logical_or_expr : vex_conditional_expr
syntax:5 vex_logical_or_expr:5 "?" vex_logical_or_expr:5 ":" vex_conditional_expr:4 : vex_conditional_expr

-- Assignment expressions (lhs)
declare_syntax_cat vex_lhs_assignment
syntax vex_nc_unary_expr "=" vex_assignment_expr : vex_lhs_assignment
syntax vex_nc_unary_expr "*=" vex_assignment_expr : vex_lhs_assignment
syntax vex_nc_unary_expr "/=" vex_assignment_expr : vex_lhs_assignment
syntax vex_nc_unary_expr "%=" vex_assignment_expr : vex_lhs_assignment
syntax vex_nc_unary_expr "+=" vex_assignment_expr : vex_lhs_assignment
syntax vex_nc_unary_expr "-=" vex_assignment_expr : vex_lhs_assignment
syntax vex_nc_unary_expr "&=" vex_assignment_expr : vex_lhs_assignment
syntax vex_nc_unary_expr "^=" vex_assignment_expr : vex_lhs_assignment
syntax vex_nc_unary_expr "|=" vex_assignment_expr : vex_lhs_assignment

-- Assignment expressions (rhs)
declare_syntax_cat vex_rhs_assignment
syntax vex_conditional_expr : vex_rhs_assignment

-- Assignment expressions
syntax vex_rhs_assignment : vex_assignment_expr
syntax vex_lhs_assignment : vex_assignment_expr

-- Expression
syntax vex_assignment_expr : vex_expr
syntax vex_expr "," vex_assignment_expr : vex_expr

-- LHS expression
declare_syntax_cat vex_lhs_expr
syntax vex_lhs_assignment : vex_lhs_expr
syntax vex_lhs_unary_expr : vex_lhs_expr
syntax vex_lhs_expr "," vex_lhs_assignment : vex_lhs_expr

-- Argument list
syntax vex_assignment_expr : vex_argument_expr_list
syntax vex_argument_expr_list "," vex_assignment_expr : vex_argument_expr_list

-- Signature (function call)
syntax "(" ")" : vex_signature
syntax "(" vex_argument_expr_list ")" : vex_signature

-- Constant expression
syntax vex_conditional_expr : vex_constant_expr

-- Forward declare statement categories
declare_syntax_cat vex_statement
declare_syntax_cat vex_compound_statement
declare_syntax_cat vex_statement_list
declare_syntax_cat vex_declaration
declare_syntax_cat vex_function_parameter_list

-- Declarations
declare_syntax_cat vex_declaration_specifiers
syntax vex_type : vex_declaration_specifiers
syntax "void" : vex_declaration_specifiers
syntax vex_type vex_declaration_specifiers : vex_declaration_specifiers
syntax vex_storage vex_type : vex_declaration_specifiers
syntax vex_storage "void" : vex_declaration_specifiers

declare_syntax_cat vex_declarator
syntax ident : vex_declarator
syntax "(" vex_declarator ")" : vex_declarator
syntax vex_declarator "[" "]" : vex_declarator
syntax vex_declarator "[" vex_constant_expr "]" : vex_declarator
syntax vex_declarator "(" ")" : vex_declarator
syntax vex_declarator "(" vex_function_parameter_list ")" : vex_declarator

declare_syntax_cat vex_init_declarator
syntax vex_declarator : vex_init_declarator
syntax vex_declarator "=" vex_assignment_expr : vex_init_declarator

declare_syntax_cat vex_init_declarator_list
syntax vex_declaration_specifiers vex_init_declarator : vex_init_declarator_list
syntax vex_init_declarator_list "," vex_init_declarator : vex_init_declarator_list

syntax vex_declaration_specifiers ";" : vex_declaration
syntax vex_init_declarator_list ";" : vex_declaration

-- Statements
declare_syntax_cat vex_return_statement
syntax "return" vex_argument_expr_list ";" : vex_return_statement
syntax "return" ";" : vex_return_statement

syntax "{" "}" : vex_compound_statement
syntax "{" vex_statement_list "}" : vex_compound_statement

declare_syntax_cat vex_expression_statement
syntax ";" : vex_expression_statement
syntax vex_lhs_expr ";" : vex_expression_statement

declare_syntax_cat vex_selection_statement
syntax "if" "(" vex_expr ")" vex_statement : vex_selection_statement
syntax "if" "(" vex_expr ")" vex_statement "else" vex_statement : vex_selection_statement

declare_syntax_cat vex_iteration_statement
syntax "while" "(" vex_expr ")" vex_statement : vex_iteration_statement
syntax "foreach" "(" ident ";" vex_assignment_expr ")" vex_statement : vex_iteration_statement
syntax "foreach" "(" ident "," ident ";" vex_assignment_expr ")" vex_statement : vex_iteration_statement
syntax "gather" "(" vex_argument_expr_list ")" vex_statement : vex_iteration_statement
syntax "gather" "(" vex_argument_expr_list ")" vex_statement "else" vex_statement : vex_iteration_statement
syntax "illuminance" "(" vex_argument_expr_list ")" vex_statement : vex_iteration_statement
syntax "forpoints" "(" vex_argument_expr_list ")" vex_statement : vex_iteration_statement
syntax "do" vex_statement "while" "(" vex_expr ")" ";" : vex_iteration_statement
syntax "for" "(" vex_expression_statement vex_expr ";" vex_expr ")" vex_statement : vex_iteration_statement
syntax "for" "(" vex_expression_statement ";" vex_expr ")" vex_statement : vex_iteration_statement
syntax "for" "(" vex_expression_statement vex_expr ";" ")" vex_statement : vex_iteration_statement
syntax "for" "(" vex_expression_statement ";" ")" vex_statement : vex_iteration_statement

declare_syntax_cat vex_jump_statement
syntax "break" ";" : vex_jump_statement
syntax "continue" ";" : vex_jump_statement

syntax vex_compound_statement : vex_statement
syntax vex_declaration : vex_statement
syntax vex_expression_statement : vex_statement
syntax vex_selection_statement : vex_statement
syntax vex_iteration_statement : vex_statement
syntax vex_jump_statement : vex_statement
syntax vex_return_statement : vex_statement

syntax vex_statement : vex_statement_list
syntax vex_statement_list vex_statement : vex_statement_list

-- Function parameters
declare_syntax_cat vex_function_variables_list
syntax vex_type ident : vex_function_variables_list
syntax vex_type ident "[" "]" : vex_function_variables_list
syntax vex_storage vex_type ident : vex_function_variables_list
syntax vex_storage vex_type ident "[" "]" : vex_function_variables_list
syntax vex_function_variables_list "," ident : vex_function_variables_list

syntax vex_function_variables_list : vex_function_parameter_list
syntax vex_function_parameter_list ";" vex_function_variables_list : vex_function_parameter_list

-- Function definition
declare_syntax_cat vex_forward_declaration
syntax vex_type ident "(" ")" : vex_forward_declaration
syntax vex_type ident "(" vex_function_parameter_list ")" : vex_forward_declaration
syntax vex_type ident "(" vex_function_parameter_list ";" ")" : vex_forward_declaration
syntax vex_type "[" "]" ident "(" ")" : vex_forward_declaration
syntax vex_type "[" "]" ident "(" vex_function_parameter_list ")" : vex_forward_declaration
syntax vex_type "[" "]" ident "(" vex_function_parameter_list ";" ")" : vex_forward_declaration
syntax "void" ident "(" ")" : vex_forward_declaration
syntax "void" ident "(" vex_function_parameter_list ")" : vex_forward_declaration
syntax "void" ident "(" vex_function_parameter_list ";" ")" : vex_forward_declaration

declare_syntax_cat vex_function
syntax vex_forward_declaration ";" : vex_function
syntax vex_forward_declaration vex_compound_statement : vex_function
syntax vex_forward_declaration vex_compound_statement ";" : vex_function

declare_syntax_cat vex_functions
syntax vex_function : vex_functions
syntax vex_functions vex_function : vex_functions

-- Shader parameters
declare_syntax_cat vex_shader_instance_variable
syntax ident "=" vex_negconst : vex_shader_instance_variable
syntax ident "[" "]" "=" vex_array : vex_shader_instance_variable
syntax ident "=" vex_array : vex_shader_instance_variable
syntax ident "=" str : vex_shader_instance_variable
syntax ident : vex_shader_instance_variable

declare_syntax_cat vex_shader_instance_variables_list
syntax vex_type vex_shader_instance_variable : vex_shader_instance_variables_list
syntax vex_storage vex_type vex_shader_instance_variable : vex_shader_instance_variables_list
syntax vex_shader_instance_variables_list "," vex_shader_instance_variable : vex_shader_instance_variables_list

declare_syntax_cat vex_shader_parameter_list
syntax vex_shader_instance_variables_list : vex_shader_parameter_list
syntax vex_shader_parameter_list ";" vex_shader_instance_variables_list : vex_shader_parameter_list

declare_syntax_cat vex_shader_declaration
syntax ident "(" ")" : vex_shader_declaration
syntax ident "(" vex_shader_parameter_list ")" : vex_shader_declaration
syntax ident "(" vex_shader_parameter_list ";" ")" : vex_shader_declaration

declare_syntax_cat vex_declaration_list
syntax vex_declaration : vex_declaration_list
syntax vex_declaration_list vex_declaration : vex_declaration_list

declare_syntax_cat vex_shader_body
syntax vex_compound_statement : vex_shader_body
syntax vex_declaration_list vex_compound_statement : vex_shader_body

declare_syntax_cat vex_shader_definition
syntax vex_shader_declaration vex_shader_body : vex_shader_definition

-- Top-level VFL file structure
declare_syntax_cat vex_context
syntax ident : vex_context

declare_syntax_cat vex_vfl_file_head
syntax vex_context : vex_vfl_file_head
syntax vex_functions vex_context : vex_vfl_file_head

declare_syntax_cat vex_vfl_file
syntax vex_vfl_file_head vex_shader_definition : vex_vfl_file

end VEX
