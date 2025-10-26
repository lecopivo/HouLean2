/-
VEX to Lean 4 Macro Transformation
Translates VEX syntax to Lean code with proper handling of assignments
-/

import HouLean.Vex.Parser
import Lean

namespace VEX

open Lean Parser

-- Vector literal syntax for VEX arrays
syntax "#vex[" term,* "]" : term

--------------------------------------------------------------------------------
-- VEX Context for Context-Aware Translation
--------------------------------------------------------------------------------

inductive WrangleType
  | point
  | primitive
  | vertex
  | detail
  | volume

structure VexContext where
  wrangleType : WrangleType
  boundVars : Std.HashMap String Ident

abbrev VexMacroM := ReaderT VexContext MacroM

-- Helper: Get the appropriate attribute getter function based on wrangle type
def getAttribFn (ctx : VexContext) (baseAttribType : String) : String :=
  match ctx.wrangleType with
  | .point => "getPoint" ++ baseAttribType ++ "Attrib"
  | .primitive => "getPrim" ++ baseAttribType ++ "Attrib"
  | .vertex => "getVertex" ++ baseAttribType ++ "Attrib"
  | .detail => "getDetail" ++ baseAttribType ++ "Attrib"
  | .volume => "getVolume" ++ baseAttribType ++ "Attrib"

-- Helper: Get the appropriate attribute setter function based on wrangle type
def setAttribFn (ctx : VexContext) (baseAttribType : String) : String :=
  match ctx.wrangleType with
  | .point => "setPoint" ++ baseAttribType ++ "Attrib"
  | .primitive => "setPrim" ++ baseAttribType ++ "Attrib"
  | .vertex => "setVertex" ++ baseAttribType ++ "Attrib"
  | .detail => "setDetail" ++ baseAttribType ++ "Attrib"
  | .volume => "setVolume" ++ baseAttribType ++ "Attrib"

-- Helper: Check if an attribute name is a bound global variable
def isBoundVar (ctx : VexContext) (name : String) : Option Ident :=
  ctx.boundVars[name]?

-- Default contexts for different wrangle types
def defaultPointContext (ptnum primnum vtxnum : Ident) : VexContext := {
  wrangleType := .point
  boundVars := Std.HashMap.emptyWithCapacity 3
    |>.insert "ptnum" ptnum
    |>.insert "primnum" primnum
    |>.insert "vtxnum" vtxnum
}

def defaultPrimContext (primnum ptnum : Ident) : VexContext := {
  wrangleType := .primitive
  boundVars := Std.HashMap.emptyWithCapacity 2
    |>.insert "primnum" primnum
    |>.insert "ptnum" ptnum
}

def defaultVertexContext (vtxnum ptnum primnum : Ident) : VexContext := {
  wrangleType := .vertex
  boundVars := Std.HashMap.emptyWithCapacity 3
    |>.insert "vtxnum" vtxnum
    |>.insert "ptnum" ptnum
    |>.insert "primnum" primnum
}

def defaultVolumeContext (ix iy iz : Ident) : VexContext := {
  wrangleType := .volume
  boundVars := Std.HashMap.emptyWithCapacity 3
    |>.insert "ix" ix
    |>.insert "iy" iy
    |>.insert "iz" iz
}

def defaultDetailContext : VexContext := {
  wrangleType := .detail
  boundVars := Std.HashMap.emptyWithCapacity
}

--------------------------------------------------------------------------------
-- Helper: Check if type qualifier contains "const"
--------------------------------------------------------------------------------

def isConstQualified (quals : Array (TSyntax `vexTypeQual)) : Bool :=
  quals.any fun q => q.raw[0]!.getAtomVal == "const"

--------------------------------------------------------------------------------
-- Helper functions for type conversion
--------------------------------------------------------------------------------

def vexTypeToTerm (ty : TSyntax `vexType) : MacroM Term :=
  match ty with
  | `(vexType| int) => `(Int)
  | `(vexType| float) => `(Float)
  | `(vexType| vector) => `(Vector3)
  | `(vexType| vector2) => `(Vector2)
  | `(vexType| vector4) => `(Vector4)
  | `(vexType| matrix) => `(Matrix3)
  | `(vexType| matrix2) => `(Matrix2)
  | `(vexType| matrix3) => `(Matrix3)
  | `(vexType| string) => `(String)
  | `(vexType| void) => `(Unit)
  | `(vexType| dict) => `(Dict)
  | `(vexType| bsdf) => `(BSDF)
  | `(vexType| $id:ident) => `($id)
  | _ => Macro.throwError s!"Unsupported VEX type: {ty.raw.prettyPrint}"

def vexFullTypeToTerm (fullTy : TSyntax `vexFullType) : MacroM Term :=
  match fullTy with
  | `(vexFullType| $[$_:vexTypeQual]* $ty:vexType) => 
    vexTypeToTerm ty
  | `(vexFullType| $[$_:vexTypeQual]* $ty:vexType [ ]) => do
    let elemTy ← vexTypeToTerm ty
    `(Array $elemTy)
  | _ => Macro.throwError s!"Unsupported VEX full type: {fullTy.raw.prettyPrint}"

def vexBuiltinTypeToIdent (ty : TSyntax `vexBuiltinType) : MacroM Ident :=
  match ty with
  | `(vexBuiltinType| int) => `(Int)
  | `(vexBuiltinType| float) => `(Float)
  | `(vexBuiltinType| vector) => `(Vector3)
  | `(vexBuiltinType| vector2) => `(Vector2)
  | `(vexBuiltinType| vector4) => `(Vector4)
  | `(vexBuiltinType| matrix) => `(Matrix3)
  | `(vexBuiltinType| matrix2) => `(Matrix2)
  | `(vexBuiltinType| matrix3) => `(Matrix3)
  | `(vexBuiltinType| string) => `(String)
  | `(vexBuiltinType| dict) => `(Dict)
  | `(vexBuiltinType| bsdf) => `(BSDF)
  | _ => Macro.throwError s!"Unsupported VEX builtin type: {ty.raw.prettyPrint}"

--------------------------------------------------------------------------------
-- Macro: vexExpr → term (Pure expressions, no assignments allowed)
--------------------------------------------------------------------------------

partial def vexExprToTerm (e : TSyntax `vexExpr) : VexMacroM Term := do
  let ctx ← read
  match e with
  -- Literals
  | `(vexExpr| $n:num) => `($n)
  | `(vexExpr| $s:scientific) => `($s)
  | `(vexExpr| $s:str) => `($s)
  | `(vexExpr| $id:ident) => `($id)
  
  -- Array/vector literals
  | `(vexExpr| { $[$elems],* }) => do
    let elemTerms ← elems.mapM vexExprToTerm
    `(#vex[ $[$elemTerms],* ])
  
  -- Parenthesized
  | `(vexExpr| ( $inner:vexExpr )) => vexExprToTerm inner
  
  -- Channel/Attribute access - context-aware translation
  | `(vexExpr| @$name:ident) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    if let some boundId := isBoundVar ctx nameStr then
      return boundId
    else
      let fnName := getAttribFn ctx ""
      let nameStrLit := Syntax.mkStrLit nameStr
      `($(mkIdent (Name.mkSimple fnName)):ident $nameStrLit)
  
  | `(vexExpr| s@$name:ident) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    if let some boundId := isBoundVar ctx nameStr then
      return boundId
    else
      let fnName := getAttribFn ctx "String"
      let nameStrLit := Syntax.mkStrLit nameStr
      `($(mkIdent (Name.mkSimple fnName)):ident $nameStrLit)
  
  | `(vexExpr| v@$name:ident) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    if let some boundId := isBoundVar ctx nameStr then
      return boundId
    else
      let fnName := getAttribFn ctx "Vector"
      let nameStrLit := Syntax.mkStrLit nameStr
      `($(mkIdent (Name.mkSimple fnName)):ident $nameStrLit)
  
  | `(vexExpr| p@$name:ident) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    if let some boundId := isBoundVar ctx nameStr then
      return boundId
    else
      let fnName := getAttribFn ctx "Vector4"
      let nameStrLit := Syntax.mkStrLit nameStr
      `($(mkIdent (Name.mkSimple fnName)):ident $nameStrLit)
  
  | `(vexExpr| i@$name:ident) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    if let some boundId := isBoundVar ctx nameStr then
      return boundId
    else
      let fnName := getAttribFn ctx "Int"
      let nameStrLit := Syntax.mkStrLit nameStr
      `($(mkIdent (Name.mkSimple fnName)):ident $nameStrLit)
  
  | `(vexExpr| f@$name:ident) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    if let some boundId := isBoundVar ctx nameStr then
      return boundId
    else
      let fnName := getAttribFn ctx "Float"
      let nameStrLit := Syntax.mkStrLit nameStr
      `($(mkIdent (Name.mkSimple fnName)):ident $nameStrLit)
  
  | `(vexExpr| u@$name:ident) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    if let some boundId := isBoundVar ctx nameStr then
      return boundId
    else
      let fnName := getAttribFn ctx "Int"
      let nameStrLit := Syntax.mkStrLit nameStr
      `($(mkIdent (Name.mkSimple fnName)):ident $nameStrLit)
  
  | `(vexExpr| d@$name:ident) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    if let some boundId := isBoundVar ctx nameStr then
      return boundId
    else
      let fnName := getAttribFn ctx "Dict"
      let nameStrLit := Syntax.mkStrLit nameStr
      `($(mkIdent (Name.mkSimple fnName)):ident $nameStrLit)
  
  | `(vexExpr| m2@$name:ident) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    if let some boundId := isBoundVar ctx nameStr then
      return boundId
    else
      let fnName := getAttribFn ctx "Matrix2"
      let nameStrLit := Syntax.mkStrLit nameStr
      `($(mkIdent (Name.mkSimple fnName)):ident $nameStrLit)
  
  | `(vexExpr| m3@$name:ident) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    if let some boundId := isBoundVar ctx nameStr then
      return boundId
    else
      let fnName := getAttribFn ctx "Matrix3"
      let nameStrLit := Syntax.mkStrLit nameStr
      `($(mkIdent (Name.mkSimple fnName)):ident $nameStrLit)
  
  | `(vexExpr| m4@$name:ident) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    if let some boundId := isBoundVar ctx nameStr then
      return boundId
    else
      let fnName := getAttribFn ctx "Matrix4"
      let nameStrLit := Syntax.mkStrLit nameStr
      `($(mkIdent (Name.mkSimple fnName)):ident $nameStrLit)
  
  -- Array attribute access
  | `(vexExpr| []@$name:ident) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    if let some boundId := isBoundVar ctx nameStr then
      return boundId
    else
      let fnName := getAttribFn ctx "Array"
      let nameStrLit := Syntax.mkStrLit nameStr
      `($(mkIdent (Name.mkSimple fnName)):ident $nameStrLit)
  
  | `(vexExpr| s[]@$name:ident) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    if let some boundId := isBoundVar ctx nameStr then
      return boundId
    else
      let fnName := getAttribFn ctx "StringArray"
      let nameStrLit := Syntax.mkStrLit nameStr
      `($(mkIdent (Name.mkSimple fnName)):ident $nameStrLit)
  
  | `(vexExpr| v[]@$name:ident) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    if let some boundId := isBoundVar ctx nameStr then
      return boundId
    else
      let fnName := getAttribFn ctx "VectorArray"
      let nameStrLit := Syntax.mkStrLit nameStr
      `($(mkIdent (Name.mkSimple fnName)):ident $nameStrLit)
  
  | `(vexExpr| i[]@$name:ident) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    if let some boundId := isBoundVar ctx nameStr then
      return boundId
    else
      let fnName := getAttribFn ctx "IntArray"
      let nameStrLit := Syntax.mkStrLit nameStr
      `($(mkIdent (Name.mkSimple fnName)):ident $nameStrLit)
  
  | `(vexExpr| f[]@$name:ident) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    if let some boundId := isBoundVar ctx nameStr then
      return boundId
    else
      let fnName := getAttribFn ctx "FloatArray"
      let nameStrLit := Syntax.mkStrLit nameStr
      `($(mkIdent (Name.mkSimple fnName)):ident $nameStrLit)
  
  -- Global variables
  | `(vexExpr| $$$name:ident) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    if let some boundId := isBoundVar ctx nameStr then
      return boundId
    else
      let nameStrLit := Syntax.mkStrLit nameStr
      `(getGlobal $nameStrLit)
  
  -- Function call
  | `(vexExpr| $fn:vexExpr ( $[$args],* )) => do
    let fnTerm ← vexExprToTerm fn
    let argTerms ← args.mapM vexExprToTerm
    `(term| $fnTerm:term $argTerms*)
  
  -- Array subscript
  | `(vexExpr| $arr:vexExpr [ $idx:vexExpr ]) => do
    let arrTerm ← vexExprToTerm arr
    let idxTerm ← vexExprToTerm idx
    `($arrTerm[$idxTerm]!)
  
  -- Member access
  | `(vexExpr| $obj:vexExpr . $field:ident) => do
    let objTerm ← vexExprToTerm obj
    `($objTerm.$field)
  
  -- Method call (arrow operator)
  | `(vexExpr| $obj:vexExpr -> $method:ident ( $[$args],* )) => do
    let objTerm ← vexExprToTerm obj
    let argTerms ← args.mapM vexExprToTerm
    `($objTerm.$method $argTerms*)
  
  -- Binary operators
  | `(vexExpr| $lhs:vexExpr * $rhs:vexExpr) => do
    let l ← vexExprToTerm lhs
    let r ← vexExprToTerm rhs
    `($l * $r)
  | `(vexExpr| $lhs:vexExpr / $rhs:vexExpr) => do
    let l ← vexExprToTerm lhs
    let r ← vexExprToTerm rhs
    `($l / $r)
  | `(vexExpr| $lhs:vexExpr + $rhs:vexExpr) => do
    let l ← vexExprToTerm lhs
    let r ← vexExprToTerm rhs
    `($l + $r)
  | `(vexExpr| $lhs:vexExpr - $rhs:vexExpr) => do
    let l ← vexExprToTerm lhs
    let r ← vexExprToTerm rhs
    `($l - $r)
  | `(vexExpr| $lhs:vexExpr % $rhs:vexExpr) => do
    let l ← vexExprToTerm lhs
    let r ← vexExprToTerm rhs
    `($l % $r)
  
  -- Comparison operators
  | `(vexExpr| $lhs:vexExpr == $rhs:vexExpr) => do
    let l ← vexExprToTerm lhs
    let r ← vexExprToTerm rhs
    `($l == $r)
  | `(vexExpr| $lhs:vexExpr != $rhs:vexExpr) => do
    let l ← vexExprToTerm lhs
    let r ← vexExprToTerm rhs
    `($l != $r)
  | `(vexExpr| $lhs:vexExpr < $rhs:vexExpr) => do
    let l ← vexExprToTerm lhs
    let r ← vexExprToTerm rhs
    `($l < $r)
  | `(vexExpr| $lhs:vexExpr > $rhs:vexExpr) => do
    let l ← vexExprToTerm lhs
    let r ← vexExprToTerm rhs
    `($l > $r)
  | `(vexExpr| $lhs:vexExpr <= $rhs:vexExpr) => do
    let l ← vexExprToTerm lhs
    let r ← vexExprToTerm rhs
    `($l <= $r)
  | `(vexExpr| $lhs:vexExpr >= $rhs:vexExpr) => do
    let l ← vexExprToTerm lhs
    let r ← vexExprToTerm rhs
    `($l >= $r)
  
  -- Logical operators
  | `(vexExpr| $lhs:vexExpr && $rhs:vexExpr) => do
    let l ← vexExprToTerm lhs
    let r ← vexExprToTerm rhs
    `($l && $r)
  | `(vexExpr| $lhs:vexExpr || $rhs:vexExpr) => do
    let l ← vexExprToTerm lhs
    let r ← vexExprToTerm rhs
    `($l || $r)
  
  -- Bitwise operators
  | `(vexExpr| $lhs:vexExpr & $rhs:vexExpr) => do
    let l ← vexExprToTerm lhs
    let r ← vexExprToTerm rhs
    `($l &&& $r)
  | `(vexExpr| $lhs:vexExpr | $rhs:vexExpr) => do
    let l ← vexExprToTerm lhs
    let r ← vexExprToTerm rhs
    `($l ||| $r)
  | `(vexExpr| $lhs:vexExpr ^ $rhs:vexExpr) => do
    let l ← vexExprToTerm lhs
    let r ← vexExprToTerm rhs
    `($l ^^^ $r)
  | `(vexExpr| $lhs:vexExpr << $rhs:vexExpr) => do
    let l ← vexExprToTerm lhs
    let r ← vexExprToTerm rhs
    `($l <<< $r)
  | `(vexExpr| $lhs:vexExpr >> $rhs:vexExpr) => do
    let l ← vexExprToTerm lhs
    let r ← vexExprToTerm rhs
    `($l >>> $r)
  
  -- Unary operators
  | `(vexExpr| - $inner:vexExpr) => do
    let t ← vexExprToTerm inner
    `(- $t)
  | `(vexExpr| + $inner:vexExpr) => vexExprToTerm inner
  | `(vexExpr| ! $inner:vexExpr) => do
    let t ← vexExprToTerm inner
    `(! $t)
  -- Note: Bitwise NOT (~) skipped due to Lean syntax conflicts
  
  -- Type cast
  | `(vexExpr| ( $ty:vexFullType ) $inner:vexExpr) => do
    let innerTerm ← vexExprToTerm inner
    let tyTerm ← vexFullTypeToTerm ty
    `(($innerTerm : $tyTerm))
  
  -- Type constructor
  | `(vexExpr| $ty:vexBuiltinType ( $inner:vexExpr )) => do
    let innerTerm ← vexExprToTerm inner
    let tyIdent ← vexBuiltinTypeToIdent ty
    `(($innerTerm : $tyIdent))
  
  -- Ternary conditional
  | `(vexExpr| $cond:vexExpr ? $thenExpr:vexExpr : $elseExpr:vexExpr) => do
    let c ← vexExprToTerm cond
    let t ← vexExprToTerm thenExpr
    let e ← vexExprToTerm elseExpr
    `(if $c then $t else $e)
  
  -- ERROR: Assignment operators not allowed in pure term context
  | `(vexExpr| $_ = $_) => 
    Macro.throwError "Assignment expressions are not allowed in pure term context"
  | `(vexExpr| $_ += $_) => 
    Macro.throwError "Assignment expressions are not allowed in pure term context"
  | `(vexExpr| $_ -= $_) => 
    Macro.throwError "Assignment expressions are not allowed in pure term context"
  | `(vexExpr| $_ *= $_) => 
    Macro.throwError "Assignment expressions are not allowed in pure term context"
  | `(vexExpr| $_ /= $_) => 
    Macro.throwError "Assignment expressions are not allowed in pure term context"
  | `(vexExpr| $_ %= $_) => 
    Macro.throwError "Assignment expressions are not allowed in pure term context"
  | `(vexExpr| $_ <<= $_) => 
    Macro.throwError "Assignment expressions are not allowed in pure term context"
  | `(vexExpr| $_ >>= $_) => 
    Macro.throwError "Assignment expressions are not allowed in pure term context"
  | `(vexExpr| $_ &= $_) => 
    Macro.throwError "Assignment expressions are not allowed in pure term context"
  | `(vexExpr| $_ ^= $_) => 
    Macro.throwError "Assignment expressions are not allowed in pure term context"
  | `(vexExpr| $_ |= $_) => 
    Macro.throwError "Assignment expressions are not allowed in pure term context"
  
  -- ERROR: Increment/decrement not allowed in pure term context
  | `(vexExpr| $_ ++) => 
    Macro.throwError "Increment/decrement expressions are not allowed in pure term context. Use as statement: 'i++;'"
  | `(vexExpr| ++ $_) => 
    Macro.throwError "Increment/decrement expressions are not allowed in pure term context. Use as statement: '++i;'"
  
  -- Comma operator - just return the last value in pure term context
  | `(vexExpr| $_ , $rhs:vexExpr) => vexExprToTerm rhs
  
  | _ => Macro.throwError s!"Unsupported VEX expression in term context: {e.raw.prettyPrint}"

--------------------------------------------------------------------------------
-- Macro: vexExpr → doElem (Allows assignments, converts to statements)
--------------------------------------------------------------------------------

partial def vexExprToDoElem (e : TSyntax `vexExpr) : VexMacroM (TSyntax `doElem) := do
  let ctx ← read
  match e with
  -- Attribute assignments - convert to setter calls
  | `(vexExpr| @$name:ident = $rhs:vexExpr) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    let rhsTerm ← vexExprToTerm rhs
    let fnName := setAttribFn ctx ""
    let nameStrLit := Syntax.mkStrLit nameStr
    `(doElem| $(mkIdent (Name.mkSimple fnName)):ident $nameStrLit $rhsTerm)
  
  | `(vexExpr| s@$name:ident = $rhs:vexExpr) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    let rhsTerm ← vexExprToTerm rhs
    let fnName := setAttribFn ctx "String"
    let nameStrLit := Syntax.mkStrLit nameStr
    `(doElem| $(mkIdent (Name.mkSimple fnName)):ident $nameStrLit $rhsTerm)
  
  | `(vexExpr| v@$name:ident = $rhs:vexExpr) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    let rhsTerm ← vexExprToTerm rhs
    let fnName := setAttribFn ctx "Vector"
    let nameStrLit := Syntax.mkStrLit nameStr
    `(doElem| $(mkIdent (Name.mkSimple fnName)):ident $nameStrLit $rhsTerm)
  
  | `(vexExpr| p@$name:ident = $rhs:vexExpr) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    let rhsTerm ← vexExprToTerm rhs
    let fnName := setAttribFn ctx "Vector4"
    let nameStrLit := Syntax.mkStrLit nameStr
    `(doElem| $(mkIdent (Name.mkSimple fnName)):ident $nameStrLit $rhsTerm)
  
  | `(vexExpr| i@$name:ident = $rhs:vexExpr) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    let rhsTerm ← vexExprToTerm rhs
    let fnName := setAttribFn ctx "Int"
    let nameStrLit := Syntax.mkStrLit nameStr
    `(doElem| $(mkIdent (Name.mkSimple fnName)):ident $nameStrLit $rhsTerm)
  
  | `(vexExpr| f@$name:ident = $rhs:vexExpr) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    let rhsTerm ← vexExprToTerm rhs
    let fnName := setAttribFn ctx "Float"
    let nameStrLit := Syntax.mkStrLit nameStr
    `(doElem| $(mkIdent (Name.mkSimple fnName)):ident $nameStrLit $rhsTerm)
  
  | `(vexExpr| u@$name:ident = $rhs:vexExpr) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    let rhsTerm ← vexExprToTerm rhs
    let fnName := setAttribFn ctx "Int"
    let nameStrLit := Syntax.mkStrLit nameStr
    `(doElem| $(mkIdent (Name.mkSimple fnName)):ident $nameStrLit $rhsTerm)
  
  | `(vexExpr| d@$name:ident = $rhs:vexExpr) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    let rhsTerm ← vexExprToTerm rhs
    let fnName := setAttribFn ctx "Dict"
    let nameStrLit := Syntax.mkStrLit nameStr
    `(doElem| $(mkIdent (Name.mkSimple fnName)):ident $nameStrLit $rhsTerm)
  
  | `(vexExpr| m2@$name:ident = $rhs:vexExpr) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    let rhsTerm ← vexExprToTerm rhs
    let fnName := setAttribFn ctx "Matrix2"
    let nameStrLit := Syntax.mkStrLit nameStr
    `(doElem| $(mkIdent (Name.mkSimple fnName)):ident $nameStrLit $rhsTerm)
  
  | `(vexExpr| m3@$name:ident = $rhs:vexExpr) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    let rhsTerm ← vexExprToTerm rhs
    let fnName := setAttribFn ctx "Matrix3"
    let nameStrLit := Syntax.mkStrLit nameStr
    `(doElem| $(mkIdent (Name.mkSimple fnName)):ident $nameStrLit $rhsTerm)
  
  | `(vexExpr| m4@$name:ident = $rhs:vexExpr) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    let rhsTerm ← vexExprToTerm rhs
    let fnName := setAttribFn ctx "Matrix4"
    let nameStrLit := Syntax.mkStrLit nameStr
    `(doElem| $(mkIdent (Name.mkSimple fnName)):ident $nameStrLit $rhsTerm)
  
  -- Array attribute assignments
  | `(vexExpr| []@$name:ident = $rhs:vexExpr) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    let rhsTerm ← vexExprToTerm rhs
    let fnName := setAttribFn ctx "Array"
    let nameStrLit := Syntax.mkStrLit nameStr
    `(doElem| $(mkIdent (Name.mkSimple fnName)):ident $nameStrLit $rhsTerm)
  
  | `(vexExpr| s[]@$name:ident = $rhs:vexExpr) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    let rhsTerm ← vexExprToTerm rhs
    let fnName := setAttribFn ctx "StringArray"
    let nameStrLit := Syntax.mkStrLit nameStr
    `(doElem| $(mkIdent (Name.mkSimple fnName)):ident $nameStrLit $rhsTerm)
  
  | `(vexExpr| v[]@$name:ident = $rhs:vexExpr) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    let rhsTerm ← vexExprToTerm rhs
    let fnName := setAttribFn ctx "VectorArray"
    let nameStrLit := Syntax.mkStrLit nameStr
    `(doElem| $(mkIdent (Name.mkSimple fnName)):ident $nameStrLit $rhsTerm)
  
  | `(vexExpr| i[]@$name:ident = $rhs:vexExpr) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    let rhsTerm ← vexExprToTerm rhs
    let fnName := setAttribFn ctx "IntArray"
    let nameStrLit := Syntax.mkStrLit nameStr
    `(doElem| $(mkIdent (Name.mkSimple fnName)):ident $nameStrLit $rhsTerm)
  
  | `(vexExpr| f[]@$name:ident = $rhs:vexExpr) => do
    let nameStr := name.getId.eraseMacroScopes.toString
    let rhsTerm ← vexExprToTerm rhs
    let fnName := setAttribFn ctx "FloatArray"
    let nameStrLit := Syntax.mkStrLit nameStr
    `(doElem| $(mkIdent (Name.mkSimple fnName)):ident $nameStrLit $rhsTerm)
  
  -- Simple assignment (non-attribute)
  | `(vexExpr| $lhs:vexExpr = $rhs:vexExpr) => do
    let lhsTerm ← vexExprToTerm lhs
    let rhsTerm ← vexExprToTerm rhs
    `(doElem| $lhsTerm:term := $rhsTerm)
  
  -- Compound assignments
  | `(vexExpr| $lhs:vexExpr += $rhs:vexExpr) => do
    let lhsTerm ← vexExprToTerm lhs
    let rhsTerm ← vexExprToTerm rhs
    `(doElem| $lhsTerm:term := $lhsTerm + $rhsTerm)
  | `(vexExpr| $lhs:vexExpr -= $rhs:vexExpr) => do
    let lhsTerm ← vexExprToTerm lhs
    let rhsTerm ← vexExprToTerm rhs
    `(doElem| $lhsTerm:term := $lhsTerm - $rhsTerm)
  | `(vexExpr| $lhs:vexExpr *= $rhs:vexExpr) => do
    let lhsTerm ← vexExprToTerm lhs
    let rhsTerm ← vexExprToTerm rhs
    `(doElem| $lhsTerm:term := $lhsTerm * $rhsTerm)
  | `(vexExpr| $lhs:vexExpr /= $rhs:vexExpr) => do
    let lhsTerm ← vexExprToTerm lhs
    let rhsTerm ← vexExprToTerm rhs
    `(doElem| $lhsTerm:term := $lhsTerm / $rhsTerm)
  | `(vexExpr| $lhs:vexExpr %= $rhs:vexExpr) => do
    let lhsTerm ← vexExprToTerm lhs
    let rhsTerm ← vexExprToTerm rhs
    `(doElem| $lhsTerm:term := $lhsTerm % $rhsTerm)
  | `(vexExpr| $lhs:vexExpr &= $rhs:vexExpr) => do
    let lhsTerm ← vexExprToTerm lhs
    let rhsTerm ← vexExprToTerm rhs
    `(doElem| $lhsTerm:term := $lhsTerm &&& $rhsTerm)
  | `(vexExpr| $lhs:vexExpr |= $rhs:vexExpr) => do
    let lhsTerm ← vexExprToTerm lhs
    let rhsTerm ← vexExprToTerm rhs
    `(doElem| $lhsTerm:term := $lhsTerm ||| $rhsTerm)
  | `(vexExpr| $lhs:vexExpr ^= $rhs:vexExpr) => do
    let lhsTerm ← vexExprToTerm lhs
    let rhsTerm ← vexExprToTerm rhs
    `(doElem| $lhsTerm:term := $lhsTerm ^^^ $rhsTerm)
  | `(vexExpr| $lhs:vexExpr <<= $rhs:vexExpr) => do
    let lhsTerm ← vexExprToTerm lhs
    let rhsTerm ← vexExprToTerm rhs
    `(doElem| $lhsTerm:term := $lhsTerm <<< $rhsTerm)
  | `(vexExpr| $lhs:vexExpr >>= $rhs:vexExpr) => do
    let lhsTerm ← vexExprToTerm lhs
    let rhsTerm ← vexExprToTerm rhs
    `(doElem| $lhsTerm:term := $lhsTerm >>> $rhsTerm)
  
  -- Post-increment
  | `(vexExpr| $var:vexExpr ++) => do
    let varTerm ← vexExprToTerm var
    `(doElem| $varTerm:term := $varTerm + 1)
  
  -- Pre-increment
  | `(vexExpr| ++ $var:vexExpr) => do
    let varTerm ← vexExprToTerm var
    `(doElem| $varTerm:term := $varTerm + 1)
  
  -- Comma operator - convert to sequence
  | `(vexExpr| $lhs:vexExpr , $rhs:vexExpr) => do
    let lhsElem ← vexExprToDoElem lhs
    let rhsElem ← vexExprToDoElem rhs
    `(doElem| do $lhsElem; $rhsElem:doElem)
  
  -- Pure expression - convert to discarded result
  | _ => do
    let term ← vexExprToTerm e
    `(doElem| let _ := $term)

--------------------------------------------------------------------------------
-- Macro: vexStmt → doElem (Statement conversion)
--------------------------------------------------------------------------------

partial def vexStmtToDoElem (s : TSyntax `vexStmt) : VexMacroM (Array (TSyntax `doElem)) := do
  match s with
  -- Expression statement
  | `(vexStmt| $e:vexExpr ;) => do
    let elem ← vexExprToDoElem e
    return #[elem]
  
  -- Empty statement
  | `(vexStmt| ;) => return #[]
  
  -- Variable declaration without initialization
  | `(vexStmt| $[$quals:vexTypeQual]* $ty:vexType $name:ident ;) => do
    let tyTerm ← vexTypeToTerm ty
    if isConstQualified quals then
      Macro.throwError "Uninitialized const variable"
    else
      return #[← `(doElem| let mut $name : $tyTerm := default)]
  
  -- Variable declaration with initialization
  | `(vexStmt| $[$quals:vexTypeQual]* $ty:vexType $name:ident = $init:vexExpr ;) => do
    let tyTerm ← vexTypeToTerm ty
    let initTerm ← vexExprToTerm init
    if isConstQualified quals then
      return #[← `(doElem| let $name : $tyTerm := $initTerm)]
    else
      return #[← `(doElem| let mut $name : $tyTerm := $initTerm)]
  
  -- Array declaration with size
  | `(vexStmt| $[$quals:vexTypeQual]* $ty:vexType $name:ident [ $size:vexExpr ] ;) => do
    let tyTerm ← vexTypeToTerm ty
    let sizeTerm ← vexExprToTerm size
    if isConstQualified quals then
      return #[← `(doElem| let $name : Array $tyTerm := Array.mkArray $sizeTerm default)]
    else
      return #[← `(doElem| let mut $name : Array $tyTerm := Array.mkArray $sizeTerm default)]
  
  -- Array declaration with initialization
  | `(vexStmt| $[$quals:vexTypeQual]* $ty:vexType $name:ident [ $[$_:vexExpr]? ] = $init:vexExpr ;) => do
    let tyTerm ← vexTypeToTerm ty
    let initTerm ← vexExprToTerm init
    if isConstQualified quals then
      return #[← `(doElem| let $name : Array $tyTerm := $initTerm)]
    else
      return #[← `(doElem| let mut $name : Array $tyTerm := $initTerm)]
  
  -- If statement
  | `(vexStmt| if ( $cond:vexExpr ) $thenStmt:vexStmt) => do
    let condTerm ← vexExprToTerm cond
    let thenElems ← vexStmtToDoElem thenStmt
    return #[← `(doElem| if $condTerm then do $[$thenElems:doElem]*)]
  
  | `(vexStmt| if ( $cond:vexExpr ) $thenStmt:vexStmt else $elseStmt:vexStmt) => do
    let condTerm ← vexExprToTerm cond
    let thenElems ← vexStmtToDoElem thenStmt
    let elseElems ← vexStmtToDoElem elseStmt
    return #[← `(doElem| if $condTerm then do $[$thenElems:doElem]* else do $[$elseElems:doElem]*)]
  
  -- While loop
  | `(vexStmt| while ( $cond:vexExpr ) $body:vexStmt) => do
    let condTerm ← vexExprToTerm cond
    let bodyElems ← vexStmtToDoElem body
    return #[← `(doElem| while $condTerm do $[$bodyElems:doElem]*)]
  
  -- For loop (basic form)
  | `(vexStmt| for ( $[$init:vexExpr]? ; $[$cond:vexExpr]? ; $[$update:vexExpr]? ) $body:vexStmt) => do
    let mut elems := #[]

    -- Add initialization if present
    if let some init := init then
      elems := elems.push (← vexExprToDoElem init)
    
    -- Build while loop body
    let bodyElems ← vexStmtToDoElem body
    let update ← update.mapM vexExprToDoElem
    let loopBody : Array (TSyntax `doElem) := if let some update := update then
      bodyElems.push update
    else
      bodyElems
    
    -- Add while loop
    let whileLoop ← if let some cond := cond then
      let condTerm ← vexExprToTerm cond
      `(doElem| while $condTerm do $[$loopBody:doElem]*)
    else
      `(doElem| while true do $[$loopBody:doElem]*)
    
    elems := elems.push whileLoop
    return elems

  | `(vexStmt| for ( $[$init:vexStmt]? $[$cond:vexExpr]? ; $[$update:vexExpr]? ) $body:vexStmt) => do
    let init := (← init.mapM vexStmtToDoElem) |>.getD #[(← `(doElem| pure () ))]
    let empty ← `(vexExpr| 0) -- this is a bit hacky, those two for loop notations should be unified
    let forLoop ← vexStmtToDoElem (← `(vexStmt| for ( $empty ;$[$cond:vexExpr]?; $[$update]?) $body))
    let r ← `(doElem|
      do
        $[$init:doElem]*
        $[$forLoop:doElem]*)
    return #[r]
      -- Return statement
  | `(vexStmt| return $e:vexExpr ;) => do
    let term ← vexExprToTerm e
    return #[← `(doElem| return $term)]
  
  | `(vexStmt| return ;) => 
    return #[← `(doElem| return ())]
  
  -- Break/continue
  | `(vexStmt| break ;) => return #[← `(doElem| break)]
  | `(vexStmt| continue ;) => return #[← `(doElem| continue)]
  
  -- Compound statement
  | `(vexStmt| { $[$stmts:vexStmt]* }) => do
    let mut allElems := #[]
    for stmt in stmts do
      let elems ← vexStmtToDoElem stmt
      allElems := allElems ++ elems
    return allElems
  
  | _ => Macro.throwError s!"Unsupported VEX statement: {s.raw.prettyPrint}"

--------------------------------------------------------------------------------
-- Parameter List Conversion
--------------------------------------------------------------------------------

partial def vexParamListToParams (paramList : TSyntax `vexParamList) : MacroM (Array (TSyntax ``Parser.Term.bracketedBinder)) := do
  let rec collect (pl : TSyntax `vexParamList) (acc : Array (TSyntax ``Parser.Term.bracketedBinder)) : MacroM (Array (TSyntax ``Parser.Term.bracketedBinder)) := do
    match pl with
    | `(vexParamList| $param:vexParam) => do
      let p ← vexParamToBinder param
      return acc.push p
    | `(vexParamList| $param:vexParam ; $rest:vexParamList) => do
      let p ← vexParamToBinder param
      collect rest (acc.push p)
    | _ => Macro.throwError s!"Invalid parameter list: {pl.raw.prettyPrint}"
  collect paramList #[]
where
  vexParamToBinder (param : TSyntax `vexParam) : MacroM (TSyntax ``Parser.Term.bracketedBinder) := do
    match param with
    | `(vexParam| $[$_:vexTypeQual]* $ty:vexType $name:ident) => do
      let tyTerm ← vexTypeToTerm ty
      return ⟨Lean.Syntax.node6 .none ``Lean.Parser.Term.bracketedBinder
        (Lean.Syntax.atom .none "(")
        (Lean.Syntax.node1 .none `null name.raw)
        (Lean.Syntax.atom .none ":")
        tyTerm.raw
        (Lean.Syntax.node1 .none `null .missing)
        (Lean.Syntax.atom .none ")")⟩
    | `(vexParam| $[$_:vexTypeQual]* $ty:vexType $name:ident [ ]) => do
      let elemTy ← vexTypeToTerm ty
      let arrTy ← `(Array $elemTy)
      return ⟨Lean.Syntax.node6 .none ``Lean.Parser.Term.bracketedBinder
        (Lean.Syntax.atom .none "(")
        (Lean.Syntax.node1 .none `null name.raw)
        (Lean.Syntax.atom .none ":")
        arrTy.raw
        (Lean.Syntax.node1 .none `null .missing)
        (Lean.Syntax.atom .none ")")⟩
    | _ => Macro.throwError s!"Invalid parameter: {param.raw.prettyPrint}"

--------------------------------------------------------------------------------
-- Function Declaration to Command
--------------------------------------------------------------------------------

def vexFuncDeclToCommand (funcDecl : TSyntax `vexFuncDecl) (ctx : VexContext) : MacroM (TSyntax `command) := do
  match funcDecl with
  -- Function declaration (forward declaration)
  | `(vexFuncDecl| $[$_:vexTypeQual]* $retTy:vexType $name:ident ( $[$params:vexParamList]? ) ;) => do
    let retTyTerm ← vexTypeToTerm retTy
    let paramBinders ← match params with
      | some p => vexParamListToParams p
      | none => pure #[]
    `(opaque $name:ident $[$paramBinders]* : $retTyTerm)
  
  -- Function definition
  | `(vexFuncDecl| $[$_:vexTypeQual]* $retTy:vexType $name:ident ( $[$params:vexParamList]? ) { $[$stmts:vexStmt]* }) => do
    let retTyTerm ← vexTypeToTerm retTy
    let paramBinders ← match params with
      | some p => vexParamListToParams p
      | none => pure #[]
    
    -- Convert statements to do elements (run in VexMacroM context)
    let bodyElems ← ReaderT.run (do
      let mut elems := #[]
      for stmt in stmts do
        let stmtElems ← vexStmtToDoElem stmt
        elems := elems ++ stmtElems
      return elems
    ) ctx
    
    `(def $name:ident $[$paramBinders]* : $retTyTerm := do
        $[$bodyElems:doElem]*)
  
  -- Function with "function" keyword
  | `(vexFuncDecl| function $[$_:vexTypeQual]* $retTy:vexType $name:ident ( $[$params:vexParamList]? ) { $[$stmts:vexStmt]* }) => do
    let retTyTerm ← vexTypeToTerm retTy
    let paramBinders ← match params with
      | some p => vexParamListToParams p
      | none => pure #[]
    
    -- Convert statements to do elements (run in VexMacroM context)
    let bodyElems ← ReaderT.run (do
      let mut elems := #[]
      for stmt in stmts do
        let stmtElems ← vexStmtToDoElem stmt
        elems := elems ++ stmtElems
      return elems
    ) ctx
    
    `(def $name:ident $[$paramBinders]* : $retTyTerm := do
        $[$bodyElems:doElem]*)
  
  | _ => Macro.throwError s!"Unsupported function declaration: {funcDecl.raw.prettyPrint}"

-- Macro that uses a default point wrangle context
macro "vex%(" fn:vexFuncDecl ")" : command => do
  let ctx := defaultPointContext (mkIdent `ptnum) (mkIdent `primnum) (mkIdent `vtxnum)
  vexFuncDeclToCommand fn ctx
  
end VEX
