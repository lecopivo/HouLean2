import Lean
open Lean

namespace HouLean.Meta

/--
Declare an overloadable function by creating a single-method typeclass.

## Example
```lean
declfun sin {α} (x : α) : α
```

This creates a typeclass `Sin` with a single method `sin`. You can then define
overloads using `defun`:

```lean
defun sin (x : Float) := x.sin
defun sin (x : Float32) := x.sin
defun Vector2.sin (u : Vector2) : Vector2 := ⟨u.x.sin, u.y.sin⟩
```

## Implementation Details
Internally, `declfun` creates a typeclass and exports its method. For example,
`declfun sin {α} (x : α) : α` generates:
```lean
class Sin (α : Type) where
  sin (x : α) : α
export Sin (sin)
```
-/
syntax (docComment)? "declfun" declId bracketedBinder* ":" term : command

/--
Define an overload for a function declared with `declfun`.

## Example
```lean
-- Simple overload
defun sin (x : Float) := x.sin

-- Member function (automatically creates both the function and the overload instance)
defun Vector2.sin (u : Vector2) : Vector2 := ⟨u.x.sin, u.y.sin⟩
```

## Behavior
- For simple names like `sin`, `defun` creates an instance of the corresponding typeclass
- For qualified names like `Vector2.sin`, it defines the member function and creates an instance

## Implementation Details
`defun` generates typeclass instances. For example, `defun sin (x : Float) := x.sin`
creates:
```lean
instance : Sin Float where
  sin x := x.sin
```
-/
syntax (docComment)? "defun" declId bracketedBinder* (":" term)? ":=" term : command

open Lean.Parser.Term in
/-- Extract leading implicit and instance implicit binders from a binder array. -/
def extractImplicitBinders (binders : TSyntaxArray ``bracketedBinder) : 
    Array (TSyntax ``bracketedBinder) × Array (TSyntax ``bracketedBinder) := 
  let implicit := binders.takeWhile fun b : TSyntax ``bracketedBinder => 
    match b with
    | `(bracketedBinder| { $_ }) => true
    | `(bracketedBinder| [ $_ ]) => true
    | _ => false
  (implicit, binders[implicit.size:])

open Lean.Parser.Term in
/-- Convert implicit binders `{x}` to explicit binders `(x)`. -/
def implicitBindersToExplicit (binders : TSyntaxArray ``bracketedBinder) : 
    MacroM (TSyntaxArray ``bracketedBinder) := 
  binders.mapM fun b : TSyntax ``bracketedBinder => 
    match b with
    | `(bracketedBinder| { $x }) => `(bracketedBinder| ( $x ))
    | b => pure b

open Lean Elab Command in
elab_rules : command
| `($[$doc:docComment]? declfun $id:ident $bs:bracketedBinder* : $ty:term) => do
  let className := mkIdent <| id.getId.capitalize
  let (classBinders, funBinders) := extractImplicitBinders bs
  let classBinders ← liftMacroM <| implicitBindersToExplicit classBinders

  elabCommand (← `(class $className:ident $classBinders* where
      $[$doc:docComment]?
      $id:ident $funBinders:bracketedBinder* : $ty))
  
  elabCommand (← `(export $className ($id)))

open Meta Elab Command Term in
elab_rules : command
| `($[$doc:docComment]? defun $id:ident $bs:bracketedBinder* $[: $ty:term]? := $body) => do
  liftTermElabM do
    unless id.getId.isStr do
      throwError "invalid function name {id}"
    
    let funName := id.getId.getString!
    let className := funName.capitalize
    let funId ← resolveGlobalConstNoOverload (mkIdent <| Name.mkSimple className |>.append (.mkSimple funName))
    let classId ← resolveGlobalConstNoOverload (mkIdent <| Name.mkSimple className)

    -- Elaborate function definition
    let f ← Term.elabBinders bs fun xs => do
      let t? ← ty.mapM (elabTerm · none)
      let b ← elabTermAndSynthesize body t?
      mkLambdaFVars xs b
    let F ← inferType f >>= instantiateMVars
    
    if F.hasLevelMVar then
      throwError "Universe polymorphic functions are not supported yet!"

    -- Prepare function declaration with mvars
    let funExpr ← mkConstWithFreshMVarLevels funId
    let classExpr ← mkConstWithFreshMVarLevels classId
    let (ys, _, _) ← forallMetaTelescope (← inferType classExpr)
    let classExpr := classExpr.beta ys
    let inst ← mkFreshExprMVar classExpr
    let funExpr := funExpr.beta (ys.push inst)
    let funType ← inferType funExpr

    -- Overload should have the same type, which fills type mvars
    if ← isDefEq funType F then
      let classExpr ← instantiateMVars classExpr
      let mut inst ← mkAppM (classId.append `mk) #[f] >>= instantiateMVars

      let ns ← getCurrNamespace
      let strName :=
        if id.getId.getNumParts > 1 then
          id.getId.getPrefix
        else
          ns
    
      -- Handle member function case
      if (← getEnv).contains strName then
        -- Resolve namespace properly
        let funName := id.getId.getString!
        let strId ← resolveGlobalConstNoOverload (mkIdent strName)
        let declId := strId.append (.mkSimple funName)
        let hints := ReducibilityHints.regular (getMaxHeight (← getEnv) f + 1)
        let decl ← Lean.mkDefinitionValInferrringUnsafe declId [] F f hints
        
        -- Add documentation if provided
        match doc with
        | some doc => 
          addDecl (Declaration.defnDecl decl)
          addDocString declId doc
          compileDecl (Declaration.defnDecl decl)
        | none =>
          addAndCompile (Declaration.defnDecl decl)
        
        inst ← mkAppM (classId.append `mk) #[(.const declId [])]

      -- Generate and add instance
      let instName := (← getCurrNamespace) ++ (← NameGen.mkBaseNameWithSuffix "inst" classExpr)
      if (←getEnv).contains instName then
        throwError "Enviroment already contains {instName}"
      let hints := ReducibilityHints.regular (getMaxHeight (← getEnv) inst + 1)
      let decl ← Lean.mkDefinitionValInferrringUnsafe instName [] classExpr inst hints
      addAndCompile (Declaration.defnDecl decl)
      addInstance instName AttributeKind.global (eval_prio default)
    else
      throwError m!"Invalid function type {F}, expected {funType}!"

end HouLean.Meta
