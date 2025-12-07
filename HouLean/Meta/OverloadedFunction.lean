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

open Lean Meta Elab Parser Term Command PrettyPrinter in
elab_rules : command
| `($[$doc:docComment]? declfun $id:ident $bs:bracketedBinder* : $ty:term) => do

  let (clsBinders, funBinders) ←
    runTermElabM fun ctx => do
    Term.elabBinders bs fun xs => do
    let r ← elabType ty

    let ts ← liftM <| xs.mapM inferType
    let ts := ts.push r
    let ctx' := ctx.filter (fun c => ts.any (fun x => x.containsFVar c.fvarId!))

    let mut clsBinders : Array (TSyntax ``bracketedBinder) := #[]
    let mut funBinders : Array (TSyntax ``bracketedBinder) := #[]

    for x in ctx' ++ xs do
      let t ← inferType x >>= Lean.Elab.Term.levelMVarToParam
      let n ← x.fvarId!.getUserName
      let ty ← delab t
      let id := mkIdent n
      match ← x.fvarId!.getBinderInfo with
      | .default =>
        funBinders := funBinders.push (← `(bracketedBinder| ($id : $ty)))
      | .implicit =>
        clsBinders := clsBinders.push (← `(bracketedBinder| ($id : $ty)))
      | .instImplicit =>
        clsBinders := clsBinders.push (← `(bracketedBinder| [$ty]))
      | .strictImplicit =>
        clsBinders := clsBinders.push (← `(bracketedBinder| {$id : $ty}))
    return (clsBinders, funBinders)
  let className := mkIdent <| id.getId.capitalize
  let cmd ← `(class $className:ident $clsBinders* where
      $[$doc:docComment]?
      $id:ident $funBinders:bracketedBinder* : $ty)
  elabCommand cmd

  elabCommand (← `(export $className ($id)))

open Lean Meta Elab Command Term in
elab_rules : command
| `($[$doc:docComment]? defun $id:ident $bs:bracketedBinder* $[: $ty:term]? := $body) => do

  runTermElabM fun _ctx => do
  Term.elabBinders bs fun xs => do

  let type? ← ty.mapM elabType
  let body ← elabTermAndSynthesize body type?

  let xs' ← xs.filterM (fun x => (·.isExplicit) <$> x.fvarId!.getBinderInfo)
  let f ← mkLambdaFVars xs' body

  let className := id.getId.getString!.capitalize
  let className ← resolveGlobalConstNoOverload (mkIdent (.mkSimple className))
  let inst ← mkAppM (className.append `mk) #[f]
  let ys := (← inst.collectFVars.run {}).2.fvarIds.map Expr.fvar
  let inst ← mkLambdaFVars ys inst >>= instantiateMVars
  let f ← mkLambdaFVars ys f >>= instantiateMVars

  let ns ← getCurrNamespace
  let strName :=
    if id.getId.getNumParts > 1 then
      id.getId.getPrefix
    else
      ns

  -- Handle member function case
  if (← getEnv).contains strName then
    -- Resolve namespace properly
    let F ← inferType f
    let funName := id.getId.getString!
    let strId ← resolveGlobalConstNoOverload (mkIdent strName)
    let declId := strId.append (.mkSimple funName)
    let hints := ReducibilityHints.regular (getMaxHeight (← getEnv) f + 1)
    let decl ← Lean.mkDefinitionValInferringUnsafe declId [] F f hints
    addDeclarationRangesFromSyntax declId id

    -- Add documentation if provided
    match doc with
    | some doc =>
      addDecl (Declaration.defnDecl decl)
      addDocString declId (mkNullNode bs) doc
      compileDecl (Declaration.defnDecl decl)
    | none =>
      addAndCompile (Declaration.defnDecl decl)

  -- Generate and add instance
  let classExpr ← inferType inst
  let instName := (← getCurrNamespace) ++ (← NameGen.mkBaseNameWithSuffix "inst" classExpr)
  if (←getEnv).contains instName then
    throwError "Enviroment already contains {instName}"
  let hints := ReducibilityHints.regular (getMaxHeight (← getEnv) inst + 1)
  let decl ← Lean.mkDefinitionValInferringUnsafe instName [] classExpr inst hints
  addAndCompile (Declaration.defnDecl decl)
  addInstance instName AttributeKind.global (eval_prio default)
  addDeclarationRangesFromSyntax instName id


end HouLean.Meta
