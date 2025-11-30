import HouLean.Python.Grammar

open Lean Std

namespace HouLean.Python

structure TranslateCtx where
  introducedVars : HashSet Name := {}

structure TranslateState where
  elems : Array (TSyntax `doElem) := #[]

abbrev TranslateM := ReaderT TranslateCtx (StateT TranslateState MacroM)


def emit (elem : TSyntax `doElem) : TranslateM Unit :=
  modify fun s => { s with elems := s.elems.push elem }

def emitAll (elems : Array (TSyntax `doElem)) : TranslateM Unit :=
  modify fun s => { s with elems := s.elems ++ elems }

def withVar (name : Name) (m : TranslateM α) : TranslateM α := do
  let ctx ← read
  withTheReader TranslateCtx (fun _ => { ctx with introducedVars := ctx.introducedVars.insert name }) m

def withVars (names : List Name) (m : TranslateM α) : TranslateM α :=
  names.foldr withVar m

def isVarIntroduced (name : Name) : TranslateM Bool := do
  let ctx ← read
  return ctx.introducedVars.contains name

/-- Generate projection for index `idx` out of `total` elements in a nested tuple
(a, b, c) is (a, (b, c)) so: .1 = a, .2.1 = b, .2.2 = c -/
def mkNestedProjection (tmp : Term) (idx : Nat) (total : Nat) : MacroM Term := do
  if total == 1 then
    return tmp
  else if idx == 0 then
    `($tmp.1)
  else
    let mut result : Term ← `($tmp.2)
    for _ in [1:idx] do
      result ← `($result.2)
    if idx < total - 1 then
      result ← `($result.1)
    return result


/-- Generate flat numeric projection .1, .2, .3, etc. (for structs) -/
def mkFlatProjection (tmp : Term) (idx : Nat) : MacroM Term := do
  let fieldIdx := Lean.Syntax.mkNumLit (toString (idx + 1))
  return ⟨Lean.mkNode ``Lean.Parser.Term.proj #[tmp, Lean.mkAtom ".", fieldIdx]⟩

open Meta Elab Term in
elab "maybe_pure% " t:term : term <= expectedType? => do
  let e ← elabTerm t expectedType?
  let type ← inferType e
  if ← isMonadApp type then
    return e
  else
    elabTerm (← `(pure $t)) none


open Meta Elab Term in
elab "python_fun_app% " fn:term args:term*  : term  => do
  elabTerm (← `(maybe_pure% ($fn $args*))) none

-- open Meta Elab Term in
-- elab "python_eval_expr% " e  : term  => do
--   elabTerm (← `(maybe_pure% ($fn $args*))) none

partial def translatePyExpr (stx : Syntax) : TranslateM Term := do
  withRef stx do
    match stx with
    | `(pyExpr| $n:num) => return n
    | `(pyExpr| $s:str) => return s
    | `(pyExpr| $i:ident) => withRef i (pure i)
    -- Boolean and None literals
    | `(pyExpr| True) => `(true)
    | `(pyExpr| False) => `(false)
    | `(pyExpr| None) => `(none)
    -- Binary operations
    | `(pyExpr| $e1 + $e2) => do
        let e1' ← translatePyExpr e1
        let e2' ← translatePyExpr e2
        `($e1' + $e2')
    | `(pyExpr| $e1 - $e2) => do
        let e1' ← translatePyExpr e1
        let e2' ← translatePyExpr e2
        `($e1' - $e2')
    | `(pyExpr| $e1 * $e2) => do
        let e1' ← translatePyExpr e1
        let e2' ← translatePyExpr e2
        `($e1' * $e2')
    | `(pyExpr| $e1 / $e2) => do
        let e1' ← translatePyExpr e1
        let e2' ← translatePyExpr e2
        `($e1' / $e2')
    | `(pyExpr| $e1 % $e2) => do
        let e1' ← translatePyExpr e1
        let e2' ← translatePyExpr e2
        `($e1' % $e2')
    | `(pyExpr| $e1 ** $e2) => do
        let e1' ← translatePyExpr e1
        let e2' ← translatePyExpr e2
        `($e1' ^ $e2')
    | `(pyExpr| - $e) => do
        let e' ← translatePyExpr e
        `(-$e')
    -- Comparison operations
    | `(pyExpr| $e1 == $e2) => do
        let e1' ← translatePyExpr e1
        let e2' ← translatePyExpr e2
        `($e1' == $e2')
    | `(pyExpr| $e1 != $e2) => do
        let e1' ← translatePyExpr e1
        let e2' ← translatePyExpr e2
        `($e1' != $e2')
    | `(pyExpr| $e1 < $e2) => do
        let e1' ← translatePyExpr e1
        let e2' ← translatePyExpr e2
        `($e1' < $e2')
    | `(pyExpr| $e1 > $e2) => do
        let e1' ← translatePyExpr e1
        let e2' ← translatePyExpr e2
        `($e1' > $e2')
    | `(pyExpr| $e1 <= $e2) => do
        let e1' ← translatePyExpr e1
        let e2' ← translatePyExpr e2
        `($e1' <= $e2')
    | `(pyExpr| $e1 >= $e2) => do
        let e1' ← translatePyExpr e1
        let e2' ← translatePyExpr e2
        `($e1' >= $e2')
    | `(pyExpr| $e1 in $e2) => do
        let e1' ← translatePyExpr e1
        let e2' ← translatePyExpr e2
        `(Membership.mem $e2' $e1')
    -- Boolean operations
    | `(pyExpr| $e1 and $e2) => do
        let e1' ← translatePyExpr e1
        let e2' ← translatePyExpr e2
        `($e1' && $e2')
    | `(pyExpr| $e1 or $e2) => do
        let e1' ← translatePyExpr e1
        let e2' ← translatePyExpr e2
        `($e1' || $e2')
    | `(pyExpr| not $e) => do
        let e' ← translatePyExpr e
        `(!$e')
    -- -- Ternary conditional
    -- | `(pyExpr| $thenE if $cond else $elseE) => do
    --     let cond' ← translatePyExpr cond
    --     let then' ← translatePyExpr thenE
    --     let else' ← translatePyExpr elseE
    --     `(if $cond' then $then' else $else')
    -- Function calls
    | `(pyExpr| $f($args,*)) => do
        let f' ← translatePyExpr f
        -- let args' : Array Syntax ← args.getElems.mapM fun arg => do
        --   match arg with
        --   | `(pyArg| $name:ident = $e:pyExpr) => do
        --     let e' ← translatePyExpr e
        --     `(Lean.Parser.Term.namedArgument| ($name:ident := $e'))
        --   | `(pyArg| $e:pyExpr) => translatePyExpr e
        --   | _ => Macro.throwUnsupported
        -- let args' : Array Term := args'.map (fun arg => ⟨arg⟩)
        let args' ← args.getElems.mapM translatePyExpr
        `(← (python_fun_app% $f' $args'*))
    -- Lambda expressions
    | `(pyExpr| lambda $params,* : $body) => do
        let body' ← translatePyExpr body
        `(fun $params* => do return ($body':term))
    -- List literals
    | `(pyExpr| [ $es,* ]) => do
        let es' ← es.getElems.mapM translatePyExpr
        `([$es',*])
    -- List comprehension: [expr for x in iter]
    | `(pyExpr| [ $expr for $x:ident in $iter $[if $cond:pyExpr]? ]) => do
        let expr' ← translatePyExpr expr
        let iter' ← translatePyExpr iter
        let cond?' ← cond.mapM translatePyExpr
        if let some cond' := cond?' then
          `(List.filterMap (fun $x => if $cond' then some $expr' else none) $iter')
        else
          `(List.map (fun $x => $expr') $iter')
    -- Indexing
    | `(pyExpr| $e1[$e2]) => do
        let e1' ← translatePyExpr e1
        let e2' ← translatePyExpr e2
        `($e1'[$e2']!)
    -- -- Attribute access
    | `(pyExpr| $e.$field:ident) => do
        let e' ← withRef e (translatePyExpr e)
        `($e'.$field)
    -- Parenthesized expression
    | `(pyExpr| ( $e )) => translatePyExpr e
    -- Tuple literal
    | `(pyExpr| ( $e , $es,* )) => do
        let e' ← translatePyExpr e
        let es' ← es.getElems.mapM translatePyExpr
        `(($e', $es',*))
    | _ =>
      Macro.throwErrorAt stx s!"failed to find case for {stx}"

mutual

partial def translateStmts (stmts : List (TSyntax `pyStmt)) : TranslateM Unit := do
  match stmts with
  | [] => pure ()
  | stmt :: rest => do
      match stmt with
      -- Expression statement
      | `(pyStmt| $e:pyExpr) => do
          let e' ← translatePyExpr e
          emit (← withRef stmt `(doElem| let _ ← (pure ($e':term))))
          translateStmts rest

      -- Assignment with an optional type annotation
      | `(pyStmt| $i:ident $[: $t:term]? = $e:pyExpr) => do
          let e' ← translatePyExpr e
          let varName := i.getId
          let introduced ← isVarIntroduced varName
          if introduced then
            emit (← withRef stmt `(doElem| $i:term $[: $t]? := $e'))
            translateStmts rest
          else
            emit (← withRef stmt `(doElem| let mut $i $[: $t]? := $e'))
            withVar varName (translateStmts rest)

      -- Tuple unpacking assignment
      | `(pyStmt| $i:ident , $is,* = $e:pyExpr) => do
          let e' ← translatePyExpr e
          let allIdents := #[i] ++ is.getElems
          let n := allIdents.size
          let tmp ← `(_pyTmp)
          emit (← withRef stmt `(doElem| let $tmp:term := $e'))
          let mut newVars : List Name := []
          for h : idx in [0:n] do
            let ident := allIdents[idx]
            let proj ← mkNestedProjection tmp idx n
            let varName := ident.getId
            let introduced ← isVarIntroduced varName
            let justIntroduced := newVars.contains varName
            if introduced || justIntroduced then
              emit (← `(doElem| $ident:ident := $proj))
            else
              emit (← `(doElem| let mut $ident:ident := $proj))
              newVars := varName :: newVars
          withVars newVars (translateStmts rest)

      -- Struct unpacking with numeric projection {a, b, c} = expr
      | `(pyStmt| { $is,* } = $e:pyExpr) => do
          let e' ← translatePyExpr e
          let allIdents := is.getElems
          let tmp ← `(_pyTmp)
          emit (← withRef stmt `(doElem| let $tmp:term := $e'))
          let mut newVars : List Name := []
          for h : idx in [0:allIdents.size] do
            let ident := allIdents[idx]
            let proj ← mkFlatProjection tmp idx
            let varName := ident.getId
            let introduced ← isVarIntroduced varName
            let justIntroduced := newVars.contains varName
            if introduced || justIntroduced then
              emit (← `(doElem| $ident:ident := $proj))
            else
              emit (← `(doElem| let mut $ident:ident := $proj))
              newVars := varName :: newVars
          withVars newVars (translateStmts rest)

      -- Augmented assignments (unified via helper)
      | `(pyStmt| $i:ident += $e:pyExpr) =>
          translateAugAssign stmt i e (fun a b => `($a + $b)) rest
      | `(pyStmt| $i:ident -= $e:pyExpr) =>
          translateAugAssign stmt i e (fun a b => `($a - $b)) rest
      | `(pyStmt| $i:ident *= $e:pyExpr) =>
          translateAugAssign stmt i e (fun a b => `($a * $b)) rest
      | `(pyStmt| $i:ident /= $e:pyExpr) =>
          translateAugAssign stmt i e (fun a b => `($a / $b)) rest
      | `(pyStmt| $i:ident %= $e:pyExpr) =>
          translateAugAssign stmt i e (fun a b => `($a % $b)) rest

      -- If statement
      | `(pyStmt| if $cond:pyExpr : $tt:pyBlock) => do
          let cond' ← translatePyExpr cond
          let then' ← translatePyBlock tt
          emit (← withRef stmt `(doElem| if $cond' then do $[$then':doElem]*))
          translateStmts rest

      | `(pyStmt| if $cond:pyExpr : $t:pyBlock
                  else : $e:pyBlock) => do
          let cond' ← translatePyExpr cond
          let then' ← translatePyBlock t
          let else' ← translatePyBlock e
          emit (← withRef stmt `(doElem| if $cond' then do $[$then':doElem]* else do $[$else':doElem]*))
          translateStmts rest

      -- -- If-elif chain (no final else)
      -- | `(pyStmt| if $cond:pyExpr : $t:pyBlock $elifs:pyElif*) => do
      --     let cond' ← translatePyExpr cond
      --     let then' ← translatePyBlock t
      --     let mut elseBody : Array (TSyntax ``doElem) ← pure #[← `(doElem| pure ())]
      --     for elif in elifs.reverse do
      --       let `(pyElif| elif $elifCond:pyExpr : $elifThen:pyBlock) := elif | Macro.throwUnsupported
      --       let elifCond' ← translatePyExpr elifCond
      --       let elifThen' ← translatePyBlock elifThen
      --       elseBody ← pure #[← `(doElem| if $elifCond' then do $elifThen'* else do $elseBody*)]
      --     emit (← withRef stmt `(doElem| if $cond' then do $then'* else do $elseBody*))
      --     translateStmts rest

      -- -- If-elif chain with final else
      -- | `(pyStmt| if $cond:pyExpr : $then:pyBlock $elifs:pyElif* else : $else:pyBlock) => do
      --     let cond' ← translatePyExpr cond
      --     let then' ← translatePyBlock then
      --     let else' ← translatePyBlock else
      --     let mut elseBody := else'
      --     for elif in elifs.reverse do
      --       let `(pyElif| elif $elifCond:pyExpr : $elifThen:pyBlock) := elif | Macro.throwUnsupported
      --       let elifCond' ← translatePyExpr elifCond
      --       let elifThen' ← translatePyBlock elifThen
      --       elseBody ← pure #[← `(doElem| if $elifCond' then do $elifThen'* else do $elseBody*)]
      --     emit (← withRef stmt `(doElem| if $cond' then do $then'* else do $elseBody*))
      --     translateStmts rest

      -- While loop
      | `(pyStmt| while $cond:pyExpr : $body:pyBlock) => do
          let cond' ← translatePyExpr cond
          let body' ← translatePyBlock body
          emit (← withRef stmt `(doElem| while $cond' do $[$body':doElem]*))
          translateStmts rest

      -- For loop
      | `(pyStmt| for $i:ident in $iter:pyExpr : $body:pyBlock) => do
          let iter' ← translatePyExpr iter
          let varName := i.getId
          let body' ← withVar varName (translatePyBlock body)
          emit (← withRef stmt `(doElem| for $i:term in $iter' do $[$body':doElem]*))
          translateStmts rest

      -- Function definition
      | `(pyStmt| def $name:ident ( $params,* ) : $body:pyBlock) => do
          let body' : Array (TSyntax `doElem) ← translatePyBlock body
          let paramSyntax : Array (TSyntax ``Parser.Term.funBinder) ← params.getElems.mapM fun p => do
            match p with
            -- | `(pyParam| $i:ident $[: $t:term]? = $e:pyExpr) => do
            --     let e' ← translatePyExpr e
            --     `(funBinder| ($i : $t := $e'))
            -- | `(pyParam| $i:ident = $e:pyExpr) => do
            --     let e' ← translatePyExpr e
            --     `(($i:ident := $e':term))
            | `(pyParam| $i:ident : $t:term) => `(($i : $t))
            | `(pyParam| $i:ident) => `($i)
            | _ => Macro.throwUnsupported
          emit (← withRef stmt `(doElem| let $name := fun $paramSyntax* => do $[$body':doElem]*))
          withVar name.getId (translateStmts rest)

      -- Return statement
      | `(pyStmt| return $e:pyExpr) => do
          let e' ← translatePyExpr e
          emit (← withRef stmt `(doElem| return $e'))
          translateStmts rest

      -- Return multiple values (tuple)
      | `(pyStmt| return $e:pyExpr , $es,* ) => do
          let e' ← translatePyExpr e
          let es' ← es.getElems.mapM translatePyExpr
          let tuple ← `(($e', $es',*))
          emit (← withRef stmt `(doElem| return $tuple))
          translateStmts rest

      | `(pyStmt| return) => do
          emit (← withRef stmt `(doElem| return ()))
          translateStmts rest

      -- Break statement
      | `(pyStmt| break) => do
          emit (← withRef stmt `(doElem| break))
          translateStmts rest

      -- Continue statement
      | `(pyStmt| continue) => do
          emit (← withRef stmt `(doElem| continue))
          translateStmts rest

      -- Pass statement
      | `(pyStmt| pass) => do
          emit (← withRef stmt `(doElem| pure ()))
          translateStmts rest

      -- Print statement
      -- | `(pyStmt| print ( $args,* )) => do
      --     let args' ← args.getElems.mapM translatePyExpr
      --     let elem ← match args' with
      --       | [] => withRef stmt `(doElem| IO.println "")
      --       | [a] => withRef stmt `(doElem| IO.println $a)
      --       | _ =>
      --         let formatted ← args'.foldlM (init := ← `("")) fun acc arg => `($acc ++ " " ++ toString $arg)
      --         withRef stmt `(doElem| IO.println $formatted)
      --     emit elem
      --     translateStmts rest

      | _ => Macro.throwUnsupported
where
  /-- Helper for augmented assignment (+=, -=, *=, /=, %=) -/
  translateAugAssign (stmt : Syntax) (i : Ident) (e : TSyntax `pyExpr)
      (mkOp : Term → Term → MacroM Term) (rest : List (TSyntax `pyStmt)) : TranslateM Unit := do
    let e' ← translatePyExpr e
    let varName := i.getId
    let introduced ← isVarIntroduced varName
    if introduced then
      let rhs ← mkOp i e'
      emit (← withRef stmt `(doElem| $i:ident := $rhs:term))
      translateStmts rest
    else
      emit (← withRef i `(doElem| let mut $i := default))
      let rhs ← mkOp i e'
      emit (← withRef stmt `(doElem| $i:ident := $rhs:term))
      withVar varName (translateStmts rest)


partial def translatePyBlock (stx : Syntax) : TranslateM (Array (TSyntax `doElem)) := do
  let `(pyBlock| $stmts:pyStmt*) := stx | Macro.throwUnsupported
  -- Run with fresh state, preserving reader context
  let ctx ← read
  let (_, state) ← (translateStmts stmts.toList).run ctx |>.run {}
  return state.elems

end

open Lean Macro in
def translatePython (block : TSyntax `pyBlock) : MacroM Term := do
  let (elems, _) ← (translatePyBlock block).run {} |>.run {}
  withRef block `(term| do $[$elems:doElem]*)

syntax withPosition("python%" pyBlock) : term
macro "python%" block:pyBlock : term =>
  translatePython block


elab "#python_translation " block:pyBlock : command => do
  let stx ← Elab.liftMacroM <| translatePython block
  logInfo stx
