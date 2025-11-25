import HouLean.Apex.Compile.Graph
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Apex.Lean.ForIn
import HouLean.Apex.Lean.Ite
import HouLean.Apex.Compile.Meta
import HouLean.Init

open Lean Meta Std

namespace HouLean.Apex.Compiler

structure Context where
  -- recDepth : Nat
  -- maxRecDepth : Nat := 100
  unfoldStack : List Name -- todo: store recursion depth too and probably use it to report errors
deriving Inhabited

structure GraphState where
  /-- Currently built graph -/
  graph : ApexGraph
  /-- Mapping from local fvars to ports -/
  fvarToPort : HashMap FVarId PortBundle
  /-- Counts how many times we added a node with particular name -/
  nameCounter : HashMap String Nat
  /-- Cache output ports for expressions -/
  cache : ExprMap (Array PortBundle × PortBundle)
deriving Inhabited

abbrev GraphCompileM := ReaderT Context <| StateT GraphState MetaM

-- def withIncRecDepth {α} (go : GraphCompileM α) : GraphCompileM α := do
--   throwMaxRecDepthAt (← getRef)


def withUnfolded {α} (name : Name) (go : GraphCompileM α) : GraphCompileM α := do
--   Meta.withIncRecDepth do
--     sorry
  fun ctx => go {ctx with unfoldStack := name :: ctx.unfoldStack}

def GraphCompileM.run {α} (go : GraphCompileM α)
    (c : Context := default) (s : GraphState := default) : MetaM α := do
  return (← go c s).1

def GraphCompileM.run' {α} (go : GraphCompileM α) (c : Context := default)
    (s : GraphState := default) : MetaM ApexGraph := do
  return (← go c s).2.graph

def modifyGraph {α} (f : ApexGraph → MetaM (ApexGraph × α)) : GraphCompileM α := do
  let s ← get
  let (g, val) ← f s.graph
  set {s with graph := g}
  return val

def readGraph {α} (f : ApexGraph → MetaM α) : GraphCompileM α := do
  return (← f (← get).graph)

def getPortName (portId : Nat) : GraphCompileM Name := do
  readGraph (fun g => return g.ports[portId]!.name)

def getPortType (portId : Nat) : GraphCompileM PortType := do
  readGraph (fun g => return g.ports[portId]!.type)

def getFreshNodeName (name : String) : GraphCompileM String := do
  let nameCounter := (← get).nameCounter
  if let some i := nameCounter[name]? then
    modify (fun s => {s with nameCounter := nameCounter.insert name (i+1)})
    return name ++ toString i
  else
    modify (fun s => {s with nameCounter := nameCounter.insert name 1})
    return name

def getUnfoldStackMsg : GraphCompileM MessageData := do
  let stack := (← read).unfoldStack
  if stack.isEmpty then
    return m!""
  else
    let s := stack.toArray.joinl (map:=toString) (· ++ " → " ++ ·)
    return m!", unfolded: {s}"

def throwErrorWithStack (msg : MessageData) : GraphCompileM α := do
  throwErrorAt (← getRef) m!"{msg}\n{← getUnfoldStackMsg}"

/-- Add a node to the graph with optional subports -/
def addNode (nodeType : NodeType)  (nodeName : Name) (argPorts : Array PortBundle) :
    GraphCompileM (struct {nodeId : Nat, inputs : Array PortBundle, output : PortBundle}) := do
  let userName := toString nodeName.eraseMacroScopes
  let userName ← getFreshNodeName userName
  modifyGraph (fun g => do
     let r ← g.addNode nodeType userName argPorts
     pure <| (r.graph, struct r pop% graph))

def addDefaultGeometry : GraphCompileM PortBundle := do
  let some t ← getNodeType (.const ``Generated.ValueGeometry [])
    | throwError "APEX compiler bug in {decl_name%}!"
  return (← addNode t `empty_geometry #[]).output

def makeSingleConnection (src trg : PortPtr) : GraphCompileM Unit := do
  modifyGraph (fun g => do
    let g ← g.addConnection src trg
    return (g,()))

def isVariadic (p : PortPtr) : GraphCompileM Bool := do
  match p with
  | .port id =>
    let p : Port := (← get).graph.ports[id]!
    return p.isVariadic
  | .subport .. =>
    return true
  | .input id =>
    let t := (← get).graph.inputs[id]!.1
    match t.type with
    | .variadic .. => return true
    | _ => return false
  | .output .. => return false
  | .literal .. => return false

def makeConnection (src trg : PortBundle) : GraphCompileM Unit := do
  trace[HouLean.Apex.compiler] m!"Making connection {src.toString} → {trg.toString}"
  match src, trg with
  | .leaf s, .leaf t => makeSingleConnection s t
  | .node as, .node bs =>
    if as.size != bs.size then
      throwError "Can't make a connection {src.toString} → {trg.toString}, invalid shape. case different size"
    else do
      for a in as, b in bs do
        makeConnection a b
  | .node .., .leaf (.port p) =>
    if ← isVariadic (.port p) then
      let xs := src.flatten
      for x in xs, i in [0:xs.size] do
        makeSingleConnection x (.subport p i)
    else
      throwError "Can't make a connection {src.toString} → {trg.toString}, invalid shape. case node, leaf"
  | .leaf _, .node _ =>
    throwError "Can't make a connection {src.toString} → {trg.toString}, invalid shape. case left, node"
  | _, _ =>
    throwError "Can't make a connection {src.toString} → {trg.toString}, invalid shape."

def addInputPort (name : Name) (type : PortType) : GraphCompileM Nat :=
  modifyGraph (fun g => pure (g.addInput name type))

/-- Add a free variable node -/
def addFVar (var : FVarId) : GraphCompileM PortBundle := do
  let some type ← getApexType? (← var.getType) (← var.getUserName)
    | throwError m!"invalid input type {← var.getType}"
  let portBundle : PortBundle ← type.mapIdxM (fun _ (name,type) => do
    let inputId ← addInputPort name (.builtin type)
    return (.input inputId))
  modify (fun s =>
    {s with
      fvarToPort := s.fvarToPort.insert var portBundle})
  return portBundle


instance : ExceptToEmoji Exception (Array PortBundle × PortBundle) where
  toEmoji r :=
    match r with
    | .ok _ => s!"✅️"
    | .error _ => "💥️"

private def _root_.Lean.isInstanceProjection (name : Name) : MetaM Bool := do
  unless ← isProjectionFn name do return false
  let some info ← getProjectionFnInfo? name | return false
  unless info.fromClass do return false
  return true


/-- If `e` is a match expression then all discriminants are turned into a let bindings
if they are not free variable already.

For example the expression
```
let (x,y) := f x
x + y
```
gets elaborated to
```
match f x with
| (x,y) => x + y
```
and running `whnf` with `iota := true` and `zeta := false` turns it into
```
(f x).1 + (f x).2
```
which is undesirable. Therefore this function turns
```
match f x with
| (x,y) => x + y
```
into
```
let r := f x
match r with
| (x,y) => x + y
```
and now running whnf on the let body will produce
```
let r := f x
r.1 + r.2
```
which preserves the let binding of the original expression

 -/
partial def letBindMatchDiscrs (e : Expr) (doUnfold := false) : MetaM Expr := do
  let some info := isMatcherAppCore? (← getEnv) e
    | return e
  let (fn, args) := e.withApp (fun fn args => (fn,args))
  go fn args info 0 #[]
where
  go (fn : Expr) (args : Array Expr) (info : MatcherInfo) (i : Nat) (xs : Array Expr) : MetaM Expr := do
    if i = info.numDiscrs then
      let mut e := fn.beta args
      if doUnfold then
        -- todo: maybe check that the proof of unfolding is rfl as we use it int whnf
        e := (← unfold e fn.constName!).expr
      return ← mkLambdaFVars xs e
    else
      let xi := args[i + info.numParams + 1]!
      if xi.isFVar then
        -- do nothing if `xi` is already fvar
        go fn args info (i+1) xs
      else
        let name := Name.appendAfter `r (toString i)
        withLetDecl name (← inferType xi) xi fun xvar => do
          let args := args.set! (i + info.numParams + 1) xvar
          let xs := xs.push xvar
          go fn args info (i+1) xs

def doUnfold (e : Expr) : MetaM Bool := do
  let .const name _ := e.getAppFn | return false
  if ← isReducible name then return true
  unless ← isInstanceProjection name do return false
  let s := compilerExt.getState (← getEnv)
  if name == ``Id then
    return true
  if s.implementedByName.contains name then
    return false
  else
    return true

def betaThroughLet (e : Expr) : MetaM Expr := do
  let (fn, args) := e.withApp (fun fn args => (fn,args))
  let e' ← letTelescope fn (preserveNondepLet := false) fun xs b =>
    mkLambdaFVars xs (b.beta args)
  return e'

def letBind (e : Expr) : MetaM Expr := do
  if e.isAppOfArity' ``Bind.bind 6 then
    let (fn,args) := e.withApp (fun fn args => (fn,args))
    let x := args[4]!
    if x.isFVar then
      return e
    else
      let args := args.set! 4 (.bvar 0)
      let name :=
        if let .lam n .. := args[5]! then
          n
        else
          `tmp
      return .letE name (← inferType x) x (mkAppN fn args) true
  return e

/-- Specially configured `whnf` for APEX compilation.

At its core it is whnf at reducible and instances with `iota := true` and `zeta := false`

But certain instance projections are overriden by `apex_implemented_by` so those do not
get reduced. Also match statements hoist their discriminants into let binders before
reducing them.

TODO: this needs complete rework as I want (fun x => x * x) (a + b) to reduce to
`let tmp := a + b; tmp * tmp` but now it reduces to `(a + b) * (a + b)`
The main issue this is causing is compiling expressions with `bind` and
I just added a temporary fix
-/
partial def whnfC (e : Expr) : MetaM Expr :=
  withConfig (fun cfg => {cfg with iota := false, zeta := false, zetaDelta := false}) do
    let e' := e
    let e' ← letBind e'
    let e' ← whnfHeadPred e' doUnfold
    let e' ← letBindMatchDiscrs e' true
    if e.equal e' then
      return ← betaThroughLet e'
    else
      whnfC e'

open Qq in
unsafe def reduceToLiteralImpl (e : Expr) : MetaM (Option LiteralVal) := do
  if e.hasMVar ∨ e.hasFVar then
    return none

  try
    let t ← inferType e

    if ← isDefEq t q(Nat) then
      let val ← evalExpr Nat q(Nat) e
      return some (.int (Int.ofNat val))

    if ← isDefEq t q(Int) then
      let val ← evalExpr Int q(Int) e
      return some (.int val)

    if ← isDefEq t q(Float) then
      let val ← evalExpr Float q(Float) e
      return some (.float val)

    if ← isDefEq t q(String) then
      let val ← evalExpr String q(String) e
      return some (.str val)

    if ← isDefEq t q(Bool) then
      let val ← evalExpr Bool q(Bool) e
      return some (.bool val)

    if ← isDefEq t q(Vector2) then
      let val ← evalExpr Vector2 q(Vector2) e
      return some (.vector2 val)

    if ← isDefEq t q(Vector3) then
      let val ← evalExpr Vector3 q(Vector3) e
      return some (.vector3 val)

    if ← isDefEq t q(Vector4) then
      let val ← evalExpr Vector4 q(Vector4) e
      return some (.vector4 val)

    -- if ← isDefEq t q(Matrix2) then
    --   let val ← evalExpr Matrix2 q(Matrix2) e
    --   return some (.matrix2 val)

    if ← isDefEq t q(Matrix3) then
      let val ← evalExpr Matrix3 q(Matrix3) e
      return some (.matrix3 val)

    if ← isDefEq t q(Matrix4) then
      let val ← evalExpr Matrix4 q(Matrix4) e
      return some (.matrix4 val)

    return none
  catch _ =>
    return none

open Qq in
unsafe def reduceToBoolImpl (e : Expr) : MetaM (Option Bool) := do
  -- todo: do something more efficient, check if the term has no local free variables, let free variables are ok
  let e ← whnf e
  if e.hasMVar ∨ e.hasFVar then
    return none

  try
    let t ← inferType e

    if ← isDefEq t q(Bool) then
      let val ← evalExpr Bool q(Bool) e
      return some val

    return none
  catch _ =>
    return none

@[implemented_by reduceToLiteralImpl]
def tryReduceToLiteral (e : Expr) : MetaM (Option LiteralVal) := return none

@[implemented_by reduceToBoolImpl]
def tryReduceBool (e : Expr) : MetaM (Option Bool) := return none

def updateFVars (varsProd : Expr) (ports : PortBundle) : GraphCompileM Unit :=
  match varsProd, ports with
  | (.const ``Unit.unit _), _
  | (.const ``PUnit.unit _), _ => pure ()
  | .fvar id, ports =>
    modify fun s => {s with fvarToPort := s.fvarToPort.insert id ports}
  | mkApp2 (.const ``Prod.mk _) x y, .node #[xports, yports] => do
    updateFVars x xports
    updateFVars y yports
  | _, _ =>
    throwError m!"Can't update fvar ports, invalid vars, {varsProd} or port shapes {ports.toString}."

open Qq in
mutual

partial def compile (e : Expr) : GraphCompileM (Array PortBundle × PortBundle) := do
  let s ← get
  if let some output := s.cache[e]? then
    return output
  else
    let output ← toApexGraph e
    modify (fun s => {s with cache := s.cache.insert e output})
    return output

/-- Check if expression is a variadic argument and return its elements -/
partial def compileVariadicArg? (e : Expr) : GraphCompileM (Option (PortBundle)) := do
  unless e.isAppOfArity ``VariadicArg.cons 4 ||
         e.isAppOfArity ``VariadicArg.nil 2 do
    return none

  -- if e.isAppOfArity ``apexFlatten 4 then
  --   let e := e.getArg! 3
  --   let (_,r) ← compile e
  --   return some (.node (r.flatten.map ArrayTree.leaf))

  let t ← inferType e
  let n := (t.getArg! 1)
  let .lit (.natVal n) ← whnfD n
    | throwError m!"Size {n} of variadic argument must be known at compile time: {e}"

  let xs := (← split e).toArray

  unless xs.size == n do
    throwError m!"All elements of variadic argument must be known at compile time"

  let xs ← xs.mapM (fun x => do pure (← compile x).2)
  return some (.node xs)
where
  split (l : Expr) : MetaM (List Expr) := do
    match (← whnf l) with
    | .app (.const ``VariadicArg.nil _) _ => return []
    | mkApp4 (.const ``VariadicArg.cons _) _ _ x xs => return x :: (← split xs)
    | l => throwError m!"Invalid variadic argument {l} {l.getAppFn}"

/-- Compile a free variable reference -/
partial def compileVariable (id : FVarId) (e : Expr) :
    GraphCompileM (Array PortBundle × PortBundle) := do
  let some portBundle := (← get).fvarToPort[id]?
    | throwErrorWithStack m!"Unrecognized free variable: {e}"
  return (#[], portBundle)

/-- Compile literal values (Nat, String) -/
partial def compileLiteral (lit : Literal) :
    GraphCompileM (Array PortBundle × PortBundle) := do
  match lit with
  | .natVal n => return (#[], .leaf (.literal (.int n)))
  | .strVal s => return (#[], .leaf (.literal (.str s)))

/-- Compile Float literals -/
partial def compileFloat (m s e : Expr) :
    GraphCompileM (Array PortBundle × PortBundle) := do
  let .lit (.natVal m) ← whnf m
    | throwErrorWithStack s!"Invalid literal for mantissa: {m}"
  let .lit (.natVal e) ← whnf e
    | throwErrorWithStack s!"Invalid literal for exponent: {e}"
  let s := if ← isDefEq s q(true) then true else false
  let val := Float.ofScientific m s e
  return (#[], .leaf (.literal (.float val)))

/-- Compile Bool constants -/
partial def compileConstant (e : Expr) :
    GraphCompileM (Array PortBundle × PortBundle) := do
  if (← isDefEq e q(true)) then
    return (#[], .leaf (.literal (.bool true)))
  if (← isDefEq e q(false)) then
    return (#[], .leaf (.literal (.bool false)))
  if (← isDefEq e (.const `HouLean.Apex.Geometry.default [])) then
    return (#[], .leaf (.literal .empty_geometry))
  if (← isDefEq e (.const `HouLean.Apex.Dict.default [])) then
    return (#[], .leaf (.literal .empty_dict))

  -- discard inputs
  if let some (_inputs,output) ← tryCompileImplementedBy e #[] then
    return (#[], output)
  let (_inputs, output) ← compileNormalApp e #[] e
  return (#[],output)

/-- Compile match expressions -/
partial def compileMatchExpr (e : Expr) (info : MatcherInfo) :
    GraphCompileM (Array PortBundle × PortBundle) := do
  let body := e.appArg!
  let x := e.appFn!.appArg!

  trace[HouLean.Apex.compiler] "Compiling match on {x}"

  if x.isFVar then
    trace[HouLean.Apex.compiler] "  → fvar case, reducing match"
    compile (← whnf e)
  else
    let matchFn := e.getAppFn.constName!
    trace[HouLean.Apex.compiler] "  → nontrivial match"
    if info.numParams = 0 ∧ info.numDiscrs = 1 ∧ info.altNumParams.size = 1 then
      let fn := e.appFn!.appFn!
      compile (Expr.letE `r (← inferType x) x ((fn.app (.bvar 0)).app body) false)
    else
      let e' ← whnf (← unfold e matchFn).expr
      withUnfolded matchFn do
        unless e != e' do
          throwErrorWithStack m!"can't compile {e}"
        trace[HouLean.Apex.compiler] "reduced to: {e'}"
        compile e'

/-- Internal override for `toApex`, `fromApex`, `apexFlatten`, `apexUnflatten`. -/
partial def tryInternalOverride (fn : Expr) (args : Array Expr) :
    GraphCompileM (Option (Array PortBundle × PortBundle)) := do

  let (.const fname _) := fn | return none

  if fname == ``toApex ∧ args.size == 4 then
    return ← compile args[3]!

  if fname == ``fromApex ∧ args.size == 4 then
    return ← compile args[3]!

  if fname == ``apexFlatten ∧ args.size == 4 then
    return ← compile args[3]!
    -- let (inputs, output) ← compile args[3]!
    -- unless inputs.size == 0 do throwError "APEX compiler bug in {decl_name%}!"
    -- return some (inputs, .node (output.flatten.map (ArrayTree.leaf)))

  if fname == ``apexUnflatten ∧ args.size == 4 then
    return ← compile args[3]!

    -- let (inputs, output) ← compile args[3]!
    -- let some t ← getApexType? args[0]! | throwError "Invalid APEX type {args[0]!}!"

    -- let .leaf (.port outputId) := output
    --   | throwError "Trying to unflatten back into {args[0]!}, only a single port expected but got {output.toString}!"

    -- -- recover the shape of the type
    -- let output := t.mapIdx (fun i _ => PortPtr.subport outputId i)
    -- return (inputs, output)

  return none

/-- Try to compile using implemented_by override -/
partial def tryCompileImplementedBy (fn : Expr) (args : Array Expr) :
    GraphCompileM (Option (Array PortBundle × PortBundle)) := do

  let s := compilerExt.getState (← getEnv)

  if let some r ← tryInternalOverride fn args then
    return r

  -- Direct expr level overrides
  if let some fn' := s.implementedByExpr[fn]? then
    trace[HouLean.Apex.compiler] "Using implemented_by override for {fn} -> {fn'}"
    return some (← compile (fn'.beta args))

  -- Name based overrides
  if let (.const fn _) := fn then
    if let some (fn', argMap) := s.implementedByName.find? fn then
      trace[HouLean.Apex.compiler] "Using implemented_by override for {fn} -> {fn'}"
      let args' := argMap.map (fun i? => i?.map (fun i => args[i]!))
      let e' ← do
        try
          let e' ← mkAppOptM fn' args'
          pure e'
        catch err =>
          throwErrorWithStack m!"Failed to apply implemented_by override {fn} -> {fn'}\n{err.toMessageData}"
      return ← compile e'

  return none

-- /-- Try to compile variadic argument -/
-- partial def tryCompileVariadic (e : Expr) :
--     GraphCompileM (Option (Array PortBundle × PortBundle)) := do
--   if let some output ← compileVariadicArg? e then
--     return (#[], output)
--   return none

/-- Compile constructor application -/
partial def compileConstructor (_e : Expr) (info : ConstructorVal) (args : Array Expr) :
    GraphCompileM (Array PortBundle × PortBundle) := do
  -- unless ← isStructureType (← inferType e) do
  --   throwErrorWithStack m!"Constructor must produce a structure type: {e}"

  let args := args[info.numParams:].toArray
  let args ← args.mapM (fun x => do pure (← compile x).2)
  return (#[], .node args)

/-- Compile projection function -/
partial def compileProjectionFn (fname : Name) (args : Array Expr) :
    GraphCompileM (Array PortBundle × PortBundle) := do
  let some info ← getProjectionFnInfo? fname
    | throwErrorWithStack m!"Expected projection function: {fname}"

  trace[HouLean.Apex.compiler] "Compiling projection function {fname}"
  let x := args[args.size - 1]!
  let (_, x) ← compile x
  let some si := x.child? info.i
    | throwErrorWithStack m!"Invalid projection {info.i} of {args[args.size - 1]!}: {x.toString}"
  return (#[], si)

/-- Compile for-loop -/
partial def compileForLoop (e : Expr) (args : Array Expr) :
    GraphCompileM (Array PortBundle × PortBundle) := do
  trace[HouLean.Apex.compiler] "Compiling for-loop"

  let State := args[0]!
  let iterations := args[1]!
  let init := args[2]!
  let loop := args[3]!

  let (ctx, loop) ← abstractAllFVars loop

  let Context ← inferType ctx

  let e' ← mkAppOptM ``forLoop.apex_impl
    #[some State, some Context, none, none,
      some iterations, some init, some ctx, some loop]

  trace[HouLean.Apex.compiler] "Replaced for loop:\n{e}\n==>\n{e'}"

  return ← compile e'

partial def compileIte (e : Expr) (args : Array Expr) :
    GraphCompileM (Array PortBundle × PortBundle) := do
  trace[HouLean.Apex.compiler] "Compiling if .. then .. else .."

  let type := args[0]!
  let condition := args[1]!
  let onTrue := args[2]!
  let onFalse := args[3]!

  trace[HouLean.Apex.compiler] "Trying to reduce `{condition}` to a concrete value"
  if let some c ← tryReduceBool condition then
    trace[HouLean.Apex.compiler] "  success, the value is {c}"
    if c then
      return ← compile onTrue
    else
      return ← compile onFalse
  trace[HouLean.Apex.compiler] "  failed, building normal if .. then .. else .."

  -- -- when we can express branches as context mutation
  -- if let some (ctx,proj,te) ← asContextChange #[onTrue, onFalse] then

  --   let onTrue  := te[0]!
  --   let onFalse := te[1]!

  --   let Context ← inferType ctx

  --   let e' ← mkAppOptM ``ifThenElse.apex_impl_ctx_modify
  --     #[some type, some Context, none, none,
  --       some condition, some onTrue, some onFalse, some ctx, some proj]

  --   trace[HouLean.Apex.compiler] "Replaced for if:\n{e}\n==>\n{e'}"

  --   return ← compile e'
  if let some (ctx,ctx',split,te) ← asContextChange' #[onTrue, onFalse] then

    let onTrue  := te[0]!
    let onFalse := te[1]!

    let Context ← inferType ctx
    let Context' ← inferType ctx'

    let e' ← mkAppOptM ``ifThenElse.apex_impl_ctx_modify'
      #[some type, some Context, some Context', none, none,
        some condition, some onTrue, some onFalse, some ctx, some split]

    trace[HouLean.Apex.compiler] "Replaced for if:\n{e}\n==>\n{e'}"

    let (inputs, .node #[output, ctxOutput']) ← compile e'
      | throwError m!"Bud in {decl_name%}, invalid result of compiling implementation of  if .. then .. else .. "

    -- takes `ctx'` which should be a product of fvars and updates it with
    -- port bundle `ctxOutput` which will be deconstructed based on the shape of `ctx'`
    updateFVars ctx' ctxOutput'

    return (inputs, output)

  else   -- fall back option, we coudn't figure out what to mutate

    let (ctx, te) ← abstractAllFVarsMany #[onTrue, onFalse]

    let onTrue  := te[0]!
    let onFalse := te[1]!

    let Context ← inferType ctx

    trace[HouLean.Apex.compiler] m!"Can't express branches as context modification.\n\
                                    return type: {type}\n\
                                    context type: {Context}\n\
                                    Falling back to naive if .. then .. else .. implementation."

    let e' ← mkAppOptM ``ifThenElse.apex_impl_simple
      #[some type, some Context, none, none, none,
        some condition, some onTrue, some onFalse, some ctx]

    trace[HouLean.Apex.compiler] "Replaced for if:\n{e}\n==>\n{e'}"

    return ← compile e'


/-- Compile normal function application -/
partial def compileNormalApp (fn : Expr) (args : Array Expr) (e : Expr) :
    GraphCompileM (Array PortBundle × PortBundle) := do
  let .const fname _ := fn
    | throwErrorWithStack m!"Expected constant function: {fn}"

  let info ← getFunInfo fn
  let args := (args.zip info.paramInfo).filterMap (fun (arg,info) =>
    if info.isExplicit then some arg else none)
  let some nodeType ← getNodeType (.const fname [])
    | let r ← unfold e fname
      if r.expr == e then
        throwErrorWithStack m!"No APEX node for {fname}"
      trace[HouLean.Apex.compiler] "Unfolding {fname}"
      withUnfolded fname do
        return ← compile r.expr

  let argPorts ← args.mapM (fun x => do pure (← compile x).2)

  let name := (Name.mkSimple nodeType.leanDecl.getString!)
  let ⟨_, inputs, output⟩ ← addNode nodeType (Name.mkSimple nodeType.leanDecl.getString!) argPorts
  for argport in argPorts, input in inputs do
    makeConnection argport input
  trace[HouLean.Apex.compiler] "succesfully maked connections to {name}"
  return (#[], output)

/-- Compile function application -/
partial def compileApplication (e : Expr) :
    GraphCompileM (Array PortBundle × PortBundle) := do
  let (fn, args) := e.withApp fun fn args => (fn, args)

  -- Handle for-loop
  if fn.isAppOf ``forLoop then
    return ← compileForLoop e args

  -- Handle if .. then .. else ..
  if fn.isAppOf ``ifThenElse then
    return ← compileIte e args

  -- Try implemented_by override
  if let some result ← tryCompileImplementedBy fn args then
    return result

  -- Handle match statements
  if let some info := isMatcherAppCore? (← getEnv) e then
    return ← compileMatchExpr e info

  let .const fname _ := fn
    | throwErrorWithStack m!"Unexpected function application: {e}"

  -- Check if function should be unfolded
  if (compilerExt.getState (← getEnv)).toUnfold.contains fname then
    let r ← Meta.unfold e fname
    trace[HouLean.Apex.compiler] "Unfolding {fname}"
    return ← compile r.expr

  -- Try variadic argument
  if let some result ← compileVariadicArg? e then
    return (#[],result)

  -- Handle constructor
  if let some (info, args) ← constructorApp? e then
    return ← compileConstructor e info args

  -- Handle projection function
  if let some _ ← getProjectionFnInfo? fname then
    return ← compileProjectionFn fname args

  -- Normal application
  compileNormalApp fn args e

/-- Compile let expression -/
partial def compileLet (n : Name) (t : Expr) (v : Expr) (b : Expr) :
    GraphCompileM (Array PortBundle × PortBundle) := do
  if (← whnf t).isForall then
    return ← compile (b.instantiate1 v)

  withLetDecl n t v fun x => do
    let (_, letOutput) ← compile v
    modify (fun s => {s with fvarToPort := s.fvarToPort.insert x.fvarId! letOutput})
    let b := b.instantiate1 x
    compile b

/-- Compile projection expression -/
partial def compileProjection (s : Name) (i : Nat) (x : Expr) :
    GraphCompileM (Array PortBundle × PortBundle) := do
  trace[HouLean.Apex.compiler] "Compiling projection {s}.{i}"

  let t ← whnf (← inferType x)
  let some t ← getApexType? t
    | throwError "invalid type of {x} : {t}"
  if t.isNode then
    let x' := x
    let (_, x) ← compile x
    let some si := x.child? i
      | throwErrorWithStack m!"Invalid projection {i} of ({x'} : {← inferType x'})\nshape is: {x.toString}"
    return (#[], si)
  else
    let info := getStructureInfo (← getEnv) s
    let some fn := info.getProjFn? i
      | throwErrorWithStack m!"Invalid projection {i} for {t}"
    let fn := (← mkAppM fn #[x]).appFn!

    -- Try implemented_by override
    if let some result ← tryCompileImplementedBy fn #[x] then
      return result
    else
      throwErrorWithStack m!"Don't know how to compile projection {fn}"

/-- Compile lambda expression -/
partial def compileLambda (e : Expr) :
    GraphCompileM (Array PortBundle × PortBundle) := do
  forallTelescopeReducing (← inferType e) fun xs _ => do
    if xs.size = 0 then
      throwErrorWithStack m!"Function expected: {e}"
    let b := (mkAppN e xs)
    let some t ← getApexType? (← inferType b)
      | throwError "invalid return type {← inferType b}"
    let mut inputs : Array PortBundle := #[]
    for x in xs do
      let input ← addFVar x.fvarId!
      inputs := inputs.push input
    let (_,output) ← compile b

    let outputPortBundle : PortBundle :=
      t.mapIdx (fun _ (name,_) => (.output name))
    makeConnection output outputPortBundle

    return (inputs, output)

/-- Compile Lean expression to APEX graph -/
partial def toApexGraph (e : Expr) :
    GraphCompileM (Array PortBundle × PortBundle) := do
  Meta.withIncRecDepth do

  withTraceNode `HouLean.Apex.compiler
      (fun r => return m!"[{ExceptToEmoji.toEmoji r}] {e}") do

    let e' ← withConfig (fun cfg => {cfg with iota := false, zeta := false}) <| whnfC e
    if e != e' then trace[HouLean.Apex.compiler] m!"reduced to: {e'}"
    -- unless ← isDefEq e e' do -- todo: this is just for debuging, probably remove this
    --   throwError m!"Non defeq reduction\n{e}\n==>{e'}\n"
    let e := e'

    if let some val ← tryReduceToLiteral e then
      return (#[], .leaf (.literal val))

    match e with
    | .bvar _ =>
      throwErrorWithStack m!"Cannot compile bound variable: {e}"
    | .fvar id =>
      compileVariable id e
    | .lit lit =>
      compileLiteral lit
    | mkApp3 (.const ``Float.ofScientific []) m s e =>
      compileFloat m s e
    | .const _ _ =>
      compileConstant e
    | .sort _ =>
      throwErrorWithStack m!"Cannot compile sort: {e}"
    | .mvar _ =>
      let e ← instantiateMVars e
      if e.isMVar then
        throwErrorWithStack m!"Cannot compile metavariable: {e}"
      compile (← instantiateMVars e)
    | .app .. =>
      compileApplication e
    | .letE n t v b _ =>
      compileLet n t v b
    | .proj s i x =>
      compileProjection s i x
    | .lam .. =>
      compileLambda e
    | _ =>
      throwErrorWithStack m!"Cannot compile expression: {e}. Constructor {e.ctorName}"
end


def getValueNode? (tag : ApexTypeTag) : MetaM (Option NodeType) :=
    let valueNodeName := Name.append `HouLean.Apex.Generated (Name.mkSimple s!"Value{tag.toString}")
    getNodeType (.const valueNodeName [])

def PortType.typeTag? (t : PortType) : Option ApexTypeTag :=
  match t with
  | .builtin t => some t
  | .variadic t? => t?
  | _ => none

def addValueNode (t : PortType) (nodeName : Name) : GraphCompileM (Nat × Nat) := do

  let some tag := t.typeTag?
    | throwError s!"APEX compiler bug in {decl_name%}, invalid type!"
  let some nodeType ← getValueNode? tag
    | throwError s!"APEX compiler bug in {decl_name%}, can't find value node for {tag.toString}"
  let ⟨_, #[.leaf (.port inputId)], .leaf (.port outputId)⟩ ← addNode nodeType nodeName #[]
    | throwError s!"APEX compiler bug in {decl_name%}, invalid shape of value node!"

  return (inputId, outputId)

/-- If an input is directly connected to an output then APEX might not know its type.
Therefore we break up these connections by placing `Value<>` node.

todo: add value nodes for
  - output literasl
  - unconnected input ports
-/
def fixInputOutputTypes : GraphCompileM Unit := do

  -- fix outputs
  let oldOutputs := (← get).graph.outputs
  let mut newOutputs : Array (LocalPort × OutputConnection) := #[]
  for (p,oldOutput) in oldOutputs  do
    trace[HouLean.Apex.compiler] m!"fixing output port {p.name}"
    match oldOutput with
    | .input inputId =>
      trace[HouLean.Apex.compiler] m!"direct connection to input[{inputId}]"
      let (valueIn, valueOut) ← addValueNode p.type p.name
      makeSingleConnection (.input inputId) (.port valueIn)
      newOutputs := newOutputs.push (p, .port valueOut)

    | .literal val =>
      trace[HouLean.Apex.compiler] m!"connection set to literal"
      let (valueIn, valueOut) ← addValueNode p.type p.name
      newOutputs := newOutputs.push (p, .port valueOut)
      makeSingleConnection (.literal val) (.port valueIn)

    | _ =>
      trace[HouLean.Apex.compiler] m!"nothing to fix"
      newOutputs := newOutputs.push (p,oldOutput)
      continue

  modifyGraph (fun g => pure ({g with outputs := newOutputs}, ()))

  -- fix unconnected inputs i.e. ensure at least one connection
  for (inputPort,trgs) in (← get).graph.inputs do
    if trgs.size = 0 then
      trace[HouLean.Apex.compiler] m!"fixing unused input {inputPort.name}"
      let (valueIn, _) ← addValueNode inputPort.type inputPort.name
      makeSingleConnection (.input inputPort.localId) (.port valueIn)


def programToApexGraph (e : Expr) : MetaM ApexGraph := do
  GraphCompileM.run' do
    let e ← forallTelescopeReducing (← inferType e) fun xs _ =>
      mkLambdaFVars xs (e.beta xs)
    let (inputs, output) ← compile e

    -- if not a function then we have to make outputs
    if inputs.size = 0 then
      let some t ← getApexType? (← inferType e)
        | throwError "Invalid APEX type of {e}"
      let outputPorts : PortBundle :=  t.mapIdx (fun _ (n,_) => PortPtr.output n)
      makeConnection output outputPorts

    fixInputOutputTypes


open Elab Term Command in
/-- Print APEX graph for given Lean expression. -/
elab t:"#apex_graph" x:term : command => do
  liftTermElabM do
  let x ← elabTermAndSynthesize x none
  let g ← programToApexGraph x
  logInfoAt t s!"{g}"


-- todo: move this somewhere
run_meta compilerExt.add (.implementedByName ``apexFlatten ``id' #[none, some 3])
run_meta compilerExt.add (.implementedByName ``apexUnflatten ``id' #[none, some 3])


end HouLean.Apex.Compiler
