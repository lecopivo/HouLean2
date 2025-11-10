import HouLean.LeanGraph.Traverse
import HouLean.LeanGraph.Init

open Lean HouLean LeanGraph Meta Traverse


instance : Zero Vector3 := ⟨⟨0,0,0⟩⟩
set_option trace.HouLean.LeanGraph.typecheck true



run_elab
  let s ← IO.FS.withFile "Tests/LeanGraph/graph6.json" .read fun file => do
    file.readToEnd

  let .ok json := Json.parse s
    | throwError "failed parsing json!"
  let .ok graph := fromJson? (α:=LeanGraph) json 
    | throwError "failed loading graph!"


  let graph ← graph.typeCheck

  let msg := graph.nodes.map (fun n => s!"{n.name} : {n.type.inputs.map (·.toString)} -> {n.type.outputs.map (·.toString)}")
  let a := msg.joinl (map:=id) (·++"\n"++·) 
  logInfo a



#check withRef
-- open Qq
-- run_meta
--   let p ← mkPortType q(Float)
--   IO.println p.toString

#exit
  -- let ctx ← buildContext graph

  -- let go : TraverseM Unit := do
  --   let mut rest := (← mkFreshExprMVar none).mvarId!
  --   for node in graph.nodes.reverse do
  --     rest ← processNode node rest

  --   -- let _ ← Meta.synthPending rest

  --   rest.withContext do
  --     for (name,var) in (← get).visitedNodes do
  --       let val ← instantiateMVars (← var.getValue?).get!
  --       let _ ← isTypeCorrect val
  --       let (fn,allArgs) := val.withApp (fun fn args => (fn,args))
  
  --       let allArgs ← allArgs.mapM (fun arg =>do return m!"({arg} : {← inferType arg})")
  --       -- logInfo m!"{name} : {fn} {allArgs}"

  --       let e ← mkLambdaFVars (← get).vars (Expr.fvar var)
  --       withOptions (fun opts => opts.setBool `pp.structureInstanceTypes true) <|
  --         logInfo e

    --     forallTelescope (← inferType fn) fun xs _ => do
          
    --       let args ← (xs.zip allArgs).mapM (fun (x,arg) => do
    --         let bi ← x.fvarId!.getBinderInfo
    --         let xname ← x.fvarId!.getUserName
    --         if bi.isExplicit then
    --           return some (xname, (← instantiateMVars arg))
    --         else
    --           return none)
    --       let args := args.filterMap id
    --         -- (fun (info,arg) => if info.isExplicit then some arg else none)

    --       let outputPort ← mkPortType (← inferType val) false "output"
    --       let inputPorts ← args.mapM (fun (name,arg) => do 
    --         mkPortType (← inferType arg) false (toString name.eraseMacroScopes))

    --       logInfo <| m!"{name} : {← instantiateMVars (← inferType val)} := {val} "
    --                  ++ "\n" ++ 
    --                  m!"{name} : {inputPorts.map (·.toString)} → {outputPort.toString}"

    -- rest.withContext do
    --   let var := (←get).visitedNodes["HouLean_Vector3_add_1"]?.get!
    --   let val := (← var.getValue?).get!
    --   logInfo val
    --   logInfo (val.getArg! 0)
    --   logInfo (val.getArg! 1)
  -- let (_,s) ← go ctx default
  -- 

structure Vector3 where
  (x y z : Vector3)

open Qq
run_elab
  withLocalDeclD `x q(Float) fun x => do
    let e ← mkAppM ``Vector3.mk #[x,x,x]
    logInfo e

-- «HouLean.Vector3»
#eval (Name.mkSimple "HouLean.Vector3")
  
#eval "HouLean.Vector3".toName == ``HouLean.Vector3

#exit
set_option pp.structureInstanceTypes true    

#check 
  let Arith_one := One.one;
  let Arith_zero := Zero.zero;
  let HouLean_Vector3_add :=
    { x := Arith_one, y := Arith_one, z := Arith_one : Vector3}.add { x := Arith_zero, y := Arith_one, z := Arith_zero :Vector3};
  let HouLean_Vector3_fromGeodetic := { x := Arith_one, y := Arith_zero, z := Arith_one :Vector3}.fromGeodetic;
  let Arith_add := HouLean_Vector3_add + HouLean_Vector3_fromGeodetic;
  let Arith_zero_1 := Zero.zero;
  let HouLean_Vector3_add_1 := Arith_add.add Arith_zero_1;
  let Arith_add_1 := HouLean_Vector3_add_1.x + HouLean_Vector3_add_1.y;
  Arith_add_1


#check Meta.synthPending

#check (⟨0,1,3⟩ : Vector3)

open Qq PrettyPrinter
run_elab
  let e := q((Vector3.mk 0 1 2) + ⟨1,2,3⟩)
  logInfo (← delab e)


