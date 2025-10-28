import HouLean.Apex.Compile.Main
import Qq

open Lean Meta Elab Command

open HouLean.Apex.Compiler

structure ToApexGraphCompilationResult where
  messages : Array Json
  result : Option String
deriving ToJson

  -- Elab.runFrontend input opts path.toString mainModuleName
unsafe def main (args : List String) : IO UInt32 := do
  initSearchPath (← findSysroot)
    [ "/home/tskrivan/Documents/HouLean/.lake/build/lib/lean",
      "/home/tskrivan/Documents/HouLean/.lake/packages/Qq/.lake/build/lib/lean/" ]

  enableInitializersExecution
  let env ← Lean.importModules #[{ module := `Lean }, { module := `HouLean }] {} (loadExts := true)

  let some code := args[0]?
    | IO.eprint "Please provide Lean code to compile!"
      return 1

  let mut code := code
  if code.endsWith ".lean" then
    code ← IO.FS.readFile code

  -- todo: more involve stripping of imports, there has to be a lean function
  --       probably use `runFrontend` but `process` is soo simple ...
  code := code.stripPrefix "import HouLean\n"
  let (env, msgs) ← process code env default 

  let mut r : ToApexGraphCompilationResult := {
    messages := (← msgs.toArray.mapM (fun m => m.toJson))
    result := none
  }
  
  let ctx : Core.Context := {fileName := "<input>", fileMap := .ofString code}
  let s : Core.State := { env := env } 

  let (script,state,_) ← MetaM.toIO (ctxCore := ctx) (sCore := s) do
    try
      let info ← getConstInfo `run
      let val := info.value!

      let (_,s) ← functionToApexGraph val default
      let graph := s.graph

      let pythonScript := graph.pythonBuildScript

      return pythonScript
    catch e =>
      logError m!"Compilation to APEX graph failed!\n{e.toMessageData}"
      return ""

  r := {r with messages := r.messages ++ (← state.messages.toArray.mapM (fun m => m.toJson))
               result := script}

  IO.println (toJson r)

  return 0
  -- match Parser.runParserCategory env ``Parser.Module.module code "<code>" with
  -- | .error e =>
  --   IO.println "parsing failed"
  --   IO.println e
  -- | .ok stx =>
  --   IO.println "parsed string"
  --   IO.println stx.prettyPrint

#check Term.exprToSyntax

#exit
def main (args : List String) : IO UInt32 := do
  IO.println "Hello World!"

  let some input := args[0]?
    | IO.println "Error: Missing input Lean source code!"; return 1

  let inputCode ←
    if ¬(input.endsWith ".lean") then
      pure input
    else
      IO.FS.readFile (.mk input)

  let inputCode := inputCode.stripPrefix "import HouLean\n"

  IO.println s!"Input code:\n{inputCode}"

  initSearchPath (← findSysroot)
    [ "/home/tskrivan/Documents/HouLean/.lake/build/lib/lean",
      "/home/tskrivan/Documents/HouLean/.lake/packages/Qq/.lake/build/lib/lean/" ]

  let env ← Lean.importModules #[{ module := `HouLean }, { module := `Init }] {}

  let ctx : Command.Context := {
    fileName := "", 
    fileMap := .ofString inputCode, 
    snap? := none, 
    cancelTk? := none
  }
  let stateRef : IO.Ref Command.State ← IO.mkRef (mkState env)
  
  let runCommandElabM := fun {α} (x : CommandElabM α) => x ctx stateRef

  let r := runCommandElabM do
    
    IO.println "Hello from CommandElabM!"
    match Parser.runParserCategory env `command inputCode "<code>" with
    | .error e => 
      IO.println "parsing failed"
      IO.println e
      return 1
    | .ok stx =>
      Command.elabCommand stx

      let info ← getConstInfo `run
      liftTermElabM <|
      show MetaM _ from do
      IO.println s!"run : {← ppExpr info.type}"
      pure ()
      return 0

  match ← r.toIO' with
  | .error e => 
    IO.println "elaboration failed"
    IO.println (← e.toMessageData.toString)
    return 1
  | .ok r =>
    let s ← stateRef.get
    IO.println (← s.messages.toArray.mapM (fun m => m.toString))
    return r.toUInt32


