import HouLean.Apex.Compile.Main
import HouLean
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
      return "failed"

  r := {r with messages := r.messages ++ (← state.messages.toArray.mapM (fun m => m.toJson))
               result := script}

  IO.println (toJson r)

  return 0
