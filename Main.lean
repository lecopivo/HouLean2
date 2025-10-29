import HouLean.Apex.Compile.Main
import HouLean
import Qq
import Lean.Data.Json

open Lean Meta Elab Command
open HouLean.Apex.Compiler

structure ToApexGraphCompilationResult where
  messages : Array Json
  result : Option String
deriving ToJson

inductive CodeToCompile
  | file (path : System.FilePath)
  | inline (code : String)
  deriving ToJson, FromJson

structure Request where
  command : String
  data : Option CodeToCompile
  deriving ToJson, FromJson

structure Response where
  status : String
  result : Json
  deriving ToJson, FromJson

unsafe def compileCode (code : String) (env : Environment) : IO ToApexGraphCompilationResult := do
  -- Strip imports
  let mut code := code
  code := code.stripPrefix "import HouLean\n"
  
  let (env, msgs) ← process code env default 

  let mut r : ToApexGraphCompilationResult := {
    messages := (← msgs.toArray.mapM (fun m => m.toJson))
    result := none
  }

  let ctx : Core.Context := {fileName := "<input>", fileMap := .ofString code}
  let s : Core.State := { env := env } 

  let (script, state, _) ← MetaM.toIO (ctxCore := ctx) (sCore := s) do
    try
      let info ← getConstInfo `run
      let val := info.value!
      let graph ← programToApexGraph val
      let pythonScript := graph.pythonBuildScript

      return some pythonScript
    catch e =>
      logError m!"Compilation to APEX graph failed!\n{e.toMessageData}"
      return none

  r := {r with 
    messages := r.messages ++ (← state.messages.toArray.mapM (fun m => m.toJson))
    result := script
  }

  return r

unsafe def handleRequest (req : Request) (env : Environment) : IO Response :=
  match req.command with
  | "compile" => 
    match req.data with
    | some (.file path) =>
      try 
        let code ← IO.FS.readFile path
        let result ← compileCode code env
        return { status := "success", result := toJson result }
      catch e =>
        return { status := "error", result := Json.str s!"Error reading file: {e}" }
        
    | some (.inline code) => 
      try
        let result ← compileCode code env
        return { status := "success", result := toJson result }
      catch e =>
        return { status := "error", result := Json.str s!"Compilation error: {e}" }
        
    | none => 
      return { status := "error", result := Json.str "Missing code to compile" }
      
  | "quit" =>
      return { status := "shutdown", result := Json.str "Server shutting down" }
      
  | _ => 
      return { status := "error", result := Json.str "Unknown command" }

unsafe def processRequest (line : String) (env : Environment) : IO Bool := do
  if line.trim.isEmpty then
    return true  -- Continue on empty lines
  
  match Json.parse line with
  | .error err => 
      let errResp : Response := { status := "error", result := Json.str s!"Parse error: {err}" }
      IO.println (ToJson.toJson errResp).compress
      (← IO.getStdout).flush
      return true
  | .ok json =>
      match FromJson.fromJson? json with
      | .error err =>
          let errResp : Response := { status := "error", result := Json.str s!"Invalid format: {err}" }
          IO.println (ToJson.toJson errResp).compress
          (← IO.getStdout).flush
          return true
      | .ok (req : Request) =>
          let response ← handleRequest req env
          IO.println (ToJson.toJson response).compress
          (← IO.getStdout).flush
          -- Return false to stop if command is "quit"
          return req.command != "quit"

unsafe def serverLoop (env : Environment) : IO Unit := do
  let stdin ← IO.getStdin
  repeat
    let line ← stdin.getLine
    let c ← processRequest line.trim env
    if !c then break

unsafe def compileOnce (env : Environment) (codeOrFile : String) : IO Unit := do
  let req : Request := {
    command := "compile"
    data := some (if codeOrFile.endsWith ".lean" then .file codeOrFile else .inline codeOrFile)
  }
  let response ← handleRequest req env
  IO.println (ToJson.toJson response).compress
  (← IO.getStdout).flush

unsafe def main (args : List String) : IO UInt32 := do
  -- Initialize Lean environment
  initSearchPath (← findSysroot)
    [ "/home/tskrivan/Documents/HouLean/.lake/build/lib/lean",
      "/home/tskrivan/Documents/HouLean/.lake/packages/Qq/.lake/build/lib/lean/" ]

  enableInitializersExecution
  let env ← Lean.importModules #[{ module := `Lean }, { module := `HouLean }] {} (loadExts := true)
  
  if args.contains "-server" then
    serverLoop env
    return 0
  else if h : args.length > 0 then
    compileOnce env (args.getLast (by grind))
  else
    let errResp : Response := { status := "error", result := Json.str s!"Missing input code!" } 
    IO.println (toJson errResp)
    return 0
    

  return 0
