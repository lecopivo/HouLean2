import HouLean.Apex.Compile.Main
import HouLean.LeanGraph.LeanGraph
import HouLean.LeanGraph.GraphToCode
import HouLean
import Qq
import Lean.Data.Json

open Lean Meta Elab Command
open HouLean Apex.Compiler

inductive CodeToCompile
  | file (path : System.FilePath)
  | inline (code : String)
deriving ToJson, FromJson, Inhabited

inductive Request where
  | compile (data : CodeToCompile)
  | typecheck (data : LeanGraph)
  | ping
  | quit
deriving ToJson, FromJson, Inhabited

def Request.continue : Request → Bool
  | quit => false
  | _ => true

def program : String :=
"import HouLean

#check \"hello from compiler\"

def run (x : Int) : Int := x + x
"
-- #eval toJson (.compile default : Request)
-- #eval toJson (.compile (.inline program) : Request)
-- #eval toJson (.typecheck { nodes := #[default]
--                            portTypes := #[default]
--                            nodeTypes := #[default]
--                            connections := #[default]} : Request)
-- #eval toJson (.quit : Request)


structure ToApexGraphCompilationResult where
  messages : Array Json
  result : Option String
deriving ToJson, FromJson, Inhabited

structure TypeCheckResult where
  graph : LeanGraph
  leanCode : String
  pythonCode : String
  inputGeos : Array String
  outputGeos : Array String
  messages : Array Json
deriving ToJson, FromJson, Inhabited

inductive Response where
  | compile (data : ToApexGraphCompilationResult)
  | typecheck (data : _root_.TypeCheckResult)
  | error (msg : String)
  | pong
  | quitting
  deriving ToJson, FromJson, Inhabited


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


open Lean Elab Term in
unsafe def typeCheckGraph (graph : LeanGraph) (env : Environment) : IO _root_.TypeCheckResult := do

  let ctx : Core.Context := {fileName := "<input>", fileMap := .ofString ""}
  let s : Core.State := { env := env }

  -- let ((r,pythonCode,inputGeos, outputGeos), sCore, _sMeta, _sElab) ←
  let (r, _) ← TermElabM.toIO (ctxCore := ctx) (sCore := s) (ctxMeta := {}) (sMeta := {}) (ctx := {}) (s := {}) do
    try
      let r ← graph.typeCheck

      -- if result has metavariables we can't compile it so terminating early
      if r.mainProgram.hasMVar then
        logInfo "Resulting code still has some metavariables!"

        return {
          graph := r.graph
          leanCode := r.code
          pythonCode := ""
          inputGeos := #[]
          outputGeos := #[]
          messages := ← (← Core.getMessageLog).toArray.mapM (·.toJson)
        }

      let apexGraph ← programToApexGraph r.mainProgram
      let outGeos := apexGraph.outputs.filterMap (fun o =>
        if o.1.type.typeTag? == some .geometry ||
           o.1.type.typeTag? == none then -- there is bug somewhere and sometime the type is not set properly!
          some s!"output:{o.1.name}"
        else
          none)
      let inGeos := apexGraph.inputs.filterMap (fun input =>
        if input.1.type.typeTag? == some .geometry then
          some input.1.name.toString
        else
          none)
      let pythonCode := apexGraph.pythonBuildScript
      -- return (r, pythonCode, inGeos, outGeos)
      return {
          graph := r.graph
          leanCode := r.code
          pythonCode := pythonCode
          inputGeos := inGeos
          outputGeos := outGeos
          messages := ← (← Core.getMessageLog).toArray.mapM (·.toJson)
        }
    catch e =>
      logError m!"Type checking failed!\n{e.toMessageData}"
      -- return ({ graph := graph, code := "", mainProgram := default }, "", #[], #[])
      return {
          graph := graph
          leanCode := ""
          pythonCode := ""
          inputGeos := #[]
          outputGeos := #[]
          messages := ← (← Core.getMessageLog).toArray.mapM (·.toJson)
        }

  return r

unsafe def handleRequest (req : Request) (env : Environment) : IO Response :=
  match req with
  | .compile (.file path) =>
      try
        let code ← IO.FS.readFile path
        let result ← compileCode code env
        return (.compile result)
      catch e =>
        return .error s!"Compilation error:\n{e}"

  | .compile (.inline code) =>
      try
        let result ← compileCode code env
        return .compile result
      catch e =>
        return .error s!"Compilation error: {e}"

  | .typecheck graph =>
      return .typecheck (← typeCheckGraph graph env)

  | .ping => return .pong
  | .quit => return .quitting


unsafe def processRequest (line : String) (env : Environment) : IO Bool := do
  if line.trim.isEmpty then
    return true  -- Continue on empty lines

  match Json.parse line with
  | .error err =>
      let errResp : Response := .error s!"Parse error: {err}"
      IO.println (ToJson.toJson errResp).compress
      (← IO.getStdout).flush
      return true
  | .ok json =>
      match FromJson.fromJson? json with
      | .error err =>
          let errResp : Response := .error s!"Invalid format: {err}"
          IO.println (ToJson.toJson errResp).compress
          (← IO.getStdout).flush
          return true
      | .ok (req : Request) =>
          let response ← handleRequest req env
          IO.println (ToJson.toJson response).compress
          (← IO.getStdout).flush
          -- Return false to stop if command is "quit"
          return req.continue

unsafe def serverLoop (env : Environment) : IO Unit := do
  let stdin ← IO.getStdin
  repeat
    let line ← stdin.getLine
    let c ← processRequest line.trim env
    IO.sleep 50
    if !c then break

unsafe def compileOnce (env : Environment) (codeOrFile : String) : IO Unit := do
  let req : Request := .compile (if codeOrFile.endsWith ".lean" then .file codeOrFile else .inline codeOrFile)
  let response ← handleRequest req env
  IO.println (ToJson.toJson response).compress
  (← IO.getStdout).flush

unsafe def main (args : List String) : IO UInt32 := do

  let mut workspaceDir ← IO.currentDir

  if let some workspaceDirIdx := args.findIdx? (·=="-w") then
    if let some workspaceDir' := args[workspaceDirIdx+1]? then
      workspaceDir := workspaceDir'

  let leanDir := workspaceDir / ".lake/build/lib/lean"
  let qqDir := workspaceDir / ".lake/packages/Qq/.lake/build/lib/lean"
  -- Initialize Lean environment
  initSearchPath (← findSysroot) [ leanDir, qqDir ]

  enableInitializersExecution
  let env ← Lean.importModules #[{ module := `Lean }, { module := `HouLean }] {} (loadExts := true)

  if args.contains "-server" then
    serverLoop env
    return 0
  else if h : args.length > 0 then
    compileOnce env (args.getLast (by grind))
  else
    let errResp : Response := .error s!"Missing input code!"
    IO.println (toJson errResp)
    return 0

  return 0
