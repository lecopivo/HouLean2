import HouLean.Vex.Compiler.CVex

open Lean System Parser Category Elab Term


namespace HouLean.Vex.Compiler

/-- Search for Houdini installations in common locations -/
def findHoudiniDirs : IO (List FilePath) := do
  let paths ← 
    if System.Platform.isWindows then
      pure [⟨"C:\\Program Files\\Side Effects Software"⟩]
    else if System.Platform.isOSX then
      pure [⟨"/Applications"⟩, ⟨"/Library/Frameworks"⟩]
    else -- Linux
      pure [⟨"/opt"⟩, ⟨"/usr/local"⟩]
  
  -- Filter to existing directories
  paths.filterM (fun p => do pure (← p.isDir))

/-- Find all Houdini version directories -/
def findHoudiniVersions (baseDir : FilePath) : IO (List FilePath) := do
  let entries ← baseDir.readDir
  let houdiniDirs := entries.toList.filter fun entry =>
    let name := entry.fileName
    name.startsWith "hfs" || name.toLower.startsWith "houdini"
  return houdiniDirs.map (baseDir / ·.fileName)

/-- Get the vcc executable name for the current platform -/
def vccExecutableName : String :=
  if System.Platform.isWindows then "vcc.exe" else "vcc"

/-- Find vcc in a Houdini installation -/
def findVccInHoudini (houdiniDir : FilePath) : IO (Option FilePath) := do
  let vccPath := houdiniDir / "bin" / vccExecutableName
  if ← vccPath.pathExists then
    return some vccPath
  else
    return none

/-- Validate that vcc works by running it with --version -/
def validateVcc (vccPath : FilePath) : IO Bool := do
  try
    let output ← IO.Process.output {
      cmd := vccPath.toString
      args := #["--version"]
    }
    return output.exitCode == 0
  catch _ =>
    return false

/-- Find all valid vcc installations -/
def findAllVcc : IO (List FilePath) := do
  let baseDirs ← findHoudiniDirs
  let mut vccPaths : List FilePath := []
  
  for baseDir in baseDirs do
    let houdiniVersions ← findHoudiniVersions baseDir
    for houdiniDir in houdiniVersions do
      if let some vccPath ← findVccInHoudini houdiniDir then
        if ← validateVcc vccPath then
          vccPaths := vccPath :: vccPaths
  
  return vccPaths

/-- Get the path to vcc, checking environment variable first, then searching -/
def getVccPath : IO FilePath := do
  -- First check if HFS environment variable is set
  if let some hfs ← IO.getEnv "HFS" then
    let vccPath := System.mkFilePath [hfs, "bin", vccExecutableName]
    if ← vccPath.pathExists then
      if ← validateVcc vccPath then
        return vccPath
  
  -- Check HOUDINI_PATH
  if let some houdiniPath ← IO.getEnv "HOUDINI_PATH" then
    let vccPath := System.mkFilePath [houdiniPath, "bin", vccExecutableName]
    if ← vccPath.pathExists then
      if ← validateVcc vccPath then
        return vccPath
  
  -- Search for installations
  let vccPaths ← findAllVcc
  match vccPaths with
  | [] => throw (IO.userError "Could not find vcc compiler. Please set HFS environment variable or install Houdini.")
  | vccPath :: _ => 
    return vccPath


inductive VccError.Severity 
  | error | warning | unknown
deriving Repr, BEq

structure VccError where
  -- todo: row and col can be number or range
  row : String := ""
  col : String := ""
  severity : VccError.Severity
  msg : String
deriving Repr

def VccError.parse (s : String) : VccError := Id.run do
  let mut msg := s
  -- file:row:col: 
  let prfx := msg.takeWhile (fun c => c != ' ')
  msg := msg.stripPrefix (prfx ++ " ")

  let [_, row, col,_] := prfx.splitOn ":"
    | return { severity := .unknown, msg := s }

  -- Error/Warning _:
  let prfx := msg.takeWhile (fun c => c != ':')
  msg := msg.stripPrefix (prfx ++ ": ")

  let severity := prfx.takeWhile (fun c => c != ' ')
  let mut sev := Severity.unknown

  if severity == "Warning" then
    sev := .warning
  if severity == "Error" then
    sev := .error
  if sev == .unknown then
    return { severity := .unknown, msg := s }
  

  return {
    row := row
    col := col
    msg := msg
    severity := sev
  }


/-- Compile VEX snippet to CVEX. 

Returns an empty array on success. -/
def compileCVexCore (cvexCode : String) 
    (funcName : String := "snippet")
    (vccPath : FilePath := ⟨"/opt/hfs21.0/bin/vcc"⟩) : IO (Array VccError) := do

  -- Write to temporary VFL file
  let tmpVfl : FilePath := ⟨s!"/tmp/houdini_temp/cvex/{funcName}.vfl"⟩
  IO.FS.withFile tmpVfl .write fun vfl => do 
    vfl.putStr cvexCode
  
    -- Determine output path
    let vexOutput : FilePath := ⟨s!"/tmp/houdini_temp/cvex/{funcName}.vex"⟩
  
    -- Compile with vcc
    let args := #["-c", "cvex", tmpVfl.toString, "-o", vexOutput.toString]
    let output ← IO.Process.output {
      cmd := vccPath.toString
      args := args
    }

    -- Check result
    if output.exitCode != 0 then
      let errors : Array VccError := output.stderr.splitOn "\n" 
        |>.toArray |>.map VccError.parse
      return errors
    else
      return #[]



def compileCVex (code : TSyntax ``cvexProgram) : IO (Array VccError) := do
  let mainName : String := 
    match code with
    | `(cvexProgram| cvex $main ( $_ ) { $_ }) => main.getId.getString!
    | _ => ""
  let code := toString code.raw.prettyPrint
  compileCVexCore code mainName (← getVccPath)


def compileVexSnippet (code : TSyntax ``vexSnippet) : TermElabM (Array VccError) := do
  let code ← liftMacroM <| snippetToCVex code "snippet"
  compileCVex code

