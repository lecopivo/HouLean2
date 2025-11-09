import HouLean

open Lean HouLean Meta

unsafe def main : IO UInt32 := do
  -- Initialize Lean environment
  initSearchPath (← findSysroot)
    [ "/home/tskrivan/Documents/HouLean/.lake/build/lib/lean",
      "/home/tskrivan/Documents/HouLean/.lake/packages/Qq/.lake/build/lib/lean/" ]

  enableInitializersExecution
  let env ← Lean.importModules #[{ module := `Lean }, { module := `HouLean }] {} (loadExts := true)

  let ctx : Core.Context := {fileName := "<input>", fileMap := .ofString ""}
  let s : Core.State := { env := env } 

  let _ ← MetaM.toIO (ctxCore := ctx) (sCore := s) do
    let types ← LeanGraph.getGraphTypes
    let json := toJson types

    let _ ← IO.FS.withFile "NodeEditor/types.json" .write fun file => do
      file.putStr (toString json)
      file.flush

  return 0

  
