import Lean

open Lean Meta System

def ensureProjectRoot : IO FilePath := do
  let wsRoot : FilePath := "."

  -- Check for HouLean-specific markers
  let checks := #[
    wsRoot / "lakefile.toml",
    wsRoot / ".lake" / "build",
    wsRoot / "HouLean.lean"  -- or whatever your root module is
  ]

  for path in checks do
    unless ← path.pathExists do
      IO.eprintln s!"Error: {path} not found"
      IO.eprintln "Please run this from the HouLean project root directory"
      IO.Process.exit 1

  return wsRoot

def copyFile (src dst : FilePath) : IO Unit := do
  let args := if Platform.isWindows
    then #["cmd", "/c", "copy", src.toString, dst.toString]
    else #["cp", src.toString, dst.toString]
  let result ← IO.Process.spawn { cmd := args[0]!, args := args[1:] }
  discard <| result.wait

partial def copyOleansRec (src dst : FilePath) : IO Unit := do
  IO.FS.createDirAll dst
  for entry in ← src.readDir do
    let dstPath := dst / entry.fileName
    if ← entry.path.isDir then
      copyOleansRec entry.path dstPath
    else if entry.fileName.endsWith ".olean" then
      copyFile entry.path dstPath

partial def printOLeans (dir : FilePath) : IO Unit := do
  for entry in ← dir.readDir do
    if ← entry.path.isDir then
      printOLeans entry.path
    else if entry.fileName.endsWith ".olean" then
      IO.println entry.path

def main : IO Unit := do

  let projectRoot ← ensureProjectRoot

  IO.println projectRoot


  let buildLib := projectRoot / ".lake" / "build" / "lib"
  let distDir := projectRoot / ".lake" / "dist"

  if ← distDir.pathExists then
    IO.FS.removeDirAll distDir

  IO.FS.createDirAll (distDir / "lib")

  -- Core Lean library
  copyOleansRec ((← findSysroot) / "lib" / "lean") (distDir / "oleans" / "lean")
  copyOleansRec (projectRoot / ".lake" / "build" / "lib" / "lean") (distDir / "oleans" / "houlean")
