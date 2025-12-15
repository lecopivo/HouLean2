import HouLean.OpenCL.Compiler.RewriteRules
import HouLean.Data.Vector

open HouLean OpenCL Compiler Math

namespace HouLean.OpenCL



open Lean Meta Compiler3


def indexToSuffix? (i : Nat) : Option Name :=
  match i with
  | 0 => `x
  | 1 => `y
  | 2 => `z
  | 3 => `w
  | 4 => `s4
  | 5 => `s5
  | 6 => `s6
  | 7 => `s7
  | 8 => `s8
  | 9 => `s9
  | 10 => `sa
  | 11 => `sb
  | 12 => `sc
  | 13 => `sd
  | 14 => `se
  | 15 => `sf
  | _ => none


impl_by {n : Nat} {T : Type} [AtomicOpenCLType T] (u : Vector T n) (i : Nat) (h) :
    u[i]'h ==> do

  let some i ← runInterpreter? Nat i
    | throwError m!"Must know {i} at compile time when compiling `{u}[{i}]`"
  let u ← compileExpr u

  let some suffix := indexToSuffix? i
    | throwError m!"Invalid index {i}, has to be smaller than 16!"
  let idx := mkIdent suffix

  return ← `(oclExpr| $u:oclExpr.$idx:ident)

-- normalize `getElem` from indexing with `Fin n` to indexing with `Nat`
attribute [opencl_csimp] Fin.getElem_fin


variable {n : Nat} {T : Type}

-- convert component projection to `getElem`
@[opencl_csimp] theorem vectro_x_to_getElem (v : Vector T n) (h) : v.x h = v[0] := rfl
@[opencl_csimp] theorem vectro_y_to_getElem (v : Vector T n) (h) : v.y h = v[1] := rfl
@[opencl_csimp] theorem vectro_z_to_getElem (v : Vector T n) (h) : v.z h = v[2] := rfl
@[opencl_csimp] theorem vectro_w_to_getElem (v : Vector T n) (h) : v.w h = v[3] := rfl
