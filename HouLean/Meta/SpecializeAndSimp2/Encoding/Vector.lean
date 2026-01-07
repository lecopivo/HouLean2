import HouLean.Meta.SpecializeAndSimp2.Encoding

open Lean

namespace HouLean.Meta.Sas

open Lean Meta Parser.Command Elab Command

def vectorStructName (type : Expr) (n : Nat) : MetaM Name := do
  return Name.mkSimple s!"Vector{← ppExpr type}{n}"

def vectorStructCmds (type : Expr) (n : Nat) : MetaM (Array (TSyntax `command)) := do

  let name ← vectorStructName type n
  let id := mkIdent name
  let typeStx ← PrettyPrinter.delab type
  let nStx := Syntax.mkNatLit n

  let toVecId := mkIdent (name.append `toVector)
  let fromVecId := mkIdent (name.append `fromVector)
  let v : Ident ← `(v)
  let mut fields : Array (TSyntax ``structSimpleBinder) := #[]
  let mut elemAccs : Array Term := #[]
  let mut elemProjs : Array Term := #[]
  let mut simpThrms : Array Command := #[]
  for i in [0:n] do
    let iStx := Syntax.mkNatLit i
    let fieldId := mkIdent (.mkSimple s!"x{i}")
    fields := fields.push (← `(structSimpleBinder| $fieldId:ident : $typeStx))
    elemAccs := elemAccs.push (← `($v:ident[$iStx]))
    elemProjs := elemProjs.push (← `($v:ident.$fieldId))

    let thmId := mkIdent (name.append (.mkSimple s!"toVector_getElem_{i}"))
    simpThrms := simpThrms.push (←
      `(command| @[simp] theorem $thmId (v : $id) :
         ($toVecId v)[$iStx] = v.$fieldId := rfl))

  let cmd ← `(command|
    structure $id where
      $[$fields]*
    deriving Inhabited)

  let toVec ← `(command| def $toVecId:ident ($v : $id) : Vector $typeStx $nStx := #v[$elemProjs,*])
  let fromVec ← `(command| def $fromVecId:ident ($v : Vector $typeStx $nStx) : $id := ⟨$elemAccs,*⟩)

  let toFromVecId := mkIdent (name.append `toVector_fromVector)
  let toFromVec ← `(command| @[simp] theorem $toFromVecId:ident ($v : Vector $typeStx $nStx) : $toVecId ($fromVecId $v) = $v := sorry_proof)

  let fromToVecId := mkIdent (name.append `fromVector_toVector)
  let fromToVec ← `(command| @[simp] theorem $fromToVecId:ident ($v : $id) : $fromVecId ($toVecId $v) = $v := sorry_proof)

  let vectorMkId := mkIdent (name.append `vector_mk)
  let elems ← Array.range n |>.mapM (fun i => `(term| $v:ident[$(Syntax.mkNatLit i)]))
  let vectorMk ← `(command| @[simp] theorem $vectorMkId:ident ($v : Array $typeStx) (h : Array.size $v = $nStx) :
      Vector.mk $v h = $toVecId ($(mkIdent (name.append `mk)) $elems*) := sorry_proof)


  return #[cmd, toVec, fromVec, toFromVec, fromToVec, vectorMk] ++ simpThrms

-- todo: add simp attributes that should be attached to the simpe theorems!
def defVectorStruct (type : Expr) (n : Nat) : CommandElabM Unit := do
  let cmds ← liftTermElabM <| vectorStructCmds type n
  for cmd in cmds do
    elabCommand cmd

open Qq

-- todo: tag these with `opencl_csimp` such that they are applicable only during SAS for opencl
run_cmd defVectorStruct q(Float32) 2
run_cmd defVectorStruct q(Float32) 3
run_cmd defVectorStruct q(Float32) 4

run_cmd defVectorStruct q(Float) 2
run_cmd defVectorStruct q(Float) 3
run_cmd defVectorStruct q(Float) 4

-- #check VectorFloat3.vector_mk
-- #check #v[1.0, 2.0, 3.0] rewrite_by simp

-- #check VectorFloat323.toVector_getElem_0
-- theorem vector3_mk (x y z : Float32) :
--   #v[x,y,z] = VectorFloat323.toVector (.mk x y z) := by ext i; simp
