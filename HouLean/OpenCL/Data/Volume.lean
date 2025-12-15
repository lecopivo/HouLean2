import HouLean.OpenCL.Data.ArrayType
import HouLean.OpenCL.Data.Fin
import HouLean.OpenCL.Data.Vector
import HouLean.OpenCL.Data.Matrix
import HouLean.OpenCL.Data.Int
import HouLean.OpenCL.Data.Prod
import HouLean.Data.Vector
import HouLean.Meta.DoNotation

namespace HouLean.OpenCL

structure Frame (R : Type) (dim : Nat) where
  toWorldMatrix : Matrix R (dim+1) (dim+1)
  toFrameMatrix : Matrix R (dim+1) (dim+1)
deriving Inhabited

instance [t : OpenCLType R] : OpenCLType (Frame R dim) where
  name := s!"frame{t.shortName}{dim}"
  shortName := s!"frame{t.shortName}{dim}"
  definition? := s!"structure frame{t.shortName}{dim} \{ ... }"

implemented_by [Inhabited R] (f : Frame R dim) :
  f.toWorldMatrix = oclFunction (_ → _) ".toWorldMatrix" .postfix f

implemented_by [Inhabited R] (f : Frame R dim) :
  f.toWorldMatrix = oclFunction (_ → _) ".toFrameMatrix" .postfix f

set_option linter.unusedVariables false in
/-- Vector living in a particular frame -/
def FVector {dim} (R : Type) (frame : Frame R dim) := Vector R dim

-- make this dimension independent?
structure VolumeData (type : Type) {Ptr} [ArrayType type Ptr] where
  stride : Vector Nat 3
  offset : Nat
  ptr : ArrayPointer type

instance [t : OpenCLType type] {Ptr} [ArrayType type Ptr] : OpenCLType (VolumeData type) where
  name := s!"volumedata{t.shortName}"
  shortName := s!"voldata{t.shortName}"

implemented_by {type : Type} {Ptr} [ArrayType type Ptr] (data : VolumeData type) :
  data.stride = oclFunction (_ → _) ".stride" .postfix data

implemented_by {type : Type} {Ptr} [ArrayType type Ptr] (data : VolumeData type) :
  data.offset = oclFunction (_ → _) ".offset" .postfix data

implemented_by {type : Type} {Ptr} [ArrayType type Ptr] [Inhabited Ptr] (data : VolumeData type) :
  data.ptr = oclFunction (_ → _) ".ptr" .postfix data

set_option linter.unusedVariables false in
abbrev Volume (type : Type) (res : Vector Nat 3) (frame : Frame R 3) (name : String := "")
    (input : Nat := 0) (read := true) (write := false) {Ptr} [ArrayType type Ptr] :=
  VolumeData type

variable {R} [FloatType R]
  {type : Type} {res : Vector Nat 3} {frame : Frame R 3} {name : String} {input : Nat}
  {Ptr} [ArrayType type Ptr]

instance {write} :
    GetElem (Volume type res frame name input (read:=true) write) (Nat×Nat×Nat) (OpenCLM type)
      (fun _ i => i.1 < res.x ∧ i.2.1 < res.y ∧ i.2.2 < res.z) where
  getElem vol i _ := do
    let (ix,iy,iz) := i
    let idx := vol.offset + vol.stride.x * ix + vol.stride.y * iy + vol.stride.z * iz
    getElem vol.ptr idx .intro

instance {write} :
    SetElemM (Volume type res frame name input (read:=true) write)
      (Nat×Nat×Nat) type OpenCLM
      (fun _ i => i.1 < res.x ∧ i.2.1 < res.y ∧ i.2.2 < res.z) where
  setElemM vol i x _ := do
    let (ix,iy,iz) := i
    let idx := vol.offset + vol.stride.x * ix + vol.stride.y * iy + vol.stride.z * iz
    setElemM vol.ptr idx x .intro


namespace Volume

class Interpolate (Idx Dom Val : Type) (Val' : outParam Type) (m : Type → Type) where
  interpolate : (Idx → m Val) → (Dom → m Val')

export Interpolate (interpolate)

instance {m} [Monad m] : Interpolate Nat Float Float Float m where
  interpolate data x := do
    let xi := x.floor
    let w := x - xi
    let i := xi.toUInt64.toNat
    return  Math.lerp (← data i) (← data (i+1)) w

instance {m} [Monad m] [Interpolate Idx Dom Float Float m] :
    Interpolate (Nat×Idx) (Float×Dom) Float Float m where
  interpolate data x :=
    let (x0, x1) := x
    interpolate (fun i => interpolate (fun idx => data (i, idx)) x1) x0
    -- interpolate (fun idx => interpolate (fun i => data (i, idx)) x0) x1

def sample
    (vol : Volume Float res frame name input (read:=true) write)
    (p : Vector Float 3) : OpenCLM Float :=
  -- generated with
  -- (interpolate (fun ijk : Nat×Nat×Nat => vol[ijk]'sorry_proof) (p[0],p[1],p[2]))
  -- rewrite_by
  --   simp -zeta only [interpolate,pure_bind,bind_pure,bind_assoc]
  --   lift_lets
  --   simp -zeta only [interpolate,pure_bind,bind_pure,bind_assoc]
  have xi := p[0].floor;
  have w := p[0] - xi;
  have i := xi.toUInt64.toNat;
  have xi := p[1].floor;
  have w_1 := p[1] - xi;
  have i_1 := xi.toUInt64.toNat;
  have xi := p[2].floor;
  have w_2 := p[2] - xi;
  have i_2 := xi.toUInt64.toNat;
  do
  let x ← vol[(i, i_1, i_2)]'sorry_proof
  let x_1 ← vol[(i, i_1, i_2 + 1)]'sorry_proof
  let x_2 ← vol[(i, i_1 + 1, i_2)]'sorry_proof
  let x_3 ← vol[(i, i_1 + 1, i_2 + 1)]'sorry_proof
  let x_4 ← vol[(i + 1, i_1, i_2)]'sorry_proof
  let x_5 ← vol[(i + 1, i_1, i_2 + 1)]'sorry_proof
  let x_6 ← vol[(i + 1, i_1 + 1, i_2)]'sorry_proof
  let x_7 ← vol[(i + 1, i_1 + 1, i_2 + 1)]'sorry_proof
  return (Math.lerp (Math.lerp (Math.lerp x x_1 w_2) (Math.lerp x_2 x_3 w_2) w_1)
        (Math.lerp (Math.lerp x_4 x_5 w_2) (Math.lerp x_6 x_7 w_2) w_1) w)

def sampleAndGradient
    (vol : Volume Float res frame name input (read:=true) write)
    (p : Vector Float 3) : OpenCLM (Float × Vector Float 3) := do
  let y ← vol.sample p
  return (y,0)



end Volume

set_option linter.unusedVariables false in
declfun linearInterpolate {Idx Dom Val : Type} (f : Idx → Val) (x : Dom) : Val

defun linearInterpolate {n : Nat} (f : Fin n → Float) (x : Float) : Float :=
  let xi := x.floor
  let w := x - xi
  let i := xi.toUInt64.toNat
  let i0 : Fin n := ⟨min i (n-1), sorry_proof⟩
  let i1 : Fin n := ⟨min (i+1) (n-1), sorry_proof⟩
  Math.lerp (f i0) (f i1) w

defun linearInterpolate {Idx Dom} [LinearInterpolate Idx Dom Float] {n : Nat}
    (f : Fin n × Idx → Float) (x : Float × Dom) : Float :=
  let (x0,x1) := x
  linearInterpolate (fun i => linearInterpolate (fun idx => f (i, idx)) x1) x0




declfun linearInterpolateGrad {Idx Dom Val : Type} (f : Idx → Val) (x : Dom) : Val × Dom

defun linearInterpolateGrad {n : Nat} (f : Fin n → Float) (x : Float) : Float × Float :=
  let xi := x.floor
  let w := x - xi
  let i := xi.toUInt64.toNat
  let i0 : Fin n := ⟨min i (n-1), sorry_proof⟩
  let i1 : Fin n := ⟨min (i+1) (n-1), sorry_proof⟩
  let y0 := f i0
  let y1 := f i1
  (Math.lerp y0 y1 w, y1 - y0)

defun linearInterpolateGrad {Idx Dom} [Sub Dom] [Math.Lerp Dom Float]
    [LinearInterpolateGrad Idx Dom Float] {n : Nat}
    (f : Fin n × Idx → Float) (x : Float × Dom) :=
  let (x0,x1) := x

  let xi := x0.floor
  let w := x0 - xi
  let i := xi.toUInt64.toNat
  let i0 : Fin n := ⟨min i (n-1), sorry_proof⟩
  let i1 : Fin n := ⟨min (i+1) (n-1), sorry_proof⟩
  let (y0, d0) := linearInterpolateGrad (fun idx => f (i0, idx)) x1
  let (y1, d1) := linearInterpolateGrad (fun idx => f (i1, idx)) x1

  (Math.lerp y0 y1 w, (y0 - y1, Math.lerp d0 d1 w))

defun linearInterpolateGrad (f : Vector Int 0 → Float) (x : Vector Float 0) : Float × Vector Float 0 :=
  (f #v[], #v[])

defun linearInterpolateGrad {n} [inst : LinearInterpolateGrad (Vector Int n) (Vector Float n) Float]
    (f : Vector Int (n+1) → Float) (x : Vector Float (n+1)) : Float × Vector Float (n+1) :=
  let x0 := x[n]
  let x1 := x.pop

  let xi := x0.floor
  let w := x0 - xi
  let i := xi.toInt64.toInt
  let i0 : Int := i
  let i1 : Int := i+1
  have : LinearInterpolateGrad (Vector Int n) (Vector Float (n + 1 - 1)) Float := inst
  let (y0, d0) := linearInterpolateGrad (fun idx : Vector Int n => f (idx.push i0)) x1
  let (y1, d1) := linearInterpolateGrad (fun idx : Vector Int n => f (idx.push i1)) x1

  (Math.lerp y0 y1 w, (Math.lerp d0 d1 w).push (y0 - y1))
