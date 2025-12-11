import HouLean.OpenCL.Compiler.Main
import HouLean.Data.Geometry
import HouLean.Data.FloatP
import HouLean.Data.Matrix
import HouLean.OpenCL.Data.NanoVDB
import HouLean.OpenCL.Data.ArrayType
import HouLean.Apex.Generated.Defs
import HouLean.Meta.ProdLike
import HouLean.Meta.AppInfo
import HouLean.Meta.DoNotation
import HouLean.OpenCL.WorkItemFunctions

namespace HouLean.OpenCL
#exit
structure Frame (R : Type) (dim : Nat) where
  toWorldMatrix : Matrix R (dim+1) (dim+1)
  toFrameMatrix : Matrix R (dim+1) (dim+1)

set_option linter.unusedVariables false in
/-- Vector living in a particular frame -/
def FVector {dim} (R : Type) (frame : Frame R dim) := Vector R dim

-- /-- Matrix transformain vector between two particular frames -/
-- def FMatrix {dim dim'} (R : Type) (frame : Frame R dim) (frame' : Frame R dim') := Matrix R dim dim'

section
variable {R} [FloatType R] {dim : Nat}

def Frame.toFrame (frame : Frame R dim) (x : Vector R dim) : FVector R frame :=
  frame.toFrameMatrix.transformPointLeft x

def FVector.toWorld {frame : Frame R dim} (x : FVector R frame) : Vector R dim :=
  frame.toWorldMatrix.transformPointLeft (x : Vector R dim)

end



structure Attribute (cls : AttributeClass) (type : Type) (name : String := "")
    (input : Nat := 0) (read := true) (write := false) {Ptr} [ArrayType type Ptr] where
  size : Int32
  ptr : ArrayPointer type
deriving ProdLike, Inhabited

instance {cls type name input read write} {Ptr} [ArrayType type Ptr] [t : OpenCLType type] :
    OpenCLType (Attribute cls type name input read write) where
  name := s!"attribute_{t.name}"
  shortName := s!"attr{t.shortName}"
  definition? :=
    s!"todo: provide implementation"

implemented_by {cls type name input read write} {Ptr} [ArrayType type Ptr] [Inhabited Ptr]
    (attr : Attribute cls type name input read write) :
    attr.ptr
    =
    oclFunction (_ → _) ".ptr" .postfix attr

structure Volume (type : Type) (name : String := "")
    (input : Nat := 0) (read := true) (write := false) {Ptr} [ArrayType type Ptr] where
  (stridex stridey stridez offset: Int32)
  ptr : ArrayPointer type
deriving ProdLike

instance {type name input read write} {Ptr} [ArrayType type Ptr] [t : OpenCLType type] :
    OpenCLType (Volume type name input read write) where
  name := s!"volume_{t.name}"
  shortName := "vol{t.shortName}"
  definition? :=
    s!"todo: provide implementation"

structure VDB (type : Type) (name : String := "") (input : Nat := 0) [NanoVDB.ValueType type] where
  ptr : NanoVDB.ReadAccessor type

set_option linter.unusedVariables false

namespace BindingSOP

inductive TimeScale where
  | none | timestep | invtimestep | exptimestep
deriving Repr

inductive AttributeType where
  | float | int | floatarray | intarray
deriving Repr

inductive VolumeOptions where
  | aligned
  | nonaligned (resolution : Bool) (voxelsize : Bool) (voxeltoworld : Bool) (worldtovoxel : Bool)
deriving Repr

structure VDBOptions where
  (resolution : Bool) (voxelsize : Bool) (voxeltoworld : Bool) (worldtovoxel : Bool)
deriving Repr

inductive BindingOptions where
  | int (prec : Precision) (value : Int)
  | float (prec : Precision) (value : FloatP prec)
  | vector3 (prec : Precision) (value : Vector (FloatP prec) 3)
  | vector4 (prec : Precision) (value : Vector (FloatP prec) 4)
  | ramp
  | attrib (input : Nat) (name : String) (cls : AttributeClass) (type : AttributeType) (size : Nat := 1)
           (prec : Precision := .single) (read : Bool := true) (write : Bool := true) -- (optional : Bool := false)
  | volume (input : Nat) (name : String) (options : VolumeOptions)
           (prec : Precision := .single) (read : Bool := true) (write : Bool := true) -- (optional : Bool := false)
  | vdb    (input : Nat) (name : String) (options : VDBOptions)
           (prec : Precision := .single) (write : Bool := true) -- (optional : Bool := false)
deriving Repr

open Lean Meta Qq

def getParamBinding (type : Expr) : MetaM (Option BindingOptions) := do

  let mut dflt : Expr := default
  try
    dflt ← mkAppOptM ``default #[type, none]
  catch _ =>
    return none

  let defaultValue :=
    if type.isAppOfArity ``optParam 2 then
      type.appArg!
    else
      dflt

  if (← isDefEq type q(Int)) then
    let val ← unsafe evalExpr Int q(Int) defaultValue
    return some (.int .single val)

  if (← isDefEq type q(Int32)) then
    let val ← unsafe evalExpr Int32 q(Int32) defaultValue
    return some (.int .single val.toInt)

  if (← isDefEq type q(Int64)) then
    let val ← unsafe evalExpr Int64 q(Int64) defaultValue
    return some (.int .double val.toInt)

  if (← isDefEq type q(Float32)) then
    let val ← unsafe evalExpr Float32 q(Float32) defaultValue
    return some (.float .single val)

  if (← isDefEq type q(Float64)) then
    let val ← unsafe evalExpr Float64 q(Float64) defaultValue
    return some (.float .double val)

  if (← isDefEq type q(Vector Float32 3)) then
    let val ← unsafe evalExpr (Vector Float32 3) q(Vector Float32 3) defaultValue
    return some (.vector3 .single val)

  if (← isDefEq type q(Vector Float64 3)) then
    let val ← unsafe evalExpr (Vector Float64 3) q(Vector Float64 3) defaultValue
    return some (.vector3 .double val)

  if (← isDefEq type q(Vector Float32 4)) then
    let val ← unsafe evalExpr (Vector Float32 4) q(Vector Float32 4) defaultValue
    return some (.vector4 .single val)

  if (← isDefEq type q(Vector Float64 4)) then
    let val ← unsafe evalExpr (Vector Float64 4) q(Vector Float64 4) defaultValue
    return some (.vector4 .double val)

  return none


def getAttribBinding (type : Expr) (argName : String) : MetaM (Option (BindingOptions × Expr)) := do

  unless type.isAppOf ``Attribute do
    return none

  let appInfo ← getAppInfo type

  let cls ← (appInfo.getArg! `cls) |> unsafe evalExpr AttributeClass q(AttributeClass)
  let mut valType := (appInfo.getArg! `type)
  let mut name ← (appInfo.getArg! `name) |> unsafe evalExpr String q(String)
  let input ← (appInfo.getArg! `input) |> unsafe evalExpr Nat q(Nat)
  let read  ← (appInfo.getArg! `read)  |> unsafe evalExpr Bool q(Bool)
  let write ← (appInfo.getArg! `write) |> unsafe evalExpr Bool q(Bool)
  let ptrType := (appInfo.getArg! `Ptr)

  if name == "" then
    name := argName

  let mut type := AttributeType.float
  let mut size := 1
  let mut prec := Precision.single

  if valType.isAppOf ``Vector then
    size ← unsafe evalExpr Nat q(Nat) (valType.getArg! 1)
    valType := valType.getArg! 0

  if valType.isAppOf ``Matrix then
    let m ← unsafe evalExpr Nat q(Nat) (valType.getArg! 1)
    let n ← unsafe evalExpr Nat q(Nat) (valType.getArg! 2)
    size := m*n
    valType := valType.getArg! 0

  if ← isDefEq valType q(Float32) then
    type := .float
    prec := .single
  else if ← isDefEq valType q(Float) then
    type := .float
    prec := .double
  else if (← isDefEq valType q(Int)) || (← isDefEq valType q(Int32)) then
    type := .int
    prec := .single
  else if ← isDefEq valType q(Int64) then
    type := .int
    prec := .double
  else
    return none

  return some ((.attrib input name cls (type:=type) (size:=size)
                        (prec:=.single) (read:=read) (write:=write)),
               ptrType)

-- run_meta
--   let e := q(Attribute .point Float32 "pscale" (write:=true))
--   let b ← getAttribBinding e ""
--   logInfo (repr b)

-- run_meta
--   let e := q(Attribute .point (Vector Float32 3) "P" (write:=true))
--   let b ← getAttribBinding e ""
--   logInfo (repr b)

def getVolumeBinding (type : Expr) (argName : String) : MetaM (Option (BindingOptions × Expr)) := do

  unless type.isAppOf ``Volume do
    return none

  let appInfo ← getAppInfo type

  let valType := appInfo.getArg! `type
  let name ← (appInfo.getArg! `name) |> unsafe evalExpr String q(String)
  let input ← (appInfo.getArg! `input) |> unsafe evalExpr Nat q(Nat)
  let read ← (appInfo.getArg! `read) |> unsafe evalExpr Bool q(Bool)
  let write ← (appInfo.getArg! `write) |> unsafe evalExpr Bool q(Bool)
  let ptrType := (appInfo.getArg! `Ptr)

  let mut prec := Precision.single
  let opt : VolumeOptions :=
    .nonaligned
      (resolution := false)
      (voxelsize := false)
      (voxeltoworld := false)
      (worldtovoxel := false)

  if (← isDefEq valType q(Float32)) then
    prec := .single
  else if (← isDefEq valType q(Float64)) then
    prec := .double

  return some ((.volume input name (options:=opt)
                        (prec:=.single) (read:=read) (write:=write)),
               ptrType)

-- run_meta
--   let e := q(Volume Float32 "surface" (write:=true))
--   let b ← getVolumeBinding e ""
--   logInfo (repr b)

-- run_meta
--   let e := q(Volume Float64 "mass")
--   let b ← getVolumeBinding e ""
--   logInfo (repr b)


end BindingSOP

open BindingSOP in
structure BindingSOP where
  name : String
  options : BindingOptions

open Lean Meta Qq


open Lean Meta Qq BindingSOP in
def compileKernelSOP (kernel : Expr) : MetaM (String × Array BindingSOP) := do

  if kernel.hasFVar then
    throwError "Can't compile OpenCL kernels that contains free variables!"

  forallTelescope (← inferType kernel) fun xs _ => do
    go xs.toList #[] #[] #[] #[]
where
  go (xs : List Expr) (xs' letVars args : Array Expr) (bindings : Array BindingSOP) : MetaM (String × Array BindingSOP) := do
    match xs with
    | [] =>
      let kernel' ← mkLambdaFVars (args ++ letVars) (kernel.beta xs')
      logInfo m!"modified kernel is:\n{kernel'}"
      let code ← runCompiler kernel'
      return (code, bindings)
    | x :: xs =>

      let type ← inferType x
      let name := toString (← ppExpr x)
      let n ← x.fvarId!.getUserName
      --
      -- int
      if let some paramBinding ← getParamBinding type then
        go xs (xs'.push x) letVars (args.push x)
          (bindings.push {name := name, options := paramBinding})

      else if let some (attrBinding, ptrType) ← getAttribBinding type name then
        -- attributes provide size and pointer
        withLocalDeclD (n.appendAfter "_size") q(Int32) fun attrSize => do
        withLocalDeclD (n.appendAfter "_ptr") ptrType fun ptr => do
        let typeArgs := type.getAppArgs
        let attr ← mkAppOptM ``Attribute.mk ((typeArgs ++ #[attrSize, ptr]).map some)
        withLetDecl n type attr fun x' => do
        go xs (xs'.push x') (letVars.push x') (args.push attrSize |>.push ptr)
          (bindings.push {name := name, options := attrBinding})

      else if let some (volumeBinding, ptrType) ← getVolumeBinding type name then
        withLocalDeclD (n.appendAfter "_stride_x") q(Int32) fun stridex => do
        withLocalDeclD (n.appendAfter "_stride_y") q(Int32) fun stridey => do
        withLocalDeclD (n.appendAfter "_stride_z") q(Int32) fun stridez => do
        withLocalDeclD (n.appendAfter "_offset") q(Int32) fun offset => do
        withLocalDeclD (n.appendAfter "_ptr") ptrType fun ptr => do
        let typeArgs := type.getAppArgs
        let vol ← mkAppOptM ``Volume.mk ((typeArgs ++ #[stridex, stridey, stridez, offset, ptr]).map some)
        withLetDecl n type vol fun x' => do
        go xs (xs'.push x') (letVars.push x') (args ++ #[stridex, stridey, stridez, offset, ptr])
          (bindings.push {name := name, options := volumeBinding})

      else
        throwError "Unrecognized argument type: {x} : {← inferType x}"
  runCompiler (k : Expr) : MetaM String := do
    return "todo: call the compiler"

def kernel
  (P : Attribute .point (Vector Float32 3))
  (surface : Volume Float32 "surface" (input:=1))
  (maxiter : Int32)
  : OpenCLM Unit := do

  let idx : UInt64 := 0
  let mut p ← P.ptr[idx]
  for i in [0:maxiter.toInt.toNat] do
    p := p
  return


set_option pp.funBinderTypes true in
-- run_meta
--   let _ ← compileKernelSOP q(kernel)

implemented_by : Int32.toInt = oclFunction _ "" .infix
implemented_by : Int.toNat = oclFunction _ "(uint)" .prefix
