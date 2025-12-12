import Lean
import HouLean.Init
import HouLean.Meta.SimpTheoremName

namespace HouLean

opaque OpenCL.RealWorld.nonemptyType : NonemptyType.{0}

@[expose] def OpenCL.RealWorld : Type := OpenCL.RealWorld.nonemptyType.type

instance OpenCL.RealWorld.instNonempty : Nonempty OpenCL.RealWorld :=
  by exact OpenCL.RealWorld.nonemptyType.property


/-- OpenCL Context/Monad. This is the context in which kernels are executed, -/
abbrev OpenCLM := StateM OpenCL.RealWorld

namespace OpenCL

-- =================================================================================================
-- Implemented By
-- =================================================================================================

register_simp_attr opencl_csimp

/-- OpenCL compiler rewrite rule,
`implemented_by ... : lhs = rhs` will rewrite `rhs` with `lhs` at compile time.

This ia jsut a macro for:
```
@[opencl_csimp] theorem opencl_compile_rewrite_rule bs* : lhs = rhs
``` -/
scoped syntax "implemented_by" bracketedBinder* " : " term:55 " = " term (" := " term)? : command

/-- Compilers gadget that turns `f (argList [xs,*])` to `f xs.*` -/
def argList {α} (args : List α) : List α := args

open Lean Elab Term Command in
elab_rules : command
| `(implemented_by $bs:bracketedBinder* : $lhs:term = $rhs:term $[ := $prf]?) => do
  -- let lhsStr ← liftTermElabM <|
  --   withAutoBoundImplicit do
  --   elabBinders bs fun _ => do
  --   let lhs ← elabTermAndSynthesize lhs none
  --   Meta.exprToString lhs

  -- ugh this is ugly ...
  let id ← `(ident| opencl_rewrite_rule)
  -- let Lean.Syntax.ident _ _ id _ := id.raw | throwError s!"bug in {decl_name%}"
  -- let id := mkIdent (id.append (.mkSimple lhsStr))

  match prf with
  | some prf =>
    elabCommand (← `(@[opencl_csimp]
                     theorem $id $bs* : ($lhs) = ($rhs) := $prf))
  | none =>
    elabCommand (← `(@[opencl_csimp]
                     theorem $id $bs* : ($lhs) = ($rhs) := by sorry_proof))


-- =================================================================================================
-- Compile Time Value
-- =================================================================================================

/-- This value of the type `CompTime α` is enforced to be known at compile time. -/
abbrev CompTime (α : Sort u) := α

/-- This proposition has OpenCL compiler support and it checks that the interpreter evaluates
`a` to `b`. Only few types are supported like `Nat`, `Int`. -/
structure CompTimeEq {α} (a b : α) : Prop where
  eq : a = b

-- =================================================================================================
-- OpenCL Type
-- =================================================================================================

class OpenCLType (α : Type u) where
  /-- Name of the corresponding OpenCL type -/
  name : String
  /-- Short name used for function name mangling when doing monomorphization -/
  shortName : String
  /-- For non built in types we store its OpenCL definition.

  The structure shape might not be the same for OpenCL and Lean. -/
  definition? : Option String := none

/-- Atomic OpenCL types are char, uchar, short, ushort, int, uint, long, ulong, float, double

These are the types we can have pointers to and can have short vector version of, like `float3` -/
class AtomicOpenCLType (α : Type) extends OpenCLType α where
  valid : (α = Char) ∨
          (α = Int16) ∨ (α = UInt16) ∨
          (α = Int32) ∨ (α = UInt32) ∨
          (α = Int64) ∨ (α = UInt64) ∨
          (α = Float32) ∨ (α = Float64) ∨
          (α = Int) ∨ (α = Nat)

instance [inst : OpenCLType α] : OpenCLType (OpenCLM α) where
  name := inst.name
  shortName := inst.shortName

instance : OpenCLType Unit where
  name := "void"
  shortName := "v"

instance : AtomicOpenCLType Char where
  name := "char"
  shortName := "c"
  valid := by simp

instance : AtomicOpenCLType Int16 where
  name := "short"
  shortName := "s"
  valid := by simp

instance : AtomicOpenCLType UInt16 where
  name := "ushort"
  shortName := "us"
  valid := by simp

instance : AtomicOpenCLType Int32 where
  name := "int"
  shortName := "i"
  valid := by simp

instance : AtomicOpenCLType UInt32 where
  name := "uint"
  shortName := "ui"
  valid := by simp

instance : AtomicOpenCLType Int64 where
  name := "long"
  shortName := "l"
  valid := by simp

instance : AtomicOpenCLType UInt64 where
  name := "ulong"
  shortName := "ul"
  valid := by simp

instance : AtomicOpenCLType Float32 where
  name := "float"
  shortName := "f"
  valid := by simp

instance : AtomicOpenCLType Float64 where
  name := "double"
  shortName := "d"
  valid := by simp

instance : AtomicOpenCLType Int where
  name := "int"
  shortName := "i"
  valid := by simp

instance : AtomicOpenCLType Nat where
  name := "uint"
  shortName := "ui"
  valid := by simp


/-- OpenCL short vector types are allowed of only a specific length. This class allows to enforce
this restriction on the size `n`. -/
class AllowedVectorSize (n : Nat) where
  valid : n = 2 || n = 3 || n = 4 || n = 8 || n = 16

instance : AllowedVectorSize 2 := ⟨by simp⟩
instance : AllowedVectorSize 3 := ⟨by simp⟩
instance : AllowedVectorSize 4 := ⟨by simp⟩
instance : AllowedVectorSize 8 := ⟨by simp⟩
instance : AllowedVectorSize 16 := ⟨by simp⟩
instance : AllowedVectorSize (1+1) := ⟨by simp⟩
instance : AllowedVectorSize (2+1) := ⟨by simp⟩
instance : AllowedVectorSize (3+1) := ⟨by simp⟩
instance : AllowedVectorSize (7+1) := ⟨by simp⟩
instance : AllowedVectorSize (15+1) := ⟨by simp⟩

instance [t : AtomicOpenCLType T] [AllowedVectorSize n] : OpenCLType (Vector T n) where
  name := s!"{t.name}{n}"
  shortName := s!"{t.shortName}{n}"


-- =================================================================================================
-- OpenCL Function
-- =================================================================================================

namespace OpenCLFunction
-- -- todo: decide what variants are actually needed
-- inductive ArgKind where
--   | input  -- e.g. const float x
--   | output  -- e.g. float * x
--   | ref  -- e.g. const float * x
-- deriving BEq, Inhabited

-- structure Argument where
--   typeName : String
--   name : String
--   kind : ArgKind
-- deriving BEq, Inhabited

inductive FunKind where
  | normal
  | infix (priority : Nat := 100)
  | prefix
  | postfix
  | constructor
  | elemget
  | elemset
deriving BEq, Inhabited, Lean.ToExpr

-- inductive Body where
--   | builtin
--   | code (s : String)
-- deriving BEq, Inhabited

end OpenCLFunction

open OpenCLFunction in
class OpenCLFunction {F : Type} (f : F) where
  name : String
  kind : FunKind := .normal
  definition? : Option String := none
deriving BEq, Inhabited

-- todo: replace `Inhabited type` with `spec : type`. The issue is that the compile time simp
--       get's into infinite recursion as it keeps on simplifying `spec`
noncomputable
opaque oclFunction (type : Type) [Inhabited type] (name : String) (kind : OpenCLFunction.FunKind := .normal) : type



-- =========================
-- = Arithmetic operations =
-- =========================


section ArithmeticOperations

variable {α}

instance [AtomicOpenCLType α] [Add α] : OpenCLFunction (@HAdd.hAdd α α α _) where
  name := " + "
  kind := .infix

instance [AtomicOpenCLType α] [Sub α] : OpenCLFunction (@HSub.hSub α α α _) where
  name := " - "
  kind := .infix

instance [AtomicOpenCLType α] [Mul α] : OpenCLFunction (@HMul.hMul α α α _) where
  name := " * "
  kind := .infix

instance [AtomicOpenCLType α] [Div α] : OpenCLFunction (@HDiv.hDiv α α α _) where
  name := " / "
  kind := .infix

instance [AtomicOpenCLType α] [Neg α] : OpenCLFunction (@Neg.neg α _) where
  name := " -"
  kind := .prefix

instance [AtomicOpenCLType α] [HAnd α α α] : OpenCLFunction (HAnd.hAnd : α → α → α) where
  name := " & "
  kind := .infix

instance [AtomicOpenCLType α] [HShiftRight α α α] : OpenCLFunction (HShiftRight.hShiftRight : α → α → α) where
  name := " >> "
  kind := .infix


variable {n}

instance [AtomicOpenCLType α] [AllowedVectorSize n] [Add α] :
    OpenCLFunction (@HAdd.hAdd (Vector α n) (Vector α n) (Vector α n) _) where
  name := " + "
  kind := .infix

instance [AtomicOpenCLType α] [AllowedVectorSize n] [Sub α] :
    OpenCLFunction (@HSub.hSub (Vector α n) (Vector α n) (Vector α n) _) where
  name := " - "
  kind := .infix

instance [AtomicOpenCLType α] [AllowedVectorSize n] [Mul α] :
    OpenCLFunction (@HMul.hMul α (Vector α n) (Vector α n) _) where
  name := " * "
  kind := .infix

instance [AtomicOpenCLType α] [AllowedVectorSize n] [Neg α] :
    OpenCLFunction (@Neg.neg (Vector α n) _) where
  name := " -"
  kind := .prefix


end ArithmeticOperations


-- =================================================================================================
-- Pointers
-- =================================================================================================

-- inductive PointerScope where
--   | default | global | loc | priv

-- structure PointerType where
--   scope := PointerScope.default
--   restrict := false
--   volatile := false

opaque Pointer.nonemptyType (α : Type) : NonemptyType.{0}
set_option linter.unusedVariables false in
/-- Pointer i.e. something like `float *` on OpenCL level. -/
def Pointer (α : Type) : Type := (Pointer.nonemptyType α).type
instance {α} [AtomicOpenCLType α] : Nonempty (Pointer α) :=
  by exact (Pointer.nonemptyType α).property

noncomputable
instance {α} [AtomicOpenCLType α] : Inhabited (Pointer α) :=
  Classical.inhabited_of_nonempty (by infer_instance)

noncomputable
instance [ty : AtomicOpenCLType α] : OpenCLType (Pointer α) where
  name := ty.name ++ " *"
  shortName := "p" ++ ty.shortName


section LoadAndStore

variable {α} [Inhabited α]

opaque Pointer.get [AtomicOpenCLType α] (a : Pointer α) (offset : UInt64) : OpenCLM α
opaque Pointer.set [AtomicOpenCLType α] (a : Pointer α) (offset : UInt64) (val : α) : OpenCLM Unit

-- opaque Pointer'.get [AtomicOpenCLType α] {c r t} (a : Pointer' α c r t) (offset : UInt64) : OpenCLM α
-- opaque Pointer'.set [AtomicOpenCLType α] {r t} (a : Pointer' α (const:=false) r t) (offset : UInt64) (val : α) : OpenCLM Unit

opaque Pointer.vload [AtomicOpenCLType α] [AllowedVectorSize n] (offset : UInt64) (a : Pointer α) : OpenCLM (Vector α n)
opaque Pointer.vstore [AtomicOpenCLType α] [AllowedVectorSize n] (val : Vector α n) (offset : UInt64) (a : Pointer α) : OpenCLM Unit

variable [AtomicOpenCLType α]

instance : OpenCLFunction (Pointer.get (α:=α)) where
  name := ""
  kind := .elemget

instance : OpenCLFunction (Pointer.set (α:=α)) where
  name := ""
  kind := .elemset

variable [AllowedVectorSize n]

instance : OpenCLFunction (Pointer.vload (α:=α) (n:=n)) where
  name := s!"vload{n}"

instance : OpenCLFunction (Pointer.vstore (α:=α) (n:=n)) where
  name := s!"vstore{n}"


-- =================================================================================================
-- C-style Array
-- =================================================================================================

/-- C style array wrapped around in a struct to preserve value semantics. -/
structure CArray (α : Type) (n : Nat) where
  data : Vector α n

instance [t : OpenCLType α] : OpenCLType (CArray α n) where
  name := s!"array_{t.name}_{n}"
  shortName := s!"a{t.shortName}{n}"
  definition? := s!"struct array_{t.name}_{n} \{ {t.name} data[{n}]; };"

end LoadAndStore
