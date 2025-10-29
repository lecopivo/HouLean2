import HouLean.Apex.Generated.Nodes
import HouLean.Apex.Compile.ImplementedBy
import HouLean.Apex.Compile.Attr

namespace HouLean.Apex

open Generated

-- there are some problematic functions which have unused arguments
-- uncomment this to see them
set_option linter.unusedVariables false

-- High-Level API for APEX Graph Network
-- This builds on top of the basic opaque bindings

def apexPanic {α : Type} [Inhabited α] (str : String) : α := default

/-- Cause an error during compilation to APEX graph -/
macro "apexPanic!" msg:str : term => `(apexPanic $msg)

-- ============================================================================
-- Conversion Type Classes
-- ============================================================================

class ApexCast (α β : Type) where
  cast : α → β

instance : ApexCast Bool Int where
  cast := ConvertBoolInt

instance : ApexCast Int Bool where
  cast := ConvertIntBool

instance : ApexCast Float Int where
  cast := ConvertFloatInt

instance : ApexCast Int Float where
  cast := ConvertIntFloat

instance : ApexCast Matrix3 Matrix4 where
  cast := ConvertMatrix3Matrix4

instance : ApexCast Matrix4 Matrix3 where
  cast := ConvertMatrix4Matrix3

instance : ApexCast Matrix4 Vector4 where
  cast := ConvertMatrix4Vector4

instance : ApexCast Vector4 Matrix3 where
  cast := ConvertVector4Matrix3

instance : ApexCast ColorRamp FloatRamp where
  cast := ConvertColorRampFloatRamp

instance : ApexCast FloatRamp ColorRamp where
  cast := ConvertFloatRampColorRamp

def cast [ApexCast α β] (x : α) : β := ApexCast.cast x


-- ============================================================================
-- Option
-- ============================================================================

-- During compilation we should treat `Option α` the same as `α × Bool`
-- Helper function to convert APEX's (α × Bool) to Option α
def toOption {α : Type} (x : α × Bool) : Option α := 
  if x.2 then
    some x.1
  else
    none

def fromOption {α : Type} [Inhabited α] (x : Option α) : α × Bool :=
  match x with
  | some x => (x, true)
  | none => (default, false)

