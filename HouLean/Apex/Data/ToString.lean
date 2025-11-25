import HouLean.Apex.Data.String
import HouLean.Apex.Data.Nat

namespace HouLean.Apex.Compiler

open Apex Generated

class ApexToString (α : Type u) where
  toString : α → String

instance : ApexToString Int where
  toString := string_FromInteger

instance : ApexToString String where
  toString x := x

instance : ApexToString Nat where
  toString x := string_FromInteger (toApex x)

instance : ApexToString Float where
  toString x := string_Format "{}" #a[.float x]

def _root_.ToString.toString.apex_impl {α} [ApexToString α] (a : α) : String :=
  ApexToString.toString a

open Compiler in
run_meta compilerExt.add (.implementedByName ``toString ``ToString.toString.apex_impl
  #[some 0, none, some 2]) default
