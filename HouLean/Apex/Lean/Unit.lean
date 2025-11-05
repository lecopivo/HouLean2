import HouLean.Apex.ApexType
import HouLean.Apex.Data.Int
import HouLean.Apex.Compile.Attr

open HouLean Apex Generated

instance : ApexType PUnit Int where
  toApex _ := 0
  fromApex _ := PUnit.unit

def _root_.Int.zero : Int := 0

open Compiler in
run_meta compilerExt.add (.implementedByName ``PUnit.unit ``Int.zero  #[]) default
