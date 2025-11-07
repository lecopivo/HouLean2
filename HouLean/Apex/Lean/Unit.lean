import HouLean.Apex.ApexType
import HouLean.Apex.Data.Int
import HouLean.Apex.Compile.Attr

open HouLean Apex Generated

instance : ApexType PUnit Int where
  toApex _ := 0
  fromApex _ := PUnit.unit

private def zero_int : Int := 0

open Compiler in
run_meta compilerExt.add (.implementedByName ``PUnit.unit ``zero_int  #[]) default
