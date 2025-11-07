import HouLean.Apex.ApexType
import HouLean.Apex.Data.Int
import HouLean.Apex.Data.Nat
import HouLean.Apex.Compile.Attr

open HouLean Apex Generated Compiler

-- todo: rename this file

instance : ApexType UInt64 Int where
  toApex x   := x |> UInt64.toNat |> Int.ofNat
  fromApex x := x |> Int.toNat    |> Nat.toUInt64

run_cmd compilerExt.add (.implementedByName ``Nat.toUInt64 ``id' #[none, some 0])
run_cmd compilerExt.add (.implementedByName ``UInt64.toNat ``id' #[none, some 0])
run_cmd compilerExt.add (.implementedByName ``UInt64.ofNat ``id' #[none, some 0])

instance : ApexType Int64 Int where
  toApex x   := x |> Int64.toInt
  fromApex x := x |> Int.toInt64

run_cmd compilerExt.add (.implementedByName ``Int64.toInt ``id' #[none, some 0])
run_cmd compilerExt.add (.implementedByName ``Int.toInt64 ``id' #[none, some 0])

run_cmd compilerExt.add (.implementedByName ``Nat.toInt64 ``id' #[none, some 0])


-- Delegate Nat operations to Int
@[apex_implements UInt64.add]
def UInt64.add.apex_impl (x y : UInt64) : UInt64 := fromApex (toApex x + toApex y)

@[apex_implements UInt64.mul]
def UInt64.mul.apex_impl (x y : UInt64) : UInt64 := fromApex (toApex x * toApex y)

@[apex_implements UInt64.toFloat]
def UInt64.toFloat.apex_impl (x : UInt64) : Float := ConvertIntFloat (toApex x)
