import HouLean.Apex.ApexType
import HouLean.Apex.Data.Int
import HouLean.Apex.Data.Nat
import HouLean.Apex.Compile.Attr

open HouLean Apex Generated

instance : ApexType UInt64 Int where
  toApex x := x.toInt64.toInt
  fromApex x := x.toNat.toUInt64

-- todo: remove this, `apex_type` attribute should be reserved for built in types ontly
attribute [apex_type "Int"] UInt64

-- Delegate Nat operations to Int
@[apex_implements UInt64.add]
def UInt64.add.apex_impl (x y : UInt64) : UInt64 := fromApex (toApex x + toApex y)

@[apex_implements UInt64.mul]
def UInt64.mul.apex_impl (x y : UInt64) : UInt64 := fromApex (toApex x * toApex y)

@[apex_implements UInt64.toFloat]
def UInt64.toFloat.apex_impl (x : UInt64) : Float := ConvertIntFloat (toApex x)

@[apex_implements UInt64.ofNat]
def UInt64.ofNat.apex_impl (x : Nat) : UInt64 := fromApex (toApex x : Int) -- this is stupd but works ...
