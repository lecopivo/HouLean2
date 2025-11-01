import HouLean.Apex.ApexType
import HouLean.Apex.Data.Int
import HouLean.Apex.Compile.Attr

open HouLean Apex Generated

-- Nat is implemented with Int
attribute [apex_type "Int"] Nat
attribute [apex_node "Value<Int>"] Int.toNat
attribute [apex_node "Value<Int>"] Int.ofNat

instance : ApexType Nat Int where
  toApex := Int.ofNat
  fromApex := Int.toNat

-- Delegate Nat operations to Int
@[apex_implements Nat.add]
def Nat.add.apex_impl (x y : Int) : Int := x + y

@[apex_implements Nat.mul]
def Nat.mul.apex_impl (x y : Int) : Int := x * y

@[apex_implements Nat.sub]
def Nat.sub.apex_impl (x y : Int) : Int := max (x - y) 0

@[apex_implements Nat.mod]
def Nat.mod.apex_impl (x y : Int) : Int := x % y

@[apex_implements Nat.div]
def Nat.div.apex_impl (x y : Int) : Int := x / y

@[apex_implements Nat.succ]
def Nat.succ.apex_impl (x : Int) : Int := x + 1

@[apex_implements Nat.zero]
def Nat.zero.apex_impl : Int := 0

@[apex_implements Float.ofNat]
def Float.ofNat.apex_impl (x : Nat) : Float := 
  ConvertIntFloat (toApex x)

