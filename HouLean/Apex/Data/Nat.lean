import HouLean.Apex.ApexType
import HouLean.Apex.Data.Int
import HouLean.Apex.Compile.Attr

open HouLean Apex Generated Compiler

instance : ApexType Nat Int where
  toApex := Int.ofNat
  fromApex := Int.toNat

run_cmd compilerExt.add (.implementedByName ``Int.ofNat ``id' #[none, some 0])

def Int.toNat.apex_impl (x : Int) : Nat := fromApex (Generated.MaxInt x #a[0])
run_cmd compilerExt.add (.implementedByName ``Int.toNat ``Int.toNat.apex_impl #[some 0])

-- Delegate Nat operations to Int
@[apex_implements Nat.add]
def Nat.add.apex_impl (x y : Nat) : Nat := fromApex ((toApex x) + (toApex y))

@[apex_implements Nat.mul]
def Nat.mul.apex_impl (x y : Nat) : Nat := fromApex ((toApex x) * (toApex y))

@[apex_implements Nat.sub]
def Nat.sub.apex_impl (x y : Nat) : Nat := fromApex (Generated.MaxInt ((toApex x) - (toApex y)) #a[0])

@[apex_implements Nat.mod]
def Nat.mod.apex_impl (x y : Nat) : Nat := fromApex ((toApex x) % (toApex y))

@[apex_implements Nat.div]
def Nat.div.apex_impl (x y : Nat) : Nat := fromApex ((toApex x) / (toApex y))

-- @[apex_implements Nat.zero]
-- def Nat.zero.apex_impl : Nat := fromApex (0 : Int)

@[apex_implements Float.ofNat]
def Float.ofNat.apex_impl (x : Nat) : Float := 
  ConvertIntFloat (toApex x)

-- @[apex_implements Nat.decLt]
-- def Nat.decLt.apex_impl (a b : Nat) : Bool := decide (toApex a < toApex b)
