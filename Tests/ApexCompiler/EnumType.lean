import HouLean

open HouLean Apex

inductive A where
  | a | b | c
deriving EnumType

instance : ApexType A Int where
  toApex x := EnumType.toFin x
  fromApex x := EnumType.fromFin ⟨x.toNat, sorry_proof⟩

@[apex_implements A.a]
def A.a.apex_impl : Int := EnumType.toFin A.a

@[apex_implements A.b]
def A.b.apex_impl : Int := EnumType.toFin A.b

@[apex_implements A.c]
def A.c.apex_impl : Int := EnumType.toFin A.c

@[apex]
def t1 := A.a

@[apex]
def t2 := A.b

@[apex]
def t3 := A.c
