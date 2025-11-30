import HouLean.Apex.Lean.Array.ArrayType
import HouLean.Apex.Lean.Array.Arith

open HouLean Apex Compiler ArrayType

-- Compile `Array α` to the corresponding type provided by `ArrayType` class
abbrev HouLean.Apex.GetArrayType (α : Type) {As : Type} [ArrayType α As] := As

run_meta compilerExt.add (.implementedByName ``Array ``GetArrayType #[some 0, none, none]) default

def Array.emptyWithCapacity.apex_impl {α As} [ArrayType α As] (_c : Nat) : Array α :=
  fromApex (ArrayType.empty α)

run_meta compilerExt.add (.implementedByName ``Array.mkEmpty ``Array.emptyWithCapacity.apex_impl
  #[some 0, none, none, some 1]) default
run_meta compilerExt.add (.implementedByName ``Array.emptyWithCapacity ``Array.emptyWithCapacity.apex_impl
  #[some 0, none, none, some 1]) default

def Array.size.apex_impl {α As} [ArrayType α As] (a : Array α) : Nat :=
  (ArrayType.length α (toApex a)).toNat

run_meta compilerExt.add (.implementedByName ``Array.size ``Array.size.apex_impl #[some 0, none, none, some 1]) default

def Array.getInternal.apex_impl {α As} [ArrayType α As] (a : Array α) (i : @Nat) (_h : LT.lt i a.size) : α :=
  getElem (toApex a) (Int.ofNat i) .intro

run_meta compilerExt.add (.implementedByName ``Array.getInternal ``Array.getInternal.apex_impl
  #[some 0, none, none, some 1, some 2, some 3]) default

def Array.get!Internal.apex_impl {α As} [ArrayType α As] (a : Array α) (i : @Nat) : α :=
  getElem (toApex a) (Int.ofNat i) .intro

run_meta compilerExt.add (.implementedByName ``Array.get!Internal ``Array.get!Internal.apex_impl
  #[some 0, none, none, some 2, some 3]) default

def Array.push.apex_impl {α As} [ArrayType α As] (a : Array α) (v : α) : Array α :=
  fromApex (ArrayType.append (toApex a) v).1

run_meta compilerExt.add (.implementedByName ``Array.push ``Array.push.apex_impl
  #[some 0, none, none, some 1, some 2]) default

def Array.set.apex_impl {α As} [ArrayType α As] (a : Array α) (i : Nat) (v : α) (h : i < a.size) : Array α :=
  fromApex (setElem (toApex a) (Int.ofNat i) v .intro)

run_meta compilerExt.add (.implementedByName ``Array.set ``Array.set.apex_impl
  #[some 0, none, none, some 1, some 2, some 3, some 4]) default

-- todo:
-- #check ArrayType.insert
-- #check Array.insertIdx

-- #check ArrayType.remove
-- #check Array.pop


-- #check Array.usize
-- #check Array.uget
-- #check Array.uset
-- #check Array.pop
-- #check Array.replicate
-- #check Array.mkArray
-- #check Array.swap


-- Arithmetics
open ArrayAdd in
def Array.add.apex_impl {α As} [ArrayType α As] [ArrayAdd α As] (xs ys : Array α) : Array α :=
  fromApex (arrayAdd α (toApex xs) (toApex ys))

run_meta compilerExt.add (.implementedByName ``Array.add ``Array.add.apex_impl #[some 0, none, none, none, some 3, some 4]) default

open ArraySub in
def Array.sub.apex_impl {α As} [ArrayType α As] [ArraySub α As] (xs ys : Array α) : Array α :=
  fromApex (arraySub α (toApex xs) (toApex ys))

run_meta compilerExt.add (.implementedByName ``Array.sub ``Array.sub.apex_impl #[some 0, none, none, none, some 3, some 4]) default

open ArrayMul in
def Array.mul.apex_impl {α As} [ArrayType α As] [ArrayMul α As] (xs ys : Array α) : Array α :=
  fromApex (arrayMul α (toApex xs) (toApex ys))

run_meta compilerExt.add (.implementedByName ``Array.mul ``Array.mul.apex_impl #[some 0, none, none, none, some 3, some 4]) default

open ArrayDiv in
def Array.div.apex_impl {α As} [ArrayType α As] [ArrayDiv α As] (xs ys : Array α) : Array α :=
  fromApex (arrayDiv α (toApex xs) (toApex ys))

run_meta compilerExt.add (.implementedByName ``Array.div ``Array.div.apex_impl #[some 0, none, none, none, some 3, some 4]) default
