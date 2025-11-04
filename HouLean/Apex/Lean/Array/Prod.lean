import HouLean.Apex.Lean.Array.ArrayType
import HouLean.Apex.Lean.Array.Arith

namespace HouLean.Apex

namespace ArrayType

variable {α β As Bs} [ArrayType α As] [ArrayType β Bs]

instance : ArrayType (α×β) (As×Bs) where
  getElem := fun (xs,ys) i _ => (xs[i], ys[i])
  getElem? := fun (xs,ys) i => do
    let xi ← xs[i]?
    let yi ← ys[i]?
    pure (xi, yi)
  getElem! := fun {d} (xs,ys) i => 
    have : Inhabited α := ⟨d.1.1⟩
    have : Inhabited β := ⟨d.1.2⟩
    (xs[i]!, ys[i]!)
  setElem := fun (xs,ys) i (x,y) => (setElem xs i x, setElem ys i y)
  length := fun (xs,ys) => length α xs
  empty := (ArrayType.empty α, ArrayType.empty β)
  null := fun (xs, ys) => (null α xs, null β ys)
--  build := fun elems => sorry
  append := fun (xs, ys) (x,y) => 
    let (xs',i) := append xs x
    let (ys',_) := append ys y
    ((xs',ys'), i)
  insert := fun (xs,ys) (x,y) i => (insert xs x i, insert ys y i)
  remove := fun (xs,ys) i => (remove α xs i, remove β ys i)
  clear := fun (xs,ys) => (clear α xs, clear β ys)
  extend := fun (xs,ys) (xs',ys') => (extend α xs xs', extend β ys ys')
  reverse := fun (xs,ys) => (reverse α xs, reverse β ys)
  fromApex := fun (xs,ys) => 
    let xs : Array α := fromApex xs
    let ys : Array β := fromApex ys
    xs.zip ys
  toApex := fun xys => xys.unzip.map toApex toApex


-- I think find function does not make sense
-- just use `Array.find?` and hopefully it will get compiled to Apex correctly
-- instance : ArrayFind

open ArraySum in
instance [ArraySum α As] [ArraySum β Bs] : ArraySum (α×β) (As×Bs) where
  arraySum := fun (xs,ys) => (arraySum xs, arraySum ys)

open ArrayLerp in
instance [ArrayLerp α As] [ArrayLerp β Bs] : ArrayLerp (α×β) (As×Bs) where
  arrayLerp := fun (xs,ys) (xs',ys') t => (arrayLerp α xs xs' t, arrayLerp β ys ys' t)

open ArrayMin in
instance [ArrayMin α As] [ArrayMin β Bs] : ArrayMin (α×β) (As×Bs) where
  arrayMin := fun (xs,ys) => (((arrayMin xs).1, (arrayMin ys).1), -1)

open ArrayMax in
instance [ArrayMax α As] [ArrayMax β Bs] : ArrayMax (α×β) (As×Bs) where
  arrayMax := fun (xs,ys) => (((arrayMax xs).1, (arrayMax ys).1), -1)


-- Arithmetics
open ArrayAdd in
instance [ArrayAdd α As] [ArrayAdd β Bs] : ArrayAdd (α×β) (As × Bs) where
  arrayAdd := fun (x,y) (x',y') => (arrayAdd α x x', arrayAdd β y y')

open ArraySub in
instance [ArraySub α As] [ArraySub β Bs] : ArraySub (α×β) (As × Bs) where
  arraySub := fun (x,y) (x',y') => (arraySub α x x', arraySub β y y')

open ArrayMul in
instance [ArrayMul α As] [ArrayMul β Bs] : ArrayMul (α×β) (As × Bs) where
  arrayMul := fun (x,y) (x',y') => (arrayMul α x x', arrayMul β y y')

open ArrayDiv in
instance [ArrayDiv α As] [ArrayDiv β Bs] : ArrayDiv (α×β) (As × Bs) where
  arrayDiv := fun (x,y) (x',y') => (arrayDiv α x x', arrayDiv β y y')
