import Lean

/-- Prints float at given precision. -/
def _root_.Float.toString' (x : Float) (precision : Nat := 15) : String :=
  if x == 0.0 then "0.0"
  else
    let isNeg := x < 0.0
    let absX := if isNeg then -x else x
    let sign := if isNeg then "-" else ""

    -- Get exponent and mantissa
    let exponent := absX.log10.floor.toInt64
    let mantissa := (absX / (10.0 ^ exponent.toFloat) * (10.0 ^ (precision - 1).toFloat)).round.toUInt64
    let mantissaStr := toString mantissa

    -- Determine if we should use scientific notation
    let useScientific := exponent < -4 || exponent >= precision.toInt64

    if useScientific then
      -- Scientific notation: X.YYYeZZ
      let fst := mantissaStr.take 1
      let snd := mantissaStr.drop 1
      let sndTrimmed := snd.dropRightWhile (· == '0')
      let decimal := if sndTrimmed.isEmpty then "" else s!".{sndTrimmed}"
      s!"{sign}{fst}{decimal}e{exponent}"
    else
      -- Regular notation
      let exp := exponent.toInt
      let totalDigits := mantissaStr.length

      if exp >= 0 then
        -- Number >= 1: add zeros or decimal point
        let intDigits := exp + 1
        if intDigits >= totalDigits then
          -- Need to add trailing zeros
          let zerosNeeded := intDigits - totalDigits
          s!"{sign}{mantissaStr}{'0' |> List.replicate zerosNeeded.toNat |> String.mk}.0"
        else
          -- Split at decimal point
          let fst := mantissaStr.take intDigits.toNat
          let snd := mantissaStr.drop intDigits.toNat
          let sndTrimmed := snd.dropRightWhile (· == '0')
          let decimal := if sndTrimmed.isEmpty then ".0" else s!".{sndTrimmed}"
          s!"{sign}{fst}{decimal}"
      else
        -- Number < 1: add leading zeros
        let zerosNeeded := (-exp - 1)
        let zeros := '0' |> List.replicate zerosNeeded.toNat |> String.mk
        let sndTrimmed := mantissaStr.dropRightWhile (· == '0')
        s!"{sign}0.{zeros}{sndTrimmed}"


instance (priority:=high) : ToString Float := ⟨Float.toString'⟩


def _root_.Float.NaN : Float := 0.0/0.0
def _root_.Float32.NaN : Float32 := 0.0/0.0

def _root_.Float.inf : Float := 1.0/0.0
def _root_.Float32.inf : Float32 := 1.0/0.0


/-- Converts a float to scientific notation components: (isNegative, mantissa, exponent).
    Returns `none` for NaN and infinity.
    The result satisfies: `x ≈ (if isNegative then -1 else 1) * mantissa * 10^exponent` -/
partial def Float.toScientific (x : Float) (precision : Nat := 15) : Option (Bool × Nat × Int) :=
  if x.isNaN || x.isInf then none
  else if x == 0.0 then some (false, 0, 0)
  else
    let isNeg := x < 0.0
    let absX := if isNeg then -x else x
    let exp := absX.log10.floor.toInt64
    let scaledMantissa := (absX / (10.0 ^ exp.toFloat) * (10.0 ^ (precision - 1).toFloat)).round.toUInt64
    let (mantissa, trailingZeros) := removeTrailingZeros scaledMantissa.toNat
    -- We have: mantissa * 10^(exp - (precision - 1) + trailingZeros)
    let exponent := exp.toInt - (precision - 1) + trailingZeros
    some (isNeg, mantissa, exponent)
where
  removeTrailingZeros (n : Nat) : Nat × Nat :=
    if n == 0 then (0, 0)
    else
      let rec go (m : Nat) (count : Nat) : Nat × Nat :=
        if m % 10 == 0 then go (m / 10) (count + 1)
        else (m, count)
      go n 0

/-- Converts a Float32 to scientific notation components: (isNegative, mantissa, exponent).
    Returns `none` for NaN and infinity.
    The result satisfies: `x ≈ (if isNegative then -1 else 1) * mantissa * 10^exponent` -/
partial def Float32.toScientific (x : Float32) (precision : Nat := 7) : Option (Bool × Nat × Int) :=
  if x.isNaN || x.isInf then none
  else if x == 0.0 then some (false, 0, 0)
  else
    let xf := x.toFloat
    let isNeg := xf < 0.0
    let absX := if isNeg then -xf else xf
    let exp := absX.log10.floor.toInt64
    let scaledMantissa := (absX / (10.0 ^ exp.toFloat) * (10.0 ^ (precision - 1).toFloat)).round.toUInt64
    let (mantissa, trailingZeros) := removeTrailingZeros scaledMantissa.toNat
    let exponent := exp.toInt - (precision - 1) + trailingZeros
    some (isNeg, mantissa, exponent)
where
  removeTrailingZeros (n : Nat) : Nat × Nat :=
    if n == 0 then (0, 0)
    else
      let rec go (m : Nat) (count : Nat) : Nat × Nat :=
        if m % 10 == 0 then go (m / 10) (count + 1)
        else (m, count)
      go n 0


open Lean Meta in
instance : ToExpr Float where
  toExpr x :=
    match x.toScientific with
    | some (neg, m, e) =>
      if 0 ≤ e && e < 1000000 then
        let n := mkApp3 (mkConst ``OfNat.ofNat [levelZero])
                         (.const ``Float [])
                         (.lit (.natVal (m * 10^e.toNat)))
                         ((Expr.const ``instOfNatFloat []).app (.lit (.natVal (m * 10^e.toNat))))
        if neg then
          mkApp3 (mkConst ``Neg.neg [levelZero]) (mkConst ``Float) (mkConst ``instNegFloat) n
        else
          n
      else
        let (expSign, expAbs) := if e < 0 then (true, (-e).toNat) else (false, e.toNat)
        let pos := mkApp5 (mkConst ``OfScientific.ofScientific [levelZero])
          (.const ``Float [])
          (.const ``instOfScientificFloat [])
          (.lit (.natVal m))
          (toExpr expSign)
          (.lit (.natVal expAbs))
        if neg then
          mkApp3 (mkConst ``Neg.neg [levelZero]) (mkConst ``Float) (mkConst ``instNegFloat) pos
        else
          pos
    | none =>
      if x.isNaN then mkConst ``Float.NaN
      else if x > 0 then mkConst ``Float.inf
      else mkApp3 (mkConst ``Neg.neg [levelZero]) (mkConst ``Float) (mkConst ``instNegFloat) (mkConst ``Float.inf)
  toTypeExpr := mkConst ``Float


open Lean Meta in
instance : ToExpr Float32 where
  toExpr x :=
    match x.toScientific with
    | some (neg, m, e) =>
      if 0 ≤ e && e < 1000000 then
        let n := mkApp3 (mkConst ``OfNat.ofNat [levelZero])
                         (.const ``Float32 [])
                         (.lit (.natVal (m * 10^e.toNat)))
                         ((Expr.const ``instOfNatFloat32 []).app (.lit (.natVal (m * 10^e.toNat))))
        if neg then
          mkApp3 (mkConst ``Neg.neg [levelZero]) (mkConst ``Float32) (mkConst ``instNegFloat32) n
        else
          n
      else
        let (expSign, expAbs) := if e < 0 then (true, (-e).toNat) else (false, e.toNat)
        let pos := mkApp5 (mkConst ``OfScientific.ofScientific [levelZero])
          (.const ``Float32 [])
          (.const ``instOfScientificFloat32 [])
          (.lit (.natVal m))
          (toExpr expSign)
          (.lit (.natVal expAbs))
        if neg then
          mkApp3 (mkConst ``Neg.neg [levelZero]) (mkConst ``Float32) (mkConst ``instNegFloat32) pos
        else
          pos
    | none =>
      if x.isNaN then mkConst ``Float32.NaN
      else if x > 0 then mkConst ``Float32.inf
      else mkApp3 (mkConst ``Neg.neg [levelZero]) (mkConst ``Float32) (mkConst ``instNegFloat32) (mkConst ``Float32.inf)
  toTypeExpr := mkConst ``Float32
