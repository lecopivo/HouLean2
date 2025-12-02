
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
