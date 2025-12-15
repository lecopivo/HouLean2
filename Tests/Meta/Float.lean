import HouLean.Meta.Float

namespace Test.Meta.Float


-- ============================================================================
-- Basic values
-- ============================================================================

/-- info: some (false, 0, 0) -/
#guard_msgs in
#eval (0.0 : Float).toScientific

/-- info: some (false, 1, 0) -/
#guard_msgs in
#eval (1.0 : Float).toScientific

/-- info: some (true, 1, 0) -/
#guard_msgs in
#eval (-1.0 : Float).toScientific

/-- info: some (false, 5, -1) -/
#guard_msgs in
#eval (0.5 : Float).toScientific

/-- info: some (false, 25, -2) -/
#guard_msgs in
#eval (0.25 : Float).toScientific

-- ============================================================================
-- Integers and large numbers
-- ============================================================================

/-- info: some (false, 1, 1) -/
#guard_msgs in
#eval (10.0 : Float).toScientific

/-- info: some (false, 1, 2) -/
#guard_msgs in
#eval (100.0 : Float).toScientific

/-- info: some (false, 1, 3) -/
#guard_msgs in
#eval (1000.0 : Float).toScientific

/-- info: some (false, 12345, 0) -/
#guard_msgs in
#eval (12345.0 : Float).toScientific

/-- info: some (false, 1, 6) -/
#guard_msgs in
#eval (1e6 : Float).toScientific

/-- info: some (false, 123456789, 0) -/
#guard_msgs in
#eval (123456789.0 : Float).toScientific

-- ============================================================================
-- Small decimals
-- ============================================================================

/-- info: some (false, 1, -1) -/
#guard_msgs in
#eval (0.1 : Float).toScientific

/-- info: some (false, 1, -2) -/
#guard_msgs in
#eval (0.01 : Float).toScientific

/-- info: some (false, 1, -3) -/
#guard_msgs in
#eval (0.001 : Float).toScientific

/-- info: some (false, 1, -6) -/
#guard_msgs in
#eval (1e-6 : Float).toScientific

/-- info: some (false, 1, -10) -/
#guard_msgs in
#eval (1e-10 : Float).toScientific

-- ============================================================================
-- Mixed decimals
-- ============================================================================

/-- info: some (false, 12345679, -3) -/
#guard_msgs in
#eval (12345.679 : Float).toScientific

/-- info: some (false, 314159265358979, -14) -/
#guard_msgs in
#eval (3.14159265358979 : Float).toScientific

/-- info: some (false, 271828, -5) -/
#guard_msgs in
#eval (2.71828 : Float).toScientific

/-- info: some (false, 123, -5) -/
#guard_msgs in
#eval (0.00123 : Float).toScientific

-- ============================================================================
-- Scientific notation inputs
-- ============================================================================

/-- info: some (false, 3144, -12) -/
#guard_msgs in
#eval (3144e-12 : Float).toScientific

/-- info: some (false, 3144, -15) -/
#guard_msgs in
#eval (3.144e-12 : Float).toScientific

/-- info: some (false, 1003, -9) -/
#guard_msgs in
#eval (0.01003e-4 : Float).toScientific

/-- info: some (false, 602214076, -32) -/
#guard_msgs in
#eval (6.02214076e-24 : Float).toScientific

/-- info: some (false, 602214076, 0) -/
#guard_msgs in
#eval (6.02214076e8 : Float).toScientific

/-- info: some (false, 602214076, 7) -/
#guard_msgs in
#eval (6.02214076e15 : Float).toScientific

-- ============================================================================
-- Negative numbers
-- ============================================================================

/-- info: some (true, 42, 0) -/
#guard_msgs in
#eval (-42.0 : Float).toScientific

/-- info: some (true, 314159, -5) -/
#guard_msgs in
#eval (-3.14159 : Float).toScientific

/-- info: some (true, 1, -6) -/
#guard_msgs in
#eval (-1e-6 : Float).toScientific

/-- info: some (true, 999, -5) -/
#guard_msgs in
#eval (-0.00999 : Float).toScientific

-- ============================================================================
-- Special values
-- ============================================================================

/-- info: none -/
#guard_msgs in
#eval (Float.NaN : Float).toScientific

/-- info: none -/
#guard_msgs in
#eval (Float.inf : Float).toScientific

/-- info: none -/
#guard_msgs in
#eval (-Float.inf : Float).toScientific

-- ============================================================================
-- Edge cases / precision boundaries
-- ============================================================================

/-- info: some (false, 1, 0) -/
#guard_msgs in
#eval (0.9999999999999999 : Float).toScientific

/-- info: some (false, 1, -1) -/
#guard_msgs in
#eval (0.09999999999999999 : Float).toScientific

/-- info: some (false, 1, -15) -/
#guard_msgs in
#eval (1e-15 : Float).toScientific

/-- info: some (false, 1, 13) -/
#guard_msgs in
#eval (1e13 : Float).toScientific

-- ============================================================================
-- Roundtrip verification helper
-- ============================================================================

/-- Verify that toScientific produces values that reconstruct the original -/
def verifyRoundtrip (x : Float) : Bool :=
  match x.toScientific with
  | none => x.isNaN || x.isInf
  | some (neg, m, e) =>
    let reconstructed := (if neg then -1.0 else 1.0) * m.toFloat * (10.0 ^ e.toInt64.toFloat)
    let relError := if x == 0.0 then reconstructed.abs else ((reconstructed - x) / x).abs
    relError < 1e-10

#guard verifyRoundtrip 0.0
#guard verifyRoundtrip 1.0
#guard verifyRoundtrip (-1.0)
#guard verifyRoundtrip 3.14159
#guard verifyRoundtrip 1e-10
#guard verifyRoundtrip 1e10
#guard verifyRoundtrip (-123.456e-7)
#guard verifyRoundtrip 0.001
#guard verifyRoundtrip 1000.0
#guard verifyRoundtrip Float.NaN
#guard verifyRoundtrip Float.inf


-- ============================================================================
-- Basic values
-- ============================================================================

/-- info: some (false, 0, 0) -/
#guard_msgs in
#eval (0.0 : Float32).toScientific

/-- info: some (false, 1, 0) -/
#guard_msgs in
#eval (1.0 : Float32).toScientific

/-- info: some (true, 1, 0) -/
#guard_msgs in
#eval (-1.0 : Float32).toScientific

/-- info: some (false, 5, -1) -/
#guard_msgs in
#eval (0.5 : Float32).toScientific

/-- info: some (false, 25, -2) -/
#guard_msgs in
#eval (0.25 : Float32).toScientific

-- ============================================================================
-- Integers and large numbers
-- ============================================================================

/-- info: some (false, 1, 1) -/
#guard_msgs in
#eval (10.0 : Float32).toScientific

/-- info: some (false, 1, 2) -/
#guard_msgs in
#eval (100.0 : Float32).toScientific

/-- info: some (false, 1, 3) -/
#guard_msgs in
#eval (1000.0 : Float32).toScientific

/-- info: some (false, 12345, 0) -/
#guard_msgs in
#eval (12345.0 : Float32).toScientific

/-- info: some (false, 1, 6) -/
#guard_msgs in
#eval (1e6 : Float32).toScientific

-- ============================================================================
-- Small decimals
-- ============================================================================

/-- info: some (false, 1, -1) -/
#guard_msgs in
#eval (0.1 : Float32).toScientific

/-- info: some (false, 1, -2) -/
#guard_msgs in
#eval (0.01 : Float32).toScientific

/-- info: some (false, 1, -3) -/
#guard_msgs in
#eval (0.001 : Float32).toScientific

/-- info: some (false, 1, -6) -/
#guard_msgs in
#eval (1e-6 : Float32).toScientific

-- ============================================================================
-- Mixed decimals
-- ============================================================================

/-- info: some (false, 1234568, -2) -/
#guard_msgs in
#eval (12345.68 : Float32).toScientific

/-- info: some (false, 3141593, -6) -/
#guard_msgs in
#eval (3.141593 : Float32).toScientific

/-- info: some (false, 271828, -5) -/
#guard_msgs in
#eval (2.71828 : Float32).toScientific

/-- info: some (false, 123, -5) -/
#guard_msgs in
#eval (0.00123 : Float32).toScientific

-- ============================================================================
-- Scientific notation inputs
-- ============================================================================

/-- info: some (false, 3144, -12) -/
#guard_msgs in
#eval (3144e-12 : Float32).toScientific

/-- info: some (false, 3144, -15) -/
#guard_msgs in
#eval (3.144e-12 : Float32).toScientific

/-- info: some (false, 1003, -9) -/
#guard_msgs in
#eval (0.01003e-4 : Float32).toScientific

-- ============================================================================
-- Negative numbers
-- ============================================================================

/-- info: some (true, 42, 0) -/
#guard_msgs in
#eval (-42.0 : Float32).toScientific

/-- info: some (true, 314159, -5) -/
#guard_msgs in
#eval (-3.14159 : Float32).toScientific

/-- info: some (true, 1, -6) -/
#guard_msgs in
#eval (-1e-6 : Float32).toScientific

/-- info: some (true, 999, -5) -/
#guard_msgs in
#eval (-0.00999 : Float32).toScientific

-- ============================================================================
-- Special values
-- ============================================================================

/-- info: none -/
#guard_msgs in
#eval (Float32.NaN : Float32).toScientific

/-- info: none -/
#guard_msgs in
#eval (Float32.inf : Float32).toScientific

/-- info: none -/
#guard_msgs in
#eval (-Float32.inf : Float32).toScientific

-- ============================================================================
-- Edge cases / precision boundaries (7 digits for Float32)
-- ============================================================================

/-- info: some (false, 9999999, -7) -/
#guard_msgs in
#eval (0.9999999 : Float32).toScientific

/-- info: some (false, 1, -7) -/
#guard_msgs in
#eval (1e-7 : Float32).toScientific

/-- info: some (false, 1, 7) -/
#guard_msgs in
#eval (1e7 : Float32).toScientific

-- ============================================================================
-- Roundtrip verification helper
-- ============================================================================

/-- Verify that toScientific produces values that reconstruct the original -/
def verifyRoundtrip32 (x : Float32) : Bool :=
  match x.toScientific with
  | none => x.isNaN || x.isInf
  | some (neg, m, e) =>
    let reconstructed := (if neg then -1.0 else 1.0) * m.toFloat * (10.0 ^ e.toInt64.toFloat)
    let xf := x.toFloat
    let relError := if xf == 0.0 then reconstructed.abs else ((reconstructed - xf) / xf).abs
    relError < 1e-7  -- Lower precision for Float32

#guard verifyRoundtrip32 0.0
#guard verifyRoundtrip32 1.0
#guard verifyRoundtrip32 (-1.0)
#guard verifyRoundtrip32 3.14159
#guard verifyRoundtrip32 1e-6
#guard verifyRoundtrip32 1e6
#guard verifyRoundtrip32 (-123.456e-5)
#guard verifyRoundtrip32 0.001
#guard verifyRoundtrip32 1000.0
#guard verifyRoundtrip32 Float32.NaN
#guard verifyRoundtrip32 Float32.inf
