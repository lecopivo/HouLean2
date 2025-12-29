import Lean
import Qq
import HouLean.Meta.RunInterpreter

open Lean Meta Elab Term

/-- Symbolic representation of float expressions -/
inductive FloatExpr where
  | val (x : Float)
  | atom (x : Expr)
  | add (x y : FloatExpr)
  | sub (x y : FloatExpr)
  | mul (x y : FloatExpr)
  | div (x y : FloatExpr)
  | neg (x : FloatExpr)
  | inv (x : FloatExpr)
  deriving Inhabited, Repr

namespace FloatExpr

/-- Try to extract a Float literal from an Expr -/
def getFloatLit? (e : Expr) : MetaM (Option Float) :=
  HouLean.Meta.runInterpreter? Float e

/-- Parse a Lean Expr into FloatExpr -/
partial def fromExpr (e : Expr) : MetaM FloatExpr := do
  let e ← whnfR e
  -- Check for float literal first
  if let some f ← getFloatLit? e then
    return .val f
  -- Binary operations
  if e.isAppOfArity ``HAdd.hAdd 6 then
    let x ← fromExpr e.getAppArgs[4]!
    let y ← fromExpr e.getAppArgs[5]!
    return .add x y
  if e.isAppOfArity ``HSub.hSub 6 then
    let x ← fromExpr e.getAppArgs[4]!
    let y ← fromExpr e.getAppArgs[5]!
    return .sub x y
  if e.isAppOfArity ``HMul.hMul 6 then
    let x ← fromExpr e.getAppArgs[4]!
    let y ← fromExpr e.getAppArgs[5]!
    return .mul x y
  if e.isAppOfArity ``HDiv.hDiv 6 then
    let x ← fromExpr e.getAppArgs[4]!
    let y ← fromExpr e.getAppArgs[5]!
    return .div x y
  -- Unary operations
  if e.isAppOfArity ``Neg.neg 3 then
    let x ← fromExpr e.getAppArgs[2]!
    return .neg x
  if e.isAppOfArity ``Inv.inv 3 then
    let x ← fromExpr e.getAppArgs[2]!
    return .inv x
  -- Fallback to atom
  return .atom e

/-! ## Monomial Representation

A monomial is `coeff * (atom₁^exp₁ * atom₂^exp₂ * ...)`
We represent atoms by their index in a list, and exponents can be negative (for division).
-/

/-- A monomial: coefficient and map from atom index to exponent -/
structure Monomial where
  coeff : Float
  /-- Map from atom index to exponent (can be negative) -/
  factors : List (Nat × Int)
  deriving Inhabited, Repr

/-- A polynomial as a list of monomials plus atom table -/
structure Polynomial where
  atoms : Array Expr
  monomials : List Monomial
  deriving Inhabited

namespace Polynomial

def zero : Polynomial := ⟨#[], []⟩

def const (c : Float) : Polynomial :=
  ⟨#[], [{ coeff := c, factors := [] }]⟩

def single (e : Expr) : Polynomial :=
  ⟨#[e], [{ coeff := 1.0, factors := [(0, 1)] }]⟩

/-- Find or insert an atom, returning its index -/
def findOrInsertAtom (p : Polynomial) (e : Expr) : Polynomial × Nat := Id.run do
  for i in [:p.atoms.size] do
    if p.atoms[i]! == e then
      return (p, i)
  ({ p with atoms := p.atoms.push e }, p.atoms.size)

/-- Merge two atom tables, returning merged polynomial and index mapping for second -/
def mergeAtoms (p q : Polynomial) : Polynomial × (Nat → Nat) := Id.run do
  let mut result := p
  let mut mapping : Array Nat := #[]
  for atom in q.atoms do
    let (r, idx) := result.findOrInsertAtom atom
    result := r
    mapping := mapping.push idx
  (result, fun i => mapping[i]!)

/-- Remap atom indices in a monomial -/
def remapMonomial (m : Monomial) (f : Nat → Nat) : Monomial :=
  { m with factors := m.factors.map fun (i, e) => (f i, e) }

/-- Normalize monomial factors (sort and combine like terms) -/
def normalizeFactors (factors : List (Nat × Int)) : List (Nat × Int) :=
  let grouped := factors.foldl (init := Std.HashMap.empty (α := Nat) (β := Int))
    fun acc (i, e) => acc.insert i (acc.getD i 0 + e)
  grouped.toList
    |>.filter (·.2 != 0)
    |>.mergeSort (·.1 < ·.1)

/-- Multiply two monomials -/
def mulMonomial (m1 m2 : Monomial) : Monomial :=
  { coeff := m1.coeff * m2.coeff
    factors := normalizeFactors (m1.factors ++ m2.factors) }

/-- Add two polynomials -/
def add (p q : Polynomial) : Polynomial :=
  let (merged, remap) := mergeAtoms p q
  let qMonomials := q.monomials.map (remapMonomial · remap)
  { merged with monomials := p.monomials ++ qMonomials }

/-- Negate a polynomial -/
def neg (p : Polynomial) : Polynomial :=
  { p with monomials := p.monomials.map fun m => { m with coeff := -m.coeff } }

/-- Subtract two polynomials -/
def sub (p q : Polynomial) : Polynomial := p.add q.neg

/-- Multiply two polynomials -/
def mul (p q : Polynomial) : Polynomial :=
  let (merged, remap) := mergeAtoms p q
  let qMonomials := q.monomials.map (remapMonomial · remap)
  let newMonomials := p.monomials.flatMap fun m1 =>
    qMonomials.map fun m2 => mulMonomial m1 m2
  { merged with monomials := newMonomials }

/-- Scale polynomial by constant -/
def scale (p : Polynomial) (c : Float) : Polynomial :=
  { p with monomials := p.monomials.map fun m => { m with coeff := m.coeff * c } }

/-- Invert exponents in factors (for 1/x) -/
def invertFactors (factors : List (Nat × Int)) : List (Nat × Int) :=
  factors.map fun (i, e) => (i, -e)

/-- Compare factor lists lexicographically -/
def compareFactors (a b : List (Nat × Int)) : Bool :=
  match a, b with
  | [], [] => false
  | [], _ => true
  | _, [] => false
  | (i1, e1) :: rest1, (i2, e2) :: rest2 =>
    if i1 < i2 then true
    else if i1 > i2 then false
    else if e1 < e2 then true
    else if e1 > e2 then false
    else compareFactors rest1 rest2

/-- Collect like monomials (same factors → add coefficients) -/
def collectLikeTerms (p : Polynomial) : Polynomial := Id.run do
  let mut termMap : Std.HashMap (List (Nat × Int)) Float := {}
  for m in p.monomials do
    let key := normalizeFactors m.factors
    termMap := termMap.insert key (termMap.getD key 0 + m.coeff)
  let monomials : List Monomial := termMap.toList
    |>.filter (·.2 != 0)
    |>.map (fun (factors, coeff) => {coeff, factors : Monomial})
    |>.mergeSort (fun a b => compareFactors a.factors b.factors)
  { p with monomials }


/-- Pretty print a polynomial for debugging -/
def toString (p : Polynomial) : String := Id.run do
  if p.monomials.isEmpty then return "0"
  let mut parts : List String := []
  for m in p.monomials do
    let mut s := s!"{m.coeff}"
    for (i, exp) in m.factors do
      let atomStr := s!"x{i}"
      if exp == 1 then
        s := s ++ s!" * {atomStr}"
      else
        s := s ++ s!" * {atomStr}^{exp}"
    parts := parts ++ [s]
  return " + ".intercalate parts

/-- Build x^n as an Expr (handles negative exponents via division) -/
def mkPow (base : Expr) (exp : Int) : MetaM Expr := do
  if exp == 0 then
    return toExpr 1.0
  else if exp == 1 then
    return base
  else if exp == -1 then
    mkAppM ``Inv.inv #[base]
  else if exp > 0 then
    let exp := toExpr exp.toNat.toFloat
    mkAppM ``HPow.hPow #[base,exp]
    -- let mut result := base
    -- for _ in [1:exp.toNat] do
    --   result ← mkAppM ``HMul.hMul #[result, base]
    -- return result
  else -- exp < -1
    let mut result := base
    for _ in [1:(-exp).toNat] do
      result ← mkAppM ``HMul.hMul #[result, base]
    mkAppM ``Inv.inv #[result]


/-- Convert a monomial to Expr -/
def monomialToExpr (m : Monomial) (atoms : Array Expr) : MetaM Expr := do
  let mut result := toExpr m.coeff
  for (i, exp) in m.factors do
    if h : i < atoms.size then
      let factor ← mkPow atoms[i] exp
      result ← mkAppM ``HMul.hMul #[result, factor]
  return result

/-- Convert a polynomial back to Lean Expr -/
protected def toExpr (p : Polynomial) : MetaM Expr := do
  if p.monomials.isEmpty then
    return toExpr (0.0 : Float)
  else
    let mut result ← monomialToExpr p.monomials.head! p.atoms
    for m in p.monomials.tail! do
      let term ← monomialToExpr m p.atoms
      result ← mkAppM ``HAdd.hAdd #[result, term]
    return result


end Polynomial

/-- Convert FloatExpr to polynomial (sum of monomials).
    Takes the original Expr for fallback when we can't simplify division. -/
partial def toPolynomialAux (fe : FloatExpr) (origExpr? : Option Expr := none) : Polynomial :=
  match fe with
  | .val x => Polynomial.const x
  | .atom e => Polynomial.single e
  | .add x y => (toPolynomialAux x).add (toPolynomialAux y)
  | .sub x y => (toPolynomialAux x).sub (toPolynomialAux y)
  | .mul x y => (toPolynomialAux x).mul (toPolynomialAux y)
  | .div x y =>
      let px := toPolynomialAux x
      let py := toPolynomialAux y
      match py.monomials with
      | [m] =>
        -- Division by single monomial: distribute
        let invM : Monomial := {
          coeff := 1.0 / m.coeff
          factors := Polynomial.invertFactors m.factors
        }
        let (merged, remap) := Polynomial.mergeAtoms px py
        let invM' := Polynomial.remapMonomial invM remap
        { merged with
          monomials := px.monomials.map fun m1 =>
            Polynomial.mulMonomial m1 invM' }
      | _ =>
        -- Complex division: can't simplify, keep as single atom
        match origExpr? with
        | some e => Polynomial.single e
        | none => px
  | .neg x => (toPolynomialAux x).neg
  | .inv x =>
      let px := toPolynomialAux x
      match px.monomials with
      | [m] =>
        { px with monomials := [{
            coeff := 1.0 / m.coeff
            factors := Polynomial.invertFactors m.factors
          }] }
      | _ =>
        match origExpr? with
        | some e => Polynomial.single e
        | none => px

/-- Convert FloatExpr to polynomial (sum of monomials) -/
def toPolynomial (fe : FloatExpr) : Polynomial :=
  toPolynomialAux fe

/-- Simplify a FloatExpr to sum of monomials -/
def simplify (fe : FloatExpr) : Polynomial :=
  (toPolynomial fe).collectLikeTerms

end FloatExpr

open FloatExpr in
simproc_decl float_polynomial_expand (_) := fun e => do
  let t ← inferType e
  unless (← isDefEq t (.const ``Float [])) do
    return .continue

  let fexpr ← fromExpr e
  let p := fexpr.toPolynomial.collectLikeTerms
  let e ← p.toExpr

  return .done { expr := e }


/-! ## Tests -/

section Tests

open Lean Elab Term Command FloatExpr

elab "#polynomial_expand " e:term : command => do
  runTermElabM fun _ => do
    let e ← elabTermAndSynthesize e (Expr.const ``Float [])
    let fexpr ← fromExpr e
    let p := fexpr.toPolynomial.collectLikeTerms
    logInfo m!"{← p.toExpr}"

variable (x y z : Float)

/-! ### Basic Constants and Variables -/

/-- info: 0.5 -/
#guard_msgs in
#polynomial_expand 0.5

/-- info: 0.6 * x -/
#guard_msgs in
#polynomial_expand 0.5 * x + x * 0.1

/-- info: 1 * x -/
#guard_msgs in
#polynomial_expand x

/-- info: 7 -/
#guard_msgs in
#polynomial_expand 3.0 + 4.0

/-! ### Addition and Subtraction -/

/-- info: 2 * x -/
#guard_msgs in
#polynomial_expand x + x

/-- info: 5.3 * x -/
#guard_msgs in
#polynomial_expand 2.0 * x + 3.3 * x

/-- info: 0 -/
#guard_msgs in
#polynomial_expand x - x

/-- info: 1 * x + 1 * y -/
#guard_msgs in
#polynomial_expand x + y

/-- info: 2 * x + -2 * y -/
#guard_msgs in
#polynomial_expand x - y + x - y

/-! ### Multiplication -/

/-- info: 1 * x * y -/
#guard_msgs in
#polynomial_expand x * y

/-- info: 1 * x ^ 2 -/
#guard_msgs in
#polynomial_expand x * x

/-- info: 6 * x * y -/
#guard_msgs in
#polynomial_expand 2.0 * x * 3.0 * y

/-- info: 12 -/
#guard_msgs in
#polynomial_expand 3.0 * 4.0

/-! ### Polynomial Expansion -/

/-- info: 2 * x * y + 1 * x ^ 2 + 1 * y ^ 2 -/
#guard_msgs in
#polynomial_expand (x + y) * (x + y)

/-- info: 1 * x ^ 2 + -1 * y ^ 2 -/
#guard_msgs in
#polynomial_expand (x + y) * (x - y)

/-- info: 1 * x * z + 1 * y * z -/
#guard_msgs in
#polynomial_expand (x + y) * z

/-- info: 1 * x * y + 2 * x ^ 2 + -3 * y ^ 2 -/
#guard_msgs in
#polynomial_expand (2.0 * x + 3.0 * y) * (x - y)

/-- info: 2 * x * y + 2 * x * z + 1 * x ^ 2 + 2 * y * z + 1 * y ^ 2 + 1 * z ^ 2 -/
#guard_msgs in
#polynomial_expand (x + y + z) * (x + y + z)

/-! ### Division -/

/-- info: 1 * x * y⁻¹ -/
#guard_msgs in
#polynomial_expand x / y

/-- info: 1 -/
#guard_msgs in
#polynomial_expand x / x

/-- info: 3 * x -/
#guard_msgs in
#polynomial_expand (6.0 * x) / 2.0

/-! ### Negation and Inversion -/

/-- info: -1 * x -/
#guard_msgs in
#polynomial_expand -x

/-- info: 1 * x -/
#guard_msgs in
#polynomial_expand -(-x)

/-- info: 1 * x⁻¹ -/
#guard_msgs in
#polynomial_expand x⁻¹

/-- info: 1 * x -/
#guard_msgs in
#polynomial_expand (x⁻¹)⁻¹

/-- info: 1 -/
#guard_msgs in
#polynomial_expand x * x⁻¹

/-! ### Edge Cases -/

/-- info: 0 -/
#guard_msgs in
#polynomial_expand x * (y - y)

/-- info: 1 * x -/
#guard_msgs in
#polynomial_expand x + 0.0

/-- info: 0 -/
#guard_msgs in
#polynomial_expand 0.0 * x

/-! ### Higher Powers -/

/-- info: 4 * x * y ^ 3 + 6 * x ^ 2 * y ^ 2 + 4 * x ^ 3 * y + 1 * x ^ 4 + 1 * y ^ 4 -/
#guard_msgs in
#polynomial_expand (x + y) * (x + y) * (x + y) * (x + y)

end Tests
