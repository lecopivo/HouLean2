import HouLean.OpenCL.Compiler.RewriteRules

open HouLean OpenCL Compiler Math

namespace HouLean.OpenCL

variable {R} {m n : Nat} (x y z : R) (u v : Vector R n) (a b : Bool)

-- '+'
impl_by [AtomicOpenCLType R] [Add R] : x + y ==> x + y
impl_by [AtomicOpenCLType R] [AllowedVectorSize n] [Add R] : u + v ==> u + v

-- '-'
impl_by [AtomicOpenCLType R] [Sub R] : x - y ==> x - y
impl_by [AtomicOpenCLType R] [AllowedVectorSize n] [Sub R] : u - v ==> u - v

impl_by [AtomicOpenCLType R] [Neg R] : - x ==> - x
impl_by [AtomicOpenCLType R] [AllowedVectorSize n] [Neg R] : - u ==> - u

-- '*'
impl_by [AtomicOpenCLType R] [Mul R] : x * y ==> x * y
impl_by [AtomicOpenCLType R] [AllowedVectorSize n] [Mul R] : x * u ==> x * u

-- '%'
impl_by [AtomicOpenCLType R] [Mod R] : x % y ==> x % y

-- '/'
impl_by [AtomicOpenCLType R] [Div R] : x / y ==> x / y


-- '--'

-- '++'

-- '=='
impl_by : a == b ==> a == b
impl_by [AtomicOpenCLType R] [BEq R] : x == y ==> x == y
impl_by [AtomicOpenCLType R] [AllowedVectorSize n] [BEq R] : u == v ==> u == v

impl_by : decide (b = true) ==> b
impl_by : decide (b = false) ==> !b
impl_by : decide (a = b) ==> a == b
impl_by [AtomicOpenCLType R] [DecidableEq R] : decide (x = y) ==> a == b
impl_by [AtomicOpenCLType R] [AllowedVectorSize n] [DecidableEq R] : decide (u = v) ==> u == v

-- `!=`
impl_by (a b : Bool) : a != b ==> a != b
impl_by [AtomicOpenCLType R] [BEq R] : x != y ==> x != y
impl_by [AtomicOpenCLType R] [AllowedVectorSize n] [BEq R] : u != v ==> u != v

-- '&'
impl_by [AtomicOpenCLType R] [AndOp R] : x &&& y ==> x & y

-- '~'

-- '^'
impl_by [AtomicOpenCLType R] [AndOp R] : x &&& y ==> x & y

-- '>'
impl_by : decide (a > b) ==> a > b
impl_by [AtomicOpenCLType R] [LT R] [DecidableLT R] : decide (x > y) ==> x > y

-- '<'
impl_by : decide (a < b) ==> a < b
impl_by [AtomicOpenCLType R] [LT R] [DecidableLT R] : decide (x < y) ==> x < y


-- '>='
impl_by : decide (a ≥ b) ==> a >= b
impl_by [AtomicOpenCLType R] [LE R] [DecidableLE R] : decide (x ≥ y) ==> x >= y

-- '<='
impl_by : decide (a ≤ b) ==> a <= b
impl_by [AtomicOpenCLType R] [LE R] [DecidableLE R] : decide (x ≤ y) ==> x <= y

-- '|'
impl_by [AtomicOpenCLType R] [OrOp R] : x ||| y ==> x | y

-- '!'
impl_by : !a ==> !a
impl_by : decide (¬a) ==> !a
@[opencl_csimp]
theorem decide_not (P : Prop) [Decidable P] : decide (¬P) = !(decide P) := by simp only [_root_.decide_not]

-- '&&'
impl_by (a b : Bool) : a && b ==> a && b
@[opencl_csimp]
theorem decide_and {P Q} [Decidable P] [Decidable Q] : (decide (P ∧ Q)) = (decide P && decide Q) := sorry_proof

-- '||'
impl_by (a b : Bool) : a || b ==> a || b
@[opencl_csimp]
theorem decide_or {P Q} [Decidable P] [Decidable Q] : (decide (P ∨ Q)) = (decide P || decide Q) := sorry_proof


-- '?:'

-- '>>'
impl_by [AtomicOpenCLType R] [ShiftRight R] : x >>> y ==> x >> y

-- '<<'
impl_by [AtomicOpenCLType R] [ShiftLeft R]  : x <<< y ==> x << y

-- '='

-- ','

-- 'op='

-- 'sizeof'



---- move the stuff bellow
