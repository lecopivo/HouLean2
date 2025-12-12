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


-- '&&'
impl_by (a b : Bool) : a && b ==> a && b
example {P Q} [Decidable P] [Decidable Q] : decide (P ∧ Q) = decide P && decide Q := sorry_proof

-- '||'
impl_by (a b : Bool) : a || b ==> a || b
example {P Q} [Decidable P] [Decidable Q] : decide (P ∨ Q) = decide P || decide Q := sorry_proof


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


-- todo: how to generalize this ???
section
variable (x y z w : Float32)
impl_by :     #v[x,y] ==> (float2){x,y}
impl_by :   #v[x,y,z] ==> (float3){x,y,z}
impl_by : #v[x,y,z,w] ==> (float4){x,y,z,w}
end

section
variable (x y z w : Float64)
impl_by :     #v[x,y] ==> (double2){x,y}
impl_by :   #v[x,y,z] ==> (double3){x,y,z}
impl_by : #v[x,y,z,w] ==> (double4){x,y,z,w}
end

section
variable (x y z w : Int)
impl_by :     #v[x,y] ==> (int2){x,y}
impl_by :   #v[x,y,z] ==> (int3){x,y,z}
impl_by : #v[x,y,z,w] ==> (int4){x,y,z,w}
end

section
variable (x y z w : Nat)
impl_by :     #v[x,y] ==> (uint2){x,y}
impl_by :   #v[x,y,z] ==> (uint3){x,y,z}
impl_by : #v[x,y,z,w] ==> (uint4){x,y,z,w}
end


-- not sure if this is a way to go for types, but it looks nice ...
-- but for structures we want to do it automatically
-- the only exception are matrices whose OpenCL implementation does not match
impl_by : Float32 ==> float
impl_by : Float64 ==> double
impl_by :     Nat ==> uint
impl_by :     Int ==> int
impl_by :  UInt16 ==> ushort
impl_by :  UInt32 ==> uint
impl_by :  UInt64 ==> ulong
impl_by :   Int16 ==> short
impl_by :   Int32 ==> int
impl_by :   Int64 ==> long
