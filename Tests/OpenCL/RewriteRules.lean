import HouLean.OpenCL.Compiler.RewriteRules


open HouLean OpenCL Compiler Math



variable {R} {m n : Nat}
set_option pp.funBinderTypes true

variable (x y z : R) (u v : Vector R n)

-- Operators And Qualifiers

-- plus
impl_by [AtomicOpenCLType R] [Add R] : x + y ==> x + y
impl_by [AtomicOpenCLType R] [AllowedVectorSize n] [Add R] : u + v ==> u + v

-- minus
impl_by [AtomicOpenCLType R] [Sub R] : x - y ==> x - y
impl_by [AtomicOpenCLType R] [AllowedVectorSize n] [Sub R] : u - v ==> u - v

impl_by [AtomicOpenCLType R] [Neg R] : - x ==> - x
impl_by [AtomicOpenCLType R] [AllowedVectorSize n] [Neg R] : - u ==> - u

-- mul
impl_by [AtomicOpenCLType R] [Mul R] : x * y ==> x * y
impl_by [AtomicOpenCLType R] [AllowedVectorSize n] [Mul R] : x * u ==> x * u

-- div
impl_by [AtomicOpenCLType R] [Div R] : x / y ==> x / y

-- '--'
-- missing

-- '++'
-- missing

-- '=='
impl_by (a b : Bool) : a == b ==> a == b
impl_by [AtomicOpenCLType R] [BEq R] : x == y ==> x == y
impl_by [AtomicOpenCLType R] [AllowedVectorSize n] [BEq R] : u == v ==> u == v

-- `!=`
impl_by (a b : Bool) : a != b ==> a != b
impl_by [AtomicOpenCLType R] [BEq R] : x != y ==> x != y
impl_by [AtomicOpenCLType R] [AllowedVectorSize n] [BEq R] : u != v ==> u != v

-- '&'
impl_by [AtomicOpenCLType R] [AndOp R] : x &&& y ==> x & y



impl_by (b : Bool) : decide (b = true) ==> b


impl_by [AtomicOpenCLType R] [Mod R] : x % y ==> x % y
impl_by [AtomicOpenCLType R] [Mod R] : x % y ==> x % y

impl_by [AtomicOpenCLType R] [ShiftRight R] : x >>> y ==> x >> y
impl_by [AtomicOpenCLType R] [ShiftLeft R]  : x <<< y ==> x << y
impl_by [AtomicOpenCLType R] [AndOp R]      : x &&& y ==> x & y
impl_by [AtomicOpenCLType R] [OrOp R]       : x ||| y ==> x | y

impl_by (b : Bool) : decide (b = true) ==> b
impl_by (b : Bool) : decide (b = false) ==> !b
impl_by (a b : Bool) : a && b ==> a && b
impl_by (a b : Bool) : a || b ==> a || b




impl_by (P Q : Prop) [Decidable P] [Decidable Q] : decide (P ∧ Q) ==> P && Q

section Math
open Math

variable [FloatType R] (x y : R)

impl_by : sin x ==> sin(x)
impl_by : cos x ==> cos(x)
impl_by : exp x ==> exp(x)
impl_by : atan2 y x ==> atan2(y, x)


end Math

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


open Lean Meta Qq Compiler2
run_meta
  let e := q(fun x y : Float => x + atan2 (x + y) (sin (x + y)))
  lambdaTelescope e fun _ body => do
  let stx ← implementedBy body
  logInfo stx

open Lean Meta Qq Compiler2
run_meta
  let e := q(fun x y : Float => #v[x+x, x*x, x/y])
  lambdaTelescope e fun _ body => do
  let stx ← implementedBy body
  logInfo stx


open Lean Meta
def identFromString (s : String) : Ident := mkIdent (Name.mkSimple s)


impl_by [AtomicOpenCLType R] : Vector R n ==> do
  let type ← implementedBy R
  let typeName := toString type
  let id := identFromString (typeName ++ toString n)
  return ← `(oclExpr| $id:ident)

impl_by [t : AtomicOpenCLType R] : Matrix R m n ==> do
  let type ← implementedBy R
  let typeName := toString type
  let id := identFromString ("matrix" ++ typeName ++ toString m ++ toString n)
  return ← `(oclExpr| $id:ident)

impl_by : Int ==> int

-- impl_by {α β : Type} :
--   α × β
--   ==>
--   structure {
--     α fst;
--     β snd;
--   }


theorem float_add (x y : Float) : x + y = ocl%( x + y ) := sorry_proof
theorem atomic_type_add {R} [AtomicOpenCLType R] [Add R] [Inhabited R] (x y : R) :
    x + y = ocl%( x + y ) := sorry_proof
theorem float_type_sin {R} [FloatType R] (x : R) :
    sin x = ocl%( sin(x) ) := sorry_proof

-- theorem vector_mk (ls : List α) (h : ls.toArray.size = n) :
--     Vector.mk ls.toArray h = ocl%( (α){ ls,* } ) := sorry_proof

theorem float_ocl : Float32 = ocl%( float ) := sorry_proof
theorem double_ocl : Float64 = ocl%( double ) := sorry_proof
theorem nat_ocl : Nat = ocl%( uint ) := sorry_proof
theorem int_ocl : Int = ocl%( int ) := sorry_proof
-- theorem prod_ocl {α β : Type} :
--   (α × β)
--   =
--   ocl%( structure {
--            α fst;
--            β snd;
--         }) := sorry_proof


theorem float_add' (x y : Float) : x + y = oclFunction' (no_simp (·+·)) " + " .infix x y := by rfl
theorem atomic_type_add' {R} [AtomicOpenCLType R] [Add R] (x y : R) :
    x + y = oclFunction' (no_simp (·+·)) " + " .infix x y := by rfl
theorem float_type_sin' {R} [FloatType R] (x : R) :
    sin x = (oclFunction' (no_simp sin) "sin") x := by rfl
