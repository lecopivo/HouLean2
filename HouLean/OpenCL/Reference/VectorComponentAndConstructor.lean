import HouLean.OpenCL.Compiler.RewriteRules

open HouLean OpenCL Compiler Math

namespace HouLean.OpenCL

variable {α : Type} [AtomicOpenCLType α] {n : Nat}

-- -- todo: to changes to explicit projections .x, .y, .z, .w, .s4, .s5, ...
-- impl_by {α : Type} [AtomicOpenCLType α] {n : Nat} (u : Vector α n) (i : Nat) (h) :
--     getElem u i h ==> getElem(u,i)


open Lean Meta Compiler3


def indexToSuffix? (i : Nat) : Option Name :=
  match i with
  | 0 => `x
  | 1 => `y
  | 2 => `z
  | 3 => `w
  | 4 => `s4
  | 5 => `s5
  | 6 => `s6
  | 7 => `s7
  | 8 => `s8
  | 9 => `s9
  | 10 => `sa
  | 11 => `sb
  | 12 => `sc
  | 13 => `sd
  | 14 => `se
  | 15 => `sf
  | _ => none


impl_by (u : Vector α n) (i : Nat) (h) : u[i]'h ==> do

  let some i ← runInterpreter? Nat i
    | throwError m!"Must know {i} at compile time when compiling `{u}[{i}]`"
  let u ← compileExpr u

  let some suffix := indexToSuffix? i
    | throwError m!"Invalid index {i}, has to be smaller than 16!"
  let idx := mkIdent suffix

  return ← `(oclExpr| $u:oclExpr.$idx:ident)

-- normalize `getElem` from indexing with `Fin n` to indexing with `Nat`
attribute [opencl_csimp] Fin.getElem_fin

----------------------------------------------------------------------------------------------------
--



/- Vector contructor rule  e.g. #v[x,y,z] ==> (float3){x,y,z}

The notation `#v[x,y,z]` stands for `Vector.mk [x,y,z].toArray h` and this rule matches on this
irrespective of the length of the list.
 -/
set_option pp.all true in
impl_by {n : Nat} {α : Type} [AtomicOpenCLType α] (l : List α) (h) :
    Vector.mk (n:=n) l.toArray h ==> do

  let xs ← splitList l
  let xs ← xs.mapM compileExpr
  let t ← compileType α

  return ← `(oclExpr| ($t:ident){$xs:oclExpr,*} )


-- def genericKernel
--       {npts : Nat, dim : Nat}
--       [FloatType R]
--       (timeStep : R,
--        P : Attribute .point (Vector R dim) (size := npts) (write := true),
--        v : Attribute .point (Vector R dim) (size := npts)):

--     i = getGlobalWorkId()
--     if not (i < npts):
--       return

--     pos = P[i]
--     P[i] += dt * v[i]


-- -- vector 2
-- impl_by (x y : Nat) : #v[x,y] ==> (uint3){x,y}
-- impl_by (x y : Int) : #v[x,y] ==> (int3){x,y}
-- impl_by (x y : UInt16) : #v[x,y] ==> (ushort3){x,y}
-- impl_by (x y :  Int16) : #v[x,y] ==>  (short3){x,y}
-- impl_by (x y : UInt32) : #v[x,y] ==> (uint3){x,y}
-- impl_by (x y :  Int32) : #v[x,y] ==>  (int3){x,y}
-- impl_by (x y : UInt64) : #v[x,y] ==> (ulong3){x,y}
-- impl_by (x y :  Int64) : #v[x,y] ==>  (long3){x,y}
-- impl_by (x y : Float32) : #v[x,y] ==> (float3){x,y}
-- impl_by (x y : Float64) : #v[x,y] ==> (double3){x,y}

-- -- vector 3
-- impl_by (x y z : Nat) : #v[x,y,z] ==> (uint3){x,y,z}
-- impl_by (x y z : Int) : #v[x,y,z] ==> (int3){x,y,z}
-- impl_by (x y z : UInt16) : #v[x,y,z] ==> (ushort3){x,y,z}
-- impl_by (x y z :  Int16) : #v[x,y,z] ==>  (short3){x,y,z}
-- impl_by (x y z : UInt32) : #v[x,y,z] ==> (uint3){x,y,z}
-- impl_by (x y z :  Int32) : #v[x,y,z] ==>  (int3){x,y,z}
-- impl_by (x y z : UInt64) : #v[x,y,z] ==> (ulong3){x,y,z}
-- impl_by (x y z :  Int64) : #v[x,y,z] ==>  (long3){x,y,z}
-- impl_by (x y z : Float32) : #v[x,y,z] ==> (float3){x,y,z}
-- impl_by (x y z : Float64) : #v[x,y,z] ==> (double3){x,y,z}

-- -- vector 4
-- impl_by (x y z w : Nat) : #v[x,y,z,w] ==> (uint4){x,y,z,w}
-- impl_by (x y z w : Int) : #v[x,y,z,w] ==> (int4){x,y,z,w}
-- impl_by (x y z w : UInt16) : #v[x,y,z,w] ==> (ushort4){x,y,z,w}
-- impl_by (x y z w :  Int16) : #v[x,y,z,w] ==>  (short4){x,y,z,w}
-- impl_by (x y z w : UInt32) : #v[x,y,z,w] ==> (uint4){x,y,z,w}
-- impl_by (x y z w :  Int32) : #v[x,y,z,w] ==>  (int4){x,y,z,w}
-- impl_by (x y z w : UInt64) : #v[x,y,z,w] ==> (ulong4){x,y,z,w}
-- impl_by (x y z w :  Int64) : #v[x,y,z,w] ==>  (long4){x,y,z,w}
-- impl_by (x y z w : Float32) : #v[x,y,z,w] ==> (float4){x,y,z,w}
-- impl_by (x y z w : Float64) : #v[x,y,z,w] ==> (double4){x,y,z,w}
