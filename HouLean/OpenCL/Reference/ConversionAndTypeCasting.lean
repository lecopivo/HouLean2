import HouLean.OpenCL.Compiler

namespace HouLean.OpenCL

-- It is unclear if these translations are really faithful
-- do they work the same whne translating signed to unsigned,
-- int to float, float to int, ...?

impl_by (x : Nat) : Int.ofNat x  ==> (int)(x)
impl_by (x : Nat) : x.toUInt16  ==> (ushort)(x)
impl_by (x : Nat) : x.toInt16  ==> (short)(x)
impl_by (x : Nat) : x.toUInt32  ==> (uint)(x)
impl_by (x : Nat) : x.toInt32  ==> (int)(x)
impl_by (x : Nat) : x.toUInt64  ==> (ulong)(x)
impl_by (x : Nat) : x.toInt64  ==> (long)(x)
impl_by (x : Nat) : x.toFloat32  ==> (float)(x)
impl_by (x : Nat) : x.toFloat  ==> (double)(x)


impl_by (x : Int) : x.toNat  ==> (uint)(x)
impl_by (x : Int) : x.toInt16  ==> (short)(x)
impl_by (x : Int) : x.toInt32  ==> (int)(x)
impl_by (x : Int) : x.toInt64  ==> (long)(x)


impl_by (x : UInt32) : x.toNat  ==> (uint)(x)
impl_by (x : UInt32) : x.toUInt16  ==> (ushort)(x)
impl_by (x : UInt32) : x.toInt32  ==> (int)(x)
impl_by (x : UInt32) : x.toUInt64  ==> (ulong)(x)
impl_by (x : UInt32) : x.toFloat32  ==> (float)(x)
impl_by (x : UInt32) : x.toFloat  ==> (double)(x)


impl_by (x : Int32) : x.toInt  ==> (int)(x)
impl_by (x : Int32) : x.toInt16  ==> (short)(x)
impl_by (x : Int32) : x.toUInt32  ==> (uint)(x)
impl_by (x : Int32) : x.toInt64  ==> (long)(x)
impl_by (x : Int32) : x.toFloat32  ==> (float)(x)
impl_by (x : Int32) : x.toFloat  ==> (double)(x)


impl_by (x : UInt32) : x.toNat  ==> (uint)(x)
impl_by (x : UInt32) : x.toUInt16  ==> (ushort)(x)
impl_by (x : UInt32) : x.toInt32  ==> (int)(x)
impl_by (x : UInt32) : x.toUInt64  ==> (ulong)(x)
impl_by (x : UInt32) : x.toFloat32  ==> (float)(x)
impl_by (x : UInt32) : x.toFloat  ==> (double)(x)


impl_by (x : Int64) : x.toInt  ==> (int)(x)
impl_by (x : Int64) : x.toInt16  ==> (short)(x)
impl_by (x : Int64) : x.toInt32  ==> (int)(x)
impl_by (x : Int64) : x.toUInt64  ==> (ulong)(x)
impl_by (x : Int64) : x.toFloat32  ==> (float)(x)
impl_by (x : Int64) : x.toFloat  ==> (double)(x)


impl_by (x : USize) : x.toNat  ==> (uint)(x)
impl_by (x : USize) : x.toUInt16  ==> (ushort)(x)
impl_by (x : USize) : x.toUInt32  ==> (uint)(x)
impl_by (x : USize) : x.toUInt64  ==> (ulong)(x)
impl_by (x : USize) : x.toFloat32  ==> (float)(x)
impl_by (x : USize) : x.toFloat  ==> (double)(x)


impl_by (x : Float32) : x.toUInt16  ==> (ushort)(x)
impl_by (x : Float32) : x.toInt16  ==> (short)(x)
impl_by (x : Float32) : x.toUInt32  ==> (uint)(x)
impl_by (x : Float32) : x.toInt32  ==> (int)(x)
impl_by (x : Float32) : x.toUInt64  ==> (ulong)(x)
impl_by (x : Float32) : x.toInt64  ==> (long)(x)
impl_by (x : Float32) : x.toFloat  ==> (double)(x)


impl_by (x : Float64) : x.toUInt16  ==> (ushort)(x)
impl_by (x : Float64) : x.toInt16  ==> (short)(x)
impl_by (x : Float64) : x.toUInt32  ==> (uint)(x)
impl_by (x : Float64) : x.toInt32  ==> (int)(x)
impl_by (x : Float64) : x.toUInt64  ==> (ulong)(x)
impl_by (x : Float64) : x.toInt64  ==> (long)(x)
impl_by (x : Float64) : x.toFloat32  ==> (float)(x)
