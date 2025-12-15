import HouLean.OpenCL.Compiler.RewriteRules
import HouLean.OpenCL.Reference.Operators

open Lean HouLean Meta OpenCL Compiler Compiler3 Math

namespace HouLean.OpenCL

open Compiler Qq


-- It is unclear if these translations are really faithful
-- do they work the same whne translating signed to unsigned,
-- int to float, float to int, ...?


-- Nat --> ??
impl_by (x : Nat) : Int.ofNat x  ==> (int)(x)
impl_by (x : Nat) : x.toUInt16  ==> (ushort)(x)
impl_by (x : Nat) : x.toInt16  ==> (short)(x)
impl_by (x : Nat) : x.toUInt32  ==> (uint)(x)
impl_by (x : Nat) : x.toInt32  ==> (int)(x)
impl_by (x : Nat) : x.toUInt64  ==> (ulong)(x)
impl_by (x : Nat) : x.toInt64  ==> (long)(x)
impl_by (x : Nat) : x.toFloat32  ==> (float)(x)
impl_by (x : Nat) : x.toFloat  ==> (double)(x)


-- Int --> ??
impl_by (x : Int) : x.toNat  ==> (uint)(x)
impl_by (x : Int) : x.toInt16  ==> (short)(x)
impl_by (x : Int) : x.toInt32  ==> (int)(x)
impl_by (x : Int) : x.toInt64  ==> (long)(x)


-- UInt32 --> ??
impl_by (x : UInt32) : x.toNat  ==> (uint)(x)
impl_by (x : UInt32) : x.toUInt16  ==> (ushort)(x)
impl_by (x : UInt32) : x.toInt32  ==> (int)(x)
impl_by (x : UInt32) : x.toUInt64  ==> (ulong)(x)
impl_by (x : UInt32) : x.toFloat32  ==> (float)(x)
impl_by (x : UInt32) : x.toFloat  ==> (double)(x)


-- Int32 --> ??
impl_by (x : Int32) : x.toInt  ==> (int)(x)
impl_by (x : Int32) : x.toInt16  ==> (short)(x)
impl_by (x : Int32) : x.toUInt32  ==> (uint)(x)
impl_by (x : Int32) : x.toInt64  ==> (long)(x)
impl_by (x : Int32) : x.toFloat32  ==> (float)(x)
impl_by (x : Int32) : x.toFloat  ==> (double)(x)


-- UInt64 --> ??
impl_by (x : UInt32) : x.toNat  ==> (uint)(x)
impl_by (x : UInt32) : x.toUInt16  ==> (ushort)(x)
impl_by (x : UInt32) : x.toInt32  ==> (int)(x)
impl_by (x : UInt32) : x.toUInt64  ==> (ulong)(x)
impl_by (x : UInt32) : x.toFloat32  ==> (float)(x)
impl_by (x : UInt32) : x.toFloat  ==> (double)(x)


-- Int64 --> ??
impl_by (x : Int64) : x.toInt  ==> (int)(x)
impl_by (x : Int64) : x.toInt16  ==> (short)(x)
impl_by (x : Int64) : x.toInt32  ==> (int)(x)
impl_by (x : Int64) : x.toUInt64  ==> (ulong)(x)
impl_by (x : Int64) : x.toFloat32  ==> (float)(x)
impl_by (x : Int64) : x.toFloat  ==> (double)(x)


-- Float32 --> ??
impl_by (x : Float32) : x.toUInt16  ==> (ushort)(x)
impl_by (x : Float32) : x.toInt16  ==> (short)(x)
impl_by (x : Float32) : x.toUInt32  ==> (uint)(x)
impl_by (x : Float32) : x.toInt32  ==> (int)(x)
impl_by (x : Float32) : x.toUInt64  ==> (ulong)(x)
impl_by (x : Float32) : x.toInt64  ==> (long)(x)
impl_by (x : Float32) : x.toFloat  ==> (double)(x)


-- Float64 --> ??
impl_by (x : Float64) : x.toUInt16  ==> (ushort)(x)
impl_by (x : Float64) : x.toInt16  ==> (short)(x)
impl_by (x : Float64) : x.toUInt32  ==> (uint)(x)
impl_by (x : Float64) : x.toInt32  ==> (int)(x)
impl_by (x : Float64) : x.toUInt64  ==> (ulong)(x)
impl_by (x : Float64) : x.toInt64  ==> (long)(x)
impl_by (x : Float64) : x.toFloat32  ==> (float)(x)
