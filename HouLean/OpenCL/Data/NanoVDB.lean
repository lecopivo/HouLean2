import HouLean.Math
import HouLean.Data.Defs
import HouLean.Data.Float
import HouLean.Data.Vector
import HouLean.OpenCL.Basic

namespace HouLean.OpenCL

-- global void *
/-- Represents a pointer to NanoVDB grid with values of type `α` -/
opaque NanoVDB (α : Type) : Type

namespace NanoVDB



abbrev Vec3F := CArray Float32 3
abbrev Vec3D := CArray Float32 3

-- cnanovdb_coord
abbrev Coord := CArray Int32 3

-- global void *
abbrev NanoVDBPtr := Pointer Unit

-- cnanovdb_readaccessor
opaque ReadAccessor : Type

-- cnanovdb_getreadaccessor
opaque getReadAccessor (grid : Pointer Unit) (acc : Pointer ReadAccessor) : OpenCLM Unit

-- cnanovdb_readaccessor_isActiveF
opaque ReadAccessor.isActiveF (acc : Pointer ReadAccessor) (g_coord : Pointer Coord) : OpenCLM Bool

-- cnanovdb_readaccessor_isActiveF3
opaque ReadAccessor.isActiveF3 (acc : Pointer ReadAccessor) (g_coord : Pointer Coord) : OpenCLM Bool

-- cnanovdb_readaccessor_isActiveF
opaque ReadAccessor.setValueF (acc : Pointer ReadAccessor) (g_coord : Pointer Coord) (val : Float32) : OpenCLM Bool


#exit

open HouLean Math

-- Data alignment constants
def DATA_ALIGNMENT : Nat := 32

-- def alignmentPadding (x n : Nat) : Nat :=
--   ((-x : Int) &&& ((n : Int) - 1)).toNat

-- Grid type enumeration
inductive GridType where
  | Unknown : GridType
  | Float   : GridType
  | Double  : GridType
  | Int16   : GridType
  | Int32   : GridType
  | Int64   : GridType
  | Vec3f   : GridType
  | Vec3d   : GridType
  | Mask    : GridType
  | FP16    : GridType
  | End     : GridType
deriving Repr, BEq, DecidableEq

def GridType.toNat : GridType → Nat
  | Unknown => 0
  | Float   => 1
  | Double  => 2
  | Int16   => 3
  | Int32   => 4
  | Int64   => 5
  | Vec3f   => 6
  | Vec3d   => 7
  | Mask    => 8
  | FP16    => 9
  | End     => 10

def ROOT_LEVEL : Nat := 3

-- Mask structures using Vector
structure Mask (log2dim : Nat) where
  words : CArray UInt64 ((1 <<< (3 * log2dim)) >>> 6)

def Mask.size {log2dim} (m : Mask log2dim) : Nat := (1 <<< (3 * log2dim))
def Mask.wordsNum {log2dim} (m : Mask log2dim) : Nat := (1 <<< (3 * log2dim)) >>> 6
def Mask.clear {log2dim : Nat} (mask : Mask log2dim) : OpenCLM Unit :=
  for i in [0:mask.wordsNum] do
    mask.words.data.set i.toUInt64 0
def Mask.isOn {log2dim : Nat} (mask : Mask log2dim) (n : UInt32) : OpenCLM Bool := do
  let idx := n >>> 6
  let word ← mask.words.data.get idx.toUInt64
  return (0:UInt64) != (word &&& (1 <<< (n &&& 63).toUInt64))

-- Vector type aliases
abbrev Vec3F := Vector Float 3
abbrev Vec3D := Vector Float 3  -- Using Float for simplicity, can be changed
abbrev Coord := Vector Int32 3

-- Coordinate operations
namespace Coord

def compare (a b : Coord) : Ordering :=
  if a.x < b.x then .lt
  else if a.x > b.x then .gt
  else if a.y < b.y then .lt
  else if a.y > b.y then .gt
  else if a.z < b.z then .lt
  else if a.z > b.z then .gt
  else .eq

-- Single root key implementation
def toKey (ijk : Coord) : UInt64 :=
  let x := ijk.x.toUInt32
  let y := ijk.y.toUInt32
  let z := ijk.z.toUInt32
  (z >>> 12).toUInt64 |||
  ((y >>> 12).toUInt64 <<< 21) |||
  ((x >>> 12).toUInt64 <<< 42)

end Coord

#exit

namespace Map

-- Helper to multiply 3x3 matrix by vector
private def mulMatVec (m : Vector Float 9) (v : Vec3F) : Vec3F :=
  #v[m[0] * v.x + m[1] * v.y + m[2] * v.z,
     m[3] * v.x + m[4] * v.y + m[5] * v.z,
     m[6] * v.x + m[7] * v.y + m[8] * v.z]

def apply (map : Map) (src : Vec3F) : Vec3F :=
  mulMatVec map.matF src + map.vecF

def applyInverse (map : Map) (src : Vec3F) : Vec3F :=
  mulMatVec map.invMatF (src - map.vecF)

def applyJacobi (map : Map) (src : Vec3F) : Vec3F :=
  mulMatVec map.matF src

def applyInverseJacobi (map : Map) (src : Vec3F) : Vec3F :=
  mulMatVec map.invMatF src

def applyIJT (map : Map) (src : Vec3F) : Vec3F :=
  #v[src.x * map.invMatF[0] + src.y * map.invMatF[3] + src.z * map.invMatF[6],
     src.x * map.invMatF[1] + src.y * map.invMatF[4] + src.z * map.invMatF[7],
     src.x * map.invMatF[2] + src.y * map.invMatF[5] + src.z * map.invMatF[8]]

end Map

-- Grid blind metadata
structure GridBlindMetadata where
  byteOffset   : Int64
  elementCount : UInt64
  flags        : UInt32
  semantic     : UInt32
  dataClass    : UInt32
  dataType     : UInt32
  name         : String
deriving Repr

-- Grid data structure
structure GridData where
  magic                : UInt64
  checksum             : UInt64
  version              : UInt32
  flags                : UInt32
  gridIndex            : UInt32
  gridCount            : UInt32
  gridSize             : UInt64
  gridName             : String
  map                  : Map
  bbox                 : Vector Float 6  -- min.x, min.y, min.z, max.x, max.y, max.z
  voxelSize            : Vec3F
  gridClass            : UInt32
  gridType             : GridType
  blindMetadataOffset  : UInt64
  blindMetadataCount   : Int32
deriving Repr

namespace GridData

def bboxMin (grid : GridData) : Vec3F :=
  #v[grid.bbox[0], grid.bbox[1], grid.bbox[2]]

def bboxMax (grid : GridData) : Vec3F :=
  #v[grid.bbox[3], grid.bbox[4], grid.bbox[5]]

def worldToIndex (grid : GridData) (src : Vec3F) : Vec3F :=
  grid.map.applyInverse src

def indexToWorld (grid : GridData) (src : Vec3F) : Vec3F :=
  grid.map.apply src

def worldToIndexDir (grid : GridData) (src : Vec3F) : Vec3F :=
  grid.map.applyInverseJacobi src

def indexToWorldDir (grid : GridData) (src : Vec3F) : Vec3F :=
  grid.map.applyJacobi src

def applyIJT (grid : GridData) (src : Vec3F) : Vec3F :=
  grid.map.applyIJT src

end GridData

-- Tree data structure
structure TreeData where
  nodeOffset : Vector UInt64 (ROOT_LEVEL + 1)
  nodeCount  : Vector UInt32 ROOT_LEVEL
  tileCount  : Vector UInt32 ROOT_LEVEL
  voxelCount : UInt64
deriving Repr

-- Read accessor for cached access
structure ReadAccessor (α : Type) where
  key    : Coord
  node0  : Option α
  node1  : Option α
  node2  : Option α
  root   : α
deriving Repr

namespace ReadAccessor

def init (root : α) : ReadAccessor α :=
  { key := #v[0, 0, 0]
    node0 := none
    node1 := none
    node2 := none
    root := root }

def insert (acc : ReadAccessor α) (childLevel : Nat) (node : α) (ijk : Coord) : ReadAccessor α :=
  match childLevel with
  | 0 => { acc with node0 := some node, key := ijk }
  | 1 => { acc with node1 := some node, key := ijk }
  | 2 => { acc with node2 := some node, key := ijk }
  | _ => acc

def computeDirty (acc : ReadAccessor α) (ijk : Coord) : Int32 :=
  (ijk.x ^^^ acc.key.x) ||| (ijk.y ^^^ acc.key.y) ||| (ijk.z ^^^ acc.key.z)

def isCached0 (acc : ReadAccessor α) (dirty : Int32) : Bool :=
  acc.node0.isSome && (dirty &&& ~~~((1 <<< 3) - 1)) == 0

def isCached1 (acc : ReadAccessor α) (dirty : Int32) : Bool :=
  acc.node1.isSome && (dirty &&& ~~~((1 <<< 7) - 1)) == 0

def isCached2 (acc : ReadAccessor α) (dirty : Int32) : Bool :=
  acc.node2.isSome && (dirty &&& ~~~((1 <<< 12) - 1)) == 0

end ReadAccessor

-- Tile entry (union type)
inductive TileEntry (α : Type) where
  | value : α → TileEntry α
  | child : UInt64 → TileEntry α
deriving Repr

-- Leaf node (Level 0, LOG2DIM = 3, 8x8x8 = 512 voxels)
structure LeafNode (α β : Type) where
  bboxMin   : Coord
  bboxDif   : Vector UInt8 3
  flags     : UInt8
  valueMask : Mask3
  minimum   : α
  maximum   : α
  average   : β
  stdDevi   : β
  voxels    : Vector α 512  -- 8^3
deriving Repr

namespace LeafNode

def coordToOffset (ijk : Coord) : UInt32 :=
  let mask : Int32 := (1 <<< 3) - 1
  let x := (ijk.x &&& mask).toUInt32
  let y := (ijk.y &&& mask).toUInt32
  let z := (ijk.z &&& mask).toUInt32
  (x <<< 6) + (y <<< 3) + z

def getValue {α β : Type} (node : LeafNode α β) (ijk : Coord) : α :=
  let n := coordToOffset ijk
  node.voxels[n.toNat]

def setValue {α β : Type} (node : LeafNode α β) (ijk : Coord) (value : α) : LeafNode α β :=
  let n := coordToOffset ijk
  { node with voxels := node.voxels.set n.toNat value }

def isActive {α β : Type} (node : LeafNode α β) (ijk : Coord) : Bool :=
  let n := coordToOffset ijk
  node.valueMask.isOn n

end LeafNode

-- Internal node Level 1 (LOG2DIM = 4, 16x16x16 = 4096 children)
structure InternalNode1 (α β : Type) where
  bboxMin   : Coord
  bboxMax   : Coord
  offset    : Int32
  flags     : UInt32
  valueMask : Mask4
  childMask : Mask4
  minimum   : α
  maximum   : α
  average   : β
  stdDevi   : β
  table     : Vector (TileEntry α) 4096  -- 16^3
deriving Repr

namespace InternalNode1

def coordToOffset (ijk : Coord) : UInt32 :=
  let mask : Int32 := (1 <<< 7) - 1
  let shift : Int32 := 3
  let x := ((ijk.x &&& mask) >>> shift).toUInt32
  let y := ((ijk.y &&& mask) >>> shift).toUInt32
  let z := ((ijk.z &&& mask) >>> shift).toUInt32
  (x <<< 8) + (y <<< 4) + z

end InternalNode1

-- Internal node Level 2 (LOG2DIM = 5, 32x32x32 = 32768 children)
structure InternalNode2 (α β : Type) where
  bboxMin   : Coord
  bboxMax   : Coord
  offset    : Int32
  flags     : UInt32
  valueMask : Mask5
  childMask : Mask5
  minimum   : α
  maximum   : α
  average   : β
  stdDevi   : β
  table     : Vector (TileEntry α) 32768  -- 32^3
deriving Repr

namespace InternalNode2

def coordToOffset (ijk : Coord) : UInt32 :=
  let mask : Int32 := (1 <<< 12) - 1
  let shift : Int32 := 7
  let x := ((ijk.x &&& mask) >>> shift).toUInt32
  let y := ((ijk.y &&& mask) >>> shift).toUInt32
  let z := ((ijk.z &&& mask) >>> shift).toUInt32
  (x <<< 10) + (y <<< 5) + z

end InternalNode2

-- Root data tile
structure RootDataTile (α : Type) where
  key   : UInt64
  child : Int64
  state : UInt32
  value : α
deriving Repr

-- Root data structure
structure RootData (α β : Type) where
  bboxMin    : Coord
  bboxMax    : Coord
  tableSize  : UInt32
  background : α
  minimum    : α
  maximum    : α
  average    : β
  stdDevi    : β
  tiles      : Array (RootDataTile α)  -- Dynamic size
deriving Repr

namespace RootData

def getTile {α β : Type} (root : RootData α β) (n : Nat) : Option (RootDataTile α) :=
  root.tiles[n]?

def findTile {α β : Type} (root : RootData α β) (ijk : Coord) : Option (RootDataTile α) :=
  let key := ijk.toKey
  root.tiles.find? (fun tile => tile.key == key)

end RootData

-- Grid validation
def NANOVDB_MAGIC : UInt64 := 0x304244566f6e614e

namespace GridData

def isValid (grid : GridData) : Bool :=
  grid.magic == NANOVDB_MAGIC

def isValidFloat (grid : GridData) : Bool :=
  grid.isValid && grid.gridType == GridType.Float

def isValidVec3f (grid : GridData) : Bool :=
  grid.isValid && grid.gridType == GridType.Vec3f

def isValidInt32 (grid : GridData) : Bool :=
  grid.isValid && grid.gridType == GridType.Int32

end GridData

-- Convenient type aliases for common grid types
abbrev FloatLeafNode := LeafNode Float Float
abbrev Vec3fLeafNode := LeafNode Vec3F Float
abbrev Int32LeafNode := LeafNode Int32 Int32

abbrev FloatInternalNode1 := InternalNode1 Float Float
abbrev Vec3fInternalNode1 := InternalNode1 Vec3F Float
abbrev Int32InternalNode1 := InternalNode1 Int32 Int32

abbrev FloatInternalNode2 := InternalNode2 Float Float
abbrev Vec3fInternalNode2 := InternalNode2 Vec3F Float
abbrev Int32InternalNode2 := InternalNode2 Int32 Int32

abbrev FloatRootData := RootData Float Float
abbrev Vec3fRootData := RootData Vec3F Float
abbrev Int32RootData := RootData Int32 Int32

end CNanoVDB
