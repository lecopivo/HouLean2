import HouLean.TypeTag

open Lean

-- todo: use this as the canonical Geometry type
namespace HouLean


inductive AttributeClass where
  | point | primitive | vertex | detail

namespace NewGeometry

structure Attribute (n : Nat) where
  type : AttribTypeTag
  data : Vector type n

inductive PrimType where
  | Agent
  | AlembicRef
  | BezierCurve
  | BezierSurface
  | Circle
  | ChannelPrim
  | Custom
  | Hexahedron
  | Mesh
  | Metaball
  | NURBSCurve
  | NURBSSurface
  | PackedFragment
  | PackedGeometry
  | PackedPrim
  | ParticleSystem
  | PastedSurface
  | Polygon
  | PolySoup
  | Sphere
  | Tetrahedron
  | TriangleBezier
  | TriangleFan
  | TriangleStrip
  | Tube
  | Unknown
  | VDB
  | Volume

structure Geometry where
  numPoints : Nat
  numPrims : Nat
  numVectices : Nat

  pointAttribs  : NameMap (Attribute numPoints)
  primAttribs   : NameMap (Attribute numPrims)
  vertexAttribs : NameMap (Attribute numVectices)
  detailAttribs : NameMap ((type : AttribTypeTag) × type)

  primTypes : Vector PrimType numPrims
  primVertices : Vector (Array (Fin numVertices)) numPrims
