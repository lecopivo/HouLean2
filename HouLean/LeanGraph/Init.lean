import HouLean.LeanGraph.Extension
import HouLean.Apex.Geometry
import HouLean.Apex.Sop
import HouLean.Math
import HouLean.Meta.AnonymousStruct
import HouLean.Apex.Visualizer.Basic
import HouLean.Apex.Lean.Prod

-- todo: once lean graph is more mature this should be redistributed to other files

namespace HouLean 

open Apex


attribute [lean_graph_type_builtin] 
  Float Int Nat String Array Geometry Option

attribute [lean_graph_type] 
  Vector3 Matrix3

attribute [lean_graph_node] forLoop
attribute [lean_graph_node] forLoopM
attribute [lean_graph_node] ifThenElse

attribute [lean_graph_node] id
attribute [lean_graph_node] Prod.mk
attribute [lean_graph_node] Prod.fst
attribute [lean_graph_node] Prod.snd
attribute [lean_graph_node "Prod_map"] prodMap
attribute [lean_graph_node "Prod_fold"] prodFold

attribute [lean_graph_node] visualize withVisualizer

attribute [lean_graph_node] Float.add
attribute [lean_graph_node] Float.sub
attribute [lean_graph_node] Float.mul
attribute [lean_graph_node] Float.div
attribute [lean_graph_node] Float.neg
attribute [lean_graph_node] Float.sin
attribute [lean_graph_node] Float.cos
attribute [lean_graph_node] Float.exp
attribute [lean_graph_node] Float.tan
attribute [lean_graph_node] Float.atan2

attribute [lean_graph_node] Vector3.add Vector3.sub Vector3.neg Vector3.smul Vector3.dot Vector3.length Vector3.length2 Vector3.normalize Vector3.distance Vector3.distance2 Vector3.lerp  Vector3.compMul Vector3.compDiv Vector3.abs Vector3.min Vector3.max Vector3.sign Vector3.clamp Vector3.floor Vector3.ceil Vector3.round 
Vector3.toGeodetic Vector3.fromGeodetic Vector3.toSpherical Vector3.fromSpherical


attribute [lean_graph_node] Matrix3.add Matrix3.sub Matrix3.neg Matrix3.smul Matrix3.matmul Matrix3.mulVec Matrix3.vecMul Matrix3.dot Matrix3.length Matrix3.length2 Matrix3.normalize Matrix3.distance Matrix3.distance2 Matrix3.lerp  Matrix3.compMul Matrix3.compDiv Matrix3.abs Matrix3.min Matrix3.max Matrix3.sign Matrix3.clamp Matrix3.floor Matrix3.ceil Matrix3.round 


attribute [lean_graph_node "Arith_add"] HAdd.hAdd
attribute [lean_graph_node "Arith_sub"] HSub.hSub
attribute [lean_graph_node "Arith_mul"] HMul.hMul
attribute [lean_graph_node "Arith_div"] HDiv.hDiv
attribute [lean_graph_node "Arith_neg"] Neg.neg
attribute [lean_graph_node "Arith_inv"] Inv.inv
attribute [lean_graph_node "Arith_one"] One.one
attribute [lean_graph_node "Arith_zero"] Zero.zero
attribute [lean_graph_node "Arith_equal"] BEq.beq

open Math
attribute [lean_graph_node "Math_sin"] sin
attribute [lean_graph_node "Math_cos"] cos
attribute [lean_graph_node "Math_tan"] tan
attribute [lean_graph_node "Math_asin"] asin
attribute [lean_graph_node "Math_acos"] acos
attribute [lean_graph_node "Math_atan"] atan
attribute [lean_graph_node "Math_atan2"] atan2
attribute [lean_graph_node "Math_sinh"] sinh
attribute [lean_graph_node "Math_cosh"] cosh
attribute [lean_graph_node "Math_tanh"] tanh
attribute [lean_graph_node "Math_exp"] exp
attribute [lean_graph_node "Math_exp2"] exp2
attribute [lean_graph_node "Math_log"] log
attribute [lean_graph_node "Math_log2"] log2
attribute [lean_graph_node "Math_log10"] log10
attribute [lean_graph_node "Math_sqrt"] sqrt
attribute [lean_graph_node "Math_invsqrt"] invsqrt
attribute [lean_graph_node "Math_abs"] abs
attribute [lean_graph_node "Math_sign"] sign
attribute [lean_graph_node "Math_clamp"] clamp
attribute [lean_graph_node "Math_floor"] floor
attribute [lean_graph_node "Math_ceil"] ceil
attribute [lean_graph_node "Math_round"] round
attribute [lean_graph_node "Math_trunc"] trunc
attribute [lean_graph_node "Math_fract"] fract
attribute [lean_graph_node "Math_dot"] dot
attribute [lean_graph_node "Math_cross"] cross
attribute [lean_graph_node "Math_length"] length
attribute [lean_graph_node "Math_length2"] length2
attribute [lean_graph_node "Math_distance"] distance
attribute [lean_graph_node "Math_distance2"] distance2
attribute [lean_graph_node "Math_normalize"] normalize
attribute [lean_graph_node "Math_normalized"] normalized
attribute [lean_graph_node "Math_reflect"] reflect
attribute [lean_graph_node "Math_refract"] refract
attribute [lean_graph_node "Math_compMul"] compMul
attribute [lean_graph_node "Math_compDiv"] compDiv
attribute [lean_graph_node "Math_beq"] beq
attribute [lean_graph_node "Math_blt"] blt
attribute [lean_graph_node "Math_ble"] ble
attribute [lean_graph_node "Math_lerp"] lerp
attribute [lean_graph_node "Math_smoothstep"] smoothstep
attribute [lean_graph_node "Math_step"] step
attribute [lean_graph_node "Math_hermite"] hermite
attribute [lean_graph_node "Math_catmullRom"] catmullRom
attribute [lean_graph_node "Math_slerp"] slerp
attribute [lean_graph_node "Math_radians"] radians
attribute [lean_graph_node "Math_degrees"] degrees
attribute [lean_graph_node "Math_rgbToHsv"] rgbToHsv
attribute [lean_graph_node "Math_hsvToRgb"] hsvToRgb
attribute [lean_graph_node "Math_luminance"] luminance
attribute [lean_graph_node "Math_insideBox"] insideBox
attribute [lean_graph_node "Math_projectToSegment"] projectToSegment

-- attribute [lean_graph_node] GetElem.getElem
attribute [lean_graph_node] GetElem?.getElem?
attribute [lean_graph_node] GetElem?.getElem!

attribute [lean_graph_node] Array.get
attribute [lean_graph_node] Array.set!
attribute [lean_graph_node] Array.get?
attribute [lean_graph_node] Array.get!
attribute [lean_graph_node] Array.push
attribute [lean_graph_node] Array.append
attribute [lean_graph_node] Array.size
attribute [lean_graph_node] Array.size
attribute [lean_graph_node] Array.replicate
attribute [lean_graph_node] Array.empty


open Geometry in
attribute [lean_graph_node]
  Geometry.default
  numPoints
  numPrims
  detailAttrib
  detailAttrib
  setDetailAttrib
  pointAttrib
  pointAttrib
  setPointAttrib
  setPointAttribs
  setPointAttribsByName
  primAttrib
  primAttrib
  setPrimAttrib
  setPrimAttribs
  setPrimAttribsByName
  vertexAttrib
  vertexAttrib
  setVertexAttrib
  setVertexAttribs
  setVertexAttribsByName
  pointPrims
  primPoints
  globPoints
  globPrims
  merge
  mergePacked
  replace
  transform
  copyDetailAttrib
  dragPoints
  displacePoints
  smoothPoints
  addPacked
  extractPackedGeo
  updatePackedGeo
  boundingBox
  fromDisk
  lattice
  setAgentTransforms

open SOP in
attribute [lean_graph_node]
   xform
   box
   sphere
   grid
   scatter
   copyToPoints
   subdivide
   polyExtrude
   remesh
   blast
   fuse
   boolean
   error
   extractCentroid
   extractContours
   extractTransform
   facet
   file
   fit
   font
   fractal
   solidify
   sort
   splitPoints
   stash
   surfaceSplat
   sweep
   switch
   switchIf
   tetrahedralize
   tetrasurface
   texture



end HouLean
