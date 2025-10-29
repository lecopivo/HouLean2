import HouLean.Apex.Geometry

namespace HouLean

open Apex

structure VexCommonContext where
  geo : Vector Geometry 4

  -- bound variables
  time : Float
  timeInc : Float
  frame : Float


structure VexDetailContext extends VexCommonContext where

structure VexPrimContext extends VexCommonContext  where
  primnum : Int

structure VexPointContext extends VexCommonContext  where
  ptnum : Int

structure VexVertexContext extends VexCommonContext  where
  vtxnum : Int

structure VexVolumeContext extends VexCommonContext  where
  ix : Int
  iy : Int
  iz : Int
  resx : Int
  resy : Int
  resz : Int
  dPdx : Vector3
  dPdy : Vector3
  dPdz : Vector3
  
  center : Vector3
  prinnum : Int

inductive VexContext where
  | detail (ctx : VexDetailContext)
  | prim (ctx : VexPrimContext)
  | point (ctx : VexPointContext)
  | vertex (ctx : VexVertexContext)
  | volume (ctx : VexVolumeContext)
  
abbrev VexM := ReaderM VexContext
