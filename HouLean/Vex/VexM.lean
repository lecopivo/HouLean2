import HouLean.Apex.Geometry

namespace HouLean

open Apex

structure VexState where
  /-- Input geometry at index 0 -/
  geo : Geometry

structure VexCommonContext where
  /-- Input geometry at index 1,2,3 -/
  geo : Vector Geometry 3
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
  
abbrev VexM := ReaderT VexContext <| StateM VexState


def getGeo? (i : Int) : VexM (Option Geometry) := do
  if i = 0 then
    return (← get).geo
  else if _h : i < 4 then
    return panic! "getGeo? not fully implemented!"
  else
    return none

namespace Vex


def point (geo : Int) (attr : String) (ptnum : Int) : VexM Vector3 := do
  let some g ← getGeo? geo
    | return default
  return g.pointAttrib ptnum attr

def prim (geo : Int) (attr : String) (ptnum : Int) : VexM Vector3 := do
  let some g ← getGeo? geo
    | return default
  return g.pointAttrib ptnum attr

