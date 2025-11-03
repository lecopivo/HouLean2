import Lean.Data.Json

open Lean

namespace HouLean.LeanGraph

inductive PortType where
  | builtin (name type : String)
  | struct  (name type : String) (subports : Array PortType)
deriving ToJson, FromJson

structure NodeType where
  name : String
  inputs : Array PortType
  outputs : Array PortType
deriving ToJson, FromJson


def builtinPortTypes : Array PortType := #[
  .builtin "value" "Float",
  .builtin "value" "Int",
  .builtin "value" "String",
  .struct "v" "Vector3" #[
    .builtin "x" "Float", 
    .builtin "y" "Float", 
    .builtin "z" "Float"],
  .struct "Cd" "Color" #[
    .builtin "r" "Float", 
    .builtin "g" "Float", 
    .builtin "b" "Float"],
  .struct "particle" "Particle" #[
    .builtin "pos" "Vector3", 
    .builtin "vel" "Vector3"],
  .struct "character" "Character" #[
    .builtin "head" "Particle", 
    .builtin "vel" "Vector3"]
  ]

def builtinNodeTypes : Array NodeType := #[
  { name := "Float_Add"
    inputs := #[
      .builtin "a" "Float",
      .builtin "b" "Float"]
    outputs := #[
      .builtin "add" "Float"]},
  { name := "Vector3_Add"
    inputs := #[
      .builtin "a" "Vector3",
      .builtin "b" "Vector3"]
    outputs := #[
      .builtin "add" "Vector3"]},
  { name := "Vector3_Length"
    inputs := #[
      .builtin "vector" "Vector3"]
    outputs := #[
      .builtin "len" "Float"]},
  { name := "Vector3_Length"
    inputs := #[
      .builtin "vector" "Vector3"]
    outputs := #[
      .builtin "len" "Float"]},
  { name := "Particle_Create",
    inputs := #[
      .builtin "position" "Vector3",
      .builtin "velocity" "Vector3"]
    outputs := #[
      .builtin "particle" "Particle"] }
  ]



  -- .builtin "value" "Float",
  -- .builtin "value" "Int",
  -- .builtin "value" "String",
  -- .struct "v" "Vector3" #[
  --   .builtin "x" "Float", 
  --   .builtin "y" "Float", 
  --   .builtin "z" "Float"],
  -- .struct "Cd" "Color" #[
  --   .builtin "r" "Float", 
  --   .builtin "g" "Float", 
  --   .builtin "b" "Float"],
  -- .struct "particle" "Particle" #[
  --   .builtin "pos" "Vector3", 
  --   .builtin "vel" "Vector3"],
  -- .struct "character" "Character" #[
  --   .builtin "head" "Particle", 
  --   .builtin "vel" "Vector3"]
  -- ]



def builtin
