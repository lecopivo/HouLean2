import HouLean.Apex.Cop
import HouLean.Apex.Lean.Array
import HouLean.Apex.Generated.Nodes
import HouLean.Meta.ParamStruct

namespace HouLean.COP

open Apex Generated

#houdini_params_from_file "HouLean/Apex/COP/Parameters/cop_layertogeo_parameters.json"

-- #print LayertogeoParams

def layerToGeo (layer : Layer) (parms : LayertogeoParams) : CopM Geometry := do

  let inputSig := layer.signature
  let outputSig := "g"

  let cwd_node := ""
  let requests := toApex #[(2:Int)] -- TODO: figure out what to do about requests!
  let parms := parms.toDict

  match layer with
  | .image .float layer =>
    let sig := toApex #["f1", "Mono", inputSig, outputSig]
    return cop_layertogeo0 layer sig cwd_node requests parms (← read)

  | .image .vector2 layer =>
    let sig := toApex #["f2", "UV", inputSig, outputSig]
    return cop_layertogeo0 layer sig cwd_node requests parms (← read)

  | .image .vector3 layer =>
    let sig := toApex #["f3", "RGB", inputSig, outputSig]
    return cop_layertogeo0 layer sig cwd_node requests parms (← read)

  | .image .vector4 layer =>
    let sig := toApex #["f4", "RGBA", inputSig, outputSig]
    return cop_layertogeo0 layer sig cwd_node requests parms (← read)

  | .image .int layer =>
    let sig := toApex #["i", "ID", inputSig, outputSig]
    return cop_layertogeo0 layer sig cwd_node requests parms (← read)

  | .volume .float volume =>
    let sig := toApex #["F", "Float VDB", inputSig, outputSig]
    return cop_layertogeo1 volume sig cwd_node requests parms (← read)

  | .volume .vector3 volume =>
    let sig := toApex #["V", "Vector VDB", inputSig, outputSig]
    return cop_layertogeo1 volume sig cwd_node requests parms (← read)

  | .volume .int volume =>
    let sig := toApex #["I", "Integer VDB", inputSig, outputSig]
    return cop_layertogeo1 volume sig cwd_node requests parms (← read)
