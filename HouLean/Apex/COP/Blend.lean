import HouLean.Apex.Cop
import HouLean.Apex.Lean.Array
import HouLean.Apex.Generated.Nodes
import HouLean.Meta.ParamStruct

namespace HouLean.COP

open Apex Generated

#houdini_params_from_file "HouLean/Apex/COP/Parameters/cop_blend_parameters.json"

-- #print BlendParams

def blend (bg : Layer) (fg? mask? : Option Layer) (parms : BlendParams) : CopM Layer := do

  let fg := fg?.getD bg.defaultWithSameType
  let mask := mask?.getD bg.defaultMonoWithSameDim

  let bgSig := bg.signature
  let fgSig := fg?.map (·.signature) |>.getD s!"{bgSig}!"
  let maskSig := fg?.map (·.signature) |>.getD s!"{bgSig}!"

  let inputSig := bgSig ++ fgSig ++ maskSig
  let outputSig := bgSig

  let cwd_node := ""
  let requests := toApex #[(2:Int)] -- TODO: figure out what to do about requests!
  let parms := parms.toDict

  match bg, fg, mask with
  | .image .float bg, .image .float fg, .image .float mask =>
    let sig := toApex #["f1", "Mono", inputSig, outputSig]
    return .image .float (cop_blend0 bg fg mask sig cwd_node requests parms (← read))

  | .image .vector2 bg, .image .vector2 fg, .image .float mask =>
    let sig := toApex #["f2", "UV", inputSig, outputSig]
    return .image .vector2 (cop_blend0 bg fg mask sig cwd_node requests parms (← read))

  | .image .vector3 bg, .image .vector3 fg, .image .float mask =>
    let sig := toApex #["f3", "RGB", inputSig, outputSig]
    return .image .vector3 (cop_blend0 bg fg mask sig cwd_node requests parms (← read))

  | .image .vector4 bg, .image .vector4 fg, .image .float mask =>
    let sig := toApex #["f4", "RGBA", inputSig, outputSig]
    return .image .vector4 (cop_blend0 bg fg mask sig cwd_node requests parms (← read))

  | .image .int bg, .image .int fg, .image .float mask =>
    let sig := toApex #["i", "ID", inputSig, outputSig]
    return .image .int (cop_blend0 bg fg mask sig cwd_node requests parms (← read))

  | .volume .float bg, .volume .float fg, .volume .float mask =>
    let sig := toApex #["F", "Float VDB", inputSig, outputSig]
    return .volume .float (cop_blend1 bg fg mask sig cwd_node requests parms (← read))

  | .volume .float bg, .volume .vector3 fg, .volume .float mask =>
    let outputSig := fgSig
    let sig := toApex #["FV", "Float and Vector VDB", inputSig, outputSig]
    return .volume .vector3 (cop_blend1 bg fg mask sig cwd_node requests parms (← read))

  | .volume .vector3 bg, .volume .vector3 fg, .volume .float mask =>
    let outputSig := fgSig
    let sig := toApex #["V", "Vector VDB", inputSig, outputSig]
    return .volume .vector3 (cop_blend1 bg fg mask sig cwd_node requests parms (← read))

  | .volume .vector3 bg, .volume .float fg, .volume .float mask =>
    let sig := toApex #["VF", "Vector and Float VDB", inputSig, outputSig]
    return .volume .vector3 (cop_blend1 bg fg mask sig cwd_node requests parms (← read))

  | .volume .int bg, .volume .int fg, .volume .float mask =>
    let sig := toApex #["I", "Integer VDB", inputSig, outputSig]
    return .volume .vector3 (cop_blend1 bg fg mask sig cwd_node requests parms (← read))

  | _, _, _ => panic! s!"Invalid inputs to {decl_name%}"
