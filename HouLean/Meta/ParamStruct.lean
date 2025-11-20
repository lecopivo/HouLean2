import Lean
import HouLean.Apex.Basic
import HouLean.Apex.Dict

open Lean Elab Command Meta Parser

namespace HouLean.Houdini.ParamGen

-- Helper to convert snake_case to camelCase
def snakeToCamel (s : String) : String :=
  let parts := s.splitOn "_"
  match parts with
  | [] => ""
  | h :: t => h ++ String.join (t.map fun p => p.capitalize)

-- Helper to convert snake_case to PascalCase
def snakeToPascal (s : String) : String :=
  let parts := s.splitOn "_"
  String.join (parts.map fun p => p.capitalize)

-- Parse parameter type and default value from JSON
structure ParamInfo where
  fieldName : String
  leanType : String
  defaultValue : String
  originalName : String
  comment : String
  deriving Repr

-- Parse a parameter template from JSON
def parseParamTemplate (json : Json) : Except String ParamInfo := do
  -- Extract name
  let nameJson ← json.getObjVal? "name"
  let name ← nameJson.getStr?

  -- Extract type
  let typeJson ← json.getObjVal? "type"
  let parmType ← typeJson.getStr?

  match parmType with
  | "Float" =>
    -- Get num_components
    let numCompJson ← json.getObjVal? "num_components"
    let numComponents ← numCompJson.getNat?

    -- Get default_value array
    let defaultJson ← json.getObjVal? "default_value"
    let defaultArr ← defaultJson.getArr?

    if numComponents == 1 then
      let some v0Json := defaultArr[0]? | throw "Missing default_value[0]"
      let v0 ← v0Json.getNum?
      return {
        fieldName := snakeToCamel name
        leanType := "Float"
        defaultValue := toString v0.toFloat
        originalName := name
        comment := ""
      }
    else if numComponents == 2 then
      let some v0Json := defaultArr[0]? | throw "Missing default_value[0]"
      let v0 ← v0Json.getNum?
      let some v1Json := defaultArr[1]? | throw "Missing default_value[1]"
      let v1 ← v1Json.getNum?
      return {
        fieldName := snakeToCamel name
        leanType := "HouLean.Vector2"
        defaultValue := s!"⟨{v0.toFloat}, {v1.toFloat}⟩"
        originalName := name
        comment := ""
      }
    else if numComponents == 3 then
      let some v0Json := defaultArr[0]? | throw "Missing default_value[0]"
      let v0 ← v0Json.getNum?
      let some v1Json := defaultArr[1]? | throw "Missing default_value[1]"
      let v1 ← v1Json.getNum?
      let some v2Json := defaultArr[2]? | throw "Missing default_value[2]"
      let v2 ← v2Json.getNum?
      return {
        fieldName := snakeToCamel name
        leanType := "HouLean.Vector3"
        defaultValue := s!"⟨{v0.toFloat}, {v1.toFloat}, {v2.toFloat}⟩"
        originalName := name
        comment := ""
      }
    else if numComponents == 4 then
      let some v0Json := defaultArr[0]? | throw "Missing default_value[0]"
      let v0 ← v0Json.getNum?
      let some v1Json := defaultArr[1]? | throw "Missing default_value[1]"
      let v1 ← v1Json.getNum?
      let some v2Json := defaultArr[2]? | throw "Missing default_value[2]"
      let v2 ← v2Json.getNum?
      let some v3Json := defaultArr[3]? | throw "Missing default_value[3]"
      let v3 ← v3Json.getNum?
      return {
        fieldName := snakeToCamel name
        leanType := "HouLean.Vector4"
        defaultValue := s!"⟨{v0.toFloat}, {v1.toFloat}, {v2.toFloat}, {v3.toFloat}⟩"
        originalName := name
        comment := ""
      }
    else
      throw s!"Unsupported number of components: {numComponents}"

  | "Int" =>
    let defaultJson ← json.getObjVal? "default_value"
    let defaultArr ← defaultJson.getArr?
    let some v0Json := defaultArr[0]? | throw "Missing default_value[0]"
    let v0 ← v0Json.getNat?
    return {
      fieldName := snakeToCamel name
      leanType := "Int"
      defaultValue := toString v0
      originalName := name
      comment := ""
    }

  | "Toggle" =>
    let defaultJson ← json.getObjVal? "default_value"
    let defaultVal ← defaultJson.getBool?
    return {
      fieldName := snakeToCamel name
      leanType := "Bool"
      defaultValue := if defaultVal then "true" else "false"
      originalName := name
      comment := ""
    }

  | "String" =>
    let defaultJson ← json.getObjVal? "default_value"
    let defaultArr ← defaultJson.getArr?
    let some v0Json := defaultArr[0]? | throw "Missing default_value[0]"
    let v0 ← v0Json.getStr?
    return {
      fieldName := snakeToCamel name
      leanType := "String"
      defaultValue := s!"\"{v0}\""
      originalName := name
      comment := ""
    }

  | "Menu" =>
    let defaultJson ← json.getObjVal? "default_value"
    let defaultVal ← defaultJson.getNat?

    let menuLabelsJson ← json.getObjVal? "menu_labels"
    let menuLabelsArr ← menuLabelsJson.getArr?

    let labels : List String := (List.range menuLabelsArr.size).filterMap fun i =>
      match menuLabelsArr[i]? with
      | some lbl => lbl.getStr?.toOption
      | none => none

    let comment := String.intercalate ", " (labels.mapIdx fun i lbl => s!"{i} = {lbl}")
    return {
      fieldName := snakeToCamel name
      leanType := "Int"
      defaultValue := toString defaultVal
      originalName := name
      comment := s!"  -- {comment}"
    }

  | "Folder" =>
    throw "Folder parameters not yet supported in elaborator"

  | _ =>
    throw s!"Unknown parameter type: {parmType}"

-- Generate structure definition as syntax
def mkStructureSyntax (structName : Name) (params : List ParamInfo) : CommandElabM Syntax := do
  let structId := mkIdent structName

  -- Build structure fields
  let mut fieldStxs : Array (TSyntax `Lean.Parser.Command.structSimpleBinder) := #[]
  for p in params do
    let fieldName := mkIdent (Name.mkSimple p.fieldName)
    let typeName := mkIdent p.leanType.toName

    -- Parse default value as term syntax
    let defaultStr := p.defaultValue
    match Parser.runParserCategory (← getEnv) `term defaultStr with
    | .ok defaultTerm =>
      let defaultTerm : Term := ⟨defaultTerm⟩

      -- Create field syntax: fieldName : typeName := defaultTerm
      let fieldStx ← `(Parser.Command.structSimpleBinder| $fieldName:ident : $typeName:term := $defaultTerm:term)
      fieldStxs := fieldStxs.push fieldStx
    | .error _ =>
      throwError m!"Invalid default value {defaultStr}!"

  -- Generate structure command
  `(command|
    structure $structId where
      $[$fieldStxs]*
  )

-- Generate Inhabited instance as syntax
def mkInhabitedSyntax (structName : Name) : CommandElabM Syntax := do
  let structId := mkIdent structName
  `(command| instance : Inhabited $structId := ⟨{}⟩)

-- -- Generate toDict method as syntax
def mkToDictSyntax (structName : Name) (params : List ParamInfo) : CommandElabM Syntax := do
  let structId := mkIdent structName

  -- Build the dict construction expression
  let mut dictExpr ← `(term| HouLean.Apex.Dict.default)

  for p in params do
    let fieldName := mkIdent (Name.mkSimple p.fieldName)

    if p.leanType.startsWith "Vector" then
      let numStr := p.leanType.drop 6
      if let some num := numStr.toNat? then
        let components := ["x", "y", "z", "w"].take num
        for comp in components do
          let compId := mkIdent (Name.mkSimple comp)
          let keyStr := Syntax.mkStrLit <| p.originalName ++ comp
          dictExpr ← `($dictExpr |>.set $keyStr p.$fieldName.$compId)
    else
      let keyStr := Syntax.mkStrLit p.originalName
      dictExpr ← `($dictExpr |>.set $keyStr p.$fieldName)

  let toDictId := mkIdent (structName.append `toDict)
  -- Generate namespace with toDict
  `(command|
    def $toDictId:ident (p : $structId) : HouLean.Apex.Dict := $dictExpr
  )

-- Elaborate the generated syntax
def elabGeneratedStructure (structName : Name) (params : List ParamInfo) : CommandElabM Unit := do
  -- Generate and elaborate structure
  let structStx ← mkStructureSyntax structName params
  -- logInfo structStx
  elabCommand structStx

  -- Generate and elaborate Inhabited instance
  let inhabitedStx ← mkInhabitedSyntax structName
  elabCommand inhabitedStx

  -- -- Generate and elaborate toDict
  let toDictStx ← mkToDictSyntax structName params
  -- logInfo toDictStx
  elabCommand toDictStx

-- Process JSON and generate structures
def processParamJson (json : Json) : CommandElabM Unit := do
  -- Extract node info
  let nodeTypeJson ← match json.getObjVal? "node_type" with
    | .ok j => pure j
    | .error e => throwError e
  let nodeType ← match nodeTypeJson.getStr? with
    | .ok s => pure s
    | .error e => throwError e

  let templatesJson ← match json.getObjVal? "parameter_templates" with
    | .ok j => pure j
    | .error e => throwError e
  let templates ← match templatesJson.getArr? with
    | .ok a => pure a
    | .error e => throwError e

  -- Parse parameters
  let mut params : List ParamInfo := []
  for template in templates do
    match parseParamTemplate template with
    | .ok param => params := params ++ [param]
    | .error err => logWarning s!"Skipping parameter: {err}"

  -- Generate structure name
  let structName := Name.mkSimple (snakeToPascal nodeType ++ "Params")

  -- Generate and elaborate structure
  elabGeneratedStructure structName params

  -- logInfo s!"Successfully processed {params.length} parameters for {nodeType}"

-- Elaborator using json syntax directly
syntax (name := houdiniParams) "#houdini_params " json : command

elab_rules : command
  | `(command| #houdini_params $jsonStx:json) => do
    -- Elaborate the term to get Json value
    let json ← liftTermElabM do
      let jsonExpr ← Term.elabTerm (← `(term| json% $jsonStx)) none
      unsafe Meta.evalExpr Json (.const ``Json []) jsonExpr

    processParamJson json

-- Load from file version
elab "#houdini_params_from_file " filePath:str : command => do
  let filePathStr := filePath.getString

  -- Read file
  let content ← IO.FS.readFile filePathStr

  -- Parse JSON
  let json ← match Json.parse content with
    | .error err => throwError s!"JSON parse error: {err}"
    | .ok j => pure j

  processParamJson json

end HouLean.Houdini.ParamGen

-- Example usage:
--
-- From file:

#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_autostereogram_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_blend_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_blocktogeo_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_blur_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_bright_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_cameraimport_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_cameraproperties_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_channelextract_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_channeljoin_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_channelsplit_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_channelswap_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_checkerboard_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_clamp_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_constant_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_crop_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_denoiseai_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_derivative_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_distort_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_dot_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_eikonal_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_error_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_flip_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_function_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_geotolayer_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_hextile_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_idtorgb_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_illpixel_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_invert_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_julia_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_layer_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_layerattribcreate_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_layerattribdelete_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_layerproperties_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_layertogeo_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_layertopoints_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_matchcamera_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_matchudim_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_median_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_mono_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_monotorgb_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_premult_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_projectonlayer_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_ramp_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_rasterizelayer_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_remap_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_resample_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_rgbatorgb_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_rgbtorgba_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_solvepoissonmultigrid_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_sopimport_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_stash_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_streakblur_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_vdbleafpoints_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_vdbreshape_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_vectorxform2d_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_vectorxform_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_xform2d_parameters.json"
#houdini_params_from_file "HouLean/Apex/Generated/COP/Parameters/cop_xform_parameters.json"
