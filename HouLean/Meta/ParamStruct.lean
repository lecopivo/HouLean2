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

-- Info about a folder group
structure FolderInfo where
  folderName : String
  structName : String
  children : List ParamInfo
  deriving Repr

-- Parse a parameter template from JSON (returns either regular param or folder)
partial def parseParamTemplate (json : Json) : Except String (ParamInfo ⊕ FolderInfo) := do
  -- Extract name
  let nameJson ← json.getObjVal? "name"
  let name ← nameJson.getStr?

  -- Extract type
  let typeJson ← json.getObjVal? "type"
  let parmType ← typeJson.getStr?

  match parmType with
  | "Folder" =>
    -- Get folder type
    let folderTypeJson ← json.getObjVal? "folder_type"
    let folderType ← folderTypeJson.getStr?

    -- Handle MultiparmBlock specially - skip for now
    if folderType == "MultiparmBlock" then
      throw "MultiparmBlock not supported, skipping"

    -- Get label for struct name
    let labelJson ← json.getObjVal? "label"
    let label ← labelJson.getStr?

    -- Regular folder or collapsible - extract children to separate structure
    let childrenJson ← json.getObjVal? "children"
    let childrenArr ← childrenJson.getArr?

    let mut childParams : List ParamInfo := []
    for childJson in childrenArr do
      match parseParamTemplate childJson with
      | .ok (.inl param) => childParams := childParams ++ [param]
      | .ok (.inr _) => pure () -- Skip nested folders
      | .error _ => pure () -- Skip errors

    -- Generate struct name from label (remove whitespace and convert to PascalCase)
    let cleanLabel := label.replace " " ""
    let structName := snakeToPascal cleanLabel

    return .inr {
      folderName := name
      structName := structName
      children := childParams
    }

  | "Float" =>
    let numCompJson ← json.getObjVal? "num_components"
    let numComponents ← numCompJson.getNat?
    let defaultJson ← json.getObjVal? "default_value"
    let defaultArr ← defaultJson.getArr?

    if numComponents == 1 then
      let some v0Json := defaultArr[0]? | throw "Missing default_value[0]"
      let v0 ← v0Json.getNum?
      return .inl {
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
      return .inl {
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
      return .inl {
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
      return .inl {
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
    return .inl {
      fieldName := snakeToCamel name
      leanType := "Int"
      defaultValue := toString v0
      originalName := name
      comment := ""
    }

  | "Toggle" =>
    let defaultJson ← json.getObjVal? "default_value"
    let defaultVal ← defaultJson.getBool?
    return .inl {
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
    return .inl {
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
    return .inl {
      fieldName := snakeToCamel name
      leanType := "Int"
      defaultValue := toString defaultVal
      originalName := name
      comment := s!"  -- {comment}"
    }

  | _ =>
    throw s!"Unknown parameter type: {parmType}"

-- Generate folder structure
def mkFolderStructureSyntax (parentStructName : Name) (folder : FolderInfo) : CommandElabM Syntax := do
  let structId := mkIdent (parentStructName.append (Name.mkSimple folder.structName))

  let mut fieldStxs : Array (TSyntax `Lean.Parser.Command.structSimpleBinder) := #[]
  for p in folder.children do
    let fieldName := mkIdent (Name.mkSimple p.fieldName)
    let typeName := mkIdent p.leanType.toName

    match Parser.runParserCategory (← getEnv) `term p.defaultValue with
    | .ok defaultTerm =>
      let defaultTerm : Term := ⟨defaultTerm⟩
      let fieldStx ← `(Parser.Command.structSimpleBinder| $fieldName:ident : $typeName:term := $defaultTerm:term)
      fieldStxs := fieldStxs.push fieldStx
    | .error _ =>
      throwError m!"Invalid default value {p.defaultValue}!"

  `(command|
    structure $structId where
      $[$fieldStxs]*
  )

-- Generate structure definition as syntax
def mkStructureSyntax (structName : Name) (params : List ParamInfo) (folders : List FolderInfo) : CommandElabM Syntax := do
  let structId := mkIdent structName

  let mut fieldStxs : Array (TSyntax `Lean.Parser.Command.structSimpleBinder) := #[]

  -- Add regular parameters
  for p in params do
    let fieldName := mkIdent (Name.mkSimple p.fieldName)
    let typeName := mkIdent p.leanType.toName

    match Parser.runParserCategory (← getEnv) `term p.defaultValue with
    | .ok defaultTerm =>
      let defaultTerm : Term := ⟨defaultTerm⟩
      let fieldStx ← `(Parser.Command.structSimpleBinder| $fieldName:ident : $typeName:term := $defaultTerm:term)
      fieldStxs := fieldStxs.push fieldStx
    | .error _ =>
      throwError m!"Invalid default value {p.defaultValue}!"

  -- Add folder fields with qualified type names
  for folder in folders do
    let fieldName := mkIdent (Name.mkSimple (snakeToCamel folder.folderName))
    let folderTypeName := mkIdent (structName.append (Name.mkSimple folder.structName))
    let fieldStx ← `(Parser.Command.structSimpleBinder| $fieldName:ident : $folderTypeName := default)
    fieldStxs := fieldStxs.push fieldStx

  `(command|
    structure $structId where
      $[$fieldStxs]*
  )

-- Generate Inhabited instance as syntax
def mkInhabitedSyntax (structName : Name) : CommandElabM Syntax := do
  let structId := mkIdent structName
  `(command| instance : Inhabited $structId := ⟨{}⟩)

-- Generate toDict method as syntax
def mkToDictSyntax (structName : Name) (params : List ParamInfo) (folders : List FolderInfo) : CommandElabM Syntax := do
  let structId := mkIdent structName
  let mut dictExpr ← `(term| HouLean.Apex.Dict.default)

  -- Add regular parameters
  for p in params do
    let fieldName := mkIdent (Name.mkSimple p.fieldName)

    if p.leanType.endsWith "Vector2" then
      let components := ["x", "y"]
      for comp in components do
        let compId := mkIdent (Name.mkSimple comp)
        let keyStr := Syntax.mkStrLit <| p.originalName ++ comp
        dictExpr ← `($dictExpr |>.set $keyStr p.$fieldName.$compId)
    else if p.leanType.endsWith "Vector3" then
      let components := ["x", "y", "z"]
      for comp in components do
        let compId := mkIdent (Name.mkSimple comp)
        let keyStr := Syntax.mkStrLit <| p.originalName ++ comp
        dictExpr ← `($dictExpr |>.set $keyStr p.$fieldName.$compId)
    else if p.leanType.endsWith "Vector4" then
      let components := ["x", "y", "z", "w"]
      for comp in components do
        let compId := mkIdent (Name.mkSimple comp)
        let keyStr := Syntax.mkStrLit <| p.originalName ++ comp
        dictExpr ← `($dictExpr |>.set $keyStr p.$fieldName.$compId)
    else
      let keyStr := Syntax.mkStrLit p.originalName
      dictExpr ← `($dictExpr |>.set $keyStr p.$fieldName)

  -- Add folder parameters by calling their toDict
  for folder in folders do
    let fieldName := mkIdent (Name.mkSimple (snakeToCamel folder.folderName))
    let folderToDictId := mkIdent (structName.append (Name.mkSimple folder.structName) |>.append `toDict)
    dictExpr ← `($dictExpr |>.merge ($folderToDictId p.$fieldName))

  let toDictId := mkIdent (structName.append `toDict)
  `(command|
    def $toDictId (p : $structId) : HouLean.Apex.Dict := $dictExpr
  )

-- Generate toDict for folder
def mkFolderToDictSyntax (parentStructName : Name) (folder : FolderInfo) : CommandElabM Syntax := do
  let structId := mkIdent (parentStructName.append (Name.mkSimple folder.structName))
  let toDictId := mkIdent (parentStructName.append (Name.mkSimple folder.structName) |>.append `toDict)

  let mut dictExpr ← `(term| HouLean.Apex.Dict.default)

  for p in folder.children do
    let fieldName := mkIdent (Name.mkSimple p.fieldName)

    if p.leanType.endsWith "Vector2" then
      let components := ["x", "y"]
      for comp in components do
        let compId := mkIdent (Name.mkSimple comp)
        let keyStr := Syntax.mkStrLit <| p.originalName ++ comp
        dictExpr ← `($dictExpr |>.set $keyStr p.$fieldName.$compId)
    else if p.leanType.endsWith "Vector3" then
      let components := ["x", "y", "z"]
      for comp in components do
        let compId := mkIdent (Name.mkSimple comp)
        let keyStr := Syntax.mkStrLit <| p.originalName ++ comp
        dictExpr ← `($dictExpr |>.set $keyStr p.$fieldName.$compId)
    else if p.leanType.endsWith "Vector4" then
      let components := ["x", "y", "z", "w"]
      for comp in components do
        let compId := mkIdent (Name.mkSimple comp)
        let keyStr := Syntax.mkStrLit <| p.originalName ++ comp
        dictExpr ← `($dictExpr |>.set $keyStr p.$fieldName.$compId)
    else
      let keyStr := Syntax.mkStrLit p.originalName
      dictExpr ← `($dictExpr |>.set $keyStr p.$fieldName)

  `(command|
    def $toDictId (p : $structId) : HouLean.Apex.Dict := $dictExpr
  )

-- Elaborate the generated syntax
def elabGeneratedStructure (structName : Name) (params : List ParamInfo) (folders : List FolderInfo) : CommandElabM Unit := do
  -- Generate folder structures first (with qualified names)
  for folder in folders do
    let folderStx ← mkFolderStructureSyntax structName folder
    elabCommand folderStx

    let folderStructId := mkIdent (structName.append (Name.mkSimple folder.structName))
    let inhabitedStx ← `(command| instance : Inhabited $folderStructId := ⟨{}⟩)
    elabCommand inhabitedStx

    let folderToDictStx ← mkFolderToDictSyntax structName folder
    elabCommand folderToDictStx

  -- Generate main structure
  let structStx ← mkStructureSyntax structName params folders
  elabCommand structStx

  let inhabitedStx ← mkInhabitedSyntax structName
  elabCommand inhabitedStx

  let toDictStx ← mkToDictSyntax structName params folders
  elabCommand toDictStx

-- Process JSON and generate structures
def processParamJson (json : Json) : CommandElabM Unit := do
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

  let structName := Name.mkSimple (snakeToPascal nodeType ++ "Params")

  -- Check if structure already exists
  let env ← getEnv
  if env.contains structName then
    logWarning s!"Structure {structName} already exists, skipping"
    return

  -- Parse parameters and folders
  let mut params : List ParamInfo := []
  let mut folders : List FolderInfo := []

  for template in templates do
    match parseParamTemplate template with
    | .ok (.inl param) => params := params ++ [param]
    | .ok (.inr folder) => folders := folders ++ [folder]
    | .error err => logWarning s!"Skipping parameter: {err}"

  elabGeneratedStructure structName params folders

-- Elaborator using json syntax directly
syntax (name := houdiniParams) "#houdini_params " json : command

elab_rules : command
  | `(command| #houdini_params $jsonStx:json) => do
    let json ← liftTermElabM do
      let jsonExpr ← Term.elabTerm (← `(term| json% $jsonStx)) none
      unsafe Meta.evalExpr Json (.const ``Json []) jsonExpr
    processParamJson json

-- Load from file version
elab "#houdini_params_from_file " filePath:str : command => do
  let filePathStr := filePath.getString
  let content ← IO.FS.readFile filePathStr
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
