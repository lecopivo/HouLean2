import HouLean.Meta.AnonymousStruct
import HouLean.Data.Defs

namespace HouLean


open Lean Elab Command in
/-- Register common field name and type of an (anonymous) struct.

Defines a class `className` that provides `get$name` and `set$name` functions to
get and set this field from a structure.

-- todo: When you define a new structure, please add `deriving KnownFields`, like:
structure MyNewStruct where
  ...
deriving KnownFields

This will make sure that known fields are registered for your structure. This is done
automatically for anonymous structures.
 -/
elab (docComment)? "#known_field" shortName:ident longName:ident className:ident fieldType:term  : command => do
  let getIdShort := mkIdent <| Name.mkSimple <| "get" ++ shortName.getId.getString!
  let setIdShort := mkIdent <| Name.mkSimple <| "set" ++ shortName.getId.getString!
  let getIdLong := mkIdent <| Name.mkSimple <| "get" ++ longName.getId.getString!
  let setIdLong := mkIdent <| Name.mkSimple <| "set" ++ longName.getId.getString!
  let cmd ← `(
    class $className (α : Type) where
      $getIdLong:ident : α → $fieldType
      $setIdLong:ident : α → $fieldType → α
    export $className:ident ($getIdLong $setIdLong)
    )

  let cmdShortAbbrev ← `(
    abbrev $getIdShort {α} [$className α] (x : α) := $getIdLong x
    abbrev $setIdShort {α} [$className α] (x : α) (v : $fieldType) := $setIdLong x v
  )

  -- todo: generate deriving handle for structures!

  elabCommand cmd
  if shortName != longName then
    elabCommand cmdShortAbbrev


/-- Position of an object. -/
#known_field P position HasPosition Vector3

/-- Velocity of an object. -/
#known_field v velocity HasVelocity Vector3

/-- Scale of an object. -/
#known_field scale velocity HasScale Vector3

/-- Uniform scale of an object. -/
#known_field pscale pscale HasPScale Float

/-- Orientation quaternion of an object. -/
#known_field orient orient HasOrient Vector4

/-- Orientation quaternion of an object. -/
#known_field trans transform HasTransform Transform

/-- Name identifying an object. -/
#known_field name name HasName String
