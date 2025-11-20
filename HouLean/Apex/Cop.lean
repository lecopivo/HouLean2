import HouLean.Apex.Basic
import HouLean.Apex.Array
import HouLean.Apex.Dict

namespace HouLean.COP

open Apex

inductive ImageSignature where
  | int | float | vector2 | vector3 | vector4
deriving Inhabited

inductive VDBSignature where
  | int | float | vector3
deriving Inhabited

inductive Layer where
  | image (sig : ImageSignature) (image : ImageLayer)
  | volume (sig : VDBSignature) (vdb : NanoVDB)
deriving Inhabited

def Layer.signature : Layer → String
  | .image .int _ => "i"
  | .image .float _ => "f"
  | .image .vector2 _ => "u"
  | .image .vector3 _ => "v"
  | .image .vector4 _ => "p"
  | .volume .int _ => "I"
  | .volume .float _ => "F"
  | .volume .vector3 _ => "V"

abbrev CopM := ReaderM VerbContext

def Layer.defaultWithSameType (layer : Layer) : Layer :=
  match layer with
  | .image sig _ => .image sig ImageLayer.default
  | .volume sig _ => .volume sig NanoVDB.default

def Layer.defaultMonoWithSameDim (layer : Layer) : Layer :=
  match layer with
  | .image _ _ => .image .float ImageLayer.default
  | .volume _ _ => .volume .float NanoVDB.default



-- open Lean Elab Term
-- elab "struct_modify%" field:ident s:term : term => do
--   let S : Term := sorry
--   let X : Term := sorry
--   `(fun (s : $S) (f : $X → $X) => {s with $field := f s.$field })
--   `(let s := $s
--     {s with $field:ident := (fun $field => $modify) (s).$field} )

-- #eval
--   struct_modify% (Vector3.mk 1 2 3), x, (100 * x)

-- #check
--   struct_modify% (Vector3.mk 1 2 3), x, (100 * x)
