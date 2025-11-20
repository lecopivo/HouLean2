import Lean

namespace HouLean

macro:max (name:=struct_modify_stx) "struct" s:term "modify%" field:ident " := " value:term : term =>
  `( let s := $s
     {s with $field:ident := (fun $field => $value) s.$field} )

macro:max (name:=struct_modify_fun_stx) "struct" s:term "modify_fun%" field:ident : term =>
  `( let s := $s
     fun modify : type_of% s.$field → type_of% s.$field => {s with $field:ident := modify s.$field} )
