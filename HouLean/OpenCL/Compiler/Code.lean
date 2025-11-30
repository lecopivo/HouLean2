import HouLean.OpenCL.Compiler.Extension

open Lean Meta

namespace HouLean.OpenCL.Compiler

inductive CodeExpr where
  | errased
  | lit (val : String)
  | fvar (name : String)
  | app (fn : OCLFunction) (args : Array CodeExpr)
deriving Inhabited

partial def CodeExpr.toString (c : CodeExpr) (maybeBracket := false) : CoreM String := do
  match c with
  | .errased => return ""
  | .lit val => return val
  | .fvar n => return n
  | .app fn args =>
    match fn.kind with
    | .normal =>
      let args ← args.mapM (·.toString)
      let args := args.joinl (map:=id) (·++", "++·)
      return s!"{fn.name}({args})"
    | .infix =>
      -- todo: here we should use operator precendence to determine how to set `maybeBracket`
      --       on the recurives call.
      let args ← args.mapM (fun arg => arg.toString true)
      let e := args.joinl (map:=id) (·++ fn.name ++·)
      if maybeBracket then
        return s!"({e})"
      else
        return e
    | .prefix =>
      let args ← args.mapM (·.toString true)
      unless args.size ≥ 1 do
        throwError "Unexpected number of arguments, {args} , for prefix operation `{fn.name}`"
      let e := s!"{fn.name}{args[0]!}"
      if maybeBracket then
        return "(" ++ e ++ ")"
      else
        return e
    | .postfix =>
      let args ← args.mapM (·.toString true)
      unless args.size ≥ 1 do
        throwError "Unexpected number of arguments, {args} , for postrif operation `{fn.name}`"
      let e := s!"{args[0]!}{fn.name}"
      return e
    | .constructor =>
      let args ← args.mapM (·.toString)
      let args := args.joinl (map:=id) (·++ fn.name ++·)
      let e := s!"{fn.name}\{{args}}"
      return e

    | .elemget =>
      unless args.size ≥ 2 do
        throwError "Unexpected number of arguments, {args.size}, for get element operation"
      let array ← args[0]!.toString true
      let index ← args[1]!.toString
      return s!"{array}[{index}]"

    | .elemset =>
      unless args.size ≥ 3 do
        throwError "Unexpected number of arguments, {args.size}, for get element operation"
      let array ← args[0]!.toString true
      let index ← args[1]!.toString
      let value ← args[2]!.toString
      return s!"{array}[{index}] = {value}"


-- bunch of let bindings, add support for `if then else` and `for(..){ .. }`
inductive CodeBody where
  | letE (name : String) (type : OCLType) (val : CodeExpr) (body : CodeBody)
  | ret (val : CodeExpr)
deriving Inhabited

def CodeBody.toString (c : CodeBody) (indent := "") : MetaM String := do
  match c with
  | .letE n t v b =>
    let value ← v.toString
    let body ← b.toString indent
    return s!"{indent}{t.name} {n} = {value};\n{body}"
  | ret v =>
    let value ← v.toString
    return s!"{indent}return {value};"


-- bunch of let bindings, add support for `if then else` and `for(..){ .. }`
structure CodeFunction where
  name : String
  returnType : OCLType
  args : Array (OCLType × String)
  body : CodeBody
deriving Inhabited

def CodeFunction.toString (c : CodeFunction) : MetaM String := do
  let body ← c.body.toString "    "
  let args := c.args.map (fun (t,n) => s!"{t.name} {n}") |>.joinl (map:=id) (· ++ ", " ++ ·)
  return s!"{c.returnType.name} {c.name}({args})\n\{\n{body}\n}"
