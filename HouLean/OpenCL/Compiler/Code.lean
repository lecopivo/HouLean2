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
    | .infix prio =>
      -- todo: here we should use operator precendence to determine how to set `maybeBracket`
      --       on the recurives call.
      let args ← args.mapM (fun arg => arg.toString true)
      let e := args.joinl (map:=id) (·++ fn.name ++·)
      if maybeBracket && prio != 0 then -- todo: handle priority properly!
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
      let args := args.joinl (map:=id) (·++ ", " ++·)
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


inductive CodeStatement where
  | letE (name : String) (type : OCLType) (val : CodeExpr)
  | assignment (name : String) (val : CodeExpr)
  | ite (cond : CodeExpr) (t e : Array CodeStatement)
  | forLoop (index : String) (start stop step : CodeExpr) (body : Array CodeStatement)
  | funCall (val : CodeExpr)
  | ret (val : CodeExpr)
deriving Inhabited

partial def CodeStatement.toString (stmt : CodeStatement)
    (indent := "") (indentInc := "    ") : CoreM String := do
  match stmt with
  | .letE n t v => return s!"{indent}{t.name} {n} = {← v.toString};"
  | .assignment n v => return s!"{indent}{n} = {← v.toString};"
  | .ite .. => return s!"{indent}if todo:"
  | .forLoop i s e st b =>
    let s ← s.toString
    let e ← e.toString
    let st ← st.toString
    let b ← b.joinlM
      (op  := fun a b => pure (a++"\n"++b))
      (map := fun x => do pure s!"{← x.toString (indent ++ indentInc)}")
    return s!"{indent}for (uint {i} = {s}; {i} < {e}; {i} += {st})\n\
              {indent}\{\n\
              {b}\n\
              {indent}}"
  | .funCall v => return s!"{← v.toString};"
  | .ret v => return s!"return {← v.toString};"

-- bunch of let bindings, add support for `if then else` and `for(..){ .. }`
inductive CodeBody where
  | letE (name : String) (type : OCLType) (val : CodeExpr) (body : CodeBody)
  | ite (cond : CodeExpr) (t e : CodeBody)
  | forLoop (index : String) (body : CodeBody) (rest : CodeBody)
  | ret (val : CodeExpr)
deriving Inhabited

def CodeBody.toString (c : CodeBody) (indent := "") (indentIncr := "    ") : MetaM String := do
  match c with
  | .letE _ _ (.app { name := _, kind := .elemset} #[ptr, off, val]) b =>
    let ptr ← ptr.toString
    let off ← off.toString
    let val ← val.toString
    let body ← b.toString indent
    return s!"{indent}{ptr}[{off}] = {val};\n{body}"

  | .letE n t v b =>
    let value ← v.toString
    let body ← b.toString indent
    return s!"{indent}{t.name} {n} = {value};\n{body}"
  | .ite c t e =>
    let c ← c.toString
    let t ← t.toString (indent ++ indentIncr)
    let e ← e.toString (indent ++ indentIncr)
    return s!"{indent}if ({c})\n{indent}\{\n{t}\n{indent}}\n{indent}else\n{indent}\{\n{e}\n{indent}}"
  | .forLoop idx body rest =>
    let body ← body.toString (indent ++ indentIncr)
    let rest ← rest.toString indent
    return s!"{indent}for ({idx})\n{indent}\{\n{body}\n{indent}}\n{rest}"
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
  let body ← c.body.toString "    " "    "
  let args := c.args.map (fun (t,n) => s!"{t.name} {n}") |>.joinl (map:=id) (· ++ ", " ++ ·)
  return s!"{c.returnType.name} {c.name}({args})\n\{\n{body}\n}"

-- bunch of let bindings, add support for `if then else` and `for(..){ .. }`
structure CodeFunction' where
  name : String
  returnType : OCLType
  args : Array (OCLType × String)
  body : Array CodeStatement
deriving Inhabited

def CodeFunction'.toString (c : CodeFunction') : MetaM String := do
  let body ← c.body.mapM (·.toString "    " "    ")
  let args := c.args.map (fun (t,n) => s!"{t.name} {n}") |>.joinl (map:=id) (· ++ ", " ++ ·)
  return s!"{c.returnType.name} {c.name}({args})\n\{\n{body}\n}"
