#!/opt/hfs21.0/bin/hython

import apex

def decapitalize(s):
    if not s:  # check that s is not empty string
        return s
    return s[0].lower() + s[1:]

def fixArgName(name):
    if name == "in":
        return "in'"
    if name == "end":
        return "end'"
    if name == "local":
        return "local'"
    if name == "class":
        return "class'"
    if name == "break":
        return "break'"
    if name == "prefix":
        return "prefix'"
    if name == "attribute":
        return "attribute'"
    return name.replace("#", "_")

def fixTypeName(name):
    if name == "VariadicArg<void>":
        return "VariadicArg Untyped"
    if name[:11] == "VariadicArg":
        return "VariadicArg " + fixTypeName(name[12:-1]) # could be an array
    # if name[-5:] == "Array":
    #     return "Array " + name[:-5]
    return name

# convert port name/type to corresponding Lean binder
def getInputBinders(portName, portType):
    argName = fixArgName(portName)

    if portType[:11] != "VariadicArg":
        return f"({argName} : {portType})"
    else:
        argType = portType[12:-1]
        if argType == "void":
            argType = "Untyped"

        numName = "num" + argName
        return "{" + numName + ": Nat} " + f"({argName} : VariadicArg {argType} {numName})"

verb_dict = hou.apexNodeTypeCategory().nodeTypes()

typeNames = set()

for name, node in verb_dict.items():

    graph = apex.Graph()
    graph.addNode("node", name)

    for p in graph.allPorts():

        typeName = graph.portTypeName(p)
        typeNames.add(typeName)

        
typeNames.remove("geo::IntersectCache")
typeNames.remove("undefined")
typeNames.remove("void")
        
# filter rundata types
runDataTypes = {t for t in typeNames if t[-7:] == "RunData"}
typeNames = typeNames.difference(runDataTypes)

# filter variadic types
variadicTypes = {t for t in typeNames if t[:11] == "VariadicArg"}
typeNames = typeNames.difference(variadicTypes)

# filter array types
arrayTypes = {t for t in typeNames if t[-5:] == "Array"}
typeNames = typeNames.difference(arrayTypes)

# non-array, non-variadic, and not weird like void or undefined
baseTypes = typeNames

simpleTypesList = sorted(baseTypes) + sorted(arrayTypes)
normalTypes = baseTypes.union(arrayTypes).union(variadicTypes)

# exclude these from generation
nativeTypes = ["Float", "Bool", "Int", "String", "FloatArray", "Vector2", "Vector3", "Vector4", "Matrix2", "Matrix3", "Matrix4"]

codeDefs = """import HouLean.Init
import HouLean.Data.Defs

namespace HouLean.Apex

"""

code = """import HouLean.Apex.Compile.Extension
import HouLean.Apex.Generated.Defs

namespace HouLean.Apex.Compiler

open Qq

run_meta
"""

# code += "\n".join([f"attribute [apex_builtin_type \"{t}\"] {t}" for t in simpleTypesList])
code += "\n".join([f"  compilerExt.add (.apexBuiltinType (Expr.const ``{t} []) .{decapitalize(t)})" for t in simpleTypesList])
code += "\n\n"

for type in simpleTypesList:
    s_defs = f"""
opaque {type} : Type := Unit
def {type}.default : {type} := cast sorry_proof ()
instance : Inhabited {type} := ⟨{type}.default⟩    
"""

    if type not in nativeTypes:
        codeDefs += s_defs



tags = '\n  '.join([f"| {decapitalize(s)}" for s in simpleTypesList])
tagToType = '\n  '.join([f"| {decapitalize(s)} => {s}" for s in simpleTypesList])
tagToString = '\n  '.join([f"| {decapitalize(s)} => \"{s}\"" for s in simpleTypesList])
tagFromName = '\n  '.join([f"| ``{s} => some .{decapitalize(s)}" for s in simpleTypesList])
ctors = '\n  '.join([f"| {decapitalize(s)} (x : {s})" for s in simpleTypesList])
s = f"""
inductive ApexTypeTag where
  {tags}
deriving Inhabited, BEq

def ApexTypeTag.toType : ApexTypeTag → Type
  {tagToType}

def ApexTypeTag.toString : ApexTypeTag → String
  {tagToString}

open Lean in
def ApexTypeTag.fromName : Name → Option ApexTypeTag
  {tagFromName}
  | _ => none

inductive Untyped where
  {ctors}

instance : Inhabited Untyped := ⟨.float 0⟩

"""
codeDefs += s


with open("Types.lean", "w", encoding="utf-8") as f:
    f.write(code)

with open("Defs.lean", "w", encoding="utf-8") as f:
    f.write(codeDefs)


####################################################################################################    


code = """import HouLean.Apex.Compile.NodeType
import HouLean.Apex.Generated.Types

set_option synthInstance.maxSize 512

namespace HouLean.Apex.Generated
"""


for name, node in verb_dict.items():

    graph = apex.Graph()
    graph.addNode("node", name)

    numInputs = node.maxNumInputs()
    numOutputs = node.maxNumOutputs()
    inputs = []
    outputs = []

    simpleFunctions = []
    advancedFunctions = []

    numPorts = sum([1 for p in graph.allPorts()])

    for p in graph.allPorts():
        if graph.portKind(p) == apex.Graph.connectorType.Input:
            inputs.append((graph.portName(p), graph.portTypeName(p)))
        if graph.portKind(p) == apex.Graph.connectorType.Output:
            outputs.append((graph.portName(p), graph.portTypeName(p)))
            
    # rig::AddControlShape has no outputs, it can't be pure function ...
    if len(outputs)==0:
        continue

    # remove RunData type from the outputs as it is some internal APEX magic
    hasRunData = ""
    if outputs[0][1] in runDataTypes:
        hasRunData = " has_rundata"
        outputs = outputs[1:]

    # Lean input and output signature
    inputSignature = ' '.join([getInputBinders(x[0], x[1]) for x in inputs])
    outputSignature = '×'.join([fixTypeName(x[1]) for x in outputs])
    outputNames = "(" + ','.join([fixArgName(x[0]) for x in outputs]) + ")"

    # all types in the function type
    inputTypes  = [x[1] for x in inputs]
    outputTypes = [x[1] for x in outputs]
    

    # turn node name into valid Lean identifier
    declName = name.replace("::", "_").replace("<", "").replace(",", "").replace(">", "").replace(".", "_")

    # right now we do not allow VariadicArg in the output
    if all([(t in normalTypes) for t in inputTypes]) and \
       all([(t in simpleTypesList) for t in outputTypes]):
        simpleFunctions.append(name)

        s = f"""
/-- outputs: {outputNames} -/        
@[apex_node "{name}"{hasRunData}]
opaque {declName} {inputSignature} : {outputSignature}
"""
        code += s
    else:
        s = f"""
-- special function not supported yet
-- opaque {name} {inputSignature} : {outputSignature}"""
        
        code += s[:150] + "...\n"
   

with open("Nodes.lean", "w", encoding="utf-8") as f:
    f.write(code)
    
    
