#!/usr/bin/env python3
"""
Generate Lean 4 structures and functions from Houdini COP parameter JSON dumps
"""

import json
import sys
from pathlib import Path
from typing import Dict, List, Tuple, Any

# Type mapping from Houdini to Lean
TYPE_MAP = {
    'Float': 'Float',
    'Int': 'Int',
    'Toggle': 'Bool',
    'String': 'String',
    'Menu': 'Int',  # Menu represented as Int index
}

def parm_to_lean_type(parm: Dict[str, Any]) -> Tuple[str, str]:
    """
    Convert parameter template to Lean type and default value.
    Returns (lean_type, default_value_str)
    """
    parm_type = parm['type']
    
    if parm_type == 'Float':
        num_components = parm.get('num_components', 1)
        default = parm.get('default_value', [0.0] * num_components)
        
        if num_components == 1:
            return ('Float', str(default[0] if isinstance(default, list) else default))
        elif num_components == 2:
            return ('Vector2', f"⟨{default[0]}, {default[1]}⟩")
        elif num_components == 3:
            return ('Vector3', f"⟨{default[0]}, {default[1]}, {default[2]}⟩")
        elif num_components == 4:
            return ('Vector4', f"⟨{default[0]}, {default[1]}, {default[2]}, {default[3]}⟩")
    
    elif parm_type == 'Int':
        num_components = parm.get('num_components', 1)
        default = parm.get('default_value', [0] * num_components)
        
        if num_components == 1:
            return ('Int', str(default[0] if isinstance(default, list) else default))
        else:
            # Multiple ints as array or tuple - for now just use first
            return ('Int', str(default[0] if isinstance(default, list) else default))
    
    elif parm_type == 'Toggle':
        default = parm.get('default_value', False)
        return ('Bool', 'true' if default else 'false')
    
    elif parm_type == 'String':
        default = parm.get('default_value', [''])[0] if isinstance(parm.get('default_value'), list) else parm.get('default_value', '')
        # Escape quotes in string
        escaped = default.replace('\\', '\\\\').replace('"', '\\"')
        return ('String', f'"{escaped}"')
    
    elif parm_type == 'Menu':
        default = parm.get('default_value', 0)
        return ('Int', str(default))
    
    elif parm_type == 'Folder':
        # Skip folders
        return None, None
    
    # Fallback
    return ('String', '""')

def snake_to_camel(name: str) -> str:
    """Convert snake_case to camelCase"""
    parts = name.split('_')
    return parts[0] + ''.join(p.capitalize() for p in parts[1:])

def snake_to_pascal(name: str) -> str:
    """Convert snake_case to PascalCase"""
    return ''.join(p.capitalize() for p in name.split('_'))

def get_struct_fields(parm_templates: List[Dict]):
    """
    Extract struct fields from parameter templates.
    Returns tuple of:
    - Regular fields: list of (field_name, lean_type, default_value, original_parm_name, comment)
    - Multiparm blocks: list of (block_name, children_fields)
    """
    fields = []
    multiparm_blocks = []
    
    for parm in parm_templates:
        if parm['type'] == 'Folder':
            folder_type = parm.get('folder_type', '')
            
            if folder_type == 'MultiparmBlock':
                # This is a multiparm block - create separate struct for its children
                block_name = parm['name']
                children = parm.get('children', [])
                
                # Process children, removing the # suffix from names
                child_fields = []
                for child in children:
                    if child['type'] == 'Folder':
                        continue
                    
                    lean_type, default_val = parm_to_lean_type(child)
                    if lean_type is None:
                        continue
                    
                    # Remove the # suffix from parameter name
                    original_name = child['name'].replace('#', '')
                    field_name = snake_to_camel(original_name)
                    
                    # Add comment for menu types
                    comment = ""
                    if child['type'] == 'Menu' and 'menu_items' in child:
                        labels = child.get('menu_labels', child['menu_items'])
                        options = [f"{i} = {label}" for i, label in enumerate(labels)]
                        comment = f"  -- {', '.join(options)}"
                    
                    child_fields.append((field_name, lean_type, default_val, original_name, comment))
                
                multiparm_blocks.append((block_name, child_fields))
            else:
                # Regular folder - recursively process children
                if 'children' in parm:
                    child_result = get_struct_fields(parm['children'])
                    child_fields = child_result[0]
                    child_multiparms = child_result[1]
                    fields.extend(child_fields)
                    multiparm_blocks.extend(child_multiparms)
            continue
        
        lean_type, default_val = parm_to_lean_type(parm)
        if lean_type is None:
            continue
        
        field_name = snake_to_camel(parm['name'])
        original_name = parm['name']
        
        # Add comment for menu types with options
        comment = ""
        if parm['type'] == 'Menu' and 'menu_items' in parm:
            items = parm['menu_items']
            labels = parm.get('menu_labels', items)
            options = [f"{i} = {label}" for i, label in enumerate(labels)]
            comment = f"  -- {', '.join(options)}"
        
        fields.append((field_name, lean_type, default_val, original_name, comment))
    
    return (fields, multiparm_blocks)

def generate_to_dict_with_multiparms(struct_name: str, fields: List, multiparm_blocks: List) -> str:
    """Generate toDict function body with multiparm support"""
    lines = ["  let dict := Dict.default"]
    
    # Add regular fields
    for field_name, lean_type, _, original_name, _ in fields:
        if lean_type.startswith('Vector'):
            # Vector types need to be decomposed
            num = int(lean_type[-1])
            components = ['x', 'y', 'z', 'w'][:num]
            for comp in components:
                lines.append(f'    |>.set "{original_name}{comp}" p.{field_name}.{comp}')
        else:
            lines.append(f'    |>.set "{original_name}" p.{field_name}')
    
    # Add multiparm blocks
    for block_name, _ in multiparm_blocks:
        field_name = snake_to_camel(block_name)
        # Set the count
        lines.append(f'    |>.set "{block_name}" p.{field_name}.size')
        
        # Set each multiparm instance
        lines.append(f'  let dict := p.{field_name}.foldlIdx (fun i dict item =>')
        lines.append(f'    item.addToDict dict (i + 1)) dict')
    
    lines.append("  dict")
    
    return "\n".join(lines)

def generate_multiparm_struct(block_name: str, base_struct_name: str, child_fields: List) -> str:
    """Generate a structure for a multiparm block item"""
    item_struct_name = snake_to_pascal(block_name.replace('num', '')) + "Item"
    
    lines = [f"-- Multiparm item for {block_name}"]
    lines.append(f"structure {item_struct_name} where")
    
    for field_name, lean_type, default_val, _, comment in child_fields:
        lines.append(f"  {field_name} : {lean_type} := {default_val}{comment}")
    
    lines.append("  deriving Repr")
    lines.append("")
    lines.append(f"instance : Inhabited {item_struct_name} := ⟨{{}}⟩")
    lines.append("")
    
    # Add addToDict method for the item
    lines.append(f"namespace {item_struct_name}")
    lines.append("")
    lines.append("-- Add this item's fields to dict with index suffix")
    lines.append(f"def addToDict (item : {item_struct_name}) (dict : Dict) (idx : Nat) : Dict :=")
    lines.append("  dict")
    
    for field_name, lean_type, _, original_name, _ in child_fields:
        if lean_type.startswith('Vector'):
            num = int(lean_type[-1])
            components = ['x', 'y', 'z', 'w'][:num]
            for comp in components:
                lines.append(f'    |>.set "{original_name}{{idx}}{comp}" item.{field_name}.{comp}')
        else:
            lines.append(f'    |>.set "{original_name}{{idx}}" item.{field_name}')
    
    lines.append("")
    lines.append(f"end {item_struct_name}")
    
    return "\n".join(lines)

def generate_fluent_methods(struct_name: str, fields: List) -> List[str]:
    """Generate fluent API methods"""
    methods = []
    
    for field_name, lean_type, _, _, _ in fields:
        method_name = f"with{field_name[0].upper()}{field_name[1:]}"
        methods.append(f"""def {method_name} (p : {struct_name}) (v : {lean_type}) : {struct_name} :=
  {{ p with {field_name} := v }}""")
    
    return methods

def generate_menu_helpers(struct_name: str, fields: List) -> List[str]:
    """Generate helper constructors for menu parameters"""
    helpers = []
    
    for field_name, lean_type, _, original_name, comment in fields:
        if lean_type == 'Int' and comment:  # Menu parameter
            # Parse menu options from comment
            # Comment format: "-- 0 = Label1, 1 = Label2, ..."
            if '--' in comment:
                options_str = comment.split('--')[1].strip()
                options = [opt.strip().split('=')[1].strip() for opt in options_str.split(',') if '=' in opt]
                
                for idx, label in enumerate(options):
                    # Convert label to valid identifier
                    helper_name = ''.join(c if c.isalnum() else '' for c in label).lower()
                    if helper_name:
                        helpers.append(f"def {helper_name} : {struct_name} := {{ {field_name} := {idx} }}")
    
    return helpers

def get_input_params(node_type: str) -> List[Tuple[str, str]]:
    """
    Determine input parameters based on node type.
    Returns list of (param_name, param_type)
    """
    # Common patterns for COP nodes
    if node_type == 'blend':
        return [('bg', 'ImageLayer'), ('fg', 'ImageLayer'), ('mask', 'ImageLayer')]
    elif node_type == 'blur':
        return [('source', 'ImageLayer'), ('size', 'ImageLayer')]
    elif node_type == 'blocktogeo':
        return []  # No inputs
    else:
        # Default: single source input
        return [('source', 'ImageLayer')]

def get_output_type(node_type: str) -> str:
    """Determine output type based on node type"""
    if node_type == 'blocktogeo':
        return 'Geometry'
    else:
        return 'ImageLayer'

def generate_node_code(node_type: str, param_data: Dict[str, Any]) -> str:
    """Generate complete Lean code for a COP node type"""
    
    struct_name = snake_to_pascal(node_type) + "Params"
    func_name = snake_to_camel(node_type)
    
    # Extract fields and multiparm blocks
    result = get_struct_fields(param_data['parameter_templates'])
    fields = result[0]
    multiparm_blocks = result[1]
    
    if not fields and not multiparm_blocks:
        return f"-- No parameters for {node_type}\n"
    
    all_code_parts = []
    
    # Generate multiparm item structures first
    for block_name, child_fields in multiparm_blocks:
        multiparm_code = generate_multiparm_struct(block_name, struct_name, child_fields)
        all_code_parts.append(multiparm_code)
        all_code_parts.append("")
    
    # Build main structure with comments
    struct_lines = [f"structure {struct_name} where"]
    
    # Add regular fields
    for field_name, lean_type, default_val, _, comment in fields:
        struct_lines.append(f"  {field_name} : {lean_type} := {default_val}{comment}")
    
    # Add multiparm array fields
    for block_name, child_fields in multiparm_blocks:
        item_struct_name = snake_to_pascal(block_name.replace('num', '')) + "Item"
        field_name = snake_to_camel(block_name)
        struct_lines.append(f"  {field_name} : Array {item_struct_name} := #[]")
    
    struct_lines.append("  deriving Repr")
    struct_lines.append("")
    struct_lines.append(f"instance : Inhabited {struct_name} := ⟨{{}}⟩")
    
    all_code_parts.append("\n".join(struct_lines))
    
    # Generate namespace
    namespace_lines = [
        f"namespace {struct_name}",
        "",
        "-- Convert parameters to Dict",
        f"def toDict (p : {struct_name}) : Dict :=",
        generate_to_dict_with_multiparms(struct_name, fields, multiparm_blocks),
        ""
    ]
    
    # Generate helper constructors for menus (only for regular fields)
    menu_helpers = generate_menu_helpers(struct_name, fields)
    if menu_helpers:
        namespace_lines.append("-- Helper constructors")
        namespace_lines.extend(menu_helpers)
        namespace_lines.append("")
    
    # Generate fluent API methods (only for regular fields)
    if fields:
        namespace_lines.append("-- Fluent API for parameter building")
        namespace_lines.extend(generate_fluent_methods(struct_name, fields))
        namespace_lines.append("")
    
    namespace_lines.append(f"end {struct_name}")
    
    all_code_parts.append("\n".join(namespace_lines))
    
    # Generate high-level wrapper function
    input_params = get_input_params(node_type)
    output_type = get_output_type(node_type)
    
    func_lines = [
        "",
        "-- High-level typed function",
        "open Generated in",
        f"def {func_name}"
    ]
    
    # Add input parameters
    for param_name, param_type in input_params:
        func_lines.append(f"  ({param_name} : {param_type})")
    
    # Add standard parameters
    func_lines.extend([
        f"  (params : {struct_name} := default)",
        "  (signature : StringArray := default)",
        "  (cwdNode : String := default)",
        "  (requests : IntArray := default)",
        "  (contextData : VerbContext := default)",
        f"  : {output_type} :="
    ])
    
    # Build function call
    call_args = [param_name for param_name, _ in input_params]
    call_args.extend(["signature", "cwdNode", "requests", "params.toDict", "contextData"])
    func_lines.append(f"  cop_{node_type} {' '.join(call_args)}")
    
    all_code_parts.append("\n".join(func_lines))
    
    return "\n\n".join(all_code_parts)

def generate_file_header() -> str:
    """Generate file header with imports"""
    return """import HouLean.Apex.Basic
import HouLean.Apex.Dict
import HouLean.Apex.Generated.Nodes

namespace HouLean.Apex.COP

"""

def generate_file_footer() -> str:
    """Generate file footer"""
    return "\nend HouLean.Apex.COP\n"

def main():
    if len(sys.argv) < 2:
        print("Usage: generate_lean.py <json_file1> [json_file2 ...]")
        print("   or: generate_lean.py cop_all_parameters.json")
        sys.exit(1)
    
    json_path = Path(sys.argv[1])
    
    if not json_path.exists():
        print(f"Error: {json_path} not found")
        sys.exit(1)
    
    with open(json_path) as f:
        data = json.load(f)
    
    # Determine output directory
    output_dir = json_path.parent / "Generated"
    output_dir.mkdir(exist_ok=True)
    
    generated_files = []
    
    # Check if it's a combined file or single node
    if 'node_type' in data:
        # Single node file
        node_type = data['node_type']
        module_name = snake_to_pascal(node_type)
        
        output_lines = [generate_file_header()]
        code = generate_node_code(node_type, data)
        output_lines.append(code)
        output_lines.append(generate_file_footer())
        
        # Write individual file
        output_path = output_dir / f"{module_name}.lean"
        with open(output_path, 'w') as f:
            f.write("\n".join(output_lines))
        
        generated_files.append((module_name, node_type))
        print(f"Generated: {output_path}")
    else:
        # Combined file with multiple nodes
        for node_type, param_data in sorted(data.items()):
            if 'error' in param_data:
                print(f"Skipping {node_type}: {param_data['error']}")
                continue
            
            module_name = snake_to_pascal(node_type)
            
            output_lines = [generate_file_header()]
            code = generate_node_code(node_type, param_data)
            output_lines.append(code)
            output_lines.append(generate_file_footer())
            
            # Write individual file
            output_path = output_dir / f"{module_name}.lean"
            with open(output_path, 'w') as f:
                f.write("\n".join(output_lines))
            
            generated_files.append((module_name, node_type))
            print(f"Generated: {output_path}")
    
    # Generate index file that imports all modules
    if generated_files:
        index_lines = [
            "-- Auto-generated COP node wrappers",
            "-- This file imports all generated COP modules",
            ""
        ]
        
        for module_name, node_type in sorted(generated_files):
            index_lines.append(f"import HouLean.Apex.COP.Generated.{module_name}")
        
        index_lines.append("")
        index_lines.append("namespace HouLean.Apex.COP")
        index_lines.append("")
        index_lines.append("-- All COP node wrappers are available through their respective modules:")
        for module_name, node_type in sorted(generated_files):
            func_name = snake_to_camel(node_type)
            index_lines.append(f"-- • {func_name} (from {module_name})")
        index_lines.append("")
        index_lines.append("end HouLean.Apex.COP")
        
        index_path = output_dir.parent / "COP.lean"
        with open(index_path, 'w') as f:
            f.write("\n".join(index_lines) + "\n")
        
        print(f"\nGenerated index file: {index_path}")
        print(f"Total modules: {len(generated_files)}")
        print(f"Total lines: {sum(len(open(output_dir / f'{m}.lean').readlines()) for m, _ in generated_files)}")

if __name__ == '__main__':
    main()
