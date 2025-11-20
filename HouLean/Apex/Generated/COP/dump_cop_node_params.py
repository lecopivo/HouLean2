#!/opt/hfs21.0/bin/hython
import hou
import json
import sys

def dump_parm_template(parm_template):
    """Recursively dump parameter template information"""
    data = {
        'name': parm_template.name(),
        'label': parm_template.label(),
        'type': parm_template.type().name(),
        'help': parm_template.help(),
    }
    
    # Add type-specific information
    if isinstance(parm_template, hou.IntParmTemplate):
        data['default_value'] = parm_template.defaultValue()
        data['min_value'] = parm_template.minValue()
        data['max_value'] = parm_template.maxValue()
        data['num_components'] = parm_template.numComponents()
    elif isinstance(parm_template, hou.FloatParmTemplate):
        data['default_value'] = parm_template.defaultValue()
        data['min_value'] = parm_template.minValue()
        data['max_value'] = parm_template.maxValue()
        data['num_components'] = parm_template.numComponents()
    elif isinstance(parm_template, hou.StringParmTemplate):
        data['default_value'] = parm_template.defaultValue()
        data['string_type'] = parm_template.stringType().name()
    elif isinstance(parm_template, hou.MenuParmTemplate):
        data['default_value'] = parm_template.defaultValue()
        data['menu_items'] = parm_template.menuItems()
        data['menu_labels'] = parm_template.menuLabels()
    elif isinstance(parm_template, hou.ToggleParmTemplate):
        data['default_value'] = parm_template.defaultValue()
    elif isinstance(parm_template, hou.FolderParmTemplate):
        data['folder_type'] = parm_template.folderType().name()
        data['children'] = [dump_parm_template(child) for child in parm_template.parmTemplates()]
    
    return data

def dump_parm_value(parm):
    """Get current parameter value"""
    try:
        if parm.parmTemplate().numComponents() > 1:
            return parm.eval()
        else:
            return parm.eval()
    except:
        return str(parm.eval())

def dump_cop_parameters(node, include_values=True, include_templates=True):
    """Dump COP node parameters to dictionary"""
    data = {
        'node_name': node.name(),
        'node_type': node.type().name(),
        'node_path': node.path(),
    }
    
    if include_templates:
        # Get parameter template information
        parm_group = node.parmTemplateGroup()
        data['parameter_templates'] = [dump_parm_template(pt) for pt in parm_group.parmTemplates()]
    
    if include_values:
        # Get current parameter values
        data['parameter_values'] = {}
        for parm in node.parms():
            data['parameter_values'][parm.name()] = dump_parm_value(parm)
    
    return data

def dump_multiple_nodes(cop_net, node_types):
    """Dump parameters for multiple node types"""
    results = {}
    
    for node_type in node_types:
        try:
            node = cop_net.createNode(node_type)
            node.moveToGoodPosition()
            
            param_data = dump_cop_parameters(node, include_values=True, include_templates=True)
            results[node_type] = param_data
            
            print(f"✓ Dumped {node_type}")
            
        except hou.OperationFailed as e:
            print(f"✗ Failed to create {node_type}: {e}")
            results[node_type] = {"error": str(e)}
    
    return results

# Main script
if __name__ == '__main__':
    # Node types to dump (can be passed as command line args)
    if len(sys.argv) > 1:
        node_types = sys.argv[1:]
    else:
        # Default node types based on your opaque functions
        node_types = ['blend', 'blocktogeo', 'blur']
    
    # Get or create OBJ context
    obj = hou.node('/obj')
    if obj is None:
        print("Error: /obj context not found")
        exit(1)
    
    # Create a Copernicus network inside /obj
    cop_net = obj.createNode('copnet', 'copnet_dump')
    print(f"Created Copernicus network: {cop_net.path()}\n")
    
    # Dump all requested node types
    print(f"Dumping {len(node_types)} node type(s)...")
    print("=" * 80)
    
    results = dump_multiple_nodes(cop_net, node_types)
    
    print("=" * 80)
    print()
    
    # Save to individual JSON files
    output_dir = "Parameters" # hou.text.expandString('$HIP')
    
    for node_type, data in results.items():
        if 'error' not in data:
            output_path = f"{output_dir}/cop_{node_type}_parameters.json"
            with open(output_path, 'w') as f:
                json.dump(data, f, indent=2, default=str)
            print(f"Saved: {output_path}")
    
    # Also save combined file
    combined_path = f"{output_dir}/cop_all_parameters.json"
    with open(combined_path, 'w') as f:
        json.dump(results, f, indent=2, default=str)
    
    print(f"\nCombined file: {combined_path}")
    
    # Clean up
    cop_net.destroy()
    print(f"\nCleaned up temporary network")
