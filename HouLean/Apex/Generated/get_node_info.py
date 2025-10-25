#!/opt/hfs21.0/bin/hython

import hou
import json

def get_cop_node_info(node_type_name):
    """Extract parameter information for a COP node type"""
    try:
        # Create a COP network if it doesn't exist
        cop_net = hou.node("/img")
        if cop_net is None:
            cop_net = hou.node("/").createNode("img")
        
        # COP nodes use "Cop2/" prefix
        full_type_name = f"Cop2/{node_type_name}"
        
        # Create the node
        node = cop_net.createNode(full_type_name)
        
        info = {
            "node_type": node_type_name,
            "full_type": full_type_name,
            "label": node.type().description(),
            "parameters": []
        }
        
        # Get all parameters
        for parm_tuple in node.parmTuples():
            parm_info = {
                "name": parm_tuple.name(),
                "label": parm_tuple.description(),
                "type": str(parm_tuple.parmTemplate().type()),
                "size": len(parm_tuple),
                "default": []
            }
            
            # Get default values
            for i, parm in enumerate(parm_tuple):
                try:
                    if parm.parmTemplate().type() == hou.parmTemplateType.String:
                        parm_info["default"].append(parm.unexpandedString())
                    else:
                        parm_info["default"].append(parm.eval())
                except:
                    parm_info["default"].append(None)
            
            # Simplify single-value parameters
            if len(parm_info["default"]) == 1:
                parm_info["default"] = parm_info["default"][0]
            
            # Add additional info
            parm_template = parm_tuple.parmTemplate()
            if hasattr(parm_template, 'menuItems'):
                parm_info["menu_items"] = list(parm_template.menuItems())
                parm_info["menu_labels"] = list(parm_template.menuLabels())
            
            if hasattr(parm_template, 'minValue'):
                parm_info["min"] = parm_template.minValue()
            if hasattr(parm_template, 'maxValue'):
                parm_info["max"] = parm_template.maxValue()
            
            info["parameters"].append(parm_info)
        
        # Clean up
        node.destroy()
        
        return info
    
    except Exception as e:
        return {"error": str(e), "node_type": node_type_name}

def extract_all_cop_nodes():
    """Extract info for all COP nodes we're interested in"""
    
    cop_nodes = [
        "layer",
        "bright", 
        "hsv",
        "invert",
        "blur",
        "blend",
        "channelextract",
        "channelsplit",
        "channeljoin",
        "xform",
        "crop",
        "geotolayer",
        "layertogeo",
        "constant",
        "clamp",
        "flip",
        "mono",
        "remap",
        "premult",
        "resample",
    ]
    
    results = {}
    
    for node_name in cop_nodes:
        print(f"Processing {node_name}...")
        results[node_name] = get_cop_node_info(node_name)
    
    return results

if __name__ == "__main__":
    # Extract all COP node info
    cop_info = extract_all_cop_nodes()
    
    # Save to JSON file
    output_file = "cop_nodes_info.json"
    with open(output_file, "w") as f:
        json.dump(cop_info, f, indent=2)
    
    print(f"\nCOP node information saved to: {output_file}")
    
    # Also print a summary
    print("\n=== Summary ===")
    for node_name, info in cop_info.items():
        if "error" in info:
            print(f"{node_name}: ERROR - {info['error']}")
        else:
            print(f"{node_name}: {len(info['parameters'])} parameters")

