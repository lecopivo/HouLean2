"""
Type system classes for node editor
"""
import json


class PortType:
    def __init__(self, name, type_name, subports=None):
        self.name = name
        self.type_name = type_name
        self.subports = subports or []


class NodeType:
    def __init__(self, name, lean_constant, inputs, outputs, expose_implicit_args=False, custom_port_types=None):
        self.name = name
        self.lean_constant = lean_constant
        self.expose_implicit_args = expose_implicit_args
        self.inputs = inputs
        self.outputs = outputs
        self.custom_port_types = custom_port_types or {}


class NodeTypeRegistry:
    def __init__(self):
        self.node_types = {}
        self.port_types = {}
    
    def load_from_json(self, json_data):
        data = json.loads(json_data) if isinstance(json_data, str) else json_data
        
        for port_type_data in data.get('portTypes', []):
            port_type = self._parse_port_type(port_type_data)
            if port_type:
                self.port_types[port_type.type_name] = port_type
        
        for node_def in data.get('nodeTypes', []):
            node_type = self._parse_node_type(node_def)
            if node_type:
                self.node_types[node_type.name] = node_type
    
    def _parse_port_type(self, port_data):
        """Parse PortType from Lean JSON format (builtin or struct)"""
        if 'builtin' in port_data:
            builtin = port_data['builtin']
            return PortType(builtin['name'], builtin['type'], [])
        elif 'struct' in port_data:
            struct = port_data['struct']
            subports = []
            for sp_data in struct.get('subports', []):
                sp = self._parse_port_type(sp_data)
                if sp:
                    subports.append((sp.name, sp.type_name))
            return PortType(struct['name'], struct['type'], subports)
        return None
    
    def _parse_node_type(self, node_def):
        """Parse NodeType from Lean JSON format"""
        custom_port_types = {}
        
        def extract_custom_types(port_data):
            if 'struct' in port_data:
                struct = port_data['struct']
                type_name = struct['type']
                subports = []
                
                for sp_data in struct.get('subports', []):
                    sp = self._parse_port_type(sp_data)
                    if sp:
                        subports.append((sp.name, sp.type_name))
                        extract_custom_types(sp_data)
                
                if type_name not in self.port_types:
                    custom_port_types[type_name] = PortType(struct['name'], type_name, subports)
        
        inputs = []
        for inp_data in node_def.get('inputs', []):
            inp = self._parse_port_type(inp_data)
            if inp:
                inputs.append((inp.name, inp.type_name))
                extract_custom_types(inp_data)
        
        outputs = []
        for out_data in node_def.get('outputs', []):
            out = self._parse_port_type(out_data)
            if out:
                outputs.append((out.name, out.type_name))
                extract_custom_types(out_data)
        
        return NodeType(
            node_def['name'],
            node_def.get('leanConstant', ''),
            inputs,
            outputs,
            node_def.get('exposeImplicitArgs', False),
            custom_port_types
        )
    
    def get_port_type(self, type_name, node_type=None):
        """Get port type, checking node-specific custom types first"""
        if node_type and type_name in node_type.custom_port_types:
            return node_type.custom_port_types[type_name]
        return self.port_types.get(type_name)
    
    def get_node_type(self, type_name):
        return self.node_types.get(type_name)
