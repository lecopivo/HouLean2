"""
Serialization functions for saving and loading graphs
"""
from PySide6.QtCore import QPointF
from .connections import Connection
from .nodes import NodeWidget


def save_to_json(view):
    """Save the graph to JSON format"""
    nodes_data = []
    connections_data = []
    
    for node in view.nodes:
        nodes_data.append(node.to_dict())
    
    for conn in view.connections:
        connections_data.append({
            'outputNodeName': conn.output_node.node_name if conn.output_node else None,
            'outputIndex': conn.output_index_path,
            'inputNodeName': conn.input_node.node_name if conn.input_node else None,
            'inputIndex': conn.input_index_path,
            'outputRelPos': conn.output_rel_pos,
            'inputRelPos': conn.input_rel_pos,
            'isImplicit': conn.is_implicit
        })
    
    port_types_data = []
    for type_name, port_type in view.registry.port_types.items():
        if port_type.subports:
            subports_json = []
            for sp_name, sp_type in port_type.subports:
                subports_json.append({
                    'builtin': {'name': sp_name, 'type': sp_type}
                })
            port_types_data.append({
                'struct': {
                    'name': port_type.name,
                    'type': type_name,
                    'subports': subports_json
                }
            })
        else:
            port_types_data.append({
                'builtin': {
                    'name': port_type.name,
                    'type': type_name
                }
            })
    
    node_types_data = []
    for node_type in view.registry.node_types.values():
        def port_to_lean_json(port_name, port_type_name):
            if port_type_name in node_type.custom_port_types:
                custom_type = node_type.custom_port_types[port_type_name]
                subports_json = []
                for sp_name, sp_type in custom_type.subports:
                    subports_json.append(port_to_lean_json(sp_name, sp_type))
                return {
                    'struct': {
                        'name': port_name,
                        'type': port_type_name,
                        'subports': subports_json
                    }
                }
            else:
                return {
                    'builtin': {
                        'name': port_name,
                        'type': port_type_name
                    }
                }
        
        node_types_data.append({
            'name': node_type.name,
            'leanConstant': node_type.lean_constant,
            'exposeImplicitArgs': node_type.expose_implicit_args,
            'inputs': [port_to_lean_json(name, type_) for name, type_ in node_type.inputs],
            'outputs': [port_to_lean_json(name, type_) for name, type_ in node_type.outputs]
        })
    
    return {
        'portTypes': port_types_data,
        'nodeTypes': node_types_data,
        'nodes': nodes_data,
        'connections': connections_data
    }


def load_from_json(view, data):
    """Load graph from JSON format"""
    if 'portTypes' in data or 'nodeTypes' in data:
        view.registry.load_from_json(data)
    
    for node in view.nodes[:]:
        all_ports = node.get_all_ports()
        for port in all_ports:
            for conn in port.connections[:]:
                if conn in view.connections:
                    view.connections.remove(conn)
                if conn.scene():
                    view.editor_scene.removeItem(conn)
        view.editor_scene.removeItem(node)
    view.nodes.clear()
    view.connections.clear()
    
    node_map = {}
    for node_data in data.get('nodes', []):
        type_data = node_data.get('type', {})
        node_type = view.registry._parse_node_type(type_data)
        
        if node_type:
            port_values = node_data.get('portValues', [])
            node = NodeWidget(node_type, view.registry, node_data['name'], port_values)
            node.setPos(QPointF(node_data['x'], node_data['y']))
            view.editor_scene.addItem(node)
            view.nodes.append(node)
            view._update_node_ports_matching(node)
            node_map[node_data['name']] = node
    
    for conn_data in data.get('connections', []):
        output_node = node_map.get(conn_data['outputNodeName'])
        input_node = node_map.get(conn_data['inputNodeName'])
        output_index_path = conn_data.get('outputIndex', [])
        input_index_path = conn_data.get('inputIndex', [])
        is_implicit = conn_data.get('isImplicit', False)
        
        if output_node or input_node:
            output_port = None
            input_port = None
            
            if output_node and output_index_path:
                output_port = _find_port_by_index(output_node, output_index_path, False)
            
            if input_node and input_index_path:
                input_port = _find_port_by_index(input_node, input_index_path, True)
            
            conn = Connection(output_port, input_port, view.editor_scene, 
                            output_index_path, input_index_path, is_implicit)
            conn.output_node = output_node
            conn.input_node = input_node
            conn.output_rel_pos = conn_data.get('outputRelPos', 0)
            conn.input_rel_pos = conn_data.get('inputRelPos', 0)
            conn.output_valid = output_port is not None
            conn.input_valid = input_port is not None
            
            conn.setVisible(not is_implicit or view.show_implicit_connections)
            
            view.editor_scene.addItem(conn)
            view.connections.append(conn)


def _find_port_by_index(node, index_path, is_input):
    """Find port by index path"""
    if not node or not index_path:
        return None
    
    ports = node.input_ports if is_input else node.output_ports
    
    if index_path[0] >= len(ports):
        return None
    
    current_port = ports[index_path[0]]
    
    for idx in index_path[1:]:
        if not current_port.expanded:
            current_port.expand()
        
        if idx >= len(current_port.subport_widgets):
            return None
        
        current_port = current_port.subport_widgets[idx]
    
    return current_port
