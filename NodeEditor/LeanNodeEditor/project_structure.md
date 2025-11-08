# Houdini Node Editor Project

A modular node-based editor for Houdini with Lean 4 JSON format support.

## Project Structure

```
node_editor/
├── colors.py           # Color definitions and type color mapping
├── types.py            # Type system (PortType, NodeType, Registry)
├── ports.py            # Port widget classes
├── expansions.py       # Subport expansion widgets
├── connections.py      # Connection/wire rendering
├── nodes.py            # Node widget class
├── view.py             # Main editor view and scene
├── serialization.py    # Save/load JSON functions
└── main.py             # Main widget and Houdini entry point
```

## Installation

1. Create a directory for the project in your Houdini scripts path
2. Copy all 9 Python files into that directory
3. In Houdini, create a new Python Panel

## Usage

In your Houdini Python Panel, paste:

```python
from main import createInterface
```

## Features

- **Lean 4 JSON Format**: Full support with builtin/struct port types
- **Structural Type Matching**: Wildcard support and flexible type matching
- **Nested Port Expansion**: Interactive port hierarchy with hover expansion
- **Custom Node Types**: Define nodes with custom port structures
- **Connection Management**: Expand/collapse connections, visual type matching
- **Node Editing**: Live node type editing with port value support
- **Houdini Integration**: Save/load networks to Houdini node parameters

## Keyboard Shortcuts

- **Tab / RMB**: Create node menu
- **Drag**: Select items
- **Left Click Port**: Start connection
- **Middle Mouse**: Pan view
- **Mouse Wheel**: Zoom
- **Delete/Backspace**: Delete selected
- **F2**: Rename selected node
- **M**: Change node type
- **E**: Expand selected connections
- **C**: Collapse selected connections
- **G**: Frame all nodes

## File Descriptions

### colors.py
Defines the Houdini color scheme and type color mapping system. Contains the `get_type_color()` function that assigns colors to different port types.

### types.py
Core type system classes:
- `PortType`: Represents a port type with optional nested subports
- `NodeType`: Defines a node with inputs, outputs, and Lean constant
- `NodeTypeRegistry`: Manages all port and node type definitions

### ports.py
Interactive port widgets:
- `PortWidget`: Main port circle on nodes
- `SubportWidget`: Nested ports within expanded structures
- Handles hover expansion, type matching, and connection validation

### expansions.py
- `SubportExpansion`: Visual panel that displays nested subports
- Manages layout and positioning of hierarchical port structures

### connections.py
- `Connection`: Renders wires between ports
- Handles path updates, type mismatch visualization, and connection expansion/collapse

### nodes.py
- `NodeWidget`: Renders individual nodes in the graph
- Manages port layout, node resizing, and type changes

### view.py
Main canvas classes:
- `NodeEditorScene`: QGraphicsScene setup
- `NodeEditorView`: Interactive canvas with pan, zoom, drag connections
- Handles all mouse/keyboard interactions and connection creation

### serialization.py
JSON import/export functions:
- `save_to_json()`: Exports graph to Lean 4 JSON format
- `load_from_json()`: Imports graph from JSON
- `_find_port_by_index()`: Helper for port reconnection

### main.py
Main application widget:
- `NodeEditorWidget`: Complete UI with toolbar and side panel
- Node definition editor
- File operations and Houdini node parameter integration
- `createInterface()`: Entry point for Houdini

## Default Node Types

The editor comes with three default node types:
- **Float_Add**: Adds two floats
- **add**: Generic addition with wildcard types
- **makeVector3**: Constructs a Vector3 from x, y, z components

## Extending

To add custom node types, create a JSON file with this structure:

```json
{
  "portTypes": [
    {
      "builtin": {"name": "value", "type": "MyType"}
    }
  ],
  "nodeTypes": [
    {
      "name": "MyNode",
      "leanConstant": "MyModule.myFunction",
      "exposeImplicitArgs": false,
      "inputs": [
        {"builtin": {"name": "input", "type": "MyType"}}
      ],
      "outputs": [
        {"builtin": {"name": "output", "type": "MyType"}}
      ]
    }
  ]
}
```

Load it using the "Load Types" button in the toolbar.

## License

Open source - use freely in your projects.
