"""
Color definitions for the Houdini-style node editor
"""
from PySide6.QtGui import QColor

# Houdini color scheme
HOUDINI_BG = QColor(58, 58, 58)
HOUDINI_BG_DARK = QColor(38, 38, 38)
HOUDINI_BG_LIGHT = QColor(68, 68, 68)
HOUDINI_NODE_BG = QColor(48, 48, 48)
HOUDINI_NODE_BORDER = QColor(28, 28, 28)
HOUDINI_NODE_SELECTED = QColor(248, 156, 40)
HOUDINI_TEXT = QColor(220, 220, 220)
HOUDINI_TEXT_DIM = QColor(150, 150, 150)
HOUDINI_GRID_FINE = QColor(50, 50, 50)
HOUDINI_GRID_THICK = QColor(60, 60, 60)
HOUDINI_ERROR = QColor(220, 60, 60)
HOUDINI_IMPLICIT = QColor(120, 120, 120, 200)


def get_type_color(type_name, registry=None, node_type=None):
    """Get color for a port type"""
    colors = {
        'Float': QColor(120, 220, 120),
        'HouLean.Vector3': QColor(100, 180, 255),
        'Particle': QColor(255, 160, 100),
        'Array': QColor(200, 120, 200),
        'Type': QColor(180, 120, 255),
        'Int': QColor(120, 255, 180),
        'String': QColor(255, 180, 120),
        '?_': QColor(160, 160, 160),
    }
    
    if type_name in colors:
        return colors[type_name]
    else:
        if registry and _type_contains_wildcards(type_name, registry, node_type):
            return QColor(160, 160, 160)
        else:
            return QColor(255, 240, 230)


def _type_contains_wildcards(type_name, registry, node_type):
    """Check if a type or any of its subports contain wildcards"""
    if type_name == "?_":
        return True
    
    port_type = registry.get_port_type(type_name, node_type)
    if not port_type or not port_type.subports:
        return False
    
    # for subport_name, subport_type in port_type.subports:
    #     if _type_contains_wildcards(subport_type, registry, node_type):
    #         return True
    
    return False
