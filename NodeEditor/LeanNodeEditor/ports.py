"""
Port widget classes for node editor
"""
from PySide6.QtWidgets import QGraphicsItem, QGraphicsRectItem, QGraphicsTextItem
from PySide6.QtCore import Qt, QRectF, QPointF, QTimer
from PySide6.QtGui import QPen, QBrush, QPainterPath, QFont, QPainter

from .colors import *


class SubportWidget(QGraphicsItem):
    PORT_RADIUS = 5
    SUBPORT_SPACING = 22
    
    def __init__(self, name, type_name, is_input, parent_port, registry, node_type=None):
        super().__init__()
        self.name = name
        self.type_name = type_name
        self.is_input = is_input
        self.parent_port = parent_port
        self.node = parent_port.node
        self.registry = registry
        self.node_type = node_type or (parent_port.node_type if hasattr(parent_port, 'node_type') else None)
        self.connections = []
        self.hover_active = False
        
        self.subport_widgets = []
        self.expanded = False
        self.expansion_widget = None
        
        port_type = registry.get_port_type(type_name, self.node_type)
        self.subport_defs = []
        if port_type and port_type.subports:
            self.subport_defs = port_type.subports
        
        self.allow_structural_matching = True
        
        self.setAcceptHoverEvents(True)
        self.setFlag(QGraphicsItem.ItemIsSelectable, False)
        self.setAcceptedMouseButtons(Qt.NoButton)
    
    def boundingRect(self):
        return QRectF(-self.PORT_RADIUS, -self.PORT_RADIUS, 
                     self.PORT_RADIUS * 2, self.PORT_RADIUS * 2)
    
    def shape(self):
        path = QPainterPath()
        radius = self.PORT_RADIUS + 5
        path.addEllipse(QPointF(0, 0), radius, radius)
        return path
    
    def paint(self, painter, option, widget):
        painter.setRenderHint(QPainter.Antialiasing)
        color = get_type_color(self.type_name, self.registry, self.node_type)
        
        if self.has_connection():
            painter.setBrush(QBrush(color))
        else:
            painter.setBrush(QBrush(color.darker(180)))
        
        border_width = 2.5 if self.hover_active else 1.5
        painter.setPen(QPen(color.darker(120), border_width))
        painter.drawEllipse(self.boundingRect())
    
    def get_full_path(self):
        return f"{self.parent_port.get_full_path()}.{self.name}"
    
    def get_index_path(self):
        """Get the index path [port_idx, subport_idx, ...]"""
        path = []
        current = self
        while hasattr(current, 'parent_port') and current.parent_port:
            parent = current.parent_port
            if hasattr(parent, 'subport_widgets'):
                idx = parent.subport_widgets.index(current) if current in parent.subport_widgets else -1
                path.insert(0, idx)
            current = parent
        
        if hasattr(current, 'node'):
            ports = current.node.input_ports if current.is_input else current.node.output_ports
            if current in ports:
                path.insert(0, ports.index(current))
        
        return path
    
    def has_connection(self):
        if self.connections:
            return True
        for subport in self.subport_widgets:
            if subport.has_connection():
                return True
        return False
    
    def is_connectable_to(self, other_port):
        if self.is_input == other_port.is_input:
            return False
        if self.node == other_port.node:
            return False
        
        if not self.types_are_compatible(other_port):
            return False
        
        input_port = self if self.is_input else other_port
        if input_port.is_occupied():
            return False
        return True
    
    def types_are_compatible(self, other_port):
        """Check if types are compatible"""
        if self.allow_structural_matching:
            return self._types_structurally_match(other_port)
        else:
            return self.type_name == other_port.type_name
    
    def _types_structurally_match(self, other_port):
        """Check if two ports have matching structure"""
        if self.type_name == "?_" or other_port.type_name == "?_":
            return True
        
        if self.type_name == other_port.type_name:
            return True
        
        my_port_type = self.registry.get_port_type(self.type_name, self.node_type)
        other_port_type = self.registry.get_port_type(other_port.type_name, 
                                                       other_port.node_type if hasattr(other_port, 'node_type') else None)
        
        if not my_port_type or not my_port_type.subports:
            if not other_port_type or not other_port_type.subports:
                return self.type_name == other_port.type_name
            return False
        
        if not other_port_type or not other_port_type.subports:
            return False
        
        my_subports = my_port_type.subports
        other_subports = other_port_type.subports
        
        if len(my_subports) != len(other_subports):
            return False
        
        my_has_wildcards = self._structure_has_wildcards(my_subports)
        other_has_wildcards = self._structure_has_wildcards(other_subports)
        
        for (my_name, my_type), (other_name, other_type) in zip(my_subports, other_subports):
            if not (my_has_wildcards or other_has_wildcards):
                if my_name != other_name:
                    return False
            
            temp_my_subport = SubportWidget(my_name, my_type, self.is_input, self, self.registry, self.node_type)
            temp_other_subport = SubportWidget(other_name, other_type, other_port.is_input, other_port, 
                                               self.registry, other_port.node_type if hasattr(other_port, 'node_type') else None)
            
            if not temp_my_subport._types_structurally_match(temp_other_subport):
                return False
        
        return True
    
    def _structure_has_wildcards(self, subports):
        """Check if a structure contains any wildcard types"""
        for name, type_name in subports:
            if type_name == "?_":
                return True
            port_type = self.registry.get_port_type(type_name, self.node_type)
            if port_type and port_type.subports:
                if self._structure_has_wildcards(port_type.subports):
                    return True
        return False
    
    def is_occupied(self):
        if any(not conn.is_implicit for conn in self.connections):
            return True
        current = self.parent_port
        while current:
            if any(not conn.is_implicit for conn in current.connections):
                return True
            current = current.parent_port if hasattr(current, 'parent_port') else None
        return self._has_connected_descendant()
    
    def _has_connected_descendant(self):
        for subport in self.subport_widgets:
            if any(not conn.is_implicit for conn in subport.connections) or subport._has_connected_descendant():
                return True
        return False
    
    def get_scene_pos(self):
        return self.scenePos()
    
    def expand(self):
        from .expansions import SubportExpansion
        
        if not self.subport_defs or self.expanded:
            return
        
        self.expanded = True
        original_scene_pos = self.get_scene_pos()
        
        self.subport_widgets = []
        for subport_name, subport_type in self.subport_defs:
            subport = SubportWidget(subport_name, subport_type, self.is_input, self, self.registry, self.node_type)
            subport.allow_structural_matching = self.allow_structural_matching
            self.subport_widgets.append(subport)
        
        self.expansion_widget = SubportExpansion(self, self.subport_widgets, self.is_input)
        self.expansion_widget.setParentItem(self.node)
        
        node_pos = self.node.mapFromItem(self, 0, 0)
        if self.is_input:
            self.expansion_widget.setPos(node_pos.x() - 2*SubportExpansion.WIDTH, 
                                        node_pos.y() + self.PORT_RADIUS)
        else:
            self.expansion_widget.setPos(node_pos.x() + SubportExpansion.WIDTH, 
                                        node_pos.y() + self.PORT_RADIUS)
        
        self._update_parent_expansions()
        
        if self.node:
            self.node.adjust_size_for_expansion(self, original_scene_pos)
    
    def _update_parent_expansions(self):
        current = self.parent_port
        while current:
            if hasattr(current, 'expansion_widget') and current.expansion_widget:
                current.expansion_widget._update_subport_positions()
            if hasattr(current, 'parent_port'):
                current = current.parent_port
            else:
                break
    
    def collapse(self):
        if not self.expanded:
            return
        
        if self.node and self.node.scene():
            view = self.node.scene().views()[0] if self.node.scene().views() else None
            if view and hasattr(view, '_is_port_locked') and view._is_port_locked(self):
                return
        
        has_connections = any(subport.has_connection() for subport in self.subport_widgets)
        
        if not has_connections:
            original_scene_pos = self.get_scene_pos()
            self.expanded = False
            
            for subport in self.subport_widgets[:]:
                if subport.expanded:
                    subport.collapse()
            
            if self.expansion_widget:
                scene = self.scene()
                if scene and self.expansion_widget.scene() == scene:
                    scene.removeItem(self.expansion_widget)
                self.expansion_widget = None
            
            self.subport_widgets = []
            self._update_parent_expansions()
            
            if self.node:
                self.node.adjust_size_for_expansion(self, original_scene_pos)
    
    def hoverEnterEvent(self, event):
        self.hover_active = True
        self.update()
        if self.is_input and self.is_occupied():
            super().hoverEnterEvent(event)
            return
        if self.subport_defs and not self.expanded:
            self.expand()
        super().hoverEnterEvent(event)
    
    def hoverLeaveEvent(self, event):
        self.hover_active = False
        self.update()
        QTimer.singleShot(500, self.try_collapse)
        super().hoverLeaveEvent(event)
    
    def try_collapse(self):
        if self.hover_active:
            return
        if self.expansion_widget and self.expansion_widget.isUnderMouse():
            return
        if self._is_any_expansion_hovered():
            return
        self.collapse()
    
    def _is_any_expansion_hovered(self):
        if self.expansion_widget and self.expansion_widget.isUnderMouse():
            return True
        for subport in self.subport_widgets:
            if subport.hover_active:
                return True
            if subport._is_any_expansion_hovered():
                return True
        return False


class PortWidget(QGraphicsItem):
    PORT_RADIUS = 6
    SUBPORT_SPACING = 22
    
    def __init__(self, name, type_name, registry, is_input, parent_port=None, node=None, node_type=None):
        super().__init__()
        self.name = name
        self.type_name = type_name
        self.registry = registry
        self.is_input = is_input
        self.parent_port = parent_port
        self.node = node
        self.node_type = node_type
        self.subport_widgets = []
        self.expanded = False
        self.expansion_widget = None
        self.connections = []
        self.hover_active = False
        
        port_type = registry.get_port_type(type_name, self.node_type)
        self.subport_defs = []
        if port_type and port_type.subports:
            self.subport_defs = port_type.subports
        
        self.allow_structural_matching = True
        
        self.setAcceptHoverEvents(True)
        self.setFlag(QGraphicsItem.ItemIsSelectable, False)
        self.setAcceptedMouseButtons(Qt.NoButton)
    
    def boundingRect(self):
        return QRectF(-self.PORT_RADIUS, -self.PORT_RADIUS, 
                     self.PORT_RADIUS * 2, self.PORT_RADIUS * 2)
    
    def shape(self):
        path = QPainterPath()
        radius = self.PORT_RADIUS + 5
        path.addEllipse(QPointF(0, 0), radius, radius)
        return path
    
    def paint(self, painter, option, widget):
        painter.setRenderHint(QPainter.Antialiasing)
        color = get_type_color(self.type_name, self.registry, self.node_type)
        
        if self.has_connection():
            painter.setBrush(QBrush(color))
        else:
            painter.setBrush(QBrush(color.darker(180)))
        
        border_width = 2.5 if self.hover_active else 1.5
        painter.setPen(QPen(color.darker(120), border_width))
        painter.drawEllipse(self.boundingRect())
    
    def get_full_path(self):
        if self.parent_port:
            return f"{self.parent_port.get_full_path()}.{self.name}"
        return self.name
    
    def get_index_path(self):
        """Get the index path [port_idx, subport_idx, ...]"""
        ports = self.node.input_ports if self.is_input else self.node.output_ports
        if self in ports:
            return [ports.index(self)]
        return []
    
    def has_connection(self):
        if self.connections:
            return True
        for subport in self.subport_widgets:
            if subport.has_connection():
                return True
        return False
    
    def is_connectable_to(self, other_port):
        if self.is_input == other_port.is_input:
            return False
        if self.node == other_port.node:
            return False
        
        if not self.types_are_compatible(other_port):
            return False
        
        input_port = self if self.is_input else other_port
        if input_port.is_occupied():
            return False
        return True
    
    def types_are_compatible(self, other_port):
        """Check if types are compatible"""
        if self.allow_structural_matching:
            return self._types_structurally_match(other_port)
        else:
            return self.type_name == other_port.type_name
    
    def _types_structurally_match(self, other_port):
        """Check if two ports have matching structure"""
        if self.type_name == "?_" or other_port.type_name == "?_":
            return True
        
        if self.type_name == other_port.type_name:
            return True
        
        my_port_type = self.registry.get_port_type(self.type_name, self.node_type)
        other_port_type = self.registry.get_port_type(other_port.type_name, 
                                                       other_port.node_type if hasattr(other_port, 'node_type') else None)
        
        if not my_port_type or not my_port_type.subports:
            if not other_port_type or not other_port_type.subports:
                return self.type_name == other_port.type_name
            return False
        
        if not other_port_type or not other_port_type.subports:
            return False
        
        my_subports = my_port_type.subports
        other_subports = other_port_type.subports
        
        if len(my_subports) != len(other_subports):
            return False
        
        my_has_wildcards = self._structure_has_wildcards(my_subports)
        other_has_wildcards = self._structure_has_wildcards(other_subports)
        
        for (my_name, my_type), (other_name, other_type) in zip(my_subports, other_subports):
            if not (my_has_wildcards or other_has_wildcards):
                if my_name != other_name:
                    return False
            
            temp_my_port = PortWidget(my_name, my_type, self.registry, self.is_input, None, self.node, self.node_type)
            temp_other_port = PortWidget(other_name, other_type, self.registry, other_port.is_input, None, 
                                         other_port.node, other_port.node_type if hasattr(other_port, 'node_type') else None)
            
            if not temp_my_port._types_structurally_match(temp_other_port):
                return False
        
        return True
    
    def _structure_has_wildcards(self, subports):
        """Check if a structure contains any wildcard types"""
        for name, type_name in subports:
            if type_name == "?_":
                return True
            port_type = self.registry.get_port_type(type_name, self.node_type)
            if port_type and port_type.subports:
                if self._structure_has_wildcards(port_type.subports):
                    return True
        return False
    
    def is_occupied(self):
        if any(not conn.is_implicit for conn in self.connections):
            return True
        current = self.parent_port
        while current:
            if any(not conn.is_implicit for conn in current.connections):
                return True
            current = current.parent_port
        return self._has_connected_descendant()
    
    def _has_connected_descendant(self):
        for subport in self.subport_widgets:
            if any(not conn.is_implicit for conn in subport.connections) or subport._has_connected_descendant():
                return True
        return False
    
    def get_scene_pos(self):
        return self.scenePos()
    
    def expand(self):
        from .expansions import SubportExpansion
        
        if not self.subport_defs or self.expanded:
            return
        
        self.expanded = True
        original_scene_pos = self.get_scene_pos()
        
        self.subport_widgets = []
        for subport_name, subport_type in self.subport_defs:
            subport = SubportWidget(subport_name, subport_type, self.is_input, self, self.registry, self.node_type)
            subport.allow_structural_matching = self.allow_structural_matching
            self.subport_widgets.append(subport)
        
        self.expansion_widget = SubportExpansion(self, self.subport_widgets, self.is_input)
        self.expansion_widget.setParentItem(self.node)
        
        node_pos = self.node.mapFromItem(self, 0, 0)
        if self.is_input:
            self.expansion_widget.setPos(node_pos.x() - 2*SubportExpansion.WIDTH, 
                                        node_pos.y() + self.PORT_RADIUS)
        else:
            self.expansion_widget.setPos(node_pos.x() + SubportExpansion.WIDTH, 
                                        node_pos.y() + self.PORT_RADIUS)
        
        self._update_parent_expansions()
        
        if self.node:
            self.node.adjust_size_for_expansion(self, original_scene_pos)
    
    def _update_parent_expansions(self):
        current = self.parent_port
        while current:
            if hasattr(current, 'expansion_widget') and current.expansion_widget:
                current.expansion_widget._update_subport_positions()
            if hasattr(current, 'parent_port'):
                current = current.parent_port
            else:
                break
    
    def collapse(self):
        if not self.expanded:
            return
        
        if self.node and self.node.scene():
            view = self.node.scene().views()[0] if self.node.scene().views() else None
            if view and hasattr(view, '_is_port_locked') and view._is_port_locked(self):
                return
        
        has_connections = any(subport.has_connection() for subport in self.subport_widgets)
        
        if not has_connections:
            original_scene_pos = self.get_scene_pos()
            self.expanded = False
            
            for subport in self.subport_widgets[:]:
                if subport.expanded:
                    subport.collapse()
            
            if self.expansion_widget:
                scene = self.scene()
                if scene and self.expansion_widget.scene() == scene:
                    scene.removeItem(self.expansion_widget)
                self.expansion_widget = None
            
            self.subport_widgets = []
            
            if self.node:
                self.node.adjust_size_for_expansion(self, original_scene_pos)
    
    def hoverEnterEvent(self, event):
        self.hover_active = True
        self.update()
        if self.is_input and self.is_occupied():
            super().hoverEnterEvent(event)
            return
        if self.subport_defs and not self.expanded:
            self.expand()
        super().hoverEnterEvent(event)
    
    def hoverLeaveEvent(self, event):
        self.hover_active = False
        self.update()
        QTimer.singleShot(500, self.try_collapse)
        super().hoverLeaveEvent(event)
    
    def try_collapse(self):
        if self.hover_active:
            return
        if self.expansion_widget and self.expansion_widget.isUnderMouse():
            return
        if self._is_any_expansion_hovered():
            return
        self.collapse()
    
    def _is_any_expansion_hovered(self):
        if self.expansion_widget and self.expansion_widget.isUnderMouse():
            return True
        for subport in self.subport_widgets:
            if subport.hover_active:
                return True
            if subport._is_any_expansion_hovered():
                return True
        return False
