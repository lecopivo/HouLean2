"""
Connection/wire rendering between ports
"""
from PySide6.QtWidgets import QGraphicsItem, QGraphicsPathItem
from PySide6.QtCore import Qt, QPointF, QTimer
from PySide6.QtGui import QPen, QBrush, QPainterPath, QPainterPathStroker, QLinearGradient, QPainter

from .colors import *
from .ports import SubportWidget


class Connection(QGraphicsPathItem):
    def __init__(self, output_port, input_port, scene, output_index_path=None, input_index_path=None, is_implicit=False):
        super().__init__()
        self.output_port = output_port
        self.input_port = input_port
        self.output_node = output_port.node if output_port else None
        self.input_node = input_port.node if input_port else None
        self.is_implicit = is_implicit
        
        self.output_index_path = output_index_path or (output_port.get_index_path() if output_port else [])
        self.input_index_path = input_index_path or (input_port.get_index_path() if input_port else [])
        
        if self.output_node and output_port:
            self.output_rel_pos = output_port.get_scene_pos().y() - self.output_node.pos().y()
        else:
            self.output_rel_pos = 0
            
        if self.input_node and input_port:
            self.input_rel_pos = input_port.get_scene_pos().y() - self.input_node.pos().y()
        else:
            self.input_rel_pos = 0
        
        self.setFlag(QGraphicsItem.ItemIsSelectable, not is_implicit)
        self.setFlag(QGraphicsItem.ItemIsMovable, False)
        
        self.output_valid = output_port is not None
        self.input_valid = input_port is not None
        
        if is_implicit:
            self.normal_pen = QPen(HOUDINI_IMPLICIT, 2.0, Qt.DotLine)
        else:
            color = get_type_color(output_port.type_name, 
                                  output_port.registry if output_port else None,
                                  output_port.node_type if output_port and hasattr(output_port, 'node_type') else None) if output_port else HOUDINI_ERROR
            self.normal_pen = QPen(color, 2.5)
        
        self.selected_pen = QPen(HOUDINI_NODE_SELECTED, 3.5)
        self.invalid_pen = QPen(HOUDINI_ERROR, 2.5)
        self.shadow_pen = QPen(HOUDINI_TEXT.lighter(150), 6.0)
        
        if output_port:
            output_port.connections.append(self)
        if input_port:
            input_port.connections.append(self)
        
        if output_port and isinstance(output_port, SubportWidget):
            self._expand_hierarchy(output_port)
        if input_port and isinstance(input_port, SubportWidget):
            self._expand_hierarchy(input_port)
        
        self.update_path()
    
    def is_valid(self):
        """Check if connection is valid"""
        if not (self.output_valid and self.input_valid):
            return False
        if self.output_port and self.input_port:
            return self.output_port.type_name == self.input_port.type_name
        return False
    
    def has_type_mismatch(self):
        """Check if both ends exist but types don't match"""
        return (self.output_valid and self.input_valid and 
                self.output_port.type_name != self.input_port.type_name)
    
    def boundingRect(self):
        if self.isSelected():
            return self.path().boundingRect().adjusted(-4, -4, 4, 4)
        return self.path().boundingRect()
    
    def shape(self):
        stroker = QPainterPathStroker()
        stroker.setWidth(12)
        stroker.setCapStyle(Qt.RoundCap)
        return stroker.createStroke(self.path())
    
    def _expand_hierarchy(self, port):
        if isinstance(port, SubportWidget):
            try:
                current = port.parent_port
                while current:
                    _ = current.scene()
                    if not current.expanded:
                        current.expand()
                    current = current.parent_port if hasattr(current, 'parent_port') else None
            except RuntimeError:
                pass
    
    def update_path(self):
        try:
            if self.output_port:
                start = self.output_port.get_scene_pos()
            elif self.output_node:
                start = QPointF(self.output_node.pos().x() + self.output_node.rect().width(),
                              self.output_node.pos().y() + self.output_rel_pos)
            else:
                return
            
            if self.input_port:
                end = self.input_port.get_scene_pos()
            elif self.input_node:
                end = QPointF(self.input_node.pos().x(),
                            self.input_node.pos().y() + self.input_rel_pos)
            else:
                return
            
            path = QPainterPath()
            path.moveTo(start)
            
            ctrl_offset = abs(end.x() - start.x()) * 0.5
            ctrl1 = QPointF(start.x() + ctrl_offset, start.y())
            ctrl2 = QPointF(end.x() - ctrl_offset, end.y())
            path.cubicTo(ctrl1, ctrl2, end)
            
            self.setPath(path)
        except RuntimeError:
            if self.scene():
                self.scene().removeItem(self)
    
    def remove(self):
        try:
            if self.output_port and self in self.output_port.connections:
                self.output_port.connections.remove(self)
        except (RuntimeError, AttributeError):
            pass
            
        try:
            if self.input_port and self in self.input_port.connections:
                self.input_port.connections.remove(self)
        except (RuntimeError, AttributeError):
            pass
        
        if not self.is_implicit:
            def try_collapse_hierarchy(port):
                try:
                    if isinstance(port, SubportWidget):
                        port.collapse()
                        if hasattr(port, 'parent_port') and port.parent_port:
                            try_collapse_hierarchy(port.parent_port)
                    elif hasattr(port, 'collapse'):
                        port.collapse()
                except RuntimeError:
                    pass
            
            if self.output_port:
                try:
                    try_collapse_hierarchy(self.output_port)
                except (RuntimeError, AttributeError):
                    pass
                
            if self.input_port:
                try:
                    try_collapse_hierarchy(self.input_port)
                except (RuntimeError, AttributeError):
                    pass
        
        if self.scene():
            self.scene().removeItem(self)
    
    def paint(self, painter, option, widget):
        painter.setRenderHint(QPainter.Antialiasing)
        
        if self.is_implicit:
            painter.setPen(self.normal_pen)
            painter.drawPath(self.path())
            return
        
        if self.isSelected():
            painter.setPen(self.shadow_pen)
            painter.drawPath(self.path())
        
        if self.has_type_mismatch():
            output_color = get_type_color(self.output_port.type_name,
                                         self.output_port.registry,
                                         self.output_port.node_type if hasattr(self.output_port, 'node_type') else None)
            input_color = get_type_color(self.input_port.type_name,
                                        self.input_port.registry,
                                        self.input_port.node_type if hasattr(self.input_port, 'node_type') else None)
            gradient = QLinearGradient(self.path().pointAtPercent(0), self.path().pointAtPercent(1))
            gradient.setColorAt(0, output_color)
            gradient.setColorAt(1, input_color)
            pen = QPen(QBrush(gradient), 2.5)
        elif not self.is_valid():
            if self.output_valid and not self.input_valid:
                gradient = QLinearGradient(self.path().pointAtPercent(0), self.path().pointAtPercent(1))
                gradient.setColorAt(0, self.normal_pen.color())
                gradient.setColorAt(1, HOUDINI_ERROR)
                pen = QPen(QBrush(gradient), 2.5)
            elif not self.output_valid and self.input_valid:
                gradient = QLinearGradient(self.path().pointAtPercent(0), self.path().pointAtPercent(1))
                gradient.setColorAt(0, HOUDINI_ERROR)
                gradient.setColorAt(1, self.normal_pen.color())
                pen = QPen(QBrush(gradient), 2.5)
            else:
                pen = self.invalid_pen
        else:
            pen = self.normal_pen
        
        painter.setPen(pen)
        painter.drawPath(self.path())
    
    def expand_connection(self):
        """Expand this connection into subport connections"""
        if self.is_implicit:
            return []
        
        if not self.output_port or not self.input_port:
            return []
        
        if not self.output_port.subport_defs or not self.input_port.subport_defs:
            return []
        
        if not self.output_port.expanded:
            self.output_port.expand()
        if not self.input_port.expanded:
            self.input_port.expand()
        
        output_subports = {sp.name: sp for sp in self.output_port.subport_widgets}
        input_subports = {sp.name: sp for sp in self.input_port.subport_widgets}
        
        new_connections = []
        for name in output_subports:
            if name in input_subports:
                out_sp = output_subports[name]
                in_sp = input_subports[name]
                if out_sp.types_are_compatible(in_sp):
                    conn = Connection(out_sp, in_sp, self.scene())
                    if self.scene():
                        self.scene().addItem(conn)
                    new_connections.append(conn)
        
        self.remove()
        return new_connections
    
    def reconnect(self):
        """Try to reconnect to ports based on stored index paths"""
        if self.output_port and self in self.output_port.connections:
            self.output_port.connections.remove(self)
        if self.input_port and self in self.input_port.connections:
            self.input_port.connections.remove(self)
        
        self.output_port = self._find_port_by_index(self.output_node, self.output_index_path, False)
        self.input_port = self._find_port_by_index(self.input_node, self.input_index_path, True)
        
        self.output_valid = self.output_port is not None
        self.input_valid = self.input_port is not None
        
        if self.output_node and self.output_port:
            self.output_rel_pos = self.output_port.get_scene_pos().y() - self.output_node.pos().y()
            
        if self.input_node and self.input_port:
            self.input_rel_pos = self.input_port.get_scene_pos().y() - self.input_node.pos().y()
        
        if not self.is_implicit and self.output_port:
            color = get_type_color(self.output_port.type_name,
                                  self.output_port.registry,
                                  self.output_port.node_type if hasattr(self.output_port, 'node_type') else None)
            self.normal_pen = QPen(color, 2.5)
        elif not self.is_implicit and self.input_port:
            color = get_type_color(self.input_port.type_name,
                                  self.input_port.registry,
                                  self.input_port.node_type if hasattr(self.input_port, 'node_type') else None)
            self.normal_pen = QPen(color, 2.5)
        
        if self.output_port and self not in self.output_port.connections:
            self.output_port.connections.append(self)
        if self.input_port and self not in self.input_port.connections:
            self.input_port.connections.append(self)
        
        if self.output_port and isinstance(self.output_port, SubportWidget):
            self._expand_hierarchy(self.output_port)
        if self.input_port and isinstance(self.input_port, SubportWidget):
            self._expand_hierarchy(self.input_port)
        
        self.update_path()
        self.update()
    
    def _find_port_by_index(self, node, index_path, is_input):
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
