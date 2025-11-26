"""
Node widget class for rendering nodes in the graph
"""
from PySide6.QtWidgets import (QGraphicsItem, QGraphicsRectItem, QGraphicsTextItem,
                                QGraphicsEllipseItem, QGraphicsLineItem, QGraphicsPolygonItem)
from PySide6.QtCore import Qt, QRectF, QPointF
from PySide6.QtGui import QPen, QBrush, QFont, QPainter, QColor, QPolygonF

from .colors import *
from .ports import PortWidget


class VisualizerToggle(QGraphicsEllipseItem):
    """A small circular toggle button for visualizers"""
    RADIUS = 8

    def __init__(self, parent, index):
        super().__init__(-self.RADIUS, -self.RADIUS, self.RADIUS * 2, self.RADIUS * 2, parent)
        self.index = index
        self.is_active = False

        self.setBrush(QBrush(QColor(60, 60, 60)))
        self.setPen(QPen(QColor(120, 120, 120), 1.5))
        self.setAcceptHoverEvents(True)
        self.setCursor(Qt.PointingHandCursor)

    def set_active(self, active):
        """Update visual state"""
        self.is_active = active
        if active:
            self.setBrush(QBrush(QColor(255, 140, 0)))  # Orange when active
            self.setPen(QPen(QColor(255, 180, 80), 2))
        else:
            self.setBrush(QBrush(QColor(60, 60, 60)))
            self.setPen(QPen(QColor(120, 120, 120), 1.5))

    def hoverEnterEvent(self, event):
        if not self.is_active:
            self.setBrush(QBrush(QColor(80, 80, 80)))
        super().hoverEnterEvent(event)

    def hoverLeaveEvent(self, event):
        if not self.is_active:
            self.setBrush(QBrush(QColor(60, 60, 60)))
        super().hoverLeaveEvent(event)

    def mousePressEvent(self, event):
        if event.button() == Qt.LeftButton:
            # Get the node widget and notify view
            node = self.parentItem()
            if node.scene() and node.scene().views():
                view = node.scene().views()[0]
                if hasattr(view, 'activate_visualizer'):
                    view.activate_visualizer(node, self.index)
        super().mousePressEvent(event)

class VisualizerNumberBox(QGraphicsRectItem):
    """A number input box with up/down arrows and adjacent toggle for many visualizers"""
    WIDTH = 50
    HEIGHT = 20
    ARROW_WIDTH = 12
    
    def __init__(self, parent, max_index):
        super().__init__(0, 0, self.WIDTH, self.HEIGHT, parent)
        self.max_index = max_index
        self.current_index = 0
        self.is_active = False
        self.dragging = False
        self.drag_start_y = 0
        self.drag_start_index = 0
        
        self.setBrush(QBrush(QColor(40, 40, 40)))
        self.setPen(QPen(QColor(100, 100, 100), 1))
        self.setAcceptHoverEvents(True)
        
        # Text display area (middle section)
        self.text = QGraphicsTextItem(str(self.current_index), self)
        self.text.setDefaultTextColor(HOUDINI_TEXT)
        font = QFont("Arial", 8)
        self.text.setFont(font)
        self._center_text()
        
        # Up arrow
        self.up_arrow = QGraphicsPolygonItem(self)
        self.up_arrow.setBrush(QBrush(QColor(80, 80, 80)))
        self.up_arrow.setPen(QPen(Qt.NoPen))
        self._create_up_arrow()
        
        # Down arrow
        self.down_arrow = QGraphicsPolygonItem(self)
        self.down_arrow.setBrush(QBrush(QColor(80, 80, 80)))
        self.down_arrow.setPen(QPen(Qt.NoPen))
        self._create_down_arrow()
        
        # Toggle button next to the box
        self.toggle = VisualizerToggle(parent, -1)
        self.toggle.setPos(
            self.pos().x() + self.WIDTH + 4 + VisualizerToggle.RADIUS,
            self.pos().y() + self.HEIGHT / 2
        )
        self.toggle.mousePressEvent = self._toggle_clicked
    
    def _create_up_arrow(self):
        """Create upward pointing triangle"""
        x_offset = self.WIDTH - self.ARROW_WIDTH
        y_center = self.HEIGHT / 2
        points = [
            QPointF(x_offset + self.ARROW_WIDTH / 2, y_center - 4),  # Top
            QPointF(x_offset + 3, y_center + 1),  # Bottom left
            QPointF(x_offset + self.ARROW_WIDTH - 3, y_center + 1)  # Bottom right
        ]
        from PySide6.QtGui import QPolygonF
        self.up_arrow.setPolygon(QPolygonF(points))
    
    def _create_down_arrow(self):
        """Create downward pointing triangle"""
        x_offset = self.WIDTH - self.ARROW_WIDTH
        y_center = self.HEIGHT / 2
        points = [
            QPointF(x_offset + self.ARROW_WIDTH / 2, y_center + 4),  # Bottom
            QPointF(x_offset + 3, y_center - 1),  # Top left
            QPointF(x_offset + self.ARROW_WIDTH - 3, y_center - 1)  # Top right
        ]
        from PySide6.QtGui import QPolygonF
        self.down_arrow.setPolygon(QPolygonF(points))
    
    def _toggle_clicked(self, event):
        """Handle toggle click"""
        if event.button() == Qt.LeftButton:
            node = self.parentItem()
            if node.scene() and node.scene().views():
                view = node.scene().views()[0]
                if hasattr(view, 'activate_visualizer'):
                    view.activate_visualizer(node, self.current_index)
        event.accept()
    
    def setPos(self, x, y):
        """Override setPos to also move the toggle"""
        super().setPos(x, y)
        if hasattr(self, 'toggle'):
            self.toggle.setPos(
                x + self.WIDTH + 4 + VisualizerToggle.RADIUS,
                y + self.HEIGHT / 2
            )
    
    def _center_text(self):
        """Center text in the left portion of the box (leaving room for arrows)"""
        text_rect = self.text.boundingRect()
        text_area_width = self.WIDTH - self.ARROW_WIDTH
        x = (text_area_width - text_rect.width()) / 2
        y = (self.HEIGHT - text_rect.height()) / 2
        self.text.setPos(x, y)
    
    def set_index(self, index):
        """Set the current visualizer index"""
        self.current_index = max(0, min(index, self.max_index))
        self.text.setPlainText(str(self.current_index))
        self._center_text()
    
    def set_active(self, active):
        """Update visual state of the toggle"""
        self.is_active = active
        self.toggle.set_active(active)
    
    def hoverEnterEvent(self, event):
        """Highlight on hover"""
        self.setPen(QPen(QColor(150, 150, 150), 1.5))
        super().hoverEnterEvent(event)
    
    def hoverLeaveEvent(self, event):
        """Remove highlight"""
        if not self.dragging:
            self.setPen(QPen(QColor(100, 100, 100), 1))
        super().hoverLeaveEvent(event)
    
    def mousePressEvent(self, event):
        """Handle clicks - start drag or click arrows"""
        if event.button() == Qt.LeftButton:
            pos = event.pos()
            
            # Check if clicking on arrow area
            if pos.x() >= self.WIDTH - self.ARROW_WIDTH:
                # Check up vs down
                if pos.y() < self.HEIGHT / 2:
                    self.set_index(self.current_index + 1)
                else:
                    self.set_index(self.current_index - 1)
                event.accept()
            else:
                # Start drag in the text area
                self.dragging = True
                self.drag_start_y = event.scenePos().y()
                self.drag_start_index = self.current_index
                self.setPen(QPen(QColor(255, 140, 0), 2))
                self.setCursor(Qt.SizeVerCursor)
                event.accept()
        else:
            super().mousePressEvent(event)
    
    def mouseMoveEvent(self, event):
        """Handle drag to change value"""
        if self.dragging:
            delta_y = self.drag_start_y - event.scenePos().y()
            # Every 5 pixels = 1 increment
            steps = int(delta_y / 5)
            new_index = self.drag_start_index + steps
            self.set_index(new_index)
            event.accept()
        else:
            super().mouseMoveEvent(event)
    
    def mouseReleaseEvent(self, event):
        """Handle release - end drag"""
        if self.dragging:
            self.dragging = False
            self.setPen(QPen(QColor(100, 100, 100), 1))
            self.setCursor(Qt.ArrowCursor)
            event.accept()
        else:
            super().mouseReleaseEvent(event)
    
    def wheelEvent(self, event):
        """Scroll to change index"""
        delta = 1 if event.delta() > 0 else -1
        self.set_index(self.current_index + delta)
        event.accept()


class NodeWidget(QGraphicsRectItem):
    MIN_WIDTH = 180
    MIN_HEIGHT = 60
    PORT_SPACING = 28
    HEADER_HEIGHT = 32
    VISUALIZER_THRESHOLD = 3  # Use number box if more than this many visualizers

    def __init__(self, node_type, registry, name=None, port_values=None, visualizer_count=30):
        super().__init__()
        self.node_type = node_type
        self.registry = registry
        self.node_name = name or node_type.name
        self.port_values = port_values or []
        self.visualizer_count = visualizer_count
        self.input_ports = []
        self.output_ports = []
        self.visualizer_widgets = []  # Store toggle/numberbox widgets

        self.setFlag(QGraphicsItem.ItemIsMovable, True)
        self.setFlag(QGraphicsItem.ItemIsSelectable, True)
        self.setFlag(QGraphicsItem.ItemSendsGeometryChanges, True)

        self.setBrush(QBrush(HOUDINI_NODE_BG))
        self.setPen(QPen(HOUDINI_NODE_BORDER, 2))

        self.header = QGraphicsTextItem(self.node_name, self)
        self.header.setDefaultTextColor(HOUDINI_TEXT)
        font = QFont("Arial", 10, QFont.Bold)
        self.header.setFont(font)
        self.header.setPos(8, 6)

        self._create_ports()
        self._create_visualizer_widgets()
        self.adjust_size_for_expansion()

    def _create_visualizer_widgets(self):
        """Create visualizer toggle buttons or number box"""
        if self.visualizer_count == 0:
            return

        if self.visualizer_count <= self.VISUALIZER_THRESHOLD:
            # Create individual toggles
            for i in range(self.visualizer_count):
                toggle = VisualizerToggle(self, i)
                self.visualizer_widgets.append(toggle)
        else:
            # Create number box with toggle
            number_box = VisualizerNumberBox(self, self.visualizer_count - 1)
            self.visualizer_widgets.append(number_box)

        self._position_visualizer_widgets()

    def _position_visualizer_widgets(self):
        """Position visualizer widgets in the header area"""
        if not self.visualizer_widgets:
            return

        width = self.rect().width()
        y = self.HEADER_HEIGHT / 2

        if isinstance(self.visualizer_widgets[0], VisualizerToggle):
            # Multiple toggles - space them out horizontally
            spacing = 20
            total_width = len(self.visualizer_widgets) * spacing
            start_x = width - total_width - 8

            for i, toggle in enumerate(self.visualizer_widgets):
                toggle.setPos(start_x + i * spacing, y)
        else:
            # Single number box
            number_box = self.visualizer_widgets[0]
            x = width - number_box.WIDTH - VisualizerToggle.RADIUS * 2 - 12
            number_box.setPos(x, y - VisualizerNumberBox.HEIGHT / 2)

    def set_visualizer_active(self, index, active):
        """Called by view to update visualizer state"""
        if not self.visualizer_widgets:
            return

        if isinstance(self.visualizer_widgets[0], VisualizerToggle):
            if 0 <= index < len(self.visualizer_widgets):
                self.visualizer_widgets[index].set_active(active)
        else:
            # Number box
            number_box = self.visualizer_widgets[0]
            if active:
                number_box.set_index(index)
            number_box.set_active(active)

    def _create_ports(self):
        for i, (port_name, port_type) in enumerate(self.node_type.inputs):
            port = PortWidget(port_name, port_type, self.registry, True, None, self, self.node_type)
            port.setParentItem(self)
            self.input_ports.append(port)

            label = QGraphicsTextItem(port_name, self)
            label.setDefaultTextColor(HOUDINI_TEXT)
            font = QFont("Arial", 9)
            label.setFont(font)
            port.label = label

        for i, (port_name, port_type) in enumerate(self.node_type.outputs):
            port = PortWidget(port_name, port_type, self.registry, False, None, self, self.node_type)
            port.setParentItem(self)
            self.output_ports.append(port)

            label = QGraphicsTextItem(port_name, self)
            label.setDefaultTextColor(HOUDINI_TEXT)
            font = QFont("Arial", 9)
            label.setFont(font)
            port.label = label

    def change_type(self, new_node_type):
        """Change this node to a different type"""
        view = None
        if self.scene() and self.scene().views():
            view = self.scene().views()[0]

        all_connections = []
        if view:
            for conn in view.connections:
                if conn.output_node == self or conn.input_node == self:
                    all_connections.append(conn)

        for port in self.input_ports + self.output_ports:
            self._remove_port_connections(port)

        for port in self.input_ports + self.output_ports:
            self._clear_port_completely(port)
        self.input_ports.clear()
        self.output_ports.clear()

        self.node_type = new_node_type
        self.node_name = new_node_type.name
        self.header.setPlainText(self.node_name)

        self.port_values = []

        self._create_ports()
        self.adjust_size_for_expansion()

        if self.scene() and self.scene().views():
            view = self.scene().views()[0]
            if hasattr(view, '_update_node_ports_matching'):
                view._update_node_ports_matching(self)

        for conn in all_connections:
            if not conn.is_implicit:
                conn.reconnect()

    def update_ports(self, new_node_type):
        """Change this node to a different type"""
        view = None
        if self.scene() and self.scene().views():
            view = self.scene().views()[0]

        all_connections = []
        if view:
            for conn in view.connections:
                if conn.output_node == self or conn.input_node == self:
                    all_connections.append(conn)

        for port in self.input_ports + self.output_ports:
            self._remove_port_connections(port)

        for port in self.input_ports + self.output_ports:
            self._clear_port_completely(port)
        self.input_ports.clear()
        self.output_ports.clear()

        self.node_type = new_node_type

        self._create_ports()
        self.adjust_size_for_expansion()

        if self.scene() and self.scene().views():
            view = self.scene().views()[0]
            if hasattr(view, '_update_node_ports_matching'):
                view._update_node_ports_matching(self)

        for conn in all_connections:
            if not conn.is_implicit:
                conn.reconnect()

    def _remove_port_connections(self, port):
        """Remove connections from port's list"""
        for subport in port.subport_widgets[:]:
            self._remove_port_connections(subport)
        port.connections.clear()

    def _clear_port_completely(self, port):
        """Completely remove a port"""
        for subport in port.subport_widgets[:]:
            self._clear_port_completely(subport)

        if hasattr(port, 'expansion_widget') and port.expansion_widget:
            if port.expansion_widget.scene():
                port.expansion_widget.scene().removeItem(port.expansion_widget)
            port.expansion_widget = None

        if hasattr(port, 'label') and port.label:
            if port.label.scene():
                port.label.scene().removeItem(port.label)
            port.label = None

        if port.scene():
            port.scene().removeItem(port)

    def adjust_size_for_expansion(self, expanding_port=None, original_port_pos=None):
        def count_all_visible_items(ports):
            count = 0
            for port in ports:
                count += 1
                if port.expanded and port.subport_widgets:
                    count += count_nested_subports(port.subport_widgets)
            return count

        def count_nested_subports(subports):
            count = 0
            for subport in subports:
                count += 1
                if subport.expanded and subport.subport_widgets:
                    count += count_nested_subports(subport.subport_widgets)
            return count

        visible_inputs = count_all_visible_items(self.input_ports)
        visible_outputs = count_all_visible_items(self.output_ports)

        max_items = max(visible_inputs, visible_outputs)
        required_height = max_items * self.PORT_SPACING + self.HEADER_HEIGHT + 15
        height = max(self.MIN_HEIGHT, required_height)
        width = self.MIN_WIDTH

        if expanding_port and original_port_pos:
            self.setRect(0, 0, width, height)
            self._position_ports()
            self._position_visualizer_widgets()

            new_port_scene_pos = expanding_port.get_scene_pos()
            delta_y = original_port_pos.y() - new_port_scene_pos.y()

            if abs(delta_y) > 0.1:
                self.setPos(self.pos().x(), self.pos().y() + delta_y)
        else:
            self.setRect(0, 0, width, height)
            self._position_ports()
            self._position_visualizer_widgets()

        self._update_all_expansion_positions()
        self._update_all_connections()

    def _position_ports(self):
        width = self.rect().width()

        def calculate_spacing_for_port(port):
            space = self.PORT_SPACING
            if port.expanded and port.subport_widgets:
                for subport in port.subport_widgets:
                    space += calculate_spacing_for_subport(subport)
            return space

        def calculate_spacing_for_subport(subport):
            space = PortWidget.SUBPORT_SPACING
            if subport.expanded and subport.subport_widgets:
                for nested in subport.subport_widgets:
                    space += calculate_spacing_for_subport(nested)
            return space

        current_y = self.HEADER_HEIGHT
        for i, port in enumerate(self.input_ports):
            current_y += self.PORT_SPACING
            port.setPos(0, current_y)
            if hasattr(port, 'label'):
                port.label.setPos(PortWidget.PORT_RADIUS * 2 + 5, current_y - 8)

            if port.expanded and port.subport_widgets:
                for subport in port.subport_widgets:
                    subport_spacing = calculate_spacing_for_subport(subport)
                    current_y += subport_spacing

        current_y = self.HEADER_HEIGHT
        for i, port in enumerate(self.output_ports):
            current_y += self.PORT_SPACING
            port.setPos(width, current_y)
            if hasattr(port, 'label'):
                label_width = port.label.boundingRect().width()
                port.label.setPos(width - PortWidget.PORT_RADIUS * 2 - label_width - 5, current_y - 8)

            if port.expanded and port.subport_widgets:
                for subport in port.subport_widgets:
                    subport_spacing = calculate_spacing_for_subport(subport)
                    current_y += subport_spacing

    def _update_all_expansion_positions(self):
        for port in self.input_ports + self.output_ports:
            self._update_port_expansion_positions(port)

    def _update_port_expansion_positions(self, port):
        if port.expanded and port.expansion_widget:
            port.expansion_widget._update_subport_positions()
        for subport in port.subport_widgets:
            self._update_port_expansion_positions(subport)

    def _update_all_connections(self):
        for port in self.input_ports + self.output_ports:
            self._update_port_connections(port)

    def itemChange(self, change, value):
        if change == QGraphicsItem.ItemPositionHasChanged:
            for port in self.input_ports + self.output_ports:
                self._update_port_connections(port)

            if self.scene() and self.scene().views():
                view = self.scene().views()[0]
                if hasattr(view, 'connections'):
                    for conn in view.connections:
                        if (conn.output_node == self and not conn.output_valid) or \
                           (conn.input_node == self and not conn.input_valid):
                            conn.update_path()

        return super().itemChange(change, value)

    def _update_port_connections(self, port):
        try:
            for conn in port.connections[:]:
                conn.update_path()
            for subport in port.subport_widgets[:]:
                self._update_subport_connections(subport)
        except RuntimeError:
            pass

    def _update_subport_connections(self, subport):
        try:
            for conn in subport.connections[:]:
                conn.update_path()
            for nested_subport in subport.subport_widgets[:]:
                self._update_subport_connections(nested_subport)
        except RuntimeError:
            pass

    def paint(self, painter, option, widget):
        painter.setRenderHint(QPainter.Antialiasing)

        if self.isSelected():
            glow_pen = QPen(HOUDINI_TEXT.lighter(150), 6.0)
            painter.setPen(glow_pen)
            painter.setBrush(Qt.NoBrush)
            painter.drawRect(self.rect())

        self.setPen(QPen(HOUDINI_NODE_BORDER, 1.5))
        painter.setPen(self.pen())
        painter.setBrush(self.brush())
        painter.drawRect(self.rect())

    def get_all_ports(self):
        all_ports = []

        def collect_subports(subport):
            all_ports.append(subport)
            for nested_subport in subport.subport_widgets:
                collect_subports(nested_subport)

        for port in self.input_ports + self.output_ports:
            all_ports.append(port)
            for subport in port.subport_widgets:
                collect_subports(subport)

        return all_ports

    def to_dict(self):
        """Export node in Lean JSON format"""
        def port_to_lean_json(port_name, port_type_name):
            if port_type_name in self.node_type.custom_port_types:
                custom_type = self.node_type.custom_port_types[port_type_name]
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

        type_dict = {
            'name': self.node_type.name,
            'leanConstant': self.node_type.lean_constant,
            'exposeImplicitArgs': self.node_type.expose_implicit_args,
            'inputs': [port_to_lean_json(name, type_) for name, type_ in self.node_type.inputs],
            'outputs': [port_to_lean_json(name, type_) for name, type_ in self.node_type.outputs]
        }

        return {
            'name': self.node_name,
            'type': type_dict,
            'portValues': self.port_values,
            'x': self.pos().x(),
            'y': self.pos().y()
        }
