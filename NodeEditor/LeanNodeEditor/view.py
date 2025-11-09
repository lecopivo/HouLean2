"""
Node editor view - main interactive canvas
"""
from PySide6.QtWidgets import QGraphicsView, QGraphicsScene, QGraphicsPathItem, QMenu, QGraphicsTextItem
from PySide6.QtCore import Qt, QRectF, QPointF, QTimer
from PySide6.QtGui import QPainter, QPen, QBrush, QPainterPath

from .colors import *
from .connections import Connection
from .nodes import NodeWidget
from .ports import PortWidget, SubportWidget


class NodeEditorScene(QGraphicsScene):
    def __init__(self):
        super().__init__()
        self.temp_connection = None
        self.drag_start_port = None
        self.setBackgroundBrush(QBrush(HOUDINI_BG_DARK))
        self.setSceneRect(-10000, -10000, 20000, 20000)


class NodeEditorView(QGraphicsView):
    def __init__(self, registry, parent_widget=None):
        super().__init__()
        self.registry = registry
        self.parent_widget = parent_widget
        self.editor_scene = NodeEditorScene()
        self.setScene(self.editor_scene)
        
        self.setRenderHint(QPainter.Antialiasing)
        self.setRenderHint(QPainter.SmoothPixmapTransform)
        self.setDragMode(QGraphicsView.RubberBandDrag)
        self.setTransformationAnchor(QGraphicsView.AnchorUnderMouse)
        self.setViewportUpdateMode(QGraphicsView.FullViewportUpdate)
        
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        
        self.nodes = []
        self.connections = []
        self.last_mouse_pos = QPointF(0, 0)
        self.is_dragging_connection = False
        self.is_panning = False
        self.pan_start_pos = QPointF()
        self.drag_locked_ports = []
        
        self.allow_structural_matching = True
        self.show_implicit_connections = False
        
        self.editor_scene.selectionChanged.connect(self.on_selection_changed)
    
    def set_structural_matching(self, enabled):
        self.allow_structural_matching = enabled
        for node in self.nodes:
            self._update_node_ports_matching(node)
    
    def set_show_implicit(self, enabled):
        self.show_implicit_connections = enabled
        for conn in self.connections:
            if conn.is_implicit:
                conn.setVisible(enabled)
    
    def _update_node_ports_matching(self, node):
        def update_port(port):
            port.allow_structural_matching = self.allow_structural_matching
            for subport in port.subport_widgets:
                update_port(subport)
        
        for port in node.input_ports + node.output_ports:
            update_port(port)
    
    def on_selection_changed(self):
        selected = self.editor_scene.selectedItems()
        selected_nodes = [item for item in selected if isinstance(item, NodeWidget)]
        if hasattr(self, 'selection_changed_callback'):
            self.selection_changed_callback(selected_nodes)
    
    def drawBackground(self, painter, rect):
        super().drawBackground(painter, rect)
        
        grid_size = 20
        left = int(rect.left()) - (int(rect.left()) % grid_size)
        top = int(rect.top()) - (int(rect.top()) % grid_size)
        
        fine_lines = []
        thick_lines = []
        
        x = left
        while x < rect.right():
            if x % 100 == 0:
                thick_lines.append((x, rect.top(), x, rect.bottom()))
            else:
                fine_lines.append((x, rect.top(), x, rect.bottom()))
            x += grid_size
        
        y = top
        while y < rect.bottom():
            if y % 100 == 0:
                thick_lines.append((rect.left(), y, rect.right(), y))
            else:
                fine_lines.append((rect.left(), y, rect.right(), y))
            y += grid_size
        
        painter.setPen(QPen(HOUDINI_GRID_FINE, 1))
        for line in fine_lines:
            painter.drawLine(line[0], line[1], line[2], line[3])
        
        painter.setPen(QPen(HOUDINI_GRID_THICK, 1))
        for line in thick_lines:
            painter.drawLine(line[0], line[1], line[2], line[3])
    
    def _lock_port_hierarchy(self, port):
        self.drag_locked_ports.append(port)
        
        if isinstance(port, SubportWidget):
            current = port.parent_port
            while current:
                self.drag_locked_ports.append(current)
                if hasattr(current, 'parent_port'):
                    current = current.parent_port
                else:
                    break
    
    def _unlock_all_ports(self):
        self.drag_locked_ports.clear()
    
    def _is_port_locked(self, port):
        return port in self.drag_locked_ports

    def _trigger_type_check(self):
        """Notify parent widget to type check"""
        parent = self.parent()
        if self.parent_widget and hasattr(self.parent_widget, 'request_type_check'):
            print("Triggering type check!")
            self.parent_widget.request_type_check()        
        else:
            print("Can't trigger type check because I'm orphan!")
    
    def contextMenuEvent(self, event):
        item = self.itemAt(event.pos())
        if item is None or isinstance(item, QGraphicsTextItem):
            self.show_node_creation_menu(event.pos())
        else:
            super().contextMenuEvent(event)
    
    def mousePressEvent(self, event):
        pos = event.position() if hasattr(event, 'position') else event.pos()
        self.last_mouse_pos = self.mapToScene(pos.toPoint())
        
        items = self.items(pos.toPoint())
        
        if event.button() == Qt.LeftButton:
            port_item = None
            for item in items:
                if isinstance(item, (PortWidget, SubportWidget)):
                    port_item = item
                    break
            
            if port_item:
                self.setDragMode(QGraphicsView.NoDrag)
                self._lock_port_hierarchy(port_item)
                self.start_connection(port_item)
                self.is_dragging_connection = True
                event.accept()
                return
            else:
                self.setDragMode(QGraphicsView.RubberBandDrag)
        
        elif event.button() == Qt.MiddleButton:
            self.setDragMode(QGraphicsView.NoDrag)
            self.is_panning = True
            self.pan_start_pos = pos
            self.setCursor(Qt.ClosedHandCursor)
            event.accept()
            return
        
        super().mousePressEvent(event)
    
    def mouseMoveEvent(self, event):
        pos = event.position() if hasattr(event, 'position') else event.pos()
        self.last_mouse_pos = self.mapToScene(pos.toPoint())
        
        if self.is_panning:
            delta = pos - self.pan_start_pos
            self.pan_start_pos = pos
            
            self.horizontalScrollBar().setValue(
                self.horizontalScrollBar().value() - int(delta.x())
            )
            self.verticalScrollBar().setValue(
                self.verticalScrollBar().value() - int(delta.y())
            )
            event.accept()
            return
        
        if self.is_dragging_connection and self.editor_scene.temp_connection:
            try:
                if not self.editor_scene.drag_start_port:
                    self.is_dragging_connection = False
                    self.setDragMode(QGraphicsView.RubberBandDrag)
                    return
                    
                start_pos = self.editor_scene.drag_start_port.get_scene_pos()
                end_pos = self.last_mouse_pos
                
                path = QPainterPath()
                path.moveTo(start_pos)
                
                is_input_drag = self.editor_scene.drag_start_port.is_input
                ctrl_offset = abs(end_pos.x() - start_pos.x()) * 0.5
                
                if is_input_drag:
                    ctrl1 = QPointF(start_pos.x() - ctrl_offset, start_pos.y())
                    ctrl2 = QPointF(end_pos.x() + ctrl_offset, end_pos.y())
                else:
                    ctrl1 = QPointF(start_pos.x() + ctrl_offset, start_pos.y())
                    ctrl2 = QPointF(end_pos.x() - ctrl_offset, end_pos.y())
                
                path.cubicTo(ctrl1, ctrl2, end_pos)
                
                self.editor_scene.temp_connection.setPath(path)
                
                items = self.items(pos.toPoint())
                for item in items:
                    if isinstance(item, (PortWidget, SubportWidget)):
                        if item.is_input and item.is_occupied():
                            break
                        if item.subport_defs and not item.expanded:
                            item.expand()
                        break
                
                event.accept()
                return
            except RuntimeError:
                self.is_dragging_connection = False
                if self.editor_scene.temp_connection:
                    self.editor_scene.removeItem(self.editor_scene.temp_connection)
                    self.editor_scene.temp_connection = None
                self.editor_scene.drag_start_port = None
                self._unlock_all_ports()
                self.setDragMode(QGraphicsView.RubberBandDrag)
                return
        
        super().mouseMoveEvent(event)
    
    def mouseReleaseEvent(self, event):
        pos = event.position() if hasattr(event, 'position') else event.pos()
        
        if event.button() == Qt.MiddleButton:
            self.is_panning = False
            self.setDragMode(QGraphicsView.RubberBandDrag)
            self.setCursor(Qt.ArrowCursor)
            event.accept()
            return
        
        if event.button() == Qt.LeftButton and self.is_dragging_connection:
            self.is_dragging_connection = False
            
            if self.editor_scene.temp_connection:
                items = self.items(pos.toPoint())
                
                target_port = None
                for item in items:
                    if isinstance(item, (PortWidget, SubportWidget)):
                        target_port = item
                        break
                
                if target_port and self.editor_scene.drag_start_port:
                    start_port = self.editor_scene.drag_start_port
                    
                    try:
                        _ = start_port.scene()
                        _ = target_port.scene()
                        
                        if start_port.is_connectable_to(target_port):
                            output_port = start_port if not start_port.is_input else target_port
                            input_port = target_port if target_port.is_input else start_port
                            
                            conn = Connection(output_port, input_port, self.editor_scene)
                            self.editor_scene.addItem(conn)
                            self.connections.append(conn)
                            self._trigger_type_check()
                    except RuntimeError:
                        pass
                
                self.editor_scene.removeItem(self.editor_scene.temp_connection)
                self.editor_scene.temp_connection = None
                self.editor_scene.drag_start_port = None
            
            self._unlock_all_ports()
            self.setDragMode(QGraphicsView.RubberBandDrag)
            event.accept()
            return
        
        super().mouseReleaseEvent(event)
    
    def start_connection(self, port):
        self.editor_scene.drag_start_port = port
        self.editor_scene.temp_connection = QGraphicsPathItem()
        color = get_type_color(port.type_name, 
                              port.registry,
                              port.node_type if hasattr(port, 'node_type') else None)
        self.editor_scene.temp_connection.setPen(QPen(color, 2.5, Qt.DashLine))
        self.editor_scene.addItem(self.editor_scene.temp_connection)
    
    def show_node_creation_menu(self, view_pos):
        menu = QMenu(self)
        menu.setStyleSheet(f"""
            QMenu {{
                background-color: {HOUDINI_BG.name()};
                color: {HOUDINI_TEXT.name()};
                border: 1px solid {HOUDINI_NODE_BORDER.name()};
                padding: 4px;
            }}
            QMenu::item {{
                padding: 6px 25px 6px 15px;
                border-radius: 3px;
            }}
            QMenu::item:selected {{
                background-color: {HOUDINI_BG_LIGHT.name()};
            }}
            QMenu::separator {{
                height: 1px;
                background: {HOUDINI_NODE_BORDER.name()};
                margin: 4px 0px;
            }}
        """)
        
        scene_pos = self.mapToScene(view_pos)
        
        categories = {}
        for node_type_name in sorted(self.registry.node_types.keys()):
            if '_' in node_type_name:
                category = node_type_name.split('_')[0]
            else:
                category = "Other"
            
            if category not in categories:
                categories[category] = []
            categories[category].append(node_type_name)
        
        for category in sorted(categories.keys()):
            if len(categories) > 1:
                submenu = menu.addMenu(category)
                submenu.setStyleSheet(menu.styleSheet())
            else:
                submenu = menu
            
            for node_type_name in sorted(categories[category]):
                action = submenu.addAction(node_type_name)
                action.triggered.connect(lambda checked=False, name=node_type_name, pos=scene_pos: 
                                       self.add_node(name, pos))
        
        menu.exec(self.mapToGlobal(view_pos))
    
    def show_node_type_menu(self, node, view_pos):
        menu = QMenu(self)
        menu.setStyleSheet(f"""
            QMenu {{
                background-color: {HOUDINI_BG.name()};
                color: {HOUDINI_TEXT.name()};
                border: 1px solid {HOUDINI_NODE_BORDER.name()};
                padding: 4px;
            }}
            QMenu::item {{
                padding: 6px 25px 6px 15px;
                border-radius: 3px;
            }}
            QMenu::item:selected {{
                background-color: {HOUDINI_BG_LIGHT.name()};
            }}
            QMenu::separator {{
                height: 1px;
                background: {HOUDINI_NODE_BORDER.name()};
                margin: 4px 0px;
            }}
        """)
        
        categories = {}
        for node_type_name in sorted(self.registry.node_types.keys()):
            if '_' in node_type_name:
                category = node_type_name.split('_')[0]
            else:
                category = "Other"
            
            if category not in categories:
                categories[category] = []
            categories[category].append(node_type_name)
        
        for category in sorted(categories.keys()):
            if len(categories) > 1:
                submenu = menu.addMenu(category)
                submenu.setStyleSheet(menu.styleSheet())
            else:
                submenu = menu
            
            for node_type_name in sorted(categories[category]):
                node_type = self.registry.get_node_type(node_type_name)
                action = submenu.addAction(node_type_name)
                action.triggered.connect(lambda checked=False, n=node, nt=node_type: 
                                       n.change_type(nt))
        
        menu.exec(self.mapToGlobal(view_pos))
    
    def add_node(self, node_type_name, pos):
        node_type = self.registry.get_node_type(node_type_name)
            
        if node_type:
            base_name = node_type.name
            counter = 1
            name = base_name
            existing_names = {node.node_name for node in self.nodes}
            while name in existing_names:
                name = f"{base_name}_{counter}"
                counter += 1
            
            node = NodeWidget(node_type, self.registry, name)
            node.setPos(pos)
            self.editor_scene.addItem(node)
            self.nodes.append(node)
            
            self._update_node_ports_matching(node)
            self._trigger_type_check()
            
            return node
    
    def delete_selected(self):
        selected = self.editor_scene.selectedItems()
        
        for item in selected:
            if isinstance(item, NodeWidget):
                all_ports = item.get_all_ports()
                for port in all_ports:
                    for conn in port.connections[:]:
                        conn.remove()
                        if conn in self.connections:
                            self.connections.remove(conn)
                
                self.nodes.remove(item)
                self.editor_scene.removeItem(item)
            
            elif isinstance(item, Connection):
                if not item.is_implicit:
                    item.remove()
                    if item in self.connections:
                        self.connections.remove(item)

        self._trigger_type_check()                        
    
    def rename_selected(self):
        from PySide6.QtWidgets import QInputDialog, QLineEdit
        
        selected = self.editor_scene.selectedItems()
        
        for item in selected:
            if isinstance(item, NodeWidget):
                text, ok = QInputDialog.getText(self, "Rename Node", 
                                               "New name:", QLineEdit.Normal,
                                               item.node_name)
                if ok and text:
                    item.node_name = text
                    item.header.setPlainText(text)
                break
    
    def change_type_selected(self):
        selected = self.editor_scene.selectedItems()
        
        for item in selected:
            if isinstance(item, NodeWidget):
                view_pos = self.mapFromScene(item.pos())
                self.show_node_type_menu(item, view_pos)
                break
    
    def update_selected_node_type(self, type_json):
        selected = self.editor_scene.selectedItems()
        
        for item in selected:
            if isinstance(item, NodeWidget):
                try:
                    node_type = self.registry._parse_node_type(type_json)
                    if node_type:
                        item.change_type(node_type)
                except Exception as e:
                    print(f"Error updating node type: {e}")
                break
    
    def frame_all(self):
        if not self.nodes:
            return
        
        min_x = min(node.pos().x() for node in self.nodes)
        max_x = max(node.pos().x() + node.rect().width() for node in self.nodes)
        min_y = min(node.pos().y() for node in self.nodes)
        max_y = max(node.pos().y() + node.rect().height() for node in self.nodes)
        
        padding = 100
        min_x -= padding
        max_x += padding
        min_y -= padding
        max_y += padding
        
        center_x = (min_x + max_x) / 2
        center_y = (min_y + max_y) / 2
        width = max_x - min_x
        height = max_y - min_y
        
        rect = QRectF(min_x, min_y, width, height)
        self.fitInView(rect, Qt.KeepAspectRatio)
        
        if self.transform().m11() > 2.0:
            self.resetTransform()
            self.scale(1.0, 1.0)
            self.centerOn(center_x, center_y)
    
    def expand_selected_connections(self):
        selected = self.editor_scene.selectedItems()
        
        new_connections = []
        for item in selected:
            if isinstance(item, Connection) and not item.is_implicit:
                expanded = item.expand_connection()
                new_connections.extend(expanded)
                for conn in expanded:
                    if conn not in self.connections:
                        self.connections.append(conn)
                if item in self.connections:
                    self.connections.remove(item)
        
        for conn in new_connections:
            conn.setSelected(True)

    def collapse_selected_connections(self):
        selected = self.editor_scene.selectedItems()
        selected_connections = [item for item in selected if isinstance(item, Connection) and not item.is_implicit]
        
        if not selected_connections:
            return
        
        groups = {}
        
        for conn in selected_connections:
            if not conn.output_port or not conn.input_port:
                continue
            
            output_parent = self._get_parent_port(conn.output_port)
            input_parent = self._get_parent_port(conn.input_port)
            
            if not output_parent or not input_parent:
                continue
            
            key = (id(output_parent), id(input_parent), output_parent, input_parent)
            if key not in groups:
                groups[key] = []
            groups[key].append(conn)
        
        wires_to_flash = []
        new_parent_connections = []
        
        for (_, _, output_parent, input_parent), conns in groups.items():
            result = self._try_collapse_group(output_parent, input_parent, conns)
            if result == 'blocked':
                wires_to_flash.extend(conns)
            elif isinstance(result, Connection):
                new_parent_connections.append(result)
        
        for conn in new_parent_connections:
            conn.setSelected(True)
        
        if wires_to_flash:
            self._flash_wires_red(wires_to_flash)
    
    def _get_parent_port(self, port):
        if isinstance(port, SubportWidget):
            return port.parent_port
        return None
    
    def _try_collapse_group(self, output_parent, input_parent, selected_conns):
        if output_parent.connections or input_parent.connections:
            return 'blocked'
        
        all_output_subports = output_parent.subport_widgets if output_parent.expanded else []
        all_input_subports = input_parent.subport_widgets if input_parent.expanded else []
        
        if not all_output_subports or not all_input_subports:
            return 'skip'
        
        selected_output_subports = set()
        selected_input_subports = set()
        
        for conn in selected_conns:
            selected_output_subports.add(conn.output_port)
            selected_input_subports.add(conn.input_port)
        
        for out_subport in all_output_subports:
            if out_subport.connections and out_subport not in selected_output_subports:
                return 'blocked'
        
        for in_subport in all_input_subports:
            if in_subport.connections and in_subport not in selected_input_subports:
                return 'blocked'
        
        for conn in selected_conns:
            conn.remove()
            if conn in self.connections:
                self.connections.remove(conn)
        
        parent_conn = Connection(output_parent, input_parent, self.editor_scene)
        self.editor_scene.addItem(parent_conn)
        self.connections.append(parent_conn)
        
        output_parent.collapse()
        input_parent.collapse()
        
        return parent_conn
    
    def _flash_wires_red(self, wires):
        original_pens = {}
        
        for wire in wires:
            original_pens[wire] = wire.pen()
            wire.setPen(QPen(HOUDINI_ERROR, 3.5))
            wire.update()
        
        def restore_pens():
            for wire in wires:
                if wire in original_pens and wire.scene():
                    wire.setPen(original_pens[wire])
                    wire.update()
        
        QTimer.singleShot(300, restore_pens)
    
    def keyPressEvent(self, event):
        if event.key() == Qt.Key_Delete or event.key() == Qt.Key_Backspace:
            self.delete_selected()
        elif event.key() == Qt.Key_F2:
            self.rename_selected()
        elif event.key() == Qt.Key_M:
            self.change_type_selected()
        elif event.key() == Qt.Key_Tab:
            view_pos = self.mapFromScene(self.last_mouse_pos)
            self.show_node_creation_menu(view_pos)
        elif event.key() == Qt.Key_G:
            self.frame_all()
        elif event.key() == Qt.Key_E:
            self.expand_selected_connections()
        elif event.key() == Qt.Key_C:
            self.collapse_selected_connections()
        else:
            super().keyPressEvent(event)
    
    def wheelEvent(self, event):
        zoom_factor = 1.12
        if event.angleDelta().y() > 0:
            self.scale(zoom_factor, zoom_factor)
        else:
            self.scale(1 / zoom_factor, 1 / zoom_factor)
