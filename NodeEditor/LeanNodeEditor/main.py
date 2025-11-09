"""
Houdini Node Editor Python Pane with Lean 4 JSON Format
Main widget and entry point

To use: In Houdini, create a new Python Panel and paste:
    from LeanNodeEditor.main import createInterface
"""

import json
import hou
from PySide6.QtWidgets import (QWidget, QVBoxLayout, QHBoxLayout, QPushButton, 
                               QFileDialog, QLabel, QFrame, QSplitter, QCheckBox,
                               QScrollArea, QGridLayout, QLineEdit)
from PySide6.QtCore import Qt, Signal, QObject
from PySide6.QtGui import QFont

from .colors import *
from .node_types import NodeTypeRegistry
from .view import NodeEditorView
from .serialization import save_to_json, load_from_json
from .async_type_checker import AsyncTypeChecker
from .server_manager import ServerManager


def get_default_types():
    """Get default node and port type configuration"""
    return {
        "portTypes": [
            {"builtin": {"name": "type", "type": "Type"}},
            {"builtin": {"name": "value", "type": "Float"}},
            {"builtin": {"name": "value", "type": "Int"}},
            {"builtin": {"name": "value", "type": "String"}},
            {"builtin": {"name": "wildcard", "type": "?_"}},
            {"struct": {
                "name": "v",
                "type": "Vector3",
                "subports": [
                    {"builtin": {"name": "x", "type": "Float"}},
                    {"builtin": {"name": "y", "type": "Float"}},
                    {"builtin": {"name": "z", "type": "Float"}}
                ]
            }},
            {"struct": {
                "name": "particle",
                "type": "Particle",
                "subports": [
                    {"builtin": {"name": "position", "type": "Vector3"}},
                    {"builtin": {"name": "velocity", "type": "Vector3"}}
                ]
            }}
        ],
        "nodeTypes": [
            {
                "name": "Float_Add",
                "leanConstant": "Float.add",
                "exposeImplicitArgs": False,
                "inputs": [
                    {"builtin": {"name": "a", "type": "Float"}},
                    {"builtin": {"name": "b", "type": "Float"}}
                ],
                "outputs": [
                    {"builtin": {"name": "add", "type": "Float"}}
                ]
            },
            {
                "name": "add",
                "leanConstant": "HAdd.hAdd",
                "exposeImplicitArgs": False,
                "inputs": [
                    {"builtin": {"name": "a", "type": "?_"}},
                    {"builtin": {"name": "b", "type": "?_"}}
                ],
                "outputs": [
                    {"builtin": {"name": "add", "type": "?_"}}
                ]
            },
            {
                "name": "makeVector3",
                "leanConstant": "Vector3.mk",
                "exposeImplicitArgs": False,
                "inputs": [
                    {"builtin": {"name": "x", "type": "Float"}},
                    {"builtin": {"name": "y", "type": "Float"}},
                    {"builtin": {"name": "z", "type": "Float"}}
                ],
                "outputs": [
                    {"builtin": {"name": "vector", "type": "Vector3"}}
                ]
            }
        ]
    }


class NodeEditorWidget(QWidget):
    type_check_response_signal = Signal(object)
    
    def __init__(self):
        super().__init__()
        self.registry = NodeTypeRegistry()
        self.registry.load_from_json(get_default_types())

        # Type checker setup
        self.server_manager = None
        self.type_checker = None
        self.pending_type_check = False
        self.type_check_timer = None
        self.last_graph_state = None
        
        # Connect signal to slot
        self.type_check_response_signal.connect(self._apply_type_check_results)        
        
        self._create_ui()
        self.current_selected_node = None

        # Initialize type checker on startup
        self._initialize_type_checker()

    
    def _create_ui(self):
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)
        
        toolbar_frame = self._create_toolbar()
        layout.addWidget(toolbar_frame)
        
        splitter = QSplitter(Qt.Horizontal)
        splitter.setStyleSheet(f"""
            QSplitter::handle {{
                background-color: {HOUDINI_NODE_BORDER.name()};
                width: 2px;
            }}
        """)
        
        editor_panel = self._create_editor_panel()
        splitter.addWidget(editor_panel)
        
        self.editor = NodeEditorView(self.registry, parent_widget=self)
        self.editor.selection_changed_callback = self.on_node_selection_changed
        splitter.addWidget(self.editor)
        
        splitter.setSizes([250, 800])
        
        layout.addWidget(splitter)
        
        self.editor.centerOn(0, 0)
    
    def _create_toolbar(self):
        toolbar_frame = QFrame()
        toolbar_frame.setStyleSheet(f"""
            QFrame {{
                background-color: {HOUDINI_BG.name()};
                border-bottom: 1px solid {HOUDINI_NODE_BORDER.name()};
            }}
            QPushButton {{
                background-color: {HOUDINI_BG_LIGHT.name()};
                color: {HOUDINI_TEXT.name()};
                border: 1px solid {HOUDINI_NODE_BORDER.name()};
                border-radius: 3px;
                padding: 4px 12px;
                font-size: 11px;
            }}
            QPushButton:hover {{
                background-color: {HOUDINI_BG_LIGHT.lighter(110).name()};
            }}
            QPushButton:pressed {{
                background-color: {HOUDINI_BG.name()};
            }}
            QLabel {{
                color: {HOUDINI_TEXT_DIM.name()};
                font-size: 10px;
                padding: 0 8px;
            }}
            QCheckBox {{
                color: {HOUDINI_TEXT.name()};
                font-size: 10px;
                padding: 0 8px;
            }}
            QCheckBox::indicator {{
                width: 14px;
                height: 14px;
                border: 1px solid {HOUDINI_NODE_BORDER.name()};
                border-radius: 3px;
                background-color: {HOUDINI_BG_LIGHT.name()};
            }}
            QCheckBox::indicator:checked {{
                background-color: {get_type_color('Float').name()};
            }}
        """)
        
        toolbar_layout = QHBoxLayout(toolbar_frame)
        toolbar_layout.setContentsMargins(4, 4, 4, 4)
        
        save_btn = QPushButton("Save")
        save_btn.clicked.connect(self.save_graph)
        toolbar_layout.addWidget(save_btn)
        
        load_btn = QPushButton("Load")
        load_btn.clicked.connect(self.load_graph)
        toolbar_layout.addWidget(load_btn)
        
        toolbar_layout.addSpacing(8)
        
        save_to_node_btn = QPushButton("Save to Node")
        save_to_node_btn.clicked.connect(self.save_to_node)
        toolbar_layout.addWidget(save_to_node_btn)
        
        load_from_node_btn = QPushButton("Load from Node")
        load_from_node_btn.clicked.connect(self.load_from_node)
        toolbar_layout.addWidget(load_from_node_btn)
        
        toolbar_layout.addSpacing(8)
        
        load_types_btn = QPushButton("Load Types")
        load_types_btn.clicked.connect(self.load_node_types)
        toolbar_layout.addWidget(load_types_btn)
        
        toolbar_layout.addSpacing(8)
        
        self.structural_matching_cb = QCheckBox("Structural Type Matching")
        self.structural_matching_cb.setChecked(True)
        self.structural_matching_cb.toggled.connect(self.on_structural_matching_changed)
        toolbar_layout.addWidget(self.structural_matching_cb)
        
        toolbar_layout.addSpacing(8)
        
        self.show_implicit_cb = QCheckBox("Show Implicit Connections")
        self.show_implicit_cb.setChecked(False)
        self.show_implicit_cb.toggled.connect(self.on_show_implicit_changed)
        toolbar_layout.addWidget(self.show_implicit_cb)
        
        toolbar_layout.addStretch()
        
        instructions = QLabel("Drag: select | RMB/Tab: add | M: change type | E: expand | C: collapse | Del: remove | F2: rename | G: frame")
        toolbar_layout.addWidget(instructions)
        
        return toolbar_frame
    
    def _create_editor_panel(self):
        panel = QFrame()
        panel.setStyleSheet(f"""
            QFrame {{
                background-color: {HOUDINI_BG_DARK.name()};
                border-right: 1px solid {HOUDINI_NODE_BORDER.name()};
            }}
            QLineEdit {{
                background-color: {HOUDINI_BG.name()};
                color: {HOUDINI_TEXT.name()};
                border: 1px solid {HOUDINI_NODE_BORDER.name()};
                border-radius: 4px;
                padding: 8px 12px;
                font-size: 13px;
                font-family: 'Segoe UI', 'San Francisco', system-ui, sans-serif;
            }}
            QLineEdit:focus {{
                border: 1px solid {HOUDINI_NODE_SELECTED.name()};
                background-color: {HOUDINI_BG_LIGHT.name()};
            }}
            QLineEdit:disabled {{
                background-color: {HOUDINI_BG_DARK.darker(110).name()};
                color: {HOUDINI_TEXT_DIM.name()};
                border: 1px solid {HOUDINI_NODE_BORDER.darker(110).name()};
            }}
            QLabel {{
                color: {HOUDINI_TEXT.name()};
                font-size: 13px;
                font-weight: 500;
                padding: 6px 4px 4px 4px;
                font-family: 'Segoe UI', 'San Francisco', system-ui, sans-serif;
            }}
            QPushButton {{
                background-color: {HOUDINI_BG_LIGHT.name()};
                color: {HOUDINI_TEXT.name()};
                border: 1px solid {HOUDINI_NODE_BORDER.name()};
                border-radius: 4px;
                padding: 8px 16px;
                font-size: 13px;
                font-weight: 500;
                font-family: 'Segoe UI', 'San Francisco', system-ui, sans-serif;
            }}
            QPushButton:hover {{
                background-color: {HOUDINI_BG_LIGHT.lighter(115).name()};
                border: 1px solid {HOUDINI_NODE_BORDER.lighter(120).name()};
            }}
            QPushButton:pressed {{
                background-color: {HOUDINI_BG.name()};
            }}
            QPushButton:disabled {{
                background-color: {HOUDINI_BG_DARK.name()};
                color: {HOUDINI_TEXT_DIM.name()};
                border: 1px solid {HOUDINI_NODE_BORDER.darker(110).name()};
            }}
        """)
        
        panel_layout = QVBoxLayout(panel)
        panel_layout.setContentsMargins(0, 0, 0, 0)
        panel_layout.setSpacing(0)
        
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setStyleSheet(f"""
            QScrollArea {{
                border: none;
                background-color: {HOUDINI_BG_DARK.name()};
            }}
            QScrollBar:vertical {{
                background-color: {HOUDINI_BG_DARK.name()};
                width: 12px;
                border-radius: 6px;
            }}
            QScrollBar::handle:vertical {{
                background-color: {HOUDINI_BG_LIGHT.name()};
                border-radius: 6px;
                min-height: 30px;
            }}
            QScrollBar::handle:vertical:hover {{
                background-color: {HOUDINI_BG_LIGHT.lighter(120).name()};
            }}
            QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical {{
                height: 0px;
            }}
        """)
        
        content = QWidget()
        content_layout = QVBoxLayout(content)
        content_layout.setContentsMargins(12, 12, 12, 12)
        content_layout.setSpacing(6)
        
        # Header - compact
        title = QLabel("Node Properties")
        title.setStyleSheet(f"""
            font-weight: 600; 
            font-size: 14px; 
            color: {HOUDINI_NODE_SELECTED.name()};
            padding: 6px 0px;
            border-bottom: 2px solid {HOUDINI_NODE_SELECTED.darker(150).name()};
        """)
        content_layout.addWidget(title)
        
        # Node properties as single-line form fields
        props_layout = QVBoxLayout()
        props_layout.setSpacing(4)
        
        # Node Name
        name_row = QHBoxLayout()
        name_row.setSpacing(8)
        name_label = QLabel("Node Name:")
        name_label.setStyleSheet(f"font-size: 12px; color: {HOUDINI_TEXT.name()}; min-width: 90px;")
        self.name_edit = QLineEdit()
        self.name_edit.setPlaceholderText("Enter name...")
        name_row.addWidget(name_label)
        name_row.addWidget(self.name_edit)
        props_layout.addLayout(name_row)
        
        # Lean Constant
        lean_row = QHBoxLayout()
        lean_row.setSpacing(8)
        lean_label = QLabel("Lean Constant:")
        lean_label.setStyleSheet(f"font-size: 12px; color: {HOUDINI_TEXT.name()}; min-width: 90px;")
        self.lean_edit = QLineEdit()
        self.lean_edit.setPlaceholderText("Module.function")
        lean_row.addWidget(lean_label)
        lean_row.addWidget(self.lean_edit)
        props_layout.addLayout(lean_row)
        
        # Type Name
        type_row = QHBoxLayout()
        type_row.setSpacing(8)
        type_label = QLabel("Type Name:")
        type_label.setStyleSheet(f"font-size: 12px; color: {HOUDINI_TEXT.name()}; min-width: 90px;")
        self.type_name_edit = QLineEdit()
        self.type_name_edit.setPlaceholderText("TypeName")
        type_row.addWidget(type_label)
        type_row.addWidget(self.type_name_edit)
        props_layout.addLayout(type_row)
        
        content_layout.addLayout(props_layout)
        
        # Separator
        separator = QFrame()
        separator.setFrameShape(QFrame.HLine)
        separator.setStyleSheet(f"background-color: {HOUDINI_NODE_BORDER.name()}; max-height: 1px; margin: 8px 0px;")
        content_layout.addWidget(separator)
        
        # Input Ports Section
        inputs_header = QLabel("Input Ports")
        inputs_header.setStyleSheet(f"""
            font-weight: 600; 
            font-size: 13px; 
            padding: 4px 0px;
            color: {get_type_color('Vector3').name()};
        """)
        content_layout.addWidget(inputs_header)
        
        # Input ports header row with column labels and buttons
        inputs_controls = QHBoxLayout()
        inputs_controls.setSpacing(8)
        
        port_name_label = QLabel("Name")
        port_name_label.setStyleSheet(f"font-size: 11px; color: {HOUDINI_TEXT_DIM.name()}; min-width: 80px;")
        inputs_controls.addWidget(port_name_label)
        
        port_type_label = QLabel("Type")
        port_type_label.setStyleSheet(f"font-size: 11px; color: {HOUDINI_TEXT_DIM.name()}; min-width: 80px;")
        inputs_controls.addWidget(port_type_label)
        
        port_value_label = QLabel("Value")
        port_value_label.setStyleSheet(f"font-size: 11px; color: {HOUDINI_TEXT_DIM.name()}; min-width: 80px;")
        inputs_controls.addWidget(port_value_label)
        
        inputs_controls.addStretch()
        
        self.add_input_btn = QPushButton("+")
        self.add_input_btn.setMaximumWidth(28)
        self.add_input_btn.setMaximumHeight(28)
        self.add_input_btn.clicked.connect(self.add_input_port)
        self.add_input_btn.setStyleSheet(f"""
            QPushButton {{
                background-color: {get_type_color('Vector3').darker(140).name()};
                color: {HOUDINI_TEXT.name()};
                border: none;
                border-radius: 3px;
                font-size: 16px;
                font-weight: bold;
            }}
            QPushButton:hover {{
                background-color: {get_type_color('Vector3').darker(120).name()};
            }}
        """)
        inputs_controls.addWidget(self.add_input_btn)
        
        self.remove_input_btn = QPushButton("−")
        self.remove_input_btn.setMaximumWidth(28)
        self.remove_input_btn.setMaximumHeight(28)
        self.remove_input_btn.clicked.connect(self.remove_input_port)
        self.remove_input_btn.setStyleSheet(f"""
            QPushButton {{
                background-color: {HOUDINI_BG_LIGHT.name()};
                color: {HOUDINI_TEXT.name()};
                border: 1px solid {HOUDINI_NODE_BORDER.name()};
                border-radius: 3px;
                font-size: 18px;
            }}
            QPushButton:hover {{
                background-color: {HOUDINI_ERROR.darker(120).name()};
                color: {HOUDINI_TEXT.name()};
                border: 1px solid {HOUDINI_ERROR.name()};
            }}
        """)
        inputs_controls.addWidget(self.remove_input_btn)
        
        content_layout.addLayout(inputs_controls)
        
        self.inputs_layout = QVBoxLayout()
        self.inputs_layout.setSpacing(2)
        content_layout.addLayout(self.inputs_layout)
        
        # Output Ports Section
        outputs_header = QLabel("Output Ports")
        outputs_header.setStyleSheet(f"""
            font-weight: 600; 
            font-size: 13px; 
            padding: 4px 0px;
            color: {get_type_color('Particle').name()};
        """)
        content_layout.addWidget(outputs_header)
        
        # Output ports header row with column labels and buttons
        outputs_controls = QHBoxLayout()
        outputs_controls.setSpacing(8)
        
        port_name_label2 = QLabel("Name")
        port_name_label2.setStyleSheet(f"font-size: 11px; color: {HOUDINI_TEXT_DIM.name()}; min-width: 120px;")
        outputs_controls.addWidget(port_name_label2)
        
        port_type_label2 = QLabel("Type")
        port_type_label2.setStyleSheet(f"font-size: 11px; color: {HOUDINI_TEXT_DIM.name()}; min-width: 120px;")
        outputs_controls.addWidget(port_type_label2)
        
        outputs_controls.addStretch()
        
        self.add_output_btn = QPushButton("+")
        self.add_output_btn.setMaximumWidth(28)
        self.add_output_btn.setMaximumHeight(28)
        self.add_output_btn.clicked.connect(self.add_output_port)
        self.add_output_btn.setStyleSheet(f"""
            QPushButton {{
                background-color: {get_type_color('Particle').darker(140).name()};
                color: {HOUDINI_TEXT.name()};
                border: none;
                border-radius: 3px;
                font-size: 16px;
                font-weight: bold;
            }}
            QPushButton:hover {{
                background-color: {get_type_color('Particle').darker(120).name()};
            }}
        """)
        outputs_controls.addWidget(self.add_output_btn)
        
        self.remove_output_btn = QPushButton("−")
        self.remove_output_btn.setMaximumWidth(28)
        self.remove_output_btn.setMaximumHeight(28)
        self.remove_output_btn.clicked.connect(self.remove_output_port)
        self.remove_output_btn.setStyleSheet(f"""
            QPushButton {{
                background-color: {HOUDINI_BG_LIGHT.name()};
                color: {HOUDINI_TEXT.name()};
                border: 1px solid {HOUDINI_NODE_BORDER.name()};
                border-radius: 3px;
                font-size: 18px;
            }}
            QPushButton:hover {{
                background-color: {HOUDINI_ERROR.darker(120).name()};
                color: {HOUDINI_TEXT.name()};
                border: 1px solid {HOUDINI_ERROR.name()};
            }}
        """)
        outputs_controls.addWidget(self.remove_output_btn)
        
        content_layout.addLayout(outputs_controls)
        
        self.outputs_layout = QVBoxLayout()
        self.outputs_layout.setSpacing(2)
        content_layout.addLayout(self.outputs_layout)
        
        content_layout.addStretch()
        
        # Apply Button - smaller and more compact
        self.update_btn = QPushButton("Apply Changes")
        self.update_btn.setEnabled(False)
        self.update_btn.clicked.connect(self.apply_node_changes)
        self.update_btn.setStyleSheet(f"""
            QPushButton {{
                background-color: {HOUDINI_NODE_SELECTED.name()};
                color: {HOUDINI_TEXT.name()};
                border: none;
                border-radius: 4px;
                padding: 8px 20px;
                font-size: 13px;
                font-weight: 600;
            }}
            QPushButton:hover {{
                background-color: {HOUDINI_NODE_SELECTED.lighter(110).name()};
            }}
            QPushButton:pressed {{
                background-color: {HOUDINI_NODE_SELECTED.darker(110).name()};
            }}
            QPushButton:disabled {{
                background-color: {HOUDINI_BG.name()};
                color: {HOUDINI_TEXT_DIM.name()};
                border: 1px solid {HOUDINI_NODE_BORDER.name()};
            }}
        """)
        content_layout.addWidget(self.update_btn)
        
        # Status Label
        self.status_label = QLabel("")
        self.status_label.setStyleSheet(f"""
            color: {HOUDINI_TEXT_DIM.name()}; 
            font-size: 11px; 
            padding: 6px;
            font-style: italic;
        """)
        self.status_label.setWordWrap(True)
        content_layout.addWidget(self.status_label)
        
        scroll.setWidget(content)
        panel_layout.addWidget(scroll)
        
        self.input_port_widgets = []
        self.output_port_widgets = []
        
        return panel
    
    def _create_property_section(self, title, description):
        """Create a styled property section container"""
        section = QFrame()
        section.setStyleSheet(f"""
            QFrame {{
                background-color: {HOUDINI_BG.name()};
                border: 1px solid {HOUDINI_NODE_BORDER.name()};
                border-radius: 6px;
                padding: 12px;
            }}
        """)
        
        layout = QVBoxLayout(section)
        layout.setContentsMargins(12, 12, 12, 12)
        layout.setSpacing(8)
        
        title_label = QLabel(title)
        title_label.setStyleSheet(f"""
            font-weight: 600;
            font-size: 13px;
            color: {HOUDINI_TEXT.name()};
            padding: 0px;
        """)
        layout.addWidget(title_label)
        
        desc_label = QLabel(description)
        desc_label.setStyleSheet(f"""
            font-size: 11px;
            color: {HOUDINI_TEXT_DIM.name()};
            padding: 0px 0px 4px 0px;
            font-style: italic;
        """)
        layout.addWidget(desc_label)
        
        return section

    
    # Network Type Checking

    def _initialize_type_checker(self):
        """Initialize and start the type checker server."""
        try:
            # TODO: Update this path to your actual Lean server executable
            # For example: "/home/tskrivan/Documents/HouLean/.lake/build/bin/houlean-server"
            server_path = "/home/tskrivan/Documents/HouLean/.lake/build/bin/houlean"

            self.server_manager = ServerManager(server_path)

            if self.server_manager.start():
                # Pass the server process to the type checker
                self.type_checker = AsyncTypeChecker(self.server_manager.process)
                self.type_checker.start()
                print("Type checker initialized successfully")
                self.status_label.setText("Type checker ready")
            else:
                print("Failed to start type checker server")
                self.status_label.setText("Type checker unavailable")
                self.server_manager = None

        except Exception as e:
            print("Error initializing type checker: {}".format(e))
            import traceback
            traceback.print_exc()
            self.status_label.setText("Type checker error: {}".format(e))
            self.server_manager = None
            self.type_checker = None

    def request_type_check(self):
        """Request a type check with debouncing to avoid excessive checks."""
        if not self.type_checker:
            return

        # Cancel existing timer if any
        if self.type_check_timer:
            try:
                self.type_check_timer.cancel()
            except:
                pass

        # Schedule type check after 500ms of inactivity
        import threading
        self.type_check_timer = threading.Timer(0.1, self._perform_type_check)
        self.type_check_timer.start()            

        
    def _perform_type_check(self):
        """Perform the actual type check."""
        if self.pending_type_check:
            # Already have a type check in progress, skip
            return

        try:
            # Serialize current graph state
            # For now we dump everything
            graph_data = save_to_json(self.editor)

            # Check if graph actually changed
            if graph_data == self.last_graph_state:
                return

            self.last_graph_state = graph_data
            self.pending_type_check = True

            # Submit async request
            self.type_checker.check_types(
                graph_data,
                self._handle_type_check_response
            )

        except Exception as e:
            print("Error performing type check: {}".format(e))
            self.pending_type_check = False

            
    def _handle_type_check_response(self, response):
        """Handle type check response from server (called from worker thread)."""
        self.pending_type_check = False

        print(f"handling response {response}")

        if response is None:
            print("Type check failed")
            self.status_label.setText("Type check failed - server error")
            return

        print(f"handling response 2")

        # Emit signal - this is thread-safe!
        self.type_check_response_signal.emit(response)

    def _apply_type_check_results(self, response):
        """Apply type check results to the UI (must be called on main thread)."""

        print(f"applying response {response}")

        if response is None:
            self.status_label.setText("Type check failed - server error")
            return

        # just reload graph from it self, does that cause problems?
        data = save_to_json(self.editor)
        load_from_json(self.editor, data)

        
        if response.get("status") != "success":
            error_msg = response.get("result", "Unknown error")
            print("Type check error: {}".format(error_msg))
            self.status_label.setText("Type check error: {}".format(error_msg))
            return

        self.status_label.setText("Type check completed!")
        
    # Node Network Manipulation        
    
    def on_node_selection_changed(self, selected_nodes):
        if selected_nodes:
            self.current_selected_node = selected_nodes[0]
            self.load_node_definition()
            self.update_btn.setEnabled(True)
            self.status_label.setText(f"Editing: {self.current_selected_node.node_name}")
        else:
            self.current_selected_node = None
            self.clear_editor()
            self.update_btn.setEnabled(False)
            self.status_label.setText("")
    
    def clear_editor(self):
        self.name_edit.clear()
        self.lean_edit.clear()
        self.type_name_edit.clear()
        
        for widget_group in self.input_port_widgets:
            for widget in widget_group.values():
                widget.deleteLater()
        self.input_port_widgets.clear()
        
        for widget_group in self.output_port_widgets:
            for widget in widget_group.values():
                widget.deleteLater()
        self.output_port_widgets.clear()
    
    def load_node_definition(self):
        if not self.current_selected_node:
            return
        
        node = self.current_selected_node
        
        self.clear_editor()
        
        self.name_edit.setText(node.node_name)
        self.lean_edit.setText(node.node_type.lean_constant)
        self.type_name_edit.setText(node.node_type.name)
        
        for i, (port_name, port_type) in enumerate(node.node_type.inputs):
            port_value = ""
            if i < len(node.port_values):
                port_value = node.port_values[i]
            
            is_occupied = False
            if i < len(node.input_ports):
                is_occupied = node.input_ports[i].is_occupied()
            
            self.add_input_port_widget(port_name, port_type, port_value, is_occupied)
        
        for port_name, port_type in node.node_type.outputs:
            self.add_output_port_widget(port_name, port_type)
    
    def add_input_port_widget(self, name="", type_name="", value="", is_occupied=False):
        # Single row with three input fields
        row = QHBoxLayout()
        row.setSpacing(8)
        
        name_edit = QLineEdit(name)
        name_edit.setPlaceholderText("name")
        name_edit.setMinimumWidth(80)
        row.addWidget(name_edit)
        
        type_edit = QLineEdit(type_name)
        type_edit.setPlaceholderText("type")
        type_edit.setMinimumWidth(80)
        row.addWidget(type_edit)
        
        value_edit = QLineEdit(value)
        value_edit.setPlaceholderText("value" if not is_occupied else "connected")
        value_edit.setEnabled(not is_occupied)
        value_edit.setMinimumWidth(80)
        row.addWidget(value_edit)
        
        # Container to hold the row
        container = QWidget()
        container.setLayout(row)
        
        self.inputs_layout.addWidget(container)
        
        self.input_port_widgets.append({
            'container': container,
            'name': name_edit,
            'type': type_edit,
            'value': value_edit
        })
    
    def add_output_port_widget(self, name="", type_name=""):
        # Single row with two input fields
        row = QHBoxLayout()
        row.setSpacing(8)
        
        name_edit = QLineEdit(name)
        name_edit.setPlaceholderText("name")
        name_edit.setMinimumWidth(120)
        row.addWidget(name_edit)
        
        type_edit = QLineEdit(type_name)
        type_edit.setPlaceholderText("type")
        type_edit.setMinimumWidth(120)
        row.addWidget(type_edit)
        
        row.addStretch()
        
        # Container to hold the row
        container = QWidget()
        container.setLayout(row)
        
        self.outputs_layout.addWidget(container)
        
        self.output_port_widgets.append({
            'container': container,
            'name': name_edit,
            'type': type_edit
        })
    
    def add_input_port(self):
        self.add_input_port_widget()
    
    def remove_input_port(self):
        if self.input_port_widgets:
            widget_group = self.input_port_widgets.pop()
            for widget in widget_group.values():
                widget.deleteLater()
    
    def add_output_port(self):
        self.add_output_port_widget()
    
    def remove_output_port(self):
        if self.output_port_widgets:
            widget_group = self.output_port_widgets.pop()
            for widget in widget_group.values():
                widget.deleteLater()
    
    def apply_node_changes(self):
        if not self.current_selected_node:
            return
        
        try:
            node = self.current_selected_node
            
            node_name = self.name_edit.text().strip()
            lean_constant = self.lean_edit.text().strip()
            type_name = self.type_name_edit.text().strip()
            
            if not node_name or not type_name:
                self.status_label.setText("Error: Node name and type name required")
                self.status_label.setStyleSheet(f"color: {HOUDINI_ERROR.name()}; font-size: 12px; padding: 8px;")
                return
            
            inputs = []
            port_values = []
            for widget_group in self.input_port_widgets:
                port_name = widget_group['name'].text().strip()
                port_type = widget_group['type'].text().strip()
                port_value = widget_group['value'].text().strip()
                
                if not port_name or not port_type:
                    self.status_label.setText("Error: All ports must have name and type")
                    self.status_label.setStyleSheet(f"color: {HOUDINI_ERROR.name()}; font-size: 12px; padding: 8px;")
                    return
                
                inputs.append({'builtin': {'name': port_name, 'type': port_type}})
                port_values.append(port_value)
            
            outputs = []
            for widget_group in self.output_port_widgets:
                port_name = widget_group['name'].text().strip()
                port_type = widget_group['type'].text().strip()
                
                if not port_name or not port_type:
                    self.status_label.setText("Error: All ports must have name and type")
                    self.status_label.setStyleSheet(f"color: {HOUDINI_ERROR.name()}; font-size: 12px; padding: 8px;")
                    return
                
                outputs.append({'builtin': {'name': port_name, 'type': port_type}})
            
            definition = {
                'name': type_name,
                'leanConstant': lean_constant,
                'exposeImplicitArgs': node.node_type.expose_implicit_args,
                'inputs': inputs,
                'outputs': outputs
            }
            
            self.editor.update_selected_node_type(definition)
            
            node.node_name = node_name
            node.header.setPlainText(node_name)
            node.port_values = port_values
            
            success_color = get_type_color('Float')
            self.status_label.setText("✓ Changes applied successfully")
            self.status_label.setStyleSheet(f"color: {success_color.name()}; font-size: 12px; padding: 8px;")
            
            self.load_node_definition()
            
        except Exception as e:
            self.status_label.setText(f"Error: {str(e)}")
            self.status_label.setStyleSheet(f"color: {HOUDINI_ERROR.name()}; font-size: 12px; padding: 8px;")
    
    def on_structural_matching_changed(self, checked):
        self.editor.set_structural_matching(checked)
        status_text = "Structural type matching enabled" if checked else "Strict type name matching enabled"
        self.status_label.setText(status_text)
        self.status_label.setStyleSheet(f"color: {HOUDINI_TEXT_DIM.name()}; font-size: 12px; padding: 8px;")
    
    def on_show_implicit_changed(self, checked):
        self.editor.set_show_implicit(checked)
        status_text = "Showing implicit connections" if checked else "Hiding implicit connections"
        self.status_label.setText(status_text)
        self.status_label.setStyleSheet(f"color: {HOUDINI_TEXT_DIM.name()}; font-size: 12px; padding: 8px;")
    
    def save_graph(self):
        filename, _ = QFileDialog.getSaveFileName(self, "Save Graph", "", "JSON Files (*.json)")
        if filename:
            data = save_to_json(self.editor)
            with open(filename, 'w') as f:
                json.dump(data, f, indent=2)
    
    def load_graph(self):
        filename, _ = QFileDialog.getOpenFileName(self, "Load Graph", "", "JSON Files (*.json)")
        if filename:
            with open(filename, 'r') as f:
                data = json.load(f)
            load_from_json(self.editor, data)
    
    def load_node_types(self):
        filename, _ = QFileDialog.getOpenFileName(self, "Load Node Types", "", "JSON Files (*.json)")
        if filename:
            with open(filename, 'r') as f:
                data = json.load(f)
            self.registry.load_from_json(data)
    
    def save_to_node(self):
        selected = hou.selectedNodes()
        if not selected:
            hou.ui.displayMessage("Please select a node first", severity=hou.severityType.Warning)
            return
        
        node = selected[0]
        
        if not node.parm('network'):
            parm_template = hou.StringParmTemplate('network', 'Network', 1, 
                                                   string_type=hou.stringParmType.Regular)
            parm_group = node.parmTemplateGroup()
            parm_group.append(parm_template)
            node.setParmTemplateGroup(parm_group)
        
        data = save_to_json(self.editor)
        json_str = json.dumps(data, indent=2)
        node.parm('network').set(json_str)
        
        hou.ui.displayMessage(f"Network saved to node: {node.path()}", severity=hou.severityType.Message)
    
    def load_from_node(self):
        selected = hou.selectedNodes()
        if not selected:
            hou.ui.displayMessage("Please select a node first", severity=hou.severityType.Warning)
            return
        
        node = selected[0]
        
        if not node.parm('network'):
            hou.ui.displayMessage("Selected node has no 'network' parameter", severity=hou.severityType.Warning)
            return
        
        json_str = node.parm('network').eval()
        if not json_str:
            hou.ui.displayMessage("Network parameter is empty", severity=hou.severityType.Warning)
            return
        
        try:
            data = json.loads(json_str)
            load_from_json(self.editor, data)
            hou.ui.displayMessage(f"Network loaded from node: {node.path()}", severity=hou.severityType.Message)
        except json.JSONDecodeError as e:
            hou.ui.displayMessage(f"Failed to parse network data: {e}", severity=hou.severityType.Error)


def createInterface():
    """Entry point for Houdini Python Panel"""
    return NodeEditorWidget()
