from PySide6 import QtWidgets, QtGui, QtCore
import json
import subprocess

def line_col_to_offset(text, line, col):
    """Convert line+column (1-indexed) to absolute offset (0-indexed)"""
    lines = text.split('\n')
    offset = 0
    
    # Add lengths of all previous lines (including newlines)
    for i in range(line - 1):
        offset += len(lines[i]) + 1  # +1 for the newline character
    
    # Add the column offset (col is 1-indexed)
    offset += col - 1
    
    return offset

def compute_start_length(text, start_line, start_col, end_line, end_col):
    """Compute start offset and length from line+column coordinates"""
    start = line_col_to_offset(text, start_line, start_col)
    end = line_col_to_offset(text, end_line, end_col)
    length = end - start
    
    return start+1, length

class LineNumberArea(QtWidgets.QWidget):
    def __init__(self, editor):
        super().__init__(editor)
        self.code_editor = editor

    def sizeHint(self):
        return QtCore.QSize(self.code_editor.line_number_area_width(), 0)

    def paintEvent(self, event):
        self.code_editor.line_number_area_paint_event(event)


class CodeEditor(QtWidgets.QPlainTextEdit):
    def __init__(self, parent=None):
        super().__init__(parent)
        
        self.line_number_area = LineNumberArea(self)
        
        self.blockCountChanged.connect(self.update_line_number_area_width)
        self.updateRequest.connect(self.update_line_number_area)
        
        self.update_line_number_area_width(0)
        
        # Set monospace font
        font = QtGui.QFont("Courier New", 12)
        self.setFont(font)
        
        # Set tab stop width
        self.setTabStopDistance(20)  # PySide6

    def line_number_area_width(self):
        digits = len(str(max(1, self.blockCount())))
        space = 10 + self.fontMetrics().horizontalAdvance('9') * digits
        return space

    def update_line_number_area_width(self, _):
        self.setViewportMargins(self.line_number_area_width(), 0, 0, 0)

    def update_line_number_area(self, rect, dy):
        if dy:
            self.line_number_area.scroll(0, dy)
        else:
            self.line_number_area.update(0, rect.y(), self.line_number_area.width(), rect.height())
        
        if rect.contains(self.viewport().rect()):
            self.update_line_number_area_width(0)

    def resizeEvent(self, event):
        super().resizeEvent(event)
        cr = self.contentsRect()
        self.line_number_area.setGeometry(QtCore.QRect(cr.left(), cr.top(), self.line_number_area_width(), cr.height()))

    def line_number_area_paint_event(self, event):
        painter = QtGui.QPainter(self.line_number_area)
        
        # Use Houdini's palette colors
        palette = self.palette()
        bg_color = palette.color(QtGui.QPalette.Window)
        text_color = palette.color(QtGui.QPalette.WindowText)
        
        # Make background slightly darker/lighter than main background
        if bg_color.lightness() > 128:  # Light theme
            bg_color = bg_color.darker(105)
        else:  # Dark theme
            bg_color = bg_color.lighter(120)
        
        painter.fillRect(event.rect(), bg_color)

        block = self.firstVisibleBlock()
        block_number = block.blockNumber()
        top = self.blockBoundingGeometry(block).translated(self.contentOffset()).top()
        bottom = top + self.blockBoundingRect(block).height()

        while block.isValid() and top <= event.rect().bottom():
            if block.isVisible() and bottom >= event.rect().top():
                number = str(block_number + 1)
                # Use dimmed text color for line numbers
                dimmed_color = QtGui.QColor(text_color)
                dimmed_color.setAlpha(128)  # Semi-transparent
                painter.setPen(dimmed_color)
                painter.drawText(0, int(top), self.line_number_area.width() - 5, 
                               self.fontMetrics().height(), QtCore.Qt.AlignRight, number)

            block = block.next()
            top = bottom
            bottom = top + self.blockBoundingRect(block).height()
            block_number += 1


class ErrorHighlighter(QtGui.QSyntaxHighlighter):
    def __init__(self, parent, error_ranges=None):
        super(ErrorHighlighter, self).__init__(parent)
        self.error_ranges = error_ranges or []
        self.warning_ranges = []
        self.info_ranges = []
    
    def set_messages(self, errors=None, warnings=None, infos=None):
        """
        errors: list of (start_pos, length) tuples for errors
        warnings: list of (start_pos, length) tuples for warnings
        infos: list of (start_pos, length) tuples for info messages
        """
        self.error_ranges = errors or []
        self.warning_ranges = warnings or []
        self.info_ranges = infos or []
        self.rehighlight()
    
    def highlightBlock(self, text):
        block_pos = self.currentBlock().position()
        
        # Highlight errors (red squiggle)
        for start, length in self.error_ranges:
            if start < block_pos + len(text) and start + length > block_pos:
                block_start = max(0, start - block_pos)
                block_length = min(length, len(text) - block_start)
                
                error_format = QtGui.QTextCharFormat()
                error_format.setUnderlineColor(QtGui.QColor("red"))
                error_format.setUnderlineStyle(QtGui.QTextCharFormat.WaveUnderline)
                self.setFormat(block_start, block_length, error_format)
        
        # Highlight warnings (orange/yellow squiggle)
        for start, length in self.warning_ranges:
            if start < block_pos + len(text) and start + length > block_pos:
                block_start = max(0, start - block_pos)
                block_length = min(length, len(text) - block_start)
                
                warning_format = QtGui.QTextCharFormat()
                warning_format.setUnderlineColor(QtGui.QColor(255, 165, 0))  # Orange
                warning_format.setUnderlineStyle(QtGui.QTextCharFormat.WaveUnderline)
                self.setFormat(block_start, block_length, warning_format)
        
        # Highlight info (blue squiggle)
        for start, length in self.info_ranges:
            if start < block_pos + len(text) and start + length > block_pos:
                block_start = max(0, start - block_pos)
                block_length = min(length, len(text) - block_start)
                
                info_format = QtGui.QTextCharFormat()
                info_format.setUnderlineColor(QtGui.QColor(0, 120, 215))  # Blue
                info_format.setUnderlineStyle(QtGui.QTextCharFormat.WaveUnderline)
                self.setFormat(block_start, block_length, info_format)


class CompilerEditorWidget(QtWidgets.QWidget):
    def __init__(self, parent=None, node=None, parm_name="code", compiler=""):
        super(CompilerEditorWidget, self).__init__(parent)
        
        self.node = node
        self.parm_name = parm_name
        self.compiler = compiler
        
        layout = QtWidgets.QVBoxLayout()
        
        # Use custom code editor with line numbers
        self.text_edit = CodeEditor()
        
        # Error highlighter
        self.highlighter = ErrorHighlighter(self.text_edit.document())
        
        # Button bar
        button_layout = QtWidgets.QHBoxLayout()
        
        self.compile_button = QtWidgets.QPushButton("Compile")
        self.compile_button.clicked.connect(self.compile_and_highlight)
        
        self.apply_button = QtWidgets.QPushButton("Apply")
        self.apply_button.clicked.connect(self.apply_changes)
        
        self.ok_button = QtWidgets.QPushButton("OK")
        self.ok_button.clicked.connect(self.ok_and_close)
        
        self.cancel_button = QtWidgets.QPushButton("Cancel")
        self.cancel_button.clicked.connect(self.close)
        
        button_layout.addWidget(self.compile_button)
        button_layout.addStretch()
        button_layout.addWidget(self.apply_button)
        button_layout.addWidget(self.ok_button)
        button_layout.addWidget(self.cancel_button)
        
        # Message display area
        self.message_area = QtWidgets.QTextEdit()
        self.message_area.setReadOnly(True)
        self.message_area.setMaximumHeight(150)
        
        # Add widgets to layout
        layout.addWidget(self.text_edit, stretch=3)
        layout.addLayout(button_layout)
        layout.addWidget(self.message_area, stretch=1)
        self.setLayout(layout)
        
        # Auto-compile on text change (optional, with debounce)
        # self.compile_timer = QtCore.QTimer()
        # self.compile_timer.setSingleShot(True)
        # self.compile_timer.timeout.connect(self.compile_and_highlight)
        # self.text_edit.textChanged.connect(lambda: self.compile_timer.start(500))
    
    def your_compiler(self, code):
        """
        Replace this with your actual compiler.
        Should return dict with:
        {
            'errors': [(start_pos, length, message), ...],
            'warnings': [(start_pos, length, message), ...],
            'infos': [(start_pos, length, message), ...]
        }
        """
        errors = []
        warnings = []
        infos = []


        # Basic usage - capture stdout
        result = subprocess.run([self.compiler, code], capture_output=True, text=True)

        result = json.loads(result.stdout)

        for msg in result["messages"]:

            line_start = msg["pos"]["line"]
            col_start = msg["pos"]["column"]
            line_end = msg["endPos"]["line"]
            col_end = msg["endPos"]["column"]

            start, length = compute_start_length(code, line_start, col_start, line_end, col_end)

            if msg["severity"] == "error":
                errors.append((start, length, msg["data"]))
            if msg["severity"] == "warning":
                warnings.append((start, length, msg["data"]))
            if msg["severity"] == "information":
                infos.append((start, length, msg["data"]))
        
        return {
            'errors': errors,
            'warnings': warnings,
            'infos': infos
        }
    
    def compile_and_highlight(self):
        code = self.text_edit.toPlainText()
        
        # Call your compiler
        result = self.your_compiler(code)
        
        errors = result.get('errors', [])
        warnings = result.get('warnings', [])
        infos = result.get('infos', [])
        
        # Update highlights
        error_ranges = [(e[0], e[1]) for e in errors]
        warning_ranges = [(w[0], w[1]) for w in warnings]
        info_ranges = [(i[0], i[1]) for i in infos]
        
        self.highlighter.set_messages(error_ranges, warning_ranges, info_ranges)
        
        # Build message display
        message_html = ""
        
        if errors:
            message_html += "<b style='color: red;'>ERRORS:</b><br>"
            for e in errors:
                message_html += f"<span style='color: red;'>● {e[2]}</span><br>"
            message_html += "<br>"
        
        if warnings:
            message_html += "<b style='color: orange;'>WARNINGS:</b><br>"
            for w in warnings:
                message_html += f"<span style='color: orange;'>● {w[2]}</span><br>"
            message_html += "<br>"
        
        if infos:
            message_html += "<b style='color: blue;'>INFO:</b><br>"
            for i in infos:
                message_html += f"<span style='color: #0078d7;'>● {i[2]}</span><br>"
            message_html += "<br>"
        
        if not errors and not warnings and not infos:
            message_html = "<span style='color: green; font-weight: bold;'>✓ Compiled successfully</span>"
        
        self.message_area.setHtml(message_html)
    
    def apply_changes(self):
        """Apply changes back to the parameter"""
        if self.node and self.parm_name:
            try:
                code = self.text_edit.toPlainText()
                self.node.parm(self.parm_name).set(code)
            except Exception as e:
                hou.ui.displayMessage(f"Error applying changes: {str(e)}", severity=hou.severityType.Error)
    
    def ok_and_close(self):
        """Apply changes and close the window"""
        self.apply_changes()
        self.close()
    
    def set_code(self, code):
        """Set the editor content"""
        self.text_edit.setPlainText(code)
        self.compile_and_highlight()


# Keep reference to widget to prevent garbage collection
_editor_widgets = {}

def open_code_editor(node, parm_name="code", compiler=""):
    """
    Open the compiler editor for a specific parameter.
    
    Args:
        node: The HDA node
        parm_name: The name of the string parameter to edit
    """
    global _editor_widgets
    
    # Create a unique key for this node/parameter combination
    widget_key = f"{node.path()}_{parm_name}"
    
    # If editor already exists for this parameter, bring it to front
    if widget_key in _editor_widgets and _editor_widgets[widget_key].isVisible():
        _editor_widgets[widget_key].raise_()
        _editor_widgets[widget_key].activateWindow()
        return _editor_widgets[widget_key]
    
    # Create new editor
    editor = CompilerEditorWidget(parent=hou.qt.mainWindow(), node=node, parm_name=parm_name, compiler = compiler)
    editor.setWindowTitle(f"Edit {parm_name} - {node.name()}")
    editor.setWindowFlags(QtCore.Qt.Window)
    editor.resize(900, 700)
    
    # Load current parameter value
    try:
        current_code = node.parm(parm_name).eval()
        editor.set_code(current_code)
    except:
        pass
    
    editor.show()
    _editor_widgets[widget_key] = editor
    
    return editor


def open_code_editor_from_kwargs(kwargs):
    """
    Convenience function to call from a button callback.
    Use this in your parameter's callback script.
    """
    node = kwargs['node']
    # You can customize which parameter to edit here
    # By default, it will edit a parameter called 'code'
    # Change 'code' to your parameter name
    parm_name = 'code'
    compiler = node.parm("compiler").evalAsString()
    return open_code_editor(node, parm_name, compiler)
