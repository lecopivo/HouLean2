from PySide6 import QtWidgets, QtGui, QtCore
import json
import subprocess
import threading

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


class KeybindingPresets:
    """Keybinding presets for different editing styles"""
    
    @staticmethod
    def standard():
        """Standard Qt/Windows keybindings"""
        return {
            'move_up': 'Up',
            'move_down': 'Down',
            'move_left': 'Left',
            'move_right': 'Right',
            'move_word_left': 'Ctrl+Left',
            'move_word_right': 'Ctrl+Right',
            'move_line_start': 'Home',
            'move_line_end': 'End',
            'move_doc_start': 'Ctrl+Home',
            'move_doc_end': 'Ctrl+End',
            'delete_char': 'Delete',
            'backspace': 'Backspace',
            'delete_word': 'Ctrl+Delete',
            'backspace_word': 'Ctrl+Backspace',
            'kill_line': 'Ctrl+K',
            'cut': 'Ctrl+X',
            'copy': 'Ctrl+C',
            'paste': 'Ctrl+V',
            'undo': 'Ctrl+Z',
            'redo': 'Ctrl+Y',
            'select_all': 'Ctrl+A',
            'mark_set': 'Ctrl+Space',
            'zoom_in': 'Ctrl+=',
            'zoom_out': 'Ctrl+-',
            'zoom_reset': 'Ctrl+0'
        }
    
    @staticmethod
    def emacs():
        """Emacs-style keybindings"""
        return {
            'move_up': 'Ctrl+P',
            'move_down': 'Ctrl+N',
            'move_left': 'Ctrl+B',
            'move_right': 'Ctrl+F',
            'move_word_left': 'Alt+B',
            'move_word_right': 'Alt+F',
            'move_line_start': 'Ctrl+A',
            'move_line_end': 'Ctrl+E',
            'move_doc_start': 'Alt+<',
            'move_doc_end': 'Alt+>',
            'delete_char': 'Ctrl+D',
            'backspace': 'Backspace',
            'delete_word': 'Alt+D',
            'backspace_word': 'Alt+Backspace',
            'kill_line': 'Ctrl+K',
            'cut': 'Ctrl+W',
            'copy': 'Alt+W',
            'paste': 'Ctrl+Y',
            'paste_cycle': 'Alt+Y',
            'undo': 'Ctrl+/',
            'redo': 'Ctrl+Shift+/',
            'select_all': 'Ctrl+X,H',  # Emacs uses Ctrl+X H, not Ctrl+A
            'mark_set': 'Ctrl+Space',
            'cancel': 'Ctrl+G',
            'zoom_in': 'Ctrl+=',
            'zoom_out': 'Ctrl+-',
            'zoom_reset': 'Ctrl+0'
        }


class LineNumberArea(QtWidgets.QWidget):
    def __init__(self, editor):
        super().__init__(editor)
        self.code_editor = editor

    def sizeHint(self):
        return QtCore.QSize(self.code_editor.line_number_area_width(), 0)

    def paintEvent(self, event):
        self.code_editor.line_number_area_paint_event(event)


class CodeEditor(QtWidgets.QPlainTextEdit):
    # Signal to notify parent of font size changes
    font_size_changed = QtCore.Signal(int)
    
    def __init__(self, parent=None, keybindings=None):
        super().__init__(parent)
        
        self.line_number_area = LineNumberArea(self)
        self.error_messages = {}  # Map from (start, length) to message text
        
        # Kill ring for emacs-style cut/paste
        self.kill_ring = []
        self.kill_ring_index = 0
        self.max_kill_ring_size = 20
        self.last_action_was_paste_cycle = False
        
        # Mark for region selection (emacs-style)
        self.mark_position = None
        
        # Font size management
        self.default_font_size = 11
        self.current_font_size = self.default_font_size
        
        # Custom tooltip
        self.tooltip_widget = None
        
        # Keybindings
        self.keybindings = keybindings or KeybindingPresets.standard()
        
        # Disable default Ctrl+A behavior for emacs mode
        self.is_emacs_mode = False
        if keybindings:
            # Check if this looks like emacs bindings
            self.is_emacs_mode = keybindings.get('move_line_start') == 'Ctrl+A'
        
        self.setup_keybindings()
        
        self.blockCountChanged.connect(self.update_line_number_area_width)
        self.updateRequest.connect(self.update_line_number_area)
        
        self.update_line_number_area_width(0)
        
        # Set monospace font with better readability
        self.update_font_size(self.default_font_size)
        
        # Set tab stop width
        self.setTabStopDistance(40)
        
        # Modern styling
        self.setStyleSheet("""
            QPlainTextEdit {
                background-color: #1e1e1e;
                color: #d4d4d4;
                border: none;
                selection-background-color: #264f78;
            }
        """)
        
        # Enable mouse tracking for hover tooltips
        self.setMouseTracking(True)
        self.viewport().setMouseTracking(True)
    
    def setup_keybindings(self):
        """Setup keyboard shortcuts based on current keybinding scheme"""
        # Clear existing shortcuts
        for child in self.children():
            if isinstance(child, QtGui.QShortcut):
                child.deleteLater()
        
        kb = self.keybindings
        
        # Movement shortcuts
        if kb.get('move_up'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['move_up']), self, lambda: self.move_cursor(QtGui.QTextCursor.Up))
        if kb.get('move_down'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['move_down']), self, lambda: self.move_cursor(QtGui.QTextCursor.Down))
        if kb.get('move_left'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['move_left']), self, lambda: self.move_cursor(QtGui.QTextCursor.Left))
        if kb.get('move_right'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['move_right']), self, lambda: self.move_cursor(QtGui.QTextCursor.Right))
        if kb.get('move_word_left'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['move_word_left']), self, lambda: self.move_cursor(QtGui.QTextCursor.WordLeft))
        if kb.get('move_word_right'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['move_word_right']), self, lambda: self.move_cursor(QtGui.QTextCursor.WordRight))
        if kb.get('move_line_start'):
            shortcut = QtGui.QShortcut(QtGui.QKeySequence(kb['move_line_start']), self)
            shortcut.activated.connect(lambda: self.move_cursor(QtGui.QTextCursor.StartOfLine))
            shortcut.setContext(QtCore.Qt.WidgetShortcut)
        if kb.get('move_line_end'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['move_line_end']), self, lambda: self.move_cursor(QtGui.QTextCursor.EndOfLine))
        if kb.get('move_doc_start'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['move_doc_start']), self, lambda: self.move_cursor(QtGui.QTextCursor.Start))
        if kb.get('move_doc_end'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['move_doc_end']), self, lambda: self.move_cursor(QtGui.QTextCursor.End))
        
        # Deletion shortcuts
        if kb.get('delete_char'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['delete_char']), self, self.delete_char)
        if kb.get('delete_word'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['delete_word']), self, self.delete_word)
        if kb.get('backspace_word'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['backspace_word']), self, self.backspace_word)
        if kb.get('kill_line'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['kill_line']), self, self.kill_line)
        
        # Clipboard shortcuts
        if kb.get('cut'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['cut']), self, self.kill_region)
        if kb.get('copy'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['copy']), self, self.copy_region)
        if kb.get('paste'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['paste']), self, self.yank)
        if kb.get('paste_cycle'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['paste_cycle']), self, self.yank_pop)
        
        # Undo/Redo shortcuts
        if kb.get('undo'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['undo']), self, self.undo)
        if kb.get('redo'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['redo']), self, self.redo)
        
        # Selection shortcuts
        if kb.get('mark_set'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['mark_set']), self, self.set_mark)
        if kb.get('cancel'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['cancel']), self, self.cancel_operation)
        
        # Zoom shortcuts
        if kb.get('zoom_in'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['zoom_in']), self, self.zoom_in)
        if kb.get('zoom_out'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['zoom_out']), self, self.zoom_out)
        if kb.get('zoom_reset'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['zoom_reset']), self, self.zoom_reset)
    
    def set_keybindings(self, keybindings):
        """Change keybinding scheme"""
        self.keybindings = keybindings
        # Check if this is emacs mode
        self.is_emacs_mode = keybindings.get('move_line_start') == 'Ctrl+A'
        self.setup_keybindings()
    
    def update_font_size(self, size):
        """Update editor font size"""
        self.current_font_size = size
        font = QtGui.QFont("Consolas", size)
        if not font.exactMatch():
            font = QtGui.QFont("Courier New", size)
        self.setFont(font)
        # Notify parent about font size change
        self.font_size_changed.emit(size)
    
    def zoom_in(self):
        """Increase font size"""
        self.update_font_size(min(self.current_font_size + 1, 32))
    
    def zoom_out(self):
        """Decrease font size"""
        self.update_font_size(max(self.current_font_size - 1, 6))
    
    def zoom_reset(self):
        """Reset font size to default"""
        self.update_font_size(self.default_font_size)
    
    def show_custom_tooltip(self, severity, text, pos):
        """Show custom styled tooltip with severity-based color"""
        if self.tooltip_widget is None:
            self.tooltip_widget = QtWidgets.QLabel(None, QtCore.Qt.ToolTip | QtCore.Qt.FramelessWindowHint)
            self.tooltip_widget.setTextFormat(QtCore.Qt.PlainText)
            self.tooltip_widget.setWordWrap(False)
            self.tooltip_widget.setAttribute(QtCore.Qt.WA_TranslucentBackground, False)
            self.tooltip_widget.setAttribute(QtCore.Qt.WA_ShowWithoutActivating, True)
        
        # Determine border color based on severity
        border_colors = {
            'error': '#f48771',
            'warning': '#ffcc00',
            'info': '#4fc3f7'
        }
        border_color = border_colors.get(severity, '#007acc')
        
        # Modern tooltip styling with severity-based border
        self.tooltip_widget.setStyleSheet(f"""
            QLabel {{
                background-color: #2d2d30;
                color: #d4d4d4;
                border: 2px solid {border_color};
                border-radius: 4px;
                padding: 10px 14px;
                font-family: 'Consolas', 'Courier New', monospace;
                font-size: {self.current_font_size}px;
            }}
        """)
        
        self.tooltip_widget.setText(text)
        self.tooltip_widget.adjustSize()
        
        # Position tooltip near cursor with offset
        tooltip_pos = QtCore.QPoint(pos.x() + 15, pos.y() + 15)
        self.tooltip_widget.move(tooltip_pos)
        self.tooltip_widget.raise_()
        self.tooltip_widget.show()
    
    def hide_custom_tooltip(self):
        """Hide custom tooltip"""
        if self.tooltip_widget and self.tooltip_widget.isVisible():
            self.tooltip_widget.hide()
    
    def move_cursor(self, operation, keep_anchor=False):
        """Move cursor with optional selection"""
        cursor = self.textCursor()
        
        # If mark is set, maintain selection during movement
        if self.mark_position is not None:
            # Get current cursor position
            current_pos = cursor.position()
            
            # Create a new cursor and move it
            new_cursor = QtGui.QTextCursor(cursor)
            new_cursor.movePosition(operation, QtGui.QTextCursor.MoveAnchor)
            new_pos = new_cursor.position()
            
            # Now create selection between mark and new position
            final_cursor = QtGui.QTextCursor(self.document())
            final_cursor.setPosition(self.mark_position, QtGui.QTextCursor.MoveAnchor)
            final_cursor.setPosition(new_pos, QtGui.QTextCursor.KeepAnchor)
            
            self.setTextCursor(final_cursor)
        else:
            # Normal movement without selection
            mode = QtGui.QTextCursor.KeepAnchor if keep_anchor else QtGui.QTextCursor.MoveAnchor
            cursor.movePosition(operation, mode)
            self.setTextCursor(cursor)
    
    def set_mark(self):
        """Set/unset mark for region selection (emacs-style)"""
        if self.mark_position is None:
            self.mark_position = self.textCursor().position()
            # Visual feedback
            self.viewport().update()
        else:
            self.mark_position = None
            cursor = self.textCursor()
            cursor.clearSelection()
            self.setTextCursor(cursor)
    
    def cancel_operation(self):
        """Cancel current operation (Ctrl+G in Emacs)"""
        # Clear mark
        self.mark_position = None
        # Clear selection
        cursor = self.textCursor()
        cursor.clearSelection()
        self.setTextCursor(cursor)
        # Hide custom tooltip
        self.hide_custom_tooltip()
    
    def delete_char(self):
        """Delete character at cursor"""
        cursor = self.textCursor()
        if not cursor.hasSelection():
            cursor.deleteChar()
        else:
            cursor.removeSelectedText()
        self.setTextCursor(cursor)
    
    def delete_word(self):
        """Delete word forward"""
        cursor = self.textCursor()
        cursor.movePosition(QtGui.QTextCursor.EndOfWord, QtGui.QTextCursor.KeepAnchor)
        text = cursor.selectedText()
        if text:
            self.add_to_kill_ring(text)
        cursor.removeSelectedText()
        self.setTextCursor(cursor)
    
    def backspace_word(self):
        """Delete word backward"""
        cursor = self.textCursor()
        cursor.movePosition(QtGui.QTextCursor.StartOfWord, QtGui.QTextCursor.KeepAnchor)
        text = cursor.selectedText()
        if text:
            self.add_to_kill_ring(text)
        cursor.removeSelectedText()
        self.setTextCursor(cursor)
    
    def kill_line(self):
        """Kill from cursor to end of line"""
        cursor = self.textCursor()
        start_pos = cursor.position()
        cursor.movePosition(QtGui.QTextCursor.EndOfLine, QtGui.QTextCursor.KeepAnchor)
        text = cursor.selectedText()
        
        # If at end of line, kill the newline
        if not text:
            cursor.deleteChar()
            self.add_to_kill_ring('\n')
        else:
            self.add_to_kill_ring(text)
            cursor.removeSelectedText()
        
        self.setTextCursor(cursor)
    
    def kill_region(self):
        """Kill (cut) selected region"""
        cursor = self.textCursor()
        if cursor.hasSelection():
            text = cursor.selectedText()
            self.add_to_kill_ring(text)
            cursor.removeSelectedText()
            self.setTextCursor(cursor)
            self.mark_position = None
    
    def copy_region(self):
        """Copy selected region to kill ring"""
        cursor = self.textCursor()
        if cursor.hasSelection():
            text = cursor.selectedText()
            self.add_to_kill_ring(text)
            self.mark_position = None
    
    def add_to_kill_ring(self, text):
        """Add text to kill ring"""
        if text:
            self.kill_ring.append(text)
            if len(self.kill_ring) > self.max_kill_ring_size:
                self.kill_ring.pop(0)
            self.kill_ring_index = len(self.kill_ring) - 1
    
    def yank(self):
        """Paste from kill ring (emacs-style)"""
        if self.kill_ring:
            cursor = self.textCursor()
            cursor.insertText(self.kill_ring[self.kill_ring_index])
            self.setTextCursor(cursor)
            self.last_action_was_paste_cycle = True
    
    def yank_pop(self):
        """Cycle through kill ring (emacs-style)"""
        if not self.kill_ring or not self.last_action_was_paste_cycle:
            return
        
        # Remove the last yanked text
        cursor = self.textCursor()
        last_text = self.kill_ring[self.kill_ring_index]
        
        for _ in range(len(last_text)):
            cursor.deletePreviousChar()
        
        # Cycle to previous item in kill ring
        self.kill_ring_index = (self.kill_ring_index - 1) % len(self.kill_ring)
        
        # Insert new text
        cursor.insertText(self.kill_ring[self.kill_ring_index])
        self.setTextCursor(cursor)
    
    def keyPressEvent(self, event):
        """Override to track non-yank actions and handle special cases"""
        # Handle Tab key - insert 2 spaces instead of tab
        if event.key() == QtCore.Qt.Key_Tab and event.modifiers() == QtCore.Qt.NoModifier:
            cursor = self.textCursor()
            cursor.insertText("  ")  # Insert 2 spaces
            event.accept()
            return
        
        # In emacs mode, prevent Ctrl+A from selecting all
        if self.is_emacs_mode and event.key() == QtCore.Qt.Key_A and event.modifiers() == QtCore.Qt.ControlModifier:
            # Move to line start instead
            self.move_cursor(QtGui.QTextCursor.StartOfLine)
            event.accept()
            return
        
        # Reset yank cycle flag for non-yank actions
        if event.key() not in (QtCore.Qt.Key_Y,) or event.modifiers() != QtCore.Qt.AltModifier:
            if event.key() != QtCore.Qt.Key_Y or event.modifiers() != QtCore.Qt.ControlModifier:
                self.last_action_was_paste_cycle = False
        
        super().keyPressEvent(event)

    def mouseMoveEvent(self, event):
        """Show tooltip with error message on hover"""
        cursor = self.cursorForPosition(event.pos())
        position = cursor.position()
        
        # Check if cursor is over any error/warning/info range
        tooltip_data = None
        for (start, length), (severity, message) in self.error_messages.items():
            if start <= position < start + length:
                tooltip_data = (severity, message)
                break
        
        if tooltip_data:
            severity, message = tooltip_data
            self.show_custom_tooltip(severity, message, event.globalPos())
        else:
            self.hide_custom_tooltip()
        
        super().mouseMoveEvent(event)
    
    def leaveEvent(self, event):
        """Hide tooltip when mouse leaves"""
        self.hide_custom_tooltip()
        super().leaveEvent(event)

    def line_number_area_width(self):
        digits = len(str(max(1, self.blockCount())))
        space = 15 + self.fontMetrics().horizontalAdvance('9') * digits
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
        
        # Modern dark theme colors
        bg_color = QtGui.QColor("#252526")
        text_color = QtGui.QColor("#858585")
        
        painter.fillRect(event.rect(), bg_color)

        block = self.firstVisibleBlock()
        block_number = block.blockNumber()
        top = self.blockBoundingGeometry(block).translated(self.contentOffset()).top()
        bottom = top + self.blockBoundingRect(block).height()

        while block.isValid() and top <= event.rect().bottom():
            if block.isVisible() and bottom >= event.rect().top():
                number = str(block_number + 1)
                painter.setPen(text_color)
                painter.drawText(0, int(top), self.line_number_area.width() - 8, 
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
                error_format.setUnderlineColor(QtGui.QColor("#f48771"))
                error_format.setUnderlineStyle(QtGui.QTextCharFormat.WaveUnderline)
                # Add subtle background tint for more visibility
                error_format.setBackground(QtGui.QColor(244, 135, 113, 15))  # Very subtle red glow
                self.setFormat(block_start, block_length, error_format)
        
        # Highlight warnings (orange squiggle) - brighter and thicker
        for start, length in self.warning_ranges:
            if start < block_pos + len(text) and start + length > block_pos:
                block_start = max(0, start - block_pos)
                block_length = min(length, len(text) - block_start)
                
                warning_format = QtGui.QTextCharFormat()
                warning_format.setUnderlineColor(QtGui.QColor("#ffcc00"))  # Brighter yellow-orange
                warning_format.setUnderlineStyle(QtGui.QTextCharFormat.WaveUnderline)
                # Add subtle background tint for more visibility
                warning_format.setBackground(QtGui.QColor(255, 204, 0, 15))  # Very subtle yellow glow
                self.setFormat(block_start, block_length, warning_format)
        
        # Highlight info (blue squiggle) - brighter and thicker
        for start, length in self.info_ranges:
            if start < block_pos + len(text) and start + length > block_pos:
                block_start = max(0, start - block_pos)
                block_length = min(length, len(text) - block_start)
                
                info_format = QtGui.QTextCharFormat()
                info_format.setUnderlineColor(QtGui.QColor("#4fc3f7"))  # Brighter cyan-blue
                info_format.setUnderlineStyle(QtGui.QTextCharFormat.WaveUnderline)
                # Add subtle background tint for more visibility
                info_format.setBackground(QtGui.QColor(79, 195, 247, 15))  # Very subtle blue glow
                self.setFormat(block_start, block_length, info_format)


class KeybindingDialog(QtWidgets.QDialog):
    """Dialog for selecting keybinding preset"""
    
    def __init__(self, current_preset="standard", parent=None):
        super().__init__(parent)
        self.setWindowTitle("Keybinding Settings")
        self.setModal(True)
        self.selected_preset = current_preset
        
        layout = QtWidgets.QVBoxLayout()
        
        # Preset selection
        preset_group = QtWidgets.QGroupBox("Keybinding Preset")
        preset_layout = QtWidgets.QVBoxLayout()
        
        self.standard_radio = QtWidgets.QRadioButton("Standard (Windows/Qt)")
        self.emacs_radio = QtWidgets.QRadioButton("Emacs")
        
        if current_preset == "emacs":
            self.emacs_radio.setChecked(True)
        else:
            self.standard_radio.setChecked(True)
        
        preset_layout.addWidget(self.standard_radio)
        preset_layout.addWidget(self.emacs_radio)
        preset_group.setLayout(preset_layout)
        
        # Keybinding reference
        info_text = QtWidgets.QTextEdit()
        info_text.setReadOnly(True)
        info_text.setMaximumHeight(300)
        info_text.setHtml(self.get_keybinding_reference())
        
        # Buttons
        button_box = QtWidgets.QDialogButtonBox(
            QtWidgets.QDialogButtonBox.Ok | QtWidgets.QDialogButtonBox.Cancel
        )
        button_box.accepted.connect(self.accept)
        button_box.rejected.connect(self.reject)
        
        layout.addWidget(preset_group)
        layout.addWidget(QtWidgets.QLabel("Keybinding Reference:"))
        layout.addWidget(info_text)
        layout.addWidget(button_box)
        
        self.setLayout(layout)
        self.resize(500, 500)
        
        # Update reference when preset changes
        self.standard_radio.toggled.connect(lambda: info_text.setHtml(self.get_keybinding_reference()))
        self.emacs_radio.toggled.connect(lambda: info_text.setHtml(self.get_keybinding_reference()))
    
    def get_keybinding_reference(self):
        """Generate HTML reference for current preset"""
        preset = "emacs" if self.emacs_radio.isChecked() else "standard"
        kb = KeybindingPresets.emacs() if preset == "emacs" else KeybindingPresets.standard()
        
        html = "<style>table { border-collapse: collapse; width: 100%; }"
        html += "th, td { padding: 8px; text-align: left; border-bottom: 1px solid #ddd; }"
        html += "th { background-color: #0e639c; color: white; }</style>"
        html += "<table><tr><th>Action</th><th>Keybinding</th></tr>"
        
        categories = [
            ("Movement", ['move_up', 'move_down', 'move_left', 'move_right', 
                         'move_word_left', 'move_word_right', 'move_line_start', 
                         'move_line_end', 'move_doc_start', 'move_doc_end']),
            ("Editing", ['delete_char', 'backspace', 'delete_word', 'backspace_word', 'kill_line']),
            ("Clipboard", ['cut', 'copy', 'paste', 'paste_cycle']),
            ("Selection", ['mark_set', 'cancel', 'select_all']),
            ("Undo/Redo", ['undo', 'redo']),
            ("View", ['zoom_in', 'zoom_out', 'zoom_reset'])
        ]
        
        for category, keys in categories:
            html += f"<tr><td colspan='2'><b>{category}</b></td></tr>"
            for key in keys:
                if key in kb and kb[key]:
                    action_name = key.replace('_', ' ').title()
                    html += f"<tr><td>{action_name}</td><td><code>{kb[key]}</code></td></tr>"
        
        html += "</table>"
        return html
    
    def get_selected_preset(self):
        """Return selected preset name"""
        return "emacs" if self.emacs_radio.isChecked() else "standard"


class CompilerEditorWidget(QtWidgets.QWidget):
    # Signal for async compilation completion
    compilation_finished = QtCore.Signal(dict)
    
    def __init__(self, parent=None, node=None, parm_name="code", compiler=""):
        super(CompilerEditorWidget, self).__init__(parent)
        
        self.node = node
        self.parm_name = parm_name
        self.compiler = compiler
        self.is_compiling = False
        self.is_manual_compile = False  # Track if compilation was triggered manually
        self.current_preset = "emacs"
        self.compile_process = None  # Track subprocess
        self.compile_thread = None  # Track thread
        self.should_cancel_compile = False  # Flag to cancel compilation
        self.pending_compile_code = None  # Store code for pending compilation
        self.pending_is_manual = False  # Track if pending compile is manual
        
        layout = QtWidgets.QVBoxLayout()
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)
        
        # Use custom code editor with line numbers
        self.text_edit = CodeEditor(keybindings=KeybindingPresets.emacs())
        
        # Error highlighter
        self.highlighter = ErrorHighlighter(self.text_edit.document())
        
        # Button bar with modern styling
        button_bar = QtWidgets.QWidget()
        button_bar.setStyleSheet("""
            QWidget {
                background-color: #2d2d30;
                border-bottom: 1px solid #3e3e42;
            }
            QPushButton {
                background-color: #0e639c;
                color: white;
                border: none;
                padding: 6px 16px;
                font-size: 13px;
                border-radius: 2px;
            }
            QPushButton:hover {
                background-color: #1177bb;
            }
            QPushButton:pressed {
                background-color: #0d5a8f;
            }
            QPushButton:disabled {
                background-color: #3e3e42;
                color: #656565;
            }
        """)
        button_layout = QtWidgets.QHBoxLayout(button_bar)
        button_layout.setContentsMargins(8, 8, 8, 8)
        
        self.compile_button = QtWidgets.QPushButton("⚡ Compile")
        self.compile_button.clicked.connect(self.compile_and_highlight)
        
        self.keybinding_button = QtWidgets.QPushButton("⌨ Keybindings")
        self.keybinding_button.clicked.connect(self.show_keybinding_dialog)
        
        self.status_label = QtWidgets.QLabel("")
        self.status_label.setStyleSheet("color: #cccccc; padding-left: 10px;")
        
        self.apply_button = QtWidgets.QPushButton("Apply")
        self.apply_button.clicked.connect(self.apply_changes)
        
        self.ok_button = QtWidgets.QPushButton("OK")
        self.ok_button.clicked.connect(self.ok_and_close)
        
        self.cancel_button = QtWidgets.QPushButton("Cancel")
        self.cancel_button.setStyleSheet("""
            QPushButton {
                background-color: #3e3e42;
            }
            QPushButton:hover {
                background-color: #505053;
            }
        """)
        self.cancel_button.clicked.connect(self.close)
        
        button_layout.addWidget(self.compile_button)
        button_layout.addWidget(self.keybinding_button)
        button_layout.addWidget(self.status_label)
        button_layout.addStretch()
        button_layout.addWidget(self.apply_button)
        button_layout.addWidget(self.ok_button)
        button_layout.addWidget(self.cancel_button)
        
        # Splitter for resizable message area
        splitter = QtWidgets.QSplitter(QtCore.Qt.Vertical)
        splitter.setStyleSheet("""
            QSplitter::handle {
                background-color: #3e3e42;
                height: 2px;
            }
            QSplitter::handle:hover {
                background-color: #007acc;
            }
        """)
        
        # Message display area with modern styling
        self.message_area = QtWidgets.QTextEdit()
        self.message_area.setReadOnly(True)
        self.message_font_size = 11
        self.update_message_area_style()
        
        splitter.addWidget(self.text_edit)
        splitter.addWidget(self.message_area)
        splitter.setStretchFactor(0, 3)
        splitter.setStretchFactor(1, 1)
        splitter.setSizes([500, 150])
        
        # Add widgets to layout
        layout.addWidget(button_bar)
        layout.addWidget(splitter)
        self.setLayout(layout)
        
        # Connect font size changes
        self.text_edit.font_size_changed.connect(self.on_font_size_changed)
        
        # Connect async compilation signal
        self.compilation_finished.connect(self.on_compilation_finished)
        
        # Auto-compile on text change with debounce
        self.compile_timer = QtCore.QTimer()
        self.compile_timer.setSingleShot(True)
        self.compile_timer.timeout.connect(self.compile_silently)  # Use silent compile for auto
        self.text_edit.textChanged.connect(self.on_text_changed)
    
    def on_text_changed(self):
        """Handle text changes - cancel pending compilation and restart timer"""
        # Stop the timer
        self.compile_timer.stop()
        
        # If already compiling, mark for cancellation and queue new compile
        if self.is_compiling:
            self.pending_compile_code = self.text_edit.toPlainText()
            self.cancel_current_compilation()
        else:
            # Start new timer
            self.compile_timer.start(1000)
    
    def cancel_current_compilation(self):
        """Cancel the currently running compilation"""
        self.should_cancel_compile = True
        
        # Force kill the process (don't be polite)
        if self.compile_process:
            try:
                self.compile_process.kill()  # Use kill instead of terminate
                self.compile_process.wait(timeout=0.5)
            except:
                pass
    
    def update_message_area_style(self):
        """Update message area stylesheet with current font size"""
        self.message_area.setStyleSheet(f"""
            QTextEdit {{
                background-color: #1e1e1e;
                color: #d4d4d4;
                border: none;
                border-top: 1px solid #3e3e42;
                padding: 8px;
                font-family: 'Consolas', 'Courier New', monospace;
                font-size: {self.message_font_size}px;
            }}
        """)
    
    def on_font_size_changed(self, size):
        """Handle font size changes from the editor"""
        self.message_font_size = size
        self.update_message_area_style()
    
    def show_keybinding_dialog(self):
        """Show keybinding selection dialog"""
        dialog = KeybindingDialog(self.current_preset, self)
        if dialog.exec() == QtWidgets.QDialog.Accepted:
            preset = dialog.get_selected_preset()
            self.current_preset = preset
            if preset == "emacs":
                self.text_edit.set_keybindings(KeybindingPresets.emacs())
            else:
                self.text_edit.set_keybindings(KeybindingPresets.standard())
    
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

        # Check if we should cancel
        if self.should_cancel_compile:
            return {'errors': [], 'warnings': [], 'infos': [], 'cancelled': True}

        try:
            # Create process with ability to terminate
            self.compile_process = subprocess.Popen(
                [self.compiler, code],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True
            )
            
            # Wait with timeout (5 seconds for auto-compile)
            timeout = 5 if not self.is_manual_compile else 30
            
            try:
                stdout, stderr = self.compile_process.communicate(timeout=timeout)
            except subprocess.TimeoutExpired:
                # Kill the process on timeout
                self.compile_process.kill()
                self.compile_process.wait()
                return {'errors': [(0, 0, "Compilation timeout")], 'warnings': [], 'infos': []}
            
            # Check if cancelled during compilation
            if self.should_cancel_compile:
                return {'errors': [], 'warnings': [], 'infos': [], 'cancelled': True}

            result = json.loads(stdout)

            for msg in result["messages"]:

                line_start = msg["pos"]["line"]
                col_start = msg["pos"]["column"]
                
                # Check if endPos exists, otherwise use pos and length of 1
                if "endPos" in msg:
                    line_end = msg["endPos"]["line"]
                    col_end = msg["endPos"]["column"]
                    start, length = compute_start_length(code, line_start, col_start, line_end, col_end)
                else:
                    # No endPos, just compute position and use length 1
                    start = line_col_to_offset(code, line_start, col_start) + 1
                    length = 1

                if msg["severity"] == "error":
                    errors.append((start, length, msg["data"]))
                if msg["severity"] == "warning":
                    warnings.append((start, length, msg["data"]))
                if msg["severity"] == "information":
                    infos.append((start, length, msg["data"]))
            
        except Exception as e:
            if not self.should_cancel_compile:
                return {'errors': [(0, 0, f"Compilation error: {str(e)}")], 'warnings': [], 'infos': []}
            else:
                return {'errors': [], 'warnings': [], 'infos': [], 'cancelled': True}
        finally:
            self.compile_process = None
        
        return {
            'errors': errors,
            'warnings': warnings,
            'infos': infos
        }
    
    def compile_async(self, code):
        """Run compilation in background thread - only if not already compiling"""
        # Don't start a new thread if one is already running
        if self.compile_thread and self.compile_thread.is_alive():
            return
        
        def compile_thread():
            try:
                result = self.your_compiler(code)
                # Only emit if not cancelled
                if not result.get('cancelled', False):
                    self.compilation_finished.emit(result)
            except Exception as e:
                # Only report errors if not cancelled
                if not self.should_cancel_compile:
                    error_result = {
                        'errors': [(0, 0, f"Compilation error: {str(e)}")],
                        'warnings': [],
                        'infos': []
                    }
                    self.compilation_finished.emit(error_result)
            finally:
                # Always reset state when thread completes
                self.is_compiling = False
                self.should_cancel_compile = False
                
                # If there's pending code, trigger new compilation
                if self.pending_compile_code is not None:
                    pending = self.pending_compile_code
                    is_manual = self.pending_is_manual
                    self.pending_compile_code = None
                    self.pending_is_manual = False
                    # Use QTimer to ensure we're back in the main thread
                    QtCore.QTimer.singleShot(0, lambda: self._start_pending_compile(pending, is_manual))
        
        self.compile_thread = threading.Thread(target=compile_thread, daemon=True)
        self.compile_thread.start()
    
    def _start_pending_compile(self, code, is_manual):
        """Start compilation with pending code"""
        if not self.is_compiling:
            self.is_compiling = True
            self.is_manual_compile = is_manual
            
            # Update UI if manual
            if is_manual:
                self.compile_button.setEnabled(False)
                self.status_label.setText("⏳ Compiling...")
                self.status_label.setStyleSheet("color: #cca700;")
            
            self.compile_async(code)
    
    def compile_and_highlight(self):
        """Start async compilation (manual)"""
        # If already compiling, cancel it and queue this compilation
        if self.is_compiling:
            self.pending_compile_code = self.text_edit.toPlainText()
            self.pending_is_manual = True
            self.cancel_current_compilation()
            # Show status immediately
            self.compile_button.setEnabled(False)
            self.status_label.setText("⏳ Compiling...")
            self.status_label.setStyleSheet("color: #cca700;")
            return
        
        # Start compilation immediately
        self.should_cancel_compile = False
        self.pending_compile_code = None
        self.pending_is_manual = False
        self.is_compiling = True
        self.is_manual_compile = True
        self.compile_button.setEnabled(False)
        self.status_label.setText("⏳ Compiling...")
        self.status_label.setStyleSheet("color: #cca700;")
        
        code = self.text_edit.toPlainText()
        self.compile_async(code)
    
    def compile_silently(self):
        """Start async compilation without UI feedback (for auto-compile)"""
        # Don't start if already compiling
        if self.is_compiling:
            return
        
        self.should_cancel_compile = False
        self.pending_compile_code = None
        self.is_compiling = True
        self.is_manual_compile = False
        
        code = self.text_edit.toPlainText()
        self.compile_async(code)
    
    def on_compilation_finished(self, result):
        """Handle compilation results from async thread"""
        was_manual = self.is_manual_compile
        self.is_compiling = False
        self.should_cancel_compile = False
        
        # Only update button state if it was a manual compile
        if was_manual:
            self.compile_button.setEnabled(True)
        
        errors = result.get('errors', [])
        warnings = result.get('warnings', [])
        infos = result.get('infos', [])
        
        # Update error messages map for tooltips
        self.text_edit.error_messages.clear()
        for start, length, msg in errors:
            self.text_edit.error_messages[(start, length)] = ('error', msg)
        for start, length, msg in warnings:
            self.text_edit.error_messages[(start, length)] = ('warning', msg)
        for start, length, msg in infos:
            self.text_edit.error_messages[(start, length)] = ('info', msg)
        
        # Update highlights
        error_ranges = [(e[0], e[1]) for e in errors]
        warning_ranges = [(w[0], w[1]) for w in warnings]
        info_ranges = [(i[0], i[1]) for i in infos]
        
        self.highlighter.set_messages(error_ranges, warning_ranges, info_ranges)
        
        # Build message display with modern styling
        message_html = "<style>"
        message_html += f"body {{ font-family: 'Consolas', 'Courier New', monospace; font-size: {self.message_font_size}px; }}"
        message_html += ".error { color: #f48771; margin: 4px 0; }"
        message_html += ".warning { color: #ffcc00; margin: 4px 0; }"
        message_html += ".info { color: #4fc3f7; margin: 4px 0; }"
        message_html += ".success { color: #89d185; font-weight: bold; }"
        message_html += ".section { font-weight: bold; margin-top: 8px; margin-bottom: 4px; }"
        message_html += "</style>"
        
        if errors:
            message_html += "<div class='section error'>❌ ERRORS:</div>"
            for e in errors:
                message_html += f"<div class='error'>• {e[2]}</div>"
        
        if warnings:
            message_html += "<div class='section warning'>⚠️ WARNINGS:</div>"
            for w in warnings:
                message_html += f"<div class='warning'>• {w[2]}</div>"
        
        if infos:
            message_html += "<div class='section info'>ℹ️ INFO:</div>"
            for i in infos:
                message_html += f"<div class='info'>• {i[2]}</div>"
        
        if not errors and not warnings and not infos:
            message_html += "<div class='success'>✓ Compiled successfully</div>"
            if was_manual:
                self.status_label.setText("✓ Success")
                self.status_label.setStyleSheet("color: #89d185;")
        elif errors:
            if was_manual:
                self.status_label.setText(f"❌ {len(errors)} error(s)")
                self.status_label.setStyleSheet("color: #f48771;")
        elif warnings:
            if was_manual:
                self.status_label.setText(f"⚠️ {len(warnings)} warning(s)")
                self.status_label.setStyleSheet("color: #ffcc00;")
        else:
            if was_manual:
                self.status_label.setText(f"ℹ️ {len(infos)} info")
                self.status_label.setStyleSheet("color: #4fc3f7;")
        
        self.message_area.setHtml(message_html)
    
    def apply_changes(self):
        """Apply changes back to the parameter"""
        if self.node and self.parm_name:
            try:
                code = self.text_edit.toPlainText()
                self.node.parm(self.parm_name).set(code)
            except Exception as e:
                import hou
                hou.ui.displayMessage(f"Error applying changes: {str(e)}", severity=hou.severityType.Error)
    
    def ok_and_close(self):
        """Apply changes and close the window"""
        self.apply_changes()
        self.close()
    
    def closeEvent(self, event):
        """Clean up when closing"""
        # Cancel any ongoing compilation
        self.should_cancel_compile = True
        self.compile_timer.stop()
        
        if self.compile_process:
            try:
                self.compile_process.terminate()
                self.compile_process.wait(timeout=1)
            except:
                try:
                    self.compile_process.kill()
                except:
                    pass
        
        # Wait for thread to finish (with timeout)
        if self.compile_thread and self.compile_thread.is_alive():
            self.compile_thread.join(timeout=2)
        
        super().closeEvent(event)
    
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
        compiler: Path to the compiler executable
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
    import hou
    editor = CompilerEditorWidget(parent=hou.qt.mainWindow(), node=node, parm_name=parm_name, compiler=compiler)
    editor.setWindowTitle(f"Code Editor - {node.name()}.{parm_name}")
    editor.setWindowFlags(QtCore.Qt.Window)
    editor.resize(1000, 750)
    
    # Modern window styling
    editor.setStyleSheet("""
        QWidget {
            background-color: #1e1e1e;
        }
    """)
    
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
    parm_name = 'code'
    compiler = node.parm("compiler").evalAsString()
    return open_code_editor(node, parm_name, compiler)
