"""
Modern LSP Code Editor for Houdini

Combines LSP protocol support with VS Code-like UI.
Supports Lean 4 with 'lake serve' LSP server.
Works with both real files and virtual in-memory documents.

Usage:
    import lsp_editor
    node = hou.pwd()
    
    # With file
    lsp_editor.open_code_editor(
        node=node,
        parm_name="code",
        server_cmd="lake serve",
        workspace_path="/path/to/lean/project",
        file_path="/path/to/file.lean"
    )
    
    # In-memory (no file)
    lsp_editor.open_code_editor(
        node=node,
        parm_name="code",
        server_cmd="lake serve",
        workspace_path="/path/to/lean/project"
    )
"""

from PySide6 import QtWidgets, QtGui, QtCore
from PySide6.QtCore import QProcess
import json
import os
import re
from datetime import datetime


# ============================================================================
# Debug Logger
# ============================================================================

class DebugLogger(QtCore.QObject):
    """Debug logger for LSP communication"""
    logMessage = QtCore.Signal(str, str)
    
    def __init__(self):
        super().__init__()
        self.enabled = False
    
    def log(self, message, level="INFO"):
        if not self.enabled:
            return
        timestamp = datetime.now().strftime("%H:%M:%S.%f")[:-3]
        full_msg = f"[{timestamp}] [{level}] {message}"
        print(full_msg)
        self.logMessage.emit(full_msg, level)


logger = DebugLogger()


# ============================================================================
# LSP Client
# ============================================================================

class LSPClient(QtCore.QObject):
    """Language Server Protocol client implementation"""
    
    diagnosticsReceived = QtCore.Signal(list)
    completionReceived = QtCore.Signal(list)
    hoverReceived = QtCore.Signal(str)
    definitionReceived = QtCore.Signal(str, int, int)
    semanticTokensReceived = QtCore.Signal(object)
    serverInitialized = QtCore.Signal()
    
    def __init__(self, server_cmd, workspace_path):
        super().__init__()
        self.process = QProcess()
        self.server_cmd = server_cmd
        self.workspace_path = workspace_path
        self.message_id = 0
        self.pending_requests = {}
        self.buffer = b""
        self.semantic_token_legend = None
        self.is_initialized = False
        
        self.process.readyReadStandardOutput.connect(self._handle_stdout)
        self.process.readyReadStandardError.connect(self._handle_stderr)
        self.process.started.connect(self._on_process_started)
        self.process.errorOccurred.connect(self._on_process_error)
        self.process.finished.connect(self._on_process_finished)
        
        logger.log(f"LSPClient created: cmd={server_cmd}, workspace={workspace_path}")
    
    def start(self):
        """Start the LSP server"""
        logger.log(f"Starting LSP server: {self.server_cmd}")
        logger.log(f"Working directory: {self.workspace_path}")
        
        self.process.setWorkingDirectory(self.workspace_path)
        self.process.start(self.server_cmd[0], self.server_cmd[1:])
        
        if not self.process.waitForStarted(5000):
            logger.log("Failed to start LSP server process!", "ERROR")
            return False
        
        logger.log("LSP server process started successfully")
        return True
    
    def _on_process_started(self):
        logger.log("Process started signal received")
        self._send_initialize()
    
    def _on_process_error(self, error):
        error_msgs = {
            QProcess.ProcessError.FailedToStart: "Failed to start",
            QProcess.ProcessError.Crashed: "Process crashed",
            QProcess.ProcessError.Timedout: "Process timed out",
            QProcess.ProcessError.WriteError: "Write error",
            QProcess.ProcessError.ReadError: "Read error",
            QProcess.ProcessError.UnknownError: "Unknown error"
        }
        logger.log(f"Process error: {error_msgs.get(error, 'Unknown')}", "ERROR")
    
    def _on_process_finished(self, exit_code, exit_status):
        logger.log(f"Process finished: exit_code={exit_code}, status={exit_status}", 
                  "ERROR" if exit_code != 0 else "INFO")
    
    def _send_initialize(self):
        """Send initialize request"""
        logger.log("Sending initialize request")
        params = {
            "processId": os.getpid(),
            "rootUri": f"file://{self.workspace_path}",
            "capabilities": {
                "textDocument": {
                    "completion": {"completionItem": {"snippetSupport": True}},
                    "hover": {"contentFormat": ["plaintext", "markdown"]},
                    "definition": {"linkSupport": True},
                    "semanticTokens": {
                        "requests": {"full": True},
                        "tokenTypes": [
                            "namespace", "type", "class", "enum", "interface",
                            "struct", "typeParameter", "parameter", "variable",
                            "property", "enumMember", "event", "function",
                            "method", "macro", "keyword", "modifier", "comment",
                            "string", "number", "regexp", "operator"
                        ],
                        "tokenModifiers": [
                            "declaration", "definition", "readonly", "static",
                            "deprecated", "abstract", "async", "modification",
                            "documentation", "defaultLibrary"
                        ],
                        "formats": ["relative"]
                    }
                }
            }
        }
        msg_id = self._send_request("initialize", params)
        self.pending_requests[msg_id] = "initialize"
        logger.log(f"Initialize request sent with id={msg_id}")
    
    def _send_request(self, method, params):
        self.message_id += 1
        msg_id = self.message_id
        message = {
            "jsonrpc": "2.0",
            "id": msg_id,
            "method": method,
            "params": params
        }
        self._send_message(message)
        logger.log(f"Request sent: method={method}, id={msg_id}")
        return msg_id
    
    def _send_notification(self, method, params):
        message = {
            "jsonrpc": "2.0",
            "method": method,
            "params": params
        }
        self._send_message(message)
        logger.log(f"Notification sent: method={method}")
    
    def _send_message(self, message):
        if not self.process.state() == QProcess.ProcessState.Running:
            logger.log("Cannot send message: LSP server is not running", "ERROR")
            return
        
        content = json.dumps(message).encode('utf-8')
        header = f"Content-Length: {len(content)}\r\n\r\n".encode('utf-8')
        self.process.write(header + content)
    
    def _handle_stdout(self):
        data = bytes(self.process.readAllStandardOutput())
        self.buffer += data
        
        while True:
            if b"\r\n\r\n" not in self.buffer:
                break
            
            header_end = self.buffer.index(b"\r\n\r\n")
            header = self.buffer[:header_end].decode('utf-8')
            
            content_length = None
            for line in header.split('\r\n'):
                if line.startswith('Content-Length:'):
                    content_length = int(line.split(':')[1].strip())
                    break
            
            if content_length is None:
                break
            
            message_start = header_end + 4
            message_end = message_start + content_length
            
            if len(self.buffer) < message_end:
                break
            
            message_data = self.buffer[message_start:message_end]
            self.buffer = self.buffer[message_end:]
            
            try:
                message = json.loads(message_data.decode('utf-8'))
                self._handle_message(message)
            except json.JSONDecodeError as e:
                logger.log(f"JSON decode error: {e}", "ERROR")
    
    def _handle_stderr(self):
        error = bytes(self.process.readAllStandardError()).decode('utf-8')
        if error.strip():
            logger.log(f"LSP stderr: {error}", "WARNING")
    
    def _handle_message(self, message):
        """Handle a message from the LSP server"""
        if "method" in message:
            method = message["method"]
            params = message.get("params", {})
            
            if method == "textDocument/publishDiagnostics":
                self._handle_diagnostics(params)
        elif "id" in message:
            msg_id = message["id"]
            if msg_id in self.pending_requests:
                method = self.pending_requests[msg_id]
                result = message.get("result")
                error = message.get("error")
                
                if error:
                    logger.log(f"Response error for {method}: {error}", "ERROR")
                else:
                    logger.log(f"Response received for {method} (id={msg_id})")
                
                if method == "initialize":
                    self._handle_initialize(result)
                elif method == "textDocument/completion":
                    self._handle_completion(result)
                elif method == "textDocument/hover":
                    self._handle_hover(result)
                elif method == "textDocument/definition":
                    self._handle_definition(result)
                elif method == "textDocument/semanticTokens/full":
                    self._handle_semantic_tokens(result)
                
                del self.pending_requests[msg_id]
    
    def _handle_initialize(self, result):
        if result and "capabilities" in result:
            capabilities = result["capabilities"]
            logger.log(f"Server capabilities received: {list(capabilities.keys())}")
            
            if "semanticTokensProvider" in capabilities:
                provider = capabilities["semanticTokensProvider"]
                if "legend" in provider:
                    self.semantic_token_legend = provider["legend"]
                    logger.log(f"Semantic tokens legend: {len(provider['legend'].get('tokenTypes', []))} types")
        
        self._send_notification("initialized", {})
        logger.log("Sent 'initialized' notification")
        
        self.is_initialized = True
        self.serverInitialized.emit()
        logger.log("Server is now initialized and ready")
    
    def _handle_diagnostics(self, params):
        diagnostics = params.get("diagnostics", [])
        self.diagnosticsReceived.emit(diagnostics)
    
    def _handle_completion(self, result):
        if result:
            items = result if isinstance(result, list) else result.get("items", [])
            logger.log(f"Completion items received: {len(items)}")
            self.completionReceived.emit(items)
        else:
            self.completionReceived.emit([])
    
    def _handle_hover(self, result):
        if result and "contents" in result:
            contents = result["contents"]
            if isinstance(contents, dict):
                text = contents.get("value", "")
            elif isinstance(contents, str):
                text = contents
            else:
                text = str(contents)
            logger.log(f"Hover info received: {len(text)} chars")
            self.hoverReceived.emit(text)
    
    def _handle_definition(self, result):
        if result:
            if isinstance(result, list) and len(result) > 0:
                result = result[0]
            if isinstance(result, dict):
                uri = result.get("uri", "")
                range_data = result.get("range", {})
                start = range_data.get("start", {})
                line = start.get("line", 0)
                char = start.get("character", 0)
                path = uri.replace("file://", "")
                logger.log(f"Definition found: {path}:{line}:{char}")
                self.definitionReceived.emit(path, line, char)
    
    def _handle_semantic_tokens(self, result):
        if result and "data" in result:
            token_count = len(result["data"]) // 5
            logger.log(f"Semantic tokens received: {token_count} tokens")
            self.semanticTokensReceived.emit({
                "legend": self.semantic_token_legend,
                "data": result["data"]
            })
    
    def did_open(self, uri, language_id, text):
        params = {
            "textDocument": {
                "uri": uri,
                "languageId": language_id,
                "version": 1,
                "text": text
            }
        }
        self._send_notification("textDocument/didOpen", params)
        logger.log(f"Document opened: {uri}")
    
    def did_change(self, uri, version, text):
        params = {
            "textDocument": {"uri": uri, "version": version},
            "contentChanges": [{"text": text}]
        }
        self._send_notification("textDocument/didChange", params)
        logger.log(f"Document changed: version={version}")
    
    def completion(self, uri, line, character):
        params = {
            "textDocument": {"uri": uri},
            "position": {"line": line, "character": character}
        }
        msg_id = self._send_request("textDocument/completion", params)
        self.pending_requests[msg_id] = "textDocument/completion"
    
    def hover(self, uri, line, character):
        params = {
            "textDocument": {"uri": uri},
            "position": {"line": line, "character": character}
        }
        msg_id = self._send_request("textDocument/hover", params)
        self.pending_requests[msg_id] = "textDocument/hover"
    
    def definition(self, uri, line, character):
        params = {
            "textDocument": {"uri": uri},
            "position": {"line": line, "character": character}
        }
        msg_id = self._send_request("textDocument/definition", params)
        self.pending_requests[msg_id] = "textDocument/definition"
    
    def semantic_tokens(self, uri):
        params = {"textDocument": {"uri": uri}}
        msg_id = self._send_request("textDocument/semanticTokens/full", params)
        self.pending_requests[msg_id] = "textDocument/semanticTokens/full"
    
    def shutdown(self):
        logger.log("Shutting down LSP client")
        if self.process.state() == QProcess.ProcessState.Running:
            self._send_request("shutdown", {})
            self._send_notification("exit", {})
            if not self.process.waitForFinished(500):
                self.process.kill()
                self.process.waitForFinished(100)
        logger.log("LSP client shutdown complete")


# ============================================================================
# Keybinding Presets
# ============================================================================

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
            'cut': 'Ctrl+X',
            'copy': 'Ctrl+C',
            'paste': 'Ctrl+V',
            'undo': 'Ctrl+Z',
            'redo': 'Ctrl+Y',
            'select_all': 'Ctrl+A',
            'completion': 'Ctrl+Space',
            'goto_definition': 'F12',
            'save': 'Ctrl+S',
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
            'mark_set': 'Ctrl+Space',
            'cancel': 'Ctrl+G',
            'completion': 'Alt+/',
            'goto_definition': 'Alt+.',
            'save': 'Ctrl+X,Ctrl+S',
            'zoom_in': 'Ctrl+=',
            'zoom_out': 'Ctrl+-',
            'zoom_reset': 'Ctrl+0'
        }


# ============================================================================
# Line Number Area
# ============================================================================

class LineNumberArea(QtWidgets.QWidget):
    def __init__(self, editor):
        super().__init__(editor)
        self.code_editor = editor

    def sizeHint(self):
        return QtCore.QSize(self.code_editor.line_number_area_width(), 0)

    def paintEvent(self, event):
        self.code_editor.line_number_area_paint_event(event)


# ============================================================================
# Semantic Highlighter
# ============================================================================

class SemanticHighlighter(QtCore.QObject):
    """Syntax highlighter based on LSP semantic tokens"""
    
    def __init__(self, document):
        super().__init__()
        self.document = document
        self.token_colors = {
            "namespace": QtGui.QColor("#4EC9B0"),
            "type": QtGui.QColor("#4EC9B0"),
            "class": QtGui.QColor("#4EC9B0"),
            "enum": QtGui.QColor("#4EC9B0"),
            "interface": QtGui.QColor("#4EC9B0"),
            "struct": QtGui.QColor("#4EC9B0"),
            "typeParameter": QtGui.QColor("#4EC9B0"),
            "parameter": QtGui.QColor("#9CDCFE"),
            "variable": QtGui.QColor("#9CDCFE"),
            "property": QtGui.QColor("#9CDCFE"),
            "enumMember": QtGui.QColor("#4FC1FF"),
            "function": QtGui.QColor("#DCDCAA"),
            "method": QtGui.QColor("#DCDCAA"),
            "macro": QtGui.QColor("#569CD6"),
            "keyword": QtGui.QColor("#C586C0"),
            "modifier": QtGui.QColor("#569CD6"),
            "comment": QtGui.QColor("#6A9955"),
            "string": QtGui.QColor("#CE9178"),
            "number": QtGui.QColor("#B5CEA8"),
            "regexp": QtGui.QColor("#D16969"),
            "operator": QtGui.QColor("#D4D4D4"),
        }
    
    def apply_highlighting(self, legend, data):
        if not legend or not data:
            return
        
        token_types = legend.get("tokenTypes", [])
        logger.log(f"Applying highlighting with {len(token_types)} token types")
        
        tokens = []
        current_line = 0
        current_start = 0
        
        for i in range(0, len(data), 5):
            delta_line = data[i]
            delta_start = data[i + 1]
            length = data[i + 2]
            token_type_idx = data[i + 3]
            
            current_line += delta_line
            if delta_line == 0:
                current_start += delta_start
            else:
                current_start = delta_start
            
            if token_type_idx < len(token_types):
                token_type = token_types[token_type_idx]
                tokens.append({
                    "line": current_line,
                    "start": current_start,
                    "length": length,
                    "type": token_type
                })
        
        cursor = QtGui.QTextCursor(self.document)
        cursor.beginEditBlock()
        
        cursor.select(QtGui.QTextCursor.SelectionType.Document)
        fmt = QtGui.QTextCharFormat()
        fmt.setForeground(QtGui.QColor("#D4D4D4"))
        cursor.setCharFormat(fmt)
        
        for token in tokens:
            color = self.token_colors.get(token["type"], QtGui.QColor("#D4D4D4"))
            
            cursor.movePosition(QtGui.QTextCursor.MoveOperation.Start)
            cursor.movePosition(QtGui.QTextCursor.MoveOperation.Down,
                              QtGui.QTextCursor.MoveMode.MoveAnchor, token["line"])
            cursor.movePosition(QtGui.QTextCursor.MoveOperation.StartOfLine)
            cursor.movePosition(QtGui.QTextCursor.MoveOperation.Right,
                              QtGui.QTextCursor.MoveMode.MoveAnchor, token["start"])
            cursor.movePosition(QtGui.QTextCursor.MoveOperation.Right,
                              QtGui.QTextCursor.MoveMode.KeepAnchor, token["length"])
            
            fmt = QtGui.QTextCharFormat()
            fmt.setForeground(color)
            
            if token["type"] == "keyword":
                fmt.setFontWeight(QtGui.QFont.Weight.Bold)
            elif token["type"] == "comment":
                fmt.setFontItalic(True)
            
            cursor.setCharFormat(fmt)
        
        cursor.endEditBlock()


# ============================================================================
# Completion Popup
# ============================================================================

class CompletionPopup(QtWidgets.QListWidget):
    """Completion popup that doesn't steal focus from editor"""
    itemSelected = QtCore.Signal(str, str, int)
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowFlags(QtCore.Qt.WindowType.ToolTip | QtCore.Qt.WindowType.FramelessWindowHint)
        self.setAttribute(QtCore.Qt.WA_ShowWithoutActivating, True)
        self.setFocusPolicy(QtCore.Qt.NoFocus)
        self.setMaximumHeight(200)
        self.setMinimumWidth(400)
        self.completion_items = []
        self.filter_text = ""
        
        self.itemClicked.connect(self._on_item_clicked)
        
        self.setStyleSheet("""
            QListWidget {
                background-color: #252526;
                color: #CCCCCC;
                border: 1px solid #454545;
                font-family: Consolas, Monaco, monospace;
                font-size: 11px;
            }
            QListWidget::item {
                padding: 4px;
            }
            QListWidget::item:selected {
                background-color: #094771;
            }
            QListWidget::item:hover {
                background-color: #2A2D2E;
            }
        """)
    
    def show_completions(self, completions, position, prefix=""):
        """Show completion popup at position with optional prefix for filtering"""
        self.clear()
        self.filter_text = prefix.lower()
        
        def sort_key(item):
            label = item.get("label", "").lower()
            if label == self.filter_text:
                return (0, label)
            elif label.startswith(self.filter_text):
                return (1, len(label), label)
            elif self.filter_text in label:
                return (2, label)
            else:
                return (3, label)
        
        self.completion_items = sorted(completions, key=sort_key)
        
        for item in self.completion_items:
            label = item.get("label", "")
            kind = item.get("kind", "")
            detail = item.get("detail", "")
            
            kind_icons = {
                1: "📝", 2: "ƒ", 3: "ƒ", 4: "⚙", 5: "□",
                6: "x", 7: "C", 8: "I", 9: "M", 10: "P",
            }
            icon = kind_icons.get(kind, "•")
            
            display_text = f"{icon} {label}"
            if detail:
                display_text += f"  {detail}"
            
            list_item = QtWidgets.QListWidgetItem(display_text)
            list_item.setData(QtCore.Qt.ItemDataRole.UserRole, item)
            self.addItem(list_item)
        
        if self.count() > 0:
            self.setCurrentRow(0)
            self.move(position)
            self.show()
            logger.log(f"Showing {len(self.completion_items)} completions")
    
    def filter_items(self, text):
        """Filter visible items based on text"""
        self.filter_text = text.lower()
        visible_items = []
        
        for i in range(self.count()):
            item = self.item(i)
            comp_data = item.data(QtCore.Qt.ItemDataRole.UserRole)
            if comp_data:
                label = comp_data.get("label", "").lower()
                if label == self.filter_text:
                    priority = 0
                elif label.startswith(self.filter_text):
                    priority = (1, len(label))
                elif self.filter_text in label:
                    priority = (2, label)
                else:
                    priority = None
                
                if priority is not None:
                    item.setHidden(False)
                    visible_items.append((priority, i))
                else:
                    item.setHidden(True)
        
        visible_items.sort(key=lambda x: x[0])
        
        if visible_items:
            best_index = visible_items[0][1]
            self.setCurrentRow(best_index)
        
        logger.log(f"Filtered to {len(visible_items)} items")
    
    def _on_item_clicked(self, item):
        """Handle item click"""
        comp_data = item.data(QtCore.Qt.ItemDataRole.UserRole)
        if comp_data:
            self._select_completion(comp_data)
    
    def _select_completion(self, comp):
        """Select a completion item"""
        label = comp.get("label", "")
        insert_text = comp.get("insertText", label)
        text_edit = comp.get("textEdit")
        prefix_len = 0
        if text_edit:
            range_data = text_edit.get("range", {})
            start = range_data.get("start", {})
            end = range_data.get("end", {})
            if start and end:
                prefix_len = end.get("character", 0) - start.get("character", 0)
        
        self.itemSelected.emit(label, insert_text, prefix_len)
    
    def move_selection_up(self):
        """Move selection up in the list"""
        current = self.currentRow()
        for i in range(current - 1, -1, -1):
            if not self.item(i).isHidden():
                self.setCurrentRow(i)
                return True
        return False
    
    def move_selection_down(self):
        """Move selection down in the list"""
        current = self.currentRow()
        for i in range(current + 1, self.count()):
            if not self.item(i).isHidden():
                self.setCurrentRow(i)
                return True
        return False
    
    def select_current(self):
        """Select the currently highlighted item"""
        current_item = self.currentItem()
        if current_item and not current_item.isHidden():
            comp_data = current_item.data(QtCore.Qt.ItemDataRole.UserRole)
            if comp_data:
                self._select_completion(comp_data)
                return True
        return False


# ============================================================================
# Code Editor
# ============================================================================

class LSPCodeEditor(QtWidgets.QPlainTextEdit):
    """Code editor with LSP support and modern UI"""
    
    font_size_changed = QtCore.Signal(int)
    diagnosticsChanged = QtCore.Signal(list)
    
    _untitled_counter = 0
    
    def __init__(self, lsp_client, file_path, keybindings=None):
        super().__init__()
        
        self.lsp_client = lsp_client
        self.file_path = file_path
        self.is_virtual = False
        
        # Generate appropriate URI
        if file_path and os.path.exists(file_path):
            self.uri = f"file://{os.path.abspath(file_path)}"
            self.is_virtual = False
        else:
            LSPCodeEditor._untitled_counter += 1
            ext = os.path.splitext(file_path)[1] if file_path else ".lean"
            self.uri = f"untitled:Untitled-{LSPCodeEditor._untitled_counter}{ext}"
            self.is_virtual = True
            logger.log(f"Created virtual document: {self.uri}")
        
        self.version = 1
        
        # Line numbers
        self.line_number_area = LineNumberArea(self)
        
        # Diagnostics
        self.current_diagnostics = []
        self.diagnostic_ranges = {}
        
        # Completion
        self.completion_popup = CompletionPopup(self)
        self.completion_prefix = ""
        self.completion_start_pos = 0
        
        # Keybindings
        self.keybindings = keybindings or KeybindingPresets.standard()
        self.is_emacs_mode = keybindings and keybindings.get('move_line_start') == 'Ctrl+A'
        
        # Font size
        self.default_font_size = 12
        self.current_font_size = self.default_font_size
        
        # Emacs features
        self.kill_ring = []
        self.kill_ring_index = 0
        self.max_kill_ring_size = 20
        self.mark_position = None
        self.last_action_was_paste_cycle = False
        
        # Custom tooltip
        self.tooltip_widget = None
        
        # Semantic highlighting
        self.semantic_highlighter = SemanticHighlighter(self.document())
        
        # Timers
        self.change_timer = QtCore.QTimer()
        self.change_timer.setSingleShot(True)
        self.change_timer.timeout.connect(self.on_text_changed)
        
        self.highlight_timer = QtCore.QTimer()
        self.highlight_timer.setSingleShot(True)
        self.highlight_timer.timeout.connect(self.request_semantic_tokens)
        
        self.hover_timer = QtCore.QTimer()
        self.hover_timer.setSingleShot(True)
        self.hover_timer.timeout.connect(self.request_hover)
        self.last_hover_pos = None
        
        # Setup
        self.setup_editor()
        self.setup_keybindings()
        
        # Connect signals
        self.blockCountChanged.connect(self.update_line_number_area_width)
        self.updateRequest.connect(self.update_line_number_area)
        self.textChanged.connect(lambda: self.change_timer.start(500))
        
        self.lsp_client.diagnosticsReceived.connect(self.handle_diagnostics)
        self.lsp_client.completionReceived.connect(self.handle_completion)
        self.lsp_client.hoverReceived.connect(self.show_hover_tooltip)
        self.lsp_client.definitionReceived.connect(self.goto_definition)
        self.lsp_client.semanticTokensReceived.connect(self.handle_semantic_tokens)
        self.lsp_client.serverInitialized.connect(self.on_server_initialized)
        
        self.completion_popup.itemSelected.connect(self.insert_completion)
        
        # Store reference to parent window for goto_definition
        self.parent_window = None
        self.workspace_path = ""
        
        self.update_line_number_area_width(0)
        
        logger.log("Waiting for server initialization before opening document")
    
    def setup_editor(self):
        """Setup editor appearance"""
        self.update_font_size(self.default_font_size)
        self.setTabStopDistance(40)
        
        self.setStyleSheet("""
            QPlainTextEdit {
                background-color: #1e1e1e;
                color: #d4d4d4;
                border: none;
                selection-background-color: #264f78;
            }
        """)
        
        self.setMouseTracking(True)
        self.viewport().setMouseTracking(True)
    
    def setup_keybindings(self):
        """Setup keyboard shortcuts"""
        kb = self.keybindings
        
        if kb.get('zoom_in'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['zoom_in']), self, self.zoom_in)
        if kb.get('zoom_out'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['zoom_out']), self, self.zoom_out)
        if kb.get('zoom_reset'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['zoom_reset']), self, self.zoom_reset)
        
        if kb.get('move_word_left'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['move_word_left']), self, 
                          lambda: self.move_cursor(QtGui.QTextCursor.WordLeft))
        if kb.get('move_word_right'):
            QtGui.QShortcut(QtGui.QKeySequence(kb['move_word_right']), self,
                          lambda: self.move_cursor(QtGui.QTextCursor.WordRight))
        
        if kb.get('move_doc_start') and kb['move_doc_start'] != 'Ctrl+Home':
            QtGui.QShortcut(QtGui.QKeySequence(kb['move_doc_start']), self,
                          lambda: self.move_cursor(QtGui.QTextCursor.Start))
        if kb.get('move_doc_end') and kb['move_doc_end'] != 'Ctrl+End':
            QtGui.QShortcut(QtGui.QKeySequence(kb['move_doc_end']), self,
                          lambda: self.move_cursor(QtGui.QTextCursor.End))
        
        if self.is_emacs_mode:
            if kb.get('move_up'):
                QtGui.QShortcut(QtGui.QKeySequence(kb['move_up']), self,
                              lambda: self.move_cursor(QtGui.QTextCursor.Up))
            if kb.get('move_down'):
                QtGui.QShortcut(QtGui.QKeySequence(kb['move_down']), self,
                              lambda: self.move_cursor(QtGui.QTextCursor.Down))
            if kb.get('move_left'):
                QtGui.QShortcut(QtGui.QKeySequence(kb['move_left']), self,
                              lambda: self.move_cursor(QtGui.QTextCursor.Left))
            if kb.get('move_right'):
                QtGui.QShortcut(QtGui.QKeySequence(kb['move_right']), self,
                              lambda: self.move_cursor(QtGui.QTextCursor.Right))
            if kb.get('move_line_start'):
                QtGui.QShortcut(QtGui.QKeySequence(kb['move_line_start']), self,
                              lambda: self.move_cursor(QtGui.QTextCursor.StartOfLine))
            if kb.get('move_line_end'):
                QtGui.QShortcut(QtGui.QKeySequence(kb['move_line_end']), self,
                              lambda: self.move_cursor(QtGui.QTextCursor.EndOfLine))
            
            if kb.get('delete_char'):
                QtGui.QShortcut(QtGui.QKeySequence(kb['delete_char']), self, self.delete_char)
            if kb.get('delete_word'):
                QtGui.QShortcut(QtGui.QKeySequence(kb['delete_word']), self, self.delete_word)
            if kb.get('backspace_word'):
                QtGui.QShortcut(QtGui.QKeySequence(kb['backspace_word']), self, self.backspace_word)
            if kb.get('kill_line'):
                QtGui.QShortcut(QtGui.QKeySequence(kb['kill_line']), self, self.kill_line)
            
            if kb.get('cut'):
                QtGui.QShortcut(QtGui.QKeySequence(kb['cut']), self, self.kill_region)
            if kb.get('copy'):
                QtGui.QShortcut(QtGui.QKeySequence(kb['copy']), self, self.copy_region)
            if kb.get('paste'):
                QtGui.QShortcut(QtGui.QKeySequence(kb['paste']), self, self.yank)
            if kb.get('paste_cycle'):
                QtGui.QShortcut(QtGui.QKeySequence(kb['paste_cycle']), self, self.yank_pop)
            
            if kb.get('mark_set'):
                QtGui.QShortcut(QtGui.QKeySequence(kb['mark_set']), self, self.set_mark)
            if kb.get('cancel'):
                QtGui.QShortcut(QtGui.QKeySequence(kb['cancel']), self, self.cancel_operation)
    
    def delete_char(self):
        cursor = self.textCursor()
        if not cursor.hasSelection():
            cursor.deleteChar()
        else:
            cursor.removeSelectedText()
        self.setTextCursor(cursor)
    
    def delete_word(self):
        cursor = self.textCursor()
        cursor.movePosition(QtGui.QTextCursor.EndOfWord, QtGui.QTextCursor.KeepAnchor)
        text = cursor.selectedText()
        if text:
            self.add_to_kill_ring(text)
        cursor.removeSelectedText()
        self.setTextCursor(cursor)
    
    def backspace_word(self):
        cursor = self.textCursor()
        cursor.movePosition(QtGui.QTextCursor.StartOfWord, QtGui.QTextCursor.KeepAnchor)
        text = cursor.selectedText()
        if text:
            self.add_to_kill_ring(text)
        cursor.removeSelectedText()
        self.setTextCursor(cursor)
    
    def kill_region(self):
        cursor = self.textCursor()
        if cursor.hasSelection():
            text = cursor.selectedText()
            self.add_to_kill_ring(text)
            cursor.removeSelectedText()
            self.setTextCursor(cursor)
            self.mark_position = None
    
    def copy_region(self):
        cursor = self.textCursor()
        if cursor.hasSelection():
            text = cursor.selectedText()
            self.add_to_kill_ring(text)
            self.mark_position = None
    
    def yank(self):
        if self.kill_ring:
            cursor = self.textCursor()
            cursor.insertText(self.kill_ring[self.kill_ring_index])
            self.setTextCursor(cursor)
            self.last_action_was_paste_cycle = True
    
    def yank_pop(self):
        if not self.kill_ring or not hasattr(self, 'last_action_was_paste_cycle') or not self.last_action_was_paste_cycle:
            return
        
        cursor = self.textCursor()
        last_text = self.kill_ring[self.kill_ring_index]
        
        for _ in range(len(last_text)):
            cursor.deletePreviousChar()
        
        self.kill_ring_index = (self.kill_ring_index - 1) % len(self.kill_ring)
        
        cursor.insertText(self.kill_ring[self.kill_ring_index])
        self.setTextCursor(cursor)
    
    def set_keybindings(self, keybindings):
        self.keybindings = keybindings
        self.is_emacs_mode = keybindings.get('move_line_start') == 'Ctrl+A'
        
        for child in self.children():
            if isinstance(child, QtGui.QShortcut):
                child.deleteLater()
        
        self.setup_keybindings()
        logger.log(f"Keybindings changed to: {'Emacs' if self.is_emacs_mode else 'Standard'}")
    
    def update_font_size(self, size):
        self.current_font_size = size
        
        font = QtGui.QFont("Cascadia Code", size)
        if not font.exactMatch():
            font = QtGui.QFont("Consolas", size)
        
        font.setWeight(QtGui.QFont.Weight.Medium)
        font.setStyleHint(QtGui.QFont.Monospace)
        font.setFixedPitch(True)
        font.setStyleStrategy(
            QtGui.QFont.StyleStrategy.PreferAntialias |
            QtGui.QFont.StyleStrategy.PreferQuality
        )
        
        self.setFont(font)
        self.font_size_changed.emit(size)
    
    def zoom_in(self):
        self.update_font_size(min(self.current_font_size + 1, 32))
    
    def zoom_out(self):
        self.update_font_size(max(self.current_font_size - 1, 6))

    def zoom_reset(self):
        self.update_font_size(self.default_font_size)
    
    def move_cursor(self, operation):
        cursor = self.textCursor()
        
        if self.mark_position is not None:
            current_pos = cursor.position()
            new_cursor = QtGui.QTextCursor(cursor)
            new_cursor.movePosition(operation, QtGui.QTextCursor.MoveAnchor)
            new_pos = new_cursor.position()
            
            final_cursor = QtGui.QTextCursor(self.document())
            final_cursor.setPosition(self.mark_position, QtGui.QTextCursor.MoveAnchor)
            final_cursor.setPosition(new_pos, QtGui.QTextCursor.KeepAnchor)
            
            self.setTextCursor(final_cursor)
        else:
            cursor.movePosition(operation, QtGui.QTextCursor.MoveAnchor)
            self.setTextCursor(cursor)
    
    def set_mark(self):
        if self.mark_position is None:
            self.mark_position = self.textCursor().position()
            self.viewport().update()
        else:
            self.mark_position = None
            cursor = self.textCursor()
            cursor.clearSelection()
            self.setTextCursor(cursor)
    
    def cancel_operation(self):
        self.mark_position = None
        cursor = self.textCursor()
        cursor.clearSelection()
        self.setTextCursor(cursor)
        self.hide_custom_tooltip()
        if self.completion_popup.isVisible():
            self.completion_popup.hide()
    
    def kill_line(self):
        cursor = self.textCursor()
        cursor.movePosition(QtGui.QTextCursor.EndOfLine, QtGui.QTextCursor.KeepAnchor)
        text = cursor.selectedText()
        
        if not text:
            cursor.deleteChar()
            self.add_to_kill_ring('\n')
        else:
            self.add_to_kill_ring(text)
            cursor.removeSelectedText()
        
        self.setTextCursor(cursor)
    
    def add_to_kill_ring(self, text):
        if text:
            self.kill_ring.append(text)
            if len(self.kill_ring) > self.max_kill_ring_size:
                self.kill_ring.pop(0)
            self.kill_ring_index = len(self.kill_ring) - 1
    
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
        self.line_number_area.setGeometry(QtCore.QRect(cr.left(), cr.top(), 
                                                       self.line_number_area_width(), cr.height()))
    
    def line_number_area_paint_event(self, event):
        painter = QtGui.QPainter(self.line_number_area)
        
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
    
    def on_server_initialized(self):
        logger.log("Server initialized, now opening document")
        text = self.toPlainText()
        self.lsp_client.did_open(self.uri, "lean4", text)
        QtCore.QTimer.singleShot(1000, self.request_semantic_tokens)
    
    def on_text_changed(self):
        if not self.lsp_client.is_initialized:
            return
        
        if self.completion_popup.isVisible():
            cursor = self.textCursor()
            current_pos = cursor.position()
            
            cursor.setPosition(self.completion_start_pos)
            cursor.setPosition(current_pos, QtGui.QTextCursor.MoveMode.KeepAnchor)
            current_text = cursor.selectedText()
            
            self.completion_popup.filter_items(current_text)
            
            if current_pos < self.completion_start_pos:
                self.completion_popup.hide()
            
            return
        
        self.version += 1
        text = self.toPlainText()
        self.lsp_client.did_change(self.uri, self.version, text)
        self.highlight_timer.start(800)
    
    def request_semantic_tokens(self):
        if self.lsp_client.is_initialized:
            self.lsp_client.semantic_tokens(self.uri)
    
    def request_hover(self):
        if self.last_hover_pos and self.lsp_client.is_initialized:
            cursor = self.cursorForPosition(self.last_hover_pos)
            line = cursor.blockNumber()
            char = cursor.columnNumber()
            self.lsp_client.hover(self.uri, line, char)
    
    def trigger_completion(self):
        cursor = self.textCursor()
        line = cursor.blockNumber()
        char = cursor.columnNumber()
        
        text_cursor = self.textCursor()
        text_cursor.movePosition(QtGui.QTextCursor.MoveOperation.StartOfWord, 
                                QtGui.QTextCursor.MoveMode.MoveAnchor)
        self.completion_start_pos = text_cursor.position()
        
        text_cursor.setPosition(cursor.position(), QtGui.QTextCursor.MoveMode.KeepAnchor)
        self.completion_prefix = text_cursor.selectedText()
        
        logger.log(f"Requesting completion at {line}:{char}, prefix='{self.completion_prefix}'")
        self.lsp_client.completion(self.uri, line, char)
    
    def trigger_goto_definition(self):
        cursor = self.textCursor()
        line = cursor.blockNumber()
        char = cursor.columnNumber()
        logger.log(f"Requesting definition at {line}:{char}")
        self.lsp_client.definition(self.uri, line, char)
    
    def handle_completion(self, items):
        if items:
            cursor = self.textCursor()
            cursor_rect = self.cursorRect(cursor)
            global_pos = self.mapToGlobal(cursor_rect.bottomLeft())
            self.completion_popup.show_completions(items, global_pos, self.completion_prefix)
        else:
            logger.log("No completion items to show")
    
    def insert_completion(self, label, insert_text, prefix_len):
        cursor = self.textCursor()
        
        current_pos = cursor.position()
        cursor.setPosition(self.completion_start_pos)
        cursor.setPosition(current_pos, QtGui.QTextCursor.MoveMode.KeepAnchor)
        cursor.removeSelectedText()
        
        cursor.insertText(insert_text)
        self.completion_popup.hide()
        logger.log(f"Inserted completion: {label}")
        
        self.version += 1
        text = self.toPlainText()
        self.lsp_client.did_change(self.uri, self.version, text)
        self.highlight_timer.start(800)
    
    def show_hover_tooltip(self, text):
        if text:
            text = re.sub(r'```[\w]*\n', '', text)
            text = re.sub(r'```', '', text)
            text = text.strip()
            
            if text:
                QtWidgets.QToolTip.showText(QtGui.QCursor.pos(), text, self)
    
    def goto_definition(self, file_path, line, character):
        logger.log(f"Go to definition: {file_path}:{line}:{character}")
        
        editor_cmd = None
        
        if hasattr(self, 'parent_window') and self.parent_window:
            try:
                parent_window = self.parent_window
                if hasattr(parent_window, 'node') and parent_window.node:
                    node = parent_window.node
                    
                    if node.parm("editor"):
                        editor_cmd = node.parm("editor").eval()
                        logger.log(f"Got editor command from parameter: {editor_cmd}")
            except Exception as e:
                logger.log(f"Could not get editor parameter: {e}", "WARNING")
        
        if not editor_cmd:
            import hou
            hou.ui.displayMessage(
                f"No editor specified!\n\nDefinition at:\n{file_path}\nLine {line+1}, Column {character+1}\n\nPlease set the 'editor' parameter on your node (e.g., 'code', 'emacs', 'vim')",
                severity=hou.severityType.Warning,
                title="Go to Definition"
            )
            return
        
        import subprocess
        
        editor_lower = editor_cmd.lower().strip()
        
        try:
            if 'code' in editor_lower or 'vscode' in editor_lower:
                cmd = [editor_cmd, '--goto', f"{file_path}:{line+1}:{character+1}"]
                logger.log(f"Opening in VS Code: {' '.join(cmd)}")
                subprocess.Popen(cmd)
                
            elif 'emacs' in editor_lower:
                cmd = [editor_cmd, f"+{line+1}:{character+1}", file_path]
                logger.log(f"Opening in Emacs: {' '.join(cmd)}")
                subprocess.Popen(cmd)
                
            elif 'vim' in editor_lower or 'nvim' in editor_lower:
                cmd = [editor_cmd, f"+call cursor({line+1},{character+1})", file_path]
                logger.log(f"Opening in Vim: {' '.join(cmd)}")
                subprocess.Popen(cmd)
                
            elif 'subl' in editor_lower or 'sublime' in editor_lower:
                cmd = [editor_cmd, f"{file_path}:{line+1}:{character+1}"]
                logger.log(f"Opening in Sublime: {' '.join(cmd)}")
                subprocess.Popen(cmd)
                
            elif 'atom' in editor_lower:
                cmd = [editor_cmd, f"{file_path}:{line+1}:{character+1}"]
                logger.log(f"Opening in Atom: {' '.join(cmd)}")
                subprocess.Popen(cmd)
                
            elif 'kate' in editor_lower:
                cmd = [editor_cmd, '--line', str(line+1), '--column', str(character+1), file_path]
                logger.log(f"Opening in Kate: {' '.join(cmd)}")
                subprocess.Popen(cmd)
                
            elif 'gedit' in editor_lower:
                cmd = [editor_cmd, f"+{line+1}", file_path]
                logger.log(f"Opening in gedit: {' '.join(cmd)}")
                subprocess.Popen(cmd)
                
            else:
                cmd = [editor_cmd, file_path]
                logger.log(f"Opening in unknown editor (generic): {' '.join(cmd)}")
                subprocess.Popen(cmd)
                
                import hou
                hou.ui.setStatusMessage(
                    f"Opened {os.path.basename(file_path)} - navigate to line {line+1}, column {character+1}",
                    hou.severityType.Message
                )
            
            import hou
            hou.ui.setStatusMessage(
                f"Opened in {editor_cmd}: {os.path.basename(file_path)}:{line+1}:{character+1}",
                hou.severityType.Message
            )
            
        except Exception as e:
            logger.log(f"Error opening external editor: {e}", "ERROR")
            import hou
            hou.ui.displayMessage(
                f"Could not open editor '{editor_cmd}':\n{str(e)}\n\nFile: {file_path}\nLine: {line+1}, Column: {character+1}",
                severity=hou.severityType.Error,
                title="Go to Definition Error"
            )
    
    def handle_diagnostics(self, diagnostics):
        self.current_diagnostics = diagnostics
        self.diagnosticsChanged.emit(diagnostics)
        self.diagnostic_ranges.clear()
        
        extra_selections = []
        
        for diag in diagnostics:
            severity = diag.get("severity", 1)
            range_data = diag.get("range", {})
            message = diag.get("message", "")
            start = range_data.get("start", {})
            end = range_data.get("end", {})
            
            cursor = QtGui.QTextCursor(self.document())
            cursor.movePosition(QtGui.QTextCursor.MoveOperation.Start)
            cursor.movePosition(QtGui.QTextCursor.MoveOperation.Down,
                              QtGui.QTextCursor.MoveMode.MoveAnchor, start.get("line", 0))
            cursor.movePosition(QtGui.QTextCursor.MoveOperation.Right,
                              QtGui.QTextCursor.MoveMode.MoveAnchor, start.get("character", 0))
            
            start_pos = cursor.position()
            
            cursor.movePosition(QtGui.QTextCursor.MoveOperation.Start)
            cursor.movePosition(QtGui.QTextCursor.MoveOperation.Down,
                              QtGui.QTextCursor.MoveMode.MoveAnchor, end.get("line", 0))
            cursor.movePosition(QtGui.QTextCursor.MoveOperation.Right,
                              QtGui.QTextCursor.MoveMode.MoveAnchor, end.get("character", 0))
            
            end_pos = cursor.position()
            length = end_pos - start_pos
            
            severity_name = {1: 'error', 2: 'warning', 3: 'info', 4: 'hint'}.get(severity, 'info')
            self.diagnostic_ranges[(start_pos, length)] = (severity_name, message)
            
            cursor.setPosition(start_pos)
            cursor.setPosition(end_pos, QtGui.QTextCursor.MoveMode.KeepAnchor)
            
            selection = QtWidgets.QTextEdit.ExtraSelection()
            selection.cursor = cursor
            
            if severity == 1:
                selection.format.setUnderlineColor(QtGui.QColor("#f48771"))
                selection.format.setBackground(QtGui.QColor(244, 135, 113, 15))
            elif severity == 2:
                selection.format.setUnderlineColor(QtGui.QColor("#ffcc00"))
                selection.format.setBackground(QtGui.QColor(255, 204, 0, 15))
            else:
                selection.format.setUnderlineColor(QtGui.QColor("#4fc3f7"))
                selection.format.setBackground(QtGui.QColor(79, 195, 247, 15))
            
            selection.format.setUnderlineStyle(QtGui.QTextCharFormat.UnderlineStyle.WaveUnderline)
            extra_selections.append(selection)
        
        self.setExtraSelections(extra_selections)
    
    def handle_semantic_tokens(self, result):
        legend = result.get("legend")
        data = result.get("data")
        if legend and data:
            self.semantic_highlighter.apply_highlighting(legend, data)
    
    def show_custom_tooltip(self, severity, text, pos):
        if self.tooltip_widget is None:
            self.tooltip_widget = QtWidgets.QLabel(None, QtCore.Qt.ToolTip | QtCore.Qt.FramelessWindowHint)
            self.tooltip_widget.setTextFormat(QtCore.Qt.PlainText)
            self.tooltip_widget.setWordWrap(False)
            self.tooltip_widget.setAttribute(QtCore.Qt.WA_ShowWithoutActivating, True)
        
        border_colors = {
            'error': '#f48771',
            'warning': '#ffcc00',
            'info': '#4fc3f7'
        }
        border_color = border_colors.get(severity, '#007acc')
        
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
        
        tooltip_pos = QtCore.QPoint(pos.x() + 15, pos.y() + 15)
        self.tooltip_widget.move(tooltip_pos)
        self.tooltip_widget.raise_()
        self.tooltip_widget.show()
    
    def hide_custom_tooltip(self):
        if self.tooltip_widget and self.tooltip_widget.isVisible():
            self.tooltip_widget.hide()
    
    def save_file(self):
        if self.is_virtual:
            logger.log(f"Virtual file {self.uri} - content kept in memory only")
            import hou
            hou.ui.setStatusMessage("Virtual file - use Apply to save to parameter", 
                                   hou.severityType.Message)
            return True
        
        if not self.file_path:
            logger.log("No file path to save to", "WARNING")
            return False
            
        try:
            with open(self.file_path, 'w') as f:
                f.write(self.toPlainText())
            logger.log(f"File saved: {self.file_path}")
            import hou
            hou.ui.setStatusMessage(f"Saved {os.path.basename(self.file_path)}", 
                                   hou.severityType.Message)
            return True
        except Exception as e:
            logger.log(f"Error saving file: {e}", "ERROR")
            import hou
            hou.ui.displayMessage(f"Error saving file: {str(e)}")
            return False
    
    def keyPressEvent(self, event):
        is_yank = False
        
        if self.completion_popup.isVisible():
            if event.key() == QtCore.Qt.Key.Key_Up:
                self.completion_popup.move_selection_up()
                return
            elif event.key() == QtCore.Qt.Key.Key_Down:
                self.completion_popup.move_selection_down()
                return
            elif self.is_emacs_mode and event.key() == QtCore.Qt.Key_P and event.modifiers() == QtCore.Qt.ControlModifier:
                self.completion_popup.move_selection_up()
                return
            elif self.is_emacs_mode and event.key() == QtCore.Qt.Key_N and event.modifiers() == QtCore.Qt.ControlModifier:
                self.completion_popup.move_selection_down()
                return
            elif event.key() in (QtCore.Qt.Key.Key_Return, QtCore.Qt.Key.Key_Enter):
                if self.completion_popup.select_current():
                    return
                else:
                    self.completion_popup.hide()
                    super().keyPressEvent(event)
                    return
            elif event.key() == QtCore.Qt.Key.Key_Escape:
                self.completion_popup.hide()
                return
        
        if self.is_emacs_mode and event.key() == QtCore.Qt.Key_A and event.modifiers() == QtCore.Qt.ControlModifier:
            cursor = self.textCursor()
            cursor.clearSelection()
            cursor.movePosition(QtGui.QTextCursor.StartOfLine, QtGui.QTextCursor.MoveAnchor)
            self.setTextCursor(cursor)
            return
        
        if event.key() == QtCore.Qt.Key_Tab and event.modifiers() == QtCore.Qt.NoModifier:
            if not self.completion_popup.isVisible():
                cursor = self.textCursor()
                cursor.insertText("  ")
                return
        
        if event.key() == QtCore.Qt.Key_Space and event.modifiers() == QtCore.Qt.ControlModifier:
            if not self.is_emacs_mode:
                self.trigger_completion()
                return
        
        if self.is_emacs_mode and event.key() == QtCore.Qt.Key_Slash and event.modifiers() == QtCore.Qt.AltModifier:
            self.trigger_completion()
            return
        
        if event.key() == QtCore.Qt.Key.Key_F12:
            self.trigger_goto_definition()
            return
        
        if self.is_emacs_mode and event.key() == QtCore.Qt.Key_Period and event.modifiers() == QtCore.Qt.AltModifier:
            self.trigger_goto_definition()
            return
        
        if event.key() == QtCore.Qt.Key_S and event.modifiers() == QtCore.Qt.ControlModifier:
            self.save_file()
            return
        
        if event.key() == QtCore.Qt.Key_Y:
            if self.is_emacs_mode and event.modifiers() == QtCore.Qt.ControlModifier:
                is_yank = True
            elif event.modifiers() == QtCore.Qt.AltModifier:
                is_yank = True
        
        super().keyPressEvent(event)
        
        if not is_yank:
            self.last_action_was_paste_cycle = False
        
        if event.text() in ('.', ':') and not self.completion_popup.isVisible():
            QtCore.QTimer.singleShot(100, self.trigger_completion)
    
    def mouseMoveEvent(self, event):
        cursor = self.cursorForPosition(event.pos())
        position = cursor.position()
        
        tooltip_data = None
        for (start, length), (severity, message) in self.diagnostic_ranges.items():
            if start <= position < start + length:
                tooltip_data = (severity, message)
                break
        
        if tooltip_data:
            severity, message = tooltip_data
            self.show_custom_tooltip(severity, message, event.globalPos())
        else:
            self.hide_custom_tooltip()
        
        self.last_hover_pos = event.pos()
        if event.modifiers() == QtCore.Qt.KeyboardModifier.ControlModifier:
            self.hover_timer.start(300)
        else:
            self.hover_timer.stop()
        
        super().mouseMoveEvent(event)
    
    def leaveEvent(self, event):
        self.hide_custom_tooltip()
        super().leaveEvent(event)


# ============================================================================
# Info View (Diagnostics Panel)
# ============================================================================

class InfoView(QtWidgets.QWidget):
    """Info/Error view showing diagnostics"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.current_font_size = 12
        self.setup_ui()
    
    def setup_ui(self):
        layout = QtWidgets.QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        
        header = QtWidgets.QLabel("Diagnostics")
        header.setStyleSheet("font-weight: bold; padding: 4px; background-color: #2D2D30; color: #CCCCCC;")
        layout.addWidget(header)
        
        self.list_widget = QtWidgets.QListWidget()
        self.update_style()
        layout.addWidget(self.list_widget)
    
    def update_style(self):
        self.list_widget.setStyleSheet(f"""
            QListWidget {{
                background-color: #1E1E1E;
                color: #D4D4D4;
                border: none;
                font-family: Consolas, Monaco, monospace;
                font-size: {self.current_font_size}px;
            }}
            QListWidget::item {{
                padding: 4px;
                border-bottom: 1px solid #2D2D30;
            }}
        """)
    
    def set_font_size(self, size):
        self.current_font_size = size
        self.update_style()
    
    def update_diagnostics(self, diagnostics):
        self.list_widget.clear()
        
        for diag in diagnostics:
            severity = diag.get("severity", 1)
            message = diag.get("message", "")
            range_data = diag.get("range", {})
            start = range_data.get("start", {})
            line = start.get("line", 0) + 1
            char = start.get("character", 0) + 1
            
            if severity == 1:
                icon = "❌"
                color = "#F48771"
            elif severity == 2:
                icon = "⚠️"
                color = "#CCA700"
            elif severity == 3:
                icon = "ℹ️"
                color = "#75BEFF"
            else:
                icon = "💡"
                color = "#CCCCCC"
            
            text = f"{icon} Line {line}:{char} - {message}"
            item = QtWidgets.QListWidgetItem(text)
            item.setForeground(QtGui.QColor(color))
            self.list_widget.addItem(item)


# ============================================================================
# Main Editor Window
# ============================================================================

class LSPEditorWindow(QtWidgets.QWidget):
    """Main LSP editor window"""
    
    def __init__(self, parent=None, node=None, parm_name="code", 
                 server_cmd="lake serve", workspace_path="", file_path=""):
        super().__init__(parent)
        
        self.node = node
        self.parm_name = parm_name
        self.server_cmd = server_cmd
        self.workspace_path = workspace_path or os.getcwd()
        self.file_path = file_path
        self.lsp_client = None
        self.editor = None
        
        self.setup_ui()
        
        QtCore.QTimer.singleShot(100, self.start_lsp)
    
    def setup_ui(self):
        main_layout = QtWidgets.QVBoxLayout(self)
        main_layout.setContentsMargins(0, 0, 0, 0)
        main_layout.setSpacing(0)
        
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
        """)
        button_layout = QtWidgets.QHBoxLayout(button_bar)
        button_layout.setContentsMargins(8, 8, 8, 8)
        
        self.status_label = QtWidgets.QLabel("Initializing LSP...")
        self.status_label.setStyleSheet("color: #cccccc; padding-left: 10px;")
        
        self.apply_button = QtWidgets.QPushButton("Apply")
        self.apply_button.clicked.connect(self.apply_changes)
        self.apply_button.setEnabled(False)
        
        self.ok_button = QtWidgets.QPushButton("OK")
        self.ok_button.clicked.connect(self.ok_and_close)
        self.ok_button.setEnabled(False)
        
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
        
        self.keybinding_button = QtWidgets.QPushButton("⌨ Keybindings")
        self.keybinding_button.clicked.connect(self.show_keybinding_dialog)
        
        button_layout.addWidget(self.status_label)
        button_layout.addStretch()
        button_layout.addWidget(self.keybinding_button)
        button_layout.addWidget(self.apply_button)
        button_layout.addWidget(self.ok_button)
        button_layout.addWidget(self.cancel_button)
        
        main_layout.addWidget(button_bar)
        
        self.splitter = QtWidgets.QSplitter(QtCore.Qt.Vertical)
        self.splitter.setStyleSheet("""
            QSplitter::handle {
                background-color: #3e3e42;
                height: 2px;
            }
            QSplitter::handle:hover {
                background-color: #007acc;
            }
        """)
        
        self.editor_container = QtWidgets.QWidget()
        self.editor_layout = QtWidgets.QVBoxLayout(self.editor_container)
        self.editor_layout.setContentsMargins(0, 0, 0, 0)
        
        self.placeholder = QtWidgets.QLabel("Starting LSP server...")
        self.placeholder.setAlignment(QtCore.Qt.AlignCenter)
        self.placeholder.setStyleSheet("color: gray; padding: 20px;")
        self.editor_layout.addWidget(self.placeholder)
        
        self.splitter.addWidget(self.editor_container)
        
        self.info_view = InfoView()
        self.splitter.addWidget(self.info_view)
        
        self.splitter.setSizes([500, 150])
        
        main_layout.addWidget(self.splitter)
    
    def start_lsp(self):
        logger.log("=== Starting LSP ===")
        logger.log(f"Server: {self.server_cmd}")
        logger.log(f"Workspace: {self.workspace_path}")
        logger.log(f"File: {self.file_path or '(in-memory)'}")
        
        try:
            cmd_parts = self.server_cmd.split() if isinstance(self.server_cmd, str) else self.server_cmd
            self.lsp_client = LSPClient(cmd_parts, self.workspace_path)
            
            if not self.lsp_client.start():
                raise Exception("Failed to start LSP server process")
            
            content = ""
            if self.file_path and os.path.exists(self.file_path):
                with open(self.file_path, 'r') as f:
                    content = f.read()
                logger.log(f"Loaded content from file: {len(content)} chars")
            elif self.node and self.parm_name:
                content = self.node.parm(self.parm_name).eval()
                logger.log(f"Loaded content from parameter '{self.parm_name}': {len(content)} chars")
            else:
                content = ""
                logger.log("Starting with empty content")
            
            if self.placeholder:
                self.editor_layout.removeWidget(self.placeholder)
                self.placeholder.deleteLater()
                self.placeholder = None
            
            keybindings = KeybindingPresets.standard()
            if self.node and self.node.parm("keybindings_preset"):
                preset_idx = self.node.parm("keybindings_preset").eval()
                if preset_idx == 1:
                    keybindings = KeybindingPresets.emacs()
                logger.log(f"Using keybindings from parameter: {'Emacs' if preset_idx == 1 else 'Standard'}")
            
            self.editor = LSPCodeEditor(self.lsp_client, self.file_path,
                                       keybindings=keybindings)
            self.editor.setPlainText(content)
            self.editor.diagnosticsChanged.connect(self.info_view.update_diagnostics)
            
            self.editor.font_size_changed.connect(self.info_view.set_font_size)
            
            self.editor.parent_window = self
            self.editor.workspace_path = self.workspace_path
            
            self.editor_layout.addWidget(self.editor)
            
            status_msg = f"LSP running: {self.server_cmd}"
            if self.editor.is_virtual:
                status_msg += " (in-memory document)"
            self.status_label.setText(status_msg)
            self.apply_button.setEnabled(True)
            self.ok_button.setEnabled(True)
            
            logger.log("=== LSP Started Successfully ===")
            
        except Exception as e:
            logger.log(f"Error starting LSP: {e}", "ERROR")
            import hou
            hou.ui.displayMessage(f"Error starting LSP: {str(e)}",
                                severity=hou.severityType.Error)
            self.status_label.setText(f"Error: {str(e)}")
    
    def show_keybinding_dialog(self):
        if not self.editor:
            return
        
        current_preset = "emacs" if self.editor.is_emacs_mode else "standard"
        dialog = KeybindingDialog(current_preset, self)
        
        if dialog.exec() == QtWidgets.QDialog.Accepted:
            preset = dialog.get_selected_preset()
            
            if preset == "emacs":
                self.editor.set_keybindings(KeybindingPresets.emacs())
            else:
                self.editor.set_keybindings(KeybindingPresets.standard())
            
            if self.node and self.node.parm("keybindings_preset"):
                self.node.parm("keybindings_preset").set(1 if preset == "emacs" else 0)
                logger.log(f"Updated keybindings_preset parameter to: {preset}")
            
            import hou
            hou.ui.setStatusMessage(f"Keybindings changed to: {preset.title()}", hou.severityType.Message)
    
    def apply_changes(self):
        if self.node and self.parm_name and self.editor:
            try:
                code = self.editor.toPlainText()
                self.node.parm(self.parm_name).set(code)
                import hou
                hou.ui.setStatusMessage("Changes applied to parameter", hou.severityType.Message)
                logger.log(f"Applied {len(code)} chars to parameter '{self.parm_name}'")
            except Exception as e:
                import hou
                hou.ui.displayMessage(f"Error applying changes: {str(e)}", 
                                    severity=hou.severityType.Error)
                logger.log(f"Error applying changes: {e}", "ERROR")
        
        if self.editor and self.file_path and not self.editor.is_virtual:
            self.editor.save_file()
    
    def ok_and_close(self):
        self.apply_changes()
        self.close()
    
    def closeEvent(self, event):
        logger.log("Editor window closing")
        if self.lsp_client:
            if self.lsp_client.process.state() == QProcess.ProcessState.Running:
                self.lsp_client.process.kill()
            logger.log("LSP process terminated")
        event.accept()


# ============================================================================
# Keybinding Dialog
# ============================================================================

class KeybindingDialog(QtWidgets.QDialog):
    """Dialog for selecting keybinding preset"""
    
    def __init__(self, current_preset="standard", parent=None):
        super().__init__(parent)
        self.setWindowTitle("Keybinding Settings")
        self.setModal(True)
        self.selected_preset = current_preset
        
        layout = QtWidgets.QVBoxLayout()
        
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
        
        info_text = QtWidgets.QTextEdit()
        info_text.setReadOnly(True)
        info_text.setMaximumHeight(300)
        info_text.setHtml(self.get_keybinding_reference())
        
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
        
        self.standard_radio.toggled.connect(lambda: info_text.setHtml(self.get_keybinding_reference()))
        self.emacs_radio.toggled.connect(lambda: info_text.setHtml(self.get_keybinding_reference()))
    
    def get_keybinding_reference(self):
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
            ("Editing", ['delete_char', 'backspace', 'delete_word', 'backspace_word']),
            ("LSP", ['completion', 'goto_definition', 'save']),
            ("Clipboard", ['cut', 'copy', 'paste']),
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
        return "emacs" if self.emacs_radio.isChecked() else "standard"


# ============================================================================
# Public API
# ============================================================================

_editor_windows = {}

def open_code_editor(node=None, parm_name="code", server_cmd="lake serve", 
                    workspace_path="", file_path=None):
    """
    Open the LSP code editor.
    
    Args:
        node: The HDA node (optional)
        parm_name: The name of the string parameter to edit (optional)
        server_cmd: LSP server command (e.g., "lake serve" for Lean 4)
        workspace_path: Project root directory
        file_path: File to edit (optional - will create virtual file if None)
    
    Returns:
        The editor window instance
    """
    global _editor_windows
    
    widget_key = f"{node.path() if node else 'standalone'}_{parm_name}_{file_path or 'virtual'}"
    
    if widget_key in _editor_windows and _editor_windows[widget_key].isVisible():
        _editor_windows[widget_key].raise_()
        _editor_windows[widget_key].activateWindow()
        return _editor_windows[widget_key]
    
    import hou
    editor = LSPEditorWindow(
        parent=hou.qt.mainWindow(),
        node=node,
        parm_name=parm_name,
        server_cmd=server_cmd,
        workspace_path=workspace_path,
        file_path=file_path or ""
    )
    
    title = f"LSP Code Editor"
    if node:
        title += f" - {node.name()}.{parm_name}"
    elif file_path:
        title += f" - {os.path.basename(file_path)}"
    else:
        title += " - (in-memory)"
    
    editor.setWindowTitle(title)
    editor.setWindowFlags(QtCore.Qt.Window)
    editor.resize(1000, 750)
    
    editor.setStyleSheet("""
        QWidget {
            background-color: #1e1e1e;
        }
    """)
    
    editor.show()
    _editor_windows[widget_key] = editor
    
    return editor


def open_code_editor_from_kwargs(kwargs):
    """
    Convenience function to call from a button callback.
    
    Expects node parameters:
    - code: string parameter to edit
    - lsp_server: LSP server command (default: "lake serve")
    - workspace: workspace path (default: current directory)
    - file: file path (optional - creates virtual file if empty)
    """
    node = kwargs['node']
    
    server_cmd = node.evalParm("lsp_server") if node.parm("lsp_server") else "lake serve"
    workspace = node.evalParm("workspace") if node.parm("workspace") else os.getcwd()
    file_path = node.evalParm("file") if node.parm("file") else ""
    
    if not file_path:
        file_path = None
    
    return open_code_editor(
        node=node,
        parm_name='code',
        server_cmd=server_cmd,
        workspace_path=workspace,
        file_path=file_path
    )


# ============================================================================
# Example usage
# ============================================================================

if __name__ == "__main__":
    """
    Example usage in Houdini:
    
    1. From a button callback:
       import lsp_editor
       lsp_editor.open_code_editor_from_kwargs(kwargs)
    
    2. Standalone with file:
       import lsp_editor
       editor = lsp_editor.open_code_editor(
           server_cmd="lake serve",
           workspace_path="/path/to/lean/project",
           file_path="/path/to/file.lean"
       )
    
    3. In-memory editing (no file):
       import lsp_editor
       editor = lsp_editor.open_code_editor(
           server_cmd="lake serve",
           workspace_path="/path/to/lean/project"
       )
    
    4. With HDA parameter (in-memory):
       import lsp_editor
       node = hou.pwd()
       editor = lsp_editor.open_code_editor(
           node=node,
           parm_name="code",
           server_cmd="lake serve",
           workspace_path="/path/to/lean/project"
       )
    
    5. With HDA parameter and file:
       import lsp_editor
       node = hou.pwd()
       editor = lsp_editor.open_code_editor(
           node=node,
           parm_name="code",
           server_cmd="lake serve",
           workspace_path="/path/to/lean/project",
           file_path="/path/to/file.lean"
       )
    """
    pass
