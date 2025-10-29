"""
LSP Code Editor Python Panel for Houdini

To install:
1. Windows > Python Panel Editor
2. Click the "+" button to create a new interface
3. Name it "LSP Code Editor"
4. Paste this entire code
5. Click Accept
6. Find it in: Right-click pane divider > Split Pane > Python Panel > LSP Code Editor

Usage:
- Server: Enter LSP command (e.g., "lake serve" for Lean 4)
- Workspace: Project root directory
- File: .lean file to edit
- Click "Start LSP" to begin

Shortcuts:
- Ctrl+Space: Trigger completion
- Ctrl+hover: Show documentation
- F12: Go to definition
- Ctrl+S: Save file
"""

import hou
from PySide6 import QtWidgets, QtGui, QtCore
from PySide6.QtCore import QProcess
import json
import os
import re
from datetime import datetime


class DebugLogger(QtCore.QObject):
    """Debug logger for LSP communication"""
    logMessage = QtCore.Signal(str, str)  # message, level
   
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
        logger.log(f"Process finished: exit_code={exit_code}, status={exit_status}", "ERROR" if exit_code != 0 else "INFO")
        if exit_code != 0:
            logger.log("LSP server crashed or failed to start! Check the command and workspace settings.", "ERROR")
       
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
        bytes_written = self.process.write(header + content)
        logger.log(f"Message written: {bytes_written} bytes")
       
    def _handle_stdout(self):
        data = bytes(self.process.readAllStandardOutput())
        logger.log(f"Received stdout: {len(data)} bytes")
        self.buffer += data
       
        messages_processed = 0
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
                logger.log("No Content-Length header found", "WARNING")
                break
               
            message_start = header_end + 4
            message_end = message_start + content_length
           
            if len(self.buffer) < message_end:
                logger.log(f"Incomplete message: have {len(self.buffer)}, need {message_end}")
                break
               
            message_data = self.buffer[message_start:message_end]
            self.buffer = self.buffer[message_end:]
           
            try:
                message = json.loads(message_data.decode('utf-8'))
                messages_processed += 1
                self._handle_message(message)
            except json.JSONDecodeError as e:
                logger.log(f"JSON decode error: {e}", "ERROR")
               
        if messages_processed > 0:
            logger.log(f"Processed {messages_processed} messages from stdout")
               
    def _handle_stderr(self):
        error = bytes(self.process.readAllStandardError()).decode('utf-8')
        if error.strip():
            logger.log(f"LSP stderr: {error}", "WARNING")
           
    def _handle_message(self, message):
        """Handle a message from the LSP server"""
        if "method" in message:
            method = message["method"]
            logger.log(f"Received notification/request: {method}")
            params = message.get("params", {})
           
            if method == "textDocument/publishDiagnostics":
                diag_count = len(params.get("diagnostics", []))
                logger.log(f"Received {diag_count} diagnostics")
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
        else:
            logger.log(f"Unknown message type: {message}", "WARNING")
               
    def _handle_initialize(self, result):
        if result and "capabilities" in result:
            capabilities = result["capabilities"]
            logger.log(f"Server capabilities received: {list(capabilities.keys())}")
           
            if "semanticTokensProvider" in capabilities:
                provider = capabilities["semanticTokensProvider"]
                if "legend" in provider:
                    self.semantic_token_legend = provider["legend"]
                    logger.log(f"Semantic tokens legend: {len(provider['legend'].get('tokenTypes', []))} types")
                else:
                    logger.log("No semantic tokens legend found", "WARNING")
            else:
                logger.log("Server does not support semantic tokens", "WARNING")
        else:
            logger.log("Initialize result has no capabilities", "ERROR")
           
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
            logger.log("No completion items")
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
        else:
            logger.log("No hover information")
           
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
        else:
            logger.log("No definition found")
               
    def _handle_semantic_tokens(self, result):
        if result and "data" in result:
            token_count = len(result["data"]) // 5
            logger.log(f"Semantic tokens received: {token_count} tokens")
            self.semanticTokensReceived.emit({
                "legend": self.semantic_token_legend,
                "data": result["data"]
            })
        else:
            logger.log("No semantic token data")
           
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
        logger.log(f"Requested semantic tokens for {uri}")
       
    def shutdown(self):
        logger.log("Shutting down LSP client")
        self._send_request("shutdown", {})
        self._send_notification("exit", {})
        self.process.waitForFinished(1000)
        self.process.kill()


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
            logger.log("Cannot apply highlighting: missing legend or data", "WARNING")
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
       
        logger.log(f"Decoded {len(tokens)} tokens, applying to document")
       
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
        logger.log("Highlighting applied successfully")


class CompletionPopup(QtWidgets.QListWidget):
    itemSelected = QtCore.Signal(str, str, int)  # label, insertText, prefixLength
    keyTyped = QtCore.Signal(str) # Signal to send typed character back to editor
   
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowFlags(QtCore.Qt.WindowType.Popup | QtCore.Qt.WindowType.FramelessWindowHint)
        self.setMaximumHeight(200)
        self.setMinimumWidth(400)
        self.completion_items = []  # Stores sorted items with their data
        self.filter_text = ""
        self.itemClicked.connect(self._on_item_clicked)
       
        # Style
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
       
        # Sort: exact matches first, then by label
        def sort_key(item):
            label = item.get("label", "").lower()
            # Prioritize items that start with the prefix
            starts_with = label.startswith(self.filter_text)
            return (not starts_with, label)
       
        # Store the SORTED items
        self.completion_items = sorted(completions, key=sort_key)
       
        for item in self.completion_items:
            label = item.get("label", "")
            kind = item.get("kind", "")
            detail = item.get("detail", "")
           
            # Format display text
            kind_icons = {
                1: "📝",  # Text
                2: "ƒ",   # Method
                3: "ƒ",   # Function
                4: "⚙",   # Constructor
                5: "□",   # Field
                6: "x",   # Variable
                7: "C",   # Class
                8: "I",   # Interface
                9: "M",   # Module
                10: "P",  # Property
            }
            icon = kind_icons.get(kind, "•")
           
            display_text = f"{icon} {label}"
            if detail:
                display_text += f"  {detail}"
               
            list_item = QtWidgets.QListWidgetItem(display_text)
            # Store the completion data directly on the item
            list_item.setData(QtCore.Qt.ItemDataRole.UserRole, item)
            self.addItem(list_item)
           
        if self.count() > 0:
            self.setCurrentRow(0)
            self.move(position)
            self.show()
            self.setFocus()
            logger.log(f"Showing {len(self.completion_items)} completions")
           
    def filter_items(self, text):
        """Filter visible items based on text"""
        self.filter_text = text.lower()
        visible_count = 0
       
        for i in range(self.count()):
            item = self.item(i)
            comp_data = item.data(QtCore.Qt.ItemDataRole.UserRole)
            if comp_data:
                label = comp_data.get("label", "").lower()
                # Show if label contains the filter text
                matches = self.filter_text in label
                item.setHidden(not matches)
                if matches:
                    visible_count += 1
                   
        # Select first visible item
        for i in range(self.count()):
            if not self.item(i).isHidden():
                self.setCurrentRow(i)
                break
               
        logger.log(f"Filtered to {visible_count} items")
               
    def _on_item_clicked(self, item):
        """Handle item click"""
        comp_data = item.data(QtCore.Qt.ItemDataRole.UserRole)
        if comp_data:
            self._select_completion(comp_data)
           
    def _select_completion(self, comp):
        """Select a completion item"""
        label = comp.get("label", "")
        insert_text = comp.get("insertText", label)
        # Calculate prefix length to replace
        text_edit = comp.get("textEdit")
        prefix_len = 0
        if text_edit:
            range_data = text_edit.get("range", {})
            start = range_data.get("start", {})
            end = range_data.get("end", {})
            # If we have range info, calculate the replacement length
            if start and end:
                prefix_len = end.get("character", 0) - start.get("character", 0)
       
        self.itemSelected.emit(label, insert_text, prefix_len)
           
    # In CompletionPopup
    def keyPressEvent(self, event):
        """Handle key press events"""

        # 1. Handle selection/closing keys
        # (Remains the same as the previous fix)
        if event.key() in (QtCore.Qt.Key.Key_Return, QtCore.Qt.Key.Key_Enter, QtCore.Qt.Key.Key_Tab):
            current_item = self.currentItem()
            if current_item:
                comp_data = current_item.data(QtCore.Qt.ItemDataRole.UserRole)
                if comp_data:
                    self._select_completion(comp_data)
            else:
                self.hide()
                if self.editor_widget:
                    self.editor_widget.setFocus()
            event.accept()
            return

        elif event.key() == QtCore.Qt.Key.Key_Escape:
            self.hide()
            event.accept()
            if self.editor_widget:
                self.editor_widget.setFocus()
            return

        # 2. Handle navigation keys within the popup
        elif event.key() in (QtCore.Qt.Key.Key_Up, QtCore.Qt.Key.Key_Down, QtCore.Qt.Key.Key_PageUp, QtCore.Qt.Key.Key_PageDown):
            super().keyPressEvent(event)
            event.accept()
            return

        # 3. Handle character input (The new, safer fix)
        typed_text = event.text()
        if typed_text and typed_text.isprintable() and len(typed_text) == 1:
            # Instead of reposting the event, send the character via a signal
            self.keyTyped.emit(typed_text)

            # Don't hide the popup or return focus yet.
            # The editor will handle inserting the text, and its textChanged signal
            # will trigger the filter update, keeping the focus here.

            event.accept()
            return

        # 4. Handle Backspace/Delete while popup is open
        elif event.key() in (QtCore.Qt.Key.Key_Backspace, QtCore.Qt.Key.Key_Delete):
            # Allow the editor to handle the deletion logic, which will trigger the filter update
            if self.editor_widget:
                self.hide() # Hide before sending to prevent focus issue
                self.editor_widget.setFocus()
                QtCore.QCoreApplication.postEvent(self.editor_widget, event)
                event.accept()
                return

        # 5. All other keys: Hide and return focus
        self.hide()
        if self.editor_widget:
            self.editor_widget.setFocus()

        # We still pass to super in case we missed a base Qt feature
        super().keyPressEvent(event)

class InfoView(QtWidgets.QWidget):
    """Info/Error view showing diagnostics"""
   
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setup_ui()
       
    def setup_ui(self):
        layout = QtWidgets.QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
       
        header = QtWidgets.QLabel("Diagnostics")
        header.setStyleSheet("font-weight: bold; padding: 4px; background-color: #2D2D30;")
        layout.addWidget(header)
       
        self.list_widget = QtWidgets.QListWidget()
        self.list_widget.setStyleSheet("""
            QListWidget {
                background-color: #1E1E1E;
                color: #D4D4D4;
                border: none;
                font-family: Consolas, Monaco, monospace;
                font-size: 10px;
            }
            QListWidget::item {
                padding: 4px;
                border-bottom: 1px solid #2D2D30;
            }
        """)
        layout.addWidget(self.list_widget)
       
    def update_diagnostics(self, diagnostics):
        """Update the diagnostic list"""
        self.list_widget.clear()
       
        for diag in diagnostics:
            severity = diag.get("severity", 1)
            message = diag.get("message", "")
            range_data = diag.get("range", {})
            start = range_data.get("start", {})
            line = start.get("line", 0) + 1
            char = start.get("character", 0) + 1
           
            # Icon based on severity
            if severity == 1:  # Error
                icon = "❌"
                color = "#F48771"
            elif severity == 2:  # Warning
                icon = "⚠️"
                color = "#CCA700"
            elif severity == 3:  # Info
                icon = "ℹ️"
                color = "#75BEFF"
            else:  # Hint
                icon = "💡"
                color = "#CCCCCC"
               
            text = f"{icon} Line {line}:{char} - {message}"
            item = QtWidgets.QListWidgetItem(text)
            item.setForeground(QtGui.QColor(color))
            self.list_widget.addItem(item)


class LSPCodeEditor(QtWidgets.QPlainTextEdit):
    diagnosticsChanged = QtCore.Signal(list)
   
    def __init__(self, lsp_client, file_path):
        super().__init__()
        self.lsp_client = lsp_client
        self.file_path = file_path
        self.uri = f"file://{file_path}"
        self.version = 1
        self.completion_popup = CompletionPopup(self)
        self.current_diagnostics = []
        self.completion_prefix = ""
        self.completion_start_pos = 0
       
        logger.log(f"Editor created for {file_path}")
       
        self.setup_editor()
        self.semantic_highlighter = SemanticHighlighter(self.document())
       
        self.lsp_client.diagnosticsReceived.connect(self.handle_diagnostics)
        self.lsp_client.completionReceived.connect(self.handle_completion)
        self.lsp_client.hoverReceived.connect(self.show_hover)
        self.lsp_client.definitionReceived.connect(self.goto_definition)
        self.lsp_client.semanticTokensReceived.connect(self.handle_semantic_tokens)
        self.lsp_client.serverInitialized.connect(self.on_server_initialized)
        self.completion_popup.itemSelected.connect(self.insert_completion)
        self.completion_popup.keyTyped.connect(self.insert_typed_character)
       
        self.change_timer = QtCore.QTimer()
        self.change_timer.setSingleShot(True)
        self.change_timer.timeout.connect(self.on_text_changed)
        self.textChanged.connect(lambda: self.change_timer.start(500))
       
        self.highlight_timer = QtCore.QTimer()
        self.highlight_timer.setSingleShot(True)
        self.highlight_timer.timeout.connect(self.request_semantic_tokens)
       
        # Hover timer
        self.hover_timer = QtCore.QTimer()
        self.hover_timer.setSingleShot(True)
        self.hover_timer.timeout.connect(self.request_hover)
        self.last_hover_pos = None
       
        logger.log("Waiting for server initialization before opening document")
       
    def on_server_initialized(self):
        """Called when LSP server is fully initialized"""
        logger.log("Server initialized, now opening document")
        text = self.toPlainText()
        self.lsp_client.did_open(self.uri, "lean4", text)
        QtCore.QTimer.singleShot(1000, self.request_semantic_tokens)
       
    def setup_editor(self):
        font = QtGui.QFont("Courier New", 10)
        font.setStyleHint(QtGui.QFont.StyleHint.Monospace)
        self.setFont(font)
       
        palette = self.palette()
        palette.setColor(QtGui.QPalette.ColorRole.Base, QtGui.QColor("#1E1E1E"))
        palette.setColor(QtGui.QPalette.ColorRole.Text, QtGui.QColor("#D4D4D4"))
        self.setPalette(palette)
       
        metrics = QtGui.QFontMetricsF(font)
        self.setTabStopDistance(metrics.horizontalAdvance(' ') * 4)
        self.setLineWrapMode(QtWidgets.QPlainTextEdit.LineWrapMode.NoWrap)
       
        # Enable mouse tracking for hover
        self.setMouseTracking(True)
       
    def get_word_under_cursor(self, cursor=None):
        """Get the word under cursor and its start position"""
        if cursor is None:
            cursor = self.textCursor()
           
        # Select word under cursor
        cursor.select(QtGui.QTextCursor.SelectionType.WordUnderCursor)
        word = cursor.selectedText()
        start_pos = cursor.selectionStart()
       
        return word, start_pos
       
    def on_text_changed(self):
        if not self.lsp_client.is_initialized:
            logger.log("Text changed but server not initialized yet, skipping update", "WARNING")
            return
           
        # If completion popup is visible, update filter based on current text
        if self.completion_popup.isVisible():
            cursor = self.textCursor()
            current_pos = cursor.position()
           
            # Get text from completion start to current cursor
            cursor.setPosition(self.completion_start_pos)
            cursor.setPosition(current_pos, QtGui.QTextCursor.MoveMode.KeepAnchor)
            current_text = cursor.selectedText()
           
            logger.log(f"Filtering completions with: '{current_text}'")
            self.completion_popup.filter_items(current_text)
           
            # If we moved away from the completion region, hide it
            if current_pos < self.completion_start_pos:
                self.completion_popup.hide()
           
            # Don't send didChange while completing
            return
           
        self.version += 1
        text = self.toPlainText()
        self.lsp_client.did_change(self.uri, self.version, text)
        self.highlight_timer.start(800)
       
    def request_semantic_tokens(self):
        if not self.lsp_client.is_initialized:
            logger.log("Cannot request semantic tokens: server not initialized", "WARNING")
            return
        self.lsp_client.semantic_tokens(self.uri)
       
    def request_hover(self):
        """Request hover information for current position"""
        if self.last_hover_pos and self.lsp_client.is_initialized:
            cursor = self.cursorForPosition(self.last_hover_pos)
            line = cursor.blockNumber()
            char = cursor.columnNumber()
            self.lsp_client.hover(self.uri, line, char)
       
    def save_file(self):
        try:
            with open(self.file_path, 'w') as f:
                f.write(self.toPlainText())
            logger.log(f"File saved: {self.file_path}")
            return True
        except Exception as e:
            logger.log(f"Error saving file: {e}", "ERROR")
            hou.ui.displayMessage(f"Error saving file: {str(e)}")
            return False
       
    def keyPressEvent(self, event):
        # Handle completion popup first
        if self.completion_popup.isVisible():
            # Allow typing to filter
            if event.text() and event.text().isprintable() and not event.modifiers() & QtCore.Qt.KeyboardModifier.ControlModifier:
                # Let the text be inserted first
                super().keyPressEvent(event)
                return
            elif event.key() == QtCore.Qt.Key.Key_Backspace:
                # Let backspace work
                super().keyPressEvent(event)
                return
            elif event.key() in (QtCore.Qt.Key.Key_Down, QtCore.Qt.Key.Key_Up,
                              QtCore.Qt.Key.Key_PageDown, QtCore.Qt.Key.Key_PageUp):
                self.completion_popup.keyPressEvent(event)
                return
            elif event.key() in (QtCore.Qt.Key.Key_Return, QtCore.Qt.Key.Key_Enter, QtCore.Qt.Key.Key_Tab):
                self.completion_popup.keyPressEvent(event)
                return
            elif event.key() == QtCore.Qt.Key.Key_Escape:
                self.completion_popup.hide()
                return
       
        # Save file
        if event.key() == QtCore.Qt.Key.Key_S and event.modifiers() == QtCore.Qt.KeyboardModifier.ControlModifier:
            if self.save_file():
                hou.ui.setStatusMessage("File saved", hou.severityType.Message)
            return
           
        # Trigger completion
        if event.key() == QtCore.Qt.Key.Key_Space and event.modifiers() == QtCore.Qt.KeyboardModifier.ControlModifier:
            logger.log("Ctrl+Space pressed - triggering completion")
            self.trigger_completion()
            return
           
        # Go to definition
        if event.key() == QtCore.Qt.Key.Key_F12:
            logger.log("F12 pressed - triggering go to definition")
            self.trigger_goto_definition()
            return
           
        super().keyPressEvent(event)
       
        # Auto-trigger completion on certain characters
        if event.text() in ('.', ':') and not self.completion_popup.isVisible():
            QtCore.QTimer.singleShot(100, self.trigger_completion)
           
    def mouseMoveEvent(self, event):
        super().mouseMoveEvent(event)
       
        # Show hover on Ctrl or after delay
        self.last_hover_pos = event.pos()
        if event.modifiers() == QtCore.Qt.KeyboardModifier.ControlModifier:
            self.hover_timer.start(300)
        else:
            self.hover_timer.stop()
           
    def trigger_completion(self):
        """Trigger code completion"""
        cursor = self.textCursor()
        line = cursor.blockNumber()
        char = cursor.columnNumber()
       
        # Store the start position - go back to start of current word
        text_cursor = self.textCursor()
        text_cursor.movePosition(QtGui.QTextCursor.MoveOperation.StartOfWord, QtGui.QTextCursor.MoveMode.MoveAnchor)
        self.completion_start_pos = text_cursor.position()
       
        # Get current prefix
        text_cursor.setPosition(cursor.position(), QtGui.QTextCursor.MoveMode.KeepAnchor)
        self.completion_prefix = text_cursor.selectedText()
       
        logger.log(f"Requesting completion at {line}:{char}, prefix='{self.completion_prefix}', start_pos={self.completion_start_pos}")
        self.lsp_client.completion(self.uri, line, char)
       
    def trigger_goto_definition(self):
        """Trigger go to definition"""
        cursor = self.textCursor()
        line = cursor.blockNumber()
        char = cursor.columnNumber()
        logger.log(f"Requesting definition at {line}:{char}")
        self.lsp_client.definition(self.uri, line, char)
       
    def handle_completion(self, items):
        """Handle completion results"""
        if items:
            cursor = self.textCursor()
            cursor_rect = self.cursorRect(cursor)
            global_pos = self.mapToGlobal(cursor_rect.bottomLeft())
            self.completion_popup.show_completions(items, global_pos, self.completion_prefix)
        else:
            logger.log("No completion items to show")
           
    def insert_completion(self, label, insert_text, prefix_len):
        """Insert selected completion"""
        cursor = self.textCursor()
       
        # Delete from completion start to current position
        current_pos = cursor.position()
        cursor.setPosition(self.completion_start_pos)
        cursor.setPosition(current_pos, QtGui.QTextCursor.MoveMode.KeepAnchor)
        cursor.removeSelectedText()
       
        # Insert the completion text
        cursor.insertText(insert_text)
        self.completion_popup.hide()
        logger.log(f"Inserted completion: {label}")
       
        # Send didChange after completion
        self.version += 1
        text = self.toPlainText()
        self.lsp_client.did_change(self.uri, self.version, text)
        self.highlight_timer.start(800)

    # In LSPCodeEditor
    def insert_typed_character(self, text):
        """Inserts text received from the completion popup when the user types."""
        cursor = self.textCursor()

        # Insert the character directly
        cursor.insertText(text)
        self.setTextCursor(cursor)

        # The insertion should automatically fire textChanged,
        # which calls filter_completions_if_needed,
        # which will update the popup filter correctly.

        # Since the popup retains focus for filtering,
        # we don't return focus here.        
       
    def show_hover(self, text):
        """Show hover information"""
        if text:
            # Clean up the text for display
            # Remove markdown code blocks if present
            text = re.sub(r'```[\w]*\n', '', text)
            text = re.sub(r'```', '', text)
            text = text.strip()
           
            if text:
                QtWidgets.QToolTip.showText(QtGui.QCursor.pos(), text, self)
           
    def goto_definition(self, file_path, line, character):
        """Handle go to definition"""
        hou.ui.displayMessage(
            f"Definition at:\n{file_path}\nLine {line+1}, Column {character+1}",
            severity=hou.severityType.Message,
            title="Go to Definition"
        )
       
    def handle_diagnostics(self, diagnostics):
        """Handle diagnostics (errors/warnings)"""
        self.current_diagnostics = diagnostics
        self.diagnosticsChanged.emit(diagnostics)
       
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
            cursor.movePosition(QtGui.QTextCursor.MoveOperation.Down,
                              QtGui.QTextCursor.MoveMode.KeepAnchor,
                              end.get("line", 0) - start.get("line", 0))
            cursor.movePosition(QtGui.QTextCursor.MoveOperation.Right,
                              QtGui.QTextCursor.MoveMode.KeepAnchor, end.get("character", 0))
           
            selection = QtWidgets.QTextEdit.ExtraSelection()
            selection.cursor = cursor
           
            if severity == 1:  # Error
                selection.format.setUnderlineColor(QtGui.QColor("#F14C4C"))
            elif severity == 2:  # Warning
                selection.format.setUnderlineColor(QtGui.QColor("#CCA700"))
            else:  # Info/Hint - brighter blue
                selection.format.setUnderlineColor(QtGui.QColor("#3794FF"))
               
            selection.format.setUnderlineStyle(QtGui.QTextCharFormat.UnderlineStyle.WaveUnderline)
            selection.format.setToolTip(message)
            extra_selections.append(selection)
           
        self.setExtraSelections(extra_selections)
       
    def handle_semantic_tokens(self, result):
        """Handle semantic tokens from LSP"""
        legend = result.get("legend")
        data = result.get("data")
        if legend and data:
            self.semantic_highlighter.apply_highlighting(legend, data)
           
    def event(self, event):
        """Handle tooltip events to show diagnostic messages"""
        if event.type() == QtCore.QEvent.Type.ToolTip:
            help_event = event
            cursor = self.cursorForPosition(help_event.pos())
           
            # Check if cursor is on a diagnostic
            for diag in self.current_diagnostics:
                range_data = diag.get("range", {})
                start = range_data.get("start", {})
                end = range_data.get("end", {})
               
                cursor_line = cursor.blockNumber()
                cursor_char = cursor.columnNumber()
               
                if (cursor_line >= start.get("line", 0) and
                    cursor_line <= end.get("line", 0)):
                    if cursor_line == start.get("line", 0):
                        if cursor_char < start.get("character", 0):
                            continue
                    if cursor_line == end.get("line", 0):
                        if cursor_char > end.get("character", 0):
                            continue
                           
                    # Show diagnostic message
                    message = diag.get("message", "")
                    QtWidgets.QToolTip.showText(help_event.globalPos(), message, self)
                    return True
                   
        return super().event(event)


class LSPEditorPanel(QtWidgets.QWidget):
    """Main Python Panel widget"""
   
    def __init__(self, parent=None):
        super().__init__(parent)
        self.lsp_client = None
        self.editor = None
        self.setup_ui()
       
        # Connect logger to log widget
        logger.logMessage.connect(self.append_log)
       
        logger.log("LSP Editor Panel initialized")
       
    def setup_ui(self):
        main_layout = QtWidgets.QVBoxLayout(self)
        main_layout.setContentsMargins(4, 4, 4, 4)
       
        # Config section
        config_group = QtWidgets.QGroupBox("LSP Configuration")
        config_layout = QtWidgets.QGridLayout()
       
        config_layout.addWidget(QtWidgets.QLabel("Server Command:"), 0, 0)
        self.server_cmd_edit = QtWidgets.QLineEdit()
        self.server_cmd_edit.setPlaceholderText("e.g., lake serve")
        config_layout.addWidget(self.server_cmd_edit, 0, 1)
       
        config_layout.addWidget(QtWidgets.QLabel("Workspace:"), 1, 0)
        workspace_layout = QtWidgets.QHBoxLayout()
        self.workspace_edit = QtWidgets.QLineEdit()
        self.workspace_edit.setPlaceholderText("Project root directory")
        workspace_browse_btn = QtWidgets.QPushButton("Browse...")
        workspace_browse_btn.clicked.connect(self.browse_workspace)
        workspace_layout.addWidget(self.workspace_edit)
        workspace_layout.addWidget(workspace_browse_btn)
        config_layout.addLayout(workspace_layout, 1, 1)
       
        config_layout.addWidget(QtWidgets.QLabel("File:"), 2, 0)
        file_layout = QtWidgets.QHBoxLayout()
        self.file_edit = QtWidgets.QLineEdit()
        self.file_edit.setPlaceholderText(".lean file to edit")
        file_browse_btn = QtWidgets.QPushButton("Browse...")
        file_browse_btn.clicked.connect(self.browse_file)
        file_layout.addWidget(self.file_edit)
        file_layout.addWidget(file_browse_btn)
        config_layout.addLayout(file_layout, 2, 1)
       
        buttons_layout = QtWidgets.QHBoxLayout()
        self.start_btn = QtWidgets.QPushButton("Start LSP")
        self.start_btn.clicked.connect(self.start_lsp)
        buttons_layout.addWidget(self.start_btn)
       
        self.debug_toggle = QtWidgets.QCheckBox("Show Debug Log")
        self.debug_toggle.setChecked(False)
        self.debug_toggle.stateChanged.connect(self.toggle_debug_log)
        buttons_layout.addWidget(self.debug_toggle)
        buttons_layout.addStretch()
       
        config_layout.addLayout(buttons_layout, 3, 0, 1, 2)
       
        config_group.setLayout(config_layout)
        main_layout.addWidget(config_group)
       
        # Splitter for editor and info view
        self.splitter = QtWidgets.QSplitter(QtCore.Qt.Orientation.Vertical)
       
        # Editor container
        editor_widget = QtWidgets.QWidget()
        self.editor_layout = QtWidgets.QVBoxLayout(editor_widget)
        self.editor_layout.setContentsMargins(0, 0, 0, 0)
       
        self.placeholder_label = QtWidgets.QLabel("Configure LSP settings above and click 'Start LSP' to begin")
        self.placeholder_label.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.placeholder_label.setStyleSheet("color: gray; padding: 20px;")
        self.editor_layout.addWidget(self.placeholder_label)
       
        self.splitter.addWidget(editor_widget)
       
        # Info view
        self.info_view = InfoView()
        self.splitter.addWidget(self.info_view)
       
        # Set initial sizes (editor 70%, info 30%)
        self.splitter.setSizes([700, 300])
       
        main_layout.addWidget(self.splitter, 1)
       
        # Debug log section (hidden by default)
        self.log_group = QtWidgets.QGroupBox("Debug Log")
        log_layout = QtWidgets.QVBoxLayout()
       
        self.log_text = QtWidgets.QTextEdit()
        self.log_text.setReadOnly(True)
        self.log_text.setMaximumHeight(150)
        self.log_text.setStyleSheet("font-family: Courier; font-size: 9px;")
        log_layout.addWidget(self.log_text)
       
        log_buttons = QtWidgets.QHBoxLayout()
        clear_log_btn = QtWidgets.QPushButton("Clear Log")
        clear_log_btn.clicked.connect(self.log_text.clear)
        log_buttons.addWidget(clear_log_btn)
        log_buttons.addStretch()
        log_layout.addLayout(log_buttons)
       
        self.log_group.setLayout(log_layout)
        self.log_group.setVisible(False)
        main_layout.addWidget(self.log_group)
       
        # Status bar
        self.status_label = QtWidgets.QLabel("Ready")
        self.status_label.setStyleSheet("padding: 2px; font-size: 10px;")
        main_layout.addWidget(self.status_label)
       
    def toggle_debug_log(self, state):
        """Toggle debug log visibility"""
        is_checked = state == QtCore.Qt.CheckState.Checked.value
        self.log_group.setVisible(is_checked)
        logger.enabled = is_checked
       
    def append_log(self, message, level):
        """Append message to log widget"""
        colors = {
            "ERROR": "red",
            "WARNING": "orange",
            "INFO": "black"
        }
        color = colors.get(level, "black")
        self.log_text.append(f'<span style="color: {color}">{message}</span>')
        scrollbar = self.log_text.verticalScrollBar()
        scrollbar.setValue(scrollbar.maximum())
       
    def browse_workspace(self):
        path = hou.ui.selectFile(title="Select Workspace Directory",
                                  file_type=hou.fileType.Directory)
        if path:
            abs_path = os.path.abspath(os.path.expanduser(path))
            self.workspace_edit.setText(abs_path)
            logger.log(f"Workspace selected: {abs_path}")
           
    def browse_file(self):
        path = hou.ui.selectFile(title="Select Lean File",
                                  pattern="*.lean")
        if path:
            abs_path = os.path.abspath(os.path.expanduser(path))
            self.file_edit.setText(abs_path)
            logger.log(f"File selected: {abs_path}")
            if not self.workspace_edit.text():
                workspace = os.path.dirname(abs_path)
                self.workspace_edit.setText(workspace)
                logger.log(f"Auto-set workspace: {workspace}")
       
    def start_lsp(self):
        server_cmd = self.server_cmd_edit.text().strip()
        workspace = self.workspace_edit.text().strip()
        file_path = self.file_edit.text().strip()
       
        workspace = os.path.abspath(os.path.expanduser(workspace))
        file_path = os.path.abspath(os.path.expanduser(file_path))
       
        logger.log("=== Starting LSP ===")
        logger.log(f"Server: {server_cmd}")
        logger.log(f"Workspace: {workspace}")
        logger.log(f"File: {file_path}")
       
        if not server_cmd or not workspace or not file_path:
            logger.log("Missing configuration fields", "ERROR")
            hou.ui.displayMessage("Please fill in all fields", severity=hou.severityType.Error)
            return
           
        if not os.path.exists(file_path):
            logger.log(f"File does not exist: {file_path}", "ERROR")
            hou.ui.displayMessage(f"File does not exist: {file_path}",
                                severity=hou.severityType.Error)
            return
           
        if not os.path.exists(workspace):
            logger.log(f"Workspace does not exist: {workspace}", "ERROR")
            hou.ui.displayMessage(f"Workspace does not exist: {workspace}",
                                severity=hou.severityType.Error)
            return
           
        # Clear existing editor
        if self.editor:
            logger.log("Cleaning up existing editor")
            self.editor_layout.removeWidget(self.editor)
            self.editor.deleteLater()
            self.editor = None
           
        if self.lsp_client:
            logger.log("Shutting down existing LSP client")
            self.lsp_client.shutdown()
            self.lsp_client = None
           
        # Remove placeholder
        if self.placeholder_label:
            self.editor_layout.removeWidget(self.placeholder_label)
            self.placeholder_label.deleteLater()
            self.placeholder_label = None
           
        # Start new LSP client
        self.status_label.setText("Starting LSP server...")
        QtWidgets.QApplication.processEvents()
       
        try:
            cmd_parts = server_cmd.split()
            logger.log(f"Creating LSP client with command: {cmd_parts}")
           
            self.lsp_client = LSPClient(cmd_parts, workspace)
           
            if not self.lsp_client.start():
                raise Exception("Failed to start LSP server process")
           
            logger.log(f"Loading file: {file_path}")
            with open(file_path, 'r') as f:
                content = f.read()
            logger.log(f"File loaded: {len(content)} characters")
           
            logger.log("Creating editor widget")
            self.editor = LSPCodeEditor(self.lsp_client, file_path)
            self.editor.setPlainText(content)
            self.editor.diagnosticsChanged.connect(self.info_view.update_diagnostics)
            self.editor_layout.addWidget(self.editor)
           
            self.status_label.setText(f"LSP running: {server_cmd} | {file_path}")
            self.start_btn.setText("Restart LSP")
            logger.log("=== LSP Started Successfully ===")
           
        except Exception as e:
            logger.log(f"Error starting LSP: {e}", "ERROR")
            hou.ui.displayMessage(f"Error starting LSP: {str(e)}",
                                severity=hou.severityType.Error)
            self.status_label.setText(f"Error: {str(e)}")
       
    def closeEvent(self, event):
        logger.log("Panel closing")
        if self.lsp_client:
            self.lsp_client.shutdown()
        super().closeEvent(event)


def onCreateInterface():
    """Required function for Houdini Python Panel"""
    return LSPEditorPanel()
