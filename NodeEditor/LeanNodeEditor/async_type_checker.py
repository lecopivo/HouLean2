"""
Asynchronous type checker client for Lean server integration.
Handles non-blocking communication with the Lean server via stdin/stdout.
"""

import json
import threading
import time
from typing import Callable, Optional, Dict, Any


class TypeCheckRequest:
    """Represents a pending type check request."""
    
    def __init__(self, graph_data, callback, request_id=None):
        self.graph_data = graph_data
        self.callback = callback
        self.request_id = request_id or str(time.time())
        self.timestamp = time.time()


class AsyncTypeChecker:
    """Non-blocking client for Lean type checking server."""
    
    def __init__(self, server_process):
        """
        Initialize the async type checker.
        
        Args:
            server_process: The subprocess.Popen object for the Lean server
        """
        self.server_process = server_process
        self.pending_requests = {}
        self.request_lock = threading.Lock()
        self.worker_thread = None
        self.reader_thread = None
        self.is_running = False
        self.request_queue = []
        self.queue_lock = threading.Lock()
        self.response_callbacks = {}
        self.callback_lock = threading.Lock()
        self.request_counter = 0
        
    def start(self):
        """Start the async worker threads."""
        if self.is_running:
            return
            
        self.is_running = True
        
        # Thread to write requests
        self.worker_thread = threading.Thread(target=self._writer_loop, daemon=True)
        self.worker_thread.start()
        
        # Thread to read responses
        self.reader_thread = threading.Thread(target=self._reader_loop, daemon=True)
        self.reader_thread.start()
        
    def stop(self):
        """Stop the async worker threads."""
        self.is_running = False
        if self.worker_thread:
            self.worker_thread.join(timeout=2.0)
        if self.reader_thread:
            self.reader_thread.join(timeout=2.0)
            
    def check_types(self, graph_data, callback):
        """
        Submit a type check request asynchronously.
        
        Args:
            graph_data: Dictionary containing the node graph data
            callback: Function to call with results (response_data) or error (None)
        
        Returns:
            request_id: Unique identifier for this request
        """
        with self.request_lock:
            self.request_counter += 1
            request_id = "req_{}".format(self.request_counter)
        
        req = TypeCheckRequest(graph_data, callback, request_id)
        
        with self.queue_lock:
            self.request_queue.append(req)
            
        with self.callback_lock:
            self.response_callbacks[request_id] = callback
            
        return req.request_id
        
    def _writer_loop(self):
        """Background worker that writes queued requests to server stdin."""
        while self.is_running:
            request = None
            
            with self.queue_lock:
                if self.request_queue:
                    request = self.request_queue.pop(0)
                    
            if request:
                self._send_request(request)
            else:
                time.sleep(0.05)  # Sleep briefly if queue is empty
                
    def _reader_loop(self):
        """Background worker that reads responses from server stdout."""
        while self.is_running:
            try:
                if not self.server_process or self.server_process.poll() is not None:
                    print("Server process died")
                    break
                    
                # Read one line (one JSON response)
                line = self.server_process.stdout.readline()
                
                if not line:
                    # EOF or empty line
                    time.sleep(0.05)
                    continue
                    
                line = line.strip()
                if not line:
                    continue
                    
                # Parse response
                try:
                    response = json.loads(line)
                    self._handle_response(response)
                except json.JSONDecodeError as e:
                    print("Failed to parse server response: {}".format(e))
                    print("Raw line: {}".format(line))
                    
            except Exception as e:
                print("Error reading from server: {}".format(e))
                time.sleep(0.1)
                
    def _send_request(self, req):
        """Send a type check request to the server."""
        try:
            if not self.server_process or self.server_process.poll() is not None:
                print("Cannot send request - server not running")
                if req.callback:
                    req.callback(None)
                return
                
            # Prepare the request payload
            # Note: We're not including request_id in the protocol since the
            # original server doesn't support it. We'll assume responses come
            # back in order (FIFO).
            payload = {
                "typecheck": {
                    "data" : req.graph_data
                }
            }
            
            json_line = json.dumps(payload) + '\n'
            
            # Write to server stdin
            self.server_process.stdin.write(json_line)
            self.server_process.stdin.flush()
            
        except Exception as e:
            print("Failed to send request: {}".format(e))
            if req.callback:
                req.callback(None)
                
    def _handle_response(self, response):
        """Handle a response from the server."""
        try:
            # Since the server responds in FIFO order and we don't have
            # request IDs in the protocol, we'll just call the first
            # pending callback
            
            callback = None
            with self.callback_lock:
                if self.response_callbacks:
                    # Get the first (oldest) callback
                    request_id = next(iter(self.response_callbacks))
                    callback = self.response_callbacks.pop(request_id)
            
            if callback:
                callback(response)
            else:
                print("Received response but no callback waiting")
                
        except Exception as e:
            print("Error handling response: {}".format(e))
                
    def cancel_pending(self):
        """Cancel all pending requests."""
        with self.queue_lock:
            self.request_queue.clear()
        with self.callback_lock:
            self.response_callbacks.clear()
