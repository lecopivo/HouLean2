"""
Asynchronous type checker client for Lean server integration.
Handles non-blocking HTTP requests to the Lean type checking server.
"""

import json
import threading
import time
from typing import Callable, Optional, Dict, Any
try:
    from urllib import request as urllib_request, error as urllib_error
except ImportError:
    import urllib2 as urllib_request
    import urllib2 as urllib_error


class TypeCheckRequest:
    """Represents a pending type check request."""
    
    def __init__(self, graph_data, callback, request_id=None):
        self.graph_data = graph_data
        self.callback = callback
        self.request_id = request_id or str(time.time())
        self.timestamp = time.time()


class AsyncTypeChecker:
    """Non-blocking client for Lean type checking server."""
    
    def __init__(self, server_url="http://localhost:8080"):
        self.server_url = server_url
        self.pending_requests = {}
        self.request_lock = threading.Lock()
        self.worker_thread = None
        self.is_running = False
        self.request_queue = []
        self.queue_lock = threading.Lock()
        
    def start(self):
        """Start the async worker thread."""
        if self.is_running:
            return
            
        self.is_running = True
        self.worker_thread = threading.Thread(target=self._worker_loop, daemon=True)
        self.worker_thread.start()
        
    def stop(self):
        """Stop the async worker thread."""
        self.is_running = False
        if self.worker_thread:
            self.worker_thread.join(timeout=2.0)
            
    def check_types(self, graph_data, callback):
        """
        Submit a type check request asynchronously.
        
        Args:
            graph_data: Dictionary containing the node graph data
            callback: Function to call with results (response_data) or error (None)
        
        Returns:
            request_id: Unique identifier for this request
        """
        req = TypeCheckRequest(graph_data, callback)
        
        with self.queue_lock:
            self.request_queue.append(req)
            
        return req.request_id
        
    def _worker_loop(self):
        """Background worker that processes queued requests."""
        while self.is_running:
            request = None
            
            with self.queue_lock:
                if self.request_queue:
                    request = self.request_queue.pop(0)
                    
            if request:
                self._process_request(request)
            else:
                time.sleep(0.1)  # Sleep briefly if queue is empty
                
    def _process_request(self, req):
        """Process a single type check request."""
        try:
            # Prepare the request payload
            payload = {
                "command": "typecheck",
                "data": req.graph_data
            }
            
            json_data = json.dumps(payload)
            
            # Make HTTP request to server
            http_req = urllib_request.Request(
                self.server_url,
                data=json_data.encode('utf-8'),
                headers={'Content-Type': 'application/json'}
            )
            
            response = urllib_request.urlopen(http_req, timeout=10.0)
            response_data = response.read().decode('utf-8')
            
            # Parse response
            result = json.loads(response_data)
            
            # Call callback with result
            if req.callback:
                req.callback(result)
                
        except urllib_error.URLError as e:
            print("Type checker network error: {}".format(e))
            if req.callback:
                req.callback(None)
                
        except Exception as e:
            print("Type checker error: {}".format(e))
            if req.callback:
                req.callback(None)
                
    def cancel_pending(self):
        """Cancel all pending requests."""
        with self.queue_lock:
            self.request_queue.clear()
