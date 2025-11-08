"""
Manages the Lean type checker server process lifecycle.
"""

import subprocess
import time
import os
import signal


class ServerManager:
    """Manages starting and stopping the Lean type checker server."""
    
    def __init__(self, server_executable, port=8080):
        """
        Initialize server manager.
        
        Args:
            server_executable: Path to the Lean server executable
            port: Port number for the server
        """
        self.server_executable = server_executable
        self.port = port
        self.process = None
        self.server_url = "http://localhost:{}".format(port)
        
    def start(self, timeout=5.0):
        """
        Start the Lean server process.
        
        Args:
            timeout: Maximum time to wait for server to become ready
            
        Returns:
            True if server started successfully, False otherwise
        """
        if self.is_running():
            print("Server already running")
            return True
            
        try:
            # Start the server process
            self.process = subprocess.Popen(
                [self.server_executable, "-server"],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                bufsize=0
            )
            
            # Wait for server to be ready
            start_time = time.time()
            while time.time() - start_time < timeout:
                if self._check_server_ready():
                    print("Lean type checker server started on {}".format(self.server_url))
                    return True
                time.sleep(0.2)
                
            # Timeout waiting for server
            print("Timeout waiting for server to start")
            self.stop()
            return False
            
        except Exception as e:
            print("Failed to start server: {}".format(e))
            self.stop()
            return False
            
    def stop(self):
        """Stop the Lean server process."""
        if not self.process:
            return
            
        try:
            # Try graceful shutdown first by sending quit command
            if self.process.poll() is None:  # Process still running
                try:
                    quit_cmd = '{"command": "quit", "data": null}\n'
                    self.process.stdin.write(quit_cmd.encode('utf-8'))
                    self.process.stdin.flush()
                    
                    # Wait briefly for graceful shutdown
                    self.process.wait(timeout=2.0)
                except:
                    pass
                    
            # Force kill if still running
            if self.process.poll() is None:
                if os.name == 'nt':  # Windows
                    self.process.kill()
                else:  # Unix
                    os.kill(self.process.pid, signal.SIGTERM)
                    
                self.process.wait(timeout=1.0)
                
            print("Lean type checker server stopped")
            
        except Exception as e:
            print("Error stopping server: {}".format(e))
        finally:
            self.process = None
            
    def is_running(self):
        """Check if the server process is running."""
        if not self.process:
            return False
        return self.process.poll() is None
        
    def _check_server_ready(self):
        """Check if server is ready to accept requests."""
        if not self.is_running():
            return False
            
        try:
            # Try sending a simple test request
            import json
            try:
                from urllib import request as urllib_request
            except ImportError:
                import urllib2 as urllib_request
                
            test_payload = {
                "command": "typecheck",
                "data": {"nodes": [], "connections": []}
            }
            
            req = urllib_request.Request(
                self.server_url,
                data=json.dumps(test_payload).encode('utf-8'),
                headers={'Content-Type': 'application/json'}
            )
            
            response = urllib_request.urlopen(req, timeout=1.0)
            return response.getcode() == 200
            
        except:
            return False
            
    def restart(self):
        """Restart the server."""
        self.stop()
        time.sleep(0.5)
        return self.start()
        
    def __del__(self):
        """Cleanup on deletion."""
        self.stop()
