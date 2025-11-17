"""
Manages the Lean type checker server process lifecycle.
The server communicates via stdin/stdout, not HTTP.
"""

import subprocess
import time
import os
import signal
import json


class ServerManager:
    """Manages starting and stopping the Lean type checker server."""

    def __init__(self, server_executable, workspace_dir):
        """
        Initialize server manager.

        Args:
            server_executable: Path to the Lean server executable
        """
        self.server_executable = server_executable
        self.workspace_dir = workspace_dir
        self.process = None

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
            print("Starting Lean server: {}".format(self.server_executable))

            # Start the server process with sditn/stdout pipes
            self.process = subprocess.Popen(
                [self.server_executable, "-server", "-w", self.workspace_dir],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                bufsize=0,
                text=True  # Use text mode for JSON
            )

            print("Server process started with PID: {}".format(self.process.pid))

            # Give it a moment to initialize
            time.sleep(0.5)

            # Check if process crashed immediately
            if self.process.poll() is not None:
                _, stderr = self.process.communicate(timeout=1.0)
                print("Server process exited immediately!")
                print("STDERR:", stderr)
                self.process = None
                return False

            print("Server process running, testing with ping...")

            # Test server with a simple request
            if self._test_server():
                print("Lean type checker server started successfully")
                return True
            else:
                print("Server not responding to test request")
                self.stop()
                return False

        except Exception as e:
            print("Failed to start server: {}".format(e))
            import traceback
            traceback.print_exc()
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
                    quit_cmd = json.dumps("quit") + '\n'
                    self.process.stdin.write(quit_cmd)
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

    def _test_server(self):
        """Test if server is responding."""
        if not self.is_running():
            return False

        try:
            # Send a test typecheck request with empty graph
            test_payload = "ping"

            print(f"sending test request {test_payload}")

            request_line = json.dumps(test_payload) + '\n'
            self.process.stdin.write(request_line)
            self.process.stdin.flush()

            # Try to read response (with timeout)
            import select
            if hasattr(select, 'select'):
                # Unix-like systems
                ready, _, _ = select.select([self.process.stdout], [], [], 2.0)
                if ready:
                    response_line = self.process.stdout.readline()
                    if response_line:
                        print(f"recievwd {response_line}")
                        response = json.loads(response_line)
                        return response == "pong"
            else:
                # Windows - just try to read with timeout
                # This is less reliable but works
                response_line = self.process.stdout.readline()
                if response_line:
                    response = json.loads(response_line)
                    return response == "pong"

            return False

        except Exception as e:
            print("Server test failed: {}".format(e))
            return False

    def restart(self):
        """Restart the server."""
        self.stop()
        time.sleep(0.5)
        return self.start()

    def __del__(self):
        """Cleanup on deletion."""
        self.stop()
