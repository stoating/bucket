#!/usr/bin/env bash
set -euo pipefail

DEV="$1"

CONTAINER_NAMES=(
  bucket
)

echo "Stopping known containers in parallel..."
pids=()
for name in "${CONTAINER_NAMES[@]}"; do
  if podman ps --format '{{.Names}}' | grep -qx "$name"; then
    echo " - stopping $name"
    podman stop --time=10 "$name" >/dev/null 2>&1 &   # run in background
    pids+=($!)
  else
    echo " - $name not running"
  fi
done

# wait for all stop operations to finish
for pid in "${pids[@]}"; do
  wait "$pid" || true
done

echo "Done stopping containers."

# Function to find and kill process on a port
kill_port() {
  local port=$1
  echo "Checking for process on port $port..."

  local pid=""

  # Method 1: Try lsof
  if command -v lsof >/dev/null 2>&1; then
    pid=$(lsof -ti tcp:$port 2>/dev/null || true)
    if [ -n "${pid:-}" ]; then
      echo "  Found process $pid on port $port (via lsof)"
      kill -9 $pid 2>/dev/null || true
      return
    fi
  fi

  # Method 2: Try fuser
  if command -v fuser >/dev/null 2>&1; then
    pid=$(fuser ${port}/tcp 2>/dev/null | tr -d ' ' || true)
    if [ -n "${pid:-}" ]; then
      echo "  Found process $pid on port $port (via fuser)"
      kill -9 $pid 2>/dev/null || true
      return
    fi
  fi

  # Method 3: Try ss (more common on modern Linux)
  if command -v ss >/dev/null 2>&1; then
    # Extract PID from ss output
    pid=$(ss -tulpn 2>/dev/null | grep ":$port " | sed -n 's/.*pid=\([0-9]*\).*/\1/p' || true)
    if [ -n "${pid:-}" ]; then
      echo "  Found process $pid on port $port (via ss)"
      kill -9 $pid 2>/dev/null || true
      return
    fi
  fi

  # Method 4: Try netstat (cross-platform but needs parsing)
  if command -v netstat >/dev/null 2>&1; then
    # Linux with -p flag (requires sudo for PID)
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
      pid=$(sudo netstat -tulpn 2>/dev/null | grep ":$port " | awk '{print $7}' | cut -d'/' -f1 || true)
      if [ -n "${pid:-}" ] && [ "${pid}" != "-" ]; then
        echo "  Found process $pid on port $port (via netstat)"
        kill -9 $pid 2>/dev/null || true
        return
      fi
    fi
  fi

  # If we get here, couldn't find process with any method
  echo "  No process found on port $port (or unable to detect)"
}

# Check and kill processes on known ports
for port in 7080 8080; do
  kill_port $port
done

# Final cleanup - remove any stopped containers
echo ""
echo "Removing stopped containers..."
for name in "${CONTAINER_NAMES[@]}"; do
  if podman ps -a --format '{{.Names}}' | grep -qx "$name"; then
    echo " - removing $name"
    podman rm -f "$name" >/dev/null 2>&1 || true
  fi
done

echo ""
echo "Cleanup complete."
