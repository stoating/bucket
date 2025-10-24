#!/usr/bin/env bash
set -euo pipefail

echo "Pulling base Clojure image..."
podman pull docker.io/library/clojure:temurin-21-tools-deps

echo ""
echo "Starting containers sequentially..."
echo "================================"

echo ""
echo "4. Building image and running container..."
./devenv/container/image-build.sh
./devenv/container/run-container.sh &
sleep 2

echo ""
echo "================================"
echo "All containers started!"
echo ""
echo "Container status:"
podman ps --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"

echo ""
echo "To check logs for a specific container:"
echo "  podman logs <container-name>"
echo ""
echo "To stop all containers:"
echo "  stop"
echo ""
