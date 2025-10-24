#!/usr/bin/env bash
set -euo pipefail

# Create gen directory if it doesn't exist
mkdir -p gen

OUT="gen/config.toml"

# Resolve nix-store binaries (inside devenv / WSL)
MCP_PROXY="$(command -v mcp-proxy)"

cat > "$OUT" <<EOF
[mcp_servers.clj-mcp-container-proxy]
type = "stdio"
command = "$MCP_PROXY"
args = ["http://localhost:7080/sse"]
EOF


echo "Wrote Codex config to $OUT"
echo "  mcp-proxy : $MCP_PROXY"

# Copy config to Codex location
echo "Copy config to destination..."
bash ./bin/copy-codex-config.sh