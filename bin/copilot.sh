#!/usr/bin/env bash
set -euo pipefail

# Create gen directory if it doesn't exist
mkdir -p gen

OUT="gen/mcp.json"

# Resolve nix-store binaries (inside devenv / WSL)
MCP_PROXY="$(command -v mcp-proxy)"
BASH_BIN="$(command -v bash)"

# Define JSON snippets for each server
server_in_cont=$(cat <<JSON
    "clj-mcp-container-proxy": {
      "command": "${MCP_PROXY}",
      "args": ["http://localhost:7080/sse"]
    }
JSON
)

# Collect servers in correct order
servers="$server_in_cont"

# Write final JSON
cat > "$OUT" <<JSON
{
  "servers": {
$servers
  }
}
JSON

echo "Wrote Copilot config to $OUT"
echo "  mcp-proxy : $MCP_PROXY"

# Copy config to OS-specific location
echo "Copy config to destination..."
bash ./bin/copy-copilot-config.sh