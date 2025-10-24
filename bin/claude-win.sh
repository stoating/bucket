#!/usr/bin/env bash
set -euo pipefail

# Create gen directory if it doesn't exist
mkdir -p gen

OUT="gen/claude_desktop_config.json"

# Resolve nix-store binaries (inside devenv / WSL)
MCP_PROXY="$(command -v mcp-proxy)"
BASH_BIN="$(command -v bash)"

# Define JSON snippets for each server
server_in_cont=$(cat <<JSON
    "clojure-mcp": {
      "command": "wsl.exe",
      "args": [
        "${BASH_BIN}",
        "-c",
        "${MCP_PROXY} http://localhost:7080/sse"
      ]
    }
JSON
)

# Collect servers in correct order
servers="$server_in_cont"

# Write final JSON
cat > "$OUT" <<JSON
{
  "mcpServers": {
$servers
  }
}
JSON

echo "Wrote Windows Claude Desktop config to $OUT"
echo "  mcp-proxy : $MCP_PROXY"

# Copy config to OS-specific location
echo "Copy config to destination..."
bash ./bin/copy-claude-config.sh