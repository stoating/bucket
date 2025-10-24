#!/usr/bin/env bash
set -euo pipefail

# Script to copy mcp.json to the .vscode directory in project root

SOURCE_FILE="gen/mcp.json"
CONFIG_PATH=".vscode/mcp.json"

# Check if source file exists
if [[ ! -f "$SOURCE_FILE" ]]; then
    echo "❌ Error: $SOURCE_FILE not found"
    echo "   Run 'copilot' to generate the config"
    exit 1
fi

echo "🔧 Copying Copilot configuration..."
echo "   Source: $SOURCE_FILE"
echo "   Target: $CONFIG_PATH"

# Ensure target directory exists
mkdir -p "$(dirname "$CONFIG_PATH")"

# Create backup if config already exists
if [[ -f "$CONFIG_PATH" ]]; then
    TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
    BACKUP_PATH="${CONFIG_PATH%.json}_${TIMESTAMP}.json"
    echo "📦 Backing up existing config to:"
    echo "   $BACKUP_PATH"
    cp "$CONFIG_PATH" "$BACKUP_PATH"
fi

# Copy new config
cp "$SOURCE_FILE" "$CONFIG_PATH"

echo "✅ Successfully copied Copilot configuration!"
echo ""
echo "📋 Next steps:"
echo "   1. Restart Copilot open mcp.json and select reload above the server"
echo "   2. You should now see Clojure MCP tools available in Copilot"
