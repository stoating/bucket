#!/usr/bin/env bash
set -euo pipefail

# Script to copy config.toml to the ~/.codex directory

SOURCE_FILE="gen/config.toml"
CONFIG_PATH="$HOME/.codex/config.toml"

# Check if source file exists
if [[ ! -f "$SOURCE_FILE" ]]; then
    echo "❌ Error: $SOURCE_FILE not found"
    echo "   Run 'codex-conf' to generate the config"
    exit 1
fi

echo "🔧 Copying Codex configuration..."
echo "   Source: $SOURCE_FILE"
echo "   Target: $CONFIG_PATH"

# Ensure target directory exists
mkdir -p "$(dirname "$CONFIG_PATH")"

# Create backup if config already exists
if [[ -f "$CONFIG_PATH" ]]; then
    TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
    BACKUP_PATH="${CONFIG_PATH%.toml}_${TIMESTAMP}.toml"
    echo "📦 Backing up existing config to:"
    echo "   $BACKUP_PATH"
    cp "$CONFIG_PATH" "$BACKUP_PATH"
fi

# Copy new config
cp "$SOURCE_FILE" "$CONFIG_PATH"

echo "✅ Successfully copied Codex configuration!"
echo ""
echo "📋 Next steps:"
echo "   1. Restart Codex perhaps by restarting VSCode"
echo "   2. You should now see Clojure MCP tools available in Codex"
