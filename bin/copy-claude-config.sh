#!/usr/bin/env bash
set -euo pipefail

# Script to copy claude_desktop_config.json to the OS-specific location

SOURCE_FILE="gen/claude_desktop_config.json"

# Default value
OS_TYPE="Unknown"

detect_os() {
    local uname_str
    uname_str="$(uname -s)"

    if [[ "$uname_str" == "Linux" ]]; then
        if grep -qi "microsoft" /proc/version 2>/dev/null || \
           grep -qi "WSL" /proc/sys/kernel/osrelease 2>/dev/null; then
            OS_TYPE="WSL"
        else
            OS_TYPE="Linux"
        fi
    elif [[ "$uname_str" == "Darwin" ]]; then
        OS_TYPE="macOS"
    fi
}

# Function to get config path
get_config_path() {
    if [[ "$OS_TYPE" == "WSL" ]]; then
        # Windows (WSL) - Ask user for their Windows username
        read -p "Enter your Windows username: " windows_user
        if [[ -z "$windows_user" ]]; then
            echo "❌ Error: Windows username cannot be empty"
            exit 1
        fi
        echo "/mnt/c/users/${windows_user}/appdata/roaming/claude/claude_desktop_config.json"
    elif [[ "$OS_TYPE" == "macOS" ]]; then
        echo "$HOME/Library/Application Support/Claude/claude_desktop_config.json"
    elif [[ "$OS_TYPE" == "Linux" ]]; then
        echo "$HOME/.config/Claude/claude_desktop_config.json"
    else
        echo "❌ Error: Unsupported operating system"
        exit 1
    fi
}

# Detect OS
detect_os
echo "🔍 Detected OS: $OS_TYPE"

# Check if source file exists
if [[ ! -f "$SOURCE_FILE" ]]; then
    echo "❌ Error: $SOURCE_FILE not found"
    echo "   Run 'claude' or 'claude-win' to generate the config"
    exit 1
fi

# Get config path and directory
CONFIG_PATH="$(get_config_path)"

echo "🔧 Copying Claude Desktop configuration..."
echo "   Source: $SOURCE_FILE"
echo "   Target: $CONFIG_PATH"

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

echo "✅ Successfully copied Claude Desktop configuration!"
echo ""
echo "📋 Next steps:"
echo "   1. Restart Claude Desktop (or use View -> Reload)"
echo "   2. You should now see Clojure MCP tools available in Claude"
