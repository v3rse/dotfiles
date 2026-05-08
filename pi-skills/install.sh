#!/bin/bash
# Install pi skills and symlink them for Claude Code
# Run from ~/dotfiles: ./pi-skills/install.sh

set -euo pipefail

DOTFILES="${DOTFILES:-$HOME/dotfiles}"

echo "==> Stowing pi-skills..."
cd "$DOTFILES" && stow -t ~ pi-skills

echo "==> Linking skills for Claude Code..."
mkdir -p ~/.claude/skills
for skill in blindspots tech-catchup wiki-builder; do
  target="$HOME/.claude/skills/$skill"
  source="$DOTFILES/pi-skills/.pi/agent/skills/$skill"
  
  if [ -L "$target" ] && [ "$(readlink "$target")" = "$source" ]; then
    echo "    $skill: already linked"
  else
    ln -sf "$source" "$target"
    echo "    $skill: linked"
  fi
done

echo "==> Done. Skills available in both ~/.pi/agent/skills/ and ~/.claude/skills/"
