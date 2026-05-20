#!/bin/bash
# Install pi skills and symlink them globally
# Run from ~/dotfiles: ./pi-skills/install.sh

set -euo pipefail

DOTFILES="${DOTFILES:-$HOME/dotfiles}"

echo "==> Stowing pi-skills..."
cd "$DOTFILES" && stow -t ~ pi-skills

link_skills() {
  local dest="$1"
  mkdir -p "$dest"
  echo "==> Linking skills to $dest..."
  for source in "$DOTFILES"/pi-skills/.pi/agent/skills/*/; do
    skill=$(basename "$source")
    target="$dest/$skill"
    if [ -L "$target" ] && [ "$(readlink "$target")" = "$source" ]; then
      echo "    $skill: already linked"
    else
      ln -sf "$source" "$target"
      echo "    $skill: linked"
    fi
  done
}

link_skills "$HOME/.agents/skills"
link_skills "$HOME/.claude/skills"

echo "==> Done. Skills available in ~/.pi/agent/skills/, ~/.agents/skills/ (+ Gemini alias), ~/.claude/skills/"
