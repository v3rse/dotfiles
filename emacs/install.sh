#!/bin/bash
# Stow emacs config — pre-creates the config directory so stow doesn't fold
# it into a symlink (which would cause runtime state files to leak into dotfiles).
# Run from ~/dotfiles: ./emacs/install.sh

set -euo pipefail

DOTFILES="${DOTFILES:-$HOME/dotfiles}"

# Pre-create the directory so stow won't fold it into a symlink
mkdir -p ~/.config/emacs-configs/default

echo "==> Stowing emacs..."
cd "$DOTFILES" && stow -t ~ emacs

echo "==> Done."
