#!/usr/bin/env bash

set -euo pipefail

REPO="https://github.com/mehdignu/dotfiles.git"
EMACSD="$HOME/.emacs.d"
BACKUP="$HOME/.emacs.d.backup.$(date +%Y%m%d%H%M%S)"

# Backup existing
if [ -d "$EMACSD" ]; then
  echo "Backing up existing ~/.emacs.d to $BACKUP..."
  mv "$EMACSD" "$BACKUP"
fi

echo "Cloning dotfiles repo into ~/.emacs.d..."
git clone "$REPO" "$EMACSD"

echo "Done. ~/.emacs.d is now a git clone of your dotfiles."
echo "You can now do: cd ~/.emacs.d && git pull to update."
