#!/bin/bash
# Merge custom XResource
xrdb -merge "${HOME}/.Xdefaults" &

# Custom Keymappings
xmodmap $HOME/.xmodmap

# Start URXVT emulator daemon
urxvtd -q -f -o &

# Use previously generated wal color scheme 
wal -i "$(< "${HOME}/.cache/wal/wal")"

exec i3 -V >> ~/logs/i3log-$(date +'%F-%k-%M-%S') 2>&1
