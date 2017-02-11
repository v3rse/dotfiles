#!/bin/bash
# Export my scripts
PATH+=":$HOME/bin"
export PATH

# Merge custom XResource
xrdb -merge "${HOME}/.Xdefaults" &
xrdb -I "${HOME}/.XResources"

# Custom Keymappings
xmodmap $HOME/.xmodmap

# Start URXVT emulator daemon
urxvtd -q -f -o &

# Use previously generated wal color scheme 
wal -i "$(< "${HOME}/.cache/wal/wal")"

# Compton (Composite manager...Enter the league of shadows!!)
compton -b --config $HOME/.config/compton.conf &

exec i3 -V >> ~/logs/i3log-$(date +'%F-%k-%M-%S') 2>&1
