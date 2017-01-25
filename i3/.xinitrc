#!/bin/bash
exec i3 -V >> ~/logs/i3log-$(date + '%F-%k-%M-%S') 2>&1

#custom keymappings
xmodmap ~/.xmodmap
