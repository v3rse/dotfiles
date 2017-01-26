#!/bin/bash
#custom keymappings
+xmodmap ~/.xmodmap

exec i3 -V >> ~/logs/i3log-$(date + '%F-%k-%M-%S') 2>&1

