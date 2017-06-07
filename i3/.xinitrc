# !/bin/sh
[ -f /etc/xprofile ] && . /etc/xprofile
[ -f ~/.xprofile ] && . ~/.xprofile

# start i3wm
exec i3 -V >> ~/logs/i3log-$(date +'%F-%k-%M-%S') 2>&1
