#!/bin/bash
#Load XResource
xrdb -I $HOME/.XResources

xmodmap $HOME/.xmodmap

exec i3 -V >> ~/logs/i3log-$(date +'%F-%k-%M-%S') 2>&1

