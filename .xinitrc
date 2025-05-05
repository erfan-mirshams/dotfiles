#!/bin/sh

userresources=$XDG_CONFIG_HOME/xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# DWM Status
dwmblocks &

# Compositor
picom -c &

# Background
chbg &

# Start Emacs Daemon
emacs --init-directory=$XDG_CONFIG_HOME/vanilla-emacs --daemon &

# dwm wrapper around clipboard
clipmenud &

# Automatic screen locking with xidlehook
xidlehook --not-when-fullscreen --not-when-audio --timer 400 "slock" "" &

# Loop
while true; do
	dwm >/dev/null 2>&1
done
