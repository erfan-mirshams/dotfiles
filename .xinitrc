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

# Start the GPG agent
if [ -f "${HOME}/.gpg-agent-info" ]; then
    . "${HOME}/.gpg-agent-info"
    export GPG_AGENT_INFO
    export SSH_AUTH_SOCK
fi

if ! gpg-connect-agent --quiet /bye > /dev/null 2>&1; then
    gpg-agent --daemon --enable-ssh-support --write-env-file "${HOME}/.gpg-agent-info" > /dev/null 2>&1
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
emacs --daemon &

# CapsLock to Esc
xmodmap -e 'clear Lock' & #ensures you're not stuck in CAPS on mode
xmodmap -e 'keycode 0x42=Escape' & #remaps the keyboard so CAPS LOCK=ESC

# Loop
while true; do
	dwm >/dev/null 2>&1
done

# Execute DWM
exec dwm
