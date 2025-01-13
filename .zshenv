# Adding .local/bin to $PATH
export PATH=$PATH:/home/erfan/.local/share/go/bin:/home/erfan/.local/share/cargo/bin:/opt/jetbrains-toolbox-2.3.2.31487:/usr/lib/go/bin:/home/erfan/.local/share/go:/home/erfan/.config/emacs/bin:/home/erfan/.local/bin:/home/erfan/.local/bin/statusbar:/home/erfan/.config/pyenv:/opt/Cliq:/usr/lib/emscripten

# Default programs
export EDITOR="nvim"
export TERMINAL="st"
export BROWSER="firefox"

# HighDPI scaling for 1080p display
export GDK_SCALE=1
export GDK_DPI_SCALE=0.9

export QT_AUTO_SCREEN_SET_FACTOR=0
export QT_SCALE_FACTOR=1.5        # Scale Qt applications by 1.5
export QT_FONT_DPI=96             # Use standard DPI for Qt applications

# batman
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

# ~/ clean-up
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_DATA_DIRS="/usr/local/share:/usr/share"
export XDG_CONFIG_DIRS="/etc/xdg"
export XDG_RUNTIME_DIR="/run/user/$UID"
export ZDOTDIR=$HOME/.config/zsh
export WGETRC="$XDG_CONFIG_HOME/wgetrc"
export GOPATH="$XDG_DATA_HOME"/go
export GNUPGHOME="$XDG_DATA_HOME"/gnupg
export XAUTHORITY="$XDG_RUNTIME_DIR"/Xauthority
export PASSWORD_STORE_DIR="$XDG_DATA_HOME"/pass
export TEXMFHOME=$XDG_DATA_HOME/texmf
export TEXMFVAR=$XDG_CACHE_HOME/texlive/texmf-var
export TEXMFCONFIG=$XDG_CONFIG_HOME/texlive/texmf-config
export LEIN_HOME="$XDG_DATA_HOME"/lein 
export CARGO_HOME="$XDG_DATA_HOME"/cargo 
export LESSHISTFILE="-"
export PYENV_ROOT="$XDG_DATA_HOME"/pyenv
export MBSYNCRC=~/.config/mbsync/mbsyncrc
. "/home/erfan/.local/share/cargo/env"

# divar
export AWS_ENDPOINT=https://s3.thr1.sotoon.ir
