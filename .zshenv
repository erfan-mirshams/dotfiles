# Adding .local/bin to $PATH
export PATH=$PATH:/home/erfan/.local/share/go/bin:/home/erfan/.local/share/cargo/bin:/opt/jetbrains-toolbox-2.0.4.17212:/usr/lib/go/bin:/home/erfan/.local/share/go:/home/erfan/.config/emacs/bin:/home/erfan/.local/bin:/home/erfan/.local/bin/statusbar

# Default programs
export EDITOR="nvim"
export TERMINAL="st"
export BROWSER="firefox"

# HighDPI scaling
export GDK_SCALE=2
export GDK_DPI_SCALE=0.5
export QT_AUTO_SCREEN_SET_FACTOR=0
export QT_SCALE_FACTOR=2
export QT_FONT_DPI=96

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

