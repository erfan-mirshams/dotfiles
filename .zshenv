# Adding .local/bin to $PATH
export PATH=/usr/lib/go/bin:/home/erfan/.local/share/go:/home/erfan/.config/emacs/bin:/home/erfan/.local/bin:/home/erfan/.local/bin/statusbar:$PATH

# Default programs
export EDITOR="nvim"
export TERMINAL="st"
export BROWSER="firefox"

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
export LESSHISTFILE="-"

