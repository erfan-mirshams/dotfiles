# History configuration
HISTFILE=~/.cache/zsh/history
HISTSIZE=10000
SAVEHIST=10000

# Create cache directories if they don't exist
[[ ! -d ~/.cache/zsh ]] && mkdir -p ~/.cache/zsh

# Fast compinit with cache
autoload -Uz compinit
if [[ -n ~/.cache/zsh/zcompdump-$ZSH_VERSION(#qN.mh+24) ]]; then
  compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION
else
  compinit -C -d ~/.cache/zsh/zcompdump-$ZSH_VERSION
fi
zstyle ':completion:*' menu select cache-path $XDG_CACHE_HOME/zsh/zcompcache
zmodload zsh/complist
_comp_options+=(globdots)  # Include hidden files

# Emacs mode
bindkey -e
echo -ne '\e[5 q' # Beam cursor
preexec() { echo -ne '\e[5 q' ;}

# Load aliases
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"

# Keybindings setup - for terminal compatibility
typeset -g -A key
key[Home]="${terminfo[khome]}"
key[End]="${terminfo[kend]}"
key[Insert]="${terminfo[kich1]}"
key[Backspace]="${terminfo[kbs]}"
key[Delete]="${terminfo[kdch1]}"
key[Up]="${terminfo[kcuu1]}"
key[Down]="${terminfo[kcud1]}"
key[Left]="${terminfo[kcub1]}"
key[Right]="${terminfo[kcuf1]}"
key[PageUp]="${terminfo[kpp]}"
key[PageDown]="${terminfo[knp]}"
key[Shift-Tab]="${terminfo[kcbt]}"
key[Control-Left]="${terminfo[kLFT5]}"
key[Control-Right]="${terminfo[kRIT5]}"

# Setup default keybindings if terminal supports them
[[ -n "${key[Home]}"      ]] && bindkey -- "${key[Home]}"       beginning-of-line
[[ -n "${key[End]}"       ]] && bindkey -- "${key[End]}"        end-of-line
[[ -n "${key[Insert]}"    ]] && bindkey -- "${key[Insert]}"     overwrite-mode
[[ -n "${key[Backspace]}" ]] && bindkey -- "${key[Backspace]}"  backward-delete-char
[[ -n "${key[Delete]}"    ]] && bindkey -- "${key[Delete]}"     delete-char
[[ -n "${key[Left]}"      ]] && bindkey -- "${key[Left]}"       backward-char
[[ -n "${key[Right]}"     ]] && bindkey -- "${key[Right]}"      forward-char
[[ -n "${key[PageUp]}"    ]] && bindkey -- "${key[PageUp]}"     beginning-of-buffer-or-history
[[ -n "${key[PageDown]}"  ]] && bindkey -- "${key[PageDown]}"   end-of-buffer-or-history
[[ -n "${key[Shift-Tab]}" ]] && bindkey -- "${key[Shift-Tab]}"  reverse-menu-complete
[[ -n "${key[Control-Left]}"  ]] && bindkey -- "${key[Control-Left]}"  backward-word
[[ -n "${key[Control-Right]}" ]] && bindkey -- "${key[Control-Right]}" forward-word

# Terminal application mode setup
if (( ${+terminfo[smkx]} && ${+terminfo[rmkx]} )); then
    autoload -Uz add-zle-hook-widget
    function zle_application_mode_start { echoti smkx }
    function zle_application_mode_stop { echoti rmkx }
    add-zle-hook-widget -Uz zle-line-init zle_application_mode_start
    add-zle-hook-widget -Uz zle-line-finish zle_application_mode_stop
fi

# History search setup
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
[[ -n "${key[Up]}"   ]] && bindkey -- "${key[Up]}"   up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey -- "${key[Down]}" down-line-or-beginning-search

# Emacs-style bindings
bindkey '^A' beginning-of-line
bindkey '^E' end-of-line
bindkey '^K' kill-line
bindkey '^U' kill-whole-line
bindkey '^W' backward-kill-word
bindkey '^Y' yank
bindkey '^R' history-incremental-search-backward
bindkey '^S' history-incremental-search-forward

# Edit line in editor with ctrl-x-e
autoload edit-command-line; zle -N edit-command-line
bindkey '^X^E' edit-command-line

# Lazy load pyenv (only when first needed)
pyenv() {
  unfunction pyenv
  eval "$(command pyenv init --path)"
  eval "$(command pyenv init -)"
  eval "$(command pyenv virtualenv-init -)"
  pyenv "$@"
}

# Initialize prompt
autoload -Uz promptinit
promptinit
export STARSHIP_SHELL="zsh"
eval "$(starship init zsh)"

# Initialize keychain
eval $(keychain --eval --quiet --confhost sotoon github codeberg divar-git divar-company-laptop-git)

# Load kubectl plugin conditionally
if command -v kubectl 1>/dev/null 2>&1; then
    source ~/.config/zsh/plugins/kubectl.plugin.zsh
fi

# Load direnv
eval "$(direnv hook zsh)"

# Load zoxide
eval "$(zoxide init zsh)"

# Load syntax highlighting last
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh 2>/dev/null
