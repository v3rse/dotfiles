# Created by newuser for 5.3.1
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/v3rse/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# CUSTOM STUFF

# ALIASES:
# -File System
alias ll='ls -l'
alias la='ls -al'
alias md='mkdir -p'

# -Vim
alias vim='nvim'

# PROMPT
# -Colors
reset_color=$(tput sgr0)
red=$(tput setaf 1)
yellow=$(tput setaf 2)
green=$(tput setaf 3)
cyan=$(tput setaf 6)


# -Prompt modes (vim mode)
insert_mode_prompt="$red>$green>$yellow>$reset_color"
normal_mode_prompt="$red<$green<$yellow<$reset_color"

# -Functions
# Get current directory
function get_pwd() {
  echo "${PWD/$HOME/~}"
}

# TODO: Get git info
# TODO: Change prompt base on mode

# -Template
# Enable command substitutions, parameter expansions and arithmetic expansions
setopt promptsubst
# Wrap escape sequences to prevent displacement of prompt
PROMPT="%{$cyan%}\$(get_pwd) %{$reset_color%}%{$insert_mode_prompt%} "
