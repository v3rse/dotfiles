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
PROMPT="v3rsePrompt=>"
