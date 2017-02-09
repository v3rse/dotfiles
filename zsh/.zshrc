# For bash stuff
source ~/.bash_profile

# GENERAL
# History and cd settings
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd

# For vi bindings
bindkey -v

# zstyle :compinstall filename '/home/v3rse/.zshrc'

# BUILT-IN FUNCTIONS
# 'Import' some function modules (Cool stuff in zshcontrib section of manual)
autoload -Uz compinit promptinit
## PROMPT 
#promptinit # For build in prompt system
# List all built in prompts using prompt -l or preview with prompt-p
# Set built in prompt
#prompt walters


## COMPLETION
# Call the functions
compinit # Start up completion

# Use menu selection style for completion 
zstyle ':completion:*' menu select

# Search history up to cursor possition using up or down arrow keys
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

[[ -n "$key[Up]" ]] && bindkey -- "$key[Up]" up-line-or-beginning-search
[[ -n "$key[Down]" ]] && bindkey -- "$key[Down]" down-line-or-beginning-search

# CUSTOM STUFF

# ALIASES:
# -File System
alias ll='ls -l'
alias la='ls -al'
alias md='mkdir -p'

# -Vim
alias vim='nvim'

# PROMPT
# -Colors : %F{} an %K{} colors can be tested using print -P 'string'
# print -P '%F{cyan}Hello%f'

# TODO: Get git info
# TODO: Change prompt base on mode

# Main
# cyan-currdirfirstcomponenthomereplace colored arrors
PROMPT='%F{cyan}%-~%f %F{red}>%f%F{yellow}>%f%F{green}>%f '
# Right side prompt (use for git stuff and command failure)
RPROMPT=''

# INITIALIZATIONS
# Change n prefix
export N_PREFIX=$HOME/n
