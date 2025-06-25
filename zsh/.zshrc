# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Initialisation
# Timezone
if  [[ "$OSTYPE" != "darwin"* ]]; then
  export TZ=Europe/Berlin
fi

# PATH
export PATH="$PATH:/usr/local/go/bin"
export PATH="$PATH:$HOME/go/bin"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$HOME/bin"
export PATH="$HOME/.local/share/gem/ruby/3.0.0/bin:$PATH"
export PATH="$HOME/.qlot/bin:$PATH"
export BUN_INSTALL="$HOME/.bun"
export PATH=$BUN_INSTALL/bin:$PATH
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

if [[ "$OSTYPE" == "darwin"* ]]; then
  # MAC-ONLY
  eval "$(/opt/homebrew/bin/brew shellenv)"
  export PATH="/opt/homebrew/bin/:$HOME/.cargo/bin:$PATH"
fi

if [[ "$OSTYPE" != "darwin"* ]]; then
  export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
fi

# Defaults
export VISUAL=nvim
export EDITOR="$VISUAL"
export DENO_INSTALL="/home/v3rse/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"
export GPG_TTY=$(tty)

[[ "$TERM" == "xterm-kitty" ]] && alias ssh="kitty +kitten ssh"

if [[ "$OSTYPE" != "darwin"* ]]; then
  export BROWSER=/usr/bin/qutebrowser
  source /usr/share/nvm/init-nvm.sh
  eval $(keychain --eval --quiet --agents ssh,gpg id_ed25519 5F910544C18EE265)
else
  export SSH_AUTH_SOCK=~/Library/Group\ Containers/2BUA8C4S2C.com.1password/t/agent.sock
  export AWS_PROFILE=integration
  export PATH="/opt/homebrew/opt/openjdk@11/bin:$PATH"
  export NVM_DIR="$HOME/.nvm"
  [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
  [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
fi

# Plugin manager
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"

if [ ! -d "$ZINIT_HOME" ]; then
  mkdir -p "$(dirname $ZINIT_HOME)"
  git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
fi

source "${ZINIT_HOME}/zinit.zsh"

# Prompt (nerd fonts required)
zinit ice depth=1; zinit light romkatv/powerlevel10k

# Plugins
# syntax highlighting
zinit light zsh-users/zsh-syntax-highlighting

# tab completion
zinit light zsh-users/zsh-completions

# suggestions (like fish)
zinit light zsh-users/zsh-autosuggestions

# fzf integration
zinit light Aloxaf/fzf-tab

# Snippets
# oh-my-zsh configuration snippet
zinit snippet OMZP::git
# TODO: other for aws, kubectl, sudo, command-not-found and archlinux for linux only
# https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins

# Completions
# load completions
autoload -U compinit && compinit
# replay cached completions
zinit cdreplay -q
# make completions syntax insentive
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
# add colors to ls completions
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
# deactivate default completion menu
zstyle ':completion:*' menu no
# add some facy stuff for cd completion
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls --color $realpath'

# auto complete strategies
ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# Keybindings
# emacs mode
bindkey -e
# search for matches
bindkey "^p" history-search-backward
bindkey "^n" history-search-forward

# History
HISTSIZE=5000
HISTFILE=~/.zsh_history
SAVEHIST=$HISTSIZE
# remove duplicates
HISTDUP=erase
# don't overwrite
setopt appendhistory
# share across session
setopt sharehistory
# use space to hide things from history
setopt hist_ignore_space
# no dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups

# Aliases
# give colors to ls
alias cat='bat'
alias md='mkdir -p'
alias nr='npm run'
alias la='ls -A'
alias ll='ls -alF'
alias ls='ls --color'
alias src="cd ~/src"
alias vim='lvim'

# Fuzzy Finder
eval "$(fzf --zsh)"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# TODO: zoxide

# Hooks
# place this after nvm initialization!
autoload -U add-zsh-hook

load-nvmrc() {
  local nvmrc_path
  nvmrc_path="$(nvm_find_nvmrc)"

  if [ -n "$nvmrc_path" ]; then
    local nvmrc_node_version
    nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")

    if [ "$nvmrc_node_version" = "N/A" ]; then
      nvm install
    elif [ "$nvmrc_node_version" != "$(nvm version)" ]; then
      nvm use
    fi
  elif [ -n "$(PWD=$OLDPWD nvm_find_nvmrc)" ] && [ "$(nvm version)" != "$(nvm version default)" ]; then
    echo "Reverting to nvm default version"
    nvm use default
  fi
}

add-zsh-hook chpwd load-nvmrc
load-nvmrc
