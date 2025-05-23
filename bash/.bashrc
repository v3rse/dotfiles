# ~/.bashrc
# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
export TZ=Europe/Berlin
# for examples
if [[ "$OSTYPE" == "darwin"* ]]; then
  # MAC-ONLY
  eval "$(/opt/homebrew/bin/brew shellenv)"
  export PATH=/opt/homebrew/bin/:$PATH
fi

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# N VERSION
export N_PREFIX="$HOME"

# GO
export PATH="$PATH:/usr/local/go/bin"
export PATH="$PATH:$HOME/go/bin"

# more wildcards
shopt -s extglob

# default editor
export VISUAL=nvim
export EDITOR="$VISUAL"

# default browser
export BROWSER=/usr/bin/qutebrowser

if [[ "$OSTYPE" != "darwin"* ]]; then
  export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
fi

# local binaries
export PATH="$PATH:$HOME/.local/bin"

# personal binaries
export PATH="$PATH:$HOME/bin"

# PATH="/home/v3rse/perl5/bin${PATH:+:${PATH}}"; export PATH;
# PERL5LIB="/home/v3rse/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
# PERL_LOCAL_LIB_ROOT="/home/v3rse/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
# PERL_MB_OPT="--install_base \"/home/v3rse/perl5\""; export PERL_MB_OPT;
# PERL_MM_OPT="INSTALL_BASE=/home/v3rse/perl5"; export PERL_MM_OPT;

# Ruby
export PATH="$HOME/.local/share/gem/ruby/3.0.0/bin:$PATH"


# Load Angular CLI autocompletion.
# source <(ng completion script)

# NVM
if [[ "$OSTYPE" != "darwin"* ]]; then
  source /usr/share/nvm/init-nvm.sh
else
  export NVM_DIR="$HOME/.nvm"
  [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
  [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
fi

# BUN
export BUN_INSTALL="$HOME/.bun"
export PATH=$BUN_INSTALL/bin:$PATH

# DENO
export DENO_INSTALL="/home/v3rse/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"

# kitty ssh
[[ "$TERM" == "xterm-kitty" ]] && alias ssh="kitty +kitten ssh"

# keychain
# check if mac or linux
if [[ "$OSTYPE" != "darwin"* ]]; then
  eval $(keychain --eval --quiet --agents ssh,gpg id_ed25519 5F910544C18EE265)
else
  # 1password
  export SSH_AUTH_SOCK=~/Library/Group\ Containers/2BUA8C4S2C.com.1password/t/agent.sock
  # aws profile
  export AWS_PROFILE=integration
  # java
  export PATH="/opt/homebrew/opt/openjdk@11/bin:$PATH"
fi


#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"


# flutter
export PATH="/opt/flutter/bin:$PATH"
export CHROME_EXECUTABLE="/usr/bin/brave"

# GPG
export GPG_TTY=$(tty)

if [[ "$OSTYPE" == "darwin"* ]]; then
  if [[ $(ps -o "command" -p $PPID) != "fish" && -z ${BASH_EXECUTION_STRING} ]]
  then
        exec fish
  fi
else
  if [[ $(ps --no-header --pid=$PPID --format=cmd) != "fish" && -z ${BASH_EXECUTION_STRING} ]]
  then
        exec fish
  fi
fi





