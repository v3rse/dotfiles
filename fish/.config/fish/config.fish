set PATH $PATH $HOME/bin

# ALIASES:
# -File System
#alias ls='colorls'
#alias ll='colorls -l'
#alias la='colorls -al'
alias md='mkdir -p'

# -Shortcuts
alias factory="cd ~/TheFactory"
alias codez="cd ~/TheFactory/code"
alias docs="cd ~/TheFactory/docs"
alias work="cd ~/TheFactory/code/work/kudobuzz"
alias learn="cd ~/TheFactory/code/learn"
alias oss="cd ~/TheFactory/code/oss"

# -Xrandr
alias screen-left='xrandr --output eDP-1 --output DP-1 --auto --left-of eDP-1'
alias screen-right='xrandr --output eDP-1 --output DP-1 --auto --right-of eDP-1'
alias screen-off='xrandr --output DP-1 --auto --off'
alias sl='screen-left'
alias sr='screen-right'
alias sx='screen-off'

# -i3 workspace
alias workspace-left='i3-msg move workspace to output left'
alias workspace-right='i3-msg move workspace to output right'
alias wl='workspace-left'
alias wr='workspace-right'

# -Python
#alias python="python3"
#alias pip="pip3"
alias prp="pipenv run python"

# Docker Compose
alias setup='cd ~/TheFactory/code/work/setup/tools; docker-compose up -d; cd -'
alias teardown='cd ~/TheFactory/code/work/setup/tools; docker-compose down; cd -'

# -Vim
alias vim='nvim'

# -Xmodmap
alias escswap='xmodmap ~/.xmodmap'

# -NPM
alias nr='npm run'

# -Fuzzy Find
alias fuzzy='fzf --preview="head -$LINES {}"'

# -Love2D
alias love='/Applications/love.app/Contents/MacOS/love'

alias journal='cd ~/TheFactory/docs; code journal; cd -'

# -Git
alias gs='git status '
alias ga='git add '
alias gaa='git add -A '
alias gb='git branch '
alias gc='git commit '
alias gcm='git commit -m '
alias go='git checkout '

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/v3rse/Downloads/google-cloud-sdk/path.fish.inc' ]
  source '/Users/v3rse/Downloads/google-cloud-sdk/path.fish.inc'
end
