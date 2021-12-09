# ALIASES:
# -File System
alias ls='colorls'
alias ll='colorls -l'
alias la='colorls -al'
alias md='mkdir -p'

# -Shortcuts
alias projects="cd ~/projects"
alias learn="cd ~/projects/learnings"
alias oss="cd ~/projects/oss"

# -Python
#alias python="python3"
#alias pip="pip3"
alias prp="pipenv run python"

# Docker Compose
alias setup='projects; cd ./resources; docker-compose up -d; cd -'
alias teardown='projects; cd ./resources; docker-compose down; cd -'

# -NPM
alias nr='npm run'

# -Fuzzy Find
alias fuzzy='fzf --preview="head -$LINES {}"'

# -Git
alias gs='git status '
alias ga='git add '
alias gaa='git add -A '
alias gb='git branch '
alias gc='git commit '
alias gcm='git commit -m '
alias gco='git checkout '

# Utils
alias cat='batcat'
alias vim='nvim'

set SPACEFISH_BATTERY_SHOW false
