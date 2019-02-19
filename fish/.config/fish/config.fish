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

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/v3rse/Downloads/google-cloud-sdk/path.fish.inc' ]
  source '/Users/v3rse/Downloads/google-cloud-sdk/path.fish.inc'
end
