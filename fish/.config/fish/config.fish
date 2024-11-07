# ALIASES:
alias md='mkdir -p'

# -Shortcuts
alias src="cd ~/src"

# -NPM
alias nr='npm run'

# -Git
alias gs='git status '
alias ga='git add '
alias gaa='git add -A '
alias gb='git branch '
alias gc='git commit '
alias gcm='git commit -m '
alias gco='git checkout '
alias animedoro='notify-send --urgency normal "animedoro started. 60 minutes to go"; sleep 3600; notify-send --urgency critical "animedoro ended. time to take a break and journal some thoughts"; mpg123 -q ~/Music/beeps.mp3; days journal write';
alias awslocal='aws --endpoint-url=http://localhost:4566 --region us-east-1'
# alias animedoro30='notify-send --urgency normal "animedoro started. 30 minutes to go"; sleep 1800; notify-send --urgency  critical "animedoro ended. time to take a break and journal some thoughts"; mpg123 -q ~/Music/beeps.mp3; days journal write'
alias animedoro30='notify-send --urgency normal "animedoro started. 30 minutes to go"; sleep 1800; notify-send --urgency  critical "animedoro ended. time to take a break and journal some thoughts";'

# -Vim
alias vim='lvim'

# Bat
alias cat='bat'

# Kube
alias k=kubectl
