set PATH $PATH $HOME/bin


# ALIASES:
# -File System
#alias ls='colorls'
#alias ll='colorls -l'
#alias la='colorls -al'
alias md='mkdir -p'

# Docker
alias unleashthedragon='docker run -ti --mount src=kali-root,dst=/root --mount src=kali-postgres,dst=/var/lib/postgresql --cap-add=NET_ADMIN --device /dev/net/tun:/dev/net/tun --sysctl net.ipv6.conf.all.disable_ipv6=0 blue-dragon /bin/bash'

# -Shortcuts
alias factory="cd ~/TheFactory"
alias codez="cd ~/TheFactory/code"
alias docs="cd ~/TheFactory/docs"
alias work="cd ~/TheFactory/code/work/kudobuzz"
alias learn="cd ~/TheFactory/code/learn"
alias oss="cd ~/TheFactory/code/oss"
alias tiny="tiny-care-terminal"

# -Xrandr
alias screen-left='xrandr --output eDP-1 --output DP-1 --auto --left-of eDP-1'
alias screen-right='xrandr --output eDP-1 --output DP-1 --auto --right-of eDP-1'
alias screen-top='xrandr --output eDP-1 --output DP-1 --auto --above eDP-1'
alias screen-off='xrandr --output DP-1 --auto --off'
alias sl='screen-left'
alias sr='screen-right'
alias st='screen-top'
alias sx='screen-off'

# -i3 workspace
alias workspace-left='i3-msg move workspace to output left'
alias workspace-right='i3-msg move workspace to output right'
alias workspace-top='i3-msg move workspace to output up'
alias workspace-bottom='i3-msg move workspace to output down'
alias wl='workspace-left'
alias wr='workspace-right'
alias wt='workspace-top'
alias wb='workspace-bottom'

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

# Utils
alias cat='bat'

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/v3rse/Downloads/google-cloud-sdk/path.fish.inc' ]
  source '/Users/v3rse/Downloads/google-cloud-sdk/path.fish.inc'
end

## Environment variables

# dotnet opt
set DOTNET_CLI_TELEMETRY_OPTOUT true

# tiny-care-terminal

# List of accounts to read the last tweet from, comma separated

# The first in the list is read by the party parrot.

set -gx TTC_BOTS 'tinycarebot,selfcare_bot,magicrealismbot'

 

# Use this to have a different animal say a message in the big box. You can also use any of the following animals if you so choose: parrot, bunny, llama, cat, yeoman, mario, ironman, minions, panda

set -gx TTC_SAY_BOX 'panda'

 

# List of folders to look into for `git` commits, comma separated. I have all of my projects inside of the code/ directory, so I just used it.

set -gx TTC_REPOS '/home/v3rse/TheFactory/code/learn,/home/v3rse/TheFactory/code/oss,/home/v3rse/TheFactory/code/work'

 

# The max directory-depth to look for git repositories in

# the directories defined with `TTC_REPOS`. Note that the deeper

# the directory depth, the slower the results will be fetched.

set -gx TTC_REPOS_DEPTH 2

 

# Which method is to be used to read the git commits ('gitstandup' | 'gitlog').

# If you're having problems seeing your commits in the dahsboard, set

# this value to gitlog.

set -gx TTC_GITBOT 'gitstandup'

 

# Location/zip code to check the weather for. Both 90210 and "San Francisco, CA"

# _should_ be ok (the zip code doesn't always work -- use a location

# first, if you can). It's using weather.service.msn.com behind the curtains.

set -gx TTC_WEATHER 'Accra, Ghana'

 

# Set to false if you're an imperial savage. << I'm an imperial savage, so I set it to false...

set -gx TTC_CELSIUS true

 

# Unset this if you _don't_ want to use Twitter keys and want to

# use web scraping instead.

set -gx TTC_APIKEYS true

 

# Refresh the dashboard every 20 minutes.

set -gx TTC_UPDATE_INTERVAL 20

 

# Turn off terminal title

set -gx TTC_TERMINAL_TITLE false

 

# Twitter api keys, you can create an app here: https://apps.twitter.com

set -gx TTC_CONSUMER_KEY '8pBz8YyATg8A5o9TuSiaZAONG'

set -gx TTC_CONSUMER_SECRET 'qbVvK25AqkAbovS9keQLsE0s7Z2Jv0dcxTlnCXIj9WhBXlhTDT'

set -gx TTC_ACCESS_TOKEN '131014933-BNzC6EwW2InJqp1jHOBNqx6TJ44LuBlJkiKkVGEh'

set -gx TTC_ACCESS_TOKEN_SECRET '7vDzZ3NX0snJjXxTTSEib4D6WmaxssSokCXivue51HYvw'

 

# Note: in tiny-terminal-care < 1.0.7, the recommended variables for the Twitter

# API keys were the ones before. As of 1.0.8, they are deprecated

# (because the names are too generic), but will still be supported

# until the next major version.

set -gx CONSUMER_KEY '8pBz8YyATg8A5o9TuSiaZAONG'

set -gx CONSUMER_SECRET 'qbVvK25AqkAbovS9keQLsE0s7Z2Jv0dcxTlnCXIj9WhBXlhTDT'

set -gx ACCESS_TOKEN '131014933-BNzC6EwW2InJqp1jHOBNqx6TJ44LuBlJkiKkVGEh'

set -gx ACCESS_TOKEN_SECRET '7vDzZ3NX0snJjXxTTSEib4D6WmaxssSokCXivue51HYvw'

# pomodoro
set -gx TTC_POMODORO 25
set -gx TTC_BREAK 5
