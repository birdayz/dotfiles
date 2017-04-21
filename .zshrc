ZSH_THEME="powerlevel9k/powerlevel9k"
plugins=(git command-not-found encode64 history sudo wd)

# User configuration
source $ZSH/oh-my-zsh.sh

# Locale
#export LC_ALL=en_US.UTF-8
#export LC_TIME=de_DE.UTF-8

# ZSH stuff
bindkey "^[[5~" history-beginning-search-backward
bindkey "^[[6~" history-beginning-search-forward
setopt no_share_history
HISTFILE=~/.zsh_history
HISTSIZE=999999999
SAVEHIST=$HISTSIZE

# Git
if [[ -d $HOME/app/git-subrepo ]]; then
	source $HOME/app/git-subrepo/.rc
fi

export PATH=$HOME/bin:$GOPATH/bin:$GOROOT/bin:$GOPATH/bin:$SPRING_HOME/bin:$JAVA_HOME/bin:$M2_HOME/bin:$PATH
