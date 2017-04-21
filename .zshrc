ZSH_THEME="powerlevel9k/powerlevel9k"
plugins=(git command-not-found encode64 history sudo wd)

# User configuration
source $ZSH/oh-my-zsh.sh

# ZSH stuff
bindkey "^[[5~" history-beginning-search-backward
bindkey "^[[6~" history-beginning-search-forward
setopt no_share_history
setopt append_history
setopt inc_append_history

HISTFILE=~/.zsh_history
HISTSIZE=999999999
SAVEHIST=$HISTSIZE

# Git
if [[ -d $HOME/app/git-subrepo ]]; then
	source $HOME/app/git-subrepo/.rc
fi

export PATH=$HOME/bin:$GOPATH/bin:$GOROOT/bin:$GOPATH/bin:$SPRING_HOME/bin:$JAVA_HOME/bin:$M2_HOME/bin:$PATH
