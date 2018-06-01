export ZSH=/home/j0e/.oh-my-zsh
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

export LC_ALL="en_US.UTF-8"
export GOPATH="$HOME/gopath"
export PATH=$GRADLE_HOME/bin:$HOME/bin:$GOPATH/bin:$GOROOT/bin:$GOPATH/bin:$SPRING_HOME/bin:$JAVA_HOME/bin:$M2_HOME/bin:/home/j0e/.cargo/bin:$PATH

source $HOME/.zshenv_secret
alias tf=terraform
alias k=kubectl
alias win='exec VBoxManage startvm "crap"'

export PATH=$PATH:$HOME/.gem/ruby/2.4.0/bin:/opt/google-cloud-sdk/platform/google_appengine/:/opt/ccloud-0.2.1/bin/
