if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ]; then
    PATH="$HOME/.local/bin:$PATH"
fi

# Go
export GOROOT=$HOME/app/go_1.7/go
export GOPATH=$HOME/gopath
export PATH=$PATH:$GOPATH/bin:$GOROOT/bin
export PATH=$PATH:$GOPATH/bin:$GOROOT/bin
