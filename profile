# No capslock. Not ever.
setxkbmap -option ctrl:nocaps

# sensible editor
export VISUAL="emacsclient -c"
export EDITOR="emacsclient -t"

# sensible pager
export PAGER="less -FRSX"

# sensible terminal
export TERMINAL=xfce4-terminal -e "tmux"

## Add path modifications here for i3

# Add home bin to path
export PATH=$PATH:$HOME/bin

# Add Cabal (Haskell) bins
export PATH=$PATH:$HOME/.cabal/bin

# Add Node.JS bins
export PATH=$PATH:$HOME/npm/bin

# default GOPATH
export GOPATH=$HOME/gocode
export PATH=$PATH:$HOME/gocode/bin
