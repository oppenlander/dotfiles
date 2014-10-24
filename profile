# No capslock. Not ever.
setxkbmap -option ctrl:nocaps

# Add home bin to path
export PATH=$PATH;$HOME/bin;$HOME/.cabal/bin

# sensible editor
export VISUAL="emacsclient -c"
export EDITOR="emacsclient -t"

# sensible pager
export PAGER="less -FRSX"

# sensible terminal
export TERMINAL=terminator