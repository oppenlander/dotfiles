# No capslock. Not ever.
setxkbmap -option ctrl:nocaps

# Add home bin to path
export PATH=$PATH;$HOME/bin

# Add Cabal (Haskell) bins
export PATH=$PATH;$HOME/.cabal/bin

# Add Node.JS bins
export PATH=$PATH;$HOME/npm/bin

# Add Emacs bins
export PATH=$PATH;$HOME/.cask/bin

# sensible editor
export VISUAL="emacsclient -c"
export EDITOR="emacsclient -t"

# sensible pager
export PAGER="less -FRSX"

# sensible terminal
export TERMINAL=terminator