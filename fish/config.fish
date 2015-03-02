# Path to your oh-my-fish.
set fish_path $HOME/.oh-my-fish

set fish_greeting ""

# Theme
#set fish_theme gianu

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-fish/plugins/*)
# Custom plugins may be added to ~/.oh-my-fish/custom/plugins/
# Example format: set fish_plugins autojump bundler
set fish_plugins python tmux jump fasd

# Path to your custom folder (default path is $FISH/custom)
#set fish_custom $HOME/dotfiles/oh-my-fish

#####################################
# Aliases
alias et 'emacsclient -a "" -t'

alias ec 'emacsclient -a "" -nc'

function pj
  node -e "console.log(JSON.stringify("{$argv}", null, '\t'));"
end

alias agl 'ag --pager="less -FRSX"'

alias e "$EDITOR"
alias se "sudo $EDITOR"

alias tmux "tmux -2"
alias t "tmux"
alias ta="tmux new-session -A -s"
alias tl "tmux list-sessions"

#####################################
# Variables
# Editor
set -U EDITOR 'et'
set -gx TERM xterm-256color
# Language
set -gx LC_ALL en_US.UTF-8
set -gx LANG en_US.UTF-8
set -gx LANGUAGE en_US.UTF-8
# Java
set -gx JAVA_HOME '/opt/java'
# Go
set -gx GOPATH $HOME'/gocode'
# Android
set -gx ANDROID_HOME '/opt/android-sdk'

#####################################
# Path modifications
# Ruby
set -gx PATH (echo (ruby -e 'puts Gem.user_dir')'/bin') $PATH
# Go
set -gx PATH $GOPATH'/bin' $PATH
# Java
set -gx PATH $JAVA_HOME'/bin' $PATH
# Android
set -gx PATH $ANDROID_HOME'/platform-tools' $PATH
# Home bin
set -gx PATH $HOME'/bin' $PATH
# NPM bin
set -gx PATH $HOME'/npm/bin' $PATH
# Cabal/Haskell bin
set -gx PATH $HOME'/.cabal/bin' $PATH

# Load oh-my-fish configuration.
. $fish_path/oh-my-fish.fish

# Set vi mode because I hate myself
#fish_vi_keybindings

# NVM
source ~/.config/fish/nvm-fish-wrapper/nvm.fish
