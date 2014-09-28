# Path to your oh-my-fish.
set omf_path $HOME/.config/fish/oh-my-fish

set fish_greeting ""

# Theme
#set fish_theme gianu

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-fish/plugins/*)
# Custom plugins may be added to ~/.oh-my-fish/custom/plugins/
# Example format: set fish_plugins autojump bundler
set fish_plugins node python tmux jump

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

#####################################
# Path modifications
set -gx PATH (echo (ruby -e 'puts Gem.user_dir')'/bin') $PATH
# Go Settings
set -gx PATH $GOPATH'/bin' $PATH
# Java Home
set -gx PATH '/opt/java/bin' $PATH
# Android
set -gx PATH '/opt/android-sdk/platform-tools' $PATH
# Home bin
set -gx PATH $HOME'/bin' $PATH
# NPM bin
set -gx PATH $HOME'/npm/bin' $PATH

# Load oh-my-fish configuration.
. $omf_path/oh-my-fish.fish
