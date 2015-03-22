# Path to your oh-my-fish.
set fish_path $HOME/.config/fish/oh-my-fish

set fish_greeting ""

# Theme
#set fish_theme agnoster
set fish_theme budspencer
set -U budspencer_nogreeting
set budspencer_pwdstyle short

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-fish/plugins/*)
# Custom plugins may be added to ~/.oh-my-fish/custom/plugins/
# Example format: set fish_plugins autojump bundler
#set fish_plugins python tmux jump fasd
#set fish_plugins python

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


# NVM
source ~/.config/fish/nvm-fish-wrapper/nvm.fish

# Use vi bindings because I hate myself
set -U fish_key_bindings fish_vi_key_bindings

####################################
# Theme
set -g fish_color_normal normal
set -g fish_color_command -o noraml
set -g fish_color_quote red
set -g fish_color_redirection blue
set -g fish_color_end blue
set -g fish_color_error orange
set -g fish_color_param white
set -g fish_color_comment green
set -g fish_color_match teal
set -g fish_color_search_match yellow
set -g fish_color_operator blue
set -g fish_color_escape normal
set -g fish_color_cwd teal

# Load oh-my-fish configuration.
. $fish_path/oh-my-fish.fish
