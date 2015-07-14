####################################
# Oh My Fish
set fish_path $HOME/.config/fish/oh-my-fish
set fish_custom $HOME/.config/fish/custom

# Load oh-my-fish configuration.
. $fish_path/oh-my-fish.fish

set fish_greeting ""

####################################
# Theme
Theme "budspencer"
set -U budspencer_nogreeting
set budspencer_pwdstyle short
set budspencer_colors 2b2b2b 4f4f4f 6f6f6f dcdccc bfebbf f0dfaf cc9393 dfaf8f 93e0e3 94bff3 7f9f7f bfebbf
set -g fish_color_normal normal
set -g fish_color_command -o yellow
set -g fish_color_quote red
set -g fish_color_redirection orange
set -g fish_color_param blue
set -U fish_key_bindings fish_vi_key_bindings

#####################################
# Plugins
Plugin "balias"
Plugin "emacs"
Plugin "extract"
Plugin "msg"
Plugin "theme"

#####################################
# NVM
. $HOME/.config/fish/nvm-fish-wrapper/nvm.fish

#####################################
# Aliases
balias et 'emacsclient -a "" -t'

balias ec 'emacsclient -a "" -nc'

function pj
  node -e "console.log(JSON.stringify("{$argv}", null, '\t'));"
end

balias agl 'ag --pager=l'

balias l 'less -FRSX'

balias e "$EDITOR"
balias se "sudo $EDITOR"

balias tmux "tmux -2"
balias t "tmux"
balias ta="tmux new-session -A -s"
balias tl "tmux list-sessions"

balias ccat 'pygmentize -g'

#####################################
# Variables
# Editor
set -U EDITOR 'et'
set -gx TERM screen-256color
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
