# -*- coding: utf-8 -*-
# name: Gianu-Recolored
function _git_branch_name
  echo (command git symbolic-ref HEAD ^/dev/null | sed -e 's|^refs/heads/||')
end

function _is_git_dirty
  echo (command git status -s --ignore-submodules=dirty ^/dev/null)
end

function fish_vi_prompt_color --description "Displays the current mode"
  switch $fish_bind_mode
    case default
      set_color --bold --background red white
    case insert
      set_color --bold --background green white
    case visual
      set_color --bold --background magenta white
  end
  set_color normal
end

function fish_prompt
  set -l last_status $status
  set -l cyan (set_color -o cyan)
  set -l yellow (set_color -o yellow)
  set -l red (set_color -o red)
  set -l green (set_color -o green)
  set -l white (set_color -o white)
  set -l onormal (set_color -o normal)
  set -l blue (set_color -o blue)

  set -l prompt_status
  if [ $last_status -gt 0 ]
    set prompt_status $red '(' $last_status ')'
  end

  if [ (_git_branch_name) ]
    set -l git_branch (_git_branch_name)
    set git_info " $yellow($git_branch"

    if [ (_is_git_dirty) ]
      set -l dirty "$red ✗"
      set git_info "$git_info$dirty"
    end

    set git_info "$git_info$yellow)"
  end

  set -l arrow_color
  switch $fish_bind_mode
    case default
      set arrow_color (set_color -o normal)
    case insert
      set arrow_color (set_color -o green)
    case visual
      set arrow_color (set_color -o magenta)
  end

  echo -n -s $prompt_status $onormal '[' $green (whoami) $onormal '@' $cyan (hostname) $onormal ':' $blue (prompt_pwd) $git_info $onormal ']' $arrow_color ➜ $onormal ' '
end
