# -*- coding: utf-8 -*-
# name: Gianu-Recolored
function _git_branch_name
  echo (command git symbolic-ref HEAD ^/dev/null | sed -e 's|^refs/heads/||')
end

function _is_git_dirty
  echo (command git status -s --ignore-submodules=dirty ^/dev/null)
end

function fish_prompt
  set -l last_status $status
  set -l cyan (set_color cyan)
  set -l yellow (set_color -o yellow)
  set -l red (set_color -o red)
  set -l green (set_color -o green)
  set -l white (set_color -o white)
  set -l normal (set_color normal)
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

  echo -n -s $prompt_status $normal '[' $green (whoami) $normal '@' $cyan (hostname) $normal ':' $blue (prompt_pwd) $git_info $normal ']' $normal➜ ' '
end
