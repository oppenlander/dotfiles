#!/usr/bin/env sh

namespace=$1
action=$2


bind() {
    cmd="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/$(basename $0)"
    for key in $keys; do
        tmux bind -n $key run "$cmd $namespace $key"
    done

    # allow exiting with escape
    tmux bind -n Escape run "$cmd $namespace Escape"
}


unbind() {
    for key in $keys; do
        tmux unbind -n $key
    done

    tmux unbind -n Escape
}


getSession() {
    tmux display -p '#S'
}


case $namespace in

    session)
        keys="n N c C d D s S"

        if [ -z $action ]; then
            bind
        else
            case $action in
                n) tmux command-prompt -p "New session:" "new-session -A -s %%" ;;
                N) tmux command-prompt -p "New session:" "new-session -A -s %% -t `getSession`" ;;

                c) tmux confirm kill-session ;;
                C) tmux kill-session ;;

                d) tmux detach ;;
                D) tmux detach -s `getSession` ;;

                s) tmux choose-tree ;;
                S) tmux choose-session ;;
            esac

            unbind
        fi
        ;;


    resize)
        keys="j k h l J K H L"

        if [ -z $action ]; then
            bind
        else
            case $action in
                j) tmux resize-pane -U 15 ;;
                k) tmux resize-pane -D 15 ;;
                h) tmux resize-pane -L 25 ;;
                l) tmux resize-pane -R 25 ;;

                J) tmux resize-pane -U -15 ;;
                K) tmux resize-pane -D -15 ;;
                H) tmux resize-pane -L -25 ;;
                L) tmux resize-pane -R -25 ;;
            esac

            unbind
        fi
        ;;


    window)
        keys="j k h l J K H L v V s S c C"

        if [ -z $action ]; then
            bind
        else
            case $action in
                j) tmux select-pane -D ;;
                k) tmux select-pane -U ;;
                h) tmux select-pane -L ;;
                l) tmux select-pane -R ;;

                J) tmux swap-pane -D ;;
                K) tmux swap-pane -U ;;
                H) tmux swap-pane -U ;;
                L) tmux swap-pane -D ;;

                s) tmux split-window -v -c "#{pane_current_path}" ;;
                S) tmux split-window -v -c "#{pane_current_path}"; tmux swap-pane -U ;;
                v) tmux split-window -h -c "#{pane_current_path}" ;;
                V) tmux split-window -h -c "#{pane_current_path}"; tmux swap-pane -U ;;

                c) tmux confirm kill-pane ;;
                C) tmux kill-pane ;;
            esac

            unbind
        fi
        ;;


    frame)
        keys="h l s n N o c C"

        if [ -z $action ]; then
            bind
        else
            case $action in
                h) tmux previous-window ;;
                l) tmux next-window ;;

                s) tmux choose-window ;;

                n) tmux new-window -a ;;
                N) tmux new-window -a -s ;;

                o) tmux next-window ;;

                c) tmux confirm kill-window ;;
                C) tmux kill-window ;;
            esac

            unbind
        fi
        ;;

esac
