#!/usr/bin/env dash

# Adapted from https://github.com/MrBober/xbasket (All Rights Reserved)

[ -z "$DMENU" ] && DMENU="dmenu"
list="/tmp/minimized_$USER"

help() {
    printf "Usage: %s <command> <args>\n\n" "$0"
    printf "Commands:\n"
    printf "  help - display help\n"
    printf "  hide - hide active window if no arguments are given\n         otherwise hide a window with given id\n"
    printf "  show - show a minimized window with given id\n"
    printf "  select - display a list of minimized windows and select one\n"
}

hide() {
    [ -z "$1" ] && id=$(xdotool getactivewindow) || id=$1
    xdotool windowunmap "$id"
    echo "$id" >> "$list"
}

show() {
    [ -z "$1" ] && exit
    xdotool windowmap "$1"
    sed -i "/$1/d" "$list"
}

sel() {
    win_ids=$(cat "$list")
    wins=$(
    for id in $win_ids
    do
        [ -n "$id" ] && echo "$id: \"$(xdotool getwindowname "$id")\" ($(cat /proc/"$(xdotool getwindowpid "$id")"/comm))"
    done
    )
    search=$(echo "$wins" | $DMENU -p "show")
    [ -z "$search" ] && exit
    id=${search%%:*}
    xdotool windowmap "$id"
    sed -i "/$id/d" "$list"
}

[ $# -eq 0 ] || [ "$1" = "help" ] && help
[ "$1" = "hide" ] && hide "$2"
[ "$1" = "show" ] && show "$2"
[ "$1" = "select" ] && sel

[ -f "$list" ] && chmod 600 "$list"
