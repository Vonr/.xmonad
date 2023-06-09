#!/usr/bin/env sh
# Requires: curl, htmlq, sed, grep, xargs, rofi, xclip

([ -f "$HOME/.cache/nerdfont-icons.md" ] \
    || (curl -s 'https://www.nerdfonts.com/cheat-sheet' \
    | htmlq -t '#glyphCheatSheet > div > .codepoint, .class-name' \
    | awk '!(NR % 2) { print $0" "$0" "p } { p = $0 }' \
    | grep -v ' nf-mdi-' \
    | parallel -P1 --colsep ' ' printf '%b" "{2}" "{3}"\n"' '\\U{1}' \
    > "$HOME/.cache/nerdfont-icons.md")) \
    && rofi -dmenu < "$HOME/.cache/nerdfont-icons.md" \
    | awk '{ print $1 }' \
    | xclip -se c
