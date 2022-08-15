#!/usr/bin/env sh
# Requires: curl, htmlq, sed, grep, xargs, rofi, xclip

([ -f "$HOME/.cache/unicode-index" ] \
    || (curl -s 'https://www.unicode.org/Public/UCD/latest/ucd/Index.txt' \
    | sed -r 's/(.*\W+)(.*)/\2 \2 \1/g' \
    | sed 's/'\''/`/g' \
    | tr '[:upper:]' '[:lower:]' \
    | xargs -I {} printf '\u{}\n' \
    > "$HOME/.cache/unicode-index")) \
    && rofi -dmenu -matching fuzzy < "$HOME/.cache/unicode-index" \
    | sed 's/ .*//g' \
    | xclip -se c
