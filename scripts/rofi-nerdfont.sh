#!/usr/bin/env sh
# Requires: curl, htmlq, sed, grep, xargs, rofi, xclip

([ -f "$HOME/.cache/nerdfont-icons.md" ] \
    || (curl -s 'https://raw.githubusercontent.com/ryanoasis/nerd-fonts/gh-pages/_posts/2017-01-04-icon-cheat-sheet.md' \
    | htmlq --text '#glyphCheatSheet' \
    | sed 's/\W//g' \
    | grep . \
    | sed 's/\(.*\)\(.\{4\}\)$/\2 \2 \1/g' \
    | xargs -I {} printf '\u{}\n' \
    > "$HOME/.cache/nerdfont-icons.md")) \
    && rofi -dmenu -matching fuzzy < "$HOME/.cache/nerdfont-icons.md" \
    | sed 's/ .*//g' \
    | xclip -se c
