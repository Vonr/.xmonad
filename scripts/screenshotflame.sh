#!/usr/bin/env bash

# Requires flameshot

mkdir -p "$HOME/Pictures/screenshots"
date=$(date +%Y-%m-%d)
file="$HOME/Pictures/screenshots/$date-1.png"

count=2
while [ -f "$file" ]; do # file exists already
    file="$HOME/Pictures/screenshots/$date-$count.png"
    count=$((count + 1))
done

flameshot gui -r >> "$file"

xclip -selection clipboard -t image/png -i "$file"
notify-send -a sysnotif -t 1500 --replaces-process sysnotif "Screenshot saved to $file"
