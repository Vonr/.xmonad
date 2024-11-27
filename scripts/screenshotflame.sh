#!/usr/bin/env bash

# Requires flameshot

mkdir -p "${HOME}/pictures/screenshots" || exit 1
date=$(date +%Y-%m-%d)
file="${HOME}/pictures/screenshots/${date}-1.png"

count=2
while [ -f "$file" ]; do # file exists already
    file="${HOME}/pictures/screenshots/${date}-${count}.png"
    count=$((count + 1))
done

flameshot gui -r >> "$file"

xclip -selection clipboard -t image/png -i "$file"
notify-send -a sysnotif -t 1500 --replaces-process sysnotif "Screenshot saved to $file"
