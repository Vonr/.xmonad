#!/usr/bin/env sh

# Requires shotgun

mkdir -p "${HOME}/Pictures/screenshots"
date=$(date +%Y-%m-%d)
file="${HOME}/Pictures/screenshots/${date}-1.png"

count=2
while [ -f "$file" ]; do # file exists already
    file="${HOME}/Pictures/screenshots/${date}-${count}.png"
    count=$((count + 1))
done

shotgun "$file" && \
    xclip -se c -t 'image/png' -i "$file" && \
    notify-send -a "sysnotif" -t 1000 "Screenshot saved" "$file"
