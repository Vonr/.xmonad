#!/usr/bin/env sh

# Requires https://github.com/Vonr/ibox

sleep 0.01; ibox -c -s "Set volume (%)" "?>" | xargs -I {} pactl set-sink-volume @DEFAULT_SINK@ {}%
