#!/usr/bin/env sh

# Requires https://github.com/Vonr/ibox

sleep 0.01
set -euo pipefail
input=$(ibox -c -s "Set brightness (%)" "?>")
[ -z "$input" ] && exit
pactl set-sink-volume @DEFAULT_SINK@ "${input}%"
notify-send -a 'sysnotif' -i '/usr/share/notify-osd/icons/hicolor/scalable/status/notification-audio-volume-medium.svg' -t 1000 'Volume' "Volume set to ${input}"
