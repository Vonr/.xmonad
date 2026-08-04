#!/usr/bin/env sh

# Requires https://github.com/Vonr/ibox

sleep 0.01
set -euo pipefail
input=$(ibox -c -s "Set brightness (%)" "?>")
[ -z "$input" ] && exit
lux -S "$((input + 1))%"
notify-send -a 'sysnotif' -i '/usr/share/notify-osd/icons/hicolor/scalable/status/notification-display-brightness.svg' -t 1000 'Brightness' "Brightness set to ${input}"
