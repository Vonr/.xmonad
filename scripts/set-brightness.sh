#!/usr/bin/env sh

# Requires https://github.com/Vonr/ibox

sleep 0.01
input=$(ibox -c -s "Set brightness (%)" "?>")
[ -z "$input" ] && exit
echo $((input + 1)) | xargs -I {} lux -S {}%
