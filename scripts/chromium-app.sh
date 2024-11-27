#!/bin/sh

exec $(<"$(find ~/.local/share/applications/ -regextype posix-extended -regex '^.*\/chrome-[a-z]{32}-Default.desktop$' -exec grep -l "Name=$1" "{}" \;)" sed -n 's/Exec=\(.*\)/\1/p')
