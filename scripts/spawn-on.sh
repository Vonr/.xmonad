#!/bin/sh

workspace="$1"
shift
cmd="$@"

pipe="$(mktemp -u)"
trap 'rm '"$pipe"'' EXIT
mkfifo "$pipe" || exit 1

sh -c 'echo $$ > '"$pipe"'; exec '"$cmd" &
pid="$(<"$pipe" head -n 1)"
echo "PID: $pid" >&2

end_time="$(($(date +"%s%3N") + 500))"
while :; do
    window_id="$(wmctrl -lp | awk '$3 == '"$pid"' { print $1 }')"
    if [ -z "$window_id" ]; then
        if [ "$(date "+%s%3N")" -ge "$end_time" ]; then
            echo "Failed to find window in 500ms" >&2
            exit 1
        fi
        sleep 0.01
    else
        echo "Found window with ID $window_id in $(($(date +"%s%3N") + 500 - "$end_time"))ms" >&2
        break
    fi
done

echo "Moving window to workspace $workspace" >&2
wmctrl -i -r "$window_id" -t "$workspace"
