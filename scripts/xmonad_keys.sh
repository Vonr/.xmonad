#!/usr/bin/env sh
# shellcheck disable=SC2016

# Unmaintained!

keywords='on|cmd|nsp|sh|sh'\''|ter'
sleep 0.01; sed -n '/START_KEYS/,/END_KEYS/p' ~/.xmonad/xmonad.hs | \
    grep -E -e '^ *((,|\[) ('"$keywords"') |\]|$)' \
    -e 'KB_GROUP' | \
    sed -r \
    -e 's/ \[(.*)\]/ \\e[33m[\1\\e[33m]\\e[m/'  \
    -e 's/((\,|\[) ('"$keywords"') \"[^\"]*\" *)(\w+)/\1\\e[1;32m\4\\e[m/' \
    -e 's/([\[\,] )('"$keywords"')/\1\\e[1;32m\2\\e[m/' \
    -e 's/^ *\].*/\\e[33m]/' \
    -e 's/( *)([\[\,])/\1\\e[33m\2/' \
    -e 's/-- KB_GROUP /\\e[37m/' \
    -e 's/( (Next|Prev) )/\\e[33m\1\\e[m/g' \
    -e 's/^ *//' \
    -e 's/%/%%/g' \
    -e 's/(\"[^\"]*\")/\\e[32m\1\\e[m/g' \
    -e 's/\$( +(\w|'\'')*)/\\e[33m\$\\e[1;32m\1\\e[m/' \
    -e 's/( (<\+?>|>>|\$|\.) +)((\w|'\'')+)/\\e[33m\1\\e[1;32m\3\\e[m/g' \
    -e 's/( \([^\)]*\).*)/\\e[33m\1\\e[m/g' \
    -e 's/([ \(])([A-Z]+)\.(\w+)/\1\\e[36m\2\\e[m.\3/g' \
    -e 's/\.(.*)\)/.\\e[33m\1\)/g' \
    -e 's/( |\()((-)?[0-9]+(\.[0-9]+)?)( |\))/\1\\e[35m\2\\e[33m\5/g' \
    -e 's/( -- )/\\e[37m\0/g'  | \
    xargs -0 -I {} printf "{}\n" | \
    bat --wrap=never --style=plain
