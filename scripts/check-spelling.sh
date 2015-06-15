#!/bin/bash

# 

[[ $# -eq 0 ]] && echo "Usage: $0  ‹directory or file to scan›+" && exit 0
LowerLimit=2

function tokenize() {
    local file="$1" word
    for word in $(grep -oE '[a-zA-Z]{'$LowerLimit',}' "$file" | sort -u); do
        # unCamelCase http://stackoverflow.com/a/7971063/1418165
        echo $word \
            | sed -e 's|\([A-Z][^A-Z]\)|\n\1|g' -e 's|\([a-z]\)\([A-Z]\)|\1\n\2|g' \
            | tr '[:upper:]' '[:lower:]'
    done
}

function cleanse() {
    local line="$1" firstChar=${line:0:1} word="$line" #word=$(echo "$line" | cut -d ' ' -f 2)
    [[ $firstChar = '#' ]] && echo "$word"
    [[ $firstChar = '?' ]] && echo "$word"
}

function spelling() {
    local file="$1" words
    words=$(tokenize "$file" | sort --unique)
    echo $words \
        | aspell -a --lang='en_US' --ignore=$LowerLimit --dont-suggest --personal=./scripts/.aspell.en.pws \
        | ( while read line; do cleanse "$line"; done )
}

shopt -s globstar  # Turns glob pattern ** into recursive search

until [[ "$1" = '' ]]; do
    if [[ -d "$1" ]]; then
        for file in "$1"/**; do
            [[ ! -f "$file" ]] && continue
            spelling "$file"
        done
    else
        spelling "$1"
    fi
    shift 1
done

# End of Script
