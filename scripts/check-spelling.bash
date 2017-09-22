#!/bin/bash

pushd "$(dirname "$0")" >/dev/null

ROOT="$(pwd -P)"/..


# from https://en.wikipedia.org/wiki/Commonly_misspelled_English_words
FILE="misspellings.txt"

function check_spelling {
    correct=$(echo "$1" | cut -f1 -d"|")
    bad=$(echo "$1" | cut -f2 -d"|")
    bad_grep=${bad// /|}
    bad_sed=${bad// /\\|}

    while IFS= read f; do
        echo "fixing $f with $correct"
        sed -i "s/$bad_sed/$correct/g" $f
    done < <(egrep -rlw "$bad_grep" $ROOT/{applications,core,doc})
}

while read LINE; do
    check_spelling "$LINE"
done < $FILE

popd >/dev/null
